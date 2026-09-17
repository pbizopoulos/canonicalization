#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
"""Inline supported direct Python calls in place without executing the input."""

from __future__ import annotations

import argparse
import ast
import builtins
import copy
import inspect
import io
import os
import sys
import tempfile
import tokenize
import warnings
from dataclasses import dataclass
from pathlib import Path

MAX_EXPANSIONS = 1000
MAX_DEPTH = 32
MAX_STATEMENTS = 10000
DYNAMIC = frozenset(
    [
        "eval",
        "exec",
        "compile",
        "__import__",
        "getattr",
        "setattr",
        "delattr",
        "globals",
        "locals",
        "vars",
        "dir",
        "help",
        "breakpoint",
        "super",
    ],
)
BUILTINS = frozenset(
    name for name in dir(builtins) if callable(getattr(builtins, name))
)
BUILTIN_NAMES = frozenset(dir(builtins))
MODULE_NAMES = frozenset(
    {
        "__name__",
        "__doc__",
        "__file__",
        "__package__",
        "__spec__",
        "__loader__",
        "__cached__",
    },
)
ALLOWED = (
    ast.TypeIgnore,
    ast.Delete,
    ast.Del,
    ast.NamedExpr,
    ast.Starred,
    ast.With,
    ast.withitem,
    ast.Try,
    ast.ExceptHandler,
    ast.Raise,
    ast.Assert,
    ast.JoinedStr,
    ast.FormattedValue,
    ast.AnnAssign,
    ast.Module,
    ast.FunctionDef,
    ast.arguments,
    ast.arg,
    ast.Import,
    ast.ImportFrom,
    ast.alias,
    ast.Assign,
    ast.AugAssign,
    ast.Expr,
    ast.If,
    ast.For,
    ast.While,
    ast.Break,
    ast.Continue,
    ast.Pass,
    ast.Return,
    ast.Name,
    ast.Constant,
    ast.List,
    ast.Tuple,
    ast.Set,
    ast.Dict,
    ast.BinOp,
    ast.UnaryOp,
    ast.BoolOp,
    ast.Compare,
    ast.IfExp,
    ast.Attribute,
    ast.Subscript,
    ast.Slice,
    ast.Call,
    ast.keyword,
    ast.Load,
    ast.Store,
    ast.Add,
    ast.Sub,
    ast.Mult,
    ast.MatMult,
    ast.Div,
    ast.FloorDiv,
    ast.Mod,
    ast.Pow,
    ast.LShift,
    ast.RShift,
    ast.BitOr,
    ast.BitXor,
    ast.BitAnd,
    ast.Invert,
    ast.Not,
    ast.UAdd,
    ast.USub,
    ast.And,
    ast.Or,
    ast.Eq,
    ast.NotEq,
    ast.Lt,
    ast.LtE,
    ast.Gt,
    ast.GtE,
    ast.Is,
    ast.IsNot,
    ast.In,
    ast.NotIn,
)


@dataclass(frozen=True, order=True)
class Diagnostic:
    """A deterministic source location, stable code, and explanatory message."""

    line: int
    column: int
    code: str
    message: str


def function_metadata_reads(tree: ast.Module) -> set[int]:
    """Allow metadata reads without letting a mutable defaults dictionary escape."""
    parents = {
        id(child): parent
        for parent in ast.walk(tree)
        for child in ast.iter_child_nodes(parent)
    }
    return {
        id(node)
        for node in ast.walk(tree)
        if isinstance(node, ast.Attribute)
        and isinstance(node.ctx, ast.Load)
        and (
            node.attr in {"__name__", "__defaults__"}
            or (
                node.attr == "__kwdefaults__"
                and isinstance(parent := parents.get(id(node)), ast.Subscript)
                and parent.value is node
                and isinstance(parent.ctx, ast.Load)
            )
        )
    }


class Checker(ast.NodeVisitor):
    """Collect symbols and validate every body, including unused functions."""

    def __init__(self, tree: ast.Module) -> None:
        """Initialize module symbols before visiting any calls."""
        self.tree = tree
        self.metadata_reads = function_metadata_reads(tree)
        self.errors: list[Diagnostic] = []
        self.functions: dict[str, ast.FunctionDef] = {}
        self.imports: dict[str, str] = {}
        self.classes: dict[str, ast.ClassDef] = {}
        self.module_locals: set[str] = set()
        self.locals: set[str] = set()
        self.scope: str | None = None
        self.class_scope = False
        self.comprehension_bindings: list[set[str]] = []
        self.unsupported_bindings: set[str] = set()
        self.in_annotation = False
        self.postponed_annotations = any(
            isinstance(node, ast.ImportFrom)
            and node.module == "__future__"
            and any(alias.name == "annotations" for alias in node.names)
            for node in tree.body
        )
        self.loops = 0
        self.lazy = 0
        self.edges: dict[str, list[tuple[str, ast.Call]]] = {}
        self.external_aliases: set[str] = set()

    def error(self, node: ast.AST, code: str, message: str) -> None:
        """Record a one-based source location."""
        self.errors.append(
            Diagnostic(
                getattr(node, "lineno", 1),
                getattr(node, "col_offset", 0) + 1,
                code,
                message,
            ),
        )

    def declare(self, name: str, node: ast.AST) -> None:
        """Prevent duplicate or builtin-shadowing declarations."""
        if (
            name in self.functions
            or name in self.imports
            or name in self.classes
            or name in BUILTINS
        ):
            self.error(node, "binding", f"Ambiguous callable/import binding: {name}")

    def collect(self) -> None:
        """Collect unconditional top-level declarations and module stores."""
        for node in self.tree.body:
            if isinstance(node, ast.FunctionDef):
                self.declare(node.name, node)
                self.functions[node.name] = node
                self.edges[node.name] = []
                for default in node.args.defaults + [
                    value for value in node.args.kw_defaults if value is not None
                ]:
                    self.module_locals.update(self.stores(default))
            elif isinstance(node, ast.ClassDef):
                self.declare(node.name, node)
                self.classes[node.name] = node
            elif isinstance(node, (ast.Import, ast.ImportFrom)):
                for alias in node.names:
                    name = alias.asname or alias.name.split(".")[0]
                    self.declare(name, node)
                    origin = (
                        alias.name
                        if isinstance(node, ast.Import)
                        else f"{node.module}.{alias.name}"
                    )
                    self.imports[name] = origin
            else:
                self.module_locals.update(self.stores(node, annotation_bindings=False))
        self.locals = self.module_locals
        self.unsupported_bindings = self.declaration_names(self.tree) - (
            self.functions.keys() | self.classes.keys() | self.imports.keys()
        )
        self.external_aliases = self.aliases(self.tree)

    def aliases(self, scope: ast.Module | ast.FunctionDef) -> set[str]:
        """Recognize single-assignment aliases of opaque external operations."""
        assignments: dict[str, list[ast.AST]] = {}
        pending: list[ast.AST] = list(scope.body)
        while pending:
            node = pending.pop()
            if isinstance(node, (ast.FunctionDef, ast.ClassDef, ast.Lambda)):
                continue
            if isinstance(node, ast.Name) and isinstance(
                node.ctx,
                (ast.Store, ast.Del),
            ):
                assignments.setdefault(node.id, []).append(node)
            pending.extend(ast.iter_child_nodes(node))
        locals_ = self.stores(scope)
        if isinstance(scope, ast.FunctionDef):
            locals_.update(
                arg.arg
                for arg in scope.args.posonlyargs
                + scope.args.args
                + scope.args.kwonlyargs
            )
            locals_.update(
                arg.arg
                for arg in (scope.args.vararg, scope.args.kwarg)
                if arg is not None
            )
        result = set()
        pending = list(scope.body)
        while pending:
            node = pending.pop()
            if isinstance(node, (ast.FunctionDef, ast.ClassDef, ast.Lambda)):
                continue
            if (
                isinstance(node, ast.Assign)
                and len(node.targets) == 1
                and isinstance(node.targets[0], ast.Name)
            ):
                value = node.value
                external = (
                    isinstance(value, ast.Name)
                    and value.id not in locals_
                    and (value.id in self.imports or value.id in BUILTINS - DYNAMIC)
                ) or (
                    isinstance(value, ast.Attribute)
                    and not self.dynamic_reference(value)
                    and value.attr not in self.functions
                    and value.attr
                    not in {
                        "__call__",
                        "__getattribute__",
                        "__getattr__",
                        "__setattr__",
                        "__delattr__",
                    }
                )
                if external and len(assignments.get(node.targets[0].id, [])) == 1:
                    result.add(node.targets[0].id)
            pending.extend(ast.iter_child_nodes(node))
        return result

    @staticmethod
    def stores(node: ast.AST, *, annotation_bindings: bool = True) -> set[str]:
        """Find statically local assignment names (unsupported scopes fail later)."""
        names: set[str] = set()
        if (
            not annotation_bindings
            and isinstance(node, ast.AnnAssign)
            and node.value is None
        ):
            return names
        pending: list[ast.AST] = (
            list(node.body)
            if isinstance(node, ast.FunctionDef)
            else list(ast.iter_child_nodes(node))
        )
        if isinstance(node, ast.Name) and isinstance(node.ctx, (ast.Store, ast.Del)):
            names.add(node.id)
        while pending:
            child = pending.pop()
            if (
                not annotation_bindings
                and isinstance(child, ast.AnnAssign)
                and child.value is None
            ):
                continue
            if isinstance(
                child,
                (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Lambda),
            ):
                continue
            if isinstance(
                child,
                (ast.ListComp, ast.SetComp, ast.DictComp, ast.GeneratorExp),
            ):
                names.update(Checker.walrus_stores(child))
                continue
            if isinstance(child, ast.Name) and isinstance(
                child.ctx,
                (ast.Store, ast.Del),
            ):
                names.add(child.id)
            if isinstance(child, ast.ExceptHandler) and child.name is not None:
                names.add(child.name)
            pending.extend(ast.iter_child_nodes(child))
        return names

    @staticmethod
    def declaration_names(node: ast.AST) -> set[str]:
        """Keep rejected local declarations known for diagnostic recovery."""
        names: set[str] = set()
        pending = list(ast.iter_child_nodes(node))
        while pending:
            child = pending.pop()
            if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                names.add(child.name)
                continue
            if isinstance(child, (ast.Import, ast.ImportFrom)):
                names.update(
                    alias.asname or alias.name.split(".")[0] for alias in child.names
                )
            else:
                pending.extend(ast.iter_child_nodes(child))
        return names

    def local_data(self, name: str) -> bool:
        """Distinguish lexical data bindings from module callable names."""
        return (
            name in self.locals and (self.scope is not None or self.class_scope)
        ) or any(name in bound for bound in self.comprehension_bindings)

    def annotation(self, node: ast.AST | None) -> None:
        """Preserve type metadata without conflating it with parameter bindings."""
        if node is None or self.postponed_annotations:
            return
        previous = self.in_annotation
        self.in_annotation = True
        self.visit(node)
        self.in_annotation = previous

    @staticmethod
    def walrus_stores(node: ast.AST) -> set[str]:
        """Comprehension assignment expressions bind in the enclosing scope."""
        names: set[str] = set()
        pending = [node]
        while pending:
            child = pending.pop()
            if isinstance(child, (ast.Lambda, ast.FunctionDef, ast.ClassDef)):
                continue
            if isinstance(child, ast.NamedExpr) and isinstance(child.target, ast.Name):
                names.add(child.target.id)
            pending.extend(ast.iter_child_nodes(child))
        return names

    def reference(self, node: ast.AST) -> str:
        """Resolve import aliases without executing or importing input code."""
        if isinstance(node, ast.Name):
            return (
                "" if self.local_data(node.id) else self.imports.get(node.id, node.id)
            )
        if isinstance(node, ast.Attribute):
            return f"{self.reference(node.value)}.{node.attr}"
        return ""

    def visit_If(self, node: ast.If) -> None:
        """Skip a proven typing-only branch while checking its runtime else."""
        if self.reference(node.test) == "typing.TYPE_CHECKING":
            for statement in node.orelse:
                self.visit(statement)
            return
        self.generic_visit(node)

    def visit_ClassDef(self, node: ast.ClassDef) -> None:
        """Preserve passive data/exception classes; reject user method dispatch."""
        if self.classes.get(node.name) is not node or self.scope is not None:
            self.error(node, "class", "Only top-level passive classes are supported")
            return
        allowed = (ast.Pass, ast.Assign, ast.AnnAssign)
        if getattr(node, "type_params", []) or any(
            not isinstance(statement, allowed)
            and not (
                isinstance(statement, ast.Expr)
                and isinstance(statement.value, ast.Constant)
                and isinstance(statement.value.value, str)
            )
            for statement in node.body
        ):
            self.error(
                node,
                "class",
                "Methods and executable class control flow require dispatch analysis",
            )
            return
        for decorator in node.decorator_list:
            target = decorator.func if isinstance(decorator, ast.Call) else decorator
            if self.reference(target) != "dataclasses.dataclass":
                self.error(decorator, "class", "Unsupported class decorator")
            self.visit(decorator)
        for base in node.bases:
            self.visit(base)
        for keyword in node.keywords:
            self.visit(keyword.value)
        previous = self.locals
        previous_class = self.class_scope
        self.class_scope = True
        self.locals = set()
        for statement in node.body:
            self.visit(statement)
            if not isinstance(statement, ast.AnnAssign) or statement.value is not None:
                self.locals.update(self.stores(statement))
        self.locals = previous
        self.class_scope = previous_class

    def visit_ListComp(
        self,
        node: ast.ListComp | ast.SetComp | ast.DictComp | ast.GeneratorExp,
        *,
        eager: bool = False,
    ) -> None:
        """Analyze eager comprehensions in their isolated binding scope."""
        self.visit(node.generators[0].iter)
        if isinstance(node, ast.GeneratorExp) and not eager:
            self.lazy += 1
        previous = self.locals
        bound = set().union(*(self.stores(g.target) for g in node.generators))
        self.locals = previous | bound
        self.comprehension_bindings.append(bound)
        for index, generator in enumerate(node.generators):
            if generator.is_async:
                self.error(
                    node,
                    "syntax",
                    "Async comprehensions require suspension-aware expansion",
                )
            if index:
                self.visit(generator.iter)
            self.visit(generator.target)
            for condition in generator.ifs:
                self.visit(condition)
        if isinstance(node, ast.DictComp):
            self.visit(node.key)
            self.visit(node.value)
        else:
            self.visit(node.elt)
        self.locals = previous
        self.comprehension_bindings.pop()
        if isinstance(node, ast.GeneratorExp) and not eager:
            self.lazy -= 1

    visit_SetComp = visit_ListComp  # noqa: N815
    visit_DictComp = visit_ListComp  # noqa: N815
    visit_GeneratorExp = visit_ListComp  # noqa: N815

    def visit_Lambda(self, node: ast.Lambda) -> None:
        """Reject anonymous callbacks whose invocation site is unresolved."""
        self.error(
            node,
            "callback",
            "Lambda invocation cannot be expanded at a resolved call site",
        )

    def generic_visit(self, node: ast.AST) -> None:
        """Reject everything outside the explicit syntax allowlist."""
        if type(node) not in ALLOWED:
            self.error(node, "syntax", f"Unsupported syntax: {type(node).__name__}")
            return
        super().generic_visit(node)

    @staticmethod
    def supported_signature(node: ast.FunctionDef) -> bool:
        """Identify signatures whose binding rules this profile supports."""
        return not node.decorator_list

    def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
        """Validate declarations and analyze every function body."""
        if self.scope is not None or self.functions.get(node.name) is not node:
            self.error(node, "declaration", "Only top-level functions are supported")
            return
        args = node.args
        params = args.posonlyargs + args.args + args.kwonlyargs
        params += [arg for arg in (args.vararg, args.kwarg) if arg is not None]
        if not self.supported_signature(node):
            self.error(
                node,
                "signature",
                "Decorated call targets cannot be resolved statically",
            )
        if getattr(node, "type_params", []):
            self.error(
                node,
                "annotation",
                "Generic type parameters require annotation-scope analysis",
            )
        self.annotation(node.returns)
        for parameter in params:
            self.annotation(parameter.annotation)
        for default in args.defaults + [
            value for value in args.kw_defaults if value is not None
        ]:
            self.visit(default)
        names = [arg.arg for arg in params]
        if len(names) != len(set(names)):
            self.error(node, "signature", "Duplicate parameter names")
        previous = self.locals
        previous_aliases = self.external_aliases
        self.external_aliases = self.aliases(node)
        self.scope = node.name
        self.locals = self.stores(node) | set(names)
        previous_unsupported = self.unsupported_bindings
        self.unsupported_bindings = previous_unsupported | self.declaration_names(node)
        for statement in node.body:
            self.visit(statement)
        self.scope = None
        self.locals = previous
        self.external_aliases = previous_aliases
        self.unsupported_bindings = previous_unsupported

    def visit_Import(self, node: ast.Import | ast.ImportFrom) -> None:
        """Disallow conditional, local, relative, and wildcard imports."""
        if self.scope is not None or node not in self.tree.body:
            self.error(
                node,
                "declaration",
                "Imports must be unconditional and module-level",
            )
        if isinstance(node, ast.ImportFrom) and (
            node.level or any(a.name == "*" for a in node.names)
        ):
            self.error(node, "import", "Relative and wildcard imports are unsupported")
        self.generic_visit(node)

    visit_ImportFrom = visit_Import  # noqa: N815

    def visit_Name(self, node: ast.Name) -> None:
        """Check module rebinding while respecting class and function namespaces."""
        if isinstance(node.ctx, (ast.Store, ast.Del)):
            if (
                self.scope is None
                and not self.class_scope
                and not any(node.id in bound for bound in self.comprehension_bindings)
                and (
                    node.id in self.functions
                    or node.id in self.imports
                    or node.id in self.classes
                    or node.id in BUILTINS
                )
            ):
                self.error(node, "binding", f"Cannot rebind callable/import: {node.id}")
        elif self.local_data(node.id) or node.id in self.unsupported_bindings:
            return
        elif self.dynamic_reference(node) or node.id == "__builtins__":
            self.error(node, "dynamic", "Reflective builtin values are unsupported")
        elif node.id in self.functions:
            self.error(node, "function-value", "Function names require direct calls")
        elif (
            node.id not in self.locals
            and node.id not in self.imports
            and node.id not in self.classes
            and node.id not in BUILTIN_NAMES
            and node.id not in self.module_locals
            and node.id not in MODULE_NAMES
            and not self.in_annotation
        ):
            self.error(node, "scope", f"Data name is outside this scope: {node.id}")

    def dynamic_reference(self, node: ast.AST) -> bool:
        """Recognize reflective builtin values, including imported aliases."""
        parts: list[str] = []
        while isinstance(node, ast.Attribute):
            parts.insert(0, node.attr)
            node = node.value
        if not isinstance(node, ast.Name) or self.local_data(node.id):
            return False
        origin = ".".join([self.imports.get(node.id, f"builtins.{node.id}"), *parts])
        return origin.startswith("builtins.") and origin.split(".")[1] in DYNAMIC

    def visit_ExceptHandler(self, node: ast.ExceptHandler) -> None:
        """Exception aliases obey the current lexical namespace."""
        if (
            node.name
            and self.scope is None
            and not self.class_scope
            and (
                node.name in self.functions
                or node.name in self.imports
                or node.name in self.classes
                or node.name in BUILTINS
            )
        ):
            self.error(
                node,
                "binding",
                "Exception alias cannot rebind a callable/import",
            )
        self.generic_visit(node)

    def visit_AnnAssign(self, node: ast.AnnAssign) -> None:
        """Keep evaluated metadata and actual assignments distinct."""
        if self.scope is None:
            self.annotation(node.annotation)
        if node.value is not None:
            self.visit(node.value)
            self.visit(node.target)

    def visit_Attribute(self, node: ast.Attribute) -> None:
        """Permit data fields while protecting potentially aliased user callables."""
        if (
            isinstance(node.ctx, ast.Load)
            and isinstance(node.value, ast.Name)
            and node.value.id in self.functions
            and not self.local_data(node.value.id)
            and id(node) in self.metadata_reads
        ):
            return
        if not isinstance(node.ctx, ast.Load) and (
            node.attr in self.functions or node.attr in self.classes
        ):
            self.error(
                node,
                "binding",
                "Attribute mutation may replace a user callable binding",
            )
        if self.dynamic_reference(node) or node.attr in {
            "__dict__",
            "__globals__",
            "__builtins__",
            "f_globals",
            "f_locals",
        }:
            self.error(node, "dynamic", "Reflective namespace access is unsupported")
        self.generic_visit(node)

    def visit_Subscript(self, node: ast.Subscript) -> None:
        """Permit data updates while retaining checks on keys and values."""
        self.generic_visit(node)

    def visit_For(self, node: ast.For | ast.While) -> None:
        """Track loop control; a loop else is outside that loop's control scope."""
        if isinstance(node, ast.For):
            self.visit(node.target)
            self.visit(node.iter)
        else:
            self.visit(node.test)
        self.loops += 1
        for statement in node.body:
            self.visit(statement)
        self.loops -= 1
        for statement in node.orelse:
            self.visit(statement)

    visit_While = visit_For  # noqa: N815

    def visit_Break(self, node: ast.Break | ast.Continue) -> None:
        """Reject loop control without an enclosing loop."""
        if not self.loops:
            self.error(node, "control", "Loop control requires an enclosing loop")

    visit_Continue = visit_Break  # noqa: N815

    def bind(self, node: ast.Call, function: ast.FunctionDef) -> None:
        """Validate binding only when the declaration has a supported signature."""
        if (
            not self.supported_signature(function)
            or any(isinstance(arg, ast.Starred) for arg in node.args)
            or any(keyword.arg is None for keyword in node.keywords)
        ):
            return
        args = function.args
        positional = args.posonlyargs + args.args
        default_start = len(positional) - len(args.defaults)
        parameters = [
            inspect.Parameter(
                arg.arg,
                inspect.Parameter.POSITIONAL_ONLY
                if index < len(args.posonlyargs)
                else inspect.Parameter.POSITIONAL_OR_KEYWORD,
                default=None if index >= default_start else inspect.Parameter.empty,
            )
            for index, arg in enumerate(positional)
        ]
        if args.vararg:
            parameters.append(
                inspect.Parameter(args.vararg.arg, inspect.Parameter.VAR_POSITIONAL),
            )
        parameters.extend(
            inspect.Parameter(
                arg.arg,
                inspect.Parameter.KEYWORD_ONLY,
                default=None if default is not None else inspect.Parameter.empty,
            )
            for arg, default in zip(args.kwonlyargs, args.kw_defaults, strict=True)
        )
        if args.kwarg:
            parameters.append(
                inspect.Parameter(args.kwarg.arg, inspect.Parameter.VAR_KEYWORD),
            )
        try:
            inspect.Signature(parameters).bind(
                *[None for _ in node.args],
                **{
                    keyword.arg: None
                    for keyword in node.keywords
                    if keyword.arg is not None
                },
            )
        except TypeError:
            self.error(node, "arguments", f"Arguments do not bind to {function.name}")

    def visit_Call(self, node: ast.Call) -> None:
        """Resolve direct user, builtin, or import-rooted external calls."""
        target = node.func
        parts: list[str] = []
        while isinstance(target, ast.Attribute):
            parts.insert(0, target.attr)
            target = target.value
        name = target.id if isinstance(target, ast.Name) else ""
        shadowed = name in self.locals
        if name in self.unsupported_bindings:
            self.visit_actual_arguments(node)
            return
        if not parts and name in self.functions and not shadowed:
            if self.in_annotation:
                self.error(
                    node,
                    "annotation-call",
                    "User calls in annotations require annotation-aware expansion",
                )
            self.bind(node, self.functions[name])
            if self.lazy:
                self.error(
                    node,
                    "generator-call",
                    "User calls in lazy generators require suspension-aware expansion",
                )
            if self.scope is not None:
                self.edges[self.scope].append((name, node))
        elif (not parts and name in self.external_aliases) or (
            not shadowed
            and (
                name in self.imports
                or (not parts and (name in BUILTINS or name in self.classes))
            )
        ):
            origin = ".".join([self.imports.get(name, f"builtins.{name}"), *parts])
            if origin.startswith("builtins.") and origin.split(".")[1] in DYNAMIC:
                self.error(
                    node,
                    "dynamic",
                    "Reflective/dynamic builtin calls are unsupported",
                )
            else:
                self.visit(node.func)
        elif isinstance(node.func, ast.Attribute) and node.func.attr not in {
            "__call__",
            "__getattribute__",
            "__getattr__",
            "__setattr__",
            "__delattr__",
        }:
            self.visit(node.func)
        else:
            self.error(
                node,
                "call",
                "Unresolved computed call target",
            )
            self.visit(node.func)
        self.visit_actual_arguments(node)

    def visit_actual_arguments(self, node: ast.Call) -> None:
        """Validate explicit arguments and analyze their evaluation expressions."""
        keywords = [k.arg for k in node.keywords if k.arg is not None]
        if len(keywords) != len(set(keywords)):
            self.error(node, "arguments", "Duplicate keyword argument")
        eager_consumer = (
            isinstance(node.func, ast.Name)
            and node.func.id in {"any", "all", "sorted", "tuple", "list"}
            and node.func.id not in self.locals
            and node.func.id not in self.imports
            and len(node.args) == 1
            and not node.keywords
        )
        for argument in node.args:
            if eager_consumer and isinstance(argument, ast.GeneratorExp):
                self.visit_ListComp(argument, eager=True)
            else:
                self.visit(argument)
        for keyword in node.keywords:
            self.visit(keyword.value)

    def cycles(self) -> None:
        """Report each strongly connected recursive component exactly once."""
        reachable: dict[str, set[str]] = {}
        for name in sorted(self.functions):
            seen: set[str] = set()
            pending = [name]
            while pending:
                current = pending.pop()
                if current not in seen:
                    seen.add(current)
                    pending.extend(target for target, _ in self.edges[current])
            reachable[name] = seen
        remaining = set(self.functions)
        while remaining:
            name = min(remaining)
            component = {
                target for target in reachable[name] if name in reachable[target]
            }
            remaining -= component
            edges = [
                node
                for source in sorted(component)
                for target, node in self.edges[source]
                if target in component
            ]
            if edges:
                node = min(edges, key=lambda call: (call.lineno, call.col_offset))
                self.error(
                    node,
                    "cycle",
                    f"Recursive call component: {', '.join(sorted(component))}",
                )


def check_source(source: str, filename: str = "<string>") -> list[Diagnostic]:
    """Return diagnostics without importing or executing the input."""
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", SyntaxWarning)
            tree = ast.parse(source, filename=filename, type_comments=True)
            compile(tree, filename, "exec", dont_inherit=True)
    except (SyntaxError, ValueError, UnicodeError) as error:
        return [
            Diagnostic(
                getattr(error, "lineno", None) or 1,
                len(
                    (
                        getattr(error, "text", None)
                        or (
                            source.splitlines()[error.lineno - 1]
                            if isinstance(error, SyntaxError)
                            and error.lineno
                            and error.lineno <= len(source.splitlines())
                            else ""
                        )
                    )[: max((getattr(error, "offset", None) or 1) - 1, 0)].encode(
                        "utf-8",
                    ),
                )
                + 1,
                "parse",
                str(error),
            ),
        ]
    checker = Checker(tree)
    checker.collect()
    checker.visit(tree)
    checker.cycles()
    return sorted(set(checker.errors))


def check_file(path: str | Path) -> list[Diagnostic]:
    """Read one source using its Python encoding declaration, then check it."""
    try:
        with tokenize.open(path) as stream:
            source = stream.read()
    except (OSError, UnicodeError, SyntaxError, LookupError, ValueError) as error:
        return [Diagnostic(1, 1, "input", str(error))]
    return check_source(source, str(path))


class InlineUnsupportedError(Exception):
    """Abort a transaction before writing any source."""

    def __init__(self, node: ast.AST, message: str) -> None:
        """Attach the original call site's location to a refusal."""
        super().__init__(message)
        self.diagnostic = Diagnostic(
            getattr(node, "lineno", 1),
            getattr(node, "col_offset", 0) + 1,
            "inline",
            message,
        )


class RenameLocals(ast.NodeTransformer):
    """Give each expanded invocation its own statically bound names."""

    def __init__(self, names: dict[str, str]) -> None:
        """Keep the caller's bindings separate from the callee's bindings."""
        self.names = names

    def visit_Name(self, node: ast.Name) -> ast.Name:
        """Rename both reads and writes, retaining their source locations."""
        return ast.copy_location(
            ast.Name(id=self.names.get(node.id, node.id), ctx=node.ctx),
            node,
        )


class Inliner:
    """Expand supported callees at eager, statement-level evaluation sites."""

    def __init__(
        self,
        tree: ast.Module,
        *,
        strict: bool = True,
        limits: tuple[int, int, int] = (MAX_EXPANSIONS, MAX_DEPTH, MAX_STATEMENTS),
    ) -> None:
        """Reserve every source identifier and retain original function bodies."""
        self.strict = strict
        self.diagnostics: list[Diagnostic] = []
        self.functions = {
            node.name: node for node in tree.body if isinstance(node, ast.FunctionDef)
        }
        self.reserved = {
            node.id for node in ast.walk(tree) if isinstance(node, ast.Name)
        } | {node.arg for node in ast.walk(tree) if isinstance(node, ast.arg)}
        self.reserved.update(
            node.name
            for node in ast.walk(tree)
            if isinstance(node, (ast.FunctionDef, ast.ClassDef))
        )
        self.reserved.update(
            node.asname or node.name.split(".")[0]
            for node in ast.walk(tree)
            if isinstance(node, ast.alias)
        )
        self.counter = 0
        self.expansions = 0
        self.completed = 0
        if any(limit <= 0 for limit in limits):
            msg = "Expansion limits must be positive"
            raise ValueError(msg)
        self.max_expansions, self.max_depth, self.max_statements = limits
        self.depth = 0

    def fresh(self) -> str:
        """Allocate a collision-free temporary without class name mangling."""
        while True:
            self.counter += 1
            name = f"_inline_{self.counter}"
            if name not in self.reserved:
                self.reserved.add(name)
                return name

    def has_call(self, node: ast.AST) -> bool:
        """Find direct calls whose declarations belong to this source file."""
        return any(
            isinstance(child, ast.Call)
            and isinstance(child.func, ast.Name)
            and child.func.id in self.functions
            for child in ast.walk(node)
        )

    def save(self, value: ast.expr, statements: list[ast.stmt]) -> ast.Name:
        """Evaluate a value once before evaluating any later sibling."""
        name = self.fresh()
        statements.append(
            ast.Assign(targets=[ast.Name(id=name, ctx=ast.Store())], value=value),
        )
        return ast.Name(id=name, ctx=ast.Load())

    def call_expression(
        self,
        node: ast.Call,
        caller_locals: set[str],
    ) -> tuple[list[ast.stmt], ast.expr]:
        """Freeze an opaque callee before evaluating its expanded arguments."""
        statements: list[ast.stmt] = []
        if any(isinstance(arg, ast.Starred) for arg in node.args) or any(
            keyword.arg is None for keyword in node.keywords
        ):
            raise InlineUnsupportedError(
                node,
                "Calls with argument unpacking require runtime binding",
            )
        if (
            isinstance(node.func, ast.Name)
            and node.func.id in self.functions
            and node.func.id not in caller_locals
        ):
            completed = self.completed
            try:
                return self.expand(node, caller_locals)
            except InlineUnsupportedError as error:
                self.completed = completed
                if self.strict:
                    raise
                self.diagnostics.append(error.diagnostic)
        prefix, function = self.expression(node.func, caller_locals)
        statements.extend(prefix)
        function = self.save(function, statements)
        args: list[ast.expr] = []
        for argument in node.args:
            prefix, value = self.expression(argument, caller_locals)
            statements.extend(prefix)
            args.append(self.save(value, statements))
        keywords = []
        for keyword in node.keywords:
            prefix, value = self.expression(keyword.value, caller_locals)
            statements.extend(prefix)
            keywords.append(
                ast.keyword(arg=keyword.arg, value=self.save(value, statements)),
            )
        return statements, ast.Call(func=function, args=args, keywords=keywords)

    def expression(
        self,
        node: ast.expr,
        caller_locals: set[str],
    ) -> tuple[list[ast.stmt], ast.expr]:
        """Retain unsupported expressions while expanding independent siblings."""
        completed = self.completed
        try:
            result = self.lower_expression(node, caller_locals)
        except InlineUnsupportedError as error:
            self.completed = completed
            if self.strict:
                raise
            self.diagnostics.append(error.diagnostic)
            return [], copy.deepcopy(node)
        else:
            if not self.strict and self.completed == completed:
                return [], copy.deepcopy(node)
            return result

    def lower_expression(
        self,
        node: ast.expr,
        caller_locals: set[str],
    ) -> tuple[list[ast.stmt], ast.expr]:
        """Lower eager expressions in Python evaluation order."""
        if not self.has_call(node):
            return [], copy.deepcopy(node)
        statements: list[ast.stmt] = []
        if isinstance(node, ast.Call):
            return self.call_expression(node, caller_locals)
        if isinstance(
            node,
            (
                ast.ListComp,
                ast.SetComp,
                ast.DictComp,
                ast.Compare,
                ast.JoinedStr,
                ast.List,
                ast.Tuple,
                ast.Subscript,
            ),
        ):
            return self.compound_expression(node, caller_locals)
        if isinstance(node, ast.BinOp):
            prefix, left = self.expression(node.left, caller_locals)
            statements.extend(prefix)
            left = self.save(left, statements)
            prefix, right = self.expression(node.right, caller_locals)
            statements.extend(prefix)
            return statements, ast.BinOp(left=left, op=node.op, right=right)
        if isinstance(node, (ast.UnaryOp, ast.Attribute)):
            child = node.operand if isinstance(node, ast.UnaryOp) else node.value
            prefix, value = self.expression(child, caller_locals)
            replacement = copy.deepcopy(node)
            if isinstance(replacement, ast.UnaryOp):
                replacement.operand = value
            else:
                replacement.value = value
            return prefix, replacement
        if isinstance(node, ast.IfExp):
            prefix, test = self.expression(node.test, caller_locals)
            result = self.fresh()
            body, value = self.expression(node.body, caller_locals)
            body.append(
                ast.Assign(targets=[ast.Name(id=result, ctx=ast.Store())], value=value),
            )
            otherwise, value = self.expression(node.orelse, caller_locals)
            otherwise.append(
                ast.Assign(targets=[ast.Name(id=result, ctx=ast.Store())], value=value),
            )
            prefix.append(ast.If(test=test, body=body, orelse=otherwise))
            return prefix, ast.Name(id=result, ctx=ast.Load())
        raise InlineUnsupportedError(
            node,
            f"Calls inside {type(node).__name__} require "
            "additional evaluation-order handling",
        )

    def subscript_expression(
        self,
        node: ast.Subscript,
        caller_locals: set[str],
    ) -> tuple[list[ast.stmt], ast.expr]:
        """Preserve the order of subscription operands."""
        prefix, value = self.expression(node.value, caller_locals)
        value = self.save(value, prefix)
        suffix, index = self.subscript_index(node.slice, caller_locals)
        return [*prefix, *suffix], ast.Subscript(
            value=value,
            slice=index,
            ctx=node.ctx,
        )

    def subscript_index(
        self,
        node: ast.expr,
        caller_locals: set[str],
    ) -> tuple[list[ast.stmt], ast.expr]:
        """Evaluate slice components and multidimensional indices in order."""
        if not isinstance(node, (ast.Slice, ast.Tuple)):
            return self.expression(node, caller_locals)
        statements: list[ast.stmt] = []
        values: list[ast.expr | None] = []
        children = (
            [node.lower, node.upper, node.step]
            if isinstance(node, ast.Slice)
            else node.elts
        )
        for child in children:
            if child is None:
                values.append(None)
                continue
            prefix, value = self.subscript_index(child, caller_locals)
            statements.extend(prefix)
            values.append(
                value if isinstance(value, ast.Slice) else self.save(value, statements),
            )
        if isinstance(node, ast.Slice):
            return statements, ast.Slice(
                lower=values[0],
                upper=values[1],
                step=values[2],
            )
        return statements, ast.Tuple(
            elts=[value for value in values if value is not None],
            ctx=ast.Load(),
        )

    def compound_expression(
        self,
        node: ast.expr,
        caller_locals: set[str],
    ) -> tuple[list[ast.stmt], ast.expr]:
        """Lower eager containers and formatted values in evaluation order."""
        statements: list[ast.stmt] = []
        if isinstance(node, ast.Subscript):
            return self.subscript_expression(node, caller_locals)
        if isinstance(node, (ast.ListComp, ast.SetComp, ast.DictComp)):
            return self.list_comprehension(node, caller_locals)
        if isinstance(node, ast.Compare) and len(node.ops) == 1:
            prefix, left = self.expression(node.left, caller_locals)
            left = self.save(left, prefix)
            suffix, right = self.expression(node.comparators[0], caller_locals)
            return [*prefix, *suffix], ast.Compare(
                left=left,
                ops=node.ops,
                comparators=[right],
            )
        if isinstance(node, ast.JoinedStr):
            return self.formatted_string(node, caller_locals)
        if isinstance(node, (ast.List, ast.Tuple)):
            elements: list[ast.expr] = []
            for element in node.elts:
                starred = isinstance(element, ast.Starred)
                prefix, value = self.expression(
                    element.value if isinstance(element, ast.Starred) else element,
                    caller_locals,
                )
                statements.extend(prefix)
                if starred:
                    value = ast.Tuple(
                        elts=[ast.Starred(value=value, ctx=ast.Load())],
                        ctx=ast.Load(),
                    )
                saved = self.save(value, statements)
                elements.append(
                    ast.Starred(value=saved, ctx=ast.Load()) if starred else saved,
                )
            return statements, type(node)(elts=elements, ctx=node.ctx)
        raise InlineUnsupportedError(
            node,
            "Chained comparisons require short-circuit expansion",
        )

    def formatted_string(
        self,
        node: ast.JoinedStr,
        caller_locals: set[str],
    ) -> tuple[list[ast.stmt], ast.expr]:
        """Finish each formatted segment before evaluating the next segment."""
        statements: list[ast.stmt] = []
        values: list[ast.expr] = []
        for part in node.values:
            if isinstance(part, ast.Constant):
                values.append(copy.deepcopy(part))
                continue
            if not isinstance(part, ast.FormattedValue):
                raise InlineUnsupportedError(node, "Unsupported string segment")
            prefix, value = self.expression(part.value, caller_locals)
            statements.extend(prefix)
            value = self.save(value, statements)
            spec = None
            conversion = part.conversion
            if part.format_spec is not None:
                prefix, spec = self.expression(part.format_spec, caller_locals)
                if prefix and conversion != -1:
                    converter = {ord("s"): "str", ord("r"): "repr", ord("a"): "ascii"}[
                        conversion
                    ]
                    if converter in caller_locals:
                        raise InlineUnsupportedError(
                            part,
                            "Format conversion builtin is shadowed in the caller",
                        )
                    value = self.save(
                        ast.Call(
                            func=ast.Name(id=converter, ctx=ast.Load()),
                            args=[value],
                            keywords=[],
                        ),
                        statements,
                    )
                    conversion = -1
                statements.extend(prefix)
            formatted = self.save(
                ast.JoinedStr(
                    values=[
                        ast.FormattedValue(
                            value=value,
                            conversion=conversion,
                            format_spec=spec,
                        ),
                    ],
                ),
                statements,
            )
            values.append(ast.FormattedValue(value=formatted, conversion=-1))
        return statements, ast.JoinedStr(values=values)

    @staticmethod
    def comprehension_bindings(
        node: ast.ListComp | ast.SetComp | ast.DictComp,
    ) -> set[str]:
        """Reject deferred scopes and reads of uninitialized iteration variables."""
        if any(generator.is_async for generator in node.generators) or any(
            isinstance(
                child,
                (
                    ast.NamedExpr,
                    ast.Lambda,
                    ast.GeneratorExp,
                ),
            )
            or (
                isinstance(child, (ast.ListComp, ast.SetComp, ast.DictComp))
                and child is not node
            )
            for child in ast.walk(node)
        ):
            raise InlineUnsupportedError(
                node,
                "Comprehension requires nested or deferred scope analysis",
            )
        bound = set().union(*(Checker.stores(g.target) for g in node.generators))
        assigned: set[str] = set()
        for index, generator in enumerate(node.generators):
            if any(
                isinstance(child, (ast.Attribute, ast.Subscript))
                for child in ast.walk(generator.target)
            ):
                raise InlineUnsupportedError(
                    node,
                    "Comprehension targets must be local names",
                )
            expressions = [generator.iter] if index else []
            for expression in expressions:
                reads = {
                    child.id
                    for child in ast.walk(expression)
                    if isinstance(child, ast.Name) and isinstance(child.ctx, ast.Load)
                }
                if (reads & bound) - assigned:
                    raise InlineUnsupportedError(
                        node,
                        "Comprehension may read a local before it is assigned",
                    )
            assigned.update(Checker.stores(generator.target))
            for condition in generator.ifs:
                reads = {
                    child.id
                    for child in ast.walk(condition)
                    if isinstance(child, ast.Name) and isinstance(child.ctx, ast.Load)
                }
                if (reads & bound) - assigned:
                    raise InlineUnsupportedError(
                        node,
                        "Comprehension may read a local before it is assigned",
                    )
        return bound

    def list_comprehension(
        self,
        node: ast.ListComp | ast.SetComp | ast.DictComp,
        caller_locals: set[str],
    ) -> tuple[list[ast.stmt], ast.expr]:
        """Lower eager loops and filters with private iteration bindings."""
        bound = self.comprehension_bindings(node)
        names = {name: self.fresh() for name in sorted(bound)}
        rename = RenameLocals(names)
        statements, iterable = self.expression(node.generators[0].iter, caller_locals)
        iterable = self.save(iterable, statements)
        container = (
            ast.Dict(keys=[], values=[])
            if isinstance(node, ast.DictComp)
            else ast.Set(elts=[])
            if isinstance(node, ast.SetComp)
            else ast.List(elts=[], ctx=ast.Load())
        )
        result = self.save(container, statements)
        scope = caller_locals | set(names.values())
        statements.extend(
            ast.Assign(
                targets=[ast.Name(id=name, ctx=ast.Store())],
                value=ast.Constant(value=None),
            )
            for name in names.values()
        )
        if isinstance(node, ast.DictComp):
            body, key = self.expression(rename.visit(copy.deepcopy(node.key)), scope)
            key = self.save(key, body)
            prefix, value = self.expression(
                rename.visit(copy.deepcopy(node.value)),
                scope,
            )
            body.extend(prefix)
            body.append(
                ast.Assign(
                    targets=[
                        ast.Subscript(
                            value=copy.deepcopy(result),
                            slice=key,
                            ctx=ast.Store(),
                        ),
                    ],
                    value=value,
                ),
            )
        else:
            body, value = self.expression(rename.visit(copy.deepcopy(node.elt)), scope)
            body.append(
                ast.Expr(
                    value=ast.Call(
                        func=ast.Attribute(
                            value=copy.deepcopy(result),
                            attr="add" if isinstance(node, ast.SetComp) else "append",
                            ctx=ast.Load(),
                        ),
                        args=[value],
                        keywords=[],
                    ),
                ),
            )
        for index in reversed(range(len(node.generators))):
            generator = node.generators[index]
            for condition in reversed(generator.ifs):
                prefix, test = self.expression(
                    rename.visit(copy.deepcopy(condition)),
                    scope,
                )
                body = [*prefix, ast.If(test=test, body=body, orelse=[])]
            prefix = []
            iterator: ast.expr = iterable
            if index:
                prefix, iterator = self.expression(
                    rename.visit(copy.deepcopy(generator.iter)),
                    scope,
                )
            body = [
                *prefix,
                ast.For(
                    target=rename.visit(copy.deepcopy(generator.target)),
                    iter=iterator,
                    body=body,
                    orelse=[],
                ),
            ]
        statements.extend(body)
        if names:
            statements.append(
                ast.Delete(
                    targets=[
                        ast.Name(id=name, ctx=ast.Del()) for name in names.values()
                    ],
                ),
            )
        return statements, result

    def validate_body(
        self,
        function: ast.FunctionDef,
        call: ast.Call,
        caller_locals: set[str],
    ) -> tuple[list[ast.stmt], set[str]]:
        """Reject control flow, capture, and possibly uninitialized callee locals."""
        args = function.args
        body = function.body
        if (
            body
            and isinstance(body[0], ast.Expr)
            and isinstance(body[0].value, ast.Constant)
            and isinstance(body[0].value.value, str)
        ):
            body = body[1:]
        parameters = args.posonlyargs + args.args + args.kwonlyargs
        bound = {parameter.arg for parameter in parameters}
        local_names = Checker.stores(function) | bound
        free = {
            node.id
            for statement in body
            for node in ast.walk(statement)
            if isinstance(node, ast.Name) and isinstance(node.ctx, ast.Load)
        } - local_names
        if conflicts := free & caller_locals:
            raise InlineUnsupportedError(
                call,
                f"Caller shadows callee globals: {', '.join(sorted(conflicts))}",
            )
        try:
            self.validate_flow(body, bound, local_names, call)
        except InlineUnsupportedError as error:
            raise InlineUnsupportedError(
                call,
                f"Callee {function.name} (defined at "
                f"{function.lineno}:{function.col_offset + 1}): "
                f"{error.diagnostic.message}",
            ) from error
        return body, local_names

    @staticmethod
    def validate_statement(
        statement: ast.stmt,
        bound: set[str],
        local_names: set[str],
        call: ast.Call,
    ) -> None:
        """Check supported statements and their reads before flow analysis."""
        if not isinstance(
            statement,
            (
                ast.Assign,
                ast.AnnAssign,
                ast.AugAssign,
                ast.Expr,
                ast.Pass,
                ast.Return,
                ast.If,
                ast.For,
                ast.While,
                ast.Break,
                ast.Continue,
                ast.Raise,
                ast.With,
                ast.Try,
            ),
        ):
            raise InlineUnsupportedError(
                call,
                f"Unsupported callee statement {type(statement).__name__} at "
                f"{statement.lineno}:{statement.col_offset + 1}; "
                "supported statements: local assignments, expressions, if, return, "
                "raise, with, try, and loops without returns",
            )
        expressions: list[ast.AST] = []
        if isinstance(statement, (ast.If, ast.While)):
            expressions = [statement.test]
        elif isinstance(statement, ast.For):
            expressions = [statement.iter]
        elif isinstance(statement, ast.Raise):
            expressions = [value for value in (statement.exc, statement.cause) if value]
        elif (
            isinstance(
                statement,
                (ast.Assign, ast.AnnAssign, ast.AugAssign, ast.Expr, ast.Return),
            )
            and statement.value is not None
        ):
            expressions = [statement.value]
        Inliner.validate_targets(statement, bound, call)
        for expression in expressions:
            if any(
                isinstance(
                    node,
                    (
                        ast.NamedExpr,
                        ast.ListComp,
                        ast.SetComp,
                        ast.DictComp,
                        ast.GeneratorExp,
                        ast.Lambda,
                    ),
                )
                for node in ast.walk(expression)
            ):
                raise InlineUnsupportedError(
                    call,
                    "Callee contains expression-local bindings or deferred execution",
                )
            reads = {
                node.id
                for node in ast.walk(expression)
                if isinstance(node, ast.Name) and isinstance(node.ctx, ast.Load)
            }
            if (reads & local_names) - bound:
                raise InlineUnsupportedError(
                    call,
                    "Callee may read a local before it is assigned",
                )

    @staticmethod
    def validate_targets(statement: ast.stmt, bound: set[str], call: ast.Call) -> None:
        """Allow local unpacking and reject loop returns and nonlocal stores."""
        targets = (
            statement.targets
            if isinstance(statement, ast.Assign)
            else [statement.target]
            if isinstance(statement, (ast.For, ast.AnnAssign, ast.AugAssign))
            else []
        )
        if any(
            not isinstance(
                node,
                (ast.Name, ast.Tuple, ast.List, ast.Starred, ast.Store),
            )
            for target in targets
            for node in ast.walk(target)
        ):
            raise InlineUnsupportedError(
                call,
                "Callee assignments must target local names",
            )
        if isinstance(statement, ast.AugAssign) and (
            not isinstance(statement.target, ast.Name)
            or statement.target.id not in bound
        ):
            raise InlineUnsupportedError(
                call,
                "Augmented assignment requires an initialized local",
            )
        if isinstance(statement, (ast.For, ast.While)) and any(
            isinstance(node, ast.Return) for node in ast.walk(statement)
        ):
            raise InlineUnsupportedError(
                call,
                "Returns inside callee loops are not supported",
            )

    def validate_try(
        self,
        statement: ast.Try,
        bound: set[str],
        local_names: set[str],
        call: ast.Call,
    ) -> None:
        """Validate exception regions without assuming a successful try body."""
        if any(isinstance(node, ast.Return) for node in ast.walk(statement)):
            raise InlineUnsupportedError(
                call,
                "Returns inside callee try require unwind analysis",
            )
        for handler in statement.handlers:
            if handler.name is not None:
                raise InlineUnsupportedError(
                    call,
                    "Exception target cleanup requires binding analysis",
                )
            if handler.type is not None:
                self.validate_statement(
                    ast.Expr(value=handler.type),
                    bound,
                    local_names,
                    call,
                )
                if self.has_call(handler.type):
                    raise InlineUnsupportedError(
                        call,
                        "Exception types with local calls require dispatch analysis",
                    )
            self.validate_flow(handler.body, bound, local_names, call)
        success = self.validate_flow(statement.body, bound, local_names, call)
        self.validate_flow(
            statement.orelse,
            bound if success is None else success,
            local_names,
            call,
        )
        self.validate_flow(statement.finalbody, bound, local_names, call)

    def validate_with(
        self,
        statement: ast.With,
        bound: set[str],
        local_names: set[str],
        call: ast.Call,
    ) -> None:
        """Check entry bindings without assuming exceptions propagate."""
        if any(isinstance(node, ast.Return) for node in ast.walk(statement)):
            raise InlineUnsupportedError(
                call,
                "Returns inside callee context managers require unwind analysis",
            )
        inner_bound = set(bound)
        for item in statement.items:
            probe = ast.Expr(value=item.context_expr)
            self.validate_statement(probe, inner_bound, local_names, call)
            if item.optional_vars is not None:
                assignment = ast.Assign(
                    targets=[item.optional_vars],
                    value=ast.Constant(value=None),
                )
                self.validate_targets(assignment, inner_bound, call)
                inner_bound.update(Checker.stores(item.optional_vars))
        self.validate_flow(statement.body, inner_bound, local_names, call)

    def validate_region(
        self,
        statement: ast.stmt,
        bound: set[str],
        local_names: set[str],
        call: ast.Call,
    ) -> None:
        """Validate regions whose exceptional exits prevent new binding guarantees."""
        if isinstance(statement, ast.With):
            self.validate_with(statement, bound, local_names, call)
        elif isinstance(statement, ast.Try):
            self.validate_try(statement, bound, local_names, call)

    def validate_flow(
        self,
        body: list[ast.stmt],
        bound: set[str],
        local_names: set[str],
        call: ast.Call,
    ) -> set[str] | None:
        """Prove local initialization on every reachable path through branches."""
        bound = set(bound)
        for statement in body:
            self.validate_statement(statement, bound, local_names, call)
            self.validate_region(statement, bound, local_names, call)
            if isinstance(statement, ast.Return):
                return None
            if isinstance(statement, ast.Assign):
                bound.update(
                    set().union(
                        *(Checker.stores(target) for target in statement.targets),
                    ),
                )
            if isinstance(statement, ast.AnnAssign) and statement.value is not None:
                bound.update(Checker.stores(statement.target))
            if isinstance(statement, (ast.For, ast.While)):
                loop_bound = bound | (
                    Checker.stores(statement.target)
                    if isinstance(statement, ast.For)
                    else set()
                )
                self.validate_flow(statement.body, loop_bound, local_names, call)
                self.validate_flow(statement.orelse, bound, local_names, call)
            if isinstance(statement, ast.If):
                yes = self.validate_flow(statement.body, bound, local_names, call)
                no = self.validate_flow(statement.orelse, bound, local_names, call)
                if yes is None and no is None:
                    return None
                merged = no if yes is None else yes if no is None else yes & no
                if merged is not None:
                    bound = merged
        return bound

    def lower_try(
        self,
        statement: ast.Try,
        caller_locals: set[str],
        result: ast.Name,
        done: ast.Name,
    ) -> list[ast.stmt]:
        """Retain native exception matching, reraising, and finalization."""
        replacement = copy.deepcopy(statement)
        replacement.body = self.lower_body(
            statement.body,
            caller_locals,
            result,
            done,
        ) or [ast.Pass()]
        replacement.orelse = self.lower_body(
            statement.orelse,
            caller_locals,
            result,
            done,
        )
        replacement.finalbody = self.lower_body(
            statement.finalbody,
            caller_locals,
            result,
            done,
        )
        if statement.finalbody and not replacement.finalbody:
            replacement.finalbody = [ast.Pass()]
        for handler in replacement.handlers:
            handler.body = self.lower_body(
                handler.body,
                caller_locals,
                result,
                done,
            ) or [ast.Pass()]
        return [replacement]

    def lower_with(
        self,
        statement: ast.With,
        caller_locals: set[str],
        result: ast.Name,
        done: ast.Name,
    ) -> list[ast.stmt]:
        """Nest entries so later expressions run inside earlier managers."""
        lowered = self.lower_body(
            statement.body,
            caller_locals,
            result,
            done,
        ) or [ast.Pass()]
        for item in reversed(statement.items):
            prefix, context = self.expression(item.context_expr, caller_locals)
            lowered = [
                *prefix,
                ast.With(
                    items=[
                        ast.withitem(
                            context_expr=context,
                            optional_vars=copy.deepcopy(item.optional_vars),
                        ),
                    ],
                    body=lowered,
                ),
            ]
        return lowered

    def lower_body(
        self,
        body: list[ast.stmt],
        caller_locals: set[str],
        result: ast.Name,
        done: ast.Name,
    ) -> list[ast.stmt]:
        """Guard statements after a return without introducing exception handlers."""
        statements: list[ast.stmt] = []
        for statement in body:
            lowered: list[ast.stmt] = []
            if isinstance(statement, ast.Pass) or (
                isinstance(statement, ast.AnnAssign) and statement.value is None
            ):
                continue
            if isinstance(statement, ast.If):
                lowered, test = self.expression(statement.test, caller_locals)
                lowered.append(
                    ast.If(
                        test=test,
                        body=self.lower_body(
                            statement.body,
                            caller_locals,
                            result,
                            done,
                        )
                        or [ast.Pass()],
                        orelse=self.lower_body(
                            statement.orelse,
                            caller_locals,
                            result,
                            done,
                        ),
                    ),
                )
            elif isinstance(statement, (ast.With, ast.Try)):
                lowered = (
                    self.lower_with(statement, caller_locals, result, done)
                    if isinstance(statement, ast.With)
                    else self.lower_try(statement, caller_locals, result, done)
                )
            elif isinstance(statement, (ast.For, ast.While)):
                lowered = self.lower_loop(statement, caller_locals, result, done)
            elif isinstance(statement, (ast.Break, ast.Continue)):
                lowered = [copy.deepcopy(statement)]
            elif isinstance(statement, ast.Raise):
                lowered = self.lower_raise(statement, caller_locals)
            elif isinstance(statement, ast.AugAssign):
                lowered = self.lower_augmented(statement, caller_locals)
            else:
                lowered = self.lower_value(statement, caller_locals, result, done)
            statements.append(
                ast.If(
                    test=ast.UnaryOp(op=ast.Not(), operand=copy.deepcopy(done)),
                    body=lowered,
                    orelse=[],
                ),
            )
            if isinstance(statement, ast.Return):
                break
        return statements

    def lower_value(
        self,
        statement: ast.stmt,
        caller_locals: set[str],
        result: ast.Name,
        done: ast.Name,
    ) -> list[ast.stmt]:
        """Lower a value and record a return without leaving the caller."""
        if not isinstance(statement, (ast.Assign, ast.AnnAssign, ast.Expr, ast.Return)):
            raise InlineUnsupportedError(statement, "Unsupported value statement")
        lowered, value = self.expression(
            statement.value
            if statement.value is not None
            else ast.Constant(value=None),
            caller_locals,
        )
        if isinstance(statement, ast.Return):
            lowered.extend(
                [
                    ast.Assign(
                        targets=[ast.Name(id=result.id, ctx=ast.Store())],
                        value=value,
                    ),
                    ast.Assign(
                        targets=[ast.Name(id=done.id, ctx=ast.Store())],
                        value=ast.Constant(value=True),
                    ),
                ],
            )
        else:
            replacement = (
                ast.Assign(
                    targets=[copy.deepcopy(statement.target)],
                    value=value,
                )
                if isinstance(statement, ast.AnnAssign)
                else copy.deepcopy(statement)
            )
            replacement.value = value
            lowered.append(replacement)
        return lowered

    def lower_augmented(
        self,
        statement: ast.AugAssign,
        caller_locals: set[str],
    ) -> list[ast.stmt]:
        """Read the local before the RHS and preserve in-place operator dispatch."""
        statements: list[ast.stmt] = []
        target = copy.deepcopy(statement.target)
        target.ctx = ast.Load()
        value = self.save(target, statements)
        prefix, right = self.expression(statement.value, caller_locals)
        statements.extend(prefix)
        statements.extend(
            [
                ast.AugAssign(
                    target=ast.Name(id=value.id, ctx=ast.Store()),
                    op=statement.op,
                    value=right,
                ),
                ast.Assign(targets=[copy.deepcopy(statement.target)], value=value),
            ],
        )
        return statements

    def lower_raise(
        self,
        statement: ast.Raise,
        caller_locals: set[str],
    ) -> list[ast.stmt]:
        """Evaluate the exception before its cause, including side effects."""
        statements: list[ast.stmt] = []
        values: list[ast.expr | None] = []
        for expression in (statement.exc, statement.cause):
            if expression is None:
                values.append(None)
            else:
                prefix, value = self.expression(expression, caller_locals)
                statements.extend(prefix)
                values.append(self.save(value, statements))
        statements.append(ast.Raise(exc=values[0], cause=values[1]))
        return statements

    def lower_loop(
        self,
        statement: ast.For | ast.While,
        caller_locals: set[str],
        result: ast.Name,
        done: ast.Name,
    ) -> list[ast.stmt]:
        """Retain loop control and else semantics when the body cannot return."""
        loop = copy.deepcopy(statement)
        loop.body = self.lower_body(statement.body, caller_locals, result, done) or [
            ast.Pass(),
        ]
        loop.orelse = self.lower_body(statement.orelse, caller_locals, result, done)
        if isinstance(loop, ast.For):
            prefix, loop.iter = self.expression(loop.iter, caller_locals)
            return [*prefix, loop]
        return self.lower_while(loop, caller_locals)

    def lower_while(
        self,
        loop: ast.While,
        caller_locals: set[str],
    ) -> list[ast.stmt]:
        """Repeat condition prefixes and distinguish exhaustion from body breaks."""
        prefix, test = self.expression(loop.test, caller_locals)
        if not prefix:
            return [loop]
        statements: list[ast.stmt] = []
        exhausted = self.save(ast.Constant(value=False), statements)
        finish = ast.Assign(
            targets=[ast.Name(id=exhausted.id, ctx=ast.Store())],
            value=ast.Constant(value=True),
        )
        statements.append(
            ast.While(
                test=ast.Constant(value=True),
                body=[
                    *prefix,
                    ast.If(
                        test=ast.UnaryOp(op=ast.Not(), operand=test),
                        body=[finish, ast.Break()],
                        orelse=[],
                    ),
                    *loop.body,
                ],
                orelse=[],
            ),
        )
        if loop.orelse:
            statements.append(ast.If(test=exhausted, body=loop.orelse, orelse=[]))
        return statements

    def bind_arguments(
        self,
        function: ast.FunctionDef,
        call: ast.Call,
        caller_locals: set[str],
        names: dict[str, str],
        statements: list[ast.stmt],
    ) -> None:
        """Evaluate actuals left to right and reuse the function's default objects."""
        args = function.args
        parameters = args.posonlyargs + args.args + args.kwonlyargs
        values: dict[str, ast.expr] = {}
        positional = args.posonlyargs + args.args
        for parameter, argument in zip(positional, call.args, strict=False):
            prefix, value = self.expression(argument, caller_locals)
            statements.extend(prefix)
            values[parameter.arg] = self.save(value, statements)
        for keyword in call.keywords:
            prefix, value = self.expression(keyword.value, caller_locals)
            statements.extend(prefix)
            if keyword.arg is not None:
                values[keyword.arg] = self.save(value, statements)
        for index, parameter in enumerate(positional):
            if parameter.arg not in values:
                values[parameter.arg] = ast.Subscript(
                    value=ast.Attribute(
                        value=copy.deepcopy(call.func),
                        attr="__defaults__",
                        ctx=ast.Load(),
                    ),
                    slice=ast.Constant(
                        value=index - (len(positional) - len(args.defaults)),
                    ),
                    ctx=ast.Load(),
                )
        for parameter in args.kwonlyargs:
            if parameter.arg not in values:
                values[parameter.arg] = ast.Subscript(
                    value=ast.Attribute(
                        value=copy.deepcopy(call.func),
                        attr="__kwdefaults__",
                        ctx=ast.Load(),
                    ),
                    slice=ast.Constant(value=parameter.arg),
                    ctx=ast.Load(),
                )
        statements.extend(
            ast.Assign(
                targets=[ast.Name(id=names[parameter.arg], ctx=ast.Store())],
                value=values[parameter.arg],
            )
            for parameter in parameters
        )

    def expand(
        self,
        call: ast.Call,
        caller_locals: set[str],
    ) -> tuple[list[ast.stmt], ast.expr]:
        """Bound nested expansion before constructing an oversized call body."""
        self.expansions += 1
        if self.expansions > self.max_expansions:
            raise InlineUnsupportedError(
                call,
                f"Expansion exceeds the {self.max_expansions}-call safety budget",
            )
        if self.depth >= self.max_depth:
            raise InlineUnsupportedError(
                call,
                f"Expansion exceeds the depth limit of {self.max_depth}",
            )
        self.depth += 1
        try:
            statements, result = self.expand_body(call, caller_locals)
            self.check_size(statements, call)
            return statements, result
        finally:
            self.depth -= 1

    def check_size(self, statements: list[ast.stmt], node: ast.AST) -> int:
        """Reject a replacement exceeding the configured statement budget."""
        count = sum(
            isinstance(child, ast.stmt)
            for statement in statements
            for child in ast.walk(statement)
        )
        if count > self.max_statements:
            raise InlineUnsupportedError(
                node,
                f"Expansion exceeds the {self.max_statements}-statement safety budget",
            )
        return count

    def expand_body(
        self,
        call: ast.Call,
        caller_locals: set[str],
    ) -> tuple[list[ast.stmt], ast.expr]:
        """Bind arguments once, rename local storage, and substitute a finite body."""
        if not isinstance(call.func, ast.Name):
            raise InlineUnsupportedError(call, "Expected a direct function call")
        function = self.functions[call.func.id]
        if any(isinstance(arg, ast.Starred) for arg in call.args) or any(
            keyword.arg is None for keyword in call.keywords
        ):
            raise InlineUnsupportedError(
                call,
                "Calls with argument unpacking require runtime binding",
            )
        args = function.args
        if args.vararg or args.kwarg:
            raise InlineUnsupportedError(call, "Variadic callees are not supported yet")
        body, local_names = self.validate_body(function, call, caller_locals)
        self.check_size(body, call)
        names = {name: self.fresh() for name in sorted(local_names)}
        statements: list[ast.stmt] = [
            ast.Expr(
                value=ast.Attribute(
                    value=copy.deepcopy(call.func),
                    attr="__name__",
                    ctx=ast.Load(),
                ),
            ),
        ]
        self.bind_arguments(function, call, caller_locals, names, statements)
        result = self.save(ast.Constant(value=None), statements)
        done = self.save(ast.Constant(value=False), statements)
        parameters = {
            parameter.arg
            for parameter in args.posonlyargs + args.args + args.kwonlyargs
        }
        statements.extend(
            ast.Assign(
                targets=[ast.Name(id=names[name], ctx=ast.Store())],
                value=ast.Constant(value=None),
            )
            for name in sorted(local_names - parameters)
        )
        renamed = [
            RenameLocals(names).visit(copy.deepcopy(statement)) for statement in body
        ]
        statements.extend(
            self.lower_body(renamed, caller_locals | set(names.values()), result, done),
        )
        if names:
            statements.append(
                ast.Delete(
                    targets=[
                        ast.Name(id=name, ctx=ast.Del()) for name in names.values()
                    ],
                ),
            )
        self.completed += 1
        return statements, result


class SourceEdits:
    """Retain untouched text while planning every call-site replacement."""

    def __init__(self, source: str, inliner: Inliner, *, strict: bool = True) -> None:
        """Index source lines once for byte-aware edits."""
        self.inliner = inliner
        self.strict = strict
        self.diagnostics: list[Diagnostic] = []
        self.edits: list[tuple[int, int, str]] = []
        self.generated = 0
        self.lines = io.StringIO(source, newline="").readlines()
        self.offsets = [0]
        for line in self.lines:
            self.offsets.append(self.offsets[-1] + len(line))

    def offset(self, line: int, column: int) -> int:
        """Translate AST UTF-8 byte columns into source string offsets."""
        return self.offsets[line - 1] + len(
            self.lines[line - 1].encode()[:column].decode(),
        )

    def replace_statement(
        self,
        statement: ast.Assign | ast.Expr | ast.Return,
        caller_locals: set[str],
    ) -> None:
        """Render only an affected statement, preserving its surrounding text."""
        if statement.value is None:
            return
        if isinstance(statement, ast.Assign) and any(
            self.inliner.has_call(target) for target in statement.targets
        ):
            raise InlineUnsupportedError(
                statement,
                "Calls in assignment targets are not supported",
            )
        prefix, value = self.inliner.expression(statement.value, caller_locals)
        if not prefix and ast.dump(value) == ast.dump(statement.value):
            return
        replacement = copy.deepcopy(statement)
        replacement.value = value
        self.record_replacement(statement, [*prefix, replacement])

    def record_replacement(self, statement: ast.stmt, body: list[ast.stmt]) -> None:
        """Render a complete statement replacement after reserving its budget."""
        self.reserve_size(body, statement)
        rendered = ast.unparse(
            ast.fix_missing_locations(
                ast.Module(body=body, type_ignores=[]),
            ),
        )
        start = self.offset(statement.lineno, statement.col_offset)
        end_line = statement.end_lineno or statement.lineno
        end_column = statement.end_col_offset or statement.col_offset
        end = self.offset(end_line, end_column)
        indent = self.lines[statement.lineno - 1][: statement.col_offset]
        if indent.strip() or self.lines[end_line - 1][
            len(
                self.lines[end_line - 1].encode()[:end_column].decode(),
            ) :
        ].lstrip().startswith(";"):
            raise InlineUnsupportedError(
                statement,
                "Expansion requires a statement on its own line",
            )
        newline = "\r\n" if self.lines[statement.lineno - 1].endswith("\r\n") else "\n"
        self.edits.append((start, end, rendered.replace("\n", newline + indent)))

    def replace_while(self, statement: ast.While, caller_locals: set[str]) -> None:
        """Combine nested source edits before replacing a repeated condition."""
        checkpoint = len(self.edits)
        generated = self.generated
        for child in [*statement.body, *statement.orelse]:
            self.visit(child, caller_locals)
        source = "".join(self.lines)
        for start, end, replacement in sorted(self.edits[checkpoint:], reverse=True):
            source = source[:start] + replacement + source[end:]
        loop = next(
            node
            for node in ast.walk(ast.parse(source))
            if isinstance(node, ast.While)
            and (node.lineno, node.col_offset)
            == (statement.lineno, statement.col_offset)
        )
        body = self.inliner.lower_while(loop, caller_locals)
        if body == [loop]:
            return
        del self.edits[checkpoint:]
        self.generated = generated
        self.record_replacement(statement, body)

    def reserve_size(self, statements: list[ast.stmt], node: ast.AST) -> None:
        """Apply the statement budget across all committed edits in the file."""
        count = self.inliner.check_size(statements, node)
        if self.generated + count > self.inliner.max_statements:
            raise InlineUnsupportedError(
                node,
                "File rewrites exceed the "
                f"{self.inliner.max_statements}-statement safety budget",
            )
        self.generated += count

    def replace_header(
        self,
        statement: ast.If | ast.For,
        caller_locals: set[str],
    ) -> str:
        """Expand once-evaluated headers without replacing their source bodies."""
        field = "test" if isinstance(statement, ast.If) else "iter"
        expression = getattr(statement, field)
        if not self.inliner.has_call(expression):
            return field
        start = self.offset(statement.lineno, statement.col_offset)
        expression_start = self.offset(expression.lineno, expression.col_offset)
        end = self.offset(expression.end_lineno, expression.end_col_offset)
        source = "".join(self.lines)
        indent = self.lines[statement.lineno - 1][: statement.col_offset]
        if indent.strip() or source[start:].startswith("elif"):
            raise InlineUnsupportedError(
                expression,
                "Header expansion requires an if or for statement on its own line",
            )
        prefix, value = self.inliner.expression(expression, caller_locals)
        if not prefix:
            return field
        self.reserve_size(prefix, statement)
        rendered = ast.unparse(
            ast.fix_missing_locations(ast.Module(body=prefix, type_ignores=[])),
        )
        newline = "\r\n" if self.lines[statement.lineno - 1].endswith("\r\n") else "\n"
        replacement = (
            rendered.replace("\n", newline + indent)
            + newline
            + indent
            + source[start:expression_start]
            + ast.unparse(ast.fix_missing_locations(value))
        )
        self.edits.append((start, end, replacement))
        return field

    def visit_function(self, statement: ast.FunctionDef) -> None:
        """Check headers separately and establish the caller's lexical bindings."""
        header = [
            *statement.args.defaults,
            *[value for value in statement.args.kw_defaults if value is not None],
        ]
        if any(self.inliner.has_call(value) for value in header):
            raise InlineUnsupportedError(
                statement,
                "Calls in function defaults are not supported yet",
            )
        scope = Checker.stores(statement) | {
            arg.arg
            for arg in statement.args.posonlyargs
            + statement.args.args
            + statement.args.kwonlyargs
        }
        scope.update(
            arg.arg
            for arg in (statement.args.vararg, statement.args.kwarg)
            if arg is not None
        )
        for child in statement.body:
            self.visit(child, scope)

    def visit_handler(
        self,
        handler: ast.ExceptHandler,
        caller_locals: set[str],
    ) -> None:
        """Keep handler type evaluation separate from its executable body."""
        if handler.type is not None and self.inliner.has_call(handler.type):
            raise InlineUnsupportedError(
                handler,
                "Calls in exception types are not supported",
            )
        for statement in handler.body:
            self.visit(statement, caller_locals)

    def visit(self, statement: ast.stmt, caller_locals: set[str]) -> None:
        """Keep unsupported statements intact without discarding independent edits."""
        checkpoint = len(self.edits)
        generated = self.generated
        completed = self.inliner.completed
        try:
            self.plan_statement(statement, caller_locals)
        except InlineUnsupportedError as error:
            if self.strict:
                raise
            del self.edits[checkpoint:]
            self.generated = generated
            self.inliner.completed = completed
            self.diagnostics.append(error.diagnostic)

    def plan_statement(self, statement: ast.stmt, caller_locals: set[str]) -> None:
        """Plan statement replacements without overlapping source spans."""
        if isinstance(statement, ast.ClassDef) and self.inliner.has_call(statement):
            raise InlineUnsupportedError(
                statement,
                "Calls in class namespaces require separate binding analysis",
            )
        if isinstance(statement, ast.FunctionDef):
            self.visit_function(statement)
            return
        if isinstance(statement, ast.While) and self.inliner.has_call(statement.test):
            self.replace_while(statement, caller_locals)
            return
        if (
            isinstance(statement, (ast.Assign, ast.Expr, ast.Return))
            and statement.value is not None
            and self.inliner.has_call(statement.value)
        ):
            self.replace_statement(statement, caller_locals)
            return
        header = (
            self.replace_header(statement, caller_locals)
            if isinstance(statement, (ast.If, ast.For))
            else None
        )
        self.visit_fields(statement, caller_locals, header)

    def visit_fields(
        self,
        statement: ast.stmt,
        caller_locals: set[str],
        header: str | None,
    ) -> None:
        """Visit nested statements while excluding an already expanded header."""
        for field, value in ast.iter_fields(statement):
            if field == header:
                continue
            children = value if isinstance(value, list) else [value]
            for child in children:
                if isinstance(child, ast.stmt):
                    self.visit(child, caller_locals)
                elif isinstance(child, ast.ExceptHandler):
                    self.visit_handler(child, caller_locals)
                elif isinstance(child, ast.AST) and self.inliner.has_call(child):
                    raise InlineUnsupportedError(
                        child,
                        f"Calls in {type(statement).__name__}.{field} "
                        "are not supported yet",
                    )


def namespace_hazard(tree: ast.Module, diagnostics: list[Diagnostic]) -> bool:
    """Keep namespace-wide hazards conservative even inside skipped definitions."""
    namespace = DYNAMIC | {
        "__dict__",
        "__globals__",
        "__builtins__",
        "f_globals",
        "f_locals",
    }
    dynamic_names = (
        DYNAMIC
        | {"__builtins__"}
        | {
            alias.asname or alias.name
            for node in ast.walk(tree)
            if isinstance(node, ast.ImportFrom) and node.module == "builtins"
            for alias in node.names
            if alias.name in namespace
        }
    )
    return any(d.code in {"dynamic", "import"} for d in diagnostics) or any(
        isinstance(node, ast.Nonlocal)
        or (isinstance(node, ast.Name) and node.id in dynamic_names)
        or (
            isinstance(node, ast.Attribute)
            and (
                node.attr in namespace
                or (
                    node.attr in {"__code__", "__defaults__", "__kwdefaults__"}
                    and not isinstance(node.ctx, ast.Load)
                )
            )
        )
        or (isinstance(node, ast.ImportFrom) and any(a.name == "*" for a in node.names))
        for node in ast.walk(tree)
    )


def lexical_local_uses(tree: ast.Module) -> set[int]:
    """Separate ordinary function locals from unrelated module function objects."""
    local_uses: set[int] = set()
    for function in tree.body:
        if not isinstance(function, ast.FunctionDef):
            continue
        bound = Checker.stores(function) | {
            arg.arg
            for arg in [
                *function.args.posonlyargs,
                *function.args.args,
                *function.args.kwonlyargs,
            ]
        }
        bound.update(
            arg.arg
            for arg in (function.args.vararg, function.args.kwarg)
            if arg is not None
        )
        bound.difference_update(
            name
            for node in ast.walk(function)
            if isinstance(node, (ast.Global, ast.Nonlocal))
            for name in node.names
        )
        pending: list[ast.AST] = list(function.body)
        while pending:
            node = pending.pop()
            if isinstance(
                node,
                (
                    ast.FunctionDef,
                    ast.AsyncFunctionDef,
                    ast.ClassDef,
                    ast.Lambda,
                    ast.ListComp,
                    ast.SetComp,
                    ast.DictComp,
                    ast.GeneratorExp,
                ),
            ):
                continue
            if isinstance(node, ast.Name) and node.id in bound:
                local_uses.add(id(node))
            pending.extend(ast.iter_child_nodes(node))
    return local_uses


def unavailable_functions(
    tree: ast.Module,
    functions: dict[str, ast.FunctionDef],
) -> set[str]:
    """Exclude escaped objects and declarations with competing bindings."""
    direct_targets = {
        id(node.func) for node in ast.walk(tree) if isinstance(node, ast.Call)
    }
    metadata_reads = function_metadata_reads(tree)
    direct_targets.update(
        id(node.value)
        for node in ast.walk(tree)
        if isinstance(node, ast.Attribute) and id(node) in metadata_reads
    )
    unavailable: set[str] = set()
    local_uses = lexical_local_uses(tree)
    for node in ast.walk(tree):
        if id(node) in local_uses:
            continue
        if isinstance(node, ast.Name) and (
            not isinstance(node.ctx, ast.Load) or id(node) not in direct_targets
        ):
            unavailable.add(node.id)
        elif (
            isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef))
            and functions.get(node.name) is not node
        ):
            unavailable.add(node.name)
        elif isinstance(node, ast.alias):
            unavailable.add(node.asname or node.name.split(".")[0])
        elif (
            isinstance(node, (ast.ExceptHandler, ast.MatchAs, ast.MatchStar))
            and node.name
        ):
            unavailable.add(node.name)
        elif isinstance(node, ast.MatchMapping) and node.rest:
            unavailable.add(node.rest)
    return unavailable


def partial_regions(
    tree: ast.Module,
    inliner: Inliner,
    diagnostics: list[Diagnostic],
) -> set[int]:
    """Isolate validated top-level regions and their unambiguous callees."""
    blocked: set[int] = set()
    uncertain = {
        name
        for node in ast.walk(tree)
        if isinstance(node, ast.Global)
        for name in node.names
    }
    uncertain.update(
        diagnostic.message.rsplit(": ", 1)[-1]
        for diagnostic in diagnostics
        if diagnostic.code == "binding"
    )
    for index, statement in enumerate(tree.body):
        start = min(
            [
                statement.lineno,
                *[d.lineno for d in getattr(statement, "decorator_list", [])],
            ],
        )
        if any(
            start <= d.line <= (statement.end_lineno or statement.lineno)
            for d in diagnostics
        ):
            blocked.add(index)
        if any(
            isinstance(node, ast.Name) and node.id in uncertain
            for node in ast.walk(statement)
        ):
            blocked.add(index)
    unavailable = unavailable_functions(tree, inliner.functions)
    unavailable.update(uncertain)
    for name in sorted(unavailable & inliner.functions.keys()):
        function = inliner.functions[name]
        diagnostics.append(
            Diagnostic(
                function.lineno,
                function.col_offset + 1,
                "partial",
                f"Function {name} escapes or has competing bindings; calls retained",
            ),
        )
    inliner.functions = {
        statement.name: statement
        for index, statement in enumerate(tree.body)
        if isinstance(statement, ast.FunctionDef)
        and index not in blocked
        and statement.name not in unavailable
    }
    return blocked


def validate_rewrite(
    source: str,
    filename: str,
    inliner: Inliner,
    *,
    strict: bool,
) -> list[Diagnostic]:
    """Validate the final transaction before allowing a filesystem replacement."""
    try:
        compile(source, filename, "exec", dont_inherit=True)
        if strict and inliner.has_call(ast.parse(source)):
            return [Diagnostic(1, 1, "inline", "Unexpanded local calls remain")]
    except (SyntaxError, RecursionError) as error:
        return [
            Diagnostic(1, 1, "inline", f"Generated source failed validation: {error}"),
        ]
    return []


def initialize_audit(
    source: str,
    diagnostics: list[Diagnostic],
    audit: dict[str, int],
) -> None:
    """Count syntactic candidates even when strict validation refuses a file."""
    audit.clear()
    audit.update(candidate_calls=0, rewritten_statements=0)
    if any(d.code == "parse" for d in diagnostics):
        return
    tree = ast.parse(source)
    functions = {node.name for node in tree.body if isinstance(node, ast.FunctionDef)}
    audit["candidate_calls"] = sum(
        isinstance(node, ast.Call)
        and isinstance(node.func, ast.Name)
        and node.func.id in functions
        for node in ast.walk(tree)
    )


def inline_source(
    source: str,
    filename: str = "<string>",
    *,
    strict: bool = True,
    audit: dict[str, int] | None = None,
    limits: tuple[int, int, int] = (MAX_EXPANSIONS, MAX_DEPTH, MAX_STATEMENTS),
) -> tuple[str, list[Diagnostic]]:
    """Plan compilable edits; strict mode retains the original all-or-nothing API."""
    original_source = source
    audit = {} if audit is None else audit
    diagnostics = check_source(source, filename)
    initialize_audit(source, diagnostics, audit)
    if diagnostics and (strict or any(d.code == "parse" for d in diagnostics)):
        return source, diagnostics
    tree = ast.parse(source)
    inliner = Inliner(tree, strict=strict, limits=limits)
    blocked: set[int] = set()
    if not strict:
        if namespace_hazard(tree, diagnostics):
            return source, sorted(
                {
                    *diagnostics,
                    Diagnostic(
                        1,
                        1,
                        "partial",
                        "Namespace mutation or ambiguous bindings "
                        "prevent safe partial inlining",
                    ),
                },
            )
        blocked = partial_regions(tree, inliner, diagnostics)
    planner = SourceEdits(source, inliner, strict=strict)
    try:
        for index, statement in enumerate(tree.body):
            if index in blocked:
                continue
            planner.visit(statement, set())
    except InlineUnsupportedError as error:
        return source, [error.diagnostic]
    except RecursionError:
        return source, [
            Diagnostic(1, 1, "inline", "Expansion exceeds the supported nesting depth"),
        ]
    for start, end, replacement in sorted(planner.edits, reverse=True):
        source = source[:start] + replacement + source[end:]
    if failures := validate_rewrite(source, filename, inliner, strict=strict):
        return original_source, failures
    audit["rewritten_statements"] = len(planner.edits)
    return source, sorted({*diagnostics, *planner.diagnostics, *inliner.diagnostics})


def rewrite_file(path: Path, original: bytes, updated: bytes) -> None:
    """Replace a regular file atomically without truncating it on failure."""
    if path.is_symlink() or path.stat().st_nlink != 1:
        msg = "Refusing to replace a symbolic link or multiply linked file"
        raise OSError(msg)
    temporary = None
    try:
        with tempfile.NamedTemporaryFile(
            dir=path.parent,
            prefix=f".{path.name}.",
            delete=False,
        ) as stream:
            temporary = Path(stream.name)
            stream.write(updated)
            stream.flush()
            os.fchmod(stream.fileno(), path.stat().st_mode & 0o777)
        if path.read_bytes() != original:
            msg = "Source changed while planning edits"
            raise OSError(msg)
        temporary.replace(path)
    finally:
        if temporary is not None:
            temporary.unlink(missing_ok=True)


def main() -> None:
    """Validate and autofix supported files, or report changes with --check."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("file", type=Path)
    for option, default, help_text in (
        ("--max-expansions", MAX_EXPANSIONS, "maximum attempted call expansions"),
        ("--max-depth", MAX_DEPTH, "maximum nested expansion depth"),
        ("--max-statements", MAX_STATEMENTS, "maximum generated statements per file"),
    ):
        parser.add_argument(option, type=int, default=default, help=help_text)
    parser.add_argument(
        "--summary",
        action="store_true",
        help="summarize local-call candidates, rewritten statements, and blockers",
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="validate and report required edits without writing",
    )
    parser.add_argument(
        "--strict",
        action="store_true",
        help="reject the entire file if any construct or call cannot be inlined",
    )
    parser.epilog = (
        "By default, supported statements are rewritten "
        "and unsupported code is retained. "
        "Exit status: 0 complete, 1 skipped code or pending --check edits, "
        "2 input/parse error."
    )
    args = parser.parse_args()
    changed = False
    audit: dict[str, int] = {}
    try:
        original = args.file.read_bytes()
        encoding, _ = tokenize.detect_encoding(io.BytesIO(original).readline)
        source = original.decode(encoding)
        updated, diagnostics = inline_source(
            source,
            str(args.file),
            strict=args.strict,
            audit=audit,
            limits=(args.max_expansions, args.max_depth, args.max_statements),
        )
        changed = updated != source
        if changed and not args.check:
            rewrite_file(args.file, original, updated.encode(encoding))
    except (OSError, UnicodeError, LookupError, SyntaxError, ValueError) as error:
        diagnostics = [Diagnostic(1, 1, "input", str(error))]
        changed = False
        audit["rewritten_statements"] = 0
    for diagnostic in diagnostics:
        sys.stderr.write(
            f"{args.file}:{diagnostic.line}:{diagnostic.column}: "
            f"{diagnostic.code}: {diagnostic.message}\n",
        )
    if changed or not diagnostics:
        label = (
            "Would inline" if changed and args.check else "Inlined" if changed else "OK"
        )
        sys.stdout.write(f"{label} {args.file}\n")
    if args.summary:
        sys.stdout.write(
            f"Summary: {audit.get('candidate_calls', 0)} "
            "syntactic local-call candidates; "
            f"{audit.get('rewritten_statements', 0)} statements "
            f"{'planned' if args.check else 'rewritten'}; "
            f"{len(diagnostics)} diagnostics\n",
        )
        groups: dict[tuple[str, str], list[Diagnostic]] = {}
        for diagnostic in diagnostics:
            key = (diagnostic.code, diagnostic.message)
            groups.setdefault(key, []).append(diagnostic)
        for (code, message), sites in sorted(groups.items()):
            locations = ", ".join(f"{site.line}:{site.column}" for site in sites)
            sys.stdout.write(f"  {code}: {message} ({len(sites)} sites: {locations})\n")
    sys.exit(
        2
        if any(d.code in {"input", "parse"} for d in diagnostics)
        else int(bool(diagnostics) or (changed and args.check)),
    )


if __name__ == "__main__":
    main()
