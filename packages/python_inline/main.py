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
import subprocess
import sys
import tempfile
import tokenize
import warnings
from dataclasses import dataclass
from pathlib import Path

MAX_EXPANSIONS = 1000
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

    def __init__(self, tree: ast.Module, *, strict: bool = True) -> None:
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
        if isinstance(node, ast.ListComp):
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
    def comprehension_bindings(node: ast.ListComp) -> set[str]:
        """Reject deferred scopes and reads of uninitialized iteration variables."""
        if any(generator.is_async for generator in node.generators) or any(
            isinstance(
                child,
                (
                    ast.NamedExpr,
                    ast.Lambda,
                    ast.GeneratorExp,
                    ast.SetComp,
                    ast.DictComp,
                ),
            )
            or (isinstance(child, ast.ListComp) and child is not node)
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
        node: ast.ListComp,
        caller_locals: set[str],
    ) -> tuple[list[ast.stmt], ast.expr]:
        """Lower eager loops and filters with private iteration bindings."""
        bound = self.comprehension_bindings(node)
        names = {name: self.fresh() for name in sorted(bound)}
        rename = RenameLocals(names)
        statements, iterable = self.expression(node.generators[0].iter, caller_locals)
        iterable = self.save(iterable, statements)
        result = self.save(ast.List(elts=[], ctx=ast.Load()), statements)
        scope = caller_locals | set(names.values())
        statements.extend(
            ast.Assign(
                targets=[ast.Name(id=name, ctx=ast.Store())],
                value=ast.Constant(value=None),
            )
            for name in names.values()
        )
        body, value = self.expression(rename.visit(copy.deepcopy(node.elt)), scope)
        body.append(
            ast.Expr(
                value=ast.Call(
                    func=ast.Attribute(
                        value=copy.deepcopy(result),
                        attr="append",
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
        self.validate_flow(body, bound, local_names, call)
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
            (ast.Assign, ast.Expr, ast.Pass, ast.Return, ast.If),
        ):
            raise InlineUnsupportedError(
                call,
                "Callee control flow supports assignments, expressions, if, and return",
            )
        expressions: list[ast.AST] = []
        if isinstance(statement, ast.If):
            expressions = [statement.test]
        elif (
            isinstance(statement, (ast.Assign, ast.Expr, ast.Return))
            and statement.value is not None
        ):
            expressions = [statement.value]
        if isinstance(statement, ast.Assign) and any(
            not isinstance(target, ast.Name) for target in statement.targets
        ):
            raise InlineUnsupportedError(
                call,
                "Callee assignments must target simple local names",
            )
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
            if isinstance(statement, ast.Return):
                return None
            if isinstance(statement, ast.Assign):
                bound.update(
                    target.id
                    for target in statement.targets
                    if isinstance(target, ast.Name)
                )
            if isinstance(statement, ast.If):
                yes = self.validate_flow(statement.body, bound, local_names, call)
                no = self.validate_flow(statement.orelse, bound, local_names, call)
                if yes is None and no is None:
                    return None
                merged = no if yes is None else yes if no is None else yes & no
                if merged is not None:
                    bound = merged
        return bound

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
            if isinstance(statement, ast.Pass):
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
            elif isinstance(statement, (ast.Assign, ast.Expr, ast.Return)):
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
                    replacement = copy.deepcopy(statement)
                    replacement.value = value
                    lowered.append(replacement)
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
        """Bind arguments once, rename local storage, and substitute a finite body."""
        self.expansions += 1
        if self.expansions > MAX_EXPANSIONS:
            raise InlineUnsupportedError(
                call,
                "Expansion exceeds the 1000-call safety budget",
            )
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
        rendered = ast.unparse(
            ast.fix_missing_locations(
                ast.Module(body=[*prefix, replacement], type_ignores=[]),
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
        try:
            self.plan_statement(statement, caller_locals)
        except InlineUnsupportedError as error:
            if self.strict:
                raise
            del self.edits[checkpoint:]
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
        if (
            isinstance(statement, (ast.Assign, ast.Expr, ast.Return))
            and statement.value is not None
            and self.inliner.has_call(statement.value)
        ):
            self.replace_statement(statement, caller_locals)
            return
        for _, value in ast.iter_fields(statement):
            children = value if isinstance(value, list) else [value]
            for child in children:
                if isinstance(child, ast.stmt):
                    self.visit(child, caller_locals)
                elif isinstance(child, ast.ExceptHandler):
                    self.visit_handler(child, caller_locals)
                elif isinstance(child, ast.AST) and self.inliner.has_call(child):
                    raise InlineUnsupportedError(
                        child,
                        "Calls in this statement context are not supported yet",
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
    return any(d.code in {"binding", "dynamic", "import"} for d in diagnostics) or any(
        isinstance(node, (ast.Global, ast.Nonlocal))
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
    for node in ast.walk(tree):
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
    unavailable = unavailable_functions(tree, inliner.functions)
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


def inline_source(
    source: str,
    filename: str = "<string>",
    *,
    strict: bool = True,
) -> tuple[str, list[Diagnostic]]:
    """Plan compilable edits; strict mode retains the original all-or-nothing API."""
    original_source = source
    diagnostics = check_source(source, filename)
    if diagnostics and (strict or any(d.code == "parse" for d in diagnostics)):
        return source, diagnostics
    tree = ast.parse(source)
    inliner = Inliner(tree, strict=strict)
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
    try:
        original = args.file.read_bytes()
        encoding, _ = tokenize.detect_encoding(io.BytesIO(original).readline)
        source = original.decode(encoding)
        updated, diagnostics = inline_source(source, str(args.file), strict=args.strict)
        changed = updated != source
        if changed and not args.check:
            rewrite_file(args.file, original, updated.encode(encoding))
    except (OSError, UnicodeError, LookupError, SyntaxError, ValueError) as error:
        diagnostics = [Diagnostic(1, 1, "input", str(error))]
        changed = False
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
    sys.exit(
        2
        if any(d.code in {"input", "parse"} for d in diagnostics)
        else int(bool(diagnostics) or (changed and args.check)),
    )


def test_inline_behavior() -> None:
    """Compare observable values, side effects, and failures before and after."""
    cases = [
        (
            "def square(x):\n"
            "    return x*x\n"
            "def calculate(x):\n"
            "    return square(x)+1\n"
            "result=calculate(3)\n"
        ),
        (
            "events=[]\n"
            "def mark(x):\n"
            "    events.append(x)\n"
            "    return x\n"
            "result=mark('left')+mark('right')\n"
        ),
        (
            "import operator\n"
            "events=[]\n"
            "def mark(x):\n"
            "    events.append(x)\n"
            "    return x\n"
            "result=operator.add(mark(2),mark(3))\n"
        ),
        (
            "events=[]\n"
            "def mark(x):\n"
            "    events.append(x)\n"
            "    return x\n"
            "def pair(a,/,b,*,c):\n"
            "    return (a,b,c)\n"
            "result=pair(mark(1),c=mark(3),b=mark(2))\n"
        ),
        (
            "def add(x,b=[]):\n"
            "    b.append(x)\n"
            "    return b\n"
            "a=add(1)\n"
            "b=add(2)\n"
            "result=(a is b,b)\n"
        ),
        "def f(a=2,*,b=3):\n    return a+b\nresult=f()\n",
        (
            "events=[]\n"
            "def mark(x):\n"
            "    events.append(x)\n"
            "    return x\n"
            "result=mark(1) if False else mark(2)\n"
        ),
        (
            "def f(x):\n"
            "    y=x+1\n"
            "    z=y*2\n"
            "    return z\n"
            "result=[]\n"
            "for x in [1,2,3]:\n"
            "    result.append(f(x))\n"
        ),
        "_inline_1=40\ndef f(x):\n    return x+_inline_1\nresult=f(2)\n",
        "def f(x):\n    pass\nresult=f(1)\n",
        "def f():\n    return\nresult=f()\n",
        "def f(x):\n    return x+1\ndef g(x):\n    return f(x)\nresult=[g(1),g(2)]\n",
        "def f(x):\n    return 1/x\nresult=f(0)\n",
        "events=[]\ndef f(x):\n    return x\nresult=f(events.append(1))\n",
        "events=[]\nresult=f(events.append(1))\ndef f(x):\n    return x\n",
    ]
    for source in cases:
        updated, diagnostics = inline_source(source)
        if diagnostics or updated == source:
            raise AssertionError((source, diagnostics))
        observed = []
        for program in (source, updated):
            namespace: dict[str, object] = {}
            failure = None
            try:
                exec(program, namespace)  # noqa: S102
            except (ZeroDivisionError, NameError) as error:
                failure = (type(error).__name__, str(error))
            observed.append((namespace.get("result"), namespace.get("events"), failure))
        if observed[0] != observed[1]:
            raise AssertionError((source, updated, observed))
        second, diagnostics = inline_source(updated)
        if diagnostics or second != updated:
            raise AssertionError((updated, diagnostics))
        if Inliner(ast.parse(source)).has_call(ast.parse(updated)):
            raise AssertionError(updated)


def test_inline_branches_and_comprehensions() -> None:
    """Preserve branching, early exit, iteration scope, formatting, and effects."""
    cases = [
        "def f(x):\n    if x:\n        return 1\n    return 2\nresult=[f(0), f(1)]\n",
        (
            "events=[]\n"
            "def f(x):\n"
            "    if x < 0:\n"
            "        return\n"
            "    events.append(x)\n"
            "    if x:\n"
            "        y=2\n"
            "    else:\n"
            "        y=3\n"
            "    return y\n"
            "result=[f(-1),f(0),f(1)]\n"
        ),
        (
            "def f(x):\n"
            "    if x:\n"
            "        return 1\n"
            "    else:\n"
            "        y=2\n"
            "    return y\n"
            "result=[f(0),f(1)]\n"
        ),
        "def f(x):\n    return x+1\nx=99\nresult=([f(x) for x in [1,2,3]],x)\n",
        "def f(x):\n    return x+1\nresult=[f(x) for x in []]\n",
        (
            "events=[]\n"
            "def f(x):\n"
            "    events.append(x)\n"
            "    return x\n"
            "result=[f((x,y)) for x in [1,2] if f(x) > 1 for y in [3,4] if f(y) > 3]\n"
        ),
        "def f(x):\n    return x\nresult=[f(a+b) for a,b in [(1,2),(3,4)]]\n",
        "x=40\ndef f(y):\n    return x+y\nresult=[f(x) for x in [1,2]]\n",
        "def f(x):\n    return x\nx=[1,2]\nresult=[f(x) for x in x]\n",
        (
            "events=[]\n"
            "def f(x):\n"
            "    events.append(x)\n"
            "    return x\n"
            "result=[f'{f(x)}:{f(x+1):03d}' for x in [1,2]]\n"
        ),
        (
            "events=[]\n"
            "def f(x):\n"
            "    events.append(x)\n"
            "    return x\n"
            "result=(f(1), *f([2,3]), f(4))\n"
        ),
        "def f(x):\n    return x\nresult=f'{f(3):{f(4)}d}'\n",
        (
            "def f(x):\n"
            "    if x:\n"
            "        if x > 1:\n"
            "            return 3\n"
            "        return 2\n"
            "    return 1\n"
            "result=[f(x) for x in [0,1,2]]\n"
        ),
        "def f(x):\n    return 1/x\nresult=[f(x) for x in [1,0]]\n",
    ]
    for source in cases:
        updated, diagnostics = inline_source(source)
        if diagnostics or updated == source:
            raise AssertionError((source, diagnostics))
        observed = []
        for program in (source, updated):
            namespace: dict[str, object] = {}
            failure = None
            try:
                exec(program, namespace)  # noqa: S102
            except ZeroDivisionError as error:
                failure = (type(error).__name__, str(error))
            observed.append(
                (
                    namespace.get("result"),
                    namespace.get("events"),
                    namespace.get("x"),
                    failure,
                ),
            )
        if observed[0] != observed[1]:
            raise AssertionError((source, updated, observed))
        second, diagnostics = inline_source(updated)
        if diagnostics or second != updated:
            raise AssertionError((updated, diagnostics))


def test_inline_format_conversion_order() -> None:
    """Convert values before expanding their format specs, including failures."""
    for conversion in ("s", "r", "a"):
        for failure in (False, True):
            source = (
                "events=[]\n"
                "class C:\n"
                "    def __str__(self):\n"
                "        events.append('str')\n"
                "        return self.__repr__()\n"
                "    def __repr__(self):\n"
                "        events.append('repr')\n"
                + (
                    "        raise ValueError('conversion')\n"
                    if failure
                    else "        return 'é'\n"
                )
                + "def spec():\n"
                "    events.append('spec')\n"
                "    return ''\n"
                f"result=f'{{C()!{conversion}:{{spec()}}}}'\n"
            )
            updated, diagnostics = inline_source(source, strict=False)
            if updated == source or any(d.code != "class" for d in diagnostics):
                raise AssertionError((updated, diagnostics))
            observed = []
            for program in (source, updated):
                namespace: dict[str, object] = {}
                error = None
                try:
                    exec(program, namespace)  # noqa: S102
                except ValueError as exception:
                    error = str(exception)
                observed.append((namespace.get("result"), namespace["events"], error))
            if observed[0] != observed[1]:
                raise AssertionError((source, updated, observed))
    source = (
        "def spec():\n    return ''\ndef render(repr):\n    return f'{1!r:{spec()}}'\n"
    )
    updated, diagnostics = inline_source(source, strict=False)
    if updated != source or not diagnostics:
        raise AssertionError((updated, diagnostics))


def test_mutable_keyword_defaults() -> None:
    """Retain calls when default keys can disappear or their dictionary escapes."""
    for mutation in (
        "f.__kwdefaults__.clear()",
        "del f.__kwdefaults__['x']",
        "defaults=f.__kwdefaults__\ndefaults.clear()",
    ):
        source = f"def f(*, x=1):\n    return x\n{mutation}\nresult=f()\n"
        for strict in (False, True):
            updated, diagnostics = inline_source(source, strict=strict)
            if updated != source or not diagnostics:
                raise AssertionError((source, updated, diagnostics))
    source = (
        "def f(*, x=[]):\n    return x\nf.__kwdefaults__['x'].append(1)\nresult=f()\n"
    )
    updated, diagnostics = inline_source(source)
    if updated == source or diagnostics:
        raise AssertionError((updated, diagnostics))
    namespace: dict[str, object] = {}
    exec(updated, namespace)  # noqa: S102
    if namespace["result"] != [1]:
        raise AssertionError(namespace["result"])
    second, diagnostics = inline_source(updated)
    if second != updated or diagnostics:
        raise AssertionError((second, diagnostics))


def test_source_line_boundaries() -> None:
    """Use Python physical lines rather than Unicode text line boundaries."""
    for separator in ("\v", "\f", "\x85", "\u2028", "\u2029"):
        for newline in ("\n", "\r\n", "\r"):
            source = (
                f"label='a{separator}b'\ndef f():\n    return 2\nresult=f()\n"
            ).replace("\n", newline)
            updated, diagnostics = inline_source(source)
            if updated == source or diagnostics:
                raise AssertionError((source, updated, diagnostics))
            namespace: dict[str, object] = {}
            exec(updated, namespace)  # noqa: S102
            expected = 2
            if (
                namespace["result"] != expected
                or namespace["label"] != f"a{separator}b"
            ):
                raise AssertionError(namespace)


def test_partial_inlining() -> None:
    """Commit independent safe statements while retaining diagnosed constructs."""
    sources = [
        "def f(x):\n    return x+1\na=f(1)\nb=f(*[2])\nresult=(a,b)\n",
        "def f(x):\n    return x+1\na=f(1)\nb=False and f(2)\nresult=(a,b)\n",
        (
            "def f(x):\n"
            "    return x+1\n"
            "class C:\n"
            "    def method(self):\n"
            "        return 4\n"
            "result=(f(1),C().method())\n"
        ),
        (
            "def f(x):\n"
            "    return x+1\n"
            "def recurse(x):\n"
            "    if x:\n"
            "        return recurse(x-1)\n"
            "    return 0\n"
            "result=(f(1),recurse(2))\n"
        ),
        (
            "def f(x):\n"
            "    return x+1\n"
            "def bad(x):\n"
            "    for y in [x]:\n"
            "        return y\n"
            "a=bad(2)\n"
            "result=(f(1),a)\n"
        ),
    ]
    for source in sources:
        updated, diagnostics = inline_source(source, strict=False)
        if not diagnostics or source == updated:
            raise AssertionError((source, updated, diagnostics))
        values = []
        for program in (source, updated):
            namespace: dict[str, object] = {}
            exec(program, namespace)  # noqa: S102
            values.append(namespace["result"])
        if values[0] != values[1]:
            raise AssertionError((source, updated, values))
        second, _ = inline_source(updated, strict=False)
        if second != updated:
            raise AssertionError((updated, second))
        strict_source, strict_diagnostics = inline_source(source, strict=True)
        if strict_source != source or not strict_diagnostics:
            raise AssertionError((strict_source, strict_diagnostics))


def test_partial_expression_siblings() -> None:
    """Expand safe siblings and arguments of retained calls exactly once."""
    for expression in (
        "(bad(1), good(2))",
        "(good(1), bad(2))",
        "bad(good(2))",
        "(good(1), good(*[2]))",
        "(good(1), False and good(2))",
        "(good(1), {good(x) for x in [2]})",
    ):
        source = (
            "events=[]\n"
            "def good(x):\n    events.append(x)\n    return x+1\n"
            "def bad(x):\n    for y in [x]:\n        events.append(y)\n"
            "    return x\n"
            f"result={expression}\n"
        )
        updated, diagnostics = inline_source(source, strict=False)
        if updated == source or not diagnostics:
            raise AssertionError((updated, diagnostics))
        outcomes = []
        for program in (source, updated):
            namespace: dict[str, object] = {}
            exec(program, namespace)  # noqa: S102
            outcomes.append((namespace["result"], namespace["events"]))
        if outcomes[0] != outcomes[1]:
            raise AssertionError((updated, outcomes))
        second, errors = inline_source(updated, strict=False)
        if second != updated or not errors:
            raise AssertionError((updated, second, errors))


def test_inline_subscript_order() -> None:
    """Preserve subscript operands and slice evaluation order."""
    for expression in (
        "[10,20,30][f(1)]",
        "[10,20,30][f(0):f(3):f(2)]",
    ):
        source = (
            "events=[]\n"
            "def f(x):\n    events.append(x)\n    return x\n"
            f"result={expression}\n"
        )
        updated, diagnostics = inline_source(source)
        if diagnostics or updated == source:
            raise AssertionError((updated, diagnostics))
        outcomes = []
        for program in (source, updated):
            namespace: dict[str, object] = {}
            exec(program, namespace)  # noqa: S102
            outcomes.append((namespace["result"], namespace["events"]))
        if outcomes[0] != outcomes[1]:
            raise AssertionError((updated, outcomes))


def test_partial_safety_barriers() -> None:
    """Do not speculate about mutable namespaces or escape-sensitive scopes."""
    sources = [
        "def f(x):\n    return x\nf=print\nf(1)\n",
        "def f(x):\n    return x\neval('1')\nresult=f(1)\n",
        (
            "def f(x):\n"
            "    return x\n"
            "class C:\n"
            "    def m(self):\n"
            "        global f\n"
            "        f=print\n"
            "result=f(1)\n"
        ),
        "def f(x):\n    return x\nfrom math import *\nresult=f(1)\n",
        "def f(x):\n    return x\ncallback=f\nresult=f(1)\n",
        "def f(x):\n    return x\nresult=[f(x) for x in [1] if y for y in [2]]\n",
        "def f(x):\n    if x:\n        y=1\n    return y\nresult=f(0)\n",
        "def f(x):\n    return x\nresult=[f(x) for x in [1] for y in y]\n",
        "def f(x):\n    return x\nresult=[f(x) for x in [1] if (y:=x)]\n",
        (
            "def f(x):\n    return x\n"
            "if True:\n    def f(x):\n        return x+10\n"
            "result=f(1)\n"
        ),
        (
            "def f(x):\n    return x\n"
            "match print:\n    case f:\n        pass\n"
            "result=f(1)\n"
        ),
        (
            "from builtins import exec as run\n"
            "def f(x):\n    return x\n"
            "class C:\n    def method(self):\n        run('pass')\n"
            "result=f(1)\n"
        ),
        (
            "import builtins as b\n"
            "def f(x):\n    return x\n"
            "class C:\n    def method(self):\n        b.eval('1')\n"
            "result=f(1)\n"
        ),
    ]
    for source in sources:
        updated, diagnostics = inline_source(source, strict=False)
        if updated != source or not diagnostics:
            raise AssertionError((source, updated, diagnostics))


def test_partial_cli() -> None:
    """Preview partial edits, write them with diagnostics, and preserve strict mode."""
    executable = os.environ["PACKAGE_E2E_EXECUTABLE"]
    source = b"def f(x):\n    return x+1\nresult=(f(1),f(*[2]))\n"
    with tempfile.TemporaryDirectory() as directory:
        path = Path(directory) / "input.py"
        for arguments, changed, label in [
            (["--check"], False, "Would inline"),
            (["--strict"], False, ""),
            ([], True, "Inlined"),
        ]:
            path.write_bytes(source)
            result = subprocess.run(  # noqa: S603
                [executable, *arguments, str(path)],
                capture_output=True,
                text=True,
                check=False,
            )
            if (
                result.returncode != 1
                or not result.stderr
                or (path.read_bytes() != source) != changed
                or (label and label not in result.stdout)
            ):
                raise AssertionError(result)


def test_inline_refusals_are_transactional() -> None:
    """Never return partial edits after finding an unsupported call site."""
    cases = [
        "def f(x):\n    return x\na=f(1)\nb=f(*[2])\n",
        "def f(x):\n    return x\na=f(1)\nb=False and f(2)\n",
        "def f(x):\n    return x\nwhile f(False):\n    pass\n",
        "x=1\ndef f():\n    return x\ndef g(x):\n    return f()\na=g(2)\n",
        "def f():\n    y=x\n    x=1\n    return y\na=f()\n",
        "def f(*args):\n    return args\na=f(1)\n",
        "def f():\n    return f()\na=f()\n",
        "def f(x):\n    return x\na=f(1); b=2\n",
        "def f(x):\n    return x\nif True: a=f(1)\n",
        "def f():\n    return 1\ndef g(x=f()):\n    return x\na=g()\n",
        "def f():\n    x=[i for i in [1]]\n    return x\na=f()\n",
        "def f():\n    return 1\na={f(): 2}\n",
        ("x=1\ndef f():\n    return x\nclass C:\n    x=2\n    y=f()\n"),
    ]
    for source in cases:
        updated, diagnostics = inline_source(source)
        if not diagnostics or updated != source:
            raise AssertionError((source, updated, diagnostics))


def test_inline_source_preservation() -> None:
    """Retain shebangs, encoding headers, Unicode offsets, and unrelated comments."""
    source = (
        "#!/usr/bin/env python3\n# coding: utf-8\n# Keep this comment\n"
        "def f(x):\n    return x+1  # Keep the declaration\n"
        "résultat=f(2)  # Keep the caller comment\n# Keep the footer\n"
    )
    updated, diagnostics = inline_source(source)
    if diagnostics:
        raise AssertionError(diagnostics)
    for comment in (
        "#!/usr/bin/env python3",
        "# coding: utf-8",
        "# Keep this comment",
        "# Keep the declaration",
        "# Keep the caller comment",
        "# Keep the footer",
    ):
        if comment not in updated:
            raise AssertionError((comment, updated))
    namespace: dict[str, object] = {}
    exec(updated, namespace)  # noqa: S102
    expected = 3
    if namespace["résultat"] != expected:
        raise AssertionError(namespace)


def test_inline_cli() -> None:
    """Exercise preview, automatic validation, idempotence, and refused writes."""
    executable = os.environ["PACKAGE_E2E_EXECUTABLE"]
    with tempfile.TemporaryDirectory() as directory:
        path = Path(directory) / "source.py"
        original = (
            b"# coding: latin-1\n# caf\xe9\ndef f(x):\n    return x+1\nresult=f(2)\n"
        )
        path.write_bytes(original)
        mode = 0o751
        path.chmod(mode)
        for arguments, status, changes, label in [
            (["--check"], 1, False, "Would inline"),
            ([], 0, True, "Inlined"),
            (["--check"], 0, True, "OK"),
            ([], 0, True, "OK"),
        ]:
            completed = subprocess.run(  # noqa: S603
                [executable, *arguments, str(path)],
                capture_output=True,
                text=True,
                check=False,
            )
            if (
                completed.returncode != status
                or (path.read_bytes() != original) != changes
                or completed.stdout != f"{label} {path}\n"
            ):
                raise AssertionError(completed)
        if path.stat().st_mode & 0o777 != mode or b"# caf\xe9" not in path.read_bytes():
            msg = "File permissions or encoding changed"
            raise AssertionError(msg)
        refused = b"def f(x):\n    return x\na=f(1)\nb=f(*[2])\n"
        path.write_bytes(refused)
        completed = subprocess.run(  # noqa: S603
            [executable, "--strict", str(path)],
            capture_output=True,
            text=True,
            check=False,
        )
        if completed.returncode != 1 or path.read_bytes() != refused:
            raise AssertionError(completed)
        path.write_bytes(original)
        link = path.with_name("link.py")
        link.symlink_to(path)
        completed = subprocess.run(  # noqa: S603
            [executable, str(link)],
            capture_output=True,
            text=True,
            check=False,
        )
        input_error = 2
        if (
            completed.returncode != input_error
            or path.read_bytes() != original
            or not link.is_symlink()
        ):
            raise AssertionError(completed)


def test_profile() -> None:
    """Exercise accepted procedural bodies and every excluded syntax family."""
    accepted = [
        "",
        "x = 1\nx = x + 2",
        "a, b = (1, 2)",
        "def f(a, /, b, *, c):\n return a+b+c\nx=f(1,c=3,b=2)",
        "def a(x):\n return x+1\ndef b(x):\n y=a(x)\n y=a(y)\n return y\nz=b(2)",
        (
            "def f(x):\n while x:\n  x=x-1\n  if x == 2:\n   continue\n"
            "  if x == 1:\n   break\n return x\ny=f(3)"
        ),
        "def f():\n pass\nx=f()",
        "def f():\n return\nx=f()",
        "def f(x):\n return x\nx=False and f(1)\ny=f(2) if x else f(3)",
        "import math as m\nfrom math import sqrt as root\nx=m.sin(root(4))",
        "def f(xs):\n total=0\n for x in xs:\n  total += x\n return total\nx=f([1,2])",
        "x={'a': [1,2]}\ny=x['a'][0]\nz={1,2}\na=(1 < 2 < 3)",
    ]
    accepted.extend(
        [
            "def f(x):\n if x:\n  return 1\n return 2",
            "def f():\n return 1\n pass",
            "x=1\ndef f():\n return x",
            "def f():\n return x\nx=1",
            "x=[]\nx.append(1)",
            "def f(x=1):\n pass",
            "def f(*x):\n pass",
            "x=1 # type: int",
            "try:\n pass\nexcept:\n pass",
            "with open('x') as f:\n pass",
            "assert True",
            "raise ValueError()",
            "x=1\nimport math",
        ],
    )
    accepted.extend(
        [
            "class C:\n pass",
            "x=[i for i in [1]]",
            "x={i for i in [1]}",
            "x={i:i for i in [1]}",
            "x=[1]\nx[0]=2",
            "print(*[1])",
            "print(**{})",
        ],
    )
    accepted.extend(
        [
            "x=(i for i in [1])",
            "x=(y:=1)",
        ],
    )
    accepted.extend(
        [
            "def f(x: int):\n pass",
            "x: int=1",
            "x=1\ndel x",
            "def f(len):\n pass",
            "def f():\n len=1",
            "import math\ndef f(math):\n pass",
        ],
    )
    accepted.extend(["import math\nmath.x=1"])
    rejected = [
        "def f():\n return f()",
        "def f():\n return g()\ndef g():\n return f()",
        "def f():\n def g():\n  pass",
        "def f():\n pass\nx=f",
        "def f():\n pass\nprint(f)",
        "def f():\n pass\nf=1",
        "def f(g):\n return g()",
        "(lambda: 1)()",
        "@print\ndef f():\n pass",
        "def f():\n yield 1",
        "async def f():\n pass",
        "match 1:\n case 1: pass",
        "def f():\n global x",
        "def f():\n nonlocal x",
        "from math import *",
        "from . import x",
        "if True:\n import math",
        "def f():\n import math",
        "import math\nmath=1",
        "eval('1')",
        "import builtins as b\nb.eval('1')",
        "from builtins import eval as e\ne('1')",
        "def f(a):\n pass\nf()",
        "def f(a):\n pass\nf(1,2)",
        "def f(a):\n pass\nf(1,a=2)",
        "def f(a, /):\n pass\nf(a=1)",
        "def f(*,a):\n pass\nf(1)",
        "def f(a):\n pass\nf(b=1)",
        "print(a=1,a=2)",
        "break",
        "continue",
        "def f():\n pass\ndef f():\n pass",
        "import math\nimport math",
        "def f():\n pass\nx=[f][0]()",
        "def f():\n return f",
    ]
    for source in accepted:
        if diagnostics := check_source(source):
            raise AssertionError((source, diagnostics))
    for source in rejected:
        if not check_source(source):
            raise AssertionError(source)


def test_diagnostic_cascades() -> None:
    """Report root violations without misleading dependent diagnostics."""
    accepted = (
        '"module documentation"\nimport math\ndef f():\n return math.sqrt(4)\n'
        'if __name__ == "__main__":\n f()'
    )
    if diagnostics := check_source(accepted):
        raise AssertionError(diagnostics)
    cases = [
        ('"doc"\nx=1\nimport math', []),
        ('"doc"\n"another string"\nimport math', []),
        ("def f():\n return __name__", []),
        ("def f(x=1):\n return x\nf()", []),
        ("def f(*, x=1):\n return x\nf()", []),
        ("def f(*xs):\n return xs\nf(1, 2)", []),
        ("def f():\n return g()\ndef g(x=1):\n return x", []),
        ("x=1\nwith open('x') as stream:\n stream.read()", []),
        ("x=1\ny=[item for item in missing]", [(2, 21, "scope")]),
        ("def f():\n return [x for x in missing]", [(2, 21, "scope")]),
        ("class C:\n def method(self):\n  return missing", [(1, 1, "class")]),
        ("def f(a):\n return a\nf()", [(3, 1, "arguments")]),
        ("def f(a=1):\n return missing\nf()", [(2, 9, "scope")]),
    ]
    for source, expected in cases:
        actual = [(d.line, d.column, d.code) for d in check_source(source)]
        if actual != expected:
            raise AssertionError((source, actual, expected))


def test_expanded_profile() -> None:
    """Preserve cleanup, default binding, and opaque external operations."""
    accepted = [
        (
            "def f(x, y=1, *rest, flag=True, **options):\n"
            " return y\n"
            "f(1,2,3,flag=False,z=4)"
        ),
        "def f(x, /, **options):\n return x\nf(1,x=2)",
        "def f(x=[]):\n x.append(1)\n return x\nf()\nf()",
        "def f(x):\n return x\ndef g(x=f(2)):\n return x\ng()",
        "from __future__ import annotations\ndef f(x: int) -> int:\n return x\nf(1)",
        "def f():\n x: UnknownType=1\n return x\nf()",
        "def f():\n return DATA\nDATA=2\nf()\nDATA=3\nf()",
        (
            "def helper(x):\n"
            " return x\n"
            "def f():\n"
            " with open('x') as stream:\n"
            "  return helper(stream.read())\n"
            "f()"
        ),
        (
            "def f():\n"
            " with open('a') as a, open('b') as b:\n"
            "  return a.read()+b.read()\n"
            "f()"
        ),
        (
            "def helper():\n"
            " return 1\n"
            "def f():\n"
            " try:\n"
            "  return helper()\n"
            " finally:\n"
            "  print('cleanup')\n"
            "f()"
        ),
        "def f():\n try:\n  return 1\n finally:\n  return 2\nf()",
        (
            "def f():\n"
            " try:\n"
            "  raise ValueError('x')\n"
            " except ValueError as error:\n"
            "  return str(error)\n"
            "f()"
        ),
        (
            "def f():\n"
            " for i in range(3):\n"
            "  with open('x') as stream:\n"
            "   if i:\n"
            "    continue\n"
            "   break\n"
            "f()"
        ),
        "def f(x):\n if x:\n  return 1\n return 2\nf(True)",
        "def f():\n return 1\nx=f'{f():04d}'\nassert f()==1, f'{f()}'",
        (
            "def f():\n"
            " try:\n"
            "  pass\n"
            " except ValueError:\n"
            "  raise\n"
            " else:\n"
            "  return 1\n"
            " finally:\n"
            "  print('done')"
        ),
    ]
    accepted.extend(
        [
            "def f():\n with open('x') as f:\n  pass",
            "def f():\n try:\n  pass\n except ValueError as f:\n  pass",
        ],
    )
    rejected = [
        "def f():\n pass\ntry:\n pass\nexcept ValueError as f:\n pass",
        "def f():\n with open('x') as stream:\n  return f()",
        "def f():\n try:\n  return f()\n finally:\n  pass",
        "def f(x=1):\n return x\nf(1,x=2)",
        "def f(x=1, *, y):\n return x\nf()",
        "def f(x, /):\n return x\nf(x=1)",
        "def f():\n return 1\nx=f\nx()",
        "operation=eval\noperation.__call__('1')",
        "from builtins import eval as operation\nx=operation",
        "import builtins\nx=builtins.eval",
        "def f():\n return 1\nf.__call__()",
        "x=[]\nx.__getattribute__('append')(1)",
        "def f():\n return 1\ndef g(x=f):\n pass",
    ]
    for source in accepted:
        if diagnostics := check_source(source):
            raise AssertionError((source, diagnostics))
    for source in rejected:
        if not check_source(source):
            raise AssertionError(source)


def test_audit_regressions() -> None:
    """Accept ordinary data operations without hiding unresolved invocation paths."""
    accepted = [
        "class Error(RuntimeError):\n pass\nraise Error('x')",
        (
            "from __future__ import annotations\n"
            "from dataclasses import dataclass\n"
            "@dataclass(frozen=True)\n"
            "class Record:\n"
            " value: int\n"
            "r=Record(1)"
        ),
        (
            "from typing import TYPE_CHECKING as TC\n"
            "if TC:\n"
            " from missing import Type\n"
            "x=1"
        ),
        "import typing as t\nif t.TYPE_CHECKING:\n import missing\nelse:\n x=1",
        "def f(x):\n return x+1\nxs=[f(i) for i in range(4) if f(i)>1]",
        (
            "def f(x):\n"
            " return x\n"
            "xs={f(i) for i in range(3)}\n"
            "ys={f(i):f(i) for i in range(3)}"
        ),
        "xs=[[j for j in range(i)] for i in range(3)]",
        "i=3\nxs=[i for i in range(i)]\nx=i",
        "def f():\n values=[y for x in range(3) if (y:=x)]\n return y\nf()",
        "values=[y for x in range(3) if (y:=x)]\nx=y",
        "x=[1]\nx[0]=2\nd={}\nd.setdefault('k', {})['v']=3",
        "def f(*args, **kwargs):\n return args\nf(*[1], **{'x':2})",
        "def f(a):\n return a\nf(*[])\nf(**{'unknown':1})",
        "x=[0,*range(3)]\na,*rest=x",
        "def f():\n return range(3)\nx=(str(i) for i in f())",
        "def f():\n return (str(i) for i in range(3))\nx=f()",
        "def f(value=(x:=1)):\n return x\nf()",
        "def f(x):\n return x\nx=any(f(i) for i in range(3))",
        "def f(x):\n return x\nx=all(f(i) for i in range(3))",
        "def f(x):\n return x\nx=sorted(f(i) for i in range(3))",
        "def f():\n return 1\nx=any(any(f() for i in range(3)) for j in range(3))",
    ]
    accepted.extend(["class C:\n pass\ndef f(C):\n return C"])
    rejected = [
        "class C:\n def method(self):\n  pass\nx=C()",
        "class C:\n pass\nC=1",
        (
            "from typing import TYPE_CHECKING\n"
            "TYPE_CHECKING=True\n"
            "if TYPE_CHECKING:\n"
            " import missing"
        ),
        "if True:\n import math",
        "def f():\n return 1\nx=[f() for f in [1]]",
        "xs=[i for i in range(3)]\nx=i",
        "def f():\n xs=[i for i in range(3)]\n return i",
        "def f():\n return 1\nxs=[(f:=i) for i in range(3)]",
        "import math\nx=math.__dict__\nx['sin']=1",
        "import math\nmath.__dict__.update(sin=1)",
        "def f():\n return 1\nxs=[f]",
        "def f():\n return 1\nxs=(f() for _ in range(3))",
        "def f():\n return 1\nxs=(any(f() for i in range(3)) for j in range(3))",
        "def f():\n return 1\nxs=([f() for _ in range(3)] for _ in range(2))",
        "def f():\n return 1\nx=[(f() for _ in range(3)) for _ in range(2)]",
    ]
    for source in accepted:
        if diagnostics := check_source(source):
            raise AssertionError((source, diagnostics))
    for source in rejected:
        if not check_source(source):
            raise AssertionError(source)
    source = "class C:\n def method(self):\n  pass\nx=C()\nprint(C)"
    if [d.code for d in check_source(source)] != ["class"]:
        raise AssertionError(check_source(source))


def test_recursive_components() -> None:
    """Report one root per recursive component, excluding all upstream callers."""
    source = (
        "def entry():\n return a()\n"
        "def a():\n return b()\n"
        "def b():\n return a()\n"
        "def c():\n return c()\n"
        "def leaf():\n return 1\n"
        "def caller():\n return leaf()\n"
    )
    expected = [
        Diagnostic(4, 9, "cycle", "Recursive call component: a, b"),
        Diagnostic(8, 9, "cycle", "Recursive call component: c"),
    ]
    if check_source(source) != expected or check_source(source) != expected:
        raise AssertionError(check_source(source))
    for source in (
        "def f():\n return [f() for i in range(3)]",
        "def f(*xs):\n return f(*xs)",
    ):
        if [d.code for d in check_source(source)] != ["cycle"]:
            raise AssertionError(check_source(source))


def test_repository_scope_regressions() -> None:
    """Keep Python metadata, class fields, and lexical data bindings distinct."""
    accepted = [
        (
            '"doc"\n'
            "def main() -> None:\n"
            " print(__doc__, __file__, __package__, __spec__, __loader__, __cached__)\n"
            "main()"
        ),
        "x=Ellipsis\ny=NotImplemented\nz=__debug__",
        "from math import sin\nclass Row:\n sin: float\nsin(0)",
        "from math import sin\nclass Row:\n sin: float=0\n other=sin\nsin(0)",
        "class Row:\n value: int=1\nx=Row()  # type: ignore[misc]",
        (
            "from sqlmodel import SQLModel, Field\n"
            "class Row(SQLModel, table=True):\n"
            " value: int=Field(primary_key=True)"
        ),
        "from math import sin\ndef f(sin: float) -> float:\n return sin+1\nf(1)",
        "def f():\n return 1\ndef g(f):\n return f+1\ng(2)",
        "def f():\n return 1\nxs=[f for f in range(3)]\nf()",
        (
            "import typing\n"
            "def f(typing):\n"
            " if typing.TYPE_CHECKING:\n"
            "  return 1\n"
            " return 0"
        ),
        "def main() -> None:\n return\nmain()",
        "from pathlib import Path\nx: Path=Path('x')",
        "from math import sin\nsin: object\nsin(0)",
        "len: object\nx=len([1])",
        "def f():\n x=1\n del x\n return None\nf()",
        "def f(state):\n state.frame_index+=1\n state.previous_frame=None",
        "import torch\ntorch.backends.cudnn.benchmark=False",
        "def label(x):\n return str(x)\nx=tuple(label(i) for i in range(3))",
    ]
    rejected = [
        "def f():\n return 1\nf=2",
        "def f():\n return 1\ndel f",
        "def f():\n return 1\ndef g(f):\n return f()",
        "def f():\n return 1\nxs=[f() for f in range(3)]",
        "def f():\n return 1\nxs=[(f:=i) for i in range(3)]",
        "import typing\ndef f(typing):\n if typing.TYPE_CHECKING:\n  eval('1')",
        "def f():\n return 1\nimport other\nother.f=print",
        "def f():\n return 1\ndef g(x: f()):\n return x",
        "x: __import__('os')",
    ]
    for source in accepted:
        if diagnostics := check_source(source):
            raise AssertionError((source, diagnostics))
    for source in rejected:
        if not check_source(source):
            raise AssertionError(source)
    cascades = [
        ("def outer():\n def inner():\n  return 1\n return inner()", ["declaration"]),
        ("def f():\n from module import Client\n return Client()", ["declaration"]),
        ("def f():\n class C:\n  pass\n return C()", ["class"]),
        ("if True:\n def f():\n  pass\nf()", ["declaration"]),
    ]
    for source, codes in cascades:
        if [d.code for d in check_source(source)] != codes:
            raise AssertionError(check_source(source))


def test_input_error_recovery() -> None:
    """Return diagnostics for malformed paths, text, and contextual syntax."""
    if check_file("bad\0path")[0].code != "input":
        msg = "Embedded NUL paths must produce input diagnostics"
        raise AssertionError(msg)
    if check_source("\ud800")[0].code != "parse":
        msg = "Unencodable source must produce parsing diagnostics"
        raise AssertionError(msg)
    result = check_source("pass\nreturn 1")
    expected_line = 2
    if result[0].code != "parse" or result[0].line != expected_line:
        raise AssertionError(result)


def test_dynamic_aliases() -> None:
    """Resolve every prohibited builtin through direct and imported aliases."""
    for name in sorted(DYNAMIC):
        for source in (
            f"{name}()",
            f"import builtins as b\nb.{name}()",
            f"from builtins import {name} as operation\noperation()",
        ):
            if "dynamic" not in {d.code for d in check_source(source)}:
                raise AssertionError(source)


def test_diagnostics() -> None:
    """Keep source positions, stable codes, and deterministic ordering."""
    source = "def f():\n return f()\ndef g():\n return missing\n"
    result = check_source(source)
    if result != check_source(source) or result != sorted(result):
        raise AssertionError(result)
    if [(d.line, d.column, d.code) for d in result] != [
        (2, 9, "cycle"),
        (4, 9, "scope"),
    ]:
        raise AssertionError(result)


def test_files_and_cli() -> None:
    """Check encoding, malformed input, exit codes, and absence of execution."""
    with tempfile.TemporaryDirectory() as directory:
        path = Path(directory) / "input.py"
        marker = Path(directory) / "executed"
        cases = [
            (b"# coding: latin-1\nx='caf\xe9'\n", 0),
            (f"import pathlib\npathlib.Path({str(marker)!r})\n".encode(), 0),
            (f"open({str(marker)!r}, 'w')\n".encode(), 0),
            (b"eval('1')", 1),
            (b"break", 2),
            (b"print(a=1,a=2)", 2),
            (b"def broken(", 2),
            (b"\xff", 2),
            (b"# coding: nonexistent\nx=1", 2),
        ]
        for source, status in cases:
            path.write_bytes(source)
            diagnostics = check_file(path)
            if bool(diagnostics) != bool(status):
                raise AssertionError(diagnostics)
            result = subprocess.run(  # noqa: S603
                [os.environ["PACKAGE_E2E_EXECUTABLE"], str(path)],
                capture_output=True,
                text=True,
                check=False,
            )
            if (
                result.returncode != status
                or str(path) not in result.stdout + result.stderr
            ):
                raise AssertionError(result)
        path.unlink()
        for missing in (path, Path(directory)):
            if check_file(missing)[0].code != "input":
                raise AssertionError(missing)
            result = subprocess.run(  # noqa: S603
                [os.environ["PACKAGE_E2E_EXECUTABLE"], str(missing)],
                capture_output=True,
                text=True,
                check=False,
            )
            if result.returncode != status:
                raise AssertionError(result)
        if marker.exists():
            msg = "Input was executed"
            raise AssertionError(msg)


if __name__ == "__main__":
    main()
