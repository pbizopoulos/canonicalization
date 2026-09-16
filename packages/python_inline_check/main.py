#!/usr/bin/env python3
# Copyright (c) 2026- Paschalis Bizopoulos
"""Check a deliberately restricted, mechanically inlineable Python profile."""

from __future__ import annotations

import argparse
import ast
import builtins
import inspect
import os
import subprocess
import sys
import tempfile
import tokenize
import warnings
from dataclasses import dataclass
from pathlib import Path

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


class Checker(ast.NodeVisitor):
    """Collect symbols and validate every body, including unused functions."""

    def __init__(self, tree: ast.Module) -> None:
        """Initialize module symbols before visiting any calls."""
        self.tree = tree
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
        self.scope = node.name
        self.locals = self.stores(node) | set(names)
        previous_unsupported = self.unsupported_bindings
        self.unsupported_bindings = previous_unsupported | self.declaration_names(node)
        for statement in node.body:
            self.visit(statement)
        self.scope = None
        self.locals = previous
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
        elif not shadowed and (
            name in self.imports
            or (not parts and (name in BUILTINS or name in self.classes))
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


def main() -> None:
    """Print acceptance or source-located diagnostics with distinct exit codes."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("file", type=Path)
    args = parser.parse_args()
    diagnostics = check_file(args.file)
    for diagnostic in diagnostics:
        sys.stderr.write(
            f"{args.file}:{diagnostic.line}:{diagnostic.column}: "
            f"{diagnostic.code}: {diagnostic.message}\n",
        )
    if not diagnostics:
        sys.stdout.write(f"OK {args.file}\n")
    sys.exit(
        2
        if any(d.code in {"input", "parse"} for d in diagnostics)
        else int(bool(diagnostics)),
    )


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
