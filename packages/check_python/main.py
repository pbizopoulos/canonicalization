#!/usr/bin/env python3
"""Check Python."""

from __future__ import annotations

import os
import shutil
import sys
import unittest
from pathlib import Path

import fire
import libcst
import libcst.matchers as m

_PACKAGE_PATH = (
    Path.home() / "github.com/pbizopoulos/canonicalization/packages/check_python/"
)
if Path(__file__).resolve().as_posix().startswith("/nix/store/"):
    _PARENT_PATH = Path(__file__).resolve().parent
else:
    _PARENT_PATH = _PACKAGE_PATH
_OUT_PATH = _PACKAGE_PATH / "tmp"


class _CSTTransformer(libcst.CSTTransformer):  # type: ignore[misc]
    def __init__(self) -> None:
        self.is_class_private = False
        self.is_in_class_body = False
        self.is_in_function_body = False
        self.is_function_private = False

    def leave_ClassDef(  # noqa: C901,N802,PLR0912,PLR0915
        self,
        original_node: libcst.ClassDef,  # noqa: ARG002
        updated_node: libcst.ClassDef,
    ) -> libcst.ClassDef:
        self.is_in_class_body = False
        docstring_node = None
        statements = updated_node.body.body
        if updated_node.get_docstring():
            docstring_node = statements[0]
            statements = statements[1:]
        should_include_docstring = docstring_node and not self.is_class_private
        processed_functions = []
        processed_attributes = {}
        processed_others = []
        for node in statements:
            if isinstance(node, libcst.FunctionDef):
                modified_func = node.with_changes(
                    body=node.body.with_changes(
                        footer=(libcst.EmptyLine(indent=False),),
                    ),
                )
                processed_functions.append(modified_func)
            elif isinstance(node, libcst.SimpleStatementLine):
                stmt = node.body[0]
                key = None
                if m.matches(stmt, m.Assign(targets=[m.AssignTarget(target=m.Name())])):
                    key = stmt.targets[0].target.value
                elif m.matches(stmt, m.AnnAssign(target=m.Name())):
                    key = stmt.target.value
                elif (
                    hasattr(stmt, "value")
                    and hasattr(stmt.value, "value")
                    and isinstance(stmt.value.value, str)
                ):
                    key = stmt.value.value
                if key is not None:
                    modified_attr = node.with_changes(leading_lines=())
                    processed_attributes[key] = modified_attr
                else:
                    processed_others.append(node)
            else:
                processed_others.append(node)
        sorted_attribute_nodes = [
            processed_attributes[key] for key in sorted(processed_attributes)
        ]
        sorted_function_nodes = sorted(processed_functions, key=_get_sort_key)
        init_index = -1
        for i, func_node in enumerate(sorted_function_nodes):
            if func_node.name.value == "__init__":
                init_index = i
                break
        if init_index != -1:
            init_node = sorted_function_nodes.pop(init_index)
            sorted_function_nodes.insert(0, init_node)
        if sorted_function_nodes:
            needs_leading_line = (
                bool(sorted_attribute_nodes) or should_include_docstring
            )
            if needs_leading_line:
                first_func = sorted_function_nodes[0]
                if not first_func.leading_lines or first_func.leading_lines[
                    -1
                ] != libcst.EmptyLine(indent=False):
                    sorted_function_nodes[0] = first_func.with_changes(
                        leading_lines=(libcst.EmptyLine(indent=False),),
                    )
            else:
                sorted_function_nodes[0] = sorted_function_nodes[0].with_changes(
                    leading_lines=(),
                )
            last_func = sorted_function_nodes[-1]
            sorted_function_nodes[-1] = last_func.with_changes(
                body=last_func.body.with_changes(footer=()),
            )
        final_body_statements = []
        if should_include_docstring:
            final_body_statements.append(docstring_node)
            if sorted_attribute_nodes or sorted_function_nodes:
                final_body_statements.append(libcst.EmptyLine(indent=False))
        final_body_statements.extend(sorted_attribute_nodes)
        final_body_statements.extend(sorted_function_nodes)
        final_body_statements.extend(processed_others)
        return updated_node.with_changes(
            body=updated_node.body.with_changes(body=tuple(final_body_statements)),
        )

    def leave_EmptyLine(  # noqa: N802
        self,
        original_node: libcst.EmptyLine,  # noqa: ARG002
        updated_node: libcst.EmptyLine,
    ) -> libcst.EmptyLine | libcst.RemovalSentinel:
        if self.is_in_function_body:
            return libcst.RemoveFromParent()
        return updated_node

    def leave_FunctionDef(  # noqa: N802
        self,
        original_node: libcst.FunctionDef,  # noqa: ARG002
        updated_node: libcst.FunctionDef,
    ) -> libcst.FunctionDef:
        self.is_in_function_body = False
        if updated_node.get_docstring() and (
            (self.is_in_class_body and self.is_class_private)
            or self.is_function_private
        ):
            updated_node = updated_node.with_deep_changes(
                updated_node.body,
                body=updated_node.body.body[1:],
            )
        return updated_node

    def leave_IndentedBlock(  # noqa: N802
        self,
        original_node: libcst.IndentedBlock,  # noqa: ARG002
        updated_node: libcst.IndentedBlock,
    ) -> libcst.IndentedBlock:
        return updated_node.with_changes(footer=())

    def leave_Module(  # noqa: C901,N802,PLR0912
        self,
        original_node: libcst.Module,  # noqa: ARG002
        updated_node: libcst.Module,
    ) -> libcst.Module:
        if not updated_node.body:
            return updated_node
        classes = []
        functions = []
        for node in updated_node.body:
            if isinstance(node, libcst.ClassDef):
                classes.append(
                    node.with_changes(
                        body=node.body.with_changes(
                            footer=(
                                libcst.EmptyLine(indent=False),
                                libcst.EmptyLine(indent=False),
                            ),
                        ),
                        leading_lines=(),
                    ),
                )
            elif isinstance(node, libcst.FunctionDef):
                functions.append(
                    node.with_changes(
                        body=node.body.with_changes(
                            footer=(
                                libcst.EmptyLine(indent=False),
                                libcst.EmptyLine(indent=False),
                            ),
                        ),
                    ),
                )
        sorted_classes = sorted(classes, key=lambda n: n.name.value)
        sorted_functions = sorted(functions, key=_get_sort_key)
        new_body = []
        first_class_or_function_found = False
        first_import_found = False
        for node in updated_node.body:
            is_import = isinstance(node.body, tuple) and (
                m.matches(node.body[0], m.ImportFrom())
                or m.matches(node.body[0], m.Import())
            )
            if is_import:
                first_import_found = True
                new_body.append(node)
                continue
            if first_import_found and not is_import:
                node = node.with_changes(  # noqa: PLW2901
                    leading_lines=(libcst.EmptyLine(indent=False),),
                )
                first_import_found = False
            if (
                isinstance(node, (libcst.ClassDef, libcst.FunctionDef))
                and not first_class_or_function_found
            ):
                if sorted_classes:
                    sorted_classes[0] = sorted_classes[0].with_changes(
                        leading_lines=(libcst.EmptyLine(), libcst.EmptyLine()),
                    )
                    new_body.extend(sorted_classes)
                new_body.extend(sorted_functions)
                first_class_or_function_found = True
                continue
            if isinstance(node, (libcst.ClassDef, libcst.FunctionDef)):
                continue
            if first_class_or_function_found:
                node = node.with_changes(leading_lines=())  # noqa: PLW2901
            new_body.append(node)
        if (
            new_body
            and isinstance(new_body[-1], libcst.If)
            and hasattr(new_body[-1].test, "left")
            and hasattr(new_body[-1].test.left, "value")
            and new_body[-1].test.left.value == "__name__"
            and new_body[-1].test.comparisons[0].comparator.value
            in ["'__main__'", '"__main__"']
        ):
            if len(new_body) > 1 and isinstance(
                new_body[-2],
                (libcst.ClassDef, libcst.FunctionDef),
            ):
                new_body[-2] = new_body[-2].with_changes(
                    body=new_body[-2].body.with_changes(footer=()),
                )
            new_body[-1] = new_body[-1].with_changes(
                leading_lines=(
                    libcst.EmptyLine(indent=False),
                    libcst.EmptyLine(indent=False),
                ),
            )
        return updated_node.with_changes(body=new_body)

    def leave_SimpleStatementLine(  # noqa: N802
        self,
        original_node: libcst.SimpleStatementLine,  # noqa: ARG002
        updated_node: libcst.SimpleStatementLine,
    ) -> libcst.RemovalSentinel | libcst.SimpleStatementLine:
        if all(
            isinstance(statement, libcst.Comment) for statement in updated_node.body
        ):
            return libcst.RemoveFromParent()
        is_import = isinstance(updated_node.body, tuple) and (
            m.matches(updated_node.body[0], m.ImportFrom())
            or m.matches(updated_node.body[0], m.Import())
        )
        if not is_import:
            clean_body = tuple(
                stmt
                for stmt in updated_node.body
                if not isinstance(stmt, libcst.Comment)
            )
            return updated_node.with_changes(
                leading_lines=(),
                body=clean_body,
            )
        return updated_node

    def leave_While(  # noqa: N802
        self,
        original_node: libcst.While,  # noqa: ARG002
        updated_node: libcst.While,
    ) -> libcst.While:
        return updated_node.with_changes(leading_lines=())

    def visit_ClassDef(self, node: libcst.ClassDef) -> None:  # noqa: N802
        self.is_in_class_body = True
        self.is_class_private = node.name.value.startswith(
            "_",
        ) and not node.name.value.startswith("__")

    def visit_FunctionDef(self, node: libcst.FunctionDef) -> None:  # noqa: N802
        self.is_in_function_body = True
        self.is_function_private = node.name.value.startswith(
            "_",
        ) and not node.name.value.startswith("__")


class _TestCase(unittest.TestCase):
    def test_check_python_bytes_input(self) -> None:
        with (_PARENT_PATH / "prm/main_before.py").open() as file:
            code_output_before = check_python(file.read().encode())
        with (_PARENT_PATH / "prm/main_after.py").open() as file:
            code_output_after = file.read()
        if code_output_before.decode() != code_output_after:  # type: ignore[union-attr]
            raise AssertionError

    def test_check_python_empty_input(self) -> None:
        code_output_after = check_python(b"")
        if code_output_after != b"":
            raise AssertionError

    def test_check_python_file_input(self) -> None:
        _OUT_PATH.mkdir(exist_ok=True, parents=True)
        shutil.copyfile(
            _PARENT_PATH / "prm/main_before.py",
            _OUT_PATH / "main_after.py",
        )
        check_python((_OUT_PATH / "main_after.py").as_posix())
        with (_OUT_PATH / "main_after.py").open() as file:
            code_output_before_processed = file.read()
        with (_PARENT_PATH / "prm/main_after.py").open() as file:
            code_output_after = file.read()
        if code_output_before_processed != code_output_after:
            raise AssertionError


def _get_sort_key(node: libcst.FunctionDef) -> str:
    decorator_str = ""
    for decorator in node.decorators:
        dec = decorator.decorator
        if hasattr(dec, "value"):
            if hasattr(dec.value, "value"):
                decorator_str += dec.value.value
            elif isinstance(dec.value, str):
                decorator_str += dec.value
        elif (
            hasattr(dec, "func")
            and hasattr(dec.func, "value")
            and hasattr(dec.func.value, "value")
        ):
            decorator_str += dec.func.value.value
    if decorator_str:
        return f"@{decorator_str}{node.name.value}"
    node_name_value: str = node.name.value
    return node_name_value


def check_python(*args: str | bytes) -> str | bytes | None:
    """Check Python.

    It does the following:
    1. Sorts alphabetically classes and functions in that order.
    2. Exception is the __init__ method which is placed in the top.
    3. Removes comments and redundant newlines.
    """
    for input_str_or_bytes in args:
        if isinstance(input_str_or_bytes, str):
            with Path(input_str_or_bytes).open() as file:
                cst = libcst.parse_module(file.read())
        else:
            cst = libcst.parse_module(input_str_or_bytes.decode())
        cst_transformer = _CSTTransformer()
        modified_tree = cst.visit(cst_transformer)
        code_unparsed: str = modified_tree.code
        if isinstance(input_str_or_bytes, str):
            with Path(input_str_or_bytes).open("w") as file:
                file.write(code_unparsed)
            if len(args) == 1:
                return None
        if len(args) == 1:
            return code_unparsed.encode()
    return None


def main() -> None:
    """Launch check_python using the Fire module."""
    fire.Fire(check_python)


if __name__ == "__main__":
    if os.getenv("DEBUG"):
        sys.exit()
        unittest.main()
    else:
        main()
