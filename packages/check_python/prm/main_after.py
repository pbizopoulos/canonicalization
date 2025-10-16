"""Docstring."""  # noqa: INP001

from __future__ import annotations

import functools
import os
from pathlib import Path
from typing import Callable

variable_top = 10
variable = 0
while variable < variable_top:
    variable += 1


class ClassFirst:
    """Docstring in public class."""

    class_attribute: list[str]

    def __init__(self: ClassFirst) -> None:
        """Docstring in public method."""
        self.instance_attribute_another = 1

    def __getitem__(self: ClassFirst, index: int) -> int:
        """Docstring in public method."""
        self.instance_attribute_other = 1
        return index

    def method_in_class_first(self: ClassFirst) -> None:
        """Docstring in public method."""
        self.instance_attribute = 1


class ClassThird:
    """Docstring in public class."""

    class_attribute: list[str]
    class_attribute_2 = 2


class _ClassFourth:
    def method_in_class_fourth(self: _ClassFourth) -> None:
        self.instance_attribute = 1


class _ClassSecond:  # inline function comment
    class_attribute_first: list[str]
    class_attribute_second: int | None = 2  # inline attribute comment
    class_attribute_third: str = "value"

    def __init__(self: _ClassSecond) -> None:
        self.instance_attribute = 1
        retrieve_arg("string")

    @functools.lru_cache(maxsize=128)  # noqa: B019
    def get_property(self: _ClassSecond) -> int:
        return self.instance_attribute

    def method_in_class_second(self: _ClassSecond) -> None:
        self.instance_attribute = 2


class _MyDecorator:
    def add_info(self: Callable[[Path], Path]) -> Callable[[Path], Path]:  # type: ignore[misc]
        self.info = "This is some info"
        return self


@_MyDecorator.add_info
def function_first(path: Path) -> Path:
    """Docstring in public function."""
    return path


@functools.lru_cache(maxsize=128)
def retrieve_arg(arg: str) -> str:
    """Docstring in public function."""
    return arg


def main(arg: int) -> None:  # inline function comment
    """Docstring in public function."""
    os.getenv("HOME")
    if True:
        pass
    else:
        pass
    magic_number = 10
    while arg < magic_number:
        arg += 1
    variable_list: list[int] = []
    magic_number_2 = 3
    magic_number_3 = 5
    for i in range(10):
        if i > magic_number_2 and i < magic_number_3:
            variable_list.pop(0)
        else:
            variable_list.append(i + 1)
    try:
        pass
    except AssertionError:
        print("assertion error")  # noqa: T201
    path = Path("non-existent/")
    function_first(path)


variable_bottom = 2


if __name__ == "__main__":
    main(1)
