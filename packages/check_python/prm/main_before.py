"""Docstring."""  # noqa: INP001

from __future__ import annotations

import functools
import os
from pathlib import Path
from typing import Callable

variable_top = 10


# standalone comment


variable = 0

while variable < variable_top:
    # comment in while header

    variable += 1

    # comment in while footer


class _MyDecorator:
    def add_info(self: Callable[[Path], Path]) -> Callable[[Path], Path]:  # type: ignore[misc]
        self.info = "This is some info"
        return self


def main(arg: int) -> None:  # inline function comment
    """Docstring in public function."""
    os.getenv("HOME")
    # standalone comment

    # comment before if
    if True:
        # comment inside if

        pass
    else:
        pass

        # comment inside if
    magic_number = 10
    while arg < magic_number:
        arg += 1

        # comment inside a while in the footer

    variable_list: list[int] = []

    magic_number_2 = 3

    magic_number_3 = 5

    for i in range(10):
        # comment inside a for loop

        if i > magic_number_2 and i < magic_number_3:
            variable_list.pop(0)

        else:
            variable_list.append(i + 1)

        # comment inside a for loop in the footer

    try:
        # comment inside a try

        pass

        # comment inside a try in the footer
    except AssertionError:
        print("assertion error")  # noqa: T201

    path = Path("non-existent/")
    function_first(path)

    # comment inside a function in the footer


@functools.lru_cache(maxsize=128)
def retrieve_arg(arg: str) -> str:
    """Docstring in public function."""
    return arg


class _ClassSecond:  # inline function comment
    """Docstring in private class."""

    def method_in_class_second(self: _ClassSecond) -> None:
        self.instance_attribute = 2

    class_attribute_second: int | None = 2  # inline attribute comment

    @functools.lru_cache(maxsize=128)  # noqa: B019
    def get_property(self: _ClassSecond) -> int:
        return self.instance_attribute

    class_attribute_third: str = "value"

    def __init__(self: _ClassSecond) -> None:
        """Docstring in method in private class."""
        # comment inside method
        self.instance_attribute = 1

        retrieve_arg("string")

    class_attribute_first: list[str]


@_MyDecorator.add_info
def function_first(path: Path) -> Path:
    """Docstring in public function."""
    return path


class _ClassFourth:
    def method_in_class_fourth(self: _ClassFourth) -> None:
        self.instance_attribute = 1


class ClassThird:
    """Docstring in public class."""

    class_attribute_2 = 2
    class_attribute: list[str]


class ClassFirst:
    """Docstring in public class."""

    def method_in_class_first(self: ClassFirst) -> None:
        """Docstring in public method."""
        self.instance_attribute = 1

    def __getitem__(self: ClassFirst, index: int) -> int:
        """Docstring in public method."""
        self.instance_attribute_other = 1
        return index

    class_attribute: list[str]

    def __init__(self: ClassFirst) -> None:
        """Docstring in public method."""
        self.instance_attribute_another = 1

    # this is a comment inside a class in the footer


variable_bottom = 2

# this is a comment before the if __name__ block

if __name__ == "__main__":
    main(1)
