# Copyright (c) 2026- Paschalis Bizopoulos
"""Tests for python_inline."""

from __future__ import annotations

import ast
import os
import subprocess
import tempfile
from pathlib import Path

from packages.python_inline.main import (
    DYNAMIC,
    MAX_DEPTH,
    MAX_EXPANSIONS,
    MAX_STATEMENTS,
    Diagnostic,
    Inliner,
    check_file,
    check_source,
    inline_source,
)


def test_contexts_and_eager_comprehensions() -> None:
    """Preserve context unwinding, suppression, and key/value evaluation order."""
    cases = [
        (
            "events=[]\n"
            "def f(x):\n"
            "    result=0\n"
            "    try:\n        result=10//x\n"
            "    except ZeroDivisionError:\n        events.append('caught')\n"
            "    else:\n        events.append('success')\n"
            "    finally:\n        events.append('finally')\n"
            "    return result\n"
            "result=(f(0),f(2),events)\n"
        ),
        (
            "from contextlib import nullcontext, suppress\n"
            "events=[]\n"
            "def f(x):\n"
            "    with nullcontext(x) as a, nullcontext(a+1) as b:\n"
            "        events.append((a,b))\n"
            "    with suppress(ValueError):\n"
            "        events.append('before')\n"
            "        raise ValueError('expected')\n"
            "        events.append('unreachable')\n"
            "    return x+2\n"
            "result=(f(3),events)\n"
        ),
        (
            "events=[]\n"
            "def f(x):\n    events.append(x)\n    return x\n"
            "result=({f(x) for x in [2,1,2]}, "
            "{f(x): f(x+10) for x in [2,1,2]}, events)\n"
        ),
    ]
    for source in cases:
        updated, diagnostics = inline_source(source)
        if diagnostics or updated == source:
            raise AssertionError((updated, diagnostics))
        outcomes = []
        for program in (source, updated):
            namespace: dict[str, object] = {}
            exec(program, namespace)  # noqa: S102
            outcomes.append(namespace["result"])
        if outcomes[0] != outcomes[1]:
            raise AssertionError((updated, outcomes))
    for source in (
        "def f(cm):\n    with cm:\n        x=1\n    return x\nf(None)\n",
        "def f(cm):\n    with cm:\n        return 1\nf(None)\n",
    ):
        updated, diagnostics = inline_source(source)
        if updated != source or not diagnostics:
            raise AssertionError((updated, diagnostics))


def test_extended_control_flow() -> None:
    """Compare loop control, unpacking, annotations, mutation, and exceptions."""
    cases = [
        (
            "def f(xs):\n"
            "    total: int = 0\n"
            "    for a, b in xs:\n"
            "        if a == 2:\n            continue\n"
            "        total += a+b\n"
            "        if total > 10:\n            break\n"
            "    else:\n        total += 100\n"
            "    return total\n"
            "result=(f([]),f([(1,2),(2,3)]),f([(9,9)]))\n"
        ),
        (
            "def f(x):\n"
            "    items: list = []\n"
            "    alias=items\n"
            "    while x:\n"
            "        x -= 1\n"
            "        if x == 2:\n            continue\n"
            "        items += [x]\n"
            "    else:\n        items += [99]\n"
            "    first,*rest=items\n"
            "    return (first,rest,alias is items)\n"
            "result=f(4)\n"
        ),
        (
            "events=[]\n"
            "def f(x):\n"
            "    events.append(x)\n"
            "    raise ValueError(x) from TypeError('cause')\n"
            "try:\n    f('failure')\n"
            "except ValueError as error:\n"
            "    result=(str(error),str(error.__cause__),events)\n"
        ),
        (
            "events=[]\n"
            "def f(xs):\n"
            "    events.append('unpack')\n"
            "    a,b=xs\n"
            "    events.append('after')\n"
            "    return a+b\n"
            "try:\n    f([1])\n"
            "except ValueError as error:\n    result=(str(error),events)\n"
        ),
    ]
    for source in cases:
        updated, diagnostics = inline_source(source)
        if diagnostics or updated == source:
            raise AssertionError((updated, diagnostics))
        outcomes = []
        for program in (source, updated):
            namespace: dict[str, object] = {}
            exec(program, namespace)  # noqa: S102
            outcomes.append(namespace["result"])
        if outcomes[0] != outcomes[1]:
            raise AssertionError((updated, outcomes))


def test_header_expansion() -> None:
    """Evaluate headers once and retain body comments, loop else, and scope."""
    source = (
        "events=[]\n"
        "def f(x):\n    events.append(x)\n    return x\n"
        "def run():\n"
        "    result=[]\n"
        "    if (f(True)):\n"
        "        # preserve this body comment\n"
        "        for x in f([1,2]):\n"
        "            result.append(f(x))\n"
        "        else:\n            result.append(3)\n"
        "    return result\n"
        "result=run()\n"
    )
    for newline in ("\n", "\r\n"):
        original = source.replace("\n", newline)
        updated, diagnostics = inline_source(original)
        if diagnostics or "# preserve this body comment" not in updated:
            raise AssertionError((updated, diagnostics))
        outcomes = []
        for program in (original, updated):
            namespace: dict[str, object] = {}
            exec(program, namespace)  # noqa: S102
            outcomes.append((namespace["result"], namespace["events"]))
        if outcomes[0] != outcomes[1]:
            raise AssertionError((updated, outcomes))


def test_repeated_conditions() -> None:
    """Preserve repeated tests, continues, and else control targeting outer loops."""
    sources = [
        (
            "events=[]\n"
            "def more(x):\n    events.append(x)\n    return x>0\n"
            "for stop in [False,True]:\n"
            "    x=3\n"
            "    while more(x):\n"
            "        x -= 1\n"
            "        if x == 2:\n            continue\n"
            "        if stop:\n            break\n"
            "    else:\n        events.append('else')\n"
            "result=events\n"
        ),
        (
            "events=[]\n"
            "def more(x):\n    events.append(x)\n    return x>0\n"
            "for x in [0,1]:\n"
            "    while more(x):\n        x -= 1\n"
            "    else:\n        break\n"
            "    events.append('unreachable')\n"
            "result=events\n"
        ),
        (
            "def more(x):\n    return x>0\n"
            "def f(x):\n"
            "    while more(x):\n        x -= 1\n"
            "    else:\n        x=42\n"
            "    return x\n"
            "result=f(3)\n"
        ),
    ]
    for source in sources:
        updated, diagnostics = inline_source(source)
        if diagnostics or updated == source:
            raise AssertionError((updated, diagnostics))
        outcomes = []
        for program in (source, updated):
            namespace: dict[str, object] = {}
            exec(program, namespace)  # noqa: S102
            outcomes.append(namespace["result"])
        if outcomes[0] != outcomes[1]:
            raise AssertionError((updated, outcomes))


def test_scoped_namespace_barriers() -> None:
    """Isolate known changed names while retaining unknown-mutation barriers."""
    sources = [
        (
            "def f(x):\n    return x+1\n"
            "def unrelated():\n    f=42\n    return f\n"
            "def read():\n    for x in [1]:\n        return x\n"
            "result=(f(1),unrelated(),read())\n"
        ),
        (
            "g=1\n"
            "def change():\n    global g\n    g=2\n"
            "def read():\n    return g\n"
            "def f(x):\n    return x+1\n"
            "change()\nresult=(f(1),read())\n"
        ),
        (
            "import math\nimport math\n"
            "def f(x):\n    return x+1\n"
            "def read():\n    return math.sqrt(4)\n"
            "result=(f(1),read())\n"
        ),
    ]
    for source in sources:
        updated, diagnostics = inline_source(source, strict=False)
        if updated == source or not diagnostics or "read()" not in updated:
            raise AssertionError((updated, diagnostics))
        outcomes = []
        for program in (source, updated):
            namespace: dict[str, object] = {}
            exec(program, namespace)  # noqa: S102
            outcomes.append(namespace["result"])
        if outcomes[0] != outcomes[1]:
            raise AssertionError((updated, outcomes))


def test_file_statement_budget() -> None:
    """Keep earlier edits when a later replacement exhausts the file budget."""
    source = "def f(x):\n    return x+1\na=f(1)\nb=f(2)\nresult=(a,b)\n"
    audit: dict[str, int] = {}
    updated, diagnostics = inline_source(
        source,
        strict=False,
        audit=audit,
        limits=(MAX_EXPANSIONS, MAX_DEPTH, 12),
    )
    if (
        audit["rewritten_statements"] != 1
        or not any("File rewrites exceed" in d.message for d in diagnostics)
        or "b=f(2)" not in updated
    ):
        raise AssertionError((updated, diagnostics, audit))
    outcomes = []
    for program in (source, updated):
        namespace: dict[str, object] = {}
        exec(program, namespace)  # noqa: S102
        outcomes.append(namespace["result"])
    if outcomes[0] != outcomes[1]:
        raise AssertionError((updated, outcomes))


def test_expansion_budgets() -> None:
    """Retain oversized calls and preserve strict transactions and file budgets."""
    source = (
        "def leaf(x):\n    return x+1\n"
        "def branch(x):\n    return leaf(x)+leaf(x)\n"
        "result=branch(2)\n"
    )
    for limits in (
        (1, MAX_DEPTH, MAX_STATEMENTS),
        (MAX_EXPANSIONS, 1, MAX_STATEMENTS),
        (MAX_EXPANSIONS, MAX_DEPTH, 1),
    ):
        for strict in (False, True):
            updated, diagnostics = inline_source(source, strict=strict, limits=limits)
            if not diagnostics or (strict and updated != source):
                raise AssertionError((updated, diagnostics))
            outcomes = []
            for program in (source, updated):
                namespace: dict[str, object] = {}
                exec(program, namespace)  # noqa: S102
                outcomes.append(namespace["result"])
            if outcomes[0] != outcomes[1]:
                raise AssertionError((updated, outcomes))
    executable = os.environ["PACKAGE_E2E_EXECUTABLE"]
    with tempfile.TemporaryDirectory() as directory:
        path = Path(directory) / "source.py"
        path.write_text(source)
        result = subprocess.run(  # noqa: S603
            [executable, "--check", "--max-statements", "1", str(path)],
            capture_output=True,
            text=True,
            check=False,
        )
        if (
            result.returncode != 1
            or "safety budget" not in result.stderr
            or path.read_text() != source
        ):
            raise AssertionError(result)


def test_audit_summary() -> None:
    """Report concrete blockers and count only committed statement edits."""
    source = "def f(x):\n    del x\na=f([])\nb=f([])\n"
    audit: dict[str, int] = {}
    updated, diagnostics = inline_source(source, strict=False, audit=audit)
    if updated != source or audit != {
        "candidate_calls": 2,
        "rewritten_statements": 0,
    }:
        raise AssertionError((updated, audit))
    expected_sites = 2
    if len(diagnostics) != expected_sites or any(
        "Callee f (defined at 1:1): Unsupported callee statement Delete at 2:5"
        not in diagnostic.message
        for diagnostic in diagnostics
    ):
        raise AssertionError(diagnostics)
    executable = os.environ["PACKAGE_E2E_EXECUTABLE"]
    with tempfile.TemporaryDirectory() as directory:
        path = Path(directory) / "source.py"
        path.write_text(source)
        result = subprocess.run(  # noqa: S603
            [executable, "--check", "--summary", str(path)],
            capture_output=True,
            text=True,
            check=False,
        )
        if (
            result.returncode != 1
            or "2 syntactic local-call candidates; 0 statements planned"
            not in result.stdout
            or "2 sites: 3:3, 4:3" not in result.stdout
            or path.read_text() != source
        ):
            raise AssertionError(result)
    inline_source("def f():\n    return 1\na=f()\n", audit=audit)
    if audit != {"candidate_calls": 1, "rewritten_statements": 1}:
        raise AssertionError(audit)
    inline_source("def broken(", audit=audit)
    if audit != {"candidate_calls": 0, "rewritten_statements": 0}:
        raise AssertionError(audit)


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
        "(good(1), {bad(x) for x in [2]})",
    ):
        source = (
            "events=[]\n"
            "def good(x):\n    events.append(x)\n    return x+1\n"
            "def bad(x):\n    for y in [x]:\n"
            "        events.append(y)\n        return x\n"
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
        "def f(x):\n    return x\nwith f(False):\n    pass\n",
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
