# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""Source-linked contract test: a plan model must read exactly what the C++ files it mirrors read.

Each model names its C++ reader files in `cpp_sources` (repo-relative) and its exceptions in `cpp_ignore`. The test
scans those files, after stripping comments, for the quoted config keys of every accessor style:

- `getBool/getString/getInt/getLong/getInt64/getUnsigned/getDouble/getFloat("k"[, default])` and the `*Vector` forms
- `has("k")` and `isString/isList/isSubConfiguration/isBoolean/isIntegral/isFloatingPoint...("k")`
- `getSubConfiguration(s)("k")` and `subComponent(s)("k")`
- `util::parseBool(cfg, "k", default)`, `parseBoolOption(cfg, "k")` and `util::parseEnabled(cfg, default)` (key `enable`)
- `cf::optionalEntry("k", &Struct::member)` and `cf::requiredEntry(...)` of the strict parser (`fields_` tuples)

and checks, for the model and the models nested in it, recursively:

1. Keys: every key that C++ reads is a field alias or is in `cpp_ignore`, and every field is read, or is in `cpp_ignore`.
2. Types and defaults: a literal default (`true`, `1.5`, `"info"`, `std::string("x")`, or the member initialiser of a
   strict-parser struct) equals the Python default, and the annotation fits the accessor (bool, str, int, float, list).
3. Enums: the string literals that C++ compares a key's value with (`var == "x"` chains, `EnumTrait` tables) equal the
   values of the model's `Literal`. A `Literal` that cannot be extracted needs a `cpp_ignore` entry. Where C++ throws
   for any other string the field must reject it; where it falls through (a chain without a `throw`, such as the
   `stream` of `print`) the model lists the key in `cpp_open_enums` and the field must accept any string.
4. Conversions: a field read with `getBool`, `parseBool`, `getLong`, `getInt`, `getDouble`, `getString`, `getStringVector`
   or by the strict parser has the type that converts like that reader (`ConfigBool`, `ParsedBool`, `ConfigInt`,
   `ConfigStr`, ... in `multio.plans.base`), so the strict `from_yaml` accepts exactly the values that C++ converts.
   `getString` converts numbers, booleans and lists of one element, so a plain `str` there is a drift. The strings
   of the strict parser are strict, and a `ConfigStr` there is a drift too. The only exceptions are the `type` of an
   action, sink or trigger (a discriminator that the registry test checks) and the keys of `cpp_strict_strings`, which
   C++ reads with `getString` after an `isString` check; the test verifies that check.

Keys are checked per model. A reader file is owned by every model that lists it in `cpp_sources`, and the keys read in it
must belong to that model: a field, a `cpp_ignore` entry, or a field of a model that the model uses directly and that
lists the same file. The key sets of unrelated models are never merged, so a key that happens to exist in another model
(a `path` of the file sink, say) does not hide a missing field.

Set `MULTIO_CPP_SOURCE_ROOT` to the directory that replaces `src/multio` to scan another copy of the sources.
"""

import ast
import os
import re
import types
import typing
from dataclasses import dataclass
from pathlib import Path
from typing import Annotated, Any, Callable, Literal, Union

import pytest
from pydantic import BaseModel, BeforeValidator, TypeAdapter, ValidationError

from multio.plans import actions, plans, sinks
from multio.plans import base
from multio.plans.base import MultioBaseModel

REPO_ROOT = Path(__file__).resolve().parents[3]
CPP_PREFIX = "src/multio/"


def _source_root() -> Path:
    return Path(os.environ.get("MULTIO_CPP_SOURCE_ROOT") or REPO_ROOT / "src" / "multio")


# Keys whose values are not compared where they are read: they flow into a function that dispatches on a parameter.
# {key: C++ variable that is compared with the string literals}
ENUM_VARIABLES = {"operations": "opname"}

# Keys that are polymorphic discriminators, validated by the registry test instead.
DISCRIMINATORS = {"type"}

# ----------------------------------------------------------------------------------------------------------------------
# Scanning C++

UNSET = object()  # no default argument
NONLITERAL = object()  # a default that is not a literal: unchecked

_SCALAR_KINDS = {
    "Bool": "bool",
    "String": "str",
    "Int": "int",
    "Long": "int",
    "Int64": "int",
    "Unsigned": "int",
    "Double": "float",
    "Float": "float",
}
# accessor name -> kind of the value read (None: presence or type test only)
ACCESSOR_KINDS: dict[str, str | None] = {
    **{f"get{name}": kind for name, kind in _SCALAR_KINDS.items()},
    **{f"get{name}Vector": f"list[{kind}]" for name, kind in _SCALAR_KINDS.items()},
    "getSubConfiguration": "map",
    "getSubConfigurations": "maplist",
    "subComponent": "map",
    "subComponents": "maplist",
    "has": None,
    **{
        name: None
        for name in (
            "isString",
            "isList",
            "isSubConfiguration",
            "isSubConfigurationList",
            "isBoolean",
            "isIntegral",
            "isFloatingPoint",
            "isStringList",
            "isIntegralList",
            "isFloatingPointList",
            "isBooleanList",
        )
    },
}
# helpers that take the config first: name -> (kind, index of the key argument)
KEY_SECOND = {"parseBool": "bool", "parseBoolOption": "bool"}
# helpers that always read the same key, with the default as second argument: name -> (key, kind)
FIXED_KEY = {"parseEnabled": ("enable", "bool")}
ENTRIES = {"optionalEntry": False, "requiredEntry": True}  # name -> required

_CALL = re.compile(
    r"\b("
    + "|".join(sorted([*ACCESSOR_KINDS, *KEY_SECOND, *FIXED_KEY, *ENTRIES], key=len, reverse=True))
    + r")\s*(?:<[^<>()]*>)?\s*\("
)
_COMMENTS = re.compile(r'"(?:\\.|[^"\\\n])*"|\'(?:\\.|[^\'\\\n])*\'|//[^\n]*|/\*.*?\*/', re.DOTALL)
_NUMBER = re.compile(r"[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?[fFlLuU]*")


def strip_comments(text: str) -> str:
    """Remove C++ comments, keeping string and character literals (`"http://"` is not a comment)."""
    return _COMMENTS.sub(lambda m: m.group(0) if m.group(0)[0] in "\"'" else " ", text)


def _skip_literal(text: str, i: int) -> int:
    quote = text[i]
    i += 1
    while i < len(text) and text[i] != quote:
        i += 2 if text[i] == "\\" else 1
    return i + 1


def split_args(text: str, start: int) -> tuple[list[str], int]:
    """Split the arguments of the call whose `(` ends at `start`; returns them with the index of the closing `)`."""
    args, depth, begin, i = [], 0, start, start
    while i < len(text):
        char = text[i]
        if char in "\"'":
            i = _skip_literal(text, i)
            continue
        if char in "([{":
            depth += 1
        elif char in ")]}":
            if depth == 0:
                args.append(text[begin:i])
                return args, i
            depth -= 1
        elif char == "," and depth == 0:
            args.append(text[begin:i])
            begin = i + 1
        i += 1
    return args, len(text)


def parse_string_literal(arg: str) -> str | None:
    match = re.fullmatch(r'\s*"((?:\\.|[^"\\])*)"\s*', arg)
    if not match:
        return None
    try:
        return ast.literal_eval(f'"{match.group(1)}"')
    except (SyntaxError, ValueError):
        return match.group(1)


def parse_literal(arg: str) -> Any:
    """The value of a C++ literal (true/false, number, string, `std::string("x")`), else NONLITERAL."""
    arg = arg.strip()
    wrapped = re.fullmatch(r"std::string\s*[({]\s*(.*?)\s*[)}]", arg, flags=re.DOTALL)
    if wrapped:
        arg = wrapped.group(1)
    if arg in ("true", "false"):
        return arg == "true"
    text = parse_string_literal(arg)
    if text is not None:
        return text
    if _NUMBER.fullmatch(arg):
        number = arg.rstrip("fFlLuU")
        return float(number) if re.search(r"[.eE]", number) else int(number)
    return NONLITERAL


@dataclass(frozen=True)
class Entry:
    """A field of a strict-parser struct: `cf::optionalEntry("key", &Struct::member)`."""

    required: bool
    kind: str | None  # from the C++ type of the member
    default: Any  # UNSET, NONLITERAL or a literal (member initialiser)
    enum_type: str | None  # C++ enum type, whose `EnumTrait` lists the allowed strings


@dataclass(frozen=True)
class Read:
    key: str
    accessor: str
    kind: str | None
    default: Any
    file: str
    entry: Entry | None = None


def _cxx_kind(cxx_type: str) -> tuple[str | None, str | None]:
    """(kind, enum type name) of a C++ member type."""
    cxx_type = cxx_type.strip()
    optional = re.fullmatch(r"std::optional<(.+)>", cxx_type)
    if optional:
        cxx_type = optional.group(1).strip()
    if cxx_type == "bool":
        return "bool", None
    if cxx_type == "std::string":
        return "str", None
    if re.fullmatch(
        r"(?:std::)?(?:u?int\d*_t|int|long|long long|unsigned|unsigned int|unsigned long|size_t)", cxx_type
    ):
        return "int", None
    if cxx_type in ("double", "float"):
        return "float", None
    if re.fullmatch(r"[\w:]+", cxx_type):
        return None, cxx_type.split("::")[-1]
    return None, None


def _struct_body(text: str, name: str) -> str | None:
    match = re.search(rf"\bstruct\s+{re.escape(name)}\b[^{{;]*\{{", text)
    if not match:
        return None
    depth, i = 1, match.end()
    while i < len(text) and depth:
        depth += {"{": 1, "}": -1}.get(text[i], 0)
        i += 1
    return text[match.end() : i - 1]


def _entry(text: str, required: bool, member_arg: str) -> Entry:
    """Resolve `&Struct::member` to the declaration of the member in the same file."""
    ref = re.fullmatch(r"\s*&\s*(\w+)::(\w+)\s*", member_arg)
    body = _struct_body(text, ref.group(1)) if ref else None
    if body is None:
        return Entry(required, None, NONLITERAL, None)
    decl = re.search(rf"(?:^|[;{{}}])\s*([\w:<>,\s]+?)\s+{ref.group(2)}\s*(?:=\s*([^;]+?)|\{{([^;]*?)\}})?\s*;", body)
    if not decl:
        return Entry(required, None, NONLITERAL, None)
    kind, enum_type = _cxx_kind(decl.group(1))
    initialiser = decl.group(2) if decl.group(2) is not None else decl.group(3)
    default = UNSET if initialiser is None else parse_literal(initialiser)
    return Entry(required, kind, default, enum_type)


def scan_text(text: str, file: str) -> list[Read]:
    """All config keys read in `text` (comments already stripped), with the accessor and the literal default."""
    reads = []
    for call in _CALL.finditer(text):
        name = call.group(1)
        args, _ = split_args(text, call.end())
        if name in FIXED_KEY:
            key, kind = FIXED_KEY[name]
            default = parse_literal(args[1]) if len(args) > 1 else UNSET
            reads.append(Read(key, name, kind, default, file))
            continue
        key_index = 1 if name in KEY_SECOND else 0
        if len(args) <= key_index:
            continue
        key = parse_string_literal(args[key_index])
        if key is None:
            continue  # dynamic key: not scannable
        if name in ENTRIES:
            if len(args) < 2:
                continue
            entry = _entry(text, ENTRIES[name], args[1])
            reads.append(Read(key, name, entry.kind, entry.default, file, entry))
            continue
        kind = KEY_SECOND.get(name, ACCESSOR_KINDS.get(name))
        if len(args) > key_index + 1:
            default = parse_literal(args[key_index + 1])
        else:
            default = UNSET
        reads.append(Read(key, name, kind, default, file))
    return reads


def _variable_scope(text: str, start: int, variable: str) -> str:
    """The text where a variable assigned at `start` is still the same variable.

    Members (trailing underscore) live in the whole file. A local ends with its block, or where the name is
    declared again (`const std::string input = ...` in a nested block shadows it).
    """
    if variable.endswith("_"):
        return text
    depth, end = 0, len(text)
    redeclaration = re.compile(rf"\b(?:auto|std::string|const\s+auto|const\s+std::string)\s+{re.escape(variable)}\b")
    for i in range(start, len(text)):
        if text[i] == "{":
            depth += 1
        elif text[i] == "}":
            depth -= 1
            if depth < 0:
                end = i
                break
    shadow = redeclaration.search(text, start, end)
    return text[start : shadow.start() if shadow else end]


def _comparisons(text: str, variable: str, start: int, end: int) -> list[tuple[str, int]]:
    """`(literal, position)` of every `variable == "literal"` or `"literal" == variable` (or `!=`) in `text[start:end]`."""
    found = []
    for pattern in (
        rf'\b{re.escape(variable)}\s*[!=]=\s*"((?:\\.|[^"\\])*)"',
        rf'"((?:\\.|[^"\\])*)"\s*[!=]=\s*{re.escape(variable)}\b',
    ):
        found += [(m.group(1), start + m.start()) for m in re.finditer(pattern, text[start:end])]
    return found


def comparison_sites(text: str, key: str) -> list[tuple[str, int]]:
    """`(literal, position)` of the comparisons with the variables that get their value from `getString("key", ...)`."""
    sites = []
    assignment = re.compile(rf'(\w+)\s*(?:=|\{{)[^;{{}}=]*?\bgetString\s*\(\s*"{re.escape(key)}"')
    for match in assignment.finditer(text):
        variable = match.group(1)
        scope = _variable_scope(text, match.end(), variable)
        sites += _comparisons(text, variable, match.end(), match.end() + len(scope))
    return sites


def comparison_literals(text: str, key: str) -> set[str]:
    """String literals compared with the variables that get their value from `getString("key", ...)`."""
    return {literal for literal, _ in comparison_sites(text, key)}


def variable_sites(text: str, variable: str) -> list[tuple[str, int]]:
    return _comparisons(text, variable, 0, len(text))


def variable_literals(text: str, variable: str) -> set[str]:
    return {literal for literal, _ in variable_sites(text, variable)}


_NOT_A_FUNCTION = {"if", "while", "for", "switch", "catch"}


def _brace_blocks(text: str) -> list[tuple[int, int]]:
    """`(open, close)` indices of every `{...}` block outside string and character literals."""
    blocks, stack, i = [], [], 0
    while i < len(text):
        char = text[i]
        if char in "\"'":
            i = _skip_literal(text, i)
            continue
        if char == "{":
            stack.append(i)
        elif char == "}" and stack:
            blocks.append((stack.pop(), i))
        i += 1
    return blocks


def enclosing_function(text: str, position: int) -> str:
    """The body of the function that contains `position`; the whole text when there is none (a brace initialiser).

    The innermost block whose `{` follows a `)` (and `const`, `noexcept`...) and is not the block of an `if`, `for`,
    `while`, `switch` or `catch`.
    """
    blocks = [block for block in _brace_blocks(text) if block[0] < position < block[1]]
    for opening, closing in sorted(blocks, reverse=True):
        head = re.search(r"\)\s*(?:(?:const|noexcept|override|final)\s*)*$", text[:opening])
        if not head:
            continue
        depth, j = 0, head.start()
        while j >= 0:  # the `(` that matches the `)`
            depth += {")": 1, "(": -1}.get(text[j], 0)
            if depth == 0:
                break
            j -= 1
        keyword = re.search(r"(\w+)\s*$", text[:j])
        if not (keyword and keyword.group(1) in _NOT_A_FUNCTION):
            return text[opening : closing + 1]
    return text


def throws_after_comparing(text: str, sites: list[tuple[str, int]]) -> bool:
    """Whether a function that compares the value with a literal can throw: C++ rejects an unknown value then.

    A chain of comparisons without a `throw` (and no `EnumTrait`) falls through for any other value.
    """
    return any(re.search(r"\bthrow\b", enclosing_function(text, position)) for _, position in sites)


def enum_trait_values(texts: list[str], enum_type: str) -> set[str]:
    """Strings of `EnumTrait<...::Type> { values{ std::pair{Type::X, "x"}, ... } }`."""
    values = set()
    for text in texts:
        for match in re.finditer(rf"EnumTrait<(?:[\w:]*::)?{re.escape(enum_type)}>\s*\{{(.*?)\}};", text, re.DOTALL):
            values |= set(re.findall(r'std::pair\s*\{[^,{}]+,\s*"([^"]*)"\s*\}', match.group(1)))
    return values


# ----------------------------------------------------------------------------------------------------------------------
# The source tree, optionally mutated for the mutation tests


class Sources:
    """Reads the cited C++ files from `root`; `mutations` maps a file name suffix to a function on its raw text."""

    def __init__(self, root: Path, mutations: dict[str, Callable[[str], str]] | None = None):
        self.root = root
        self.mutations = mutations or {}
        self._cache: dict[str, str | None] = {}

    def path(self, rel: str) -> Path:
        if rel.startswith(CPP_PREFIX):
            return self.root / rel[len(CPP_PREFIX) :]
        return REPO_ROOT / rel

    def text(self, rel: str) -> str | None:
        """Comment-free text, or None when the file does not exist."""
        if rel not in self._cache:
            path = self.path(rel)
            if not path.is_file():
                self._cache[rel] = None
            else:
                raw = path.read_text()
                for suffix, mutate in self.mutations.items():
                    if rel.endswith(suffix):
                        raw = mutate(raw)
                self._cache[rel] = strip_comments(raw)
        return self._cache[rel]


# ----------------------------------------------------------------------------------------------------------------------
# Models


def _members(annotation: Any) -> list[Any]:
    """The alternatives of an annotation: `Annotated` unwrapped, unions flattened, `None` dropped."""
    origin = typing.get_origin(annotation)
    if origin is Annotated:
        return _members(typing.get_args(annotation)[0])
    if origin in (Union, types.UnionType):
        return [member for arg in typing.get_args(annotation) for member in _members(arg)]
    return [] if annotation is type(None) else [annotation]


def _walk(annotation: Any):
    """Every type inside an annotation, at any depth."""
    for member in _members(annotation):
        yield member
        for arg in typing.get_args(member):
            yield from _walk(arg)


def nested_models(model: type[MultioBaseModel]) -> list[type[MultioBaseModel]]:
    """`model` and every plan model reachable through its fields, recursively, each once, `model` first."""
    found: list[type[MultioBaseModel]] = []

    def visit(current: type[MultioBaseModel]) -> None:
        if current in found:
            return
        found.append(current)
        for field in current.model_fields.values():
            for member in _walk(field.annotation):
                if isinstance(member, type) and issubclass(member, MultioBaseModel):
                    visit(member)

    visit(model)
    return found


def aliases(model: type[MultioBaseModel]) -> dict[str, str]:
    """{key (alias): field name}"""
    return {field.alias or name: name for name, field in model.model_fields.items()}


def literal_values(annotation: Any) -> set[str] | None:
    """The string values of every `Literal` in the annotation, None if there is none."""
    values, found = set(), False
    for member in _walk(annotation):
        if typing.get_origin(member) is Literal:
            found = True
            values |= {value for value in typing.get_args(member) if isinstance(value, str)}
    return values if found else None


def compatible(kind: str, annotation: Any) -> bool:
    """Whether some alternative of the annotation can hold a value read with an accessor of `kind`."""
    for member in _members(annotation):
        origin = typing.get_origin(member)
        if kind in ("bool", "int", "float"):
            ok = member is {"bool": bool, "int": int, "float": float}[kind]
        elif kind == "str":
            ok = member is str or (origin is Literal and all(isinstance(v, str) for v in typing.get_args(member)))
        elif kind.startswith("list["):
            ok = origin is list and compatible(kind[5:-1], typing.get_args(member)[0])
        elif kind == "map":
            ok = member is dict or origin is dict or (isinstance(member, type) and issubclass(member, BaseModel))
        elif kind == "maplist":
            ok = origin is list and compatible("map", typing.get_args(member)[0])
        else:
            raise AssertionError(f"unknown accessor kind {kind}")
        if ok:
            return True
    return False


def _equal_default(kind: str | None, python: Any, cpp: Any) -> bool:
    if kind == "bool":
        return isinstance(python, bool) and isinstance(cpp, bool) and python == cpp
    if kind == "int":
        return type(python) is int and isinstance(cpp, (bool, int)) and python == int(cpp)
    if kind == "float":
        return (
            isinstance(python, (int, float))
            and not isinstance(python, bool)
            and isinstance(cpp, (bool, int, float))
            and float(python) == float(cpp)
        )
    if kind == "str":
        return isinstance(python, str) and isinstance(cpp, str) and python == cpp
    return True  # lists and maps: C++ defaults of those are not literals


def _model_default(field) -> Any:
    return UNSET if field.is_required() else field.get_default(call_default_factory=True)


def _files(model: type[MultioBaseModel], root: type[MultioBaseModel]) -> tuple[str, ...]:
    return model.cpp_sources or root.cpp_sources


def used_models(model: type[MultioBaseModel]) -> list[type[MultioBaseModel]]:
    """The models that the fields of `model` use directly: its option sub-models, one level down."""
    found: list[type[MultioBaseModel]] = []
    for field in model.model_fields.values():
        for member in _walk(field.annotation):
            if isinstance(member, type) and issubclass(member, MultioBaseModel) and member not in found:
                found.append(member)
    return found


def sharing_models(model: type[MultioBaseModel], file: str) -> list[type[MultioBaseModel]]:
    """The models below `model` that list `file` and are reached through models that list it: its option models.

    The chain must keep listing the file, so a model that does not read the file (the `File` sink under a plan) is
    never part of it, and neither is anything below that model.
    """
    found: list[type[MultioBaseModel]] = []
    pending = [model]
    while pending:
        for sub in used_models(pending.pop()):
            if file in sub.cpp_sources and sub not in found and sub is not model:
                found.append(sub)
                pending.append(sub)
    return found


def owned_keys(model: type[MultioBaseModel], file: str) -> set[str]:
    """The keys that C++ may read in `file` on behalf of `model`, which lists the file.

    Its own fields and `cpp_ignore`, and the fields of its option models that list the same file, directly or through
    other option models that do (`interpolate-mtg2`: action, output, options). The keys of every other model are not
    allowed, whatever they are called.
    """
    keys = set(aliases(model)) | set(model.cpp_ignore)
    for sub in sharing_models(model, file):
        keys |= set(aliases(sub))
    return keys


def check_model(root: type[MultioBaseModel], sources: Sources) -> list[str]:
    """Problems found when comparing `root` and its nested models with the C++ files they cite; empty if aligned."""
    problems: list[str] = []
    models = nested_models(root)
    scanned: dict[str, list[Read]] = {}

    def reads_of(rel: str) -> list[Read]:
        if rel not in scanned:
            text = sources.text(rel)
            scanned[rel] = [] if text is None else scan_text(text, rel)
        return scanned[rel]

    # Guards: the scanner must not pass because it looked at nothing
    for model in models:
        label = model.__name__
        if not model.cpp_sources and model is not root:
            problems.append(f"{label}: nested model declares no cpp_sources")
            continue
        for rel in _files(model, root):
            if sources.text(rel) is None:
                problems.append(f"{label}: cpp_sources file {rel} does not exist under {sources.path(rel).parent}")
        if model.model_fields and not [read for rel in _files(model, root) for read in reads_of(rel)]:
            problems.append(f"{label}: no config key could be extracted from {list(_files(model, root))}")

    for model in models:
        if not model.cpp_sources and model is not root:
            continue
        files = _files(model, root)
        reads = [read for rel in files for read in reads_of(rel)]
        texts = [text for text in (sources.text(rel) for rel in files) if text is not None]
        label = model.__name__

        # 1a. Every key read in a file that the model lists belongs to the model, not to any model in the tree.
        # An option model that shares the file with the model that uses it is checked as part of that model.
        unowned: dict[str, set[str]] = {}
        parents = [other for other in models if other is not model and model in used_models(other)]
        for rel in files:
            if any(rel in _files(parent, root) for parent in parents):
                continue
            allowed = owned_keys(model, rel)
            for read in reads_of(rel):
                if read.key not in allowed:
                    unowned.setdefault(read.key, set()).add(rel.rsplit("/", 1)[-1])
        for key, where in sorted(unowned.items()):
            problems.append(
                f"{label}: C++ reads key '{key}' ({', '.join(sorted(where))}) which is not a model field of {label} "
                f"(or of a model it uses directly that lists the same file); add the field or a cpp_ignore entry "
                f"with a reason"
            )

        for key, name in aliases(model).items():
            if key in model.cpp_ignore:
                continue
            field = model.model_fields[name]
            mine = [read for read in reads if read.key == key]
            # 1b. Every field is read
            if not mine:
                problems.append(f"{label}.{name}: key '{key}' is never read in {[f.rsplit('/', 1)[-1] for f in files]}")
                continue
            problems += _check_reads(model, name, key, field, mine)
            problems += _check_enum(model, name, key, field, mine, texts)
        for key, reason in model.cpp_strict_strings.items():
            problems += _check_strict_string(model, key, reason, reads)
        for key in sorted(model.cpp_open_enums - set(aliases(model))):
            problems.append(f"{label}: cpp_open_enums lists '{key}', which is not a field")
    return problems


def converters_of(field) -> list[Callable]:
    """The functions of the `BeforeValidator`s of a field (`ConfigBool`, `ParsedBool`, ...), also inside unions."""
    # pydantic moves the metadata of a top-level `Annotated` from the annotation to `field.metadata`
    found: list[Callable] = [item.func for item in field.metadata if isinstance(item, BeforeValidator)]

    def visit(current: Any) -> None:
        if typing.get_origin(current) is Annotated:
            found.extend(item.func for item in typing.get_args(current)[1:] if isinstance(item, BeforeValidator))
        for arg in typing.get_args(current):
            visit(arg)

    visit(field.annotation)
    return found


# The converter that the field of a key read by the strict parser must use, by the kind of the C++ member
STRICT_CONVERTERS = {"bool": base.strict_bool, "int": base.strict_int, "float": base.strict_float}


STRING_ACCESSORS = ("getString", "getStringVector")
STRING_CHECKS = ("isString", "isStringList")  # a read that tells whether a value is a string, before it is read


def string_exemption(model: type[MultioBaseModel], key: str, field) -> str | None:
    """Why a field read with `getString` is not a `ConfigStr`, None when it must be one."""
    values = literal_values(field.annotation)
    if key == "type" and values is not None and len(values) == 1:
        return "discriminator"
    if key in model.cpp_strict_strings:
        return "isString check"
    return None


def expected_converter(read: Read, exempt: bool = False) -> Callable | None:
    """The converter that the field of a read must use; `exempt` for a string that C++ never converts"""
    if read.entry is not None:
        return STRICT_CONVERTERS.get(read.kind)
    if exempt and read.accessor in STRING_ACCESSORS:
        return None
    return base.CONVERTERS.get(read.accessor)


def _check_strict_string(model: type[MultioBaseModel], key: str, reason: str, reads: list[Read]) -> list[str]:
    """A `cpp_strict_strings` entry needs the `isString` check in C++, and a field that does not convert"""
    label = model.__name__
    name = aliases(model).get(key)
    if name is None:
        return [f"{label}: cpp_strict_strings lists '{key}', which is not a field"]
    if len(reason.split()) < 4:
        return [f"{label}: cpp_strict_strings['{key}'] needs a real reason, got {reason!r}"]
    problems = []
    if not any(read.key == key and read.accessor in STRING_CHECKS for read in reads):
        problems.append(
            f"{label}.{name}: cpp_strict_strings lists '{key}', but C++ has no isString('{key}') check before it reads it"
        )
    if base.config_str in converters_of(model.model_fields[name]):
        problems.append(f"{label}.{name}: C++ never converts '{key}' to a string, the field must not be a ConfigStr")
    return problems


def _check_reads(model: type[MultioBaseModel], name: str, key: str, field, reads: list[Read]) -> list[str]:
    """Types, conversions and literal defaults."""
    problems = []
    label = model.__name__
    exempt = string_exemption(model, key, field) is not None
    for read in reads:
        where = f"{label}.{name}: C++ {read.accessor}('{key}') in {read.file.rsplit('/', 1)[-1]}"
        if read.kind is not None and not compatible(read.kind, field.annotation):
            problems.append(f"{where} reads a {read.kind}, annotation is {field.annotation}")
        converter = expected_converter(read, exempt)
        if converter is not None and converter not in converters_of(field):
            problems.append(
                f"{where} converts its value like `{converter.__name__}`, the annotation {field.annotation} does not "
                f"use that conversion (see multio.plans.base)"
            )
        if read.entry is not None and read.kind == "str" and base.config_str in converters_of(field):
            problems.append(f"{where} reads strings only, the field must be a plain `str`, not a ConfigStr")
        if read.entry is not None and read.entry.required != field.is_required():
            problems.append(
                f"{where} is {'required' if read.entry.required else 'optional'}, field required={field.is_required()}"
            )
        if read.default in (UNSET, NONLITERAL):
            continue
        python = _model_default(field)
        if python is UNSET:
            problems.append(f"{where} has default {read.default!r}, the field is required")
        elif not _equal_default(read.kind, python, read.default):
            problems.append(f"{where} has default {read.default!r}, model default is {python!r}")
    return problems


UNKNOWN_VALUE = "a-value-that-no-cpp-comparison-knows"


def accepts_any_string(annotation: Any) -> bool:
    try:
        TypeAdapter(annotation).validate_python(UNKNOWN_VALUE)
    except ValidationError:
        return False
    return True


def _check_enum(
    model: type[MultioBaseModel], name: str, key: str, field, reads: list[Read], texts: list[str]
) -> list[str]:
    """The strings C++ compares the value with against the `Literal` of the field, and open against closed enums.

    C++ either throws for a value that matches none of the strings (closed: the field must reject it), or falls
    through to a default behaviour (open: the key is in `cpp_open_enums` and the field must accept any string).
    A `throw` in the function that compares is what tells them apart; an `EnumTrait` of the strict parser always throws.
    """
    if key in DISCRIMINATORS:
        return []
    label = model.__name__
    cpp: set[str] = set()
    sites: list[tuple[str, tuple[str, int]]] = []  # (text, (literal, position))
    for text in texts:
        found = comparison_sites(text, key)
        if key in ENUM_VARIABLES:
            found += variable_sites(text, ENUM_VARIABLES[key])
        cpp |= {literal for literal, _ in found}
        sites += [(text, site) for site in found]
    trait = False
    for read in reads:
        if read.entry is not None and read.entry.enum_type:
            values = enum_trait_values(texts, read.entry.enum_type)
            cpp |= values
            trait = trait or bool(values)
    python = literal_values(field.annotation)
    where = f"{label}.{name}: key '{key}'"
    if key in ENUM_VARIABLES and not variable_literals("\n".join(texts), ENUM_VARIABLES[key]):
        return [f"{where}: no comparisons with '{ENUM_VARIABLES[key]}' found in the sources"]
    problems = []
    declared_open = key in model.cpp_open_enums
    if declared_open and python is None and not cpp:
        problems.append(f"{where}: declared in cpp_open_enums, but no comparisons with literals were found")
    if cpp or declared_open:
        closed = trait or any(throws_after_comparing(text, [site]) for text, site in sites)
        open_field = accepts_any_string(field.annotation)
        if closed and declared_open:
            problems.append(f"{where}: declared in cpp_open_enums, but C++ throws for a value it does not know")
        if closed and not declared_open and open_field:
            problems.append(f"{where}: C++ throws for other values, but the field accepts any string")
        if not closed and not declared_open:
            problems.append(
                f"{where}: C++ falls through for values other than {sorted(cpp)} (no throw where it compares); "
                f"list the key in cpp_open_enums and let the field accept any string"
            )
        if declared_open and not open_field:
            problems.append(f"{where}: declared in cpp_open_enums, but the field rejects unknown strings")
    if python is None:
        if cpp:
            problems.append(f"{where}: C++ only accepts {sorted(cpp)}, the field has no Literal")
        return problems
    if not cpp:
        return problems + [
            f"{where}: the allowed values {sorted(python)} cannot be extracted from C++; add a cpp_ignore entry"
        ]
    cpp |= {read.default for read in reads if isinstance(read.default, str)}  # an absent key is accepted too
    if cpp != python:
        problems.append(
            f"{where}: Literal {sorted(python)} differs from C++ {sorted(cpp)}: only in Python {sorted(python - cpp)}, only in C++ {sorted(cpp - python)}"
        )
    return problems


# ----------------------------------------------------------------------------------------------------------------------
# Tests

ACTION_MODELS = list(typing.get_args(actions.ACTIONS))


@pytest.fixture(scope="module")
def sources():
    root = _source_root()
    if not root.is_dir():
        pytest.skip(f"C++ sources not found at {root} (set MULTIO_CPP_SOURCE_ROOT)")
    return Sources(root)


@pytest.mark.parametrize("model", ACTION_MODELS, ids=lambda model: model.__name__)
def test_action_matches_cpp(model, sources):
    problems = check_model(model, sources)
    assert not problems, "\n" + "\n".join(problems)


def test_every_action_is_checked(sources):
    """The check covers every action type and its nested models."""
    assert len(ACTION_MODELS) >= 15
    nested = {sub for model in ACTION_MODELS for sub in nested_models(model)}
    assert {actions.StatisticsOptions, actions.EncodeEncoding, actions.ScaleMapping, actions.SelectMatcher} <= nested
    assert actions.Mask in ACTION_MODELS


# Sinks, triggers, plans and the client and server configurations. The roots are the models that nest the others:
# `Client` nests the plans and so every action, and `Server` the transports.
SINK_MODELS = list(typing.get_args(sinks.SINKS))
CONFIG_MODELS = [plans.Client, plans.Server]

# Python-only models: they have no C++ reader, with the reason
PYTHON_ONLY = {plans.Collection: "groups named configurations for hammer-style files, which C++ never reads whole"}


def _models_of_module(*modules) -> set[type[MultioBaseModel]]:
    return {
        value
        for module in modules
        for value in vars(module).values()
        if isinstance(value, type) and issubclass(value, MultioBaseModel) and value.__module__ == module.__name__
    }


@pytest.mark.parametrize("model", [*SINK_MODELS, *CONFIG_MODELS], ids=lambda model: model.__name__)
def test_sink_and_config_match_cpp(model, sources):
    problems = check_model(model, sources)
    assert not problems, "\n" + "\n".join(problems)


def test_every_sink_and_plan_model_is_checked(sources):
    """Every model of sinks.py and plans.py is a root of the checks or nested in one; none is left out."""
    checked = {sub for root in [*SINK_MODELS, *CONFIG_MODELS, *ACTION_MODELS] for sub in nested_models(root)}
    defined = _models_of_module(sinks, plans) - set(PYTHON_ONLY)
    # the bases that are never instantiated: `Client` and `Server` declare the sources of `BaseConfig`
    bases = {sinks.Sinks, sinks.Trigger, plans.MultioBaseModel, plans.BaseConfig}
    assert defined - bases - checked == set()
    for model in defined - {plans.MultioBaseModel, plans.BaseConfig}:
        assert model.cpp_sources, f"{model.__name__} declares no cpp_sources"
    assert {sinks.File, sinks.FDB, sinks.Maestro, sinks.MultIO, sinks.MetadataChange, plans.Plan} <= checked
    assert {plans.DisseminationPlan, plans.MpiCommunicator, plans.TcpPeer, plans.OnServerErrorOptions} <= checked


def test_cpp_ignore_entries_give_a_reason():
    for model in _models_of_module(actions, sinks, plans):
        for key, reason in model.cpp_ignore.items():
            assert len(reason.split()) >= 4, f"{model.__name__}.cpp_ignore[{key!r}] needs a real reason, got {reason!r}"


def test_plan_models_inherit_the_alias_generator():
    assert plans.Plan.model_fields["on_error"].alias == "on-error"
    assert plans.Server.model_fields["client_group"].alias == "client-group"
    assert plans.Server.model_fields["local_port"].alias == "local_port"  # C++ spells it with an underscore
    assert plans.Server.model_fields["client_count"].alias == "clientCount"  # and this in camel case


# Registrations and tag lists that a Literal cannot be compared with where the key is read


def test_transport_names_match_cpp(sources):
    """The `transport` Literal of the server equals the names registered with `TransportBuilder`."""
    registered = set()
    for path in sorted((sources.root / "transport").glob("*.cc")):
        registered |= set(
            re.findall(r'TransportBuilder<\w+>\s+\w+\(\s*"([^"]+)"\s*\)', strip_comments(path.read_text()))
        )
    assert registered == literal_values(plans.Server.model_fields["transport"].annotation)


def _failure_tags(sources: Sources, header: str) -> set[str]:
    """The strings of the tags that `TagSequence` of the failure traits in `header` lists."""
    text = sources.text(header)
    enum_type = re.search(r"using\s+OnErrorType\s*=\s*util::(\w+)\s*;", text).group(1)
    sequence = re.search(r"TagSequence\s*=\s*util::integer_sequence<\s*OnErrorType\s*,([^>]*)>", text).group(1)
    names = re.findall(r"OnErrorType::(\w+)", sequence)
    handling = sources.text("src/multio/util/FailureHandling.cc")
    strings = dict(re.findall(rf'case\s+{enum_type}::(\w+)\s*:\s*return\s+std::string\("([^"]+)"\)', handling))
    return {strings[name] for name in names}


@pytest.mark.parametrize(
    ("header", "annotation"),
    [
        ("src/multio/server/MultioClient.h", plans.Client.model_fields["on_error"].annotation),
        ("src/multio/server/MultioServer.h", plans.Server.model_fields["on_error"].annotation),
        ("src/multio/server/Listener.h", plans.Server.model_fields["on_receive_error"].annotation),
        ("src/multio/server/Dispatcher.h", plans.Server.model_fields["on_dispatch_error"].annotation),
        ("src/multio/action/Plan.h", plans.Plan.model_fields["on_error"].annotation),
    ],
    ids=["client", "server", "listener", "dispatcher", "plan"],
)
def test_on_error_tags_match_cpp(header, annotation, sources):
    """`on-error` and its relatives accept the tags that the failure traits of the component list."""
    tags = _failure_tags(sources, header)
    assert tags
    options = [member for member in _members(annotation) if isinstance(member, type)]
    assert literal_values(annotation) == tags
    assert [literal_values(option.model_fields["type"].annotation) for option in options] == [tags]
    # the string form throws for an unknown tag, but the map form falls back to the default (an open enum)
    handling = sources.text("src/multio/util/FailureHandling.h")
    assert "Unsupported value" in handling and "throw FailureAwareException" in handling
    assert re.search(
        r"parsedOnErrTag_\s*=\s*errorTagMaybe\s*\?\s*\*errorTagMaybe\s*:\s*\w+::defaultOnErrorTag\(\)", handling
    )
    assert not accepts_any_string(annotation), "the string form of on-error must reject unknown tags"
    for option in options:
        assert option.cpp_open_enums == {"type"}
        assert accepts_any_string(option.model_fields["type"].annotation), "the map form accepts any type"


def test_mpi_communicator_types_match_cpp(sources):
    """The `type` of an MPI communicator accepts the strings that `parseType` compares with."""
    text = sources.text("src/multio/transport/MpiCommSetup.cc")
    body = re.search(r"parseType\s*\([^)]*\)\s*\{(.*?)\n\}", text, re.DOTALL).group(1)
    assert set(re.findall(r'"([^"]+)"', body)) == literal_values(plans.MpiCommunicator.model_fields["type"].annotation)


# Scanner and checks on synthetic text


def test_strip_comments_keeps_strings():
    text = 'a("http://x"); // getString("c1")\n/* getString("c2") */ b(\'"\');'
    stripped = strip_comments(text)
    assert "http://x" in stripped and "c1" not in stripped and "c2" not in stripped


@pytest.mark.parametrize(
    ("arg", "value"),
    [
        ("true", True),
        ("false", False),
        ("-1", -1),
        ("3600", 3600),
        ("273.15", 273.15),
        ("1e-3", 0.001),
        ("0.5f", 0.5),
        ('"info"', "info"),
        ('""', ""),
        ('std::string("x")', "x"),
        ('std::string{"y"}', "y"),
        ("std::numeric_limits<float>::max()", NONLITERAL),
        ("{}", NONLITERAL),
        ("defaultName", NONLITERAL),
    ],
)
def test_parse_literal(arg, value):
    assert parse_literal(arg) == value


def test_scan_text_accessor_styles():
    text = """
        a = cfg.getBool("b", false); c = cfg.getString("s", std::string("x")); d = cfg.getLong("l", -1);
        cfg.getDoubleVector("dv"); cfg.has("h"); cfg.isSubConfiguration("sc"); cfg.getSubConfigurations("subs");
        subComponents("sinks"); auto r = util::parseBool(cfg, "pb", false); parseBoolOption(cfg, "pbo");
        cf::optionalEntry("opt", &X::opt); cf::requiredEntry("req", &X::req); cfg.getString(key); cfg.getInt64("i64");
        cfg.getString("with-default", compute(1, 2)); auto e = util::parseEnabled(cfg, true);
    """
    found = {read.key: read for read in scan_text(text, "f")}
    assert set(found) == {
        "b", "s", "l", "dv", "h", "sc", "subs", "sinks", "pb", "pbo", "opt", "req", "i64", "with-default", "enable",
    }  # fmt: skip
    assert (found["enable"].kind, found["enable"].default) == ("bool", True)
    assert (found["b"].kind, found["b"].default) == ("bool", False)
    assert (found["s"].kind, found["s"].default) == ("str", "x")
    assert (found["l"].kind, found["l"].default) == ("int", -1)
    assert found["dv"].kind == "list[float]" and found["dv"].default is UNSET
    assert found["h"].kind is None
    assert (found["pb"].kind, found["pb"].default) == ("bool", False)
    assert found["pbo"].default is UNSET
    assert found["req"].entry.required and not found["opt"].entry.required
    assert found["with-default"].default is NONLITERAL


def test_scan_text_struct_members():
    text = """
        struct Out { std::string grid; bool enable = true; std::string cachePath = "."; std::optional<Level> level;
                     std::int64_t id; MIROptions options = {}; std::vector<Out> more;
          static constexpr auto fields_ = std::make_tuple(cf::requiredEntry("grid", &Out::grid),
              cf::optionalEntry("enable", &Out::enable), cf::optionalEntry("cache-path", &Out::cachePath),
              cf::optionalEntry("level", &Out::level), cf::requiredEntry("id", &Out::id),
              cf::optionalEntry("options", &Out::options), cf::optionalEntry("more", &Out::more)); };
    """
    found = {read.key: read for read in scan_text(text, "f")}
    assert (found["grid"].kind, found["grid"].default, found["grid"].entry.required) == ("str", UNSET, True)
    assert (found["enable"].kind, found["enable"].default) == ("bool", True)
    assert found["cache-path"].default == "."
    assert found["level"].entry.enum_type == "Level"
    assert found["id"].kind == "int"
    assert found["options"].default is NONLITERAL and found["more"].kind is None


def test_enum_extraction():
    text = """
        stream_ = cfg.getString("stream", "info"); if (stream_ == "info") {} else if ("mars" == stream_) {}
        const auto w = cfg.getString("window"); if (w != "a" && w != "b") {}
        template <> struct x::EnumTrait<ns::Preset> { static constexpr std::array values{
            std::pair{ns::Preset::A, "a-1"}, std::pair{ns::Preset::B, "b-2"}, }; };
    """
    assert comparison_literals(text, "stream") == {"info", "mars"}
    assert comparison_literals(text, "window") == {"a", "b"}
    assert enum_trait_values([text], "Preset") == {"a-1", "b-2"}


def test_enum_extraction_scope():
    shadowed = """
        void f() { if (c) { const std::string input = cfg.getString("grid"); if (input == "ll") {} }
                   if (d) { const std::string input = other(); if (input != "fesom") {} } }
        void g() { const auto opname = cfg.getString("grid"); }
        void h() { if (opname == "later") {} }
    """
    assert comparison_literals(shadowed, "grid") == {"ll"}
    members = 'A::A() : mode_{cfg.getString("mode")} {}\nvoid A::f() { if (mode_ == "fast") {} }'
    assert comparison_literals(members, "mode") == {"fast"}


# Guards: a broken scanner or a wrong citation must fail, not pass


class _Cited(MultioBaseModel):
    cpp_sources = ("src/multio/action/print/Print.cc",)
    cpp_open_enums = frozenset({"stream"})
    stream: Annotated[Literal["cout", "info", "error", "mars", "mars-misc"] | str, base.CONFIG_STR] = "info"


def test_missing_cpp_source_is_reported(sources):
    class Missing(_Cited):
        cpp_sources = ("src/multio/action/print/DoesNotExist.cc",)

    assert any("does not exist" in problem for problem in check_model(Missing, sources))


def test_model_without_extracted_keys_is_reported(sources):
    class NothingRead(_Cited):
        cpp_sources = ("src/multio/action/Action.h",)  # a header that reads no config key

    problems = check_model(NothingRead, sources)
    assert any("no config key could be extracted" in problem for problem in problems), problems


def test_nested_model_without_sources_is_reported(sources):
    class Inner(MultioBaseModel):
        value: int = 0

    class Outer(_Cited):
        inner: Inner | None = None

    assert any("declares no cpp_sources" in problem for problem in check_model(Outer, sources))


def test_extra_field_and_unlisted_key_are_reported(sources):
    class Extra(_Cited):
        prefix: base.ConfigStr = ""
        only_fields: base.ConfigBool = False
        invented: int = 1

    problems = check_model(Extra, sources)
    assert any("invented" in problem and "never read" in problem for problem in problems), problems
    assert len(problems) == 1, problems  # `stream`, `prefix` and `only-fields` are fine


def test_unlisted_cpp_key_is_reported(sources):
    problems = check_model(_Cited, sources)  # Print.cc also reads `prefix` and `only-fields`
    assert any("'prefix'" in problem and "not a model field" in problem for problem in problems), problems
    assert any("'only-fields'" in problem for problem in problems), problems


def test_cpp_ignore_silences_the_listed_keys_only(sources):
    class Ignored(_Cited):
        cpp_ignore = {"prefix": "reason", "only-fields": "reason"}

    assert check_model(Ignored, sources) == []

    class Partly(_Cited):
        cpp_ignore = {"prefix": "reason"}

    assert len(check_model(Partly, sources)) == 1


# Mutation tests: the scanned text is changed, the check must notice


def _replace(old: str, new: str) -> Callable[[str], str]:
    def mutate(text: str) -> str:
        assert old in text, f"mutation target {old!r} not found: the test needs updating"
        return text.replace(old, new)

    return mutate


def _append(code: str) -> Callable[[str], str]:
    return lambda text: text + "\n" + code + "\n"


@pytest.fixture(scope="module")
def mutated(sources):
    def run(model, mutations):
        return check_model(model, Sources(sources.root, mutations))

    return run


def test_unmutated_sources_are_clean(mutated):
    assert mutated(actions.Print, {}) == []
    assert mutated(actions.Statistics, {}) == []


def test_mutation_new_key_is_caught(mutated):
    problems = mutated(actions.Print, {"print/Print.cc": _append('void f(const C& c) { c.getString("new-key"); }')})
    assert any("'new-key'" in problem and "not a model field" in problem for problem in problems), problems


def test_mutation_changed_default_is_caught(mutated):
    problems = mutated(
        actions.Print, {"print/Print.cc": _replace('getBool("only-fields", false)', 'getBool("only-fields", true)')}
    )
    assert any("only_fields" in problem and "default" in problem for problem in problems), problems
    problems = mutated(
        actions.Print, {"print/Print.cc": _replace('getString("stream", "info")', 'getString("stream", "cout")')}
    )
    assert any("stream" in problem and "default" in problem for problem in problems), problems


def test_mutation_changed_strict_parser_default_is_caught(mutated):
    mutation = {"encode-mtg2/EncodeMtg2.h": _replace("bool cached = false;", "bool cached = true;")}
    problems = mutated(actions.EncodeMTG, mutation)
    assert any("cached" in problem and "default" in problem for problem in problems), problems


def test_mutation_removed_enum_literal_is_caught(mutated):
    problems = mutated(actions.Print, {"print/Print.cc": _replace('stream_ == "error"', "false")})
    assert any("stream" in problem and "error" in problem for problem in problems), problems
    problems = mutated(actions.Statistics, {"statistics/Operations.h": _replace('opname == "flux-average"', "false")})
    assert any("operations" in problem and "flux-average" in problem for problem in problems), problems
    problems = mutated(actions.StatisticsMTG, {"statistics-mtg2/Operations.h": _replace('opname == "stddev"', "false")})
    assert any("operations" in problem and "stddev" in problem for problem in problems), problems
    problems = mutated(actions.Encode, {"encode/Encode.cc": _replace('format == "raw"', "false")})
    assert any("format" in problem and "raw" in problem for problem in problems), problems
    # string-to-enum table of the strict parser
    problems = mutated(
        actions.Scale,
        {"scale/Scale.cc": _replace('ScalePreset::WmoToLocal, "wmo-to-local"', 'ScalePreset::WmoToLocal, "wmo"')},
    )
    assert any("preset" in problem and "wmo-to-local" in problem for problem in problems), problems


def test_mutation_added_enum_literal_is_caught(mutated):
    problems = mutated(
        actions.Print, {"print/Print.cc": _replace('stream_ == "error"', 'stream_ == "stderr" || stream_ == "error"')}
    )
    assert any("stream" in problem and "stderr" in problem for problem in problems), problems


def test_mutation_sink_default_changes_are_caught(mutated):
    problems = mutated(
        sinks.File, {"sink/FileSink.cc": _replace('getBool("append", false)', 'getBool("append", true)')}
    )
    assert any("File.append" in problem and "default" in problem for problem in problems), problems
    problems = mutated(
        sinks.File, {"sink/DataSink.cc": _replace('getBool("failOnError", true)', 'getBool("failOnError", false)')}
    )
    assert any("File.fail_on_error" in problem and "default" in problem for problem in problems), problems
    problems = mutated(
        sinks.Maestro, {"maestro/MaestroSink.cc": _replace('getBool("ready-cdo", true)', 'getBool("ready-cdo", false)')}
    )
    assert any("ready_cdo" in problem and "default" in problem for problem in problems), problems


def test_mutation_enable_helper_is_scanned(mutated):
    """`enable` is read through `parseEnabled`, without a literal key at the call."""
    mutation = {
        "sink/MultIO.cc": _replace(
            "parseEnabled(subComp.parsedConfig(), true)", "parseEnabled(subComp.parsedConfig(), false)"
        )
    }
    problems = mutated(sinks.File, mutation)
    assert any("File.enable" in problem and "default" in problem for problem in problems), problems
    problems = mutated(
        plans.Client,
        {"config/PlanConfiguration.cc": _replace("parseEnabled(cfg, true)", "parseEnabled(cfg, false)")},
    )
    assert any("Plan.enable" in problem and "default" in problem for problem in problems), problems


def test_mutation_new_keys_of_sinks_plans_and_servers_are_caught(mutated):
    new_key = _append('void f(const C& c) { c.getString("new-key"); }')
    cases = [
        (sinks.File, "sink/FileSink.cc"),
        (sinks.MultIO, "sink/Trigger.cc"),
        (plans.Client, "config/PlanConfiguration.cc"),
        (plans.Client, "server/MultioClient.cc"),
        (plans.Server, "server/Dispatcher.cc"),
        (plans.Server, "transport/TcpTransport.cc"),
        (plans.Server, "transport/MpiCommSetup.cc"),
    ]
    for model, suffix in cases:
        problems = mutated(model, {suffix: new_key})
        assert any("'new-key'" in problem and "not a model field" in problem for problem in problems), (
            suffix,
            problems,
        )


def test_mutation_transport_and_plan_changes_are_caught(mutated):
    problems = mutated(
        plans.Server,
        {"transport/MpiTransport.cc": _replace('getString("group", "multio")', 'getString("group", "world")')},
    )
    assert any("Server.group" in problem and "default" in problem for problem in problems), problems
    problems = mutated(
        plans.Client,
        {"config/PlanConfiguration.cc": _replace('getBool("per-server", true)', 'getBool("per-server", false)')},
    )
    assert any("DisseminationPlan.per_server" in problem and "default" in problem for problem in problems), problems
    problems = mutated(
        plans.Client,
        {
            "config/PlanConfiguration.cc": _replace(
                'getString("templates-path", ".")', 'getString("templates-path", "t")'
            )
        },
    )
    assert any("templates_path" in problem and "default" in problem for problem in problems), problems


def test_mutation_unread_field_is_caught(mutated):
    """A field whose reader disappears from C++ is reported."""
    problems = mutated(
        plans.Client,
        {
            "config/PlanConfiguration.cc": lambda text: _replace('componentConfig.has("output-name")', "false")(
                _replace('componentConfig.getString("output-name")', "std::string()")(text)
            )
        },
    )
    assert any("DisseminationPlan.output_name" in problem and "never read" in problem for problem in problems), problems


# Keys are checked against the model that owns the reader, never against the merged keys of the models in the tree


def test_mutation_key_of_another_model_does_not_hide_a_missing_field(mutated):
    """`File` has a `path` field, but `Client` reads PlanConfiguration.cc, which `File` does not: a new `path` is drift."""
    assert "path" in aliases(sinks.File)
    assert "path" not in {
        key for model in (plans.Client, plans.Plan, plans.DisseminationPlan) for key in aliases(model)
    }
    problems = mutated(
        plans.Client,
        {"config/PlanConfiguration.cc": _append('void f(const C& c) { c.getString("path"); }')},
    )
    assert any(
        "Client" in problem and "'path'" in problem and "not a model field" in problem for problem in problems
    ), problems
    # the file sink itself reads `path`, which is its field
    assert not any("'path'" in problem for problem in mutated(plans.Client, {}))


def test_mutation_key_of_a_nested_model_that_does_not_share_the_reader_is_caught(mutated):
    """`File` is nested in `MultIO`, but does not read Trigger.cc: its `append` must not excuse a trigger key."""
    assert "append" in aliases(sinks.File) and "append" not in aliases(sinks.MultIO)
    problems = mutated(sinks.MultIO, {"sink/Trigger.cc": _append('void f(const C& c) { c.getBool("append"); }')})
    assert any("MultIO" in problem and "'append'" in problem for problem in problems), problems


def test_mutation_key_of_an_option_model_that_shares_the_reader_is_accepted(mutated):
    """The option models of `interpolate-mtg2` share the header of the action: their keys are the action's to read."""
    assert mutated(actions.InterpolateMTG, {}) == []
    assert mutated(actions.Statistics, {}) == []
    problems = mutated(
        actions.InterpolateMTG,
        {"interpolate-mtg2/InterpolateMtg2.h": _append('void f(const C& c) { c.getString("new-key"); }')},
    )
    assert any("InterpolateMTG" in problem and "'new-key'" in problem for problem in problems), problems


def test_each_model_is_checked_against_its_own_files():
    """`owned_keys` merges nothing: the keys of a model that does not list the file are not part of the result"""
    file = "src/multio/config/PlanConfiguration.cc"
    owned = owned_keys(plans.Client, file)
    assert {"plans", "name", "actions", "dissemination", "templates", "per-server"} <= owned
    assert "path" not in owned and "append" not in owned
    assert sharing_models(plans.Client, file) == [plans.Plan, plans.DisseminationPlan]
    assert sinks.File not in sharing_models(plans.Client, file)


# Open and closed enums


def test_closed_enum_is_closed_in_cpp(sources):
    """The enums that the models check against `Literal`s, reviewed: C++ throws for any other string"""
    closed = [
        (actions.Statistics, "operations"),
        (actions.StatisticsMTG, "operations"),
        (actions.Encode, "format"),
        (actions.StatisticsOptions, "window_type"),
        (actions.StatisticsOptions, "solver_reset_accumulate_fields_every"),
        (actions.StatisticsMTGOptions, "window_type"),
        (actions.StatisticsMTGOptions, "output_time_reference"),
        (actions.Scale, "preset_mappings"),
        (actions.InterpolateMTGAdditionalMetadata, "resolution"),
    ]
    for model, name in closed:
        assert not model.cpp_open_enums
        value = [UNKNOWN_VALUE] if name == "operations" else UNKNOWN_VALUE  # `operations` is a list of enum values
        with pytest.raises(ValidationError):
            TypeAdapter(model.model_fields[name].annotation).validate_python(value)
        problems = [problem for problem in check_model(model, sources) if f".{name}" in problem]
        assert problems == [], problems


def test_open_enum_print_stream(sources):
    assert actions.Print.cpp_open_enums == {"stream"}
    assert accepts_any_string(actions.Print.model_fields["stream"].annotation)
    assert check_model(actions.Print, sources) == []


def test_enclosing_function_finds_the_body_that_compares():
    text = """
        A::A(const C& c) : B(c) { s_ = c.getString("k"); if (s_ == "x") { f(); } else { g(); } }
        void h(const C& c) { const auto k = c.getString("k"); if (k != "y") { throw E(); } }
    """
    sites = comparison_sites(text, "k")
    assert {literal for literal, _ in sites} == {"x", "y"}
    by_literal = dict(sites)
    assert "throw" not in enclosing_function(text, by_literal["x"])  # the `if` block is not a function
    assert "g()" in enclosing_function(text, by_literal["x"])
    assert "throw" in enclosing_function(text, by_literal["y"])
    assert not throws_after_comparing(text, [("x", by_literal["x"])])
    assert throws_after_comparing(text, [("y", by_literal["y"])])


def test_closed_enum_that_falls_through_in_cpp_is_reported(sources):
    """A `Literal` of a key that C++ does not reject is wrong: the model must list it in `cpp_open_enums`"""

    class Closed(_Cited):
        cpp_open_enums = frozenset()
        stream: Literal["cout", "info", "error", "mars", "mars-misc"] = "info"

    problems = check_model(Closed, sources)
    assert any("'stream'" in problem and "falls through" in problem for problem in problems), problems
    assert any("'stream'" in problem and "cpp_open_enums" in problem for problem in problems), problems


def test_open_enum_that_rejects_strings_is_reported(sources):
    class Rejecting(_Cited):
        stream: Literal["cout", "info", "error", "mars", "mars-misc"] = "info"

    problems = check_model(Rejecting, sources)
    assert any("'stream'" in problem and "rejects unknown strings" in problem for problem in problems), problems


def test_mutation_open_enum_that_cpp_closes_is_reported(mutated):
    """If C++ starts to throw for an unknown stream, `Print.stream` is not open any more"""
    problems = mutated(
        actions.Print, {"print/Print.cc": _replace("os_ = &eckit::Log::debug();", 'throw eckit::BadValue("stream");')}
    )
    assert any(
        "'stream'" in problem and "declared in cpp_open_enums, but C++ throws" in problem for problem in problems
    ), problems


def test_mutation_closed_enum_that_cpp_opens_is_reported(mutated):
    """If C++ stops throwing for an unknown window type, the model must accept it"""
    mutation = {
        "statistics/cfg/StatisticsOptions.cc": lambda text: text.replace(
            "throw eckit::UserError(os.str(), Here());", ""
        )
    }
    problems = mutated(actions.Statistics, mutation)
    assert any("'window-type'" in problem and "falls through" in problem for problem in problems), problems


def test_open_enum_key_must_be_a_field(sources):
    class Typo(_Cited):
        cpp_open_enums = frozenset({"stream", "strem"})

    assert any("'strem'" in problem and "not a field" in problem for problem in check_model(Typo, sources))


# Conversions of the readers


def test_field_with_the_wrong_conversion_is_reported(sources):
    class Plain(_Cited):
        prefix: str = ""
        only_fields: bool = False  # read with getBool: needs `ConfigBool`

    problems = check_model(Plain, sources)
    assert any("Plain.only_fields" in problem and "config_bool" in problem for problem in problems), problems

    class Wrong(_Cited):
        prefix: str = ""
        only_fields: base.ParsedBool = False  # parseBool is not the conversion of getBool

    assert any("Wrong.only_fields" in problem and "config_bool" in problem for problem in check_model(Wrong, sources))


def test_mutation_reader_that_changes_its_conversion_is_reported(mutated):
    """`write-restart` is read with `parseBool`; if C++ read it with `getBool`, the type would have to change"""
    mutation = {
        "statistics/cfg/StatisticsOptions.cc": _replace(
            'util::parseBool(cfg, "write-restart", false)', 'cfg.getBool("write-restart", false)'
        )
    }
    problems = mutated(actions.Statistics, mutation)
    assert any("write_restart" in problem and "config_bool" in problem for problem in problems), problems


def test_getstring_field_with_a_plain_str_is_reported(sources):
    """`getString` converts numbers, so a strict `str` accepts less than C++: `prefix` and `stream` must be ConfigStr"""

    class Plain(_Cited):
        prefix: str = ""  # read with getString
        only_fields: base.ConfigBool = False
        stream: str = "info"

    problems = check_model(Plain, sources)
    assert any(
        "Plain.prefix" in problem and "getString('prefix')" in problem and "config_str" in problem
        for problem in problems
    ), problems
    assert any("Plain.stream" in problem and "config_str" in problem for problem in problems), problems

    class Converting(_Cited):
        prefix: base.ConfigStr = ""
        only_fields: base.ConfigBool = False

    assert check_model(Converting, sources) == []

    class Wrong(_Cited):
        prefix: base.ConfigInt = 0  # a converter, but not that of getString
        only_fields: base.ConfigBool = False

    assert any("Wrong.prefix" in problem and "config_str" in problem for problem in check_model(Wrong, sources))


def test_getstringvector_field_with_a_plain_list_is_reported(sources):
    class Plain(actions.Mask):
        offset_fields: list[str] = []

    problems = check_model(Plain, sources)
    assert any(
        "Plain.offset_fields" in problem and "getStringVector" in problem and "config_str" in problem
        for problem in problems
    ), problems
    assert [problem for problem in check_model(actions.Mask, sources) if "offset_fields" in problem] == []


def test_mutation_bool_reader_that_becomes_a_string_reader_is_reported(mutated):
    """If C++ read `only-fields` with `getString`, the `ConfigBool` would no longer be the conversion that C++ does"""
    mutation = {"print/Print.cc": _replace('getBool("only-fields", false)', 'getString("only-fields", "false")')}
    problems = mutated(actions.Print, mutation)
    assert any("only_fields" in problem and "config_str" in problem for problem in problems), problems


def test_mutation_string_reader_that_becomes_a_bool_reader_is_reported(mutated):
    mutation = {"print/Print.cc": _replace('getString("prefix", "")', 'getBool("prefix", false)')}
    problems = mutated(actions.Print, mutation)
    assert any("Print.prefix" in problem and "config_bool" in problem for problem in problems), problems


def test_strict_parser_strings_are_not_converted(sources):
    """The strict parser reads a string only when `isString` says so: a `ConfigStr` there accepts what C++ rejects"""

    class Converting(actions.InterpolateMTGOutput):
        grid: base.ConfigStr

    problems = check_model(Converting, sources)
    assert any("Converting.grid" in problem and "reads strings only" in problem for problem in problems), problems
    assert [problem for problem in check_model(actions.InterpolateMTGOutput, sources) if ".grid" in problem] == []


def test_string_exemptions(sources):
    """The discriminators and the `isString` guarded keys are the only fields read with getString that do not convert"""
    assert set(actions.Interpolate.cpp_strict_strings) == {"grid"}
    exempt = {
        (model.__name__, key)
        for root in [*ACTION_MODELS, *SINK_MODELS, *CONFIG_MODELS]
        for model in nested_models(root)
        for key, name in aliases(model).items()
        if string_exemption(model, key, model.model_fields[name]) is not None
    }
    assert {key for _, key in exempt} == {"type", "grid"}
    assert ("Interpolate", "grid") in exempt and ("Print", "type") in exempt
    # the guard is real: Interpolate.cc checks isString("grid") before it reads the string
    assert any(
        read.accessor == "isString" and read.key == "grid"
        for read in scan_text(sources.text(CPP_PREFIX + "action/interpolate/Interpolate.cc"), "f")
    )


def test_strict_string_entry_needs_a_guard_in_cpp_and_a_field_that_does_not_convert(sources):
    class Guarded(_Cited):
        prefix: base.ConfigStr = ""
        only_fields: base.ConfigBool = False
        cpp_strict_strings = {"prefix": "C++ checks isString first, so a number is never converted"}

    problems = check_model(Guarded, sources)
    assert any("no isString('prefix') check" in problem for problem in problems), problems
    assert any("never converts 'prefix'" in problem for problem in problems), problems

    class Typo(_Cited):
        prefix: base.ConfigStr = ""
        only_fields: base.ConfigBool = False
        cpp_strict_strings = {"prefx": "C++ checks isString first, so a number is never converted"}

    assert any("'prefx', which is not a field" in problem for problem in check_model(Typo, sources))

    class Interp(actions.Interpolate):
        grid: base.ConfigStr | None = None

    assert any("never converts 'grid'" in problem for problem in check_model(Interp, sources))


def test_mutation_guard_removed_in_cpp_is_reported(mutated):
    """If C++ stops checking `isString("grid")`, the exemption of `Interpolate.grid` is stale"""
    mutation = {"interpolate/Interpolate.cc": lambda text: text.replace('isString("grid")', 'isFloatingPoint("grid")')}
    problems = mutated(actions.Interpolate, mutation)
    assert any("no isString('grid') check" in problem for problem in problems), problems


def test_every_converted_field_is_checked_against_its_reader(sources):
    """The conversions are enforced where a field has a reader of a known kind: bool, int, float, string, strict parser"""
    checked, kinds = 0, set()
    for root in [*ACTION_MODELS, *SINK_MODELS, *CONFIG_MODELS]:
        for model in nested_models(root):
            for key, name in aliases(model).items():
                field = model.model_fields[name]
                if key in model.cpp_ignore or not model.cpp_sources:
                    continue
                exempt = string_exemption(model, key, field) is not None
                for rel in model.cpp_sources:
                    text = sources.text(rel)
                    for read in scan_text(text, rel) if text else []:
                        converter = expected_converter(read, exempt)
                        if read.key == key and converter is not None:
                            checked += 1
                            kinds.add(converter)
                            assert converter in converters_of(field), (model, name)
    assert checked > 150
    assert {base.config_str, base.config_bool, base.config_int, base.config_float, base.strict_int} <= kinds
