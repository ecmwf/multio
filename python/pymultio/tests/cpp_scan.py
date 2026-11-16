# (C) Copyright 2026 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""Scanner of the C++ sources and the comparison of a plan model with them, for the contract and registry tests.

`test_cpp_contract` documents what is compared. This module has no tests: it strips comments, extracts the config keys,
defaults, converters and enum strings that C++ reads, and checks a model and the models nested in it against them.
"""

import ast
import re
import types
import typing
from dataclasses import dataclass
from pathlib import Path
from typing import Annotated, Any, Callable, Literal, Union

from pydantic import BaseModel, BeforeValidator, TypeAdapter, ValidationError

from multio.plans import base
from multio.plans.base import MultioBaseModel

from multio_testing import CPP_PREFIX, REPO_ROOT

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
