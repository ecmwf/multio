# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""Multio Plans Base Model"""

import math
import re
from typing import Annotated, Any, ClassVar, Literal

from pydantic import AliasGenerator, BaseModel, BeforeValidator, ConfigDict, Field

# ----------------------------------------------------------------------------------------------------------------------
# Value conversions of C++
#
# C++ reads a config from YAML, replaces `{NAME}` placeholders in strings (so a value that came from an environment
# variable is always a string: it is never parsed again, `MultioConfiguration.cc:223-247`) and then converts each value
# with the accessor of its reader. The types below do the same conversions, so that the strict parsing of `from_yaml`
# accepts what C++ accepts and no more. Each type names its C++ reader; the contract test checks that every field
# read through an accessor has the type of that accessor.

_TRUE_WORDS = ("true", "on", "yes", "1")
_FALSE_WORDS = ("false", "off", "no", "0")
_INTEGER = re.compile(r"\s*[+-]?[0-9]+")


def translate_bool(text: str) -> bool:
    """eckit `Translator<std::string, bool>`: used by `util::parseBool` and `util::parseEnabled` for strings.

    `yes`, `on`, `true` and `1` are true, `no`, `off`, `false` and `0` false (any case), any other string is read like
    `atoi`: the leading integer decides, and a string without one is false.
    """
    lowered = text.lower()
    if lowered in _FALSE_WORDS:
        return False
    if lowered in _TRUE_WORDS:
        return True
    match = re.match(r"\s*[-+]?[0-9]+", lowered)
    return bool(match) and int(match.group()) != 0


def _is_int(value: Any) -> bool:
    return isinstance(value, int) and not isinstance(value, bool)


def config_bool(value: Any) -> bool:
    """`Configuration::getBool`: a boolean, a number (non-zero is true) or a string that eckit reads as a boolean.

    The strings are `true`, `on`, `yes`, `1` and `false`, `off`, `no`, `0` in any case; eckit throws for any other.
    A float is not convertible.
    """
    if isinstance(value, bool):
        return value
    if _is_int(value):
        return value != 0
    if isinstance(value, str):
        lowered = value.lower()
        if lowered in _TRUE_WORDS:
            return True
        if lowered in _FALSE_WORDS:
            return False
    raise ValueError(f"{value!r} is not a boolean for C++ `getBool` (true/false, on/off, yes/no, 0/1 or a number)")


def parsed_bool(value: Any) -> bool:
    """`util::parseBool` (`Substitution.cc:19-30`): a boolean, or a string converted by `translate_bool`.

    A number is rejected: C++ only looks at strings and booleans and fails with "Unable to read ...".
    """
    if isinstance(value, bool):
        return value
    if isinstance(value, str):
        return translate_bool(value)
    raise ValueError(f"{value!r} is not a boolean for C++ `parseBool`: a boolean or a string is required")


_LONG_PREFIX = re.compile(r"[ \t\n\v\f\r]*([+-]?[0-9]+)")
_SIZE_SUFFIX = re.compile(r"[ \t\n\v\f\r]*([a-zA-Z])(?:[bB]|[iI][bB])")
_SIZE_UNITS = {"k": 10, "m": 20, "g": 30, "t": 40, "p": 50, "e": 60}
_INT64_MIN, _INT64_MAX = -(2**63), 2**63 - 1


def translate_long(text: str) -> int:
    """eckit `Translator<std::string, long long>` (`utils/Translator.cc`): `strtoll` times a size suffix.

    The leading integer is read like `strtoll` (a string without one is 0, a number out of range is clamped) and
    the text after it may be a size suffix: `k`, `m`, `g`, `t`, `p` or `e`, followed by `b` or `ib`, in any case,
    multiplies by 1024 to the power 1 to 6 (`10KB` is 10240). Any other trailing text is ignored.
    """
    match = _LONG_PREFIX.match(text)
    if not match:
        return 0
    result = max(_INT64_MIN, min(_INT64_MAX, int(match.group(1))))
    suffix = _SIZE_SUFFIX.match(text, match.end())
    if suffix and suffix.group(1).lower() in _SIZE_UNITS:
        result <<= _SIZE_UNITS[suffix.group(1).lower()]
        result = (result + 2**63) % 2**64 - 2**63  # the multiplication wraps in C++
    return result


def config_int(value: Any) -> int:
    """`Configuration::getLong` / `getInt`: an integer, a boolean (0 or 1), a whole float or a string.

    A string goes through `translate_long`, so any string is accepted (`abc` is 0), as in C++. A float that is not a
    whole number, and anything else, is rejected.
    """
    if isinstance(value, bool):
        return int(value)
    if _is_int(value):
        return value
    if isinstance(value, float) and value.is_integer():
        return int(value)
    if isinstance(value, str):
        return translate_long(value)
    raise ValueError(f"{value!r} is not an integer for C++ `getLong`/`getInt`")


def config_float(value: Any) -> float:
    """`Configuration::getDouble`: a number, a boolean (0 or 1) or a string that is a whole decimal number."""
    if isinstance(value, bool):
        return float(value)
    if isinstance(value, (int, float)):
        return float(value)
    if isinstance(value, str) and value == value.strip() and "_" not in value:
        try:
            return float(value)
        except ValueError:
            pass
    raise ValueError(f"{value!r} is not a number for C++ `getDouble`")


def _format_double(value: float) -> str:
    """`std::ostream << double` with the default format, which is `%g` with 6 significant digits."""
    if math.isnan(value):
        return "-nan" if math.copysign(1.0, value) < 0 else "nan"
    return format(value, ".6g")


def config_str(value: Any) -> str:
    """`Configuration::getString` and `getStringVector` (eckit `Value` to `std::string`, `*Content::value`).

    A string is kept. A number, a boolean or a list of one element are converted to text, any other value is an error:

    - an integer is written in decimal (`130` is `"130"`), clamped to the 64 bit range as C++ reads it;
    - a float is written like `std::ostream`: 6 significant digits, `130.0` is `"130"`, `1234567.0` is `"1.23457e+06"`,
      `1e-5` is `"1e-05"`, `0.1` is `"0.1"`;
    - a boolean is `"true"` or `"false"`;
    - a list is converted through its element when it has exactly one (`[130]` is `"130"`, `[[7]]` is `"7"`), an empty
      list or one with more elements is an error (`Cannot convert (1,2) (List) to std::string`);
    - a map and a null are errors.

    `getStringVector` converts each element of a list in the same way, and a value that is not a list is an error.
    """
    if isinstance(value, str):
        return value
    if isinstance(value, bool):
        return "true" if value else "false"
    if _is_int(value):
        return str(max(_INT64_MIN, min(_INT64_MAX, value)))
    if isinstance(value, float):
        return _format_double(value)
    if isinstance(value, list) and len(value) == 1:
        return config_str(value[0])
    raise ValueError(
        f"{value!r} cannot be converted to a string by C++ `getString`: only a string, a number, a boolean or a list "
        "of one of those is accepted"
    )


def strict_bool(value: Any) -> bool:
    """Strict parser (`detail/Parser.cc` `parseEntry(bool&)`): a boolean, the integers 0 and 1 or the strings "0", "1"."""
    if isinstance(value, bool):
        return value
    if _is_int(value) and value in (0, 1):
        return bool(value)
    if isinstance(value, str) and value in ("0", "1"):
        return value == "1"
    raise ValueError(f"{value!r} is not a boolean for the strict parser of C++: use a boolean, 0, 1, '0' or '1'")


def strict_int(value: Any) -> int:
    """Strict parser (`parseEntry(int64&)`): an integer, or a string that is entirely an integer. A float is rejected."""
    if _is_int(value):
        return value
    if isinstance(value, str) and _INTEGER.fullmatch(value):
        return int(value)
    raise ValueError(f"{value!r} is not an integer for the strict parser of C++")


def strict_float(value: Any) -> float:
    """Strict parser (`parseEntry(double&)`): a float or an integer, or a string that is entirely a number.

    `std::stod` skips leading white space but `pos != size` rejects trailing text, white space included.
    """
    if isinstance(value, bool):
        raise ValueError(f"{value!r} is not a number for the strict parser of C++")
    if isinstance(value, (int, float)):
        return float(value)
    if isinstance(value, str) and not value[-1:].isspace() and "_" not in value:
        try:
            return float(value)
        except ValueError:
            pass
    raise ValueError(f"{value!r} is not a number for the strict parser of C++")


# Each type converts like the C++ accessor named in its first word, and the model then holds the converted value
ConfigBool = Annotated[bool, BeforeValidator(config_bool)]  # getBool
ParsedBool = Annotated[bool, BeforeValidator(parsed_bool)]  # util::parseBool
ConfigInt = Annotated[int, BeforeValidator(config_int)]  # getLong, getInt
ConfigFloat = Annotated[float, BeforeValidator(config_float)]  # getDouble
# `CONFIG_STR` is for annotations that are not `str`, such as `Annotated[Literal[...], CONFIG_STR]`
CONFIG_STR = BeforeValidator(config_str)
ConfigStr = Annotated[str, CONFIG_STR]  # getString, getStringVector (per element)
# the strict parser of scale, encode-mtg2 and interpolate-mtg2, where a string field takes a string and nothing else:
StrictParserBool = Annotated[bool, BeforeValidator(strict_bool)]
StrictParserInt = Annotated[int, BeforeValidator(strict_int)]
StrictParserFloat = Annotated[float, BeforeValidator(strict_float)]

# C++ accessor kinds (`read.accessor` of the contract test) and the converter that its fields must use
CONVERTERS = {
    "getBool": config_bool,
    "parseBool": parsed_bool,
    "parseBoolOption": parsed_bool,
    "getInt": config_int,
    "getLong": config_int,
    "getDouble": config_float,
    "getString": config_str,
    "getStringVector": config_str,
}


class MultioBaseModel(BaseModel):
    """Multio Base Model

    Sets up a common configuration for all Multio models.

    Models that mirror C++ configuration declare these class variables, which the C++ contract test reads:

    - `cpp_sources`: repo-relative paths of the C++ files that read the keys of this model.
    - `cpp_ignore`: `{key: reason}` for keys read in those files that are not config keys of this model
      (metadata keys, keys of shared helpers) or that cannot be checked statically.
    - `cpp_open_enums`: keys whose value is a string that C++ compares with known values but accepts when it
      matches none of them (a fall-through `else`). The field must accept any string, the known values are
      documented in its description or as a `Literal | str` union.
    - `cpp_strict_strings`: `{key: reason}` for keys that C++ reads with `getString` only after an `isString` check, so
      that a value that is not a string is never converted (every other field read with `getString` is a `ConfigStr`).

    A family of models that share C++ readers (the actions, the sinks, the client and server configs) sets
    `cpp_merge_parents = True` on its base. Every model below it then lists only what it adds to `cpp_sources` and
    `cpp_ignore`: the declaration is merged with those of its parents (the parents first, the sources without
    duplicates, the ignore reason of the model winning over the one of a parent). A `cpp_ignore` entry whose value is
    `None` removes the entry that the parents give for that key, for a model that reads a key that its parents ignore.
    Without the flag a model that declares `cpp_sources` or `cpp_ignore` replaces what it inherits.
    """

    cpp_sources: ClassVar[tuple[str, ...]] = ()
    cpp_ignore: ClassVar[dict[str, str | None]] = {}
    cpp_open_enums: ClassVar[frozenset[str]] = frozenset()
    cpp_strict_strings: ClassVar[dict[str, str]] = {}
    cpp_merge_parents: ClassVar[bool] = False

    @classmethod
    def __pydantic_init_subclass__(cls, **kwargs: Any) -> None:
        super().__pydantic_init_subclass__(**kwargs)
        if cls.cpp_merge_parents:
            cls._merge_cpp_declarations()

    @classmethod
    def _merge_cpp_declarations(cls) -> None:
        """Add the `cpp_sources` and `cpp_ignore` of the model to those of its parents, see the class docstring."""
        parents = [parent for parent in cls.__bases__ if issubclass(parent, MultioBaseModel)]
        sources = dict.fromkeys(path for parent in parents for path in parent.cpp_sources)
        sources.update(dict.fromkeys(vars(cls).get("cpp_sources", ())))
        ignore: dict[str, str | None] = {}
        for parent in reversed(parents):  # the first parent wins
            ignore.update(parent.cpp_ignore)
        ignore.update(vars(cls).get("cpp_ignore", {}))
        cls.cpp_sources = tuple(sources)
        cls.cpp_ignore = {key: reason for key, reason in ignore.items() if reason is not None}

    model_config = ConfigDict(
        extra="forbid",
        serialize_by_alias=True,
        validate_by_name=True,
        validate_by_alias=True,
        alias_generator=AliasGenerator(
            alias=lambda field_name: field_name.replace("_", "-"),
        ),
    )


# ----------------------------------------------------------------------------------------------------------------------
# `on-error` and its relatives (util/FailureHandling.h)
#
# Every failure-aware component of C++ (`FailureAware<ComponentFailureTraits>`) reads its key the same way: a string is
# a tag that throws "Unsupported value" when it is not one of the tags of the component, a map has a `type` and
# `maxRetries`. Only the tags differ (the `TagSequence` of the traits of the component, a test compares them):
#
# - `OnErrorTag`: `propagate` and `recover`, for the `on-error` of an action (action/Action.h) and of a plan
#   (action/Plan.h);
# - `FailureTag`: `propagate`, `recover` and `abort-transport`, for the `on-error` of a client (server/MultioClient.h)
#   and of a server (server/MultioServer.h);
# - `PropagateTag`: `propagate` only, for `on-receive-error` (server/Listener.h) and `on-dispatch-error`
#   (server/Dispatcher.h).


def _lower(value: Any) -> str:
    """C++ reads an `on-error` tag with `getString` (a number or a list of one element converts) and compares it
    case-insensitively. A value that does not convert, such as a map, raises, so that the union moves on."""
    return config_str(value).lower()


OnErrorTag = Annotated[Literal["propagate", "recover"], BeforeValidator(_lower)]
FailureTag = Annotated[Literal["propagate", "recover", "abort-transport"], BeforeValidator(_lower)]
PropagateTag = Annotated[Literal["propagate"], BeforeValidator(_lower)]


class _OnErrorMap(MultioBaseModel):
    """Map form of the failure handling keys: a tag with options (util/FailureHandling.h).

    A model per set of tags derives from it and only narrows `type`, which keeps its place among the fields.
    """

    cpp_sources = ("src/multio/util/FailureHandling.h",)
    # C++ reads `type` of the map form with `getString` and falls back to `propagate` when it is none of the tags
    # (FailureHandling.h, `parsedOnErrTag_ = errorTagMaybe ? *errorTagMaybe : defaultOnErrorTag()`), unlike the string
    # form, which throws "Unsupported value". The known tags are those of the subclass.
    cpp_open_enums = frozenset({"type"})

    type: ConfigStr
    max_retries: ConfigInt | None = Field(None, alias="maxRetries")


class OnErrorOptions(_OnErrorMap):
    """Map form of `on-error`: a tag with options (util/FailureHandling.h)."""

    type: OnErrorTag | ConfigStr


class OnServerErrorOptions(_OnErrorMap):
    """Map form of `on-error` of the client and the server: a tag with options (util/FailureHandling.h)."""

    type: FailureTag | ConfigStr


class PropagateOnErrorOptions(_OnErrorMap):
    """Map form of `on-receive-error` and `on-dispatch-error` (util/FailureHandling.h)."""

    type: PropagateTag | ConfigStr
