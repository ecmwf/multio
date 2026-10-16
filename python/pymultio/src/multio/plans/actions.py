# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""
Multio Actions

Defined as Pydantic models for type validation.

Every model mirrors the configuration read by the C++ action of the same name. Two class variables record this:

- `cpp_sources`: the C++ files (repo-relative) that read the keys of the model.
- `cpp_ignore`: `{key: reason}` for keys that are read in those files but are not config keys of the model, or that
  cannot be checked statically.

Nested option models carry their own class variables. The keys of nested models belong to the key set of the model
that contains them, because the C++ reader files are shared.
"""

from __future__ import annotations

from typing import Any, Literal, Union

from pydantic import (
    BeforeValidator,
    ConfigDict,
    Field,
    StrictInt,
    model_validator,
    validate_call,
)
from typing_extensions import Annotated

from .base import (
    CONFIG_STR,
    ConfigBool,
    ConfigFloat,
    ConfigInt,
    ConfigStr,
    MultioBaseModel,
    ParsedBool,
    StrictParserBool,
    StrictParserFloat,
    StrictParserInt,
    config_str,
)
from .sinks import SinksType, TriggersType

# Files read by every action (framework keys `type` and `next`), relative to src/multio
_COMMON_SOURCES = ("action/Action.cc", "action/ChainedAction.cc", "util/FailureHandling.h")

_COMMON_IGNORE = {"next": "internal key, generated from the order of the actions in a plan; users must not set it"}

_ON_ERROR_IGNORE = {
    "on-error": "the key name comes from FailureAware::configKey() in action/Action.h, not from a literal accessor",
}

# FailureHandling.h is shared by all actions, but the strict parser rejects `on-error` and with it its `maxRetries`
_STRICT_PARSER_IGNORE = {
    **_COMMON_IGNORE,
    "next": "removed by parseActionConfig before the strict parser runs",
    "maxRetries": "option of `on-error`, which the strict parser rejects as an unknown key",
}


def _sources(*paths: str) -> tuple[str, ...]:
    """Repo-relative C++ reader files: the common action files plus `paths` (relative to src/multio)."""
    return tuple(f"src/multio/{path}" for path in (*_COMMON_SOURCES, *paths))


def _lower(value: Any) -> str:
    """C++ reads an `on-error` tag with `getString` (a number or a list of one element converts) and compares it
    case-insensitively. A value that does not convert, such as a map, raises, so that the union moves on."""
    return config_str(value).lower()


def _not_bool(value: Any) -> Any:
    """Booleans are not numbers for C++ (a list of booleans is rejected), although they are for Python."""
    if isinstance(value, bool):
        raise ValueError("a number is required, not a boolean")
    return value


def _float_only(value: Any) -> Any:
    """C++ only reads a list as doubles when its first element is a float: `[1, 1]` is an integer list."""
    if not isinstance(value, float):
        raise ValueError("a float is required, C++ rejects integer lists: write `1.0` instead of `1`")
    return value


NonBoolFloat = Annotated[float, BeforeValidator(_not_bool)]
FloatOnly = Annotated[float, BeforeValidator(_float_only)]

OnErrorTag = Annotated[Literal["propagate", "recover"], BeforeValidator(_lower)]


class OnErrorOptions(MultioBaseModel):
    """Map form of `on-error`: a tag with options (util/FailureHandling.h)."""

    cpp_sources = ("src/multio/util/FailureHandling.h",)
    cpp_open_enums = frozenset({"type"})

    # C++ reads `type` of the map form with `getString` and falls back to `propagate` when it is none of the tags
    # (FailureHandling.h, `parsedOnErrTag_ = errorTagMaybe ? *errorTagMaybe : defaultOnErrorTag()`), unlike the string
    # form, which throws "Unsupported value". The known tags are `propagate` and `recover`.
    type: OnErrorTag | ConfigStr
    max_retries: ConfigInt | None = Field(None, alias="maxRetries")


class Action(MultioBaseModel):
    """Base Action class.

    Contains only a `type` field.

    Should not be instantiated directly, use one of the subclasses instead.
    """

    cpp_sources = _sources()
    cpp_ignore = {**_COMMON_IGNORE}

    type: Any = Field(..., description="Action type")


class FailureAwareAction(Action):
    """Base class of the actions that accept the common `on-error` key.

    Every action except those read with the strict parser (`scale`, `encode-mtg2` and `interpolate-mtg2`), which
    reject `on-error` as an unknown key.
    """

    cpp_ignore = {**_COMMON_IGNORE, **_ON_ERROR_IGNORE}

    on_error: OnErrorTag | OnErrorOptions | None = Field(
        None, description="Failure handling: `propagate` (default) or `recover`, optionally with `maxRetries`"
    )


class Null(FailureAwareAction):
    """Null Action.

    Does nothing.
    """

    cpp_sources = _sources("action/null/Null.cc")

    type: Literal["null"] = Field("null", init=False)


# Metadata matching values: a scalar, or a homogeneous list of integers, floats or strings
MatchValue = Union[bool, int, float, str, list[StrictInt], list[NonBoolFloat], list[str]]
MatchKeys = dict[str, MatchValue]


def _only_one_matcher_group(model: SelectMatcher) -> SelectMatcher:
    groups = {
        "match/ignore": model.match is not None or model.ignore is not None,
        "any": model.any is not None,
        "all": model.all is not None,
        "not": model.not_ is not None,
    }
    present = [name for name, given in groups.items() if given]
    if len(present) != 1:
        raise ValueError(f"exactly one of match/ignore, any, all or not is required, got: {present or 'none'}")
    return model


class SelectMatcher(MultioBaseModel):
    """A metadata matcher block of the `select` action, as read by C++ `MatchReduce::construct`.

    Exactly one of `match`/`ignore` (one or both), `any`, `all` or `not`. Nested blocks inherit
    `enforce-same-key-types` from their parent.
    """

    cpp_sources = ("src/multio/message/MetadataMatcher.cc",)

    match: MatchKeys | list[MatchKeys] | None = Field(None, description="Keys to match, a map or list of maps (OR)")
    ignore: MatchKeys | list[MatchKeys] | None = Field(None, description="Keys to ignore, a map or list of maps (OR)")
    any: list[SelectMatcher] | None = Field(None, description="Matches if any block matches")
    all: list[SelectMatcher] | None = Field(None, description="Matches if all blocks match")
    not_: SelectMatcher | None = Field(None, alias="not", description="Inverts the block")
    enforce_same_key_types: ConfigBool | None = Field(
        None, description="Inherited from the parent block, false at the root"
    )

    @model_validator(mode="after")
    def check_single_group(self) -> SelectMatcher:
        return _only_one_matcher_group(self)


class Select(FailureAwareAction, SelectMatcher):
    """Select Action.

    Select on some metadata within the data.
    """

    cpp_sources = _sources("action/select/Select.cc", "message/MetadataMatcher.cc")
    cpp_ignore = {**_COMMON_IGNORE, **_ON_ERROR_IGNORE}

    type: Literal["select"] = Field("select", init=False)


# Operations of the `statistics` action (action/statistics/Operations.h)
StatisticsOperation = Annotated[
    Literal[
        "instant",
        "average",
        "difference",
        "inverse-difference",
        "flux-average",
        "minimum",
        "maximum",
        "accumulate",
        "de-accumulate",
        "fixed-window-flux-average",
    ],
    CONFIG_STR,  # read with getStringVector
]

# Operations of the `statistics-mtg2` action (action/statistics-mtg2/Operations.h)
StatisticsMTGOperation = Annotated[
    Literal["instant", "average", "difference", "inverse-difference", "minimum", "maximum", "accumulate", "stddev"],
    CONFIG_STR,
]


class StatisticsOptions(MultioBaseModel):
    """Options of the `statistics` action.

    When `options` is absent C++ uses its constructor defaults, which differ for `restart-prefix`
    (`StatisticsRestartFile` instead of `StatisticsDump`).
    """

    cpp_sources = ("src/multio/action/statistics/cfg/StatisticsOptions.cc",)
    cpp_ignore = {
        "options": "the container key, a field of the Statistics action",
        "help": "a key of the Statistics action: its presence makes C++ throw 'Usage requested'",
        "restart": "legacy key that no C++ code reads: C++ ignores it, and the nextGEMS plans still set it",
    }

    restart: Any = Field(
        None,
        description="Legacy: C++ has no reader for this key and ignores it, whatever the value (the plans set `true` "
        "and `'true'`), use `read-restart` and `write-restart`. "
        "Accepted because the plans in tests/multio/nextGEMS and the statistics tests (`standard_144_*`) set it",
    )
    use_current_time: ConfigInt = Field(0, description="Read as an integer by C++ (0 or 1), `true` is 1")
    step_frequency: ConfigInt = Field(1)
    time_step: ConfigInt = Field(3600, description="Time step in seconds")
    initial_condition_present: ConfigBool = Field(False)
    write_restart: ParsedBool = Field(False)
    debug_restart: ParsedBool = Field(False)
    is_client_side: ParsedBool = Field(False)
    read_restart: ParsedBool = Field(False)
    restart_path: ConfigStr = Field(".", description="Must be an existing directory")
    restart_prefix: ConfigStr = Field("StatisticsDump")
    restart_lib: ConfigStr = Field("fstream_io")
    restart_time: ConfigStr = Field("latest", description="`latest` or `YYYYMMDD-HHMMSS`")
    log_prefix: ConfigStr = Field("Plan")
    window_type: Annotated[Literal["forward-offset", "backward-offset"], CONFIG_STR] = Field("forward-offset")
    solver_reset_accumulate_fields_every: Annotated[Literal["hour", "day", "month", "year", "never"], CONFIG_STR] = (
        Field("month")
    )
    value_count_threshold: ConfigInt = Field(-1, description="Either -1 (unset) or a positive value")

    @model_validator(mode="after")
    def check_value_count_threshold(self) -> StatisticsOptions:
        if self.value_count_threshold != -1 and self.value_count_threshold <= 0:
            raise ValueError("value-count-threshold must be -1 (unset) or positive")
        return self


class MappingParamWhere(MultioBaseModel):
    """Condition of a `mapping-param` entry of the `statistics` action."""

    cpp_sources = ("src/multio/action/statistics/RemapParamID.cc",)

    param_is: ConfigStr
    operation_is: ConfigStr
    output_frequency_is: ConfigStr = Field(description="Must equal the `output-frequency` of the action")


class MappingParam(MultioBaseModel):
    """Entry of `mapping-param` of the `statistics` action: remaps the output `paramId`."""

    cpp_sources = ("src/multio/action/statistics/RemapParamID.cc",)

    where: MappingParamWhere
    map_to_param: ConfigStr


class Statistics(FailureAwareAction):
    """Statistics Action.

    Calculate statistics on the data. Operations are plain names: C++ rejects anything else, for example
    `DailyHours::average`, with "Invalid opname".
    """

    cpp_sources = _sources(
        "action/statistics/Statistics.cc",
        "action/statistics/cfg/StatisticsOptions.cc",
        "action/statistics/RemapParamID.cc",
        "action/statistics/Operations.h",  # the `operations` values are dispatched in make_operation
    )
    cpp_ignore = {
        **_COMMON_IGNORE,
        **_ON_ERROR_IGNORE,
        "help": "its presence makes C++ throw 'Usage requested', deliberately not a model field",
        "restartDateTime": "message metadata key, not config",
        "flushKind": "message metadata key, not config",
        "serverRank": "message metadata key, not config",
    }

    type: Literal["statistics"] = Field("statistics", init=False)
    operations: list[StatisticsOperation]
    output_frequency: ConfigStr = Field(examples=["5h", "10d", "1w"])
    options: StatisticsOptions | None = Field(None, description="C++ constructor defaults are used when absent")
    mapping_param: list[MappingParam] | None = Field(None)


class StatisticsMTGOptions(MultioBaseModel):
    """Options of the `statistics-mtg2` action. An absent `options` is the same as an empty one."""

    cpp_sources = ("src/multio/action/statistics-mtg2/cfg/StatisticsOptions.cc",)
    cpp_ignore = {"options": "the container key, a field of the StatisticsMTG action"}

    initial_condition_present: ParsedBool = Field(False)
    read_restart: ParsedBool = Field(False)
    write_restart: ParsedBool = Field(False)
    debug_restart: ParsedBool = Field(False)
    restart_time: ConfigStr = Field("latest", description="`latest` or `YYYYMMDD-HHMMSS`")
    restart_path: ConfigStr = Field(".", description="Must be an existing directory")
    restart_prefix: ConfigStr = Field("StatisticsDump")
    restart_lib: ConfigStr = Field("fstream_io")
    log_prefix: ConfigStr = Field("Plan")
    window_type: Annotated[Literal["forward-offset", "backward-offset"], CONFIG_STR] = Field("forward-offset")
    value_count_threshold: ConfigInt = Field(-1, description="Either -1 (unset) or a positive value")
    disable_strict_mapping: ParsedBool = Field(False)
    disable_squashing: ParsedBool = Field(False)
    emit_incomplete_statistics: ParsedBool = Field(False)
    allow_non_uniform_statistics: ParsedBool = Field(False)
    debug: ParsedBool = Field(False)
    set_metadata: dict[str, ConfigStr] = Field(
        default_factory=dict, description="Metadata to set, all values are read as strings (`1` is `'1'`)"
    )
    output_time_reference: Annotated[Literal["", "start-of-window", "start-of-forecast"], CONFIG_STR] = Field(
        "", description="Empty means unset"
    )

    @model_validator(mode="after")
    def check_value_count_threshold(self) -> StatisticsMTGOptions:
        if self.value_count_threshold != -1 and self.value_count_threshold <= 0:
            raise ValueError("value-count-threshold must be -1 (unset) or positive")
        return self


class StatisticsMTG(FailureAwareAction):
    """StatisticsMTG Action.

    Calculate statistics on the data based on MARS keys. `mapping-param` is not read by C++ and is not accepted.
    """

    cpp_sources = _sources(
        "action/statistics-mtg2/Statistics.cc",
        "action/statistics-mtg2/cfg/StatisticsOptions.cc",
        "action/statistics-mtg2/Operations.h",  # the `operations` values are dispatched in make_operation
    )
    cpp_ignore = {**_COMMON_IGNORE, **_ON_ERROR_IGNORE}

    type: Literal["statistics-mtg2"] = Field("statistics-mtg2", init=False)
    operations: list[StatisticsMTGOperation]
    output_frequency: ConfigStr = Field(examples=["5h", "10d", "1w"])
    options: StatisticsMTGOptions | None = Field(None, description="Absent is the same as empty: all defaults")


class ScaleMapping(MultioBaseModel):
    """Scale Mapping"""

    cpp_sources = ("src/multio/action/scale/Scale.h",)

    param_in: StrictParserInt
    param_out: StrictParserInt
    scaling: StrictParserFloat


class Scale(Action):
    """Scale Action.

    Read with the strict parser: unknown keys, including `on-error`, are rejected by C++.
    """

    cpp_sources = _sources(
        "action/scale/Scale.h",
        "action/scale/Scale.cc",
        "util/config/detail/Parser.h",
        "util/config/detail/Parser.cc",
    )
    cpp_ignore = {**_STRICT_PARSER_IGNORE}

    type: Literal["scale"] = Field("scale", init=False)
    preset_mappings: Literal["local-to-wmo", "wmo-to-local"] | None = Field(None)
    custom_mappings: list[ScaleMapping] | None = Field(None)

    @model_validator(mode="after")
    def check_has_mapping(self) -> Scale:
        if self.preset_mappings is None and not self.custom_mappings:
            raise ValueError("preset-mappings or custom-mappings is required, C++ finds no scale mapping otherwise")
        return self


class Transport(FailureAwareAction):
    """Transport Action.

    `target` names a top-level key of the whole multio config, which must hold a `transport` block.
    """

    cpp_sources = _sources("action/transport/Transport.cc", "transport/TransportRegistry.cc")
    cpp_ignore = {
        **_COMMON_IGNORE,
        **_ON_ERROR_IGNORE,
        "transport": "key of the block named by `target` (a top-level config key), not of the action",
    }

    type: Literal["transport"] = Field("transport", init=False)
    target: ConfigStr
    hash_keys: list[ConfigStr] = Field(default_factory=lambda: ["category", "name", "level"])


class Aggregation(FailureAwareAction):
    """Aggregation Action"""

    cpp_sources = _sources("action/aggregate/Aggregate.cc")

    type: Literal["aggregate"] = Field("aggregate", init=False)


class Interpolate(FailureAwareAction):
    """Interpolate Action.

    The only model that allows extra keys: C++ forwards the top-level keys that are in the metkit `_postproc` set
    (accuracy, bitmap, format, frame, gaussian, area, grid, interpolation, packing, resol, rotation, intgrid,
    truncation, process) to MIR, and the set depends on the installed metkit. Keys of `options` are forwarded as well.

    Input grids that are lists, and `grid` lists, must hold floats (`[1.0, 1.0]`): C++ rejects integer lists.
    """

    model_config = ConfigDict(extra="allow")  # metkit `_postproc` keys are forwarded to MIR

    cpp_sources = _sources("action/interpolate/Interpolate.cc")
    cpp_ignore = {
        **_COMMON_IGNORE,
        **_ON_ERROR_IGNORE,
        "missing_value": "key inside `options` (read through getSubConfiguration), not an action key",
    }
    cpp_strict_strings = {
        "grid": 'read with getString only after `isString("grid")`: a number or a list of one string is not a '
        "string there, so it is never converted (Interpolate.cc)",
    }

    type: Literal["interpolate"] = Field("interpolate", init=False)
    input: str | Annotated[list[FloatOnly], Field(min_length=2, max_length=2)] | None = Field(
        None, description="Input grid, required unless the message carries `atlas-grid-kind`"
    )
    grid: str | Annotated[list[FloatOnly], Field(min_length=2, max_length=2)] | None = Field(
        None, description="Output grid, e.g. `0.25/0.25`, `H128` or `[0.25, 0.25]`"
    )
    area: Annotated[list[float], Field(min_length=4, max_length=4)] | None = Field(
        None, description="[north, west, south, east], used for regular lat/lon output only"
    )
    cache_path: ConfigStr = Field(
        "", description="Directory of the interpolation matrices, `matrix` interpolation only"
    )
    options: dict[str, Any] = Field(default_factory=dict, description="Forwarded verbatim to MIR")


class InterpolateMTGOptions(MultioBaseModel):
    """Options of an Interpolate-mtg2 output"""

    cpp_sources = ("src/multio/action/interpolate-mtg2/InterpolateMtg2.h",)

    interpolation: str | None = Field(None)
    interpolation_matrix: str | None = Field(None)
    intermediate_interpolation: str | None = Field(None)
    intgrid: str | None = Field(None)
    caching: StrictParserBool | None = Field(None)


class InterpolateMTGAdditionalMetadata(MultioBaseModel):
    """Additional metadata of an Interpolate-mtg2 output"""

    cpp_sources = ("src/multio/action/interpolate-mtg2/InterpolateMtg2.h",)

    resolution: Literal["low", "standard", "high"] | None = Field(None)


class InterpolateMTGOutput(MultioBaseModel):
    """Output grid of an Interpolate-mtg2 action"""

    cpp_sources = ("src/multio/action/interpolate-mtg2/InterpolateMtg2.h",)

    grid: str
    enable: StrictParserBool = Field(True)
    cache_path: str = Field(".")
    options: InterpolateMTGOptions | None = Field(None)
    additional_metadata: InterpolateMTGAdditionalMetadata | None = Field(None)


class InterpolateMTG(Action):
    """Interpolate-mtg2 Action.

    Read with the strict parser: unknown keys, including `on-error`, are rejected by C++.
    """

    cpp_sources = _sources(
        "action/interpolate-mtg2/InterpolateMtg2.h",
        "action/interpolate-mtg2/InterpolateMtg2.cc",
        "util/config/detail/Parser.h",
        "util/config/detail/Parser.cc",
    )
    cpp_ignore = {
        **_STRICT_PARSER_IGNORE,
        "missing_value": "key of the MIR input parametrisation and output metadata, not action config",
    }

    type: Literal["interpolate-mtg2"] = Field("interpolate-mtg2", init=False)
    outputs: list[InterpolateMTGOutput]


class RenumberHealpix(FailureAwareAction):
    """Renumber-healpix Action"""

    cpp_sources = _sources("action/renumber-healpix/HEALPix_ring2nest.cc")

    type: Literal["renumber-healpix"] = Field("renumber-healpix", init=False)
    cache_file_name: ConfigStr = Field(description="Path to the cache file, which must exist")


class AverageRate(FailureAwareAction):
    """Average-rate Action"""

    cpp_sources = _sources("action/average-rate/AverageRate.cc")
    cpp_ignore = {
        **_COMMON_IGNORE,
        **_ON_ERROR_IGNORE,
        "paramIn": "key of the resource file average_rate_param_mappings.yaml, not action config",
        "paramOut": "key of the resource file average_rate_param_mappings.yaml, not action config",
    }

    type: Literal["average-rate"] = Field("average-rate", init=False)


class DebugSink(FailureAwareAction):
    """Debug-sink Action.

    Terminal action which keeps the messages in a queue, intended for testing.
    """

    cpp_sources = _sources("action/debug-sink/DebugSink.cc")

    type: Literal["debug-sink"] = Field("debug-sink", init=False)


class MetadataMapping(FailureAwareAction):
    """Metadata-mapping Action"""

    cpp_sources = _sources("action/metadata-mapping/MetadataMapping.cc", "config/MetadataMappings.cc")
    cpp_ignore = {
        **_COMMON_IGNORE,
        **_ON_ERROR_IGNORE,
        **{
            key: "key of the mapping file that `mapping` points to, not action config"
            for key in ("data", "mappings", "match", "map", "optional-map", "target-path")
        },
    }

    type: Literal["metadata-mapping"] = Field("metadata-mapping", init=False)
    mapping: ConfigStr = Field(description="Path to the YAML mapping file")
    enforce_match: ConfigBool = Field(True)
    overwrite_existing: ConfigBool = Field(False)


class Print(FailureAwareAction):
    """Print Action.

    `stream` is one of `info`, `error`, `cout`, `mars` and `mars-misc`; C++ prints to the debug log for any other value.
    """

    cpp_sources = _sources("action/print/Print.cc")
    cpp_open_enums = frozenset({"stream"})

    type: Literal["print"] = Field("print", init=False)
    # Any string is valid: C++ sends an unknown stream to the debug log (Print.cc:42-64) instead of throwing
    stream: Annotated[Literal["cout", "info", "error", "mars", "mars-misc"] | str, CONFIG_STR] = Field(
        "info",
        description="Stream to print to: `info`, `error`, `cout`, `mars` or `mars-misc`. "
        "Any other string prints to the debug log",
    )
    prefix: ConfigStr = Field("", description="Prefix to print, a number is printed as C++ writes it (`1.0` is `1`)")
    only_fields: ConfigBool = Field(False)


class Mask(FailureAwareAction):
    """Mask Action"""

    cpp_sources = _sources("action/mask/Mask.cc")
    cpp_ignore = {
        **_COMMON_IGNORE,
        **_ON_ERROR_IGNORE,
        "missing-value": "the C++ default is std::numeric_limits<float>::max(), not a literal",
    }

    type: Literal["mask"] = Field("mask", init=False)
    offset_fields: list[ConfigStr] = Field(default_factory=list, description="Fields to offset by `offset-value`")
    apply_bitmap: ConfigBool = Field(True)
    missing_value: ConfigFloat = Field(3.4028234663852886e38, description="Defaults to the largest float32")
    offset_value: ConfigFloat = Field(273.15)


# Values that C++ `Encode` can set as ecCodes keys after encoding: lists and maps are not supported
OverwriteMap = dict[str, Union[bool, int, float, str]]


class EncodeKeys(MultioBaseModel):
    """Keys of the `encode` action that C++ reads from the encoding config.

    They are read from the action itself, or from its `encoding` sub-map when there is one.
    """

    cpp_sources = ("src/multio/action/encode/Encode.cc", "src/multio/action/encode/GribEncoder.cc")

    format: Annotated[Literal["grib", "raw"], CONFIG_STR] | None = Field(
        None, description="Required: either here or inside `encoding`"
    )
    template: ConfigStr | None = Field(None, description="GRIB sample path, required for the `grib` format")
    atlas_named_grid: ConfigStr | None = Field(
        None, description="Atlas grid, e.g. `O320`, `N320`, `F320` or `L360x181`"
    )
    overwrite: OverwriteMap | list[OverwriteMap] | None = Field(None, description="ecCodes keys set after encoding")
    additional_metadata: dict[str, Any] | None = Field(None, description="Wins over `run` when both are given")
    run: dict[str, Any] | None = Field(None, description="Used as additional metadata if there is none")
    unstructured_grid_type: ConfigStr | None = Field(None, description="e.g. `ORCA1`, required for ocean fields")


def _check_format_and_template(model: EncodeKeys) -> None:
    if model.format is None:
        raise ValueError("format is required")
    if model.format == "grib" and model.template is None:
        raise ValueError("template is required for grib format")


class EncodeEncoding(EncodeKeys):
    """The `encoding` sub-map of the `encode` action."""

    format: Annotated[Literal["grib", "raw"], CONFIG_STR]  # type: ignore[assignment]

    @model_validator(mode="after")
    def check_template(self) -> EncodeEncoding:
        _check_format_and_template(self)
        return self


# Keys that C++ ignores on the action when `encoding` is present
_ENCODING_KEYS = ("format", "template", "atlas_named_grid", "overwrite", "additional_metadata", "run")


class Encode(FailureAwareAction, EncodeKeys):
    """Encode Action.

    The encoding keys are either given flat, or inside `encoding`, never both: C++ ignores the flat keys when
    `encoding` is present, so they are rejected here. `grid-downloader-template` and `unstructured-grid-type` are
    read from the action itself by the grid downloader, whatever `encoding` holds.
    """

    cpp_sources = _sources("action/encode/Encode.cc", "action/encode/GribEncoder.cc", "action/encode/GridDownloader.cc")
    cpp_ignore = {
        **_COMMON_IGNORE,
        **_ON_ERROR_IGNORE,
        "uid": "key of an atlas grid spec (`gridSpec.getString`) in GridDownloader.cc, not action config",
    }

    type: Literal["encode"] = Field("encode", init=False)
    encoding: EncodeEncoding | None = Field(None, description="Nested encoding keys, instead of flat ones")
    grid_downloader_template: ConfigStr | None = Field(None, description="GRIB sample for ORCA coordinate fields")

    @model_validator(mode="after")
    def check_encoding(self) -> Encode:
        if self.encoding is None:
            _check_format_and_template(self)
            return self
        flat = [key for key in _ENCODING_KEYS if key in self.model_fields_set]
        if flat:
            raise ValueError(f"{flat} must be given inside `encoding`: C++ ignores them when `encoding` is present")
        return self


class EncodeMTG(Action):
    """Encode-mtg2 Action.

    The action is read with the strict parser, which only knows `cached` and rejects everything else, including
    `on-error`. C++ also reads `mars2grib-options`, but only after the parser has thrown on it, so it is rejected
    here as well. When C++ is fixed, add the option (a map of booleans with camel case names) to this model.
    """

    cpp_sources = _sources(
        "action/encode-mtg2/EncodeMtg2.h",
        "action/encode-mtg2/EncodeMtg2.cc",
        "util/config/detail/Parser.h",
        "util/config/detail/Parser.cc",
    )
    cpp_ignore = {
        **_STRICT_PARSER_IGNORE,
        "mars2grib-options": "C++ bug: strict parser rejects it before makeEncoder reads it",
    }

    type: Literal["encode-mtg2"] = Field("encode-mtg2", init=False)
    cached: StrictParserBool = Field(False)


class Sink(FailureAwareAction):
    """Sink Action.

    Contains a list of sinks to write to.
    """

    cpp_sources = _sources("action/sink/Sink.cc", "sink/MultIO.cc", "sink/DataSink.cc", "sink/Trigger.cc")
    cpp_ignore = {
        **_COMMON_IGNORE,
        **_ON_ERROR_IGNORE,
        "enable": "read from each entry of `sinks`, not from the action",
    }

    type: Literal["sink"] = Field("sink", init=False)
    sinks: list[SinksType] = Field(default_factory=lambda: [])
    triggers: list[TriggersType] | None = Field(None)
    fail_on_error: ConfigBool = Field(True, alias="failOnError")

    @validate_call
    def add_sink(self, sink: SinksType):
        self.sinks.append(sink)

    @validate_call
    def extend_sinks(self, sink: list[SinksType]):
        self.sinks.extend(sink)


class SingleField(FailureAwareAction):
    """Single Field Sink"""

    cpp_sources = _sources("action/single-field-sink/SingleFieldSink.cc")
    cpp_ignore = {
        **_COMMON_IGNORE,
        **_ON_ERROR_IGNORE,
        "path": "set (not read) on the file sink that the action builds internally",
        "append": "set (not read) on the file sink that the action builds internally",
    }

    type: Literal["single-field-sink"] = Field("single-field-sink", init=False)
    root_path: ConfigStr = Field(
        "", alias="root_path", description="Directory of the files, spelled with an underscore"
    )


ACTIONS = Union[
    Select,
    Scale,
    Statistics,
    StatisticsMTG,
    Transport,
    Aggregation,
    Interpolate,
    InterpolateMTG,
    Null,
    Print,
    Mask,
    Encode,
    EncodeMTG,
    Sink,
    SingleField,
    RenumberHealpix,
    AverageRate,
    DebugSink,
    MetadataMapping,
]

__all__ = [
    "ACTIONS",
    "Action",
    "FailureAwareAction",
    "OnErrorOptions",
    "Select",
    "SelectMatcher",
    "Scale",
    "ScaleMapping",
    "Statistics",
    "StatisticsOptions",
    "MappingParam",
    "MappingParamWhere",
    "StatisticsMTG",
    "StatisticsMTGOptions",
    "Transport",
    "Aggregation",
    "Interpolate",
    "InterpolateMTG",
    "InterpolateMTGOptions",
    "InterpolateMTGAdditionalMetadata",
    "InterpolateMTGOutput",
    "Null",
    "Print",
    "Mask",
    "Encode",
    "EncodeEncoding",
    "EncodeMTG",
    "Sink",
    "SingleField",
    "RenumberHealpix",
    "AverageRate",
    "DebugSink",
    "MetadataMapping",
]
