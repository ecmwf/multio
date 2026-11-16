# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import pytest
from pydantic import ValidationError

from multio.plans import actions, sinks
from multio.plans.actions import (
    ACTIONS,
    Aggregation,
    AverageRate,
    DebugSink,
    Encode,
    EncodeMTG,
    Interpolate,
    InterpolateMTG,
    Mask,
    MetadataMapping,
    Null,
    Print,
    RenumberHealpix,
    Scale,
    Select,
    SingleField,
    Sink,
    Statistics,
    StatisticsMTG,
    StatisticsOptions,
    Transport,
)
from multio.plans.base import MultioBaseModel


def test_allow_underscore():
    action = Encode(format="grib", template="template", atlas_named_grid="O320")
    assert action.type == "encode"
    assert action.model_dump()["atlas-named-grid"] == "O320"


def test_allow_dash():
    action = Encode(**{"format": "grib", "template": "template", "atlas-named-grid": "O320"})
    assert action.type == "encode"
    assert action.model_dump()["atlas-named-grid"] == "O320"


@pytest.mark.parametrize(
    ("action", "type", "kwargs"),
    (
        (Select, "select", {"match": [{"category": "custom"}]}),
        (Print, "print", {"stream": "cout", "prefix": " ++ MULTIO-PRINT-ALL-DEBUG :: ", "only-fields": False}),
        (Mask, "mask", {}),
        (Encode, "encode", {"format": "grib", "template": "template", "atlas-named-grid": "O320"}),
        (EncodeMTG, "encode-mtg2", {"cached": True}),
        (Transport, "transport", {"target": "target"}),
        (Aggregation, "aggregate", {}),
        (InterpolateMTG, "interpolate-mtg2", {"outputs": [{"grid": "O320", "cache-path": "/tmp"}]}),
        (RenumberHealpix, "renumber-healpix", {"cache-file-name": "cache.bin"}),
        (AverageRate, "average-rate", {}),
        (DebugSink, "debug-sink", {}),
        (MetadataMapping, "metadata-mapping", {"mapping": "mapping.yaml", "enforce-match": False}),
        (Sink, "sink", {"sinks": [{"append": True, "path": "debug.grib", "per-server": False, "type": "file"}]}),
    ),
)
def test_action_default_values(action, type, kwargs):
    action_cls = action(**kwargs)
    assert action_cls.type == type
    assert action_cls.model_dump(exclude_unset=True) == {
        **kwargs,
    }


# One valid and one rejected configuration per action. The valid ones use the C++ key spelling.
VALID = {
    "select": {"match": {"category": "ocean-2d", "level": [1, 2]}, "ignore": [{"param": "a"}, {"param": "b"}]},
    "print": {"stream": "mars-misc", "prefix": "p", "only-fields": True},
    "mask": {"offset-fields": ["t2m"], "apply-bitmap": False, "missing-value": 1e30, "offset-value": 0.0},
    "transport": {"target": "server", "hash-keys": ["param", "level"]},
    "interpolate": {
        "input": "T639",
        "grid": "0.25/0.25",
        "area": [90.0, 0.0, -90.0, 360.0],
        "cache-path": "/tmp",
        "options": {"interpolation": "linear"},
    },
    "statistics": {
        "operations": ["average", "inverse-difference", "flux-average", "de-accumulate"],
        "output-frequency": "6h",
        "options": {"restart-prefix": "dump", "window-type": "backward-offset", "value-count-threshold": 4},
        "mapping-param": [
            {"where": {"param-is": "1", "operation-is": "average", "output-frequency-is": "6h"}, "map-to-param": "2"}
        ],
    },
    "statistics-mtg2": {
        "operations": ["average", "stddev", "inverse-difference"],
        "output-frequency": "1d",
        "options": {"output-time-reference": "start-of-window", "set-metadata": {"a": "b"}, "debug": True},
    },
    "encode": {"format": "grib", "template": "t.tmpl", "overwrite": {"k": 1}, "run": {"class": "od"}},
    "encode-mtg2": {"cached": True},
    "single-field-sink": {"root_path": "/tmp/out"},
    "null": {},
    "aggregate": {},
    "scale": {"preset-mappings": "wmo-to-local"},
    "interpolate-mtg2": {"outputs": [{"grid": "O320"}]},
    "renumber-healpix": {"cache-file-name": "c.bin"},
    "average-rate": {},
    "debug-sink": {},
    "metadata-mapping": {"mapping": "m.yaml"},
    "sink": {"sinks": [{"type": "file", "path": "o", "append": False}], "failOnError": False},
}

REJECTED = {
    "select": {"match": {"a": 1}, "any": [{"match": {"a": 1}}]},
    "print": {"only-fields": "maybe"},  # getBool: eckit cannot read it as a boolean. An unknown `stream` is valid
    "mask": {"offset-fields": "t2m"},
    "transport": {"hash-keys": ["param"]},
    "interpolate": {"area": [90.0, 0.0]},
    "statistics": {"operations": ["DailyHours::average"], "output-frequency": "6h"},
    "statistics-mtg2": {"operations": ["flux-average"], "output-frequency": "6h"},
    "encode": {"format": "grib", "template": "t", "grid-type": "regular_ll"},
    "encode-mtg2": {"mars2grib-options": {"applyChecks": True}},
    "single-field-sink": {"root-path": "/tmp/out"},
    "null": {"extra": 1},
    "aggregate": {"extra": 1},
    "scale": {"on-error": "recover", "preset-mappings": "wmo-to-local"},
    "interpolate-mtg2": {"outputs": [{"grid": "O320"}], "on-error": "recover"},
    "renumber-healpix": {},
    "average-rate": {"extra": 1},
    "debug-sink": {"extra": 1},
    "metadata-mapping": {},
    "sink": {"sinks": [{"type": "socket"}]},
}

MODELS = {model.model_fields["type"].default: model for model in ACTIONS.__args__}


def test_every_action_has_a_valid_and_a_rejected_case():
    assert set(VALID) == set(MODELS)
    assert set(REJECTED) == set(MODELS)


@pytest.mark.parametrize("name", sorted(VALID))
def test_valid_action(name):
    action = MODELS[name](**VALID[name])
    assert action.type == name
    assert action.model_dump(exclude_unset=True, exclude={"type"}) == VALID[name]


@pytest.mark.parametrize("name", sorted(REJECTED))
def test_rejected_action(name):
    with pytest.raises(ValidationError):
        MODELS[name](**REJECTED[name])


@pytest.mark.parametrize("operation", ["DailyHours::average", "NoFilter::average", "DailyCustom::average"])
@pytest.mark.parametrize("model", [Statistics, StatisticsMTG])
def test_synoptic_filter_operations_are_rejected(model, operation):
    """The synoptic filter grammar is not compiled in C++, which fails with "Invalid opname"."""
    with pytest.raises(ValidationError, match="operations"):
        model(operations=[operation], **{"output-frequency": "6h"})


@pytest.mark.parametrize("name", sorted(set(MODELS) - {"scale", "encode-mtg2", "interpolate-mtg2"}))
def test_on_error_is_accepted(name):
    config = {**VALID[name], "on-error": "Recover"}
    assert MODELS[name](**config).model_dump(exclude_none=True)["on-error"] == "recover"
    config = {**VALID[name], "on-error": {"type": "propagate", "maxRetries": 3}}
    assert MODELS[name](**config).model_dump(exclude_none=True)["on-error"] == {"type": "propagate", "maxRetries": 3}
    with pytest.raises(ValidationError):
        MODELS[name](**{**VALID[name], "on-error": "abort-transport"})


@pytest.mark.parametrize("name", ["scale", "encode-mtg2", "interpolate-mtg2"])
def test_strict_parser_actions_reject_on_error(name):
    """C++ reads these with a strict parser that treats `on-error` as an unknown key."""
    with pytest.raises(ValidationError):
        MODELS[name](**{**VALID[name], "on-error": "recover"})


def test_encode_mtg2_rejects_mars2grib_options():
    """C++ bug mirrored on purpose: the strict parser (`cached` only) throws before `makeEncoder` reads the option."""
    assert EncodeMTG(cached=True).cached is True
    for options in ({"applyChecks": True}, {}):
        with pytest.raises(ValidationError, match="mars2grib-options"):
            EncodeMTG(**{"mars2grib-options": options})
    assert "mars2grib-options" in EncodeMTG.cpp_ignore


def test_only_interpolate_allows_extra_keys():
    for name, model in MODELS.items():
        extra = {**VALID[name], "unexpected-key": 1}
        if model is Interpolate:
            assert model(**extra).model_dump()["unexpected-key"] == 1
        else:
            with pytest.raises(ValidationError):
                model(**extra)


def test_every_action_declares_its_cpp_sources():
    for name, model in MODELS.items():
        assert model.cpp_sources, name
        assert all(path.startswith("src/multio/") for path in model.cpp_sources), name
        assert isinstance(model.cpp_ignore, dict), name
        assert all(isinstance(reason, str) and reason for reason in model.cpp_ignore.values()), name
    assert issubclass(Null, MultioBaseModel)


def test_select_matchers():
    nested = Select(**{"not": {"any": [{"match": {"a": 1}}, {"all": [{"ignore": {"b": ["x"]}}]}]}})
    assert nested.not_.any[1].all[0].ignore == {"b": ["x"]}
    assert nested.model_dump(exclude_none=True)["not"]["any"][0] == {"match": {"a": 1}}
    assert Select(match=[{"a": 1}, {"a": 2}], **{"enforce-same-key-types": True}).enforce_same_key_types is True
    for bad in ({}, {"match": {"a": [1, "x"]}}, {"match": {"a": {"b": 1}}}, {"any": {"match": {"a": 1}}}):
        with pytest.raises(ValidationError):
            Select(**bad)


def test_defaults_match_cpp():
    assert Mask().model_dump() == {
        "type": "mask",
        "on-error": None,
        "offset-fields": [],
        "apply-bitmap": True,
        "missing-value": 3.4028234663852886e38,
        "offset-value": 273.15,
    }
    assert Transport(target="t").hash_keys == ["category", "name", "level"]
    assert Print().stream == "info"
    assert SingleField().root_path == ""
    assert Interpolate().cache_path == ""
    assert Statistics(operations=["average"], **{"output-frequency": "1d"}).options is None


def test_statistics_options_defaults_and_enums():
    options = Statistics(operations=["average"], **{"output-frequency": "1d", "options": {"time-step": 900}}).options
    assert (options.time_step, options.step_frequency, options.restart_prefix) == (900, 1, "StatisticsDump")
    assert options.solver_reset_accumulate_fields_every == "month"
    for bad in (
        {"window-type": "centred"},
        {"solver-reset-accumulate-fields-every": "week"},
        {"value-count-threshold": 0},
    ):
        with pytest.raises(ValidationError):
            Statistics(operations=["average"], **{"output-frequency": "1d", "options": bad})
    # statistics-mtg2 has no options of the old action
    with pytest.raises(ValidationError):
        StatisticsMTG(operations=["average"], **{"output-frequency": "1d", "options": {"time-step": 900}})


def test_statistics_options_accept_the_legacy_restart_key():
    # no C++ code reads `restart`, but the nextGEMS plans set it and C++ ignores it
    # C++ never reads it, so any value is fine: the plans write `true` and, through an environment variable, `'true'`
    for value in ("true", True, 1):
        config = {"output-frequency": "1d", "options": {"restart": value, "restart-path": "."}}
        assert Statistics(operations=["average"], **config).options.restart == value
        assert Statistics.model_validate({"type": "statistics", "operations": ["average"], **config}, strict=True)
    assert Statistics(operations=["average"], **{"output-frequency": "1d", "options": {}}).options.restart is None
    dumped = Statistics(operations=["average"], **{"output-frequency": "1d", "options": {}}).model_dump(
        exclude_none=True, by_alias=True
    )
    assert "restart" not in dumped["options"]


def test_interpolate_grids_are_floats():
    assert Interpolate(grid=[0.25, 0.25], input=[1.0, 1.0]).grid == [0.25, 0.25]
    for bad in ({"grid": [1, 1]}, {"input": [1, 1]}, {"grid": [0.25]}):
        with pytest.raises(ValidationError):
            Interpolate(**bad)


def test_encode_nesting():
    action = Encode(
        encoding={"format": "raw"}, **{"grid-downloader-template": "g.tmpl", "unstructured-grid-type": "ORCA1"}
    )
    assert action.model_dump(exclude_none=True)["encoding"] == {"format": "raw"}
    assert action.unstructured_grid_type == "ORCA1"
    with pytest.raises(ValidationError):
        Encode(encoding={"format": "grib"})  # template is required for grib
    with pytest.raises(ValidationError):
        Encode(format="grib", template="t", encoding={"format": "raw"})  # flat keys are ignored by C++
    with pytest.raises(ValidationError):
        Encode(template="t")  # format is required


def test_encode_overwrite_values_are_scalars():
    assert Encode(format="raw", overwrite=[{"a": 1}, {"b": "x"}]).overwrite == [{"a": 1}, {"b": "x"}]
    with pytest.raises(ValidationError):
        Encode(format="raw", overwrite={"a": [1, 2]})


def test_scale_needs_a_mapping():
    assert Scale(**{"custom-mappings": [{"param-in": 1, "param-out": 2, "scaling": 0.5}]}).custom_mappings
    with pytest.raises(ValidationError):
        Scale()


def test_add_sinks():
    sink = Sink()
    sink.add_sink({"type": "file", "path": "output.txt", "append": False})
    assert isinstance(sink.sinks[0], sinks.File)


def test_extend_sinks():
    sink = Sink()
    sink.extend_sinks([{"type": "file", "path": "output.txt", "append": False}])
    assert isinstance(sink.sinks[0], sinks.File)


def test_sink_triggers():
    triggers = [
        {"type": "MetadataChange", "key": "step", "values": ["1", "2"], "file": "triggers.log", "failOnRetry": 1},
        {"type": "NotifyMetadata", "key": "step", "host": "localhost", "info": {"app": "multio"}},
    ]
    sink = Sink(sinks=[{"type": "file", "path": "output.txt", "append": False}], triggers=triggers)
    assert [type(trigger).__name__ for trigger in sink.triggers] == ["MetadataChange", "NotifyMetadata"]
    dumped = sink.model_dump(exclude_none=True)
    assert dumped["triggers"][0]["failOnRetry"] == 1
    assert dumped["triggers"][1]["info"] == {"app": "multio"}


def test_multio_sink_nests_sinks_and_triggers():
    sink = Sink(
        sinks=[
            {
                "type": "multio",
                "sinks": [{"type": "file", "path": "output.txt", "append": False}, {"type": "maestro"}],
                "triggers": [{"type": "NotifyMetadata", "key": "step"}],
            }
        ]
    )
    multio = sink.sinks[0]
    assert isinstance(multio, sinks.MultIO)
    assert isinstance(multio.sinks[0], sinks.File)
    assert isinstance(multio.sinks[1], sinks.Maestro)
    dumped = multio.model_dump(exclude_none=True)
    assert dumped["failOnError"] is True
    # every sink also has the `enable` and `failOnError` of C++ (T5), with their defaults
    assert dumped["sinks"][1] == {
        "type": "maestro",
        "enable": True,
        "failOnError": True,
        "ready-cdo": True,
        "schema": ".maestro.ecmwf.",
    }
    # the defaults are not marked as set, so `exclude_unset` leaves them out
    assert multio.model_dump(exclude_unset=True)["sinks"][1] == {"type": "maestro"}


@pytest.mark.parametrize("removed", ["debug-sink", "trigger", "socket"])
def test_removed_sinks_are_rejected(removed):
    with pytest.raises(ValueError):
        Sink(sinks=[{"type": removed}])


# Open and closed enums: where C++ throws for an unknown string, the model rejects it; where C++ falls through to a
# default, the model accepts it (the contract test checks which is which, see `cpp_open_enums`)


@pytest.mark.parametrize("stream", ["info", "error", "cout", "mars", "mars-misc", "debug", "stderr", ""])
def test_print_stream_accepts_any_string(stream):
    # C++ sends an unknown stream to the debug log (Print.cc:42-64) instead of throwing
    assert Print(stream=stream).stream == stream
    dumped = Print.model_validate({"type": "print", "stream": stream}, strict=True).model_dump(by_alias=True)
    assert dumped["stream"] == stream


def test_print_stream_is_read_with_get_string():
    """C++ `getString` converts a number or a list of one string, so `1` is the unknown stream `"1"`; a null is an error"""
    assert Print(stream=1).stream == "1"
    assert Print(stream=["info"]).stream == "info"
    for stream in (None, [], {"a": 1}):
        with pytest.raises(ValidationError):
            Print(stream=stream)


def test_open_enums_are_declared():
    assert Print.cpp_open_enums == {"stream"}
    # the closed enums declare nothing
    for model in (Statistics, StatisticsMTG, Encode, Scale, InterpolateMTG, StatisticsOptions):
        assert not model.cpp_open_enums, model


@pytest.mark.parametrize(
    ("model", "field"),
    [
        (Statistics, "operations"),
        (StatisticsMTG, "operations"),
        (Encode, "format"),
        (StatisticsOptions, "window_type"),
        (StatisticsOptions, "solver_reset_accumulate_fields_every"),
        (actions.StatisticsMTGOptions, "window_type"),
        (actions.StatisticsMTGOptions, "output_time_reference"),
        (Scale, "preset_mappings"),
        (actions.InterpolateMTGAdditionalMetadata, "resolution"),
    ],
)
def test_closed_enums_reject_unknown_strings(model, field):
    from pydantic import TypeAdapter

    annotation = model.model_fields[field].annotation
    with pytest.raises(ValidationError):
        TypeAdapter(annotation).validate_python(["unknown"] if field == "operations" else "unknown")


# Conversions of the readers: a value that C++ converts is accepted, a value that it rejects is not


@pytest.mark.parametrize(
    ("model", "options", "expected"),
    [
        # `use-current-time` is read with `getLong`: a boolean is 0 or 1, and a string is converted
        (StatisticsOptions, {"use-current-time": True}, {"use_current_time": 1}),
        (StatisticsOptions, {"use-current-time": "1"}, {"use_current_time": 1}),
        (StatisticsOptions, {"value-count-threshold": "10"}, {"value_count_threshold": 10}),
        (StatisticsOptions, {"time-step": 900.0}, {"time_step": 900}),
        # `write-restart` is read with `parseBool`: a string goes through eckit `Translator<string, bool>`
        (StatisticsOptions, {"write-restart": "true", "read-restart": "false"}, {"write_restart": True}),
        (StatisticsOptions, {"write-restart": "Yes", "debug-restart": "off"}, {"write_restart": True}),
        (StatisticsOptions, {"read-restart": "2"}, {"read_restart": True}),  # atoi
        (StatisticsOptions, {"read-restart": "text"}, {"read_restart": False}),  # atoi gives 0
        # `initial-condition-present` is read with `getBool`: a number is true unless it is 0
        (StatisticsOptions, {"initial-condition-present": 2}, {"initial_condition_present": True}),
        (StatisticsOptions, {"initial-condition-present": "on"}, {"initial_condition_present": True}),
        (StatisticsOptions, {"initial-condition-present": "0"}, {"initial_condition_present": False}),
        (Mask, {"missing-value": "9999", "apply-bitmap": "no"}, {"missing_value": 9999.0, "apply_bitmap": False}),
    ],
)
def test_values_are_converted_like_cpp(model, options, expected):
    if model is Mask:
        built = model.model_validate({"type": "mask", **options}, strict=True)
    else:
        built = model.model_validate(options, strict=True)
    for name, value in expected.items():
        assert getattr(built, name) == value
        assert type(getattr(built, name)) is type(value)


@pytest.mark.parametrize(
    ("model", "options"),
    [
        (StatisticsOptions, {"use-current-time": 1.5}),  # eckit throws for a float that is not whole
        (StatisticsOptions, {"write-restart": 1}),  # `parseBool` only looks at strings and booleans
        (StatisticsOptions, {"write-restart": None}),
        (StatisticsOptions, {"initial-condition-present": "maybe"}),  # `getBool` throws for it
        (StatisticsOptions, {"initial-condition-present": 1.0}),  # a float is not convertible to bool
        (StatisticsOptions, {"time-step": [900]}),
    ],
)
def test_values_that_cpp_rejects_are_rejected(model, options):
    with pytest.raises(ValidationError):
        model.model_validate(options, strict=True)


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        ("900", 900),
        (" -3", -3),
        ("+7", 7),
        ("10KB", 10240),  # `Translator<std::string, long long>`: a size suffix multiplies
        ("2 MiB", 2 * 2**20),
        ("3gb", 3 * 2**30),
        ("5b", 5),  # `b` alone is not a suffix
        ("5xb", 5),  # nor is an unknown unit
        ("12abc", 12),  # trailing text is ignored
        ("fast", 0),  # no digits: 0
        ("yes", 0),  # the long translator knows no booleans
        ("", 0),
        ("99999999999999999999999", 2**63 - 1),  # strtoll clamps
    ],
)
def test_integer_strings_convert_like_cpp(text, expected):
    """`getLong` on a string runs eckit `Translator<std::string, long long>` (strtoll and a size suffix), never fails"""
    assert StatisticsOptions.model_validate({"time-step": text}, strict=True).time_step == expected
    assert StatisticsOptions.model_validate({"use-current-time": text}, strict=True).use_current_time == expected


def test_strict_parser_conversions():
    # scale, encode-mtg2 and interpolate-mtg2 use the strict parser: a boolean is a boolean, 0, 1, "0" or "1"
    for value, expected in ((True, True), (0, False), ("1", True), ("0", False)):
        assert EncodeMTG.model_validate({"type": "encode-mtg2", "cached": value}, strict=True).cached is expected
    for value in ("true", 2, 1.0, "yes"):
        with pytest.raises(ValidationError):
            EncodeMTG.model_validate({"type": "encode-mtg2", "cached": value}, strict=True)
    mapping = actions.ScaleMapping.model_validate({"param-in": "1", "param-out": 2, "scaling": "0.5"}, strict=True)
    assert (mapping.param_in, mapping.param_out, mapping.scaling) == (1, 2, 0.5)
    # `std::stod` skips leading white space, `pos != size` rejects trailing white space
    spaced = actions.ScaleMapping.model_validate({"param-in": 1, "param-out": 2, "scaling": " 0.5"}, strict=True)
    assert spaced.scaling == 0.5
    for bad in (
        {"param-in": 1.0},
        {"param-in": True},
        {"scaling": True},
        {"scaling": "half"},
        {"scaling": "0.5 "},
    ):
        with pytest.raises(ValidationError):
            actions.ScaleMapping.model_validate({"param-in": 1, "param-out": 2, "scaling": 1.0, **bad}, strict=True)


@pytest.mark.parametrize(
    ("value", "expected"),
    [
        ("text", "text"),
        ("", ""),
        (130, "130"),
        (-7, "-7"),
        (0, "0"),
        (2**63, "9223372036854775807"),  # read as a 64 bit integer: clamped
        (-(2**70), "-9223372036854775808"),
        (130.0, "130"),  # `std::ostream << double`: 6 significant digits, no trailing zero
        (1.5, "1.5"),
        (0.1, "0.1"),
        (100000.0, "100000"),
        (1234567.0, "1.23457e+06"),
        (1e-5, "1e-05"),
        (1e16, "1e+16"),
        (0.30000000000000004, "0.3"),
        (-0.0, "-0"),
        (float("inf"), "inf"),
        (float("-inf"), "-inf"),
        (float("nan"), "nan"),
        (True, "true"),
        (False, "false"),
        ([130], "130"),  # `ListContent::value(std::string&)`: a list of one element is that element
        ([[7]], "7"),
        ([True], "true"),
        (["a"], "a"),
    ],
)
def test_config_str_converts_like_cpp(value, expected):
    """`getString` of eckit: the text that C++ makes of a value (checked against the library in test_cpp_roundtrip)"""
    from multio.plans.base import config_str

    assert config_str(value) == expected


@pytest.mark.parametrize("value", [None, [], [1, 2], {}, {"a": 1}, [None], [{}], [[1, 2]], (1,), b"a"])
def test_config_str_rejects_what_cpp_cannot_convert(value):
    from multio.plans.base import config_str

    with pytest.raises(ValueError):
        config_str(value)


def test_string_fields_convert_numbers_in_strict_parsing():
    action = Print.model_validate({"type": "print", "prefix": 130, "stream": 5}, strict=True)
    assert (action.prefix, action.stream) == ("130", "5")
    assert Print.model_validate({"type": "print", "prefix": [1.0]}, strict=True).prefix == "1"
    for bad in ({"a": 1}, None, [], [1, 2]):
        with pytest.raises(ValidationError):
            Print.model_validate({"type": "print", "prefix": bad}, strict=True)


def test_statistics_mapping_params_may_be_numbers():
    """C++ `getString` reads `param-is: 130` and `map-to-param: 131`"""
    action = Statistics.model_validate(
        {
            "type": "statistics",
            "operations": ["average"],
            "output-frequency": "1d",
            "mapping-param": [
                {
                    "where": {"param-is": 130, "operation-is": "average", "output-frequency-is": "1d"},
                    "map-to-param": 131,
                }
            ],
        },
        strict=True,
    )
    mapping = action.mapping_param[0]
    assert (mapping.where.param_is, mapping.map_to_param) == ("130", "131")
    dumped = action.model_dump()["mapping-param"][0]
    assert (dumped["where"]["param-is"], dumped["map-to-param"]) == ("130", "131")


def test_string_lists_convert_each_element():
    assert Mask.model_validate({"type": "mask", "offset-fields": [1, True, 1.5, [2]]}, strict=True).offset_fields == [
        "1",
        "true",
        "1.5",
        "2",
    ]
    assert Transport.model_validate({"type": "transport", "target": 1, "hash-keys": [2]}, strict=True).hash_keys == [
        "2"
    ]
    for bad in (1, "a", None, [None], [{}]):  # `getStringVector` asserts that the value is a list
        with pytest.raises(ValidationError):
            Mask.model_validate({"type": "mask", "offset-fields": bad}, strict=True)


def test_closed_string_enums_still_reject_numbers_but_convert_lists_of_one():
    """A number is text that matches no literal, which C++ rejects; a list of one string is that string"""
    base_options = {"type": "statistics", "operations": ["average"], "output-frequency": "1d"}
    for options in ({"window-type": 1}, {"window-type": True}, {"window-type": ["x"]}):
        with pytest.raises(ValidationError):
            Statistics.model_validate({**base_options, "options": options}, strict=True)
    built = Statistics.model_validate({**base_options, "options": {"window-type": ["backward-offset"]}}, strict=True)
    assert built.options.window_type == "backward-offset"
    with pytest.raises(ValidationError):
        Statistics.model_validate({**base_options, "operations": [1]}, strict=True)
    with pytest.raises(ValidationError):
        Encode.model_validate({"type": "encode", "format": 1, "template": "t"}, strict=True)


def test_on_error_tag_is_read_with_get_string():
    assert Print.model_validate({"type": "print", "on-error": ["Recover"]}, strict=True).on_error == "recover"
    assert (
        Print.model_validate({"type": "print", "on-error": {"type": 5}}, strict=True).on_error.type == "5"
    )  # open enum
    for bad in (5, 1.0, True, {"type": [1, 2]}):
        with pytest.raises(ValidationError):
            Print.model_validate({"type": "print", "on-error": bad}, strict=True)


def test_strict_parser_strings_reject_numbers():
    with pytest.raises(ValidationError):
        InterpolateMTG.model_validate({"type": "interpolate-mtg2", "outputs": [{"grid": 32}]}, strict=True)
    with pytest.raises(ValidationError):
        InterpolateMTG.model_validate(
            {"type": "interpolate-mtg2", "outputs": [{"grid": "H32", "options": {"interpolation": 1}}]}, strict=True
        )
    with pytest.raises(ValidationError):  # `isString("grid")` guards the read of the grid of `interpolate`
        Interpolate.model_validate({"type": "interpolate", "input": "T1279", "grid": 1}, strict=True)


def test_set_metadata_and_trigger_info_values_are_read_as_strings():
    options = actions.StatisticsMTGOptions.model_validate({"set-metadata": {"a": 1, "b": False}}, strict=True)
    assert options.set_metadata == {"a": "1", "b": "false"}
    trigger = sinks.NotifyMetadata.model_validate({"type": "NotifyMetadata", "key": 3, "info": {"n": 2.5}}, strict=True)
    assert (trigger.key, trigger.info) == ("3", {"n": "2.5"})
    with pytest.raises(ValidationError):
        sinks.NotifyMetadata.model_validate({"type": "NotifyMetadata", "key": "k", "info": {"n": {}}}, strict=True)
