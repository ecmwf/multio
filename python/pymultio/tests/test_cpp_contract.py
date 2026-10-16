# (C) Copyright 2026 European Centre for Medium-Range Weather Forecasts.
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

import re
from typing import Annotated, Callable, Literal

import pytest
from pydantic import TypeAdapter, ValidationError

from multio.plans import actions, base, plans, sinks
from multio.plans.base import MultioBaseModel

from cpp_scan import (
    NONLITERAL,
    UNKNOWN_VALUE,
    UNSET,
    Sources,
    _members,
    accepts_any_string,
    aliases,
    check_model,
    comparison_literals,
    comparison_sites,
    converters_of,
    enclosing_function,
    enum_trait_values,
    expected_converter,
    literal_values,
    nested_models,
    owned_keys,
    parse_literal,
    scan_text,
    sharing_models,
    string_exemption,
    strip_comments,
    throws_after_comparing,
)
from multio_testing import CPP_PREFIX, models_of_module, union_models

# ----------------------------------------------------------------------------------------------------------------------
# Tests

ACTION_MODELS = union_models(actions.ACTIONS)


@pytest.fixture(scope="module")
def sources(cpp_source_dir):
    return Sources(cpp_source_dir)


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
SINK_MODELS = union_models(sinks.SINKS)
CONFIG_MODELS = [plans.Client, plans.Server]

# Python-only models: they have no C++ reader, with the reason
PYTHON_ONLY = {plans.Collection: "groups named configurations for hammer-style files, which C++ never reads whole"}


@pytest.mark.parametrize("model", [*SINK_MODELS, *CONFIG_MODELS], ids=lambda model: model.__name__)
def test_sink_and_config_match_cpp(model, sources):
    problems = check_model(model, sources)
    assert not problems, "\n" + "\n".join(problems)


def test_every_sink_and_plan_model_is_checked(sources):
    """Every model of sinks.py and plans.py is a root of the checks or nested in one; none is left out."""
    checked = {sub for root in [*SINK_MODELS, *CONFIG_MODELS, *ACTION_MODELS] for sub in nested_models(root)}
    defined = models_of_module(sinks, plans) - set(PYTHON_ONLY)
    # the bases that are never instantiated: `Client` and `Server` declare the sources of `BaseConfig`
    bases = {sinks.Sinks, sinks.Trigger, plans.PlanModel, plans.BaseConfig}
    assert defined - bases - checked == set()
    for model in defined - {plans.PlanModel, plans.BaseConfig}:
        assert model.cpp_sources, f"{model.__name__} declares no cpp_sources"
    assert {sinks.File, sinks.FDB, sinks.Maestro, sinks.MultIO, sinks.MetadataChange, plans.Plan} <= checked
    assert {plans.DisseminationPlan, plans.MpiCommunicator, plans.TcpPeer, plans.OnServerErrorOptions} <= checked


def test_cpp_ignore_entries_give_a_reason():
    for model in models_of_module(actions, sinks, plans):
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
