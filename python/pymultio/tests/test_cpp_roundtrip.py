# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""Pass configs made by the Python plan models through the real C++ library.

The other alignment tests compare the models with the C++ sources. This one asks C++ itself: a config dumped by a
model is given to `multio.Multio`, which builds the C++ plans, actions and sinks, and so throws on an unknown type, a
key the strict parser rejects, a plan without actions and so on. Nothing is written to the repository: every file
that a config names is made under the pytest `tmp_path`, which is also the working directory.

- Every action model of `ACTIONS` and every sink model of `SINKS` has a minimal example (a test fails when a model has
  none), which is dumped and built by C++.
- Negative cases show that the harness sees a failure: configs that Python rejects must be rejected by C++ too.
- Every plan file of the corpus (see `test_cpp_plan_files`) is read as C++ reads it, validated by the models, dumped
  again and built by C++.

A case skips only when C++ says that a builder is not in this build (`No ActionFactory called maestro`) or that it
needs MPI. Any other C++ error fails the case.
"""

import re
import os
import shutil
import subprocess
import sys
import typing
from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from multio.plans import (
    Aggregation,
    AverageRate,
    Client,
    Collection,
    DebugSink,
    Encode,
    EncodeMTG,
    Interpolate,
    InterpolateMTG,
    Mask,
    MetadataMapping,
    Null,
    Plan,
    Print,
    RenumberHealpix,
    Scale,
    Select,
    Server,
    SingleField,
    Sink,
    Statistics,
    StatisticsMTG,
    Transport,
)
from multio.plans.actions import ACTIONS
from multio.plans.sinks import FDB, SINKS, File, Maestro, MetadataChange, MultIO, NotifyMetadata
from multio.plans.plans import TERMINAL_ACTIONS

try:
    import multio

    multio.Multio  # the library is loaded on first access
except ImportError as error:
    pytest.skip(f"the multio C library cannot be loaded: {error}", allow_module_level=True)

from test_cpp_plan_files import (  # noqa: E402  the corpus loader, shared with the corpus test
    REPO_ROOT,
    TOP_LEVEL_SHARED,
    VALIDATED_FILES,
    NoRecognisedShape,
    check_file,
)

# C++ reports a missing builder as `No ActionFactory called <name>` or `No DataSinkFactory called <name>`
MISSING_BUILDER = re.compile(r"No (?:ActionFactory|DataSinkFactory) called (?P<name>[\w-]+)")
# Builders that a libmultio build may not have, with the reason. A builder registers itself when its library is loaded.
OPTIONAL_BUILDERS = {
    "maestro": "the sink needs Maestro, an optional dependency",
    "debug-sink": "libmultio-api does not link multio-action-debug-sink (src/multio/api/c/CMakeLists.txt), so the "
    "action is never registered in the C API",
}
# eckit without MPI cannot split communicators or create the MPI transport
NEEDS_MPI = re.compile(r"eckit/mpi/Serial\.cc:\d+ \w+")
# The ORCA grids are made by an atlas plugin (and downloaded by the `encode` action): without it atlas has no such grid
NEEDS_ATLAS_ORCA = re.compile(r"Registry: unknown 'e?ORCA\w*'")

MULTIO_ENVIRONMENT_PREFIX = "MULTIO"


# --- running C++ ------------------------------------------------------------------------------------------------


@pytest.fixture(autouse=True)
def isolated(tmp_path, monkeypatch):
    """Run in `tmp_path` and without the environment variables that make multio pick another configuration"""
    for name in list(os.environ):
        if name.startswith(MULTIO_ENVIRONMENT_PREFIX) or name == "ECKIT_MPI_FORCE":
            monkeypatch.delenv(name)
    monkeypatch.chdir(tmp_path)


def uses_transport(node) -> bool:
    """Whether a config has a `transport` action"""
    if isinstance(node, list):
        return any(uses_transport(item) for item in node)
    if isinstance(node, dict):
        return node.get("type") == "transport" or any(uses_transport(value) for value in node.values())
    return False


# Builds the client of a config file in a fresh process and reports the exception of C++ on the error stream
CHILD = """
import sys
import multio
try:
    multio.Multio(config_path=sys.argv[1])
except multio.MultioException as error:
    print("MULTIO_EXCEPTION", error, file=sys.stderr)
    sys.exit(3)
"""


def build_in_new_process(path: Path) -> None:
    """Build the C++ client in a new process, raising the `MultioException` of C++ as the library does

    C++ keeps the transports in a singleton, by the name of the server: once one is made in a process, a later
    `transport` action finds it and no longer builds its own, so a transport must be tested in a process of its own.
    """
    run = subprocess.run([sys.executable, "-c", CHILD, str(path)], capture_output=True, text=True, timeout=120)
    if run.returncode == 3:
        message = run.stderr.split("MULTIO_EXCEPTION", 1)[1].strip().splitlines()[0]
        raise multio.MultioException(message)
    assert run.returncode == 0, f"C++ failed with exit code {run.returncode}:\n{run.stderr[-2000:]}"


def build_from_file(config: dict, tmp_path: Path, name: str = "config.yaml") -> None:
    """Build the C++ client of a config that is read from a file, as `multio-feed --plans` does"""
    path = tmp_path / name
    path.write_text(yaml.safe_dump(config, sort_keys=False))
    if uses_transport(config):
        return build_in_new_process(path)
    handle = multio.Multio(config_path=str(path))
    del handle


def build_from_model(model) -> None:
    """Build the C++ client of a model, through `multio.MultioPlan` and the environment, as a user does"""
    with multio.MultioPlan(model):
        handle = multio.Multio()
    del handle


def skip_if_unavailable(error: multio.MultioException) -> None:
    """Skip when C++ says that a builder or a feature is not in this build, re-raise anything else"""
    message = str(error)
    missing = MISSING_BUILDER.search(message)
    if missing and missing["name"] in OPTIONAL_BUILDERS:
        pytest.skip(f"{missing[0]}: {OPTIONAL_BUILDERS[missing['name']]}")
    if NEEDS_MPI.search(message):
        pytest.skip(f"needs MPI, which this build of eckit does not have: {NEEDS_MPI.search(message)[0]}")
    if NEEDS_ATLAS_ORCA.search(message):
        pytest.skip(f"needs the atlas ORCA plugin, which is not installed: {NEEDS_ATLAS_ORCA.search(message)[0]}")
    raise error


def rejected_by_cpp(config: dict, tmp_path: Path) -> str:
    """The message of the exception that C++ throws for a config, fails the test when C++ accepts it"""
    with pytest.raises(multio.MultioException) as info:
        build_from_file(config, tmp_path)
    return str(info.value)


# --- files that the configs name ----------------------------------------------------------------------------------

GRIB_SAMPLES = REPO_ROOT / "tests/multio/config/grib-samples/samples.ecom"


def _repo_files() -> dict[str, Path]:
    """The existing files of the C++ tests and share by name, to copy the ones that a config names"""
    index: dict[str, Path] = {}
    for root in ("tests", "share"):
        for path in sorted((REPO_ROOT / root).rglob("*")):
            if path.is_file() and path.suffix not in (".so", ".o"):  # is_file follows symlinks, dangling ones are not
                index.setdefault(path.name, path)
    return index


REPO_FILES = _repo_files()

# Keys of configs that name a file which must exist when C++ builds the plan, and the keys that name a directory
INPUT_FILE_KEYS = {"template", "grid-downloader-template", "cache-file-name", "mapping", "dissemination"}
DIRECTORY_KEYS = {"templates-path", "output-path", "restart-path", "cache-path", "root_path"}


class MissingFixture(Exception):
    """A file that a config names does not exist in the repository and cannot be made here"""


class Fixtures:
    """Makes the files and directories that a config names under `root`, and rewrites the config to use them.

    Paths that are absolute (`/dummy/...`, a build directory) point out of the sandbox, so they are moved under
    `root`. Relative paths are kept, as the working directory is `root`. Files are copied from the repository when it
    has one of that name, GRIB templates are made from a sample, anything else is a `MissingFixture`.
    """

    def __init__(self, root: Path):
        self.root = root

    def _inside(self, value: str) -> Path:
        path = Path(value)
        if not path.is_absolute():
            return self.root / path
        if self.root in path.parents or path == self.root:
            return path
        return self.root / "moved" / str(path).lstrip("/").replace("/", "_")

    def directory(self, value: str) -> str:
        path = self._inside(value)
        path.mkdir(parents=True, exist_ok=True)
        return value if not Path(value).is_absolute() else str(path)

    def input_file(self, value: str, directory: Path | None = None, empty: bool = False) -> str:
        """An input file: the path of the repository file when it exists, else a copy or a sample under `root`

        With `empty`, a file that is not in the repository is made empty: C++ only checks that it exists when it builds
        the plan (the cache of `renumber-healpix`, an atlas file that a test of the build writes, is read later).
        """
        given = Path(value)
        if directory is None and given.is_absolute() and given.is_file():
            return value  # an existing file of the repository, read only
        target = (directory / given.name) if directory else self._inside(value)
        if not target.exists():
            target.parent.mkdir(parents=True, exist_ok=True)
            source = REPO_FILES.get(given.name)
            if source is None and (".grib" in given.name or given.suffix == ".tmpl"):
                edition = "grib1" if "grib1" in given.name else "grib2"
                source = GRIB_SAMPLES / f"multio-gg-sfc-{edition}.tmpl"
            if source is None and empty:
                target.touch()
                return value if not given.is_absolute() and directory is None else str(target)
            if source is None:
                raise MissingFixture(f"{value} is read by C++ when it builds the plan and is not in the repository")
            shutil.copy(source, target)
        return value if not given.is_absolute() and directory is None else str(target)

    def output_file(self, value: str) -> str:
        """An output file: C++ opens file sinks when it builds them, so they are moved under `root`"""
        target = self.root / "out" / str(value).lstrip("/").replace("/", "_")
        target.parent.mkdir(parents=True, exist_ok=True)
        return str(target)

    def prepare(self, node):
        """A copy of the config that names the files made here"""
        if isinstance(node, list):
            return [self.prepare(item) for item in node]
        if not isinstance(node, dict):
            return node
        node = {key: self.prepare(value) for key, value in node.items()}
        if "dissemination" in node and "actions" not in node:
            # a dissemination plan: the templates are files of the templates path, the requests a file of the cwd
            directory = Path(self.directory(node.get("templates-path", ".")))
            directory = directory if directory.is_absolute() else self.root / directory
            node["templates-path"] = str(directory)
            for entry in node.get("templates", []):
                for name in entry.values():
                    self.input_file(name, directory)
        for key, value in node.items():
            if isinstance(value, str) and key in INPUT_FILE_KEYS:
                node[key] = self.input_file(value, empty=key == "cache-file-name")
            elif isinstance(value, str) and key in DIRECTORY_KEYS:
                node[key] = self.directory(value)
        if node.get("type") == "file" and isinstance(node.get("path"), str):
            node["path"] = self.output_file(node["path"])
        return node


# --- minimal examples of every model -------------------------------------------------------------------------------


def _fdb_config(root: Path) -> dict:
    """A local FDB in `root`, with the schema of the C++ tests"""
    (root / "fdb").mkdir()
    shutil.copy(REPO_ROOT / "tests/multio/etc/fdb/schema", root / "fdb/schema")
    (root / "fdb/root").mkdir()
    return {
        "type": "local",
        "engine": "toc",
        "schema": str(root / "fdb/schema"),
        "spaces": [{"handler": "Default", "roots": [{"path": str(root / "fdb/root")}]}],
    }


def action_examples(root: Path) -> dict[type, typing.Callable[[], object]]:
    """A minimal valid example of each action model, as a function that makes it"""
    files = Fixtures(root)
    return {
        Null: lambda: Null(),
        Select: lambda: Select(match={"category": ["custom"]}),
        Scale: lambda: Scale(preset_mappings="local-to-wmo"),
        Statistics: lambda: Statistics(operations=["average"], output_frequency="1d"),
        StatisticsMTG: lambda: StatisticsMTG(operations=["average"], output_frequency="1d"),
        Transport: None,  # needs a server block, see `test_transport_action`
        Aggregation: lambda: Aggregation(),
        Interpolate: lambda: Interpolate(input="T1279", grid="1.0/1.0"),
        InterpolateMTG: lambda: InterpolateMTG(outputs=[{"grid": "H32"}]),
        Print: lambda: Print(),
        Mask: lambda: Mask(),
        Encode: lambda: Encode(format="grib", template=files.input_file("sample.grib")),
        EncodeMTG: lambda: EncodeMTG(),
        Sink: lambda: Sink(),
        SingleField: lambda: SingleField(root_path=files.directory("single-field")),
        RenumberHealpix: lambda: RenumberHealpix(cache_file_name=files.input_file("HEALPix.atlas", empty=True)),
        AverageRate: lambda: AverageRate(),
        DebugSink: lambda: DebugSink(),
        MetadataMapping: lambda: MetadataMapping(mapping=files.input_file("nemo-to-grib.yaml")),
    }


def sink_examples(root: Path) -> dict[type, typing.Callable[[], object]]:
    """A minimal valid example of each sink model, as a function that makes it"""
    return {
        FDB: lambda: FDB(config=_fdb_config(root)),
        File: lambda: File(path=str(root / "out.grib")),
        Maestro: lambda: Maestro(),
        MultIO: lambda: MultIO(sinks=[File(path=str(root / "nested.grib"))]),
    }


def models_of(union) -> list[type]:
    return list(typing.get_args(union))


def plan_with(*actions) -> Plan:
    """A plan of the actions, ended with a sink when the last one is not terminal (C++ needs a terminal action)"""
    actions = list(actions)
    if not isinstance(actions[-1], TERMINAL_ACTIONS):
        actions.append(Sink())
    return Plan(name="roundtrip", actions=actions)


def test_every_action_model_has_an_example(tmp_path):
    missing = [model.__name__ for model in models_of(ACTIONS) if model not in action_examples(tmp_path)]
    assert not missing, f"add a minimal example to action_examples for {missing}"


def test_every_sink_model_has_an_example(tmp_path):
    missing = [model.__name__ for model in models_of(SINKS) if model not in sink_examples(tmp_path)]
    assert not missing, f"add a minimal example to sink_examples for {missing}"


@pytest.mark.parametrize("model", [m for m in models_of(ACTIONS) if m is not Transport], ids=lambda m: m.__name__)
def test_action_is_built_by_cpp(model, tmp_path):
    action = action_examples(tmp_path)[model]()
    assert isinstance(action, model)
    plan = plan_with(action)
    assert plan.check_validity()
    try:
        build_from_model(Client(plans=[plan]))
    except multio.MultioException as error:
        skip_if_unavailable(error)


@pytest.mark.parametrize("model", models_of(SINKS), ids=lambda m: m.__name__)
def test_sink_is_built_by_cpp(model, tmp_path):
    sink = sink_examples(tmp_path)[model]()
    assert isinstance(sink, model)
    try:
        build_from_model(Client(plans=[plan_with(Sink(sinks=[sink]))]))
    except multio.MultioException as error:
        skip_if_unavailable(error)
    if model is File:
        assert Path(sink.path).exists()  # C++ opened the sink, and only in `tmp_path`


def test_chain_is_built_by_cpp(tmp_path):
    """A select, then a sink with a file sink and a trigger, as the plan of a user"""
    plan = Plan(
        name="chain",
        actions=[
            Select(match={"category": ["custom"]}),
            Print(stream="cout", prefix="roundtrip"),
            Sink(
                sinks=[File(path=str(tmp_path / "chain.grib"), append=True, per_server=False)],
                triggers=[
                    MetadataChange(key="step", values=["0", "1"], file=str(tmp_path / "trigger.log")),
                    NotifyMetadata(key="notify", file=str(tmp_path / "notify.log")),
                ],
            ),
        ],
    )
    build_from_model(Client(plans=[plan]))
    assert (tmp_path / "chain.grib").exists()


@pytest.mark.parametrize("stream", ["debug", "info", "mars-misc", "not-a-stream", ""])
def test_print_with_any_stream_is_built_by_cpp(stream, tmp_path):
    """`Print.stream` is an open enum: C++ prints an unknown stream to the debug log instead of throwing"""
    action = Print(stream=stream)
    config = {"plans": [plan_with(action).dump()]}
    assert config["plans"][0]["actions"][0]["stream"] == stream
    assert not python_rejects(config)
    try:
        build_from_file(config, tmp_path)
    except multio.MultioException as error:
        skip_if_unavailable(error)
    build_from_model(Client(plans=[plan_with(action)]))


# Values that C++ converts and that the strict `from_yaml` therefore accepts, as written in the plans of the repository
CONVERTED_VALUES = {
    "getBool from a string": {"type": "mask", "apply-bitmap": "no", "missing-value": "9999", "offset-value": 1},
    "getBool from a number": {"type": "mask", "apply-bitmap": 2},
    "statistics options": {
        "type": "statistics",
        "operations": ["average"],
        "output-frequency": "1d",
        "options": {
            "use-current-time": True,  # getLong of a boolean
            "write-restart": "false",  # parseBool of a string, as `{ENV}` leaves it
            "read-restart": "false",
            "value-count-threshold": "10",  # getLong of a string
            "restart": "true",  # never read
        },
    },
    "strict parser": {"type": "encode-mtg2", "cached": "1"},
}


@pytest.mark.parametrize("name", CONVERTED_VALUES)
def test_converted_values_are_accepted_by_models_and_cpp(name, tmp_path):
    config = plan_config(CONVERTED_VALUES[name], SINK_ACTION)
    assert not python_rejects(config)
    try:
        build_from_file(config, tmp_path)
    except multio.MultioException as error:
        skip_if_unavailable(error)


# --- strings that C++ converts: getString and getStringVector ------------------------------------------------------
#
# eckit converts a number, a boolean and a list of one element to text for `getString` (`Value` to `std::string`), and
# the elements of a list for `getStringVector`; a map, a null and a list of any other size are errors. The models do
# the same (`ConfigStr`), and the text that they keep is the text that C++ makes of the raw value.

# How C++ prints a message, by version of the library: `<prefix>: Message(...)` or `[MultIO::print][...][<plan>/<prefix>]`
PRINTED_PREFIX = (
    re.compile(r"^(?P<prefix>.*?)(?:: )?Message\(version", re.MULTILINE),
    re.compile(r"\[MultIO::print\]\[[^\]]*\]\[[^\]]*\]\[p/?(?P<prefix>[^\]]*)\] "),
)


def printed_prefix(config: dict, tmp_path: Path, capfd) -> str:
    """The prefix that a `print` action of C++ writes for a flush message, with the config read from a file"""
    path = tmp_path / "prefix.yaml"
    path.write_text(yaml.safe_dump(config, sort_keys=False))
    capfd.readouterr()
    handle = multio.Multio(config_path=str(path))
    handle.flush({"step": 1, "flushKind": 1})
    del handle
    printed = capfd.readouterr().out
    for pattern in PRINTED_PREFIX:
        found = pattern.search(printed)
        if found:
            return found["prefix"]
    raise AssertionError(f"C++ printed no message:\n{printed!r}")


def print_config(prefix) -> dict:
    """A plan named `p`, which is the name that the prefix of the print line follows in recent versions of C++"""
    return plan_config({"type": "print", "stream": "cout", "prefix": prefix}, SINK_ACTION, name="p")


# raw value of `prefix` (as a user writes it in YAML) and the text that C++ makes of it, observed on the real library
PREFIX_CONVERSIONS = {
    "integer": (130, "130"),
    "negative integer": (-7, "-7"),
    "integer above int64": (2**63, "9223372036854775807"),  # eckit clamps what it reads
    "whole float": (131.0, "131"),
    "float": (1.5, "1.5"),
    "float with 6 digits": (0.30000000000000004, "0.3"),
    "large float": (1234567.0, "1.23457e+06"),
    "small float": (1e-5, "1e-05"),
    "negative zero": (-0.0, "-0"),
    "infinity": (float("inf"), "inf"),
    "true": (True, "true"),
    "false": (False, "false"),
    "list of one integer": ([130], "130"),
    "list of one list": ([[7]], "7"),
    "list of one float": ([1.5], "1.5"),
    "list of one boolean": ([True], "true"),
    "string": ("text", "text"),
    "empty string": ("", ""),
    "string of digits": ("130", "130"),
}


@pytest.mark.parametrize("name", PREFIX_CONVERSIONS)
def test_numeric_print_prefix_is_converted_like_cpp(name, tmp_path, capfd):
    """C++ prints the text that it converts the prefix to, and the model keeps exactly that text"""
    raw, text = PREFIX_CONVERSIONS[name]
    config = print_config(raw)
    assert printed_prefix(config, tmp_path, capfd) == text  # what C++ sees of the raw value

    assert not python_rejects(config)
    client = Client.from_dict(config)
    assert client.plans[0].actions[0].prefix == text
    dumped = client.dump()
    assert dumped["plans"][0]["actions"][0]["prefix"] == text  # what Python sends
    assert printed_prefix(dumped, tmp_path, capfd) == text  # and C++ prints the same for it


@pytest.mark.parametrize("name", PREFIX_CONVERSIONS)
def test_print_prefix_made_by_the_model_is_converted_like_cpp(name, tmp_path, capfd):
    """The same through the constructor of the model and `MultioPlan`, the way that a user builds plans"""
    raw, text = PREFIX_CONVERSIONS[name]
    action = Print(stream="cout", prefix=raw)
    assert action.prefix == text
    plan = plan_with(action)
    plan.name = "p"
    capfd.readouterr()
    with multio.MultioPlan(Client(plans=[plan])):
        handle = multio.Multio()
        handle.flush({"step": 1, "flushKind": 1})
        del handle
    printed = capfd.readouterr().out
    assert any(
        pattern.search(printed) and pattern.search(printed)["prefix"] == text for pattern in PRINTED_PREFIX
    ), printed


# values that C++ and the models both reject for a string, with the message of C++
STRING_REJECTIONS = {
    "map": ({"a": 1}, "Cannot convert"),
    "null": (None, "Cannot convert"),
    "empty list": ([], "Cannot convert"),
    "list of two": ([1, 2], "Cannot convert"),
    "list of one map": ([{"a": 1}], "Cannot convert"),
    "list of one null": ([None], "Cannot convert"),
}


@pytest.mark.parametrize("name", STRING_REJECTIONS)
def test_print_prefix_that_cpp_cannot_convert_is_rejected_by_both(name, tmp_path):
    raw, message = STRING_REJECTIONS[name]
    config = print_config(raw)
    assert message in rejected_by_cpp(config, tmp_path)
    assert python_rejects(config)
    with pytest.raises(ValidationError):
        Print(prefix=raw)


def stats_mapping(param_is="130", operation_is="average", output_frequency_is="1d", map_to_param="131") -> dict:
    """A `statistics` action that remaps a parameter"""
    return {
        "type": "statistics",
        "operations": ["average"],
        "output-frequency": "1d",
        "options": {"write-restart": False, "read-restart": False},
        "mapping-param": [
            {
                "where": {
                    "param-is": param_is,
                    "operation-is": operation_is,
                    "output-frequency-is": output_frequency_is,
                },
                "map-to-param": map_to_param,
            }
        ],
    }


def test_statistics_mapping_with_numeric_params_is_accepted_by_both(tmp_path):
    """`param-is: 130` and `map-to-param: 131`, as a user writes the numeric paramIds: C++ reads them as text"""
    config = plan_config(stats_mapping(param_is=130, map_to_param=131), SINK_ACTION)
    build_from_file(config, tmp_path)  # C++ builds the plan: `getString` converts the integers
    assert not python_rejects(config)
    client = Client.from_dict(config)
    mapping = client.plans[0].actions[0].mapping_param[0]
    assert (mapping.where.param_is, mapping.map_to_param) == ("130", "131")
    dumped = client.dump()
    sent = dumped["plans"][0]["actions"][0]["mapping-param"][0]
    assert (sent["where"]["param-is"], sent["map-to-param"]) == ("130", "131")  # the text that C++ makes of them
    build_from_file(dumped, tmp_path, name="dumped.yaml")
    # the numbers and their text make the same plan
    assert dumped == Client.from_dict(plan_config(stats_mapping(), SINK_ACTION)).dump()


@pytest.mark.parametrize(
    ("where", "text"),
    [
        ({"param_is": 130.0}, "130"),
        ({"param_is": [130]}, "130"),
        ({"param_is": True}, "true"),
        ({"output_frequency_is": 1}, "1"),
        ({"operation_is": 0.5}, "0.5"),
        ({"map_to_param": 1e-5}, "1e-05"),
    ],
)
def test_statistics_mapping_values_are_converted_like_cpp(where, text, tmp_path):
    config = plan_config(stats_mapping(**where), SINK_ACTION)
    build_from_file(config, tmp_path)
    assert not python_rejects(config)
    mapping = Client.from_dict(config).plans[0].actions[0].mapping_param[0]
    assert text in (
        mapping.where.param_is,
        mapping.where.output_frequency_is,
        mapping.where.operation_is,
        mapping.map_to_param,
    )


@pytest.mark.parametrize("where", [{"param_is": {"a": 1}}, {"map_to_param": [1, 2]}, {"operation_is": None}])
def test_statistics_mapping_values_that_cpp_cannot_convert_are_rejected_by_both(where, tmp_path):
    config = plan_config(stats_mapping(**where), SINK_ACTION)
    assert "Cannot convert" in rejected_by_cpp(config, tmp_path)
    assert python_rejects(config)


# `getStringVector` converts each element, `getString` of a map entry converts its value
LIST_AND_MAP_STRINGS = {
    "mask offset-fields": (
        {"type": "mask", "offset-fields": [1, True, 1.5, [2], "a"]},
        lambda client: client.plans[0].actions[0].offset_fields,
        ["1", "true", "1.5", "2", "a"],
    ),
    "statistics-mtg2 set-metadata": (
        {
            "type": "statistics-mtg2",
            "operations": ["average"],
            "output-frequency": "1d",
            "options": {"set-metadata": {"a": 1, "b": 2.5, "c": False}},
        },
        lambda client: client.plans[0].actions[0].options.set_metadata,
        {"a": "1", "b": "2.5", "c": "false"},
    ),
    "statistics restart-prefix": (
        {
            "type": "statistics",
            "operations": ["average"],
            "output-frequency": "1d",
            "options": {"restart-prefix": 12, "log-prefix": [3]},
        },
        lambda client: [
            client.plans[0].actions[0].options.restart_prefix,
            client.plans[0].actions[0].options.log_prefix,
        ],
        ["12", "3"],
    ),
    "print stream": ({"type": "print", "stream": 5}, lambda client: client.plans[0].actions[0].stream, "5"),
}


@pytest.mark.parametrize("name", LIST_AND_MAP_STRINGS)
def test_string_lists_and_maps_are_converted_like_cpp(name, tmp_path):
    action, read, expected = LIST_AND_MAP_STRINGS[name]
    config = plan_config(action, SINK_ACTION)
    build_from_file(config, tmp_path)
    assert not python_rejects(config)
    assert read(Client.from_dict(config)) == expected


LIST_STRING_REJECTIONS = {
    "offset-fields of a scalar": {"type": "mask", "offset-fields": 1},
    "offset-fields with a null": {"type": "mask", "offset-fields": [None]},
    "offset-fields with a map": {"type": "mask", "offset-fields": [{"a": 1}]},
    "offset-fields with a list of two": {"type": "mask", "offset-fields": [[1, 2]]},
    "set-metadata with a list": {
        "type": "statistics-mtg2",
        "operations": ["average"],
        "output-frequency": "1d",
        "options": {"set-metadata": {"a": [1, 2]}},
    },
}


@pytest.mark.parametrize("name", LIST_STRING_REJECTIONS)
def test_string_lists_and_maps_that_cpp_rejects_are_rejected_by_both(name, tmp_path):
    config = plan_config(LIST_STRING_REJECTIONS[name], SINK_ACTION)
    rejected_by_cpp(config, tmp_path)
    assert python_rejects(config)


def test_strings_that_the_strict_parser_reads_are_not_converted(tmp_path):
    """`interpolate-mtg2` checks `isString`: a number for its `grid` is an error in C++, and so in the models"""
    config = plan_config({"type": "interpolate-mtg2", "outputs": [{"grid": 32}]}, SINK_ACTION)
    assert "no conversion method defined" in rejected_by_cpp(config, tmp_path)
    assert python_rejects(config)
    config = plan_config({"type": "interpolate-mtg2", "outputs": [{"grid": "H32", "cache-path": 5}]}, SINK_ACTION)
    assert "no conversion method defined" in rejected_by_cpp(config, tmp_path)
    assert python_rejects(config)


def test_numeric_name_of_a_plan_is_converted_like_cpp(tmp_path):
    """A plan `name` is read with `getString`: `1` and `'1'` are the same name, so two such plans clash in both"""
    config = {"plans": [{"name": 1, "actions": [SINK_ACTION]}, {"name": "1", "actions": [SINK_ACTION]}]}
    assert rejected_by_cpp(config, tmp_path)
    assert python_rejects(config)
    config = {"plans": [{"name": 1, "actions": [SINK_ACTION]}, {"name": 2.5, "actions": [SINK_ACTION]}]}
    build_from_file(config, tmp_path)
    assert [plan.name for plan in Client.from_dict(config).plans] == ["1", "2.5"]


def test_string_that_the_model_makes_of_a_trigger_info_value_is_accepted_by_cpp(tmp_path):
    trigger = {"type": "NotifyMetadata", "key": "notify", "file": str(tmp_path / "notify.log"), "info": {"count": 3}}
    config = plan_config({"type": "sink", "sinks": [], "triggers": [trigger]})
    build_from_file(config, tmp_path)
    client = Client.from_dict(config)
    assert client.plans[0].actions[0].triggers[0].info == {"count": "3"}
    build_from_file(client.dump(), tmp_path, name="dumped.yaml")
    trigger["info"] = {"count": [1, 2]}
    assert "Cannot convert" in rejected_by_cpp(
        plan_config({"type": "sink", "sinks": [], "triggers": [trigger]}), tmp_path
    )


def test_transport_action(tmp_path):
    """The `transport` action names a top-level block of the whole config, so it needs a client and a server"""
    config = Collection(
        client=Client(plans=[Plan(name="to-server", actions=[Transport(target="server")])]),
        server=Server(transport="thread", client_count=1, plans=[Plan(name="served", actions=[Sink()])]),
    )
    try:
        build_from_file(config.dump(), tmp_path)
    except multio.MultioException as error:
        skip_if_unavailable(error)


@pytest.mark.parametrize(
    "server",
    [
        Server(transport="thread", client_count=1, plans=[Plan(name="p", actions=[Sink()])]),
        Server(
            transport="tcp",
            local_port=4441,
            servers=[{"host": "localhost", "ports": [4441]}],
            plans=[Plan(name="p", actions=[Sink()])],
        ),
        Server(transport="mpi", plans=[Plan(name="p", actions=[Sink()])]),
    ],
    ids=lambda s: s.transport,
)
def test_server_plans_are_built_by_cpp(server, tmp_path):
    """The plans of a server are the same plans as those of a client: C++ builds them as a client does"""
    build_from_model(Client(plans=server.plans))
    build_from_model(Collection(client=Client(plans=server.plans)))


def test_two_plans_and_a_disabled_plan_are_built_by_cpp():
    first = plan_with(Print())
    first.name = "first"
    second = plan_with(Null())
    second.name = "second"
    disabled = Plan(name="off", enable=False, actions=[Print()])
    build_from_model(Client(plans=[first, second, disabled]))


# --- negative cases: the models and C++ reject the same configs ----------------------------------------------------


def plan_config(*actions: dict, **plan) -> dict:
    return {"plans": [{"name": "negative", "actions": list(actions), **plan}]}


SINK_ACTION = {"type": "sink", "sinks": []}

NEGATIVE_CASES = {
    "unknown action type": (plan_config({"type": "does-not-exist"}, SINK_ACTION), "No ActionFactory called"),
    "old action name aggregation": (plan_config({"type": "aggregation"}, SINK_ACTION), "No ActionFactory called"),
    "unknown sink type": (
        plan_config({"type": "sink", "sinks": [{"type": "does-not-exist"}]}),
        "No DataSinkFactory called",
    ),
    "removed debug-sink sink": (
        plan_config({"type": "sink", "sinks": [{"type": "debug-sink"}]}),
        "No DataSinkFactory called",
    ),
    "removed trigger sink": (
        plan_config({"type": "sink", "sinks": [{"type": "trigger"}]}),
        "No DataSinkFactory called",
    ),
    "removed socket sink": (
        plan_config({"type": "sink", "sinks": [{"type": "socket"}]}),
        "No DataSinkFactory called",
    ),
    "plan without actions": (plan_config(), None),
    "plan without a terminal action": (plan_config({"type": "print"}), None),
    "encode-mtg2 with mars2grib-options": (
        plan_config({"type": "encode-mtg2", "mars2grib-options": {"a-key": True}}, SINK_ACTION),
        None,
    ),
    "encode-mtg2 with on-error": (
        plan_config({"type": "encode-mtg2", "on-error": "recover"}, SINK_ACTION),
        None,
    ),
    "scale with on-error": (
        plan_config({"type": "scale", "preset-mappings": "local-to-wmo", "on-error": "recover"}, SINK_ACTION),
        None,
    ),
    "interpolate-mtg2 with on-error": (
        plan_config({"type": "interpolate-mtg2", "outputs": [{"grid": "H32"}], "on-error": "recover"}, SINK_ACTION),
        None,
    ),
    "scale without a mapping": (plan_config({"type": "scale"}, SINK_ACTION), None),
    "getBool of a string that is no boolean": (
        plan_config({"type": "mask", "apply-bitmap": "maybe"}, SINK_ACTION),
        None,
    ),
    "parseBool of a number": (
        plan_config(
            {
                "type": "statistics",
                "operations": ["average"],
                "output-frequency": "1d",
                "options": {"write-restart": 1},
            },
            SINK_ACTION,
        ),
        None,
    ),
    "strict parser bool from 'true'": (
        plan_config({"type": "encode-mtg2", "cached": "true"}, SINK_ACTION),
        "must be '1' or '0'",
    ),
    "two plans with the same name": (
        {"plans": [{"name": "same", "actions": [SINK_ACTION]}, {"name": "same", "actions": [SINK_ACTION]}]},
        None,
    ),
}


def python_rejects(config: dict) -> bool:
    """Whether the models refuse a config: they fail to validate it, or the plans have no terminal action"""
    try:
        client = Client.from_dict(config)
        client.validate_for_cpp()
    except (ValidationError, ValueError):
        return True
    return not all(plan.check_validity() for plan in client.plans)


@pytest.mark.parametrize("name", NEGATIVE_CASES)
def test_cpp_rejects_what_python_rejects(name, tmp_path):
    config, cpp_message = NEGATIVE_CASES[name]
    assert python_rejects(config)
    message = rejected_by_cpp(config, tmp_path)
    if cpp_message:
        assert cpp_message in message


def test_harness_accepts_a_valid_config(tmp_path):
    """The negative cases are not failing for a reason of the harness: the same shape, valid, builds"""
    config = plan_config({"type": "print"}, SINK_ACTION)
    assert not python_rejects(config)
    build_from_file(config, tmp_path)


def test_python_dump_of_null_action_is_quoted_for_yaml(tmp_path):
    """`type: null` is a YAML null, so a dump must quote it: C++ would read `Nil` and fail to convert it"""
    plan = plan_with(Null())
    assert "type: 'null'" in plan.to_client().dump_yaml() or 'type: "null"' in plan.to_client().dump_yaml()
    build_from_file(yaml.safe_load(plan.to_client().dump_yaml()), tmp_path)


# --- the plan files of the C++ tests ------------------------------------------------------------------------------


def parts_of(models: list) -> dict[str, dict]:
    """The configs that C++ reads from a file whose models were loaded, by name of the part

    - `file`: the whole file as C++ reads it, with the shared top-level keys moved out of the `client` and `server`
      blocks, where the models keep them.
    - `server plans`: the plans of the server, built as a client would: C++ does not build a server in process.
    - `<name>`: a config of a named collection, read by the test tools one at a time.
    """
    kinds = [type(model).__name__ for model in models]
    if kinds == ["Plan"]:
        return {"file": {"plans": [models[0].dump()]}}  # a file that other plans include
    if kinds == ["Client"]:
        return {"file": models[0].dump()}
    if kinds == ["Server"]:
        return {"file": models[0].dump(), "server plans": {"plans": models[0].dump()["plans"]}}
    if kinds == ["Client", "Server"]:
        client, server = models[0].dump(), models[1].dump()
        shared = {}
        for key in TOP_LEVEL_SHARED:
            for block in (client, server):
                if key in block:
                    shared[key] = block.pop(key)
        return {
            "file": {**shared, "client": client, "server": server},
            "server plans": {"plans": server["plans"]},
        }
    if kinds == ["Collection"]:
        return {f"config {name}": config.dump() for name, config in models[0].configs.items()}
    raise NoRecognisedShape(f"unknown shapes {kinds}")


@pytest.mark.parametrize("name", VALIDATED_FILES)
def test_corpus_file_is_built_by_cpp(name, tmp_path):
    models = check_file(REPO_ROOT / name)
    files = Fixtures(tmp_path)
    skipped = []
    for part, config in parts_of(models).items():
        try:
            config = files.prepare(config)
        except MissingFixture as error:
            skipped.append(f"{part}: {error}")
            continue
        try:
            build_from_file(config, tmp_path, name=f"{part.replace(' ', '_')}.yaml")
        except multio.MultioException as error:
            try:
                skip_if_unavailable(error)
            except pytest.skip.Exception as skip:
                skipped.append(f"{part}: {skip}")
    if skipped:
        pytest.skip("; ".join(skipped))
