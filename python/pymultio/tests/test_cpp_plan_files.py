# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""Load every plan and config YAML of the C++ tests and share files through the Python plan models.

The files are found by glob under `tests/` and `share/`, so a new file is picked up automatically. A file is read as
C++ reads it (placeholders substituted, `file:` includes resolved), the model is picked by the shape of the file, and
the model must validate and pass `validate_for_cpp`. The models are built with `from_dict`, the parsing that the
public `from_yaml` uses (strict, with the value conversions of C++), never with a lax mode of pydantic.

A file that is not a plan, or that C++ itself rejects or never loads, goes in `NON_PLAN_FILES` or `CPP_STALE_FILES`
with a reason. The C++ test files are not edited. `test_skipped_file_is_still_skipped` fails when a skip-listed file
starts to validate, so the lists stay honest.
"""

import os
import re
from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from multio.plans import Client, Collection, Plan, Server

REPO_ROOT = Path(__file__).resolve().parents[3]
SEARCH_ROOTS = ("tests", "share")
YAML_SUFFIXES = (".yaml", ".yml")


def _group(reason: str, *paths: str) -> dict[str, str]:
    return {path: reason for path in paths}


# Files that are not multio plan configurations: they have no shape the plan models can read
NON_PLAN_FILES: dict[str, str] = {
    **_group(
        "metadata mapping for the `metadata-mapping` action (`mappings`/`data`), not a plan",
        "tests/multio/config/metadata-mapping/grib1-ml-encoder-overwrites.yaml",
        "tests/multio/config/metadata-mapping/grib1-pl-encoder-overwrites.yaml",
        "tests/multio/config/metadata-mapping/grib1-sfc-encoder-overwrites.yaml",
        "tests/multio/config/metadata-mapping/grib2-ml-encoder-overwrites.yaml",
        "tests/multio/config/metadata-mapping/nemo-to-grib.yaml",
        "tests/multio/config/metadata-mapping/test-mapping-tol.yaml",
        "tests/multio/action/renumber-healpix/mapping_32.yaml",
        "tests/multio/action/renumber-healpix/mapping_1024.yaml",
        "tests/multio/action/interpolate-healpix-nested/mapping_32.yaml",  # symlink to renumber-healpix
        "tests/multio/action/interpolate-healpix-nested/mapping_128.yaml",
        "tests/multio/action/interpolate-healpix-nested/mapping_1024.yaml",  # symlink to renumber-healpix
        "tests/multio/nextGEMS/plans/mapping-lwda-to-dcda.yaml",
        "tests/multio/nextGEMS/plans/mapping-lwda-to-lwda.yaml",
        "tests/multio/nextGEMS/plans/mapping-lwda-to-mnth.yaml",
        "tests/multio/nextGEMS/plans/mapping-lwda-to-moda.yaml",
        "tests/multio/nextGEMS/plans/mapping-lwda-to-monr.yaml",
        "tests/multio/nextGEMS/plans/mapping-lwda-to-scda.yaml",
    ),
    **_group(
        "list of parameter scalings read by the `scale` action, not a plan",
        "share/multio/scalings/local-to-wmo.yaml",
    ),
    **_group(
        "parameter mappings of the statistics actions, not a plan",
        "share/multio/param-mappings/average_rate_param_mappings.yaml",
        "share/multio/param-mappings/statistics_operation_mappings.yaml",
        "share/multio/param-mappings/statistics_param_mappings.yaml",
    ),
    **_group(
        "option file of the grib-to-mtg2 tool (`reader`/`stages` or flat options), not a plan",
        "share/multio/grib-to-mtg2/grib-to-mtg2-options-example.yaml",
        "share/multio/grib-to-mtg2/grib2grib.example-02.yaml",
        "share/multio/grib-to-mtg2/grib2grib.example-03.yaml",
        "share/multio/grib-to-mtg2/grib2grib.example.yaml",
        "share/multio/grib-to-mtg2/optionfile-full.yaml",
        "share/multio/grib-to-mtg2/optionfile.yaml",
    ),
    **_group(
        "configuration of the Fortran multiom tools, which point at plan files through `multio-plans-file`",
        "share/multiom/output-manager-cfg-an.yaml",
        "share/multiom/output-manager-cfg-fc.yaml",
        "share/multiom/output-manager-cfg.yaml",
    ),
    **_group(
        "FDB configuration, configured by CMake (`@CMAKE_BINARY_DIR@`), not a plan",
        "tests/multio/etc/fdb/config.yaml",
    ),
}

# Files that C++ rejects, or that no C++ test or tool loads. They are not edited, so they cannot validate
CPP_STALE_FILES: dict[str, str] = {
    **_group(
        "action types are capitalised (`Select`, `Statistics`, ...), but the C++ action factory is case sensitive "
        "(action/Action.cc:84) and throws `No ActionFactory called Select`; no C++ test or tool loads the file",
        "tests/multio/config/multio-ocean-client-side-exp-1.yaml",
    ),
    **_group(
        "the action type `Sink` is capitalised, but the C++ action factory is case sensitive (action/Action.cc:84); "
        "read by multio-encode-ocean (tools/multio-encode-ocean.cc:53), which would throw",
        "tests/multio/config/test-ocean-config.yaml",
    ),
    **_group(
        "experiment marked 'NOT TESTED YET': named server blocks (`ocean-ioserver`, ...) that no C++ reader looks "
        "for, and the action type `aggregat` is a typo; no C++ test or tool loads the file",
        "tests/multio/config/multio-server-multio-model-proposal.yaml",
    ),
    **_group(
        "the maestro sink has no `config` key (maestro/MaestroSink.cc:60-66 reads `ready-cdo` and `schema`); "
        "no C++ test or tool loads the file",
        "tests/multio/config/maestro-config.yaml",
    ),
    **_group(
        "the plan refers to `{~}/fesom_CORE2_ngrid_mappings.yaml`, a dangling symlink to ../interpolate-fesom, "
        "which does not exist, so C++ cannot read the mapping; no CMake target uses the plan",
        "tests/multio/action/interpolate-healpix-nested/fesom_CORE2_ngrid_to_HEALPix_128_nested_direct.yaml",
        "tests/multio/action/interpolate-healpix-nested/fesom_CORE2_ngrid_to_HEALPix_128_nested_original.yaml",
    ),
    **_group(
        "dangling symlink to ../interpolate-fesom/fesom_CORE2_ngrid_mappings.yaml, which does not exist",
        "tests/multio/action/interpolate-healpix-nested/fesom_CORE2_ngrid_mappings.yaml",
    ),
}


# Values of the environment variables that the YAML files use as placeholders, with a plausible type for the key.
# C++ replaces `{NAME}` by the environment variable, and fails when it is not set (MultioConfiguration.cc:207-221).
ENVIRONMENT = {
    "MULTIO_RAPS_PLANS_PATH": str(REPO_ROOT / "tests/multio/nextGEMS/plans"),
    "MULTIO_RAPS_TEMPLATES_PATH": "/dummy/templates",
    "MULTIO_RAPS_OUTPUT_PATH": "/dummy/output",
    "MULTIO_RAPS_GRID_TYPE": "O1280",
    "MULTIO_RAPS_DO_STATISTICS_RESTART": "true",
    "MULTIO_RAPS_STATISTICS_RESTART_PATH": "/dummy/restart",
    "CMAKE_BINARY_HOME": "/dummy/build",
    # statistics tests set these through ctest ENVIRONMENT
    "THRESHOLD": "10",
    "RESULT_FILE": "/dummy/result.grib",
    "READ_RESTART": "false",
    "WRITE_RESTART": "true",
    "RESTART_ID": "-1",
    "OPERATION": "average",
    "RESTART_MAIN_FOLDER": "/dummy/restart",
}
ENVIRONMENT_PATTERNS = (
    # the nextGEMS script enables every plan and sink
    (re.compile(r"MULTIO_RAPS_ENABLE_(PLAN|FILE|FDB5)_\w+"), "1"),
)
# `{}` and `{key: value}` are YAML flow mappings, not placeholders
PLACEHOLDER = re.compile(r"\{(~|[A-Za-z_][A-Za-z0-9_]*)\}")


# Configs read by the test tools (multio-hammer, multio-encode-ocean), see `tool_config`
TOOL_CONFIG_FILES = (
    "tests/multio/config/test-configurations.yaml",
    "tests/multio/config/test-ocean-config.yaml",
    "tests/multio/config/maestro-config.yaml",
)


class NoRecognisedShape(Exception):
    """The file is not a plan, a list of plans, a client and server, or a named collection of configs"""


def environment_value(name: str) -> str:
    if name in ENVIRONMENT:
        return ENVIRONMENT[name]
    for pattern, value in ENVIRONMENT_PATTERNS:
        if pattern.fullmatch(name):
            return value
    raise KeyError(f"environment variable {{{name}}} has no value in this test, add it to ENVIRONMENT")


def substitute(text: str, config_dir: Path) -> str:
    """Replace `{~}` by the directory of the config file and any other `{NAME}` by the environment variable.

    This runs on the raw text, as some files have an unquoted `{~}`, which is not valid YAML.
    """
    return PLACEHOLDER.sub(lambda m: str(config_dir) if m.group(1) == "~" else environment_value(m.group(1)), text)


def read_yaml(path: Path, config_dir: Path):
    return yaml.safe_load(substitute(path.read_text(), config_dir))


def resolve_includes(plans: list, including_file: Path, config_dir: Path) -> list:
    """Replace the `file:` entries of a plans list by the plan of that file, one level deep.

    C++ forces the name of the plan to the name in the file, or `file::<path>` (PlanConfiguration.cc:447-453).
    """
    resolved = []
    for entry in plans:
        if isinstance(entry, dict) and "file" in entry:
            name = substitute(entry["file"], config_dir)
            plan = read_yaml(including_file.parent / name, config_dir)
            plan.setdefault("name", f"file::{name}")
            entry = plan
        resolved.append(entry)
    return resolved


# Keys at the top of a client/server file that no C++ reader looks at: `run` is read by `encode` from its own action
TOP_LEVEL_IGNORED = {"run": "read by the `encode` action from its own config, never from the top level"}
TOP_LEVEL_SHARED = ("mpi-communicators", "active-matchers")


def tool_config(config: dict) -> dict:
    """Give a config of the test tools the shape that C++ has when it builds the client or server from it.

    `multio-hammer` and `multio-encode-ocean` read configs with the transport `none`: they build only a client from
    the plans (`PlanOnlyTestPolicy`, multio-hammer.cc:381-401), and the `parameters` map is read by the hammer itself
    (multio-hammer.cc:747), never by multio. The hammer also sets `clientCount` for every config (:510) and
    `local_port` for the tcp transport (:337) before it builds the transport, so the files do not hold them.
    """
    config = dict(config)
    if config.get("transport") == "none":
        config.pop("transport")
        config.pop("parameters", None)
    elif config.get("transport") == "thread":
        config.setdefault("clientCount", 1)
    elif config.get("transport") == "tcp":
        config.setdefault("local_port", 4441)
    return config


def load_models(path: Path) -> list:
    """Read a file as C++ does and build the models of its shape"""
    config_dir = path.parent
    data = read_yaml(path, config_dir)
    if not isinstance(data, dict):
        raise NoRecognisedShape(f"the top level is a {type(data).__name__}, not a mapping")

    def with_includes(config: dict) -> dict:
        if path.relative_to(REPO_ROOT).as_posix() in TOOL_CONFIG_FILES:
            config = tool_config(config)
        if isinstance(config.get("plans"), list):
            config = {**config, "plans": resolve_includes(config["plans"], path, config_dir)}
        return config

    if "client" in data or "server" in data:
        unknown = set(data) - {"client", "server", *TOP_LEVEL_SHARED, *TOP_LEVEL_IGNORED}
        if unknown:
            raise NoRecognisedShape(f"unexpected top-level keys {sorted(unknown)} next to client/server")
        models = []
        if "client" in data:
            # the client reads both top-level keys (MultioClient.cc:132, MpiCommSetup.cc)
            shared = {key: data[key] for key in TOP_LEVEL_SHARED if key in data}
            models.append(Client.from_dict({**shared, **with_includes(data["client"])}))
        if "server" in data:
            # nothing in the server reads `active-matchers`
            shared = {key: data[key] for key in TOP_LEVEL_SHARED if key in data and key != "active-matchers"}
            models.append(Server.from_dict({**shared, **with_includes(data["server"])}))
        return models
    if "plans" in data:
        data = with_includes(data)
        return [(Server if "transport" in data else Client).from_dict(data)]
    if "actions" in data:
        return [Plan.from_dict(data)]
    if data and all(isinstance(value, dict) and "plans" in value for value in data.values()):
        return [Collection.from_dict({name: with_includes(config) for name, config in data.items()})]
    raise NoRecognisedShape(f"top-level keys {sorted(map(str, data))} match no plan, client, server or collection")


# `{~}/file.yaml`, the mapping files of the actions. Other files, such as GRIB templates, may be generated by the build
CONFIG_DIR_REFERENCE = re.compile(r"\{~\}[^\s'\"\],}]*\.ya?ml\b")


def missing_references(path: Path) -> list[str]:
    """YAML files that the config names relative to its own directory (`{~}/...`) and that do not exist.

    C++ opens them when it builds the action (`metadata-mapping`, `renumber-healpix`, `interpolate`), so it fails on
    such a config, which the models cannot see as they only hold the path.
    """
    missing = []
    for reference in sorted(set(CONFIG_DIR_REFERENCE.findall(path.read_text()))):
        target = path.parent / reference[len("{~}/") :]
        if not target.exists():  # follows symlinks, so a dangling one is missing too
            missing.append(os.path.relpath(target, REPO_ROOT))
    return missing


def check_file(path: Path) -> list:
    models = load_models(path)
    missing = missing_references(path)
    if missing:
        raise FileNotFoundError(f"{path.name} refers to files that do not exist: {missing}")
    for model in models:
        model.validate_for_cpp()
    return models


def find_files() -> list[str]:
    return sorted(
        path.relative_to(REPO_ROOT).as_posix()
        for root in SEARCH_ROOTS
        for suffix in YAML_SUFFIXES
        for path in (REPO_ROOT / root).rglob(f"*{suffix}")
    )


ALL_FILES = find_files()
SKIPPED = {**NON_PLAN_FILES, **CPP_STALE_FILES}
VALIDATED_FILES = [name for name in ALL_FILES if name not in SKIPPED]


def test_skip_lists_do_not_overlap():
    assert not set(NON_PLAN_FILES) & set(CPP_STALE_FILES)


def test_files_are_found():
    # a lower bound, well under the current count, so that a broken glob cannot pass silently
    assert len(ALL_FILES) > 100
    assert any(name.startswith("tests/across/") for name in ALL_FILES)
    assert any(name.startswith("tests/multio/nextGEMS/") for name in ALL_FILES)
    assert any(name.startswith("share/multiom/plans/") for name in ALL_FILES)


@pytest.mark.parametrize("name", sorted(SKIPPED))
def test_skipped_file_exists(name):
    # lexists: a skip-listed symlink may be dangling, which is why it is skipped
    assert os.path.lexists(REPO_ROOT / name), f"{name} is in a skip list but no longer exists: remove it"


@pytest.mark.parametrize("name", VALIDATED_FILES)
def test_file_validates(name):
    try:
        check_file(REPO_ROOT / name)
    except NoRecognisedShape as error:
        pytest.fail(f"{name} has no recognisable shape: {error}\nAdd it to NON_PLAN_FILES if it is not a plan.")
    except ValidationError as error:
        pytest.fail(f"{name} does not validate:\n{error}")
    except FileNotFoundError as error:
        pytest.fail(f"{name}: {error}")


def test_corpus_uses_the_strict_public_parsing():
    """The corpus goes through `from_dict`, which `from_yaml` uses: placeholders give strings, `true` is read by getLong.

    The nextGEMS plans have `enable: '{MULTIO_RAPS_ENABLE_PLAN_3_2_3}'`, which C++ substitutes into a string
    (`MultioConfiguration::replaceCurly`, strings only) and reads with `parseEnabled`, and `use-current-time: true`,
    which `getLong` reads as 1. Strict pydantic validation rejects both, unless the models convert like C++.
    """
    plans = REPO_ROOT / "tests/multio/nextGEMS/plans"
    (plan,) = check_file(plans / "3_2_3_3DVariables_m1MonthlyAverage_InstantaneousFields_1_00x1_00_grib1.yaml")
    assert plan.enable == "1"
    (plan,) = check_file(plans / "3_1_1_SFCVariables_m1MonthlyAverage_AccumulatedFields_OriginalGrids_grib1.yaml")
    statistics = next(action for action in plan.actions if action.type == "statistics")
    assert statistics.options.use_current_time == 1
    options = {"use-current-time": 1.5}  # a float with a fraction: `getLong` throws, lax pydantic would truncate it
    action = {"type": "statistics", "operations": ["average"], "output-frequency": "1d", "options": options}
    with pytest.raises(ValidationError):
        Plan.from_dict({"actions": [action, {"type": "null"}]})


@pytest.mark.parametrize("name", sorted(NON_PLAN_FILES))
def test_non_plan_file_is_still_not_a_plan(name):
    path = REPO_ROOT / name
    if not os.path.lexists(path):
        pytest.skip("reported by test_skipped_file_exists")
    try:
        models = load_models(path)
    except (NoRecognisedShape, yaml.YAMLError, KeyError):
        return
    except ValidationError:
        pytest.fail(f"{name} has a plan shape but does not validate: move it to CPP_STALE_FILES if C++ rejects it")
    pytest.fail(f"{name} reads as {[type(m).__name__ for m in models]}: it is a plan, remove it from NON_PLAN_FILES")


@pytest.mark.parametrize("name", sorted(CPP_STALE_FILES))
def test_stale_file_is_still_rejected(name):
    path = REPO_ROOT / name
    if not os.path.lexists(path):
        pytest.skip("reported by test_skipped_file_exists")
    try:
        check_file(path)
    except (NoRecognisedShape, ValidationError, ValueError, yaml.YAMLError, KeyError, OSError):
        return
    pytest.fail(f"{name} now validates: remove it from CPP_STALE_FILES")
