# (C) Copyright 2026 European Centre for Medium-Range Weather Forecasts.
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

import pytest
import yaml
from pydantic import ValidationError

from multio.plans import Plan

from cpp_corpus import (
    ALL_FILES,
    CPP_STALE_FILES,
    NON_PLAN_FILES,
    SKIPPED,
    VALIDATED_FILES,
    NoRecognisedShape,
    check_file,
    load_models,
)
from multio_testing import REPO_ROOT


def test_skip_lists_do_not_overlap():
    assert not set(NON_PLAN_FILES) & set(CPP_STALE_FILES)


@pytest.mark.requires_corpus
def test_files_are_found():
    # a lower bound, well under the current count, so that a broken glob cannot pass silently
    assert len(ALL_FILES) > 100
    assert any(name.startswith("tests/across/") for name in ALL_FILES)
    assert any(name.startswith("tests/multio/nextGEMS/") for name in ALL_FILES)
    assert any(name.startswith("share/multiom/plans/") for name in ALL_FILES)


@pytest.mark.requires_corpus
@pytest.mark.parametrize("name", sorted(SKIPPED))
def test_skipped_file_exists(name):
    # lexists: a skip-listed symlink may be dangling, which is why it is skipped
    assert os.path.lexists(REPO_ROOT / name), f"{name} is in a skip list but no longer exists: remove it"


@pytest.mark.requires_corpus
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


@pytest.mark.requires_corpus
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


@pytest.mark.requires_corpus
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


@pytest.mark.requires_corpus
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
