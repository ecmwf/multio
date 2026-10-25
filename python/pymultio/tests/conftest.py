# (C) Copyright 2026 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""Shared skip logic of the tests, so that every test says "this part of the tree is missing" the same way.

- `@pytest.mark.requires_lib`: libmultio cannot be loaded.
- `@pytest.mark.requires_corpus`: the C++ `tests` and `share` trees are not there (an installed sdist).
- the `cpp_source_dir` fixture: the C++ sources (`MULTIO_CPP_SOURCE_ROOT` or `src/multio`) are not there.
"""

import sys
from pathlib import Path

import pytest

# Make the helper modules of this directory importable by name, whatever the import mode of pytest
_HERE = str(Path(__file__).resolve().parent)
if _HERE not in sys.path:
    sys.path.insert(0, _HERE)

from multio_testing import REPO_ROOT, corpus_available, cpp_source_root, multio_lib_available  # noqa: E402

MARKERS = {
    "requires_lib": ("the libmultio C library must be loadable", multio_lib_available, "libmultio could not be loaded"),
    "requires_corpus": (
        "the C++ test and share trees must exist",
        corpus_available,
        f"the C++ tests and share trees are not found under {REPO_ROOT} (e.g. installed sdist)",
    ),
}


def pytest_configure(config):
    for name, (description, _, _) in MARKERS.items():
        config.addinivalue_line("markers", f"{name}: skip when not satisfied: {description}")


def pytest_collection_modifyitems(config, items):
    available: dict[str, bool] = {}
    for item in items:
        for name, (_, check, reason) in MARKERS.items():
            if item.get_closest_marker(name) is None:
                continue
            if name not in available:
                available[name] = check()
            if not available[name]:
                item.add_marker(pytest.mark.skip(reason=reason))


@pytest.fixture(scope="session")
def cpp_source_dir() -> Path:
    """The directory of the C++ sources; skips the test when it does not exist."""
    root = cpp_source_root()
    if not root.is_dir():
        pytest.skip(f"C++ sources not found at {root} (set MULTIO_CPP_SOURCE_ROOT)")
    return root
