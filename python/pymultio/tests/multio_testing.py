# (C) Copyright 2026 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""Shared definitions of the tests: where the repository and the C++ sources are, libmultio, and model lookups.

This is a plain module next to the tests, imported by name (`conftest.py` puts this directory on `sys.path`).
"""

import os
import typing
from pathlib import Path

from multio.plans.base import MultioBaseModel

# The root of the multio repository, which holds `src/multio`, `tests` and `share`
REPO_ROOT = Path(__file__).resolve().parents[3]
# Repository-relative prefix of the C++ sources; `MULTIO_CPP_SOURCE_ROOT` replaces the directory behind it
CPP_PREFIX = "src/multio/"
# Directories (under the repository root) of the C++ tests and share files that hold the plan and config YAML files
SEARCH_ROOTS = ("tests", "share")


def cpp_source_root() -> Path:
    """The directory of the C++ sources (`src/multio`), or the one named by `MULTIO_CPP_SOURCE_ROOT`."""
    return Path(os.environ.get("MULTIO_CPP_SOURCE_ROOT") or REPO_ROOT / "src" / "multio")


def corpus_available() -> bool:
    """Whether the C++ test and share trees that hold the plan files exist (they do not in an sdist)."""
    return all((REPO_ROOT / root).is_dir() for root in SEARCH_ROOTS)


def multio_lib_available() -> bool:
    """Whether the multio C library can be loaded."""
    try:
        import multio.multio  # noqa: F401
    except ImportError:
        return False
    return True


def union_models(union) -> list[type]:
    """The members of a union such as `actions.ACTIONS` or `sinks.SINKS`, in order."""
    return list(typing.get_args(union))


def models_of_module(*modules, base: type = MultioBaseModel, literal_type: bool = False) -> set[type]:
    """The models defined in the modules (not imported into them) that derive from `base`.

    With `literal_type` only the concrete ones: those whose `type` field is a `Literal`.
    """
    return {
        value
        for module in modules
        for value in vars(module).values()
        if isinstance(value, type)
        and issubclass(value, base)
        and value.__module__ == module.__name__
        and (not literal_type or typing.get_origin(value.model_fields["type"].annotation) is typing.Literal)
    }
