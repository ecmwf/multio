# (C) Copyright 2026 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""Check that the Python plan models mirror the actions and sinks registered in C++."""

import re
from pathlib import Path

import pytest

from multio.plans import actions, sinks

from cpp_scan import strip_comments
from multio_testing import models_of_module, union_models

# `static ActionBuilder<Foo> FooBuilder("foo");`, optionally namespace qualified (`sink::DataSinkBuilder<...>`)
ACTION_REGISTRATION = re.compile(r"\bActionBuilder\s*<[^<>;]*>\s*\w+\s*\(\s*\"([^\"]+)\"\s*\)")
SINK_REGISTRATION = re.compile(r"\bDataSinkBuilder\s*<[^<>;]*>\s*\w+\s*\(\s*\"([^\"]+)\"\s*\)")

# Lower bounds, well under the current counts (19 actions, 4 sinks), so that a broken regex cannot pass silently
MIN_ACTIONS = 15
MIN_SINKS = 3


def cpp_sources(root: Path) -> dict[Path, str]:
    return {path: path.read_text() for path in sorted(root.rglob("*.cc"))}


def registered_names(sources: dict[Path, str], pattern: re.Pattern) -> set[str]:
    return {name for text in sources.values() for name in pattern.findall(strip_comments(text))}


def python_types(union) -> set[str]:
    return {model.model_fields["type"].default for model in union_models(union)}


def diff_message(kind: str, python: set[str], cpp: set[str]) -> str:
    """Empty if the sets agree, else a readable description of the difference."""
    if python == cpp:
        return ""
    return (
        f"Python {kind} models do not match the C++ registrations:\n"
        f"  missing in Python (registered in C++): {sorted(cpp - python)}\n"
        f"  extra in Python (not registered in C++): {sorted(python - cpp)}"
    )


@pytest.fixture(scope="module")
def sources(cpp_source_dir):
    return cpp_sources(cpp_source_dir)


def test_actions_match_cpp(sources):
    cpp = registered_names(sources, ACTION_REGISTRATION)
    assert len(cpp) >= MIN_ACTIONS, f"Only found {len(cpp)} action registrations, is the regex broken?"
    message = diff_message("action", python_types(actions.ACTIONS), cpp)
    assert not message, message


def test_sinks_match_cpp(sources):
    cpp = registered_names(sources, SINK_REGISTRATION)
    assert len(cpp) >= MIN_SINKS, f"Only found {len(cpp)} sink registrations, is the regex broken?"
    message = diff_message("sink", python_types(sinks.SINKS), cpp)
    assert not message, message


def test_registry_comparison_detects_renamed_registration(sources):
    """A renamed C++ registration (here applied to the scanned text only) must be reported as a diff."""
    renamed = {
        path: text.replace('AggregateBuilder("aggregate")', 'AggregateBuilder("aggregation")')
        for path, text in sources.items()
    }
    assert renamed != sources
    cpp = registered_names(renamed, ACTION_REGISTRATION)
    message = diff_message("action", python_types(actions.ACTIONS), cpp)
    assert "missing in Python (registered in C++): ['aggregation']" in message
    assert "extra in Python (not registered in C++): ['aggregate']" in message


def test_registration_regexes():
    text = """
    static ActionBuilder<Foo> FooBuilder("foo");
    static sink::DataSinkBuilder<Bar> BarBuilder("bar");
    // static ActionBuilder<Old> OldBuilder("old");
    /* static DataSinkBuilder<Old> OldSink("old-sink"); */
    """
    assert registered_names({Path("x.cc"): text}, ACTION_REGISTRATION) == {"foo"}
    assert registered_names({Path("x.cc"): text}, SINK_REGISTRATION) == {"bar"}


def test_actions_union_and_all_are_complete():
    models = models_of_module(actions, base=actions.Action, literal_type=True)
    assert models == set(union_models(actions.ACTIONS))
    assert {model.__name__ for model in models} <= set(actions.__all__)


def test_sinks_union_and_all_are_complete():
    models = models_of_module(sinks, base=sinks.Sinks, literal_type=True)
    assert models == set(union_models(sinks.SINKS))
    assert {model.__name__ for model in models} <= set(sinks.__all__)
