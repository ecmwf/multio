# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import json
import os

import pytest

from multio import MultioPlan, plans


def _client() -> plans.Client:
    """A client with a plan C++ accepts: a plan needs at least one action"""
    return plans.Client(plans=[plans.Plan(name="test", actions=[{"type": "print"}])])


def test_from_config():
    test_config = _client()
    test_plan = MultioPlan(test_config)
    assert isinstance(test_plan, MultioPlan)
    assert test_plan._config == test_config


def test_from_json():
    test_config = _client()
    test_plan = MultioPlan(test_config.dump_json())
    assert isinstance(test_plan, MultioPlan)
    assert test_plan._config == test_config


def test_from_yaml_str():
    test_config = _client()
    test_plan = MultioPlan(test_config.dump_yaml())
    assert isinstance(test_plan, MultioPlan)
    assert test_plan._config == test_config


def test_from_dict():
    test_config = _client()
    test_plan = MultioPlan(test_config.model_dump())
    assert isinstance(test_plan, MultioPlan)
    assert test_plan._config == test_config


def test_from_file(tmp_path):
    test_config = _client()

    tmp_path.mkdir(exist_ok=True)
    tmp_path = tmp_path / "test.yaml"
    test_config.write(str(tmp_path), format="yaml")

    test_plan = MultioPlan(str(tmp_path))
    assert isinstance(test_plan, MultioPlan)
    assert test_plan._config == test_config


def test_empty_plan_is_rejected():
    """C++ rejects a plan without actions, so `MultioPlan` does not accept it, however it is given"""
    empty = plans.Client(plans=[plans.Plan(name="test")])
    with pytest.raises(ValueError, match="no actions"):
        MultioPlan(empty)
    with pytest.raises(ValueError, match="no actions"):
        MultioPlan({"plans": [{"name": "test", "actions": []}]})
    with pytest.raises(ValueError, match="no actions"):
        MultioPlan({"transport": "thread", "clientCount": 1, "plans": [{"name": "test", "actions": []}]})
    with pytest.raises(ValueError, match="no actions"):
        MultioPlan({"client": {"plans": [{"name": "test", "actions": []}]}})


def test_plan_emptied_after_creation_is_rejected_on_set():
    client = _client()
    test_plan = MultioPlan(client)
    client.plans[0].actions.clear()
    with pytest.raises(ValueError, match="no actions"):
        test_plan.set_plan()


def test_plan_filled_incrementally_is_accepted():
    plan = plans.Plan(name="test")
    plan.add_action({"type": "print"})
    with MultioPlan(plan.to_client()) as test_plan:
        assert json.loads(os.environ[test_plan._environ_var])["plans"][0]["name"] == "test"
