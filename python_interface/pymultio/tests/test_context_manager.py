# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

from multio import MultioPlan, plans


def test_from_config():
    test_config = plans.Client(plans=[plans.Plan(name="test")])
    test_plan = MultioPlan(test_config)
    assert isinstance(test_plan, MultioPlan)
    assert test_plan._config == test_config


def test_from_json():
    test_config = plans.Client(plans=[plans.Plan(name="test")])
    test_plan = MultioPlan(test_config.dump_json())
    assert isinstance(test_plan, MultioPlan)
    assert test_plan._config == test_config


def test_from_yaml_str():
    test_config = plans.Client(plans=[plans.Plan(name="test")])
    test_plan = MultioPlan(test_config.dump_yaml())
    assert isinstance(test_plan, MultioPlan)
    assert test_plan._config == test_config


def test_from_dict():
    test_config = plans.Client(plans=[plans.Plan(name="test")])
    test_plan = MultioPlan(test_config.model_dump())
    assert isinstance(test_plan, MultioPlan)
    assert test_plan._config == test_config


def test_from_file(tmp_path):
    test_config = plans.Client(plans=[plans.Plan(name="test")])

    tmp_path.mkdir(exist_ok=True)
    tmp_path = tmp_path / "test.yaml"
    test_config.write(str(tmp_path), format="yaml")

    test_plan = MultioPlan(str(tmp_path))
    assert isinstance(test_plan, MultioPlan)
    assert test_plan._config == test_config
