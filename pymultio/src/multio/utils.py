# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

from __future__ import annotations

import os
from contextlib import ContextDecorator

from .plans.plans import Client, Collection, Server

FILE = os.PathLike


def parse_plan_from_str(plan: str) -> dict:
    """
    Parse a plan from a string

    Parameters
    ----------
    plan : str
        Plan to be parsed

    Returns
    -------
    dict
        Parsed plan
    """
    import json

    import yaml

    methods = [
        lambda x: yaml.safe_load(open(x)),
        lambda x: json.load(open(x)),
        lambda x: yaml.safe_load(x),
        lambda x: json.load(x),
    ]
    for method in methods:
        try:
            return method(plan)
        except Exception:
            pass

    raise ValueError(f"Invalid plan, could not parse {plan}")


class MultioPlan(ContextDecorator):
    """
    Context manager to set multio plans.

    Will record state of the MULTIO_PLANS environment variable
    and revert it to its original state when exiting the context.
    """

    _prior_plan = None
    _config = None

    _environ_var = "MULTIO_PLANS"

    def __init__(self, plan: FILE | dict | Client | Server):
        """
        Create the MultioPlan context manager

        Parameters
        ----------
        plan : FILE | dict | Config
            Multio plan to be set, can be
            - a file path
            - a dictionary
            - a Config instance
            - a YAML string
            - a JSON string
        """
        self._environ_var = "MULTIO_PLANS"

        if isinstance(plan, (os.PathLike, str)):
            plan = parse_plan_from_str(plan)

        if isinstance(plan, dict):
            if "transport" in plan:
                plan = Server(**plan)
            elif "plans" in plan:
                plan = Client(**plan)
            else:
                plan = Collection(**plan)

        self._config = plan

    def set_plan(self):
        """Set plan in the environment"""
        self._prior_plan = os.environ.get(self._environ_var, None)
        os.environ[self._environ_var] = self._config.dump_json()

    def revert_plan(self):
        """Revert plan to the original state"""
        if self._prior_plan is not None:
            os.environ[self._environ_var] = self._prior_plan
        elif self._environ_var in os.environ:
            del os.environ[self._environ_var]

    def __enter__(self):
        self.set_plan()
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self.revert_plan()
