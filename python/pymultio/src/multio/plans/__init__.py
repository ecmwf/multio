# flake8: noqa: F401
# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""
Multio Plans module

Allows for Multio plans to be defined with Pydantic.

Examples
--------
```python
from multiopython.plans import actions, Client, Plan

plan = Plan(name="Plan1", plans = [{"type": "print"}])
plan.add_action({"type": "select", "match": [{"key": "value"}]})
plan.add_action({"type": "sink", "sinks": [{"type": "file", "path": "output.txt", "append": False}]})

client = Client(
    plans = [plan],
)
```
"""

from . import actions, sinks
from .actions import *
from .plans import Client, Collection, Plan, Server
from .sinks import FDB, File
