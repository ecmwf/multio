# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import pytest

from multio.plans import sinks
from multio.plans.actions import Aggregation, Encode, EncodeMTG, Mask, Print, Select, Sink, Transport


@pytest.mark.parametrize(
    ("action", "type", "kwargs"),
    (
        (Select, "select", {"match": [{"category": "custom"}]}),
        (Print, "print", {"stream": "cout", "prefix": " ++ MULTIO-PRINT-ALL-DEBUG :: ", "only-fields": False}),
        (Mask, "mask", {}),
        (Encode, "encode", {"format": "grib", "template": "template", "grid-type": "grid_type"}),
        (EncodeMTG, "encode-mtg2", {}),
        (Transport, "transport", {"target": "target"}),
        (Aggregation, "aggregation", {}),
        (Sink, "sink", {"sinks": [{"append": True, "path": "debug.grib", "per-server": False, "type": "file"}]}),
    ),
)
def test_action_default_values(action, type, kwargs):
    action_cls = action(**kwargs)
    assert action_cls.type == type
    assert action_cls.model_dump(exclude_unset=True) == {
        **kwargs,
    }


def test_add_sinks():
    sink = Sink()
    sink.add_sink({"type": "file", "path": "output.txt", "append": False})
    assert isinstance(sink.sinks[0], sinks.File)


def test_extend_sinks():
    sink = Sink()
    sink.extend_sinks([{"type": "file", "path": "output.txt", "append": False}])
    assert isinstance(sink.sinks[0], sinks.File)
