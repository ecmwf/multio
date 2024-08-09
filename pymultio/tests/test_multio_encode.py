import json
import os
from collections import OrderedDict

import numpy as np
import pytest
from pyeccodes import Reader

from multiopython import Multio


def test_encode_stream_mmsf_1():
    dir = os.path.dirname(__file__)

    os.environ['MULTIO_PLANS'] = json.dumps({
        "on-error": "propagate",
        "plans": [
            {
                "name": "Write grib",
                "actions": [
                    {
                        "type": "print",
                        "stream": "cout",
                        "prefix": " +++ ",
                    },
                    {
                        "type": "encode",
                        "format": "grib",
                        "template": "{dir}/test.grib".format(dir=dir),
                        "overwrite": [
                            {"setLocalDefinition": 1},
                            {"localDefinitionNumber": 15},
                            {"productDefinitionTemplateNumber": 11}, # 11 for stat, 1 for instant
                        ],
                        "additional-metadata": {
                            "stream": "mmsf",
                            "type": "fc",
                            "methodNumber": 4,
                            "systemNumber": 3,
                            "perturbationNumber": 2,
                        }
                    },
                    {
                        "type": "sink",
                        "sinks": [
                            {
                                "type": "file",
                                "append": False,
                                "per-server": False,
                                "path": "{dir}/testMMSF1.grib".format(dir=dir)
                            }
                        ]
                    }
                ]
            }
        ]
    })

    print(os.environ['MULTIO_PLANS'])

    mio = Multio()

    # Opening the binary file in binary mode as rb(read binary)

    data = np.ones(1024, dtype=np.float64)
    mio.write_field(data, metadata={"startDate": 20240101, "startTime": 1200, "startStep":1,
                                    "endStep": 2, "globalSize": len(data), "param": 228050})
    mio.flush()

    gribs = list(Reader("{dir}/testMMSF1.grib".format(dir=dir)))
    assert(len(gribs) == 1)

    assert(gribs[0].get("stream") == "mmsf")
    assert(gribs[0].get("type") == "fc")
    assert(gribs[0].get("methodNumber") == 4)
    assert(gribs[0].get("systemNumber") == 3)
    assert(gribs[0].get("forecastTime") == 1)
    assert(gribs[0].get("date") == 20240101)
    assert(gribs[0].get("hour") == 0)
    assert(gribs[0].get("minute") == 12)
    assert(gribs[0].get("second") == 0)
    assert(gribs[0].get("perturbationNumber") == 2)

    os.remove('{dir}/testMMSF1.grib'.format(dir=dir))

def test_encode_stream_mmsf_2():
    dir = os.path.dirname(__file__)

    os.environ['MULTIO_PLANS'] = json.dumps({
        "on-error": "propagate",
        "plans": [
        {
            "name": "Write grib",
            "actions": [
                { "type": "print",
                  "stream": "cout",
                  "prefix": " +++ ",
                },
                {
                    "type": "encode",
                    "format": "grib",
                    "template": "{dir}/test.grib".format(dir=dir),
                    "additional-metadata": {
                        "setLocalDefinition": 1,
                        "localDefinitionNumber": 15,
                        "productDefinitionTemplateNumber": 11,
                        "stream": "mmsf",
                        "type": "fc",
                        "methodNumber": 4,
                        "systemNumber": 3,
                        "perturbationNumber": 2,
                    }
                },
                {
                    "type": "sink",
                    "sinks": [{
                        "type": "file",
                        "append": False,
                        "per-server": False,
                        "path": "{dir}/testMMSF2.grib".format(dir=dir)
                    }]
                }
            ]
        }
        ]
    })

    print(os.environ['MULTIO_PLANS'])

    mio = Multio()

    # Opening the binary file in binary mode as rb(read binary)

    data = np.ones(1024, dtype=np.float64)
    mio.write_field(data, metadata={"startDate": 20240101, "startTime": 1200, "startStep":1,
                                    "endStep": 2, "globalSize": len(data), "param": 228050})
    mio.flush()

    gribs = list(Reader("{dir}/testMMSF2.grib".format(dir=dir)))
    assert(len(gribs) == 1)

    assert(gribs[0].get("stream") == "mmsf")
    assert(gribs[0].get("type") == "fc")
    assert(gribs[0].get("methodNumber") == 4)
    assert(gribs[0].get("systemNumber") == 3)
    assert(gribs[0].get("forecastTime") == 1)
    assert(gribs[0].get("date") == 20240101)
    assert(gribs[0].get("hour") == 0)
    assert(gribs[0].get("minute") == 12)
    assert(gribs[0].get("second") == 0)
    assert(gribs[0].get("perturbationNumber") == 2)

    os.remove('{dir}/testMMSF2.grib'.format(dir=dir))


def test_encode_stream_msmm_1():
    dir = os.path.dirname(__file__)

    os.environ['MULTIO_PLANS'] = json.dumps({
        "on-error": "propagate",
        "plans": [
        {
            "name": "Write grib",
            "actions": [
                { "type": "print",
                  "stream": "cout",
                  "prefix": " +++ ",
                },
                {
                    "type": "encode",
                    "format": "grib",
                    "template": "{dir}/test.grib".format(dir=dir),
                    "overwrite": [
                        {"setLocalDefinition": 1},
                        {"localDefinitionNumber": 16},
                        {"productDefinitionTemplateNumber": 11}, # 11 for stat, 1 for instant
                    ],
                    "additional-metadata": {
                        "stream": "msmm",
                        "type": "fcmean",
                        "methodNumber": 4,
                        "systemNumber": 3,
                        "perturbationNumber": 2,
                    }
                },
                {
                    "type": "sink",
                    "sinks": [{
                        "type": "file",
                        "append": False,
                        "per-server": False,
                        "path": "{dir}/testMSMM1.grib".format(dir=dir)
                    }]
                }
            ]
        }
        ]
    })

    print(os.environ['MULTIO_PLANS'])

    mio = Multio()

    # Opening the binary file in binary mode as rb(read binary)

    data = np.ones(1024, dtype=np.float64)
    mio.write_field(data, metadata={"startDate": 20240101, "startTime": 1200, "startStep":1,
                                    "endStep": 2, "globalSize": len(data), "param": 228050})
    mio.flush()

    gribs = list(Reader("{dir}/testMSMM1.grib".format(dir=dir)))
    assert(len(gribs) == 1)

    assert(gribs[0].get("stream") == "msmm")
    assert(gribs[0].get("type") == "fcmean")
    assert(gribs[0].get("methodNumber") == 4)
    assert(gribs[0].get("systemNumber") == 3)
    assert(gribs[0].get("forecastTime") == 1)
    assert(gribs[0].get("date") == 20240101)
    assert(gribs[0].get("hour") == 0)
    assert(gribs[0].get("minute") == 12)
    assert(gribs[0].get("second") == 0)
    assert(gribs[0].get("perturbationNumber") == 2)

    os.remove('{dir}/testMSMM1.grib'.format(dir=dir))



def test_encode_stream_lwda_1():
    dir = os.path.dirname(__file__)

    os.environ['MULTIO_PLANS'] = json.dumps({
        "on-error": "propagate",
        "plans": [
        {
            "name": "Write grib",
            "actions": [
                { "type": "print",
                  "stream": "cout",
                  "prefix": " +++ ",
                },
                {
                    "type": "encode",
                    "format": "grib",
                    "template": "{dir}/test.grib".format(dir=dir),
                    "overwrite": [
                        {"setLocalDefinition": 1},
                        {"localDefinitionNumber": 36},
                        {"productDefinitionTemplateNumber": 8}, # 8 for stat, 0 for instant
                    ],
                    "additional-metadata": {
                        "stream": "lwda",
                        "type": "4v",
                        "offsetToEndOf4DvarWindow": 3,
                        "lengthOf4DvarWindow": 4
                    }
                },
                {
                    "type": "sink",
                    "sinks": [{
                        "type": "file",
                        "append": False,
                        "per-server": False,
                        "path": "{dir}/testLWDA1.grib".format(dir=dir)
                    }]
                }
            ]
        }
        ]
    })

    print(os.environ['MULTIO_PLANS'])

    mio = Multio()

    # Opening the binary file in binary mode as rb(read binary)

    data = np.ones(1024, dtype=np.float64)
    mio.write_field(data, metadata={"startDate": 20240101, "startTime": 1200, "startStep":1,
                                    "endStep": 2, "globalSize": len(data), "param": 228050})
    mio.flush()

    gribs = list(Reader("{dir}/testLWDA1.grib".format(dir=dir)))
    assert(len(gribs) == 1)

    assert(gribs[0].get("stream") == "lwda")
    assert(gribs[0].get("type") == "4v")
    assert(gribs[0].get("offsetToEndOf4DvarWindow") == 3)
    assert(gribs[0].get("lengthOf4DvarWindow") == 4)
    assert(gribs[0].get("forecastTime") == 1) # 4v is trajectory
    assert(gribs[0].get("date") == 20240101)
    assert(gribs[0].get("hour") == 0) # 4v is trajectory
    assert(gribs[0].get("minute") == 12)
    assert(gribs[0].get("second") == 0)

    os.remove('{dir}/testLWDA1.grib'.format(dir=dir))
