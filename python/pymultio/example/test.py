import os

# Optional
import numpy as np

from multio import Multio, MultioPlan

dir = os.path.dirname(__file__)

MULTIO_PLAN = {
    "plans": [
        {
            "name": "Write grib",
            "actions": [
                {"type": "select", "match": [{"format": "grib"}]},
                {
                    "type": "print",
                    "stream": "cout",
                    "prefix": "GRIB +++ ",
                },
                {
                    "type": "sink",
                    "sinks": [
                        {
                            "type": "file",
                            "append": False,
                            "per-server": False,
                            "path": "{dir}/testOut.grib".format(dir=dir),
                        }
                    ],
                },
            ],
        },
        {
            "name": "Write float data",
            "actions": [
                {"type": "select", "match": [{"precision": "single"}]},
                {
                    "type": "print",
                    "stream": "cout",
                    "prefix": "FLOAT +++ ",
                },
                {
                    "type": "sink",
                    "sinks": [
                        {
                            "type": "file",
                            "append": False,
                            "per-server": False,
                            "path": "{dir}/testFloat.bin".format(dir=dir),
                        }
                    ],
                },
            ],
        },
        {
            "name": "Write double data",
            "actions": [
                {"type": "select", "match": [{"precision": "double"}]},
                {
                    "type": "print",
                    "stream": "cout",
                    "prefix": "DOUBLE +++ ",
                },
                {
                    "type": "sink",
                    "sinks": [
                        {
                            "type": "file",
                            "append": False,
                            "per-server": False,
                            "path": "{dir}/testDouble.bin".format(dir=dir),
                        }
                    ],
                },
            ],
        },
    ]
}

# Not really neaded, just for MPI setups with server nodes
conf_dict = {"allow_world": True, "parent_comm": 1, "client_comm": [2, 3], "server_comm": [4, 5]}

# conf = multiopython.Config(allow_world=True, parent_comm=1, client_comm=[2,3], server_comm=[4,5])

with MultioPlan(MULTIO_PLAN):
    with Multio(**conf_dict) as mio:
        metadata = {
            "category": "path",
            "globalSize": 4,
            "level": 1,
            "step": 1,
            "trigger": "step",
            "missingValue": 0.0,
            "bitmapPresent": False,
            "bitsPerValue": 16,
            "toAllServers": False,
            "name": "test",
        }

        # Opening the binary file in binary mode as rb(read binary)
        gribData = None
        with open("{dir}/test.grib".format(dir=dir), mode="rb") as file:
            gribData = file.read()
            mio.write_grib(gribData)

        mio.write_field(None, [1.0, 2.0])
        mio.write_field(None, np.array([3.0, 4.0], dtype=np.float64))
        mio.write_field(None, np.array([1.0, 2.0], dtype=np.float32))
        mio.notify(metadata={"trigger": "step", "step": 1})
        mio.flush()
        mio.field_accepted({"format": "grib"})
        mio.field_accepted({"precision": "single"})
        mio.field_accepted({"precision": "double"})
        mio.field_accepted(None)


gribTestData = None
with open("{dir}/testOut.grib".format(dir=dir), mode="rb") as file:
    gribTestData = file.read()
    assert gribTestData == gribData
os.remove("{dir}/testOut.grib".format(dir=dir))

testFloat = np.fromfile("{dir}/testFloat.bin".format(dir=dir), dtype=np.float32)
assert np.all(testFloat == np.array([1.0, 2.0], dtype=np.float32))
os.remove("{dir}/testFloat.bin".format(dir=dir))

testDouble = np.fromfile("{dir}/testDouble.bin".format(dir=dir), dtype=np.float64)
assert np.all(testDouble == np.array([1.0, 2.0, 3.0, 4.0], dtype=np.float64))
os.remove("{dir}/testDouble.bin".format(dir=dir))
