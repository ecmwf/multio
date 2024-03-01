from multiopython import Multio
from collections import OrderedDict
from pyeccodes import Reader

# Optional
import numpy as np

import json
import os

dir = os.path.dirname(__file__)

os.environ['MULTIO_PLANS'] = json.dumps({
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
                    {"localDefinitionNumber": 15},
                    {"productDefinitionTemplateNumber": 11},
                ],
                "additional-metadata": {
                    "stream": "mmsf",
                    "type": "fc",
                    "methodNumber": 4,
                    "systemNumber": 3,
                }
            },
            {
                "type": "sink",
                "sinks": [{
                    "type": "file",
                    "append": False,
                    "per-server": False,
                    "path": "{dir}/testOut.grib".format(dir=dir)
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
mio.write_field(data, metadata={"startDate": 20240101, "startTime": 1200, "startStep":1, "endStep": 2, "globalSize": len(data)})
mio.flush()

gribs = list(Reader("{dir}/testOut.grib".format(dir=dir)))
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

os.remove('{dir}/testOut.grib'.format(dir=dir))
