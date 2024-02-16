import pytest

import multiopython


default_dict = {
      "allow_world" : True,
      "parent_comm" : 1,
      "client_comm" : [2,3],
      "server_comm" : [4,5]
    }

def test_initialisation():
    multiopython.Multio(**default_dict)

def test_initialisation_no_config():
    multiopython.Multio()

def test_multio_version():
    assert(multiopython.Multio(**default_dict).__version__() == '2.1.0')

def test_multio_wrong_config_path():
    default_dict['config_path'] = "I_AM_NOT_HERE/multio/config/multio-server.yaml"
    with pytest.raises(multiopython.MultioException):
        multiopython.Multio(**default_dict)
    del default_dict['config_path']

def test_multio_config_path():
    default_dict['config_path'] = "../../tests/multio/config/testPlan.yaml"
    with pytest.raises(multiopython.MultioException):
        multiopython.Multio(**default_dict)
    del default_dict['config_path']

def test_multio_set_config_path():
    multio = multiopython.Multio(**default_dict)
    multio.set_conf_path("/Users/maaw/multio/tests/multio/config/testPlan.yaml")

def test_multio_open_close_connections():
    multio = multiopython.Multio(**default_dict)
    multio.open_connections()
    multio.close_connections()

def test_write_field():
    import os
    import json
    os.environ['MULTIO_PLANS'] = json.dumps({
        "plans": [{
            "name": "No op",
            "actions": [
                { "type": "select",
                  "match": [{
                    "category": "custom"
                  }]
                },
                {
                    "type": "sink",
                    "sinks": []
                }
            ]
        }]
    })

    metadata = {
      'category' : 'custom',
      'new' : 1,
      'new_float' : 1.0,
      'trigger' : 'step',
      'step': 1
    }
    multio_object = multiopython.Multio(**default_dict)
    multio_object.open_connections()
    multio_object.write_domain([1, 2, 3, 4], metadata={"category": "domain"})
    multio_object.write_mask([1.0, 0.0, 1.0, 0.0], metadata={"category": "mask"})
    multio_object.write_mask_float([1.0, 0.0, 1.0, 0.0], metadata={"category": "mask"})
    multio_object.write_mask_double([1.0, 0.0, 1.0, 0.0], metadata={"category": "mask"})
    multio_object.write_mask([1.0, 0.0, 1.0, 0.0], metadata={"category": "mask"})
    multio_object.write_field([1.0, 2.0, 3.0, 4.0], metadata=metadata)
    multio_object.write_field_float([1.0, 2.0, 3.0, 4.0], metadata=metadata)
    multio_object.write_field_double([1.0, 2.0, 3.0, 4.0], metadata=metadata)
    multio_object.flush(metadata=metadata)
    multio_object.notify(metadata=metadata)
    assert multio_object.field_accepted(metadata=metadata)
    assert not multio_object.field_accepted(metadata={"category": "none"})

    multio_object.close_connections()
