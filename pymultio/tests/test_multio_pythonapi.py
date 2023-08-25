import pytest

import multiopython


default_dict = {
      "allow_world" : True,
      "parent_comm" : 1,
      "client_comm" : [2,3],
      "server_comm" : [4,5]
    }

def test_initialisation():
    multiopython.Multio(allow_world=True, parent_comm=1, client_comm=[2,3], server_comm=[4,5])

#def test_initialisation_no_config():
#    with pytest.raises(AttributeError):
#        multiopython.Multio()

def test_multio_version():
    assert(multiopython.Multio(allow_world=True, parent_comm=1, client_comm=[2,3], server_comm=[4,5]).__version__() == '2.0.0')

def test_multio_wrong_config_path():
    incorrect_path = "I_AM_NOT_HERE/multio/config/multio-server.yaml"
    with pytest.raises(multiopython.MultioException):
        multiopython.Multio(config_path=incorrect_path, allow_world=True, parent_comm=1, client_comm=[2,3], server_comm=[4,5])

def test_multio_config_path():
    config_path = "../../tests/multio/config/testPlan.yaml"
    with pytest.raises(multiopython.MultioException):
        multiopython.Multio(config_path=config_path, allow_world=True, parent_comm=1, client_comm=[2,3], server_comm=[4,5])

def test_multio_set_config_path():
    multio = multiopython.Multio(allow_world=True, parent_comm=1, client_comm=[2,3], server_comm=[4,5])
    multio.set_conf_path("/Users/maaw/multio/tests/multio/config/testPlan.yaml")

def test_multio_open_close_connections():
    multio = multiopython.Multio(allow_world=True, parent_comm=1, client_comm=[2,3], server_comm=[4,5])
    multio.open_connections()
    multio.close_connections()

def test_create_metadata():
    multio = multiopython.Multio(allow_world=True, parent_comm=1, client_comm=[2,3], server_comm=[4,5])
    metadata = {'category' : 'path',
      'new' : 1,
      'new_float' : 1.0,
      'trigger' : 'step',
      'step': 1
    }
    multio.create_metadata(metadata)

def test_create_wrong_metadata():
    multio = multiopython.Multio(allow_world=True, parent_comm=1, client_comm=[2,3], server_comm=[4,5])
    metadata = {'category' : 'path',
      'new' : 1,
      'new_float' : 1.0,
      'trigger' : 'step',
      'step': 1,
      'pair' : (1,2)
    }
    with pytest.raises(TypeError):
        multio.create_metadata(metadata)

def test_write_field():
    metadata = {'category' : 'path',
      'new' : 1,
      'new_float' : 1.0,
      'trigger' : 'step',
      'step': 1
    }
    multio_object = multiopython.Multio(allow_world=True, parent_comm=1, client_comm=[2,3], server_comm=[4,5])
    multio_object.create_metadata(md=metadata)

    multio_object.open_connections()
    multio_object.write_field([1.0, 2.0, 3.0, 4.0])
    multio_object.flush()
    multio_object.notify()
    #assert multio_object.field_accepted()

    multio_object.close_connections()

def test_write_no_metadata():
    multio_object = multiopython.Multio(allow_world=True, parent_comm=1, client_comm=[2,3], server_comm=[4,5])

    multio_object.open_connections()
    with pytest.raises(AttributeError):
        multio_object.write_field([1.0, 2.0, 3.0, 4.0])

def test_field_accepted():
    metadata = {'category' : 'path',
      'new' : 1,
      'new_float' : 1.0,
      'trigger' : 'step',
      'step': 1
    }
    multio_object = multiopython.Multio(allow_world=True, parent_comm=1, client_comm=[2,3], server_comm=[4,5])
    multio_object.create_metadata(md=metadata)

    multio_object.open_connections()
    multio_object.write_field([1.0, 2.0, 3.0, 4.0])
    multio_object.flush()
    multio_object.notify()
    assert multio_object.field_accepted() == False #Unsure if this should be true or false on return need to look at definition in c api

def test_enter_exit_connections():
    metadata = {'category' : 'path',
      'new' : 1,
      'new_float' : 1.0,
      'trigger' : 'step',
      'step': 1
    }
    with multiopython.Multio(allow_world=True, parent_comm=1, client_comm=[2,3], server_comm=[4,5]) as multio_object:
        assert(multio_object.__version__() == '2.0.0')
        multio_object.create_metadata(md=metadata)

        multio_object.write_field([1.0, 2.0, 3.0, 4.0])
        multio_object.flush()
        multio_object.notify()
