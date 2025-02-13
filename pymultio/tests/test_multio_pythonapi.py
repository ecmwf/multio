import json
import os

import numpy as np
import pytest

import multio

default_dict = {"allow_world": True, "parent_comm": 1, "client_comm": [2, 3], "server_comm": [4, 5]}


NO_OP_PLAN = json.dumps(
    {
        "plans": [
            {
                "name": "No op",
                "actions": [{"type": "select", "match": [{"category": "custom"}]}, {"type": "sink", "sinks": []}],
            }
        ]
    }
)

TEST_WRITE_FILE = "testWriteOutput.grib"

WRITE_FILE_PLAN = json.dumps(
    {
        "plans": [
            {
                "name": "No op",
                "actions": [
                    {"type": "select", "match": [{"category": "custom"}]},
                    {
                        "type": "sink",
                        "sinks": [{"type": "file", "append": False, "per-server": False, "path": TEST_WRITE_FILE}],
                    },
                ],
            }
        ]
    }
)


def test_initialisation():
    multio.Multio(**default_dict)


def test_multio_version():
    assert multio.Multio(**default_dict).__version__() == "2.3.0"


def test_initialisation_no_config():
    multio.Multio()


def test_multio_wrong_config_path():
    incorrect_path = "I_AM_NOT_HERE/multio/config/multio-server.yaml"
    with pytest.raises(multio.MultioException):
        multio.Multio(config_path=incorrect_path, **default_dict)


def test_multio_config_path():
    config_path = "../../tests/multio/config/testPlan.yaml"
    with pytest.raises(multio.MultioException):
        multio.Multio(config_path=config_path, **default_dict)


def test_multio_open_close_connections():
    with multio.Multio(**default_dict):
        pass


def test_create_metadata():
    multioclient = multio.Multio(**default_dict)
    metadata = {"category": "path", "new": 1, "new_float": 1.0, "trigger": "step", "step": 1}
    metadata = multio.Metadata(multioclient, metadata)


def test_metadata_set_item_syntax():
    multioclient = multio.Multio(**default_dict)
    metadata = multio.Metadata(multioclient, None)
    metadata["category"] = "path"
    metadata["new"] = 1
    metadata["new_float"] = 1.0
    metadata["trigger"] = "step"
    metadata["step"] = 1


def test_create_wrong_metadata_object():
    multioclient = multio.Multio(**default_dict)
    metadata = {"category": "path", "new": 1, "new_float": 1.0, "trigger": "step", "step": 1, "pair": (1, 2)}
    with pytest.raises(TypeError):
        multio.Metadata(multioclient, metadata)


def test_create_wrong_metadata_dict():
    multioclient = multio.Multio(**default_dict)
    metadata = {"category": "path", "new": 1, "new_float": 1.0, "trigger": "step", "step": 1, "pair": (1, 2)}
    with pytest.raises(TypeError):
        multioclient.write_field(metadata, np.array([1.0, 2.0, 3.0, 4.0]))


def test_write_field():
    os.environ["MULTIO_PLANS"] = WRITE_FILE_PLAN
    if os.path.isfile(TEST_WRITE_FILE):
        os.remove(TEST_WRITE_FILE)
    metadata = {"category": "path", "new": 1, "new_float": 1.0, "trigger": "step", "step": 1}
    with multio.Multio(**default_dict) as multio_object:
        multio_object.write_field(metadata, np.array([1.0, 2.0, 3.0, 4.0]))
        multio_object.flush(metadata)
        multio_object.notify(metadata)
        assert os.path.isfile(TEST_WRITE_FILE) is True
        os.remove(TEST_WRITE_FILE)


def test_write_field_with_context():
    with multio.MultioPlan(WRITE_FILE_PLAN):
        if os.path.isfile(TEST_WRITE_FILE):
            os.remove(TEST_WRITE_FILE)
        with multio.Multio(**default_dict) as multio_object:
            metadata = multio.Metadata(multio_object, None)
            metadata["category"] = "path"
            metadata["new"] = 1
            metadata["new_float"] = 1.0
            metadata["trigger"] = "step"
            metadata["step"] = 1

            multio_object.write_field(metadata, np.array([1.0, 2.0, 3.0, 4.0]))
            multio_object.flush(metadata)
            multio_object.notify(metadata)
            assert os.path.isfile(TEST_WRITE_FILE)
            os.remove(TEST_WRITE_FILE)


def test_write_field_use_metadata_object():
    os.environ["MULTIO_PLANS"] = WRITE_FILE_PLAN

    if os.path.isfile(TEST_WRITE_FILE):
        os.remove(TEST_WRITE_FILE)
    with multio.Multio(**default_dict) as multio_object:
        metadata = multio.Metadata(multio_object, None)
        metadata["category"] = "path"
        metadata["new"] = 1
        metadata["new_float"] = 1.0
        metadata["trigger"] = "step"
        metadata["step"] = 1

        multio_object.write_field(metadata, np.array([1.0, 2.0, 3.0, 4.0]))
        multio_object.flush(metadata)
        multio_object.notify(metadata)
        assert os.path.isfile(TEST_WRITE_FILE)
        os.remove(TEST_WRITE_FILE)


def test_write_no_metadata():
    os.environ["MULTIO_PLANS"] = WRITE_FILE_PLAN

    if os.path.isfile(TEST_WRITE_FILE):
        os.remove(TEST_WRITE_FILE)
    with multio.Multio(**default_dict) as multio_object:
        multio_object.write_field(None, np.array([1.0, 2.0, 3.0, 4.0]))
        assert os.path.isfile(TEST_WRITE_FILE) is True
        os.remove(TEST_WRITE_FILE)


def test_field_accepted():
    os.environ["MULTIO_PLANS"] = WRITE_FILE_PLAN

    if os.path.isfile(TEST_WRITE_FILE):
        os.remove(TEST_WRITE_FILE)
    metadata = {"category": "path", "new": 1, "new_float": 1.0, "trigger": "step", "step": 1}
    with multio.Multio(**default_dict) as multio_object:
        multio_object.write_field(metadata, np.array([1.0, 2.0, 3.0, 4.0]))
        multio_object.flush(metadata)
        multio_object.notify(metadata)
        assert (
            multio_object.field_accepted(metadata) is False
        )  # Unsure if this should be true or false on return need to look at definition in c api
        assert os.path.isfile(TEST_WRITE_FILE) is True
        os.remove(TEST_WRITE_FILE)


def test_enter_exit_connections():
    os.environ["MULTIO_PLANS"] = WRITE_FILE_PLAN

    if os.path.isfile(TEST_WRITE_FILE):
        os.remove(TEST_WRITE_FILE)
    metadata = {"category": "path", "new": 1, "new_float": 1.0, "trigger": "step", "step": 1}
    with multio.Multio(**default_dict) as multio_object:
        multio_object.write_field(metadata, np.array([1.0, 2.0, 3.0, 4.0]))
        multio_object.flush(metadata)
        multio_object.notify(metadata)
        assert os.path.isfile(TEST_WRITE_FILE) is True
        os.remove(TEST_WRITE_FILE)


def test_combined():
    os.environ["MULTIO_PLANS"] = NO_OP_PLAN

    metadata = {"category": "custom", "new": 1, "new_float": 1.0, "trigger": "step", "step": 1}
    with multio.Multio(**default_dict) as multio_object:
        multio_object.write_domain({"category": "domain"}, [1, 2, 3, 4])
        multio_object.write_mask({"category": "mask"}, [1.0, 0.0, 1.0, 0.0])
        multio_object.write_mask({"category": "mask"}, np.array([1.0, 0.0, 1.0, 0.0], dtype=np.float32))
        multio_object.write_mask({"category": "mask"}, np.array([1.0, 0.0, 1.0, 0.0], dtype=np.float64))
        multio_object.write_field(metadata, [1.0, 2.0, 3.0, 4.0])
        multio_object.write_field(metadata, np.array([1.0, 2.0, 3.0, 4.0], dtype=np.float32))
        multio_object.write_field(metadata, np.array([1.0, 2.0, 3.0, 4.0], dtype=np.float64))
        multio_object.flush(metadata)
        multio_object.notify(metadata)
        assert multio_object.field_accepted(metadata)
        assert not multio_object.field_accepted({"category": "none"})
