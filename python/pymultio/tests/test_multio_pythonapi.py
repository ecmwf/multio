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
    multio_object = multio.Multio(**default_dict)
    multio_object.open_connections()
    multio_object.close_connections()


def test_create_metadata():
    multio_object = multio.Multio(**default_dict)
    metadata = {"category": "path", "new": 1, "new_float": 1.0, "trigger": "step", "step": 1}
    metadata = multio.Metadata(multio_object, metadata)


def test_metadata_set_item_syntax():
    multio_object = multio.Multio(**default_dict)
    metadata = multio.Metadata(multio_object, None)
    metadata["category"] = "path"
    metadata["new"] = 1
    metadata["new_float"] = 1.0
    metadata["trigger"] = "step"
    metadata["step"] = 1


def test_create_wrong_metadata_object():
    multio_object = multio.Multio(**default_dict)
    metadata = {"category": "path", "new": 1, "new_float": 1.0, "trigger": "step", "step": 1, "pair": (1, 2)}
    with pytest.raises(TypeError):
        multio.Metadata(multio_object, metadata)


def test_create_wrong_metadata_dict():
    multio_object = multio.Multio(**default_dict)
    metadata = {"category": "path", "new": 1, "new_float": 1.0, "trigger": "step", "step": 1, "pair": (1, 2)}
    with pytest.raises(TypeError):
        multio_object.write_field(metadata, np.array([1.0, 2.0, 3.0, 4.0]))


def test_write_field():
    os.environ["MULTIO_PLANS"] = WRITE_FILE_PLAN
    if os.path.isfile(TEST_WRITE_FILE):
        os.remove(TEST_WRITE_FILE)
    metadata = {"category": "path", "new": 1, "new_float": 1.0, "trigger": "step", "step": 1}
    multio_object = multio.Multio(**default_dict)
    multio_object.write_field(metadata, np.array([1.0, 2.0, 3.0, 4.0]))
    multio_object.flush(metadata)
    multio_object.notify(metadata)
    assert os.path.isfile(TEST_WRITE_FILE) is True
    os.remove(TEST_WRITE_FILE)


def test_write_field_with_context():
    with multio.MultioPlan(WRITE_FILE_PLAN):
        if os.path.isfile(TEST_WRITE_FILE):
            os.remove(TEST_WRITE_FILE)
        multio_object = multio.Multio(**default_dict)
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

    multio_object = multio.Multio(**default_dict)
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
    multio_object = multio.Multio(**default_dict)
    multio_object.write_field(None, np.array([1.0, 2.0, 3.0, 4.0]))
    assert os.path.isfile(TEST_WRITE_FILE) is True
    os.remove(TEST_WRITE_FILE)


def test_field_accepted():
    os.environ["MULTIO_PLANS"] = WRITE_FILE_PLAN

    if os.path.isfile(TEST_WRITE_FILE):
        os.remove(TEST_WRITE_FILE)
    metadata = {"category": "path", "new": 1, "new_float": 1.0, "trigger": "step", "step": 1}
    multio_object = multio.Multio(**default_dict)
    multio_object.write_field(metadata, np.array([1.0, 2.0, 3.0, 4.0]))
    multio_object.flush(metadata)
    multio_object.notify(metadata)
    assert (
        multio_object.field_accepted(metadata) is False
    )  # Unsure if this should be true or false on return need to look at definition in c api
    assert os.path.isfile(TEST_WRITE_FILE) is True
    os.remove(TEST_WRITE_FILE)


def test_write_parametrization_array():
    os.environ["MULTIO_PLANS"] = WRITE_FILE_PLAN
    multio_object = multio.Multio(**default_dict)
    multio_object.write_parametrization_array("key_float32", np.array([1.0, 2.0, 3.0, 4.0], dtype=np.float32))
    multio_object.write_parametrization_array("key_float64", np.array([1.0, 2.0, 3.0, 4.0], dtype=np.float64))
    multio_object.write_parametrization_array("key_int32", np.array([1, 2, 3, 4], dtype=np.int32))
    multio_object.write_parametrization_array("key_int64", np.array([1, 2, 3, 4], dtype=np.int64))
    multio_object.write_parametrization_array("key_ubyte", np.array([1, 2, 3, 4], dtype=np.ubyte))
    multio_object.write_parametrization_array("key_byte", np.array([1, 2, 3, 4], dtype=np.byte))
    multio_object.write_parametrization_array("key_raw_byte", bytearray(b"\x01\x03\x02\x05\x04"))
    multio_object.write_parametrization_array("key_list", [1, 2, 3, 4])

    # TODO extend test to check if it has arrived


def test_write_parametrization():
    os.environ["MULTIO_PLANS"] = WRITE_FILE_PLAN
    metadata = {
        "static_key": "key1",
        "key_float32": np.array([1.0, 2.0, 3.0, 4.0], dtype=np.float32),
        "key_float64": np.array([1.0, 2.0, 3.0, 4.0], dtype=np.float64),
        "key_int32": np.array([1, 2, 3, 4], dtype=np.int32),
        "key_int64": np.array([1, 2, 3, 4], dtype=np.int64),
        "key_ubyte": np.array([1, 2, 3, 4], dtype=np.ubyte),
        "key_byte": np.array([1, 2, 3, 4], dtype=np.byte),
        "key_raw_byte": bytearray(b"\x01\x03\x02\x05\x04"),
        "key_list": [1, 2, 3, 4],
    }
    multio_object = multio.Multio(**default_dict)
    multio_object.write_parametrization(metadata)
    # TODO extend test to check if it has arrived


def test_enter_exit_connections():
    os.environ["MULTIO_PLANS"] = WRITE_FILE_PLAN

    if os.path.isfile(TEST_WRITE_FILE):
        os.remove(TEST_WRITE_FILE)
    metadata = {"category": "path", "new": 1, "new_float": 1.0, "trigger": "step", "step": 1}
    multio_object = multio.Multio(**default_dict)
    multio_object.write_field(metadata, np.array([1.0, 2.0, 3.0, 4.0]))
    multio_object.flush(metadata)
    multio_object.notify(metadata)
    assert os.path.isfile(TEST_WRITE_FILE) is True
    os.remove(TEST_WRITE_FILE)


def test_combined():
    os.environ["MULTIO_PLANS"] = NO_OP_PLAN

    metadata = {"category": "custom", "new": 1, "new_float": 1.0, "trigger": "step", "step": 1}
    multio_object = multio.Multio(**default_dict)
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


# ---------------------------------------------------------------------------
# Regression tests
# ---------------------------------------------------------------------------


class _RecordingLib:
    """Stand-in for the cffi lib: records the name and arguments of every call."""

    def __init__(self):
        self.calls = []

    def __getattr__(self, name):
        if not name.startswith("multio_"):
            raise AttributeError(name)

        def record(*args):
            self.calls.append((name, args))
            return 0

        return record


@pytest.fixture
def fake_lib(monkeypatch):
    import multio.multio as multio_module

    fake = _RecordingLib()
    monkeypatch.setattr(multio_module, "lib", fake)
    return fake


@pytest.mark.requires_lib
def test_mpi_return_server_comm_calls_server_function(fake_lib):
    import multio.multio as multio_module

    config = object.__new__(multio_module._Config)
    config.config_pointer = "CONFIG"
    config.mpi_return_server_comm([4, 5])

    assert [name for name, _ in fake_lib.calls] == ["multio_mpi_return_server_comm"]
    assert fake_lib.calls[0][1][0] == "CONFIG"


@pytest.mark.requires_lib
def test_mpi_return_client_comm_calls_client_function(fake_lib):
    import multio.multio as multio_module

    config = object.__new__(multio_module._Config)
    config.config_pointer = "CONFIG"
    config.mpi_return_client_comm([2, 3])

    assert [name for name, _ in fake_lib.calls] == ["multio_mpi_return_client_comm"]


@pytest.mark.requires_lib
def test_start_server_passes_c_config_pointer(fake_lib):
    import multio.multio as multio_module

    config = object.__new__(multio_module._Config)
    config.config_pointer = "CONFIG_POINTER"
    instance = object.__new__(multio_module.Multio)
    instance._Multio__conf = config
    instance.start_server()

    assert fake_lib.calls == [("multio_start_server", ("CONFIG_POINTER",))]


@pytest.mark.requires_lib
@pytest.mark.parametrize("dtype", [np.int32, np.int64, np.float32, np.float64, np.uint8])
def test_write_grib_numpy_size_uses_array_itemsize(fake_lib, dtype):
    import multio.multio as multio_module

    instance = object.__new__(multio_module.Multio)
    instance._handle = "HANDLE"
    data = np.arange(5, dtype=dtype)
    instance.write_grib(data)

    assert len(fake_lib.calls) == 1
    name, args = fake_lib.calls[0]
    assert name == "multio_write_grib_encoded"
    assert int(args[2]) == 5 * np.dtype(dtype).itemsize


@pytest.mark.requires_lib
def test_config_from_file_does_not_print(fake_lib, capfd, tmp_path):
    import multio.multio as multio_module

    config_file = tmp_path / "config.yaml"
    config_file.write_text("")
    multio_module._Config(str(config_file), None, None, None, None)

    out, _ = capfd.readouterr()
    assert out == ""
    assert fake_lib.calls[0][0] == "multio_new_configuration_from_filename"


def _print_plan():
    return json.dumps(
        {
            "plans": [
                {
                    "name": "print",
                    "actions": [
                        {"type": "select", "match": [{"category": "custom"}]},
                        {"type": "print", "stream": "cout", "prefix": "PRINTED"},
                        {"type": "sink", "sinks": []},
                    ],
                }
            ]
        }
    )


def _printed_metadata(multio_metadata, capfd):
    """Send `multio_metadata` through a print action and return what libmultio received."""
    with multio.MultioPlan(_print_plan()):
        multio_object = multio.Multio(**default_dict)
        multio_object.write_field({"category": "custom", **multio_metadata}, np.array([1.0, 2.0]))
    out, _ = capfd.readouterr()
    line = next(line for line in out.splitlines() if "PRINTED" in line)
    start = line.index("metadata=") + len("metadata=")
    end = line.index(", payload-size")
    return json.loads(line[start:end])


@pytest.mark.requires_lib
def test_metadata_bool_reaches_set_bool(capfd):
    received = _printed_metadata({"flag_true": True, "flag_false": False}, capfd)

    assert received["flag_true"] is True
    assert received["flag_false"] is False


@pytest.mark.requires_lib
def test_metadata_bool_dispatches_to_set_bool_not_set_int(monkeypatch):
    multio_object = multio.Multio(**default_dict)
    metadata = multio.Metadata(multio_object, None)
    calls = []
    monkeypatch.setattr(metadata, "_set_bool", lambda key, value: calls.append(("bool", key, value)))
    monkeypatch.setattr(metadata, "_set_int", lambda key, value: calls.append(("int", key, value)))
    metadata["a"] = True
    metadata["b"] = 3
    assert calls == [("bool", "a", True), ("int", "b", 3)]


@pytest.mark.requires_lib
@pytest.mark.parametrize("value", [2**31, 2**40, -(2**40), 2**63 - 1])
def test_metadata_int_not_truncated_to_32_bits(capfd, value):
    received = _printed_metadata({"big": value}, capfd)

    assert received["big"] == value
