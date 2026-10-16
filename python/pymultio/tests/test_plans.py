# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import pytest
import yaml
from pydantic import ValidationError

import multio
from multio.plans import Client, DisseminationPlan, Plan, Server, actions, sinks
from multio.plans.plans import Collection

default_dict = {"allow_world": True, "parent_comm": 1, "client_comm": [2, 3], "server_comm": [4, 5]}

sample_plan = {
    "plans": [
        {
            "actions": [
                {"type": "print", "stream": "cout", "prefix": " ++ MULTIO-PRINT-ALL-DEBUG :: "},
                {
                    "sinks": [{"append": True, "path": "debug.grib", "per-server": False, "type": "file"}],
                    "type": "sink",
                },
            ],
            "name": "output-plan",
        }
    ]
}

# The shape of tests/across/plan_single_request.yaml: a plan without `actions`
dissemination_plan = {
    "name": "test-across-dissemination-single",
    "input": "O80",
    "templates-path": ".",
    "output-path": ".",
    "per-server": False,
    "templates": [{"ml": "regular_ll_pl_grib2.tmpl"}, {"sfc": "regular_ll_sfc_grib2.tmpl"}],
    "dissemination": "single_request.diss",
}

print_action = {"type": "print"}


def test_sample_config():
    Client(**sample_plan)


@pytest.mark.requires_lib
def test_with_multio_server_sample(tmp_path):
    sample = {"plans": [{**sample_plan["plans"][0]}]}
    sample["plans"][0]["actions"] = [
        sample["plans"][0]["actions"][0],
        {"type": "sink", "sinks": [{"type": "file", "path": str(tmp_path / "debug.grib"), "append": True}]},
    ]
    config = Client(**sample)
    with multio.MultioPlan(config):
        multioclient = multio.Multio(**default_dict)


@pytest.mark.requires_lib
def test_with_multio_server_empty():
    config = Client()
    with multio.MultioPlan(config):
        multioclient = multio.Multio(**default_dict)


@pytest.mark.requires_lib
def test_with_multio_enable_failonerror_and_anonymous_plan(tmp_path):
    """C++ reads a plan without name, `enable` on plans and sinks, and `failOnError`."""
    sink = {
        "type": "sink",
        "failOnError": False,
        "sinks": [
            {"type": "file", "path": str(tmp_path / "a.grib"), "enable": "yes", "failOnError": False},
            {"type": "file", "path": str(tmp_path / "b.grib"), "enable": False},
        ],
    }
    config = Client(
        plans=[
            {"actions": [print_action, sink]},
            {"name": "dropped", "enable": "no", "actions": [print_action, {"type": "sink", "sinks": []}]},
        ],
        on_error="recover",
    )
    with multio.MultioPlan(config):
        multioclient = multio.Multio(**default_dict)


def test_with_multio_server_add():
    config = Client()
    config.add_plan(Plan(name="print", actions=[{"type": "print"}]))

    assert config.plans[0].actions[0].type == "print"


def test_add_invalid_action():
    test_plan = Plan(name="testing", actions=[print_action])
    with pytest.raises(ValidationError):
        test_plan.add_action({"type": "invalid"})


def test_missing_key_action():
    test_plan = Plan(name="testing", actions=[print_action])
    with pytest.raises(ValidationError):
        test_plan.add_action({"type": "encode"})


def test_add_valid_action():
    test_plan = Plan(name="testing", actions=[print_action])
    test_plan.add_action({"type": "print"})
    assert isinstance(test_plan.actions[-1], actions.Print)


def test_extend_valid_action():
    test_plan = Plan(name="testing", actions=[print_action])
    test_plan.extend_actions([{"type": "print"}])
    assert isinstance(test_plan.actions[-1], actions.Print)


def test_conversion_to_client():
    test_plan = Plan(name="testing", actions=[{"type": "print"}])
    test_config = test_plan.to_client()
    assert isinstance(test_config, Client)
    assert test_plan == test_config.plans[0]


def test_conversion_to_server():
    test_plan = Plan(name="testing", actions=[{"type": "print"}])
    test_config = test_plan.to_server(transport="thread", client_count=1)
    assert isinstance(test_config, Server)
    assert test_plan == test_config.plans[0]


def test_conversion_to_server_requires_a_transport():
    with pytest.raises(ValidationError, match="transport"):
        Plan(name="testing", actions=[print_action]).to_server()


def test_client_conversion_to_server_does_not_nest_the_plans():
    client = Client(plans=[Plan(name="a", actions=[print_action]), Plan(name="b", actions=[print_action])])
    server = client.to_server(transport="mpi")
    assert server.plans == client.plans
    assert all(isinstance(plan, Plan) for plan in server.plans)
    assert [plan["name"] for plan in server.dump()["plans"]] == ["a", "b"]


def test_action_conversion():
    plan = Plan(
        name="testing", actions=[{"type": "print", "stream": "cout", "prefix": " ++ MULTIO-PRINT-ALL-DEBUG :: "}]
    )
    assert isinstance(plan.actions[0], actions.Print)


# ----------------------------------------------------------------------------------------------------------------------
# Plans


def test_plan_name_is_optional():
    plan = Plan(actions=[print_action])
    assert plan.name is None
    assert "name" not in plan.dump()  # C++ names it `anonymous`


def test_plan_enable():
    assert Plan(actions=[print_action]).enable is True
    assert Plan(actions=[print_action], enable=False).enable is False
    assert Plan(actions=[print_action], enable="{MULTIO_ENABLE}").enable == "{MULTIO_ENABLE}"  # an unexpanded string
    assert Plan(actions=[print_action], enable=False).dump()["enable"] is False


@pytest.mark.parametrize("enable", [1, 0, None, ["yes"], 1.5])
def test_plan_enable_rejects_other_types(enable):
    """C++ answers `Bool expected` for an `enable` that is neither a boolean nor a string."""
    with pytest.raises(ValidationError):
        Plan(actions=[print_action], enable=enable)


def test_plan_actions_must_be_a_list():
    with pytest.raises(ValidationError):
        Plan(name="empty", actions=None)


EMPTY_PLANS = [lambda: Plan(name="empty"), lambda: Plan(name="empty", actions=[]), lambda: Plan()]


@pytest.mark.parametrize("make", EMPTY_PLANS)
def test_empty_plan_is_built_but_rejected_for_cpp(make):
    """C++: `cfg.has("actions")` builds the plan, which then has no action to run. The Python plan can be built empty
    (and filled with `add_action`), but not serialised or passed to C++."""
    plan = make()
    assert plan.actions == []
    with pytest.raises(ValueError, match="no actions"):
        plan.validate_for_cpp()
    with pytest.raises(ValueError, match="no actions"):
        plan.dump()
    with pytest.raises(ValueError, match="no actions"):
        plan.model_dump()
    with pytest.raises(ValueError, match="no actions"):
        plan.dump_json()
    with pytest.raises(ValueError, match="no actions"):
        plan.to_client()
    with pytest.raises(ValueError, match="no actions"):
        plan.to_server(transport="thread", client_count=1)


def test_empty_plan_in_a_config_is_rejected_for_cpp():
    client = Client(plans=[Plan(name="ok", actions=[print_action]), Plan(name="empty")])
    server = Server(transport="thread", client_count=1, plans=client.plans)
    for config in (client, server, Collection(client=client)):
        with pytest.raises(ValueError, match="empty.*no actions"):
            config.validate_for_cpp()
        with pytest.raises(ValueError, match="no actions"):
            config.dump()
    with pytest.raises(ValueError, match="no actions"):
        client.to_server(transport="thread", client_count=1)


def test_empty_plan_can_be_filled_incrementally():
    plan = Plan(name="incremental")
    plan.add_action(print_action)
    plan.extend_actions([{"type": "null"}])
    assert plan.validate_for_cpp() is plan
    assert [action["type"] for action in plan.dump()["actions"]] == ["print", "null"]
    client = Client()
    client.add_plan(Plan(name="later"))
    client.plans[0].add_action(print_action)
    assert client.validate_for_cpp() is client
    assert client.dump()["plans"][0]["name"] == "later"


def test_disabled_empty_plan_is_dropped_by_cpp_so_it_is_accepted():
    """C++ checks `enable` before it reads the actions"""
    plan = Plan(name="off", enable=False)
    assert plan.validate_for_cpp() is plan
    assert plan.dump()["enable"] is False
    with pytest.raises(ValueError, match="no actions"):
        Plan(name="on", enable="yes").validate_for_cpp()


def test_dissemination_plan_needs_no_actions_for_cpp():
    plan = DisseminationPlan(name="d", input="O80", templates=[{"sfc": "sfc.tmpl"}], dissemination="r.diss")
    assert plan.validate_for_cpp() is plan
    Client(plans=[plan]).validate_for_cpp()
    assert plan.dump()["dissemination"] == "r.diss"


def test_plan_missing_actions_in_a_dictionary_is_an_empty_plan():
    """Without `actions` and `dissemination` C++ answers `Unable to build a plan`: the plan is empty here"""
    client = Client(plans=[{"name": "empty"}])
    with pytest.raises(ValueError, match="no actions"):
        client.validate_for_cpp()


def test_plan_on_error():
    assert Plan(actions=[print_action], on_error="Recover").on_error == "recover"  # C++ compares in lower case
    assert Plan(actions=[print_action], **{"on-error": {"type": "recover", "maxRetries": 2}}).on_error.max_retries == 2
    # the string form throws for an unknown tag in C++ ("Unsupported value")
    for invalid in ("abort-transport", "ignore", {"maxRetries": 1}):
        with pytest.raises(ValidationError):
            Plan(actions=[print_action], on_error=invalid)
    # the map form falls back to `propagate` in C++, whatever `type` holds (FailureHandling.h FailureAware)
    for unknown in ("abort-transport", "ignore"):
        assert Plan(actions=[print_action], on_error={"type": unknown}).dump()["on-error"] == {"type": unknown}


def test_plan_rejects_unknown_and_internal_keys():
    for key in ("next", "dissemination", "count", "colour"):
        with pytest.raises(ValidationError):
            Plan(actions=[print_action], **{key: "x"})


def test_plan_aliases_dump():
    plan = Plan(name="p", enable="yes", on_error="recover", actions=[print_action])
    assert plan.dump() == {
        "name": "p",
        "enable": "yes",
        "on-error": "recover",
        "actions": [{"type": "print", "stream": "info", "only-fields": False, "prefix": ""}],
    }


@pytest.mark.parametrize(
    "action",
    [
        {"type": "transport", "target": "server"},
        {"type": "sink"},
        {"type": "single-field-sink"},
        {"type": "null"},
        {"type": "debug-sink"},
    ],
)
def test_check_validity_terminal_actions(action):
    """C++ builds a chain that ends with an action that is not chained: transport, sink, single-field-sink,
    null and debug-sink."""
    assert Plan(actions=[print_action, action]).check_validity()
    assert Plan(actions=[action]).check_validity()


@pytest.mark.parametrize(
    "action",
    [print_action, {"type": "aggregate"}, {"type": "mask"}, {"type": "select", "match": {"param": 1}}],
)
def test_check_validity_chained_actions_are_not_terminal(action):
    assert not Plan(actions=[action]).check_validity()
    assert not Plan(actions=[action, print_action]).check_validity()


def test_ensure_sink():
    plan = Plan(actions=[print_action]).ensure_sink()
    assert plan.check_validity()
    assert isinstance(plan.actions[-1], actions.Sink)


# Dissemination plans


def test_dissemination_plan_without_actions():
    client = Client(plans=[dissemination_plan])
    plan = client.plans[0]
    assert isinstance(plan, DisseminationPlan)
    assert (plan.templates_path, plan.output_path, plan.per_server) == (".", ".", False)
    assert plan.check_validity()
    assert plan.dump()["dissemination"] == "single_request.diss"


def test_dissemination_plan_round_trips_to_the_c_plus_plus_shape():
    plan = Client(plans=[dissemination_plan]).plans[0].dump()
    assert {key: value for key, value in plan.items() if key != "debug"} == dissemination_plan
    assert plan["debug"] == 0


def test_dissemination_plan_defaults():
    plan = DisseminationPlan(
        name="n", input="O1280", templates=[{"sfc": "t.tmpl"}], dissemination="requests.diss", output_name="out"
    )
    assert (plan.templates_path, plan.output_path, plan.per_server, plan.debug) == (".", ".", True, 0)
    assert plan.output_name == "out"


@pytest.mark.parametrize("missing", ["name", "input", "templates", "dissemination"])
def test_dissemination_plan_missing_key_is_rejected(missing):
    plan = {key: value for key, value in dissemination_plan.items() if key != missing}
    with pytest.raises(ValidationError):
        Client(plans=[plan])


def test_dissemination_plan_ignores_actions_keys_in_c_plus_plus_so_they_are_rejected():
    for key, value in (("enable", False), ("on-error", "recover"), ("next", {})):
        with pytest.raises(ValidationError):
            Client(plans=[{**dissemination_plan, key: value}])


def test_dissemination_with_actions_is_an_action_plan():
    """C++ checks `actions` first: the plan is an ordinary one and does not read `dissemination`."""
    with pytest.raises(ValidationError):
        Client(plans=[{**dissemination_plan, "actions": [print_action]}])
    plan = Client(plans=[{"name": "a", "actions": [print_action]}]).plans[0]
    assert isinstance(plan, Plan)


def test_templates_must_be_a_list_of_maps():
    for templates in ({"sfc": "t.tmpl"}, ["t.tmpl"], [{"sfc": {"a": 1}}], [{"sfc": None}]):
        with pytest.raises(ValidationError):
            Client(plans=[{**dissemination_plan, "templates": templates}])
    # the file name is read with `getString`, which converts a number
    plan = Client(plans=[{**dissemination_plan, "templates": [{"sfc": 1}]}]).plans[0]
    assert plan.templates == [{"sfc": "1"}]


def test_dissemination_plan_can_be_added_to_config():
    client = Client()
    client.add_plan(dissemination_plan)
    client.extend_plans([{"name": "x", "actions": [print_action]}])
    assert [type(plan) for plan in client.plans] == [DisseminationPlan, Plan]


def test_client_plan_names_must_be_unique():
    """C++ rejects two plans with the same name, an unnamed plan is called `anonymous`."""
    with pytest.raises(ValidationError, match="unique"):
        Client(plans=[{"name": "a", "actions": [print_action]}, {"name": "a", "actions": [print_action]}])
    with pytest.raises(ValidationError, match="anonymous"):
        Client(plans=[{"actions": [print_action]}, {"actions": [print_action]}])
    with pytest.raises(ValidationError, match="anonymous"):
        Client(plans=[{"actions": [print_action]}, {"name": "anonymous", "actions": [print_action]}])


def test_client_plan_names_ignore_disabled_plans():
    """A disabled plan is dropped before the names are compared."""
    Client(plans=[{"name": "a", "actions": [print_action]}, {"name": "a", "enable": False, "actions": [print_action]}])
    Client(plans=[{"actions": [print_action]}, {"enable": "off", "actions": [print_action]}])


def test_server_does_not_check_plan_names():
    Server(transport="mpi", plans=[{"name": "a", "actions": [print_action]}, {"name": "a", "actions": [print_action]}])


# ----------------------------------------------------------------------------------------------------------------------
# Sinks

SINK_CONFIGS = [
    {"type": "file", "path": "out.grib"},
    {"type": "fdb5"},
    {"type": "maestro"},
    {"type": "multio", "sinks": [{"type": "file", "path": "out.grib"}]},
]


def _sink_plan(sink: dict) -> Plan:
    return Plan(name="s", actions=[{"type": "sink", "sinks": [sink]}])


@pytest.mark.parametrize("sink", SINK_CONFIGS, ids=lambda sink: sink["type"])
def test_every_sink_accepts_fail_on_error_and_enable(sink):
    built = _sink_plan({**sink, "failOnError": False, "enable": "{MULTIO_ENABLE}"}).actions[0].sinks[0]
    assert built.fail_on_error is False
    assert built.enable == "{MULTIO_ENABLE}"
    dumped = built.model_dump()
    assert dumped["failOnError"] is False and dumped["enable"] == "{MULTIO_ENABLE}"


@pytest.mark.parametrize("sink", SINK_CONFIGS, ids=lambda sink: sink["type"])
def test_sink_defaults_are_those_of_c_plus_plus(sink):
    built = _sink_plan(sink).actions[0].sinks[0]
    assert built.fail_on_error is True
    assert built.enable is True


@pytest.mark.parametrize("sink", SINK_CONFIGS, ids=lambda sink: sink["type"])
@pytest.mark.parametrize("key, value", [("enable", 1), ("enable", None), ("failOnError", [1]), ("fail-on-error", True)])
def test_sink_rejects_invalid_enable_and_fail_on_error(sink, key, value):
    """C++ answers `bool expected` for an `enable` that is neither a boolean nor a string."""
    with pytest.raises(ValidationError):
        _sink_plan({**sink, key: value})


def test_file_sink_append_defaults_to_false():
    sink = sinks.File(path="out.grib")
    assert sink.append is False
    assert sink.per_server is False
    assert sinks.File(path="out.grib", append=True).append is True


def test_file_sink_requires_a_path():
    with pytest.raises(ValidationError):
        sinks.File(append=True)


def test_fdb_config_default_is_a_fresh_map():
    first, second = sinks.FDB(), sinks.FDB()
    assert first.config == {}
    first.config["spaces"] = []
    assert second.config == {}
    assert sinks.FDB(config={"type": "local"}).config == {"type": "local"}


def test_fdb_config_file_is_loaded(tmp_path):
    path = tmp_path / "fdb.yaml"
    path.write_text("type: local\nengine: toc\n")
    assert sinks.FDB(config=path).config == {"type": "local", "engine": "toc"}


def test_maestro_sink_keys():
    sink = sinks.Maestro(**{"ready-cdo": False, "schema": ".other."})
    assert (sink.ready_cdo, sink.schema_) == (False, ".other.")
    assert sinks.Maestro().model_dump()["schema"] == ".maestro.ecmwf."


def test_nested_multio_sink_and_triggers():
    sink = sinks.MultIO(
        sinks=[{"type": "multio", "failOnError": False, "sinks": [{"type": "file", "path": "x"}]}],
        triggers=[{"type": "MetadataChange", "key": "step", "values": ["0"], "failOnRetry": 1}],
    )
    assert sink.sinks[0].fail_on_error is False
    assert sink.model_dump()["triggers"][0]["failOnRetry"] == 1


def test_removed_sinks_are_rejected():
    for sink_type in ("debug-sink", "trigger", "socket"):
        with pytest.raises(ValidationError):
            _sink_plan({"type": sink_type})


def test_sink_action_failonerror_and_sink_keys_round_trip():
    plan = Plan.from_yaml(
        "name: p\nactions:\n- type: sink\n  failOnError: false\n  sinks:\n"
        "  - {type: file, path: out.grib, enable: 'true', failOnError: false}\n"
    )
    sink = plan.actions[0]
    assert sink.fail_on_error is False
    assert sink.sinks[0].enable == "true"
    assert Plan.from_yaml(plan.dump_yaml()) == plan


# ----------------------------------------------------------------------------------------------------------------------
# Client, server and transports


def test_client_keys():
    client = Client(
        plans=[{"name": "p", "actions": [print_action]}],
        **{"on-error": "abort-transport", "active-matchers": [{"category": ["ocean-domain-map"], "name": ["sst"]}]},
    )
    assert client.on_error == "abort-transport"
    assert client.dump()["active-matchers"] == [{"category": ["ocean-domain-map"], "name": ["sst"]}]
    assert client.dump()["on-error"] == "abort-transport"


@pytest.mark.parametrize(
    "key, value",
    [
        ("on-error", "ignore"),
        ("on-error", {"type": "recover", "maxRetries": 1.5}),  # `getInt` throws for a float that is not whole
        ("active-matchers", [{"category": "ocean"}]),  # C++ reads every value as a list of strings
        ("active-matchers", {"category": ["ocean"]}),  # a map is not a list
        ("transport", "mpi"),
        ("run", {"type": "an"}),
        ("count", 2),
        ("mpi-communicators", {"multio": {"type": "other"}}),
    ],
)
def test_client_rejects_other_keys_and_values(key, value):
    with pytest.raises(ValidationError):
        Client(**{key: value})


def test_client_on_error_options():
    client = Client(on_error={"type": "RECOVER", "maxRetries": 3})
    assert client.on_error.type == "recover"
    assert client.dump()["on-error"] == {"type": "recover", "maxRetries": 3}


MPI_SERVER = {
    "transport": "mpi",
    "group": "ocean",
    "client-group": "ocean-clients",
    "server-group": "ocean-servers",
    "ecflow-step-meter-name": "step",
    "step-meter-file": "custom_step_meter_file",
    "on-error": "abort-transport",
    "on-receive-error": "propagate",
    "on-dispatch-error": {"type": "propagate", "maxRetries": 1},
    "plans": [{"name": "server-plan", "actions": [{"type": "aggregate"}, {"type": "sink", "sinks": []}]}],
}
TCP_SERVER = {
    "transport": "tcp",
    "local_port": 7771,
    "servers": [{"host": "localhost", "ports": [7771, 7772]}],
    "clients": [{"host": "localhost", "ports": [4441]}],
}
THREAD_SERVER = {"transport": "thread", "clientCount": 4}


def test_mpi_server_keys():
    server = Server(**MPI_SERVER)
    assert (server.group, server.client_group, server.server_group) == ("ocean", "ocean-clients", "ocean-servers")
    assert server.step_meter_file == "custom_step_meter_file"
    assert server.dump()["on-dispatch-error"] == {"type": "propagate", "maxRetries": 1}
    assert Server.from_yaml(server.dump_yaml()) == server


def test_mpi_server_group_defaults_to_multio():
    server = Server(transport="mpi")
    assert server.group == "multio"
    assert server.dump()["group"] == "multio"


def test_tcp_and_thread_server_keys():
    tcp = Server(**TCP_SERVER)
    assert tcp.local_port == 7771 and tcp.servers[0].ports == [7771, 7772] and tcp.clients[0].host == "localhost"
    dumped = tcp.dump()
    assert dumped["local_port"] == 7771 and "group" not in dumped  # the default group belongs to mpi only
    assert Server(**dumped) == tcp
    thread = Server(**THREAD_SERVER)
    assert thread.client_count == 4 and thread.dump()["clientCount"] == 4
    assert Server(**thread.dump()) == thread


def test_server_clients_are_optional_for_tcp():
    Server(transport="tcp", local_port=1, servers=[{"host": "h", "ports": [1]}])


@pytest.mark.parametrize(
    "server",
    [
        {},  # C++ requires `transport`
        {"transport": "none"},  # not a registered transport
        {"transport": "MPI"},
        {"transport": "tcp"},
        {"transport": "tcp", "local_port": 1},
        {"transport": "tcp", "servers": [{"host": "h", "ports": [1]}]},
        {"transport": "tcp", "local_port": 1, "servers": [{"host": "h", "ports": 1}]},  # ports must be a list
        {"transport": "tcp", "local_port": 1, "servers": [{"host": "h"}]},
        {"transport": "tcp", "local_port": -1, "servers": [{"host": "h", "ports": [1]}]},
        {"transport": "thread"},
        {"transport": "thread", "clientCount": -1},
        {"transport": "mpi", "local_port": 1},
        {"transport": "mpi", "clientCount": 1},
        {"transport": "mpi", "group": "x", "servers": [{"host": "h", "ports": [1]}]},
        {"transport": "thread", "clientCount": 1, "group": "x"},
        {"transport": "thread", "clientCount": 1, "step-meter-file": "x"},
        {**TCP_SERVER, "client-group": "x"},
        {"transport": "mpi", "count": 2},
        {"transport": "mpi", "on-receive-error": "recover"},  # only `propagate` for the listener
        {"transport": "mpi", "on-dispatch-error": "abort-transport"},
        {"transport": "mpi", "on-error": "ignore"},
    ],
)
def test_server_rejects_invalid_transport_config(server):
    with pytest.raises(ValidationError):
        Server(**server)


def test_server_count_is_removed():
    assert "count" not in Server.model_fields
    with pytest.raises(ValidationError):
        Server(transport="mpi", count=1)


def test_server_on_error_tags():
    for tag in ("propagate", "recover", "abort-transport", "Abort-Transport"):
        Server(transport="mpi", on_error=tag)


def test_mpi_communicators():
    config = Client(
        **{
            "mpi-communicators": {
                "multio": {"type": "passed", "default": "world"},
                "multio-clients": {"type": "split", "color": 777, "parent": "multio"},
                "multio-servers": {"color": 888},
                "multio-others": {},
            }
        }
    )
    assert config.mpi_communicators["multio-clients"].color == 777
    assert config.dump()["mpi-communicators"]["multio-servers"] == {"color": 888}
    server = Server(transport="mpi", mpi_communicators={"multio": {"type": "passed"}})
    assert server.mpi_communicators["multio"].type == "passed"


@pytest.mark.parametrize(
    "communicator",
    [
        {"type": "other"},
        {"type": "passed", "color": 1},
        {"type": "passed", "parent": "multio"},
        {"type": "split", "default": "world"},
        {"color": 1.5},  # `getInt` throws for a float that is not whole
        {"unknown": 1},
    ],
)
def test_mpi_communicators_reject_invalid(communicator):
    with pytest.raises(ValidationError):
        Client(mpi_communicators={"multio": communicator})


def test_client_and_server_are_discriminated_in_a_collection():
    collection = Collection(
        **{
            "client": {"plans": [{"name": "p", "actions": [print_action]}]},
            "server": {"transport": "thread", "clientCount": 1},
        }
    )
    assert isinstance(collection.configs["client"], Client)
    assert isinstance(collection.configs["server"], Server)
    collection.add_config("other", Server(transport="mpi"))  # a model instance is discriminated by its class
    collection.add_config("another", Client())
    assert isinstance(collection.configs["other"], Server)
    assert collection.dump()["server"]["clientCount"] == 1


def test_server_config_in_multio_plan_context():
    plan = multio.MultioPlan({"transport": "thread", "clientCount": 2, "plans": []})
    assert isinstance(plan._config, Server)
    assert plan._config.client_count == 2


# ----------------------------------------------------------------------------------------------------------------------
# Defaults are not marked as set


@pytest.mark.parametrize("sink", SINK_CONFIGS, ids=lambda sink: sink["type"])
def test_sink_defaults_are_not_marked_as_set(sink):
    built = _sink_plan(sink).actions[0].sinks[0]
    assert built.model_dump(exclude_unset=True, exclude={"sinks"}) == {k: v for k, v in sink.items() if k != "sinks"}
    assert built.model_fields_set == set(sink)


def test_plan_and_config_defaults_are_not_marked_as_set():
    plan = Plan(actions=[{"type": "print"}])
    assert plan.model_dump(exclude_unset=True) == {"actions": [{"type": "print"}]}
    assert Plan(name="p", actions=[{"type": "print"}], enable=False).model_dump(exclude_unset=True) == {
        "name": "p",
        "enable": False,
        "actions": [{"type": "print"}],
    }
    assert Client(plans=[plan]).model_dump(exclude_unset=True) == {"plans": [{"actions": [{"type": "print"}]}]}
    assert Server(transport="thread", client_count=1, plans=[plan]).model_dump(exclude_unset=True) == {
        "plans": [{"actions": [{"type": "print"}]}],
        "transport": "thread",
        "clientCount": 1,
    }


# The public parsing (`from_yaml`, `from_yamlfile` and `from_dict`) is strict, and converts what C++ converts

PLAN_WITH_CONVERTED_VALUES = """
plans:
  - name: nextgems
    actions:
      - type: statistics
        operations: [average]
        output-frequency: 1d
        options:
          restart: 'true'          # never read by C++
          use-current-time: true   # getLong of a boolean
          write-restart: 'false'   # parseBool of what `{ENV}` leaves: a string
          read-restart: 'false'
          value-count-threshold: '10'
      - type: sink
        failOnError: 'no'          # getBool of a string
        sinks:
          - type: file
            path: out.grib
            append: 1              # getBool of a number
            enable: '0'
"""


def test_from_yaml_converts_the_values_that_cpp_converts():
    client = Client.from_yaml(PLAN_WITH_CONVERTED_VALUES)
    statistics, sink = client.plans[0].actions
    assert statistics.options.use_current_time == 1
    assert statistics.options.write_restart is False and statistics.options.read_restart is False
    assert statistics.options.value_count_threshold == 10
    assert statistics.options.restart == "true"
    assert sink.fail_on_error is False
    assert sink.sinks[0].append is True and sink.sinks[0].enable == "0"
    assert Client.from_dict(yaml.safe_load(PLAN_WITH_CONVERTED_VALUES)) == client


def test_from_yamlfile_uses_the_same_parsing(tmp_path):
    path = tmp_path / "plans.yaml"
    path.write_text(PLAN_WITH_CONVERTED_VALUES)
    assert Client.from_yamlfile(str(path)) == Client.from_yaml(PLAN_WITH_CONVERTED_VALUES)


@pytest.mark.parametrize(
    "replacement",
    [
        ("write-restart: 'false'", "write-restart: 0"),  # parseBool does not read a number
        ("use-current-time: true", "use-current-time: 1.5"),  # a float that is not whole is no integer for getLong
        ("append: 1", "append: perhaps"),  # eckit throws for it
        ("failOnError: 'no'", "failOnError: 1.0"),  # a float does not convert to a boolean
    ],
)
def test_from_yaml_rejects_the_values_that_cpp_rejects(replacement):
    old, new = replacement
    assert old in PLAN_WITH_CONVERTED_VALUES
    with pytest.raises(ValidationError):
        Client.from_yaml(PLAN_WITH_CONVERTED_VALUES.replace(old, new))


def test_from_yaml_does_not_use_the_lax_mode_of_pydantic():
    # lax pydantic would read a float with a fraction as an integer (truncated), `getLong` throws for it
    for bad in ("port: 1.5", "port: [1]"):
        yaml_text = f"""
plans:
  - name: p
    actions:
      - type: sink
        triggers: [{{type: NotifyMetadata, key: step, {bad}}}]
        sinks: []
"""
        with pytest.raises(ValidationError):
            Client.from_yaml(yaml_text)


def test_on_error_map_form_accepts_any_type_like_cpp():
    # FailureAware: `type` of the map form that is no tag leaves the default, only the string form throws
    assert Client(on_error={"type": "whatever"}).dump()["on-error"] == {"type": "whatever"}
    assert Server(transport="thread", client_count=1, on_receive_error={"type": "x"}).dump()["on-receive-error"] == {
        "type": "x"
    }
    for bad in ("whatever", "abort-transport-now"):
        with pytest.raises(ValidationError):
            Client(on_error=bad)
