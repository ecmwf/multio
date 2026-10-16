# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""
Multio Plans

Holds a collection of actions for use with Multio.
"""

from __future__ import annotations

import os
from typing import Any, Literal, TypeVar, Union

from pydantic import (
    BaseModel,
    BeforeValidator,
    Discriminator,
    Field,
    NonNegativeInt,
    Tag,
    model_serializer,
    model_validator,
    validate_call,
)
from typing_extensions import Annotated

from .actions import (
    ACTIONS,
    DebugSink,
    Null,
    OnErrorOptions,
    OnErrorTag,
    SingleField,
    Sink,
    Transport,
    _lower,
)
from .base import CONFIG_STR, ConfigBool, ConfigInt, ConfigStr, translate_bool
from .base import MultioBaseModel as _ConfigModel
from .sinks import Enable

Name = ConfigStr  # C++ reads it with getString
Actions = Annotated[ACTIONS, Field(discriminator="type", title="Actions")]

T = TypeVar("T", bound="MultioBaseModel")

# Actions that end a chain of actions in C++: they derive from Action, not from ChainedAction, so they need no `next`
TERMINAL_ACTIONS = (Transport, Sink, SingleField, Null, DebugSink)


class MultioBaseModel(_ConfigModel):
    """Multio Base Model

    Adds reading and writing to the common model of the plan models (`base.MultioBaseModel`).
    Should not be instantiated directly, use one of the subclasses instead.
    """

    @classmethod
    def from_yamlfile(cls: T, file: str) -> T:
        """
        Create a model from a YAML file

        Parameters
        ----------
        file : str
            File to read from

        Returns
        -------
        MultioBaseModel
            Loaded BaseModel
        """
        return cls.from_yaml(open(file))

    @classmethod
    def from_yaml(cls: T, yaml_str: str) -> T:
        """
        Create a model from a YAML string

        Parameters
        ----------
        yaml_str : str
            Yaml string to load

        Returns
        -------
        MultioBaseModel
            Loaded BaseModel
        """
        import yaml

        return cls.from_dict(yaml.safe_load(yaml_str))

    @classmethod
    def from_dict(cls: T, data: Any) -> T:
        """
        Create a model from the content of a parsed YAML or JSON file

        This is the parsing that `from_yaml` and `from_yamlfile` use. It is strict: the models convert the values
        that C++ converts (`on`/`yes`/`true` strings for booleans that C++ reads with `parseBool`, a boolean for an
        integer read with `getLong`, ...) and reject every other type, see `multio.plans.base`.

        Parameters
        ----------
        data : Any
            Dictionary (or list) as returned by `yaml.safe_load`

        Returns
        -------
        MultioBaseModel
            Loaded BaseModel
        """
        return cls.model_validate(data, strict=True)

    @validate_call
    def write(self, file: os.PathLike, *, format: Literal["yaml", "json"] = "yaml") -> None:
        """
        Write the model to a file

        Parameters
        ----------
        file : os.PathLike
            Path to write to
        format : Literal['yaml', 'json']
            Package to use for dumping, defaults to 'yaml'.
        """
        if format == "yaml":
            import yaml

            method = yaml.safe_dump
        elif format == "json":
            import json

            method = json.dump

        with open(file, "w") as f:
            method(self.dump(), f)

    def dump(self) -> dict:
        """
        Dump the model to a python dict
        """
        return self.model_dump(serialize_as_any=True, by_alias=True, exclude_none=True)

    def dump_json(self) -> str:
        """
        Dump the model to a JSON string
        """
        import json

        return json.dumps(self.dump())

    def dump_yaml(self) -> str:
        """
        Dump the model to a yaml string
        """
        import yaml

        return yaml.safe_dump(self.dump(), sort_keys=False)


def is_enabled(value: bool | str) -> bool:
    """Whether C++ builds a plan or sink with this `enable` value (eckit `Translator<string, bool>` for strings)."""
    if isinstance(value, bool):
        return value
    return translate_bool(value)


# Keys that PlanConfiguration.cc reads for the plans that a client or server holds, and that no field of the client or
# server is: the include of a plan file, and the keywords of the requests of a dissemination plan
_PLAN_FILE_IGNORE = {
    "file": "includes the plan from another file, one level deep: the loader replaces the entry with the content "
    "of the file, so the model describes the included plan"
}
_REQUEST_KEYWORD_IGNORE = {
    keyword: "keyword of a request of the MARS file, read from the request (a metkit object), not from the config"
    for keyword in ("param", "levtype", "levelist", "grid", "area", "interpolation", "target")
}


class Plan(MultioBaseModel):
    """
    Multio Plan

    A plan is a chain of actions: C++ requires at least one, and the last one that is built must be terminal
    (`sink`, `transport`, `single-field-sink`, `null` or `debug-sink`), see `check_validity`.
    A plan without `name` is called `anonymous` by C++, and a client rejects two plans with the same name.

    Examples:
    ```python
        plan = Plan(name="Plan1", actions=[{"type": "print"}])
        plan.add_action({"type": "select", "match": {"key": "value"}})
        plan.add_action({"type": "sink", "sinks": [{"type": "file", "path": "output.txt"}]})
    ```

    """

    cpp_sources = (
        "src/multio/config/PlanConfiguration.cc",
        "src/multio/action/Plan.cc",
        "src/multio/action/Plan.h",
        "src/multio/util/FailureHandling.h",
    )
    cpp_ignore = {
        "on-error": "the key name comes from FailureAware::configKey() in action/Plan.h, not from a literal accessor",
        **_PLAN_FILE_IGNORE,
        "type": "the type of the first action, which Plan.cc reads to build the root of the chain of actions",
    }

    name: Name | None = Field(None, title="Name", description="Name of the plan, `anonymous` when absent")
    enable: Enable = Field(True, description="A disabled plan is dropped")
    on_error: OnErrorTag | OnErrorOptions | None = Field(
        None, description="Failure handling: `propagate` (default) or `recover`, optionally with `maxRetries`"
    )
    actions: list[Actions] = Field(
        default_factory=list,
        title="Actions",
        description="List of actions to be performed. C++ requires at least one: a plan may be built empty and "
        "filled with `add_action`, but it is rejected when it is serialised or passed to C++ (`validate_for_cpp`)",
    )

    def validate_for_cpp(self) -> Plan:
        """Check what C++ rejects when it builds the plan: an enabled plan needs at least one action.

        Called when the plan is dumped (`dump`, `model_dump`), converted to a config and used by `MultioPlan`.
        A disabled plan is dropped by C++ before its actions are looked at, so it passes. A dissemination plan is
        a `DisseminationPlan`, which has no `actions`.

        Raises
        ------
        ValueError
            If the plan is enabled and has no actions

        Returns
        -------
        Plan
            This plan
        """
        if not self.actions and is_enabled(self.enable):
            raise ValueError(
                f"plan {self.name or 'anonymous'!r} has no actions: C++ rejects a plan without actions "
                "(only a dissemination plan, see `DisseminationPlan`, has none)"
            )
        return self

    @model_serializer(mode="wrap")
    def _validate_for_cpp_on_dump(self, handler):
        self.validate_for_cpp()
        return handler(self)

    @validate_call
    def add_action(self, action: Actions):
        """
        Add action to `Plan`

        Parameters
        ----------
        action : Actions
            Action class, can be object or dictionary representing action
        """
        self.actions.append(action)

    @validate_call
    def extend_actions(self, actions: list[Actions]):
        """
        Extend the actions of the plan

        Parameters
        ----------
        actions : list[Actions]
            List of actions to add
        """
        self.actions.extend(actions)

    def check_validity(self) -> bool:
        """Check if the plan is valid: C++ fails to build a plan where no action is terminal.

        C++ builds the chain up to the first terminal action and ignores the actions after it.
        """
        return any(isinstance(action, TERMINAL_ACTIONS) for action in self.actions)

    def ensure_sink(self) -> "Plan":
        """Ensure that the plan has at least one Sink.

        Multio requires a `Sink` in each plan.
        """
        if not any([isinstance(action, Sink) for action in self.actions]):
            self.add_action(Sink())
        return self

    def to_client(self) -> Client:
        """
        Convert `Plan` to a `Client` Config

        Returns
        -------
        Client
            Client Config of this plan

        Raises
        ------
        ValueError
            If the plan has no actions, see `validate_for_cpp`
        """
        return Client(plans=[self.validate_for_cpp()])

    def to_server(self, **config) -> Server:
        """
        Convert `Plan` to a `Server` Config

        Parameters
        ----------
        **config
            Other keys of the `Server`: C++ requires a `transport`.

        Returns
        -------
        Server
            Server Config of this plan

        Raises
        ------
        ValueError
            If the plan has no actions, see `validate_for_cpp`
        """
        return Server(plans=[self.validate_for_cpp()], **config)

    def __add__(self, other) -> Client:
        if not isinstance(other, Plan):
            return NotImplemented
        return Client(plans=[self, other])


class DisseminationPlan(MultioBaseModel):
    """
    Multio dissemination plan: C++ generates one plan per request of a MARS request file.

    Each plan (named `<name>_<NNNN>`) selects, interpolates, encodes and writes the fields of its request.
    C++ does not read `enable`, `on-error` or `actions` for these plans: with `actions`, the plan is an ordinary
    `Plan` and `dissemination` is not read.

    Examples:
    ```python
        plan = DisseminationPlan(
            name="across", input="O80", templates=[{"sfc": "sfc.tmpl"}], dissemination="requests.diss"
        )
    ```
    """

    cpp_sources = ("src/multio/config/PlanConfiguration.cc",)
    cpp_ignore = {**_REQUEST_KEYWORD_IGNORE}

    name: Name = Field(title="Name", description="Name of the plan, the generated plans are `<name>_<NNNN>`")
    dissemination: ConfigStr = Field(description="Path of the file with the MARS requests")
    input: ConfigStr = Field(description="Input grid of the interpolation, for example `O1280`")
    templates: list[dict[str, ConfigStr]] = Field(
        description="Template file per level type, for example `{sfc: sfc.tmpl}`"
    )
    templates_path: ConfigStr = Field(".", description="Directory of the templates, which must exist")
    output_path: ConfigStr = Field(".", description="Directory of the output, a subdirectory per plan is created")
    per_server: ConfigBool = Field(True, description="One output file per server")
    output_name: ConfigStr | None = Field(None, description="Name of the output file, without the `.grib` extension")
    debug: ConfigInt = Field(0, description="Above 0 prints the fields before the interpolation, above 1 also after it")

    def check_validity(self) -> bool:
        """C++ generates a valid plan, ending with a sink, from each request"""
        return True

    def validate_for_cpp(self) -> DisseminationPlan:
        """Nothing to check: C++ generates the actions"""
        return self


def discriminate_plan(v: Any) -> Literal["plan", "dissemination"]:
    """
    Discriminate the plans: C++ makes a dissemination plan only when `actions` is absent and `dissemination` present
    """
    if isinstance(v, DisseminationPlan):
        return "dissemination"
    if isinstance(v, dict) and "actions" not in v and "dissemination" in v:
        return "dissemination"
    return "plan"


PlanType = Annotated[
    Union[Annotated[Plan, Tag("plan")], Annotated[DisseminationPlan, Tag("dissemination")]],
    Discriminator(discriminate_plan),
]


def discriminate_config(v: Any) -> Literal["Client", "Server"]:
    """
    Discriminate the config based on the presence of the transport key
    """
    if isinstance(v, BaseModel):
        return "Server" if isinstance(v, Server) else "Client"
    if "transport" in v:
        return "Server"
    return "Client"


# `on-error` of the client and the server accept the tags below (OnClientError, OnServerError)
FailureTag = Annotated[Literal["propagate", "recover", "abort-transport"], BeforeValidator(_lower)]
# `on-receive-error` and `on-dispatch-error` only accept `propagate`
PropagateTag = Annotated[Literal["propagate"], BeforeValidator(_lower)]


class OnServerErrorOptions(MultioBaseModel):
    """Map form of `on-error` of the client and the server: a tag with options (util/FailureHandling.h)."""

    cpp_sources = ("src/multio/util/FailureHandling.h",)
    cpp_open_enums = frozenset({"type"})

    type: (
        FailureTag | ConfigStr
    )  # C++ falls back to `propagate` for an unknown tag of the map form, see `OnErrorOptions`
    max_retries: ConfigInt | None = Field(None, alias="maxRetries")


class PropagateOnErrorOptions(MultioBaseModel):
    """Map form of `on-receive-error` and `on-dispatch-error` (util/FailureHandling.h)."""

    cpp_sources = ("src/multio/util/FailureHandling.h",)
    cpp_open_enums = frozenset({"type"})

    type: (
        PropagateTag | ConfigStr
    )  # C++ falls back to `propagate` for an unknown tag of the map form, see `OnErrorOptions`
    max_retries: ConfigInt | None = Field(None, alias="maxRetries")


class MpiCommunicator(MultioBaseModel):
    """A communicator of the MPI transport, under the `mpi-communicators` key (transport/MpiCommSetup.cc).

    `passed` takes the communicator named by `default` (`world` when absent), `split` splits `parent` by `color`.
    Without a `type`, C++ uses `passed` for the group and `split` for the client and server groups.
    """

    cpp_sources = ("src/multio/transport/MpiCommSetup.cc",)

    type: Annotated[Literal["passed", "split"], CONFIG_STR] | None = Field(None, description="`passed` or `split`")
    default: ConfigStr | None = Field(None, description="`passed`: name of the communicator that is passed")
    color: ConfigInt | None = Field(None, description="`split`: colour of the split, a default colour when absent")
    parent: ConfigStr | None = Field(None, description="`split`: name of the communicator to split, `group` by default")

    @model_validator(mode="after")
    def check_keys_of_type(self) -> MpiCommunicator:
        if self.type == "passed" and (self.color is not None or self.parent is not None):
            raise ValueError("`color` and `parent` are keys of a `split` communicator")
        if self.type == "split" and self.default is not None:
            raise ValueError("`default` is a key of a `passed` communicator")
        return self


class TcpPeer(MultioBaseModel):
    """Host and ports of the servers or clients of the TCP transport (transport/TcpTransport.cc)."""

    cpp_sources = ("src/multio/transport/TcpTransport.cc",)

    host: ConfigStr
    ports: list[NonNegativeInt] = Field(description="A list, also for a single port")


class BaseConfig(MultioBaseModel):
    """
    Multio BaseConfig.

    Examples:
    ```python
        config = Client()
        plan = Plan(name="Plan1", actions=[{"type": "print"}])
        plan.add_action({"type": "select", "match": {"key": "value"}})
        plan.add_action({"type": "sink", "sinks": [{"type": "file", "path": "output.txt", "append": False}]})
        config.add_plan(plan)
    ```
    """

    plans: list[PlanType] = Field(title="Plans", description="List of plans to be executed", default_factory=list)
    mpi_communicators: dict[str, MpiCommunicator] | None = Field(
        None, description="MPI communicators by name, read from the top level of the whole configuration"
    )

    @validate_call
    def add_plan(self, plan: PlanType):
        """
        Add plan to the list of plans

        Parameters
        ----------
        plan : Plan | DisseminationPlan
            Plan to add, can be object or dictionary representing plan
        """
        self.plans.append(plan)

    @validate_call
    def extend_plans(self, plans: list[PlanType]):
        self.plans.extend(plans)

    def validate_for_cpp(self) -> BaseConfig:
        """Check what C++ rejects when it builds the config: so far a plan without actions, see `Plan.validate_for_cpp`

        Raises
        ------
        ValueError
            If a plan is not valid for C++

        Returns
        -------
        BaseConfig
            This config
        """
        for plan in self.plans:
            plan.validate_for_cpp()
        return self


class Client(BaseConfig):
    """Client Specific Config

    This is the `client` block of a configuration file, or the whole file when it only has `plans`.
    C++ requires the `plans`, and reads `active-matchers` and `mpi-communicators` from the top level of the file.
    A client rejects two enabled plans with the same name, an unnamed plan is called `anonymous`.
    """

    cpp_sources = (
        "src/multio/server/MultioClient.cc",
        "src/multio/server/MultioClient.h",
        "src/multio/util/FailureHandling.h",
        "src/multio/config/PlanConfiguration.cc",
        "src/multio/transport/MpiCommSetup.cc",
    )
    cpp_ignore = {
        **_PLAN_FILE_IGNORE,
        **_REQUEST_KEYWORD_IGNORE,
        "client": "name of the block that holds the client config, not a key of it",
        "on-error": "the key name comes from FailureAware::configKey() in server/MultioClient.h, not from a literal "
        "accessor",
    }

    on_error: FailureTag | OnServerErrorOptions | None = Field(
        None,
        description="Failure handling: `propagate` (default), `recover` or `abort-transport`, "
        "optionally with `maxRetries`",
    )
    active_matchers: list[dict[str, list[ConfigStr]]] | None = Field(
        None,
        description="Metadata keys with their allowed values: C++ reads them and does not use them (dead code)",
    )

    @model_validator(mode="after")
    def check_plan_names_unique(self) -> Client:
        names = [
            (plan.name or "anonymous") for plan in self.plans if isinstance(plan, Plan) and is_enabled(plan.enable)
        ]
        duplicated = sorted({name for name in names if names.count(name) > 1})
        if duplicated:
            raise ValueError(f"plan names must be unique, C++ rejects the plans named {duplicated}")
        return self

    def to_server(self, **config) -> Server:
        """
        Convert `Client` to a `Server` Config

        Parameters
        ----------
        **config
            Other keys of the `Server`: C++ requires a `transport`.

        Returns
        -------
        Server
            Server Config of these plans

        Raises
        ------
        ValueError
            If a plan has no actions, see `Plan.validate_for_cpp`
        """
        return Server(plans=self.validate_for_cpp().plans, **config)


# Fields of the `Server` per transport, as they are all read from the map that holds `transport`
TRANSPORT_KEYS = {
    "mpi": ("group", "client_group", "server_group", "ecflow_step_meter_name", "step_meter_file"),
    "tcp": ("local_port", "servers", "clients"),
    "thread": ("client_count",),
}
# C++ throws when these are absent: `clients` is only read when the peers are created
REQUIRED_TRANSPORT_KEYS = {"mpi": (), "tcp": ("local_port", "servers"), "thread": ("client_count",)}


class Server(BaseConfig):
    """
    Server Config.

    This is the `server` block of a configuration file. C++ requires the `transport` and the `plans`.

    The keys of the transport sit in the same map as `transport`, so a `Server` has the keys of every transport
    and checks that those given belong to the transport and that the required ones are there. This is a single
    class, not one class per transport, because `Server(**config)` has to accept any of them, as `MultioPlan` does.

    - `mpi`: `group`, `client-group`, `server-group`, `ecflow-step-meter-name`, `step-meter-file` and the top-level
      `mpi-communicators`.
    - `tcp`: `local_port`, `servers` and `clients`.
    - `thread`: `clientCount`.
    """

    cpp_sources = (
        "src/multio/server/MultioServer.cc",
        "src/multio/server/MultioServer.h",
        "src/multio/server/Dispatcher.cc",
        "src/multio/server/Dispatcher.h",
        "src/multio/server/Listener.h",
        "src/multio/util/FailureHandling.h",
        "src/multio/config/PlanConfiguration.cc",
        "src/multio/transport/MpiTransport.cc",
        "src/multio/transport/MpiCommSetup.cc",
        "src/multio/transport/TcpTransport.cc",
        "src/multio/transport/ThreadTransport.cc",
    )
    cpp_ignore = {
        **_PLAN_FILE_IGNORE,
        **_REQUEST_KEYWORD_IGNORE,
        "server": "name of the block that holds the server config, not a key of it",
        "transport": "the allowed values are the names of the transport registrations, "
        "checked by test_transport_names_match_cpp",
        "on-error": "the key name comes from FailureAware::configKey() in server/MultioServer.h, not from a literal "
        "accessor",
        "on-receive-error": "the key name comes from FailureAware::configKey() in server/Listener.h, not from a literal "
        "accessor",
        "on-dispatch-error": "the key name comes from FailureAware::configKey() in server/Dispatcher.h, not from a "
        "literal accessor",
    }

    transport: Literal["mpi", "tcp", "thread"] = Field(title="Transport", description="Transport protocol to use")
    on_error: FailureTag | OnServerErrorOptions | None = Field(
        None,
        description="Failure handling: `propagate` (default), `recover` or `abort-transport`, "
        "optionally with `maxRetries`",
    )
    on_receive_error: PropagateTag | PropagateOnErrorOptions | None = Field(
        None, description="Failure handling of the listener: only `propagate`"
    )
    on_dispatch_error: PropagateTag | PropagateOnErrorOptions | None = Field(
        None, description="Failure handling of the dispatcher: only `propagate`"
    )

    # mpi
    group: ConfigStr = Field("multio", description="`mpi`: name of the MPI group communicator")
    client_group: ConfigStr | None = Field(
        None, description="`mpi`: name of the client communicator, `<group>-clients`"
    )
    server_group: ConfigStr | None = Field(
        None, description="`mpi`: name of the server communicator, `<group>-servers`"
    )
    ecflow_step_meter_name: ConfigStr | None = Field(None, description="`mpi`: ecFlow meter of the step, `multio_step`")
    step_meter_file: ConfigStr | None = Field(None, description="`mpi`: file of the step meter, `../multio_step_meter`")

    # tcp
    local_port: Annotated[ConfigInt, Field(ge=0)] | None = Field(
        None, alias="local_port", description="`tcp`: port of this process"
    )
    servers: list[TcpPeer] | None = Field(None, description="`tcp`: hosts and ports of the servers")
    clients: list[TcpPeer] | None = Field(
        None, description="`tcp`: hosts and ports of the clients, read when the peers are created"
    )

    # thread
    client_count: Annotated[ConfigInt, Field(ge=0)] | None = Field(
        None, alias="clientCount", description="`thread`: number of clients"
    )

    @model_validator(mode="after")
    def check_transport_keys(self) -> Server:
        for transport, names in TRANSPORT_KEYS.items():
            if transport == self.transport:
                continue
            given = [name for name in names if getattr(self, name) not in (None, type(self).model_fields[name].default)]
            if given:
                raise ValueError(f"{given} are keys of the {transport} transport, not of {self.transport}")
        missing = [name for name in REQUIRED_TRANSPORT_KEYS[self.transport] if getattr(self, name) is None]
        if missing:
            raise ValueError(f"the {self.transport} transport requires {missing}")
        return self

    def dump(self) -> dict:
        data = super().dump()
        if self.transport != "mpi" and self.group == type(self).model_fields["group"].default:
            data.pop("group", None)  # the default group is only meaningful for mpi
        return data


CONFIGS = Annotated[
    Union[Annotated[Client, Tag("Client")], Annotated[Server, Tag("Server")]], Discriminator(discriminate_config)
]


class Collection(MultioBaseModel):
    """
    Multio Collection of Configs

    Can be made of `Client` and `Server` Configs.
    """

    configs: dict[str, CONFIGS] = Field(default_factory=dict, title="Configs", description="Collection of Configs")

    @validate_call
    def add_config(self, key: str, config: CONFIGS):
        """
        Add config to the collection

        Parameters
        ----------
        key : str
            Name of the config
        config : CONFIGS
            Config to add
        """
        self.configs[key] = config

    @validate_call
    def __get__(self, key: str) -> CONFIGS:
        return self.configs[key]

    @validate_call
    def __set__(self, key: str, config: CONFIGS):
        self.configs[key] = config

    def validate_for_cpp(self) -> Collection:
        """Check what C++ rejects in each config, see `BaseConfig.validate_for_cpp`"""
        for config in self.configs.values():
            config.validate_for_cpp()
        return self

    @model_validator(mode="before")
    @classmethod
    def __convert_to_pydantic(cls, data: Any) -> dict:
        return {"configs": data}

    @model_serializer
    def __convert_to_multio(self) -> dict[str, Any]:
        return {k: v.dump() for k, v in self.configs.items()}


__all__ = [
    "Client",
    "Server",
    "Plan",
    "DisseminationPlan",
    "PlanType",
    "Collection",
    "MpiCommunicator",
    "TcpPeer",
    "OnServerErrorOptions",
    "PropagateOnErrorOptions",
]
