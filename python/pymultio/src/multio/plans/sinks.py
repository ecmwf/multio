# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""
Multio Sinks.
"""

from __future__ import annotations

from typing import Literal, Union

from pydantic import Field, FilePath, StrictBool, StrictStr, field_validator
from typing_extensions import Annotated

from .base import ConfigBool, ConfigInt, ConfigStr, MultioBaseModel

# `enable` of a plan or sink: a boolean, or a string that C++ converts with eckit's Translator<string, bool>
# ("yes", "on", "true" and "1" are true, "no", "off", "false" and "0" false, any other string is read as an integer
# and is false when it is not a number). Any other type is an error in C++.
#
# The value is kept as written and converted where it is used (`base.parsed_bool`), unlike the other `parseBool`
# fields (`ParsedBool`, converted when validated): a string is often the substitution of a `{PLACEHOLDER}` that C++
# reads when it builds the plan, and a dump must give that string back, not the boolean that it converts to.
Enable = Union[StrictBool, StrictStr]

_MULTIO_ONLY_IGNORE = {
    "sinks": "key of the `multio` sink, read by the parent MultIO in the same file",
    "triggers": "key of the `multio` sink, read by the parent MultIO in the same file",
}


def open_yaml(v: str | dict) -> dict:
    """Open a YAML file or return the dictionary"""
    if not isinstance(v, dict):
        import yaml

        return yaml.safe_load(open(v, "r"))
    return v


class Sinks(MultioBaseModel):
    """Base Sinks class

    This class should not be instantiated directly.
    Use one of the subclasses instead.

    `failOnError` is read by every sink (`DataSink`), `enable` by the parent `MultIO` that builds the sinks.
    """

    cpp_merge_parents = True
    cpp_sources = ("src/multio/sink/DataSink.cc", "src/multio/sink/MultIO.cc")
    cpp_ignore = _MULTIO_ONLY_IGNORE

    type: str
    enable: Enable = Field(True, description="A disabled sink is not built")
    fail_on_error: ConfigBool = Field(True, alias="failOnError", description="Fail when writing to the sink fails")


class Trigger(MultioBaseModel):
    """Base Trigger class.

    Triggers are not sinks: they are listed under the `triggers` key of the `sink` action
    and of the `multio` sink. Use one of the subclasses instead.

    `failOnRetry` is spelled in camel case by C++ and is read as an integer (0 or 1).
    """

    cpp_sources = ("src/multio/sink/Trigger.cc",)

    type: str
    file: ConfigStr | None = Field(None, description="File to append the trigger events to")
    host: ConfigStr | None = Field(None, description="Host to send the trigger events to")
    port: ConfigInt = Field(10000)
    retries: ConfigInt = Field(5)
    timeout: ConfigInt = Field(60)
    fail_on_retry: ConfigInt = Field(0, alias="failOnRetry")
    info: dict[str, ConfigStr] | None = Field(
        None, description="Extra information, every value is read as a string (`1` is `'1'`)"
    )


class MetadataChange(Trigger):
    """MetadataChange Trigger"""

    type: Literal["MetadataChange"] = Field("MetadataChange", init=False)
    key: ConfigStr
    values: list[ConfigStr]


class NotifyMetadata(Trigger):
    """NotifyMetadata Trigger"""

    type: Literal["NotifyMetadata"] = Field("NotifyMetadata", init=False)
    key: ConfigStr


TriggersType = Annotated[Union[MetadataChange, NotifyMetadata], Field(discriminator="type")]


class FDB(Sinks):
    """FDB Sink

    `config` is the FDB configuration, passed verbatim to FDB. C++ uses an empty map when it is absent. A path to a
    YAML file is also accepted here and is replaced by the content of the file.
    """

    cpp_sources = ("src/multio/fdb5/FDB5Sink.cc",)
    cpp_ignore = {"userConfig": "key inside the `config` map, which is passed verbatim to FDB (a dict here)"}

    type: Literal["fdb5"] = Field("fdb5", init=False)
    config: Union[FilePath, dict] = Field(
        default_factory=dict, title="Config", description="FDB configuration, or the path to its YAML file"
    )

    @field_validator("config")
    @classmethod
    def __load_yaml(cls, v):
        return open_yaml(v)


class File(Sinks):
    """File Sink"""

    cpp_sources = ("src/multio/sink/FileSink.cc",)

    type: Literal["file"] = Field("file", init=False)
    append: ConfigBool = Field(False)
    per_server: ConfigBool = Field(False)
    path: ConfigStr


class Maestro(Sinks):
    """Maestro Sink"""

    cpp_sources = ("src/multio/maestro/MaestroSink.cc",)

    type: Literal["maestro"] = Field("maestro", init=False)
    ready_cdo: ConfigBool = Field(True)
    schema_: ConfigStr = Field(".maestro.ecmwf.", alias="schema")


class MultIO(Sinks):
    """MultIO Sink.

    Contains a list of sinks to write to, can be nested.
    """

    cpp_sources = ("src/multio/sink/Trigger.cc",)
    cpp_ignore = dict.fromkeys(_MULTIO_ONLY_IGNORE)  # it reads `sinks` and `triggers` itself: not ignored here

    type: Literal["multio"] = Field("multio", init=False)
    sinks: list[SinksType] = Field(default_factory=list)
    triggers: list[TriggersType] | None = Field(None)


SINKS = Union[FDB, File, Maestro, MultIO]
SinksType = Annotated[SINKS, Field(discriminator="type", title="Sinks")]

MultIO.model_rebuild()

__all__ = [
    "Sinks",
    "FDB",
    "File",
    "Maestro",
    "MultIO",
    "Trigger",
    "MetadataChange",
    "NotifyMetadata",
    "TriggersType",
    "SINKS",
    "SinksType",
]
