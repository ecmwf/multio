# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""
Multio Sinks.
"""
from typing import Literal, Union

from pydantic import BaseModel, Field, FilePath, field_validator


def open_yaml(v: str | dict) -> dict:
    """Open a YAML file or return the dictionary"""
    if not isinstance(v, dict):
        import yaml

        return yaml.safe_load(open(v, "r"))
    return v


class Sinks(BaseModel):
    """Base Sinks class

    This class should not be instantiated directly.
    Use one of the subclasses instead.
    """

    type: str


class Debug(Sinks):
    """Debug Sink"""

    type: Literal["debug-sink"] = Field("debug-sink", init=False)


class Trigger(Sinks):
    """Trigger Sink"""

    type: Literal["trigger"] = Field("trigger", init=False)
    file: str = Field(title="File", description="Path to trigger file")
    key: str
    host: str
    failOnRetry: bool = False
    port: int = 10000
    retries: int = 5
    timeout: int = 60


class FDB(Sinks):
    """FDB Sink"""

    type: Literal["fdb5"] = Field("fdb5", init=False)
    config: Union[FilePath, dict] = Field(default_factory={}, title="Config", description="Path to FDB configuration")

    @field_validator("config")
    @classmethod
    def __load_yaml(cls, v):
        return open_yaml(v)


class File(Sinks):
    """File Sink"""

    type: Literal["file"] = Field("file", init=False)
    append: bool
    per_server: bool = Field(False, serialization_alias="per-server")
    path: str


class Socket(Sinks):  # TO BE ADDED
    """Socket Sink"""

    type: Literal["socket"] = Field("socket", init=False)


SINKS = Union[FDB, File, Socket, Debug]

__all__ = ["Sinks", "FDB", "File", "SINKS"]
