# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""
Multio Actions

Defined as Pydantic models for type validation.
"""

from __future__ import annotations

from typing import Any, Literal, Union

from pydantic import BaseModel, Field, ValidationInfo, field_validator, validate_call
from typing_extensions import Annotated

from .sinks import SINKS

SinksType = Annotated[SINKS, Field(discriminator="type", title="Sinks")]


class Action(BaseModel):
    """Base Action class.

    Contains only a `type` field.

    Should not be instantiated directly, use one of the subclasses instead.
    """

    type: str = Field(str, description="Action type")

class Null(Action):
    """Null Action.

    Does nothing.
    """

    type: Literal["null"] = Field("null", init=False)


class Select(Action):
    """Select Action.

    Select on some metadata within the data.
    """

    type: Literal["select"] = Field("select", init=False)
    match: list[dict[str, Any]] = Field(description="List of dictionaries to match against")


class Statistics(Action):
    """Statistics Action.

    Calculate statistics on the data.
    """

    type: Literal["statistics"] = Field("statistics", init=False)
    operations: list[Literal["average", "minimum", "maximum", "accumulate", "instant"]]
    output_frequency: str = Field(serialization_alias="output-frequency", examples=["5h", "10d", "1w"])


class Transport(Action):
    """Transport Action"""

    type: Literal["transport"] = "transport"
    target: str


class Aggregation(Action):
    """Aggregation Action"""

    type: Literal["aggregation"] = Field("aggregation", init=False)


class Interpolate(Action):
    """Interpolate Action"""

    type: Literal["interpolate"] = Field("interpolate", init=False)
    options: dict[str, Any] = Field(default_factory=dict)


class Print(Action):
    """Print Action"""

    type: Literal["print"] = Field("print", init=False)
    stream: Literal["cout", "info", "error"] = Field("info", description="Stream to print to")
    prefix: str = Field("", description="Prefix to print")
    only_fields: bool = Field(False, serialization_alias="only-fields")


class Mask(Action):
    """Mask Action"""

    type: Literal["mask"] = Field("mask", init=False)
    apply_bitmap: bool = Field(True, serialization_alias="apply-bitmap")
    missing_value: float = Field(None, serialization_alias="missing-value")  # Need to set to max
    offset_value: float = Field(273.15, serialization_alias="offset-value")


class Encode(Action):
    """Encode Action"""

    type: Literal["encode"] = Field("encode", init=False)
    format: Literal["grib", "raw"]
    template: str | None = Field(None, validate_default=True)
    grid_type: str | None = Field(None, serialization_alias="grid-type")
    atlas_named_grid: str | None = Field(None, serialization_alias="atlas-named-grid")
    additional_metadata: dict[str, Any] = Field(default_factory=dict, serialization_alias="additional-metadata")

    @field_validator("template", mode="after")
    @classmethod
    def check_not_none(cls, v: str, info: ValidationInfo) -> str:
        if info.data.get("format") == "grib" and v is None:
            raise ValueError("template is required for grib format")
        return v

class EncodeMTG(Action):
    """Encode-mtg2 Action"""
    type: Literal["encode-mtg2"] = Field("encode-mtg2", init=False)


class Sink(Action):
    """Sink Action.

    Contains a list of sinks to write to.
    """

    type: Literal["sink"] = Field("sink", init=False)
    sinks: list[SinksType] = Field(default_factory=lambda: [])

    @validate_call
    def add_sink(self, sink: SinksType):
        self.sinks.append(sink)

    @validate_call
    def extend_sinks(self, sink: list[SinksType]):
        self.sinks.extend(sink)


class SingleField(Action):
    """Single Field Sink"""

    type: Literal["single-field-sink"] = Field("single-field-sink", init=False)


ACTIONS = Union[Select, Statistics, Transport, Aggregation, Interpolate, Null, Print, Mask, Encode, EncodeMTG, Sink, SingleField]

__all__ = ["ACTIONS", "Action", "Select", "Statistics", "Transport", "Aggregation", "Null", "Print", "Mask", "Encode", "EncodeMTG", "Sink"]
