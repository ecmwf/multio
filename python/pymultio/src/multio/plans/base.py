# (C) Copyright 2024 European Centre for Medium-Range Weather Forecasts.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""Multio Plans Base Model"""

from pydantic import AliasGenerator, BaseModel, ConfigDict


class MultioBaseModel(BaseModel):
    """Multio Base Model
    
    Sets up a common configuration for all Multio models.
    """
    model_config = ConfigDict(
        extra="forbid",
        serialize_by_alias=True,
        validate_by_name=True,
        validate_by_alias=True,
        alias_generator=AliasGenerator(
            alias=lambda field_name: field_name.replace('_', '-'),
        )
    )