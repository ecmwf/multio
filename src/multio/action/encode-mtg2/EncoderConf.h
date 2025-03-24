/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Oct 2025

#pragma once

#include "eckit/config/LocalConfiguration.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/DataModelling.h"

#include <memory>


namespace multio {


namespace action {
enum class EncoderDef : std::uint64_t
{
    Name,
    Tag,
    Conf,
    Sample,
};
}

namespace datamod {
using action::EncoderDef;

MULTIO_KEY_SET_DESCRIPTION(EncoderDef,                                                      //
                           "encoder-configuration",                                         //
                                                                                            //
                           KeyDef<EncoderDef::Name, std::string>{"name"},                   //
                           KeyDef<EncoderDef::Tag, std::string>{"tag"},                     //
                           KeyDef<EncoderDef::Conf, eckit::LocalConfiguration>{"encoder"},  //
                           KeyDef<EncoderDef::Sample, std::string>{"sample"}.tagOptional())


};  // namespace datamod


namespace action {

using EncoderDefKeySet = datamod::KeySet<EncoderDef>;
using EncoderDefKeyValueSet = datamod::KeyValueSet<EncoderDefKeySet>;
using EncoderConf = EncoderDefKeyValueSet;

//---------------------------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio

