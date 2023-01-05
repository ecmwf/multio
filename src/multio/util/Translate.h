/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Aug 2022


/**
 * TODO: This may be added to eckit::Translator directly.
 */

#pragma once

#include "eckit/utils/Translator.h"

#include <iostream>
#include <iterator>

namespace multio {
namespace util {

// This allows using the Translator without having to explicitly name the type of an argument. For example in case of
// generic string conversion: translate<std::strig>(someVariable)
template <typename To, typename From>
To translate(From&& from) {
    return eckit::Translator<typename std::decay<From>::type, To>()(std::forward<From>(from));
}

}  // namespace util
}  // namespace multio
