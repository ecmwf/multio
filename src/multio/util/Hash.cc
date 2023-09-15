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


#include "multio/util/Hash.h"

namespace multio::util {

//-----------------------------------------------------------------------------

std::size_t hash_combine_args(std::size_t lhs) noexcept {
    return lhs;
}

//-----------------------------------------------------------------------------

}  // namespace multio::util
