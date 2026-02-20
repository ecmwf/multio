/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "multio/action/ifs2mars/Ifs2mars.h"

#include <algorithm>
#include <iomanip>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"


#include "metkit/mars/MarsLanguage.h"

#include "multio/LibMultio.h"
#include "multio/datamod/Glossary.h"
#include "multio/message/Message.h"
#include "multio/util/PrecisionTag.h"
#include "multio/util/Substitution.h"


namespace multio::action::ifs2mars {

namespace detail {

message::Message ifs2marsMapping(message::Message msg) {


    // Currently a no-op placeholder for future IFS to MARS mapping logic
    return msg;
};

}  // namespace detail


void Ifs2mars::executeImpl(message::Message msg) {

    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(msg);
        return;
    }

    executeNext(detail::ifs2marsMapping(std::move(msg)));
}


void Ifs2mars::print(std::ostream& os) const {
    os << "ifs2mars";
}


static ActionBuilder<Ifs2mars> Ifs2marsBuilder("ifs2mars");


}  // namespace multio::action::ifs2mars
