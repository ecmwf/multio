/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Parametrization.h"

#include <algorithm>

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"

#include "multio/domain/Mappings.h"
#include "multio/message/Message.h"

namespace multio {
namespace parametrization {

Parametrization& Parametrization::instance() {
    static Parametrization singleton;
    return singleton;
}

void Parametrization::add(message::Message msg) {
    std::lock_guard<std::mutex> lock{mutex_};

    // Extract parametrization size
    if ( messages_.empty() ){
        nClients_  = msg.metadata().getLong("NClients");
        nMessages_ = msg.metadata().getLong("NMessages");;
    }

    // Update parametrization
    auto key = msg.fieldId();
    if (messages_.find(key) == messages_.end()) {
        messages_[key] = std::make_pair( static_cast<std::size_t>(1), std::move(msg) );
    }
    else {
        if ( compare( messages_[key].second, msg ) ) {
            messages_[key].first++;
        }
        else
        {
            std::ostringstream os;
            os << "Inconsistent parametrization" << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
    }

    return;

}

void  Parametrization::merge(){
    // Somehow need to merge all the messages
    parametrizationData_ = {};

    // Merge messages
    for ( auto& msg : messages_ ){
        auto& tmp = msg.metadata();
        parametrizationData_.set( tmp.getString("context"), std::move(msg.metadata()) ) ;
        parametrization.
    }

    // Clear the messages
    merged_ = true;
    messages_.clear();
    return;
};

const eckit::LocalConfiguration& Parametrization::get() const {

    if ( allPartsArrived() ){
        if ( !merged_ ) {
            merge( );
        }
    }
    else {
        std::ostringstream os;
        os << "Not all parts of the parametrization arrived" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }

    return parametrizationData_;
}

bool Parametrization::allPartsArrived() const {
    // extract parametrization
    bool check = false;
    if ( messages_.size() == nMessages_ ){
        for (const auto& [key, value] : messages_ ) {
            if ( value.first != nClients_ ){
                check = false;
            }
        }
    }
    return check;
}

}  // namespace domain
}  // namespace multio
