/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Encode.h"

#include <iostream>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/StdFile.h"

#include "multio/LibMultio.h"
#include "multio/util/ConfigurationPath.h"
#include "multio/util/ScopedTimer.h"

namespace multio {
namespace action {

using util::configuration_path_name;

namespace {

ConfigurationContext getEncodingConfiguration(const ConfigurationContext& confCtx) {
    return confCtx.recast(([&](){
        if (confCtx.config().has("encoding")) {
            auto encodingKeyMaybe = ([&]() {
                try {
                    return eckit::Optional<std::string>{confCtx.config().getString("encoding")};
                }
                catch (...) {
                    return eckit::Optional<std::string>{};
                }
            })();

            if (encodingKeyMaybe) {
                auto encodings = ([&]() {
                    try {
                        return confCtx.globalConfig().getSubConfiguration("encodings");
                    }
                    catch (...) {
                        std::ostringstream oss;
                        oss << "No global \"encodings\" mapping is defined to look up encoding configuration \""
                            << *encodingKeyMaybe << "\"";
                        std::throw_with_nested(eckit::Exception(oss.str()));
                    }
                }());
                auto encoding = ([&]() {
                    try {
                        return encodings.getSubConfiguration(*encodingKeyMaybe);
                    }
                    catch (...) {
                        std::ostringstream oss;
                        oss << "Global \"encodings\" configuration contains no mapping for encoding configuration \""
                            << *encodingKeyMaybe << "\"";
                        std::throw_with_nested(eckit::Exception(oss.str()));
                    }
                }());

                /// @TODO Allow defining overwrites. Problem: we can not copy mappings/values from one to another configuration
                /// without knowing it's type explicitly
                // if (confCtx.config().has("overwrite")) {
                //     confCtx.config().getSubConfiguration("overwrite")

                // }

                return encoding;
            }
            else {
                return confCtx.config().getSubConfiguration("encoding");
            }
        } 
        return confCtx.config();
    })());
}

std::unique_ptr<GribEncoder> make_encoder(const ConfigurationContext& confCtx) {
    auto format = confCtx.config().getString("format");

    if (format == "grib") {
        ASSERT(confCtx.config().has("template"));
        std::string tmplPath = confCtx.config().getString("template");
        // TODO provide utility to distinguish between relative and absolute paths
        eckit::AutoStdFile fin{ tmplPath.rfind("/", 0) == 0 ? eckit::PathName{tmplPath} : (confCtx.pathName() / confCtx.config().getString("template"))};
        int err;
        return std::unique_ptr<GribEncoder>{
            new GribEncoder{codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err), confCtx.config()}};
    }
    else if (format == "raw") {
        return nullptr;  // leave message in raw binary format
    }
    else {
        throw eckit::SeriousBug("Encoding format <" + format + "> is not supported");
    }
}
}  // namespace

using message::Message;
using message::Peer;

Encode::Encode(const ConfigurationContext& confCtx, ConfigurationContext&& encConfCtx) :
    Action{confCtx}, format_{encConfCtx.config().getString("format")}, encoder_{make_encoder(encConfCtx)} {}
    
Encode::Encode(const ConfigurationContext& confCtx) :
    Encode(confCtx, getEncodingConfiguration(confCtx)) {}

void Encode::executeImpl(Message msg) const {
    if (msg.tag() != Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }
    if (not encoder_) {
        executeNext(std::move(msg));
        return;
    }

    if (isOcean(msg.metadata())) {
        //! TODO shoud not be checked here anymore, encoder_ should have been initialized according to format_
        ASSERT(format_ == "grib");

        LOG_DEBUG_LIB(LibMultio) << " *** Looking for grid info for subtype: " << msg.domain() << std::endl;

        if (encoder_->gridInfoReady(msg.domain())) {
            executeNext(encodeField(std::move(msg)));
        }
        else {
            LOG_DEBUG_LIB(LibMultio) << "*** Grid metadata: " << msg.metadata() << std::endl;
            if (encoder_->setGridInfo(msg)) {
                executeNext(encodeLatitudes(msg.domain()));
                executeNext(encodeLongitudes(msg.domain()));
            }
        }
    } else {
        executeNext(encodeField(std::move(msg)));
    }
}

void Encode::print(std::ostream& os) const {
    os << "Encode(format=" << format_ << ")";
}

message::Message Encode::encodeField(const message::Message& msg) const {
    try {
        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
        return encoder_->encodeField(msg);
    } catch (...) {
        std::ostringstream oss;
        oss << "Encode::encodeField with Message: " << msg;
        std::throw_with_nested(eckit::Exception(oss.str()));
    }
}

message::Message Encode::encodeLatitudes(const std::string& subtype) const {
    try {
        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
        return encoder_->encodeLatitudes(subtype);
    } catch (...) {
        std::ostringstream oss;
        oss << "Encode::encodeLatitudes with subtype: " << subtype;
        std::throw_with_nested(eckit::Exception(oss.str()));
    }
}

message::Message Encode::encodeLongitudes(const std::string& subtype) const {
    try {
        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
        return encoder_->encodeLongitudes(subtype);
    } catch (...) {
        std::ostringstream oss;
        oss << "Encode::encodeLongitudes with subtype: " << subtype;
        std::throw_with_nested(eckit::Exception(oss.str()));
    }
}

static ActionBuilder<Encode> EncodeBuilder("encode");

}  // namespace action
}  // namespace multio
