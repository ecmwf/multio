/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Mirco Valentini
/// @author Domokos Sármány

/// @date Oct 2023

#pragma once

#include <sstream>
#include <string>

#include "multio/action/ChainedAction.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"

namespace multio::action {


class HEALPixRingToNest : public ChainedAction {
public:
    explicit HEALPixRingToNest(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    template <typename Precision>
    message::Message applyMap(const message::Message&& msg, const std::vector<size_t>& map) const {

        if (map.size() != msg.size() / sizeof(Precision)) {
            std::ostringstream oss;
            oss << "HEALPix_ring2nest: Map has size " << map.size() << " but the message contains "
                << (msg.size() / sizeof(Precision)) << " values. " << std::endl;
            throw eckit::SeriousBug(oss.str(), Here());
        }

        std::vector<Precision> out(map.size(), 0.0);
        auto in = reinterpret_cast<const Precision*>(msg.payload().data());
        for (size_t i = 0; i < map.size(); ++i) {
            out[map[i]] = in[i];
        }

        message::Metadata md = msg.metadata();
        md.set("orderingConvention", "nested");
        eckit::Buffer buffer(reinterpret_cast<const char*>(out.data()), out.size() * sizeof(Precision));
        return message::Message{
            message::Message::Header{message::Message::Tag::Field, msg.source(), msg.destination(), std::move(md)},
            std::move(buffer)};
    }

    void print(std::ostream& os) const override;

    std::map<size_t, std::vector<size_t>> mapping_;
    std::string cacheFileName_;
};

}  // namespace multio::action
