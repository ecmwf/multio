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

#include "HEALPix_ring2nest.h"

#include <iomanip>
#include <string>

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/Log.h"

#include "atlas_io/atlas-io.h"

#include "multio/LibMultio.h"
#include "multio/util/PrecisionTag.h"
#include "multio/util/Substitution.h"

namespace multio::action {

namespace {
std::string parseCacheFileName(const ComponentConfiguration& compConf) {

    // Check necessary options
    const auto cfg = compConf.parsedConfig();
    if (!cfg.has("cache-file-name")) {
        std::ostringstream oss;
        oss << "HEALPix_ring2nest: expected \"cache-file-name\" option" << std::endl;
        throw eckit::UserError(oss.str(), Here());
    }

    // Expand file name
    const auto cacheFileName = compConf.multioConfig().replaceCurly(cfg.getString("cache-file-name"));

    // Check existence of the cache file
    eckit::PathName tmp{cacheFileName};
    if (!tmp.exists()) {
        std::ostringstream oss;
        oss << "HEALPix_ring2nest: cache file not exist: \"" << cacheFileName << "\"" << std::endl;
        throw eckit::UserError(oss.str(), Here());
    }

    return cacheFileName;
}

void checkMetadata(const message::Metadata& md) {
    auto searchGridType = md.find("gridType");
    if (searchGridType == md.end()) {
        std::ostringstream oss;
        oss << "HEALPix_ring2nest: expected \"gridType\" option" << std::endl;
        throw eckit::UserError(oss.str(), Here());
    }
    
    auto searchNSide = md.find("Nside");
    if (searchNSide == md.end()) {
        std::ostringstream oss;
        oss << "HEALPix_ring2nest: expected \"Nside\" option" << std::endl;
        throw eckit::UserError(oss.str(), Here());
    }
    
    auto searchOrderingConvention = md.find("orderingConvention");
    if (searchOrderingConvention == md.end()) {
        std::ostringstream oss;
        oss << "HEALPix_ring2nest: expected \"orderingConvention\" option" << std::endl;
        throw eckit::UserError(oss.str(), Here());
    }
    if (const std::string& gridType = searchGridType->second.get<std::string>(); gridType != "healpix" && gridType != "HEALPix") {
        std::ostringstream oss;
        oss << "HEALPix_ring2nest: expected \"gridType\" = \"HEALPix\", instead it is equal to: "
            << gridType << std::endl;
        throw eckit::UserError(oss.str(), Here());
    }
    if (const std::string& orderingConvention = searchOrderingConvention->second.get<std::string>(); orderingConvention != "ring") {
        std::ostringstream oss;
        oss << "HEALPix_ring2nest: expected \"orderingConvention\" = \"ring\", instead it is equal to: "
            << orderingConvention << std::endl;
        throw eckit::UserError(oss.str(), Here());
    }
}

std::vector<size_t> makeMapping(size_t Nside, const std::string& cacheFileName) {
    std::vector<size_t> map;
    atlas::io::RecordReader reader(cacheFileName);
    std::ostringstream os;
    os << "H" << std::setfill('0') << std::setw(8) << Nside << "_ring2nest";
    reader.read(os.str(), map).wait();
    if (map.size() != 12 * Nside * Nside) {
        std::ostringstream oss;
        oss << "HEALPix_ring2nest: expected map size : " << 12 * Nside * Nside << ", got: " << map.size() << std::endl;
        throw eckit::UserError(oss.str(), Here());
    }
    return map;
}
}  // namespace


HEALPixRingToNest::HEALPixRingToNest(const ComponentConfiguration& compConf) :
    ChainedAction(compConf), cacheFileName_{parseCacheFileName(compConf)} {}


void HEALPixRingToNest::executeImpl(message::Message msg) {

    // Bypass if it is not a field
    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(msg);
        return;
    }

    checkMetadata(msg.metadata());

    // Lookup cache
    auto key = static_cast<size_t>(msg.metadata().get<std::int64_t>("Nside"));
    if (mapping_.find(key) == mapping_.end()) {
        mapping_[key] = makeMapping(static_cast<size_t>(msg.metadata().get<std::int64_t>("Nside")), cacheFileName_);
    }
    const auto& map = mapping_.at(key);

    // Remap field
    executeNext(dispatchPrecisionTag(msg.precision(), [&](auto pt) -> message::Message {
        using Precision = typename decltype(pt)::type;
        return applyMap<Precision>(std::move(msg), map);
    }));
}


void HEALPixRingToNest::print(std::ostream& os) const {
    os << "HEALPixRingToNest";
}


static ActionBuilder<HEALPixRingToNest> HEALPixRingToNestBuilder("renumber-healpix");

}  // namespace multio::action
