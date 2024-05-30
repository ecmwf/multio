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


#include "multio/action/ifs-metadata-mapping/IFS_MetadataMapping.h"
#include "multio/action/ifs-metadata-mapping/IFS_MetadataMapping_debug.h"
#include <cmath>
#include <iomanip>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"


#include "IFS_MetadataMapping_debug.h"
#include "eckit/filesystem/PathName.h"
#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/util/PrecisionTag.h"
#include "multio/util/Substitution.h"


namespace multio::action::ifsMetadataMapping {

IFS_MetadataMapping::IFS_MetadataMapping(const ComponentConfiguration& compConf) : ChainedAction(compConf) {
    IFS_METADATA_MAPPING_OUT_STREAM << " enter constructor" << std::endl;

    IFS_METADATA_MAPPING_OUT_STREAM << " exit constructor" << std::endl;
};

void IFS_MetadataMapping::getSimulationParameters( const message::Metadata& metadata){
  IFS_METADATA_MAPPING_OUT_STREAM << " enter getSimulationParameters" << std::endl;

  IFS_METADATA_MAPPING_OUT_STREAM << " exit getSimulationParameters" << std::endl;
  return;
}

void IFS_MetadataMapping::executeImpl(message::Message msg) {

    IFS_METADATA_MAPPING_OUT_STREAM << " enter executeImpl" << std::endl;
    if (msg.tag() == message::Message::Tag::Flush) {
        IFS_METADATA_MAPPING_OUT_STREAM << " - exit executeImpl (on flush)" << std::endl;
        // Check if the flush is the first one
        if ( msg.metadata().getLong("flushType") == 0) {
            getSimulationParameters( msg.metadata() );
        }
        executeNext(msg);
        return;
    }
    else if (msg.tag() == message::Message::Tag::Field) {

        IFS_METADATA_MAPPING_OUT_STREAM << " - exit executeImpl (on msg)" << std::endl;
        executeNext( msg );
        return;

    } else {

        IFS_METADATA_MAPPING_OUT_STREAM << " - exit executeImpl (fallback)" << std::endl;
        executeNext(msg);
        return;
    }

}



void IFS_MetadataMapping::print(std::ostream& os) const {
    os << "ifs-metadata-mapping";
}


static ActionBuilder<IFS_MetadataMapping> InterpolateFesomBuilderSP("ifs-metadata-mapping");


}  // namespace multio::action::ifsMetadataMapping
