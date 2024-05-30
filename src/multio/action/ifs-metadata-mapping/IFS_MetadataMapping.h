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


#pragma once

#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "multio/LibMultio.h"
#include "multio/action/ChainedAction.h"


namespace multio::action::ifsMetadataMapping {




/**
 * \class MultIO Action for constructing MuleIO metadata from ifs/fortran metadata
 */
class IFS_MetadataMapping final : public ChainedAction {
public:
    using ChainedAction::ChainedAction;
    explicit IFS_MetadataMapping(const ComponentConfiguration& compConf);

private:

    eckit::LocalConfiguration simulationParameters_;
    // std::unique_ptr<Flavour<T>> flavour_;
    // eckit::LocalConfiguration encodingRules_;
    // std::map<std::string, GribEncoder> encoders_;
    void getSimulationParameters( const message::Metadata& metadata);
    void print(std::ostream&) const override;
    void executeImpl(message::Message) override;

};


}  // namespace multio::action::outputManager
