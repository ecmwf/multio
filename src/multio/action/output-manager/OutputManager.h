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

#include "OutputManagerMetadata.h"
#include "OutputManagerEncoders.h"
#include "OutputManagerDebug.h"

namespace multio::action::outputManager {




/**
 * \class MultIO Action for constructing MuleIO metadata from ifs/fortran metadata
 */
template <typename T>
class OutputManager final : public ChainedAction {
public:
    using ChainedAction::ChainedAction;
    explicit OutputManager(const ComponentConfiguration& compConf);

private:

    std::unique_ptr<Flavour<T>> flavour_;

    eckit::LocalConfiguration encodingRules_;

    std::map<std::string, GribEncoder> encoders_;

    void print(std::ostream&) const override;
    void executeImpl(message::Message) override;

    std::string generateKey(const message::Message& msg) const;
};


}  // namespace multio::action::outputManager
