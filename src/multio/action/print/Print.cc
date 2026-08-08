/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Print.h"

#include <fstream>
#include <iomanip>
#include <iostream>

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"

#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/EntryDumper.h"

#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/EntryDumper.h"
#include "multio/datamod/core/EntryParser.h"
#include "multio/datamod/core/Record.h"
#include "multio/message/Parametrization.h"
#include "multio/util/PrecisionTag.h"

namespace multio::action::print {

namespace dm = multio::datamod;

Print::Print(const ComponentConfiguration& compConf) : ChainedAction(compConf) {
    stream_ = compConf.parsedConfig().getString("stream", "info");
    onlyFields_ = compConf.parsedConfig().getBool("only-fields", false);
    marsStream_ = (stream_ == "mars");
    count_ = 1;

    if (stream_ == "info") {
        os_ = &eckit::Log::info();
    }
    else if (stream_ == "error") {
        os_ = &eckit::Log::error();
    }
    else if (stream_ == "cout") {
        os_ = &std::cout;
    }
    else if (stream_ == "mars") {
        os_ = &std::cout;
    }
    else {
        os_ = &eckit::Log::debug();
    }

    prefix_ = compConf.parsedConfig().getString("prefix", "");
}

void Print::printPrefix(std::ostream& os) const {
    if (!prefix_.empty()) {
        os << prefix_ << ": ";
    }
}

void Print::printMars(std::ostream& os, const message::Message& msg) const {
    if (msg.tag() == message::Message::Tag::Field) {
        auto mars = dm::readRecord<dm::FullMarsRecord>(msg.metadata());
        auto md = dm::dumpRecord<message::Metadata>(mars);

        // printPrefix(os);
        os << prefix_ << ": Field: " << std::setw(6) << count_++ << " :: \"mars\":";
        os << md << std::endl;
        return;
    }

    if (msg.tag() == message::Message::Tag::Flush) {
        count_ = 1;
        // printPrefix(os);
        long flushKind = msg.metadata().getOpt<long>("flushKind").value_or(-1);
        if (flushKind == 1) {
            long step = msg.metadata().getOpt<long>("step").value_or(-1);
            os << prefix_ << ": Flush: step=" << step << std::endl;
        }
        else {
            os << prefix_ << ": Flush: " << flushKind << std::endl;
        }
        os << std::endl << std::endl;
    }
}

void Print::executeImpl(message::Message msg) {
    ASSERT(os_);
    bool doOutput = onlyFields_ ? (msg.tag() == message::Message::Tag::Field) : true;
    if (doOutput) {
        if (marsStream_) {
            printMars(*os_, msg);
        }
        else {
            printPrefix(*os_);
            *os_ << msg << std::endl;
        }
    }
    // try {
        executeNext(std::move(msg));
    // }
    // catch (...) {
    //     std::cerr << "Received \"mars\":";
    //     printMars(std::cerr, msg);
    //     std::cerr << "# =======================================================================================" << std::endl;
    //     std::cerr << std::endl << std::endl << std::endl << std::endl << std::endl << std::endl << std::endl;
    // }
}

void Print::print(std::ostream& os) const {
    os << "Print(stream=" << stream_ << ")";
}


static ActionBuilder<Print> PrintBuilder("print");

}  // namespace multio::action::print
