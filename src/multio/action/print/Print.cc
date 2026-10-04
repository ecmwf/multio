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

#include <chrono>
#include <ctime>
#include <fstream>
#include <iomanip>
#include <iostream>

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/runtime/Main.h"

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
    marsMiscStream_ = (stream_ == "mars-misc");
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
    else if (marsStream_ || marsMiscStream_) {
        os_ = &std::cout;
    }
    else {
        os_ = &eckit::Log::debug();
    }

    prefix_ = compConf.parsedConfig().getString("prefix", "");
}

void Print::printPrefix(std::ostream& os) const {
    const auto now = std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
    std::tm localTime;
    localtime_r(&now, &localTime);

    os << "[MultIO::print][" << std::put_time(&localTime, "%Y-%m-%d %H:%M:%S") << "][" << eckit::Main::hostname();
    const auto& world = eckit::mpi::comm("world");
    if (world.size() > 1) {
        os << ":r" << world.rank();
    }
    os << "][";
    if (const auto& planName = compConf_.planName()) {
        os << *planName;
        if (!prefix_.empty()) {
            os << "/";
        }
    }
    os << prefix_ << "] ";
}

void Print::printMars(std::ostream& os, const message::Message& msg, bool includeMisc) const {
    if (msg.tag() == message::Message::Tag::Field) {
        const auto marsRecord = dm::readRecord<dm::FullMarsRecord>(msg.metadata());
        const auto mars = dm::dumpRecord<message::Metadata>(marsRecord);

        printPrefix(os);
        os << "Field: " << std::setw(6) << count_++ << " :: \"mars\":";
        os << mars;
        if (includeMisc) {
            const auto miscRecord = dm::readRecord<dm::MiscRecord>(msg.metadata());
            const auto misc = dm::dumpUnscopedRecord<message::Metadata>(miscRecord);
            os << " :: \"misc\":" << misc;
        }
        os << std::endl;
        return;
    }

    if (msg.tag() == message::Message::Tag::Flush) {
        count_ = 1;
        long flushKind = msg.metadata().getOpt<long>("flushKind").value_or(-1);
        printPrefix(os);
        if (flushKind == 1) {
            long step = msg.metadata().getOpt<long>("step").value_or(-1);
            os << "Flush: step=" << step << std::endl;
        }
        else {
            os << "Flush: " << flushKind << std::endl;
        }
        os << std::endl << std::endl;
    }
}

void Print::executeImpl(message::Message msg) {
    ASSERT(os_);
    bool doOutput = onlyFields_ ? (msg.tag() == message::Message::Tag::Field) : true;
    if (doOutput) {
        if (marsStream_ || marsMiscStream_) {
            printMars(*os_, msg, marsMiscStream_);
        }
        else {
            printPrefix(*os_);
            *os_ << msg << std::endl;
        }
    }
    const auto diagnostic = msg;
    try {
        executeNext(std::move(msg));
    }
    catch (...) {
        printPrefix(std::cerr);
        std::cerr << "ERROR: offending message:" << std::endl;
        try {
            printMars(std::cerr, diagnostic, true);
        }
        catch (...) {
            std::cerr << diagnostic << std::endl;
        }
        std::cerr << "# ======================================================================================="
                  << std::endl;
        throw;
    }
}

void Print::print(std::ostream& os) const {
    os << "Print(stream=" << stream_ << ")";
}


static ActionBuilder<Print> PrintBuilder("print");

}  // namespace multio::action::print
