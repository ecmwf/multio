/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Tiago Quintino
/// @author Simon Smart
/// @date Dec 2015

#include "multio/MultIO.h"

#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/io/DataBlob.h"

using namespace eckit;

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

MultIO::MultIO(const eckit::Configuration& config) :
    DataSink(config),
    journal_(config, this) {

    const std::vector<LocalConfiguration> configs = config.getSubConfigurations("sinks");

    for(std::vector<LocalConfiguration>::const_iterator c = configs.begin(); c != configs.end(); ++c) {
        sinks_.push_back( DataSinkFactory::build(c->getString("type"),*c) );
        sinks_.back()->setId(sinks_.size()-1);
    }

    // Must open after sinks are initialised, or the subsink configs won't exist yet.
    if (journaled_)
        journal_.open();
}

MultIO::~MultIO() {

    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        delete (*it);
    }

    // Ideally the Journal should be committed explicitly before we hit the destructors.
    if (journaled_ && journal_.isOpen()) {
        Log::warning() << "[" << *this << "] Journal has not been committed prior to MultIO destruction"
                       << std::endl;
    }
}

bool MultIO::ready() const
{
    for(sink_store_t::const_iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        if( ! (*it)->ready() ) {
            return false;
        }
    }
    return true;
}


Value MultIO::configValue() const {

    Value config(config_.get());

    // Overwrite the "sinks" component of the configuration with that returned by the
    // instantiated sinks. This allows them to include additional information that is
    // not by default in the Configuration (e.g. stuff included in a Resource).
    std::vector<Value> sink_configs;
    for (sink_store_t::const_iterator sink = sinks_.begin(); sink != sinks_.end(); ++sink) {
        sink_configs.push_back((*sink)->configValue());
    }
    config["sinks"] = Value(sink_configs);

    return config;
}


void MultIO::write(DataBlobPtr blob) {

    Log::info() << "[" << *this << "]: write (" << blob->length() << ")" << std::endl;

    JournalRecordPtr record;
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {

        try {

            (*it)->write(blob);

        } catch (WriteError& e) {
            if (!journaled_) throw;
            if (!record) record.reset( new JournalRecord(journal_, JournalRecord::WriteEntry));
            record->addWriteEntry(blob, (*it)->id());
        }
    }
}


void MultIO::flush() {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it)
        (*it)->flush();
}


void MultIO::replayRecord(const JournalRecord& record) {

    Log::info() << "[" << *this << "] Replaying journal record" << std::endl;
    Log::info() << "[" << *this << "]  - Record type: "
                       << JournalRecord::RecordTypeName(JournalRecord::RecordType(record.head_.tag_))
                       << std::endl;


    // Provide a journal record so that failures that occur during playback
    // will be (re)journaled.
    JournalRecordPtr newRecord;

    // Once the data entry is found, point to the associated data so it can be used
    // by the write entries.
    DataBlobPtr data;

    int i = 0;
    for (std::list<JournalRecord::JournalEntry>::const_iterator it = record.entries_.begin();
            it != record.entries_.end(); (++i, ++it) ) {

        Log::info() << "[" << *this << "]  * Entry: " << i << std::endl;

        switch (it->head_.tag_) {

        case JournalRecord::JournalEntry::Data:
            Log::info() << "[" << *this << "]    - Got data entry" << std::endl;
            data.reset(it->data_);
            break;

        case JournalRecord::JournalEntry::Write:
            Log::info() << "[" << *this << "]    - Write entry for journal: " << it->head_.id_ << std::endl;
            ASSERT(data);
            ASSERT(it->head_.id_ < sinks_.size());

            try {
                sinks_[it->head_.id_]->write(data);
            } catch (WriteError& e) {
                if (!journaled_) throw;
                if (!newRecord) newRecord.reset(new JournalRecord(journal_, JournalRecord::WriteEntry));
                newRecord->addWriteEntry(data, id_);
            }

            break;

        default:
            Log::warning() << "[" << *this << "]    - Unrecognised entry type";

        }
    }
}


void MultIO::commitJournal() {

    Log::info() << "[" << *this << "] Committing MultIO journal" << std::endl;
    if (!journaled_ || !journal_.isOpen()) {
        Log::warning() << "[" << *this << "] Attempting to commit a journal that has not been created"
                       << std::endl;
    } else
        journal_.close();
}


void MultIO::print(std::ostream& os) const {
    os << "MultIO(";
    for(sink_store_t::const_iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        os << (*it);
    }
    os << ")";
}

//----------------------------------------------------------------------------------------------------------------------

void MultIO::iopenfdb(const std::string& name, const std::string& mode) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->iopenfdb(name,mode);
    }
}

void MultIO::iclosefdb() {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->iclosefdb();
    }
}

void MultIO::iinitfdb() {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->iinitfdb();
    }
}

void MultIO::isetcommfdb(int rank) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->isetcommfdb(rank);
    }
}

void MultIO::isetrankfdb(int rank) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->isetrankfdb(rank);
    }
}

void MultIO::iset_fdb_root(const std::string& name) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->iset_fdb_root(name);
    }
}

void MultIO::iflushfdb() {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->iflushfdb();
    }
}

void MultIO::isetfieldcountfdb(int all_ranks, int this_rank) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->isetfieldcountfdb(all_ranks,this_rank);
    }
}

void MultIO::isetvalfdb(const std::string& name, const std::string& value) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->isetvalfdb(name,value);
    }
}

static DataSinkBuilder<MultIO> DataSinkSinkBuilder("multio");

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

