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

using namespace eckit;

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

MultIO::MultIO(const eckit::Configuration& config) :
    DataSink(config),
    journal_(config) {

    if (journaled_)
        journal_.open();

    const std::vector<LocalConfiguration> configs = config.getSubConfigurations("sinks");

    for(std::vector<LocalConfiguration>::const_iterator c = configs.begin(); c != configs.end(); ++c) {
        sinks_.push_back( DataSinkFactory::build(c->getString("type"),*c) );
        sinks_.back()->setId(sinks_.size()-1);
    }

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


void MultIO::write(const void* buffer, const Length& length, JournalRecord *const record, Metadata *const metadata ) {

    eckit::Log::info() << "[" << *this << "]: write (" << length << ")" << std::endl;

    JournalRecord* r;

    // shall we create our own journal record ?
    ScopedPtr<JournalRecord> newRecord;
    if( !record && journaled_ ) {
        newRecord.reset( new JournalRecord(journal_, JournalRecord::WriteEntry) );
        r = newRecord.get();
    }
    else
        r = record;

    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->write(buffer, length, r, metadata);
    }

    // if we are the creator of the journal record,
    // we are responsible for ensuring that it gets written if it is populated.
    if( newRecord ) {
        journal_.writeRecord(*newRecord);
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

int MultIO::iopenfdb(const char *name, const char *mode, int name_len, int mode_len) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->iopenfdb(name,mode,name_len,mode_len);
    }
    return 0;
}

int MultIO::iclosefdb() {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->iclosefdb();
    }
    return 0;
}

int MultIO::iinitfdb() {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->iinitfdb();
    }
    return 0;
}

int MultIO::isetcommfdb(int *rank) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->isetcommfdb(rank);
    }
    return 0;
}

int MultIO::isetrankfdb(int *rank) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->isetrankfdb(rank);
    }
    return 0;
}

int MultIO::iset_fdb_root(const char *name, int name_len) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->iset_fdb_root(name,name_len);
    }
    return 0;
}

int MultIO::iflushfdb() {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->iflushfdb();
    }
    return 0;
}

int MultIO::isetfieldcountfdb(int *all_ranks, int *this_rank) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->isetfieldcountfdb(all_ranks,this_rank);
    }
    return 0;
}

int MultIO::isetvalfdb(const char *name, const char *value, int name_len, int value_len) {
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        (*it)->isetvalfdb(name,value,name_len,value_len);
    }
    return 0;
}

static DataSinkBuilder<MultIO> DataSinkSinkBuilder("multio");

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

