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
    journal_(config, this),
    journaled_(config.getBool("journaled", false)),
    nwrites_(0),
    nflushes_(0),
    mutex_() {

    const std::vector<LocalConfiguration> configs = config.getSubConfigurations("sinks");

    for(std::vector<LocalConfiguration>::const_iterator c = configs.begin(); c != configs.end(); ++c) {

        SinkStoreElem elem;
        elem.sink_.reset(DataSinkFactory::build(c->getString("type"),*c));
        elem.journalAlways_=  c->getBool("journalAlways", false);
        elem.sink_->setId(sinks_.size());

        sinks_.push_back( elem );
    }

    // Must open after sinks are initialised, or the subsink configs won't exist yet.
    if (journaled_)
        journal_.open();
}

MultIO::~MultIO() {

    // Ideally the Journal should be committed explicitly before we hit the destructors.
    if (journaled_ && journal_.isOpen()) {
        Log::warning() << "[" << *this << "] Journal has not been committed prior to MultIO destruction"
                       << std::endl;
    }

    Log::info() << std::endl;
    Log::info() << "MultIO data movement logging:" << std::endl;
    Log::info() << "=============================" << std::endl;
    stats_.report(Log::info());
    Log::info() << std::endl;
}

bool MultIO::ready() const {
    AutoLock<Mutex> lock(mutex_);

    for(sink_store_t::const_iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        ASSERT( it->sink_ );
        if( ! it->sink_->ready() ) {
            return false;
        }
    }
    return true;
}


Value MultIO::configValue() const {
    AutoLock<Mutex> lock(mutex_);

    Value config(config_.get());

    // Overwrite the "sinks" component of the configuration with that returned by the
    // instantiated sinks. This allows them to include additional information that is
    // not by default in the Configuration (e.g. stuff included in a Resource).
    std::vector<Value> sink_configs;
    for (sink_store_t::const_iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        ASSERT( it->sink_ );
        sink_configs.push_back(it->sink_->configValue());
    }
    config["sinks"] = Value(sink_configs);

    return config;
}


void MultIO::write(DataBlobPtr blob) {

    AutoLock<Mutex> lock(mutex_);

    ++nwrites_;

    if(nwrites_ == 1) {
        Log::info() << "MultIO first write len=" << blob->length() << " -- following writes will be silent" << std::endl;
    }
    else {
        Log::debug() << "MultIO write " << nwrites_ << " len=" << blob->length() << std::endl;
    }

    JournalRecordPtr record;
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {

        ASSERT( it->sink_ );
        bool journal_entry = false;

        try {
            it->sink_->write(blob);
            if (it->journalAlways_) journal_entry = true;
        } catch (Exception& e) {
            if (!journaled_) throw;
            journal_entry = true;
        }

        if (journal_entry) {
            if (!record) record.reset( new JournalRecord(journal_, JournalRecord::WriteEntry));
            record->addWriteEntry(blob, it->sink_->id());
        }
    }

    // Log the write
    stats_.logWrite(blob->length());
}


void MultIO::flush() {

    AutoLock<Mutex> lock(mutex_);

    ++nflushes_;

    if(nflushes_ == 1) {
        Log::info() << "MultIO first flush -- following flushes will be silent" << std::endl;
    }
    else {
        Log::debug() << "MultIO flush " << nflushes_ << std::endl;
    }

    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        ASSERT( it->sink_ );
        it->sink_->flush();
    }
}


void MultIO::replayRecord(const JournalRecord& record) {
    AutoLock<Mutex> lock(mutex_);

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

        case JournalRecord::JournalEntry::Write: {
            Log::info() << "[" << *this << "]    - Write entry for journal: " << it->head_.id_ << std::endl;
            ASSERT(data);
            ASSERT(it->head_.id_ < sinks_.size());

            bool journal_entry = false;

            try {
                ASSERT( sinks_[it->head_.id_].sink_ );
                sinks_[it->head_.id_].sink_->write(data);
            } catch (Exception& e) {
                if (!journaled_) throw;
                journal_entry = true;
            }

            if (journal_entry) {
                if (!newRecord) newRecord.reset(new JournalRecord(journal_, JournalRecord::WriteEntry));
                newRecord->addWriteEntry(data, id_);
            }

            break;
        }

        default:
            Log::warning() << "[" << *this << "]    - Unrecognised entry type";

        }
    }
}


void MultIO::commitJournal() {
    AutoLock<Mutex> lock(mutex_);

    Log::info() << "[" << *this << "] Committing MultIO journal" << std::endl;
    if (!journaled_ || !journal_.isOpen()) {
        Log::warning() << "[" << *this << "] Attempting to commit a journal that has not been created"
                       << std::endl;
    } else
        journal_.close();
}


void MultIO::print(std::ostream& os) const {
    AutoLock<Mutex> lock(mutex_);
    os << "MultIO(";
    for(sink_store_t::const_iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        ASSERT( it->sink_ );
        os << *(it->sink_);
    }
    os << ")";
}

//----------------------------------------------------------------------------------------------------------------------

void MultIO::iopenfdb(const std::string& name, int& fdbaddr, const std::string& mode) {

    AutoLock<Mutex> lock(mutex_);

    Log::info() << "MultIO iopenfdb name=" << name << " mode=" << mode << std::endl;

    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        ASSERT( it->sink_ );
        /// NOTE: this does not quite work with multiple FDB4 since fdbaddr will be overwritten
        it->sink_->iopenfdb(name, fdbaddr, mode);
    }
}

void MultIO::iclosefdb(int fdbaddr) {

    AutoLock<Mutex> lock(mutex_);

    Log::info() << "MultIO iclosefdb writes=" << nwrites_ << " flushes=" << nflushes_ << std::endl;

    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        ASSERT( it->sink_ );
        it->sink_->iclosefdb(fdbaddr);
    }
}

void MultIO::iinitfdb() {
    AutoLock<Mutex> lock(mutex_);
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        ASSERT( it->sink_ );
        it->sink_->iinitfdb();
    }
}

void MultIO::isetcommfdb(int rank) {
    AutoLock<Mutex> lock(mutex_);
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        ASSERT( it->sink_ );
        it->sink_->isetcommfdb(rank);
    }
}

void MultIO::isetrankfdb(int fdbaddr, int rank) {
    AutoLock<Mutex> lock(mutex_);
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        ASSERT( it->sink_ );
        it->sink_->isetrankfdb(fdbaddr, rank);
    }
}

void MultIO::iset_fdb_root(int fdbaddr, const std::string& name) {
    AutoLock<Mutex> lock(mutex_);
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        ASSERT( it->sink_ );
        it->sink_->iset_fdb_root(fdbaddr, name);
    }
}

void MultIO::iflushfdb(int fdbaddr) {

    AutoLock<Mutex> lock(mutex_);

    ++nflushes_;

    if(nflushes_ == 1) {
        Log::info() << "MultIO first iflushfdb" << std::endl;
    }

    Log::debug() << "MultIO flush " << nflushes_ << std::endl;

    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        ASSERT( it->sink_ );
        it->sink_->iflushfdb(fdbaddr);
    }
}

void MultIO::isetfieldcountfdb(int fdbaddr, int all_ranks, int this_rank) {
    AutoLock<Mutex> lock(mutex_);
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        ASSERT( it->sink_ );
        it->sink_->isetfieldcountfdb(fdbaddr, all_ranks, this_rank);
    }
}

void MultIO::isetvalfdb(int fdbaddr, const std::string& name, const std::string& value) {
    AutoLock<Mutex> lock(mutex_);
    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        ASSERT( it->sink_ );
        it->sink_->isetvalfdb(fdbaddr, name, value);
    }
}

void MultIO::iwritefdb(int fdbaddr, eckit::DataBlobPtr blob) {

    AutoLock<Mutex> lock(mutex_);

    ++nwrites_;

    if(nwrites_ == 1) {
        Log::info() << "MultIO first iwritefdb len=" << blob->length() << " -- following writes will be silent" << std::endl;
    }

    Log::debug() << "MultIO write " << nwrites_ << " len=" << blob->length() << std::endl;

    for(sink_store_t::iterator it = sinks_.begin(); it != sinks_.end(); ++it) {
        ASSERT( it->sink_ );
        it->sink_->iwritefdb(fdbaddr, blob);
    }
}

static DataSinkBuilder<MultIO> DataSinkSinkBuilder("multio");

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

