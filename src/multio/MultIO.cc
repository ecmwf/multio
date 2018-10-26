/*
 * (C) Copyright 1996- ECMWF.
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

#include <sys/types.h>
#include <unistd.h>

#include "multio/MultIO.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/io/DataBlob.h"
#include "eckit/runtime/Main.h"
#include "eckit/utils/Translator.h"

using namespace eckit;

namespace multio {

namespace {
// TODO: move this to eckit
class ScopedTimer {
    eckit::Timer& timer_;

public:
    explicit ScopedTimer(eckit::Timer& t) : timer_(t) { timer_.start(); }
    ~ScopedTimer() { timer_.stop(); }
};
}  // namespace

//----------------------------------------------------------------------------------------------------------------------

MultIO::MultIO(const eckit::Configuration& config) :
    DataSink(config),
    journal_(config, this),
    stats_(std::string("Multio ") + Main::hostname() + ":" +
           Translator<int, std::string>()(::getpid())),
    trigger_(config),
    journaled_(config.getBool("journaled", false)) {

    const std::vector<LocalConfiguration> configs = config.getSubConfigurations("sinks");

    for (const auto& config : configs) {
        SinkStoreElem elem;
        elem.sink_.reset(DataSinkFactory::build(config.getString("type"), config));
        elem.journalAlways_ = config.getBool("journalAlways", false);
        elem.sink_->setId(sinks_.size());

        sinks_.push_back(elem);
    }

    // Must open after sinks are initialised, or the subsink configs won't exist yet.
    if (journaled_)
        journal_.open();
}

MultIO::~MultIO() {
    // Ideally the Journal should be committed explicitly before we hit the destructors.
    if (journaled_ && journal_.isOpen()) {
        Log::warning() << "[" << *this
                       << "] Journal has not been committed prior to MultIO destruction"
                       << std::endl;
    }
}

bool MultIO::ready() const {
    std::lock_guard<std::mutex> lock(mutex_);

    for (auto const& elem : sinks_) {
        ASSERT(elem.sink_);
        if (!elem.sink_->ready()) {
            return false;
        }
    }
    return true;
}


Value MultIO::configValue() const {
    std::lock_guard<std::mutex> lock(mutex_);

    Value config(config_.get());

    // Overwrite the "sinks" component of the configuration with that returned by the
    // instantiated sinks. This allows them to include additional information that is
    // not by default in the Configuration (e.g. stuff included in a Resource).
    std::vector<Value> sink_configs;
    for (const auto& elem : sinks_) {
        ASSERT(elem.sink_);
        sink_configs.push_back(elem.sink_->configValue());
    }
    config["sinks"] = Value(sink_configs);

    return config;
}


void MultIO::write(DataBlobPtr blob) {
    std::lock_guard<std::mutex> lock(mutex_);

    {
        ScopedTimer scTimer{timer_};
        JournalRecordPtr record;
        for (const auto& elem : sinks_) {
            ASSERT(elem.sink_);
            bool journal_entry = false;

            try {
                elem.sink_->write(blob);
                if (elem.journalAlways_)
                    journal_entry = true;
            } catch (Exception& e) {
                if (!journaled_)
                    throw;
                journal_entry = true;
            }

            if (journal_entry) {
                if (!record)
                    record.reset(new JournalRecord(journal_, JournalRecord::WriteEntry));
                record->addWriteEntry(blob, elem.sink_->id());
            }
        }
    }

    trigger_.events(blob);

    stats_.logWrite(blob->length(), timer_);
}

void MultIO::trigger(const eckit::StringDict& metadata) const {
    trigger_.events(metadata);
}

void MultIO::flush() {
    std::lock_guard<std::mutex> lock(mutex_);

    {
        ScopedTimer scTimer{timer_};
        for (const auto& elem : sinks_) {
            ASSERT(elem.sink_);
            elem.sink_->flush();
        }
    }

    // Log the flush
    stats_.logFlush(timer_);
}


void MultIO::replayRecord(const JournalRecord& record) {
    std::lock_guard<std::mutex> lock(mutex_);

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

    int counter = 0;
    for (const auto& entry : record.entries_) {
        Log::info() << "[" << *this << "]  * Entry: " << counter++ << std::endl;

        switch (entry.head_.tag_) {
            case JournalRecord::JournalEntry::Data:
                Log::info() << "[" << *this << "]    - Got data entry" << std::endl;
                data.reset(entry.data_);
                break;

            case JournalRecord::JournalEntry::Write: {
                Log::info() << "[" << *this << "]    - Write entry for journal: " << entry.head_.id_
                            << std::endl;
                ASSERT(data);
                ASSERT(entry.head_.id_ < sinks_.size());

                bool journal_entry = false;

                try {
                    ASSERT(sinks_[entry.head_.id_].sink_);
                    sinks_[entry.head_.id_].sink_->write(data);
                } catch (Exception& e) {
                    if (!journaled_)
                        throw;
                    journal_entry = true;
                }

                if (journal_entry) {
                    if (!newRecord)
                        newRecord.reset(new JournalRecord(journal_, JournalRecord::WriteEntry));
                    newRecord->addWriteEntry(data, id_);
                }

                break;
            }

            default:
                Log::warning() << "[" << *this << "]    - Unrecognised entry type";
        }
    }
}

void MultIO::report(std::ostream& s) {
    stats_.report(s);
}


void MultIO::commitJournal() {
    std::lock_guard<std::mutex> lock(mutex_);

    Log::info() << "[" << *this << "] Committing MultIO journal" << std::endl;
    if (!journaled_ || !journal_.isOpen()) {
        Log::warning() << "[" << *this
                       << "] Attempting to commit a journal that has not been created" << std::endl;
    } else
        journal_.close();
}


void MultIO::print(std::ostream& os) const {
    std::lock_guard<std::mutex> lock(mutex_);
    os << "MultIO(";
    for (const auto& elem : sinks_) {
        ASSERT(elem.sink_);
        os << *(elem.sink_);
    }
    os << ")";
}

//----------------------------------------------------------------------------------------------------------------------

void MultIO::iopenfdb(const std::string& name, int& fdbaddr, const std::string& mode) {
    std::lock_guard<std::mutex> lock(mutex_);

    Log::info() << "MultIO iopenfdb name=" << name << " mode=" << mode << std::endl;
    {
        ScopedTimer scTimer{timer_};
        for (auto& elem : sinks_) {
            ASSERT(elem.sink_);
            /// NOTE: this does not quite work with multiple FDB4 since fdbaddr will be overwritten
            elem.sink_->iopenfdb(name, fdbaddr, mode);
        }
    }
    stats_.logiopenfdb_(timer_);
}

void MultIO::iclosefdb(int fdbaddr) {
    std::lock_guard<std::mutex> lock(mutex_);

    {
        ScopedTimer scTimer{timer_};
        for (auto& elem : sinks_) {
            ASSERT(elem.sink_);
            elem.sink_->iclosefdb(fdbaddr);
        }
    }
    stats_.logiclosefdb_(timer_);
}

void MultIO::iinitfdb() {
    std::lock_guard<std::mutex> lock(mutex_);
    {
        ScopedTimer scTimer{timer_};
        for (auto& elem : sinks_) {
            ASSERT(elem.sink_);
            elem.sink_->iinitfdb();
        }
    }
    stats_.logiinitfdb_(timer_);
}

void MultIO::isetcommfdb(int rank) {
    std::lock_guard<std::mutex> lock(mutex_);
    for (auto& elem : sinks_) {
        ASSERT(elem.sink_);
        elem.sink_->isetcommfdb(rank);
    }
}

void MultIO::isetrankfdb(int fdbaddr, int rank) {
    std::lock_guard<std::mutex> lock(mutex_);
    for (auto& elem : sinks_) {
        ASSERT(elem.sink_);
        elem.sink_->isetrankfdb(fdbaddr, rank);
    }
}

void MultIO::iset_fdb_root(int fdbaddr, const std::string& name) {
    std::lock_guard<std::mutex> lock(mutex_);
    for (auto& elem : sinks_) {
        ASSERT(elem.sink_);
        elem.sink_->iset_fdb_root(fdbaddr, name);
    }
}

void MultIO::iflushfdb(int fdbaddr) {
    std::lock_guard<std::mutex> lock(mutex_);
    {
        ScopedTimer scTimer{timer_};
        for (auto& elem : sinks_) {
            ASSERT(elem.sink_);
            elem.sink_->iflushfdb(fdbaddr);
        }
    }
    stats_.logiflushfdb_(timer_);
}

void MultIO::isetfieldcountfdb(int fdbaddr, int all_ranks, int this_rank) {
    std::lock_guard<std::mutex> lock(mutex_);
    for (auto& elem : sinks_) {
        ASSERT(elem.sink_);
        elem.sink_->isetfieldcountfdb(fdbaddr, all_ranks, this_rank);
    }
}

void MultIO::isetvalfdb(int fdbaddr, const std::string& name, const std::string& value) {
    std::lock_guard<std::mutex> lock(mutex_);
    {
        ScopedTimer scTimer{timer_};
        for (auto& elem : sinks_) {
            ASSERT(elem.sink_);
            elem.sink_->isetvalfdb(fdbaddr, name, value);
        }
    }
    stats_.logisetvalfdb_(timer_);
}

void MultIO::iwritefdb(int fdbaddr, eckit::DataBlobPtr blob) {
    std::lock_guard<std::mutex> lock(mutex_);

    {
        ScopedTimer scTimer{timer_};
        for (auto& elem : sinks_) {
            ASSERT(elem.sink_);
            elem.sink_->iwritefdb(fdbaddr, blob);
        }
    }

    trigger_.events(blob);

    stats_.logiwritefdb_(blob->length(), timer_);
}

static DataSinkBuilder<MultIO> DataSinkSinkBuilder("multio");

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio
