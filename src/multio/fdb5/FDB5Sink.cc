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
/// @date   Dec 2015

#include <algorithm>

#include "multio/fdb5/FDB5Sink.h"

#include "eckit/exception/Exceptions.h"

#include "fdb5/config/UMask.h"

#include "multio/LibMultio.h"

using namespace eckit;
using namespace multio;

//----------------------------------------------------------------------------------------------------------------------

/// If flushOn is specified in the FDB5Sink configuration, then the archiver will flush only
/// when the specified GRIB keys are changed.
///
/// DO NOT USE THIS IN PRODUCTION
///
/// Production notifies ecFlow once flushes have returned. This option causes iflushfdb
/// calls and actual flushes to disk to become desynchronised.


FDB5Sink::FDB5Sink(const Configuration& config) :
    DataSink(config),
    dirty_(false) {

    config.get("flushOn", flushOn_);
    for (size_t i = 0; i < flushOn_.size(); i++) {
        if (lastKeys_.find(flushOn_[i]) != lastKeys_.end())
            throw UserError("Flush on key duplicated in configuration", Here());
        lastKeys_[flushOn_[i]] = std::string();
    }

    if (!flushOn_.empty())
        Log::info() << "FDB5 sink filtering iflushfdb calls according to: " << flushOn_ << std::endl;
}

FDB5Sink::~FDB5Sink() {
}

void FDB5Sink::write(DataBlobPtr blob) {

    fdb5::UMask umask(fdb5::UMask::defaultUMask());

    Log::debug<LibMultio>() << "FDB5Sink::write()" << std::endl;

    ASSERT(archiver_);

    archiver_->archive(blob);
}

void FDB5Sink::iwritefdb(int, eckit::DataBlobPtr blob) {
    FDB5Sink::write(blob);
    dirty_ = true;
}

void FDB5Sink::iopenfdb(const std::string& name, int&, const std::string& mode)
{
    Log::debug<LibMultio>() << "FDB5Sink::iopenfdb(" << name << "," << mode << ")" << std::endl;
    archiver_.reset( new fdb5::legacy::LegacyArchiver(config_) );
}

void FDB5Sink::iinitfdb()
{
    Log::debug<LibMultio>() << "FDB5Sink::iinitfdb()" << std::endl;
}

void FDB5Sink::iclosefdb(int fdbaddr)
{
    Log::debug<LibMultio>() << "FDB5Sink::iclosefdb(" << fdbaddr << ")" << std::endl;
    archiver_.reset();
}

void FDB5Sink::isetcommfdb(int comm)
{
    Log::debug<LibMultio>() << "FDB5Sink::isetcommfdb(" << comm << ")" << std::endl;
}

void FDB5Sink::isetrankfdb(int fdbaddr, int rank)
{
    Log::debug<LibMultio>() << "FDB5Sink::isetrankfdb(" << fdbaddr << "," << rank << ")" << std::endl;
}

void FDB5Sink::iset_fdb_root(int fdbaddr, const std::string& name)
{
    Log::debug<LibMultio>() << "FDB5Sink::iset_fdb_root(" << fdbaddr << "," << name << ")" << std::endl;
}

void FDB5Sink::iflushfdb(int fdbaddr)
{
    Log::debug<LibMultio>() << "FDB5Sink::iflushfdb(" << fdbaddr << ")" << std::endl;
    ASSERT(archiver_);

    // If flushOn is specified in the config, then we only flush when specified keys are changed.
    // i.e. we don't flush when iflushfdb is called.
    if (flushOn_.empty()) {
        archiver_->flush();
        dirty_ = false;
    } else {
        Log::debug<LibMultio>() << "FDB5Sink::iflushfdb skipped by flushOn configuration" << std::endl;
    }
}

void FDB5Sink::isetfieldcountfdb(int fdbaddr, int all_ranks, int this_rank)
{
    Log::debug<LibMultio>() << "FDB5Sink::isetfieldcountfdb(" << fdbaddr << "," << all_ranks << "," << this_rank << ")" << std::endl;
}

void FDB5Sink::isetvalfdb(int fdbaddr, const std::string& name, const std::string& value)
{
    Log::debug<LibMultio>() << "FDB5Sink::isetvalfdb(" << fdbaddr << "," << name << "," << value << ")" << std::endl;
    ASSERT(archiver_);
    archiver_->legacy(name, value);

    // If any of the flushOn keys are specifed, and are being changed, and not being set for the first time,
    // then flush!

    if (std::find(flushOn_.begin(), flushOn_.end(), name) != flushOn_.end()) {

        const std::string& lastVal = lastKeys_[name];
        if (lastVal != value) {
            if (dirty_) {
                Log::debug<LibMultio>() << "FDB5Sink::isetvalfdb triggering archiver flush on key change" << std::endl;
                archiver_->flush();
                dirty_ = false;
            }

            lastKeys_[name] = value;
        }
    }
}

void FDB5Sink::print(std::ostream& os) const
{
    os << "FDB5Sink()";
}

static DataSinkBuilder<FDB5Sink> FDB5SinkBuilder("fdb5");

//----------------------------------------------------------------------------------------------------------------------
