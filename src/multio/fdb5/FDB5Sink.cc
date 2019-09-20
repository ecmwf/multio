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

#include "multio/LibMultio.h"

using namespace eckit;
using namespace multio;

//--------------------------------------------------------------------------------------------------

FDB5Sink::FDB5Sink(const Configuration& config) :
    DataSink(config),
    archiver_(config){
}

FDB5Sink::~FDB5Sink() {
}

void FDB5Sink::write(DataBlobPtr blob) {
    Log::debug<LibMultio>() << "FDB5Sink::write()" << std::endl;
    archiver_.archive(blob);
}

void FDB5Sink::flush()
{
    Log::debug<LibMultio>() << "FDB5Sink::flush()" << std::endl;
    archiver_.flush();
}

void FDB5Sink::print(std::ostream& os) const {
    os << "FDB5Sink(" << config_ << ")";
}

static DataSinkBuilder<FDB5Sink> FDB5SinkBuilder("fdb5");

//--------------------------------------------------------------------------------------------------
