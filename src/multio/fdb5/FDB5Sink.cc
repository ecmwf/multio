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
/// @date   Dec 2015

#include "multio/fdb5/FDB5Sink.h"

#include "eckit/exception/Exceptions.h"
#include "fdb5/config/UMask.h"
#include "fdb5/config/MasterConfig.h"


using namespace eckit;
using namespace multio;

//----------------------------------------------------------------------------------------------------------------------

FDB5Sink::FDB5Sink(const Configuration& config) :
    DataSink(config) {
	fdb5::MasterConfig::instance();
}

FDB5Sink::~FDB5Sink() {
}

void FDB5Sink::write(DataBlobPtr blob) {

    fdb5::UMask umask(fdb5::UMask::defaultUMask());

    std::cout << "FDB5Sink::write()" << std::endl;

    ASSERT(archiver_);

    try {
        archiver_->archive(blob);
    } catch(Exception& e) {
        // TODO: Push Exception handling higher up...
        throw WriteError("Write error in FDB5 sink");
    }
}

void FDB5Sink::iopenfdb(const std::string& name, const std::string& mode)
{
    std::cout << "FDB5Sink::iopenfdb(" << name << "," << mode << ")" << std::endl;
    archiver_.reset( new fdb5::legacy::LegacyArchiver() );
}

void FDB5Sink::iinitfdb()
{
    std::cout << "FDB5Sink::iinitfdb()" << std::endl;
}

void FDB5Sink::iclosefdb()
{
    std::cout << "FDB5Sink::iclosefdb()" << std::endl;
    archiver_.reset(0);
}

void FDB5Sink::isetcommfdb(int comm)
{
    std::cout << "FDB5Sink::isetcommfdb(" << comm << ")" << std::endl;
}

void FDB5Sink::isetrankfdb(int rank)
{
    std::cout << "FDB5Sink::isetrankfdb(" << rank << ")" << std::endl;
}

void FDB5Sink::iset_fdb_root(const std::string& name)
{
    std::cout << "FDB5Sink::iset_fdb_root(" << name << ")" << std::endl;
}

void FDB5Sink::iflushfdb()
{
    std::cout << "FDB5Sink::iflushfdb()" << std::endl;
    ASSERT(archiver_);
    archiver_->flush();
}

void FDB5Sink::isetfieldcountfdb(int all_ranks, int this_rank)
{
    std::cout << "FDB5Sink::isetfieldcountfdb("<< all_ranks << "," << this_rank << ")" << std::endl;
}

void FDB5Sink::isetvalfdb(const std::string& name, const std::string& value)
{
    std::cout << "FDB5Sink::isetvalfdb(" << name << "," << value << ")" << std::endl;
    ASSERT(archiver_);
    archiver_->legacy(name, value);
}

void FDB5Sink::print(std::ostream& os) const
{
    os << "FDB5Sink()";
}

static DataSinkBuilder<FDB5Sink> FDB5SinkBuilder("fdb5");

//----------------------------------------------------------------------------------------------------------------------
