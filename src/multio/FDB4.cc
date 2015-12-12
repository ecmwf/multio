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

#include "multio/FDB4.h"

#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"
#include "eckit/exception/Exceptions.h"

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

FDB4::FDB4(const eckit::Configuration& config) : DataSink() {}

FDB4::~FDB4() {
}

void FDB4::open(const std::string& key)
{

}

void FDB4::write(const void* buffer, const eckit::Length& length)
{

}

void FDB4::close()
{

}

void FDB4::print(std::ostream&) const
{

}

//
// LEGACY INTERFACE
//

int FDB4::iclosefdb(int *addr) {

}

int FDB4::iopenfdb(const char *name, int *addr, const char *mode, int name_len, int mode_len) {

}

int FDB4::iinitfdb(void) {

}

int FDB4::isetcommfdb(int *rank) {

}

int FDB4::isetrankfdb(int *addr, int *rank) {

}

int FDB4::iset_fdb_root(int *addr, const char *name, int name_len) {

}

int FDB4::ireadfdb(int *addr, void *data, int *words) {

}

int FDB4::iwritefdb(int *addr, void *data, int *words) {

}

int FDB4::iflushfdb(int *addr) {

}

int FDB4::isetfieldcountfdb(int *addr, int *all_ranks, int *this_rank) {

}

int FDB4::isetvalfdb(int *addr, const char *name, const char *value, int name_len, int value_len) {

}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

