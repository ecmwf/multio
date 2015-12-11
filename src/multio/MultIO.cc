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

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

MultIO::MultIO() {}


MultIO::~MultIO() {
}

void MultIO::open(const std::string& key)
{

}

void MultIO::write(const void* buffer, const eckit::Length& length)
{

}

void MultIO::close()
{

}

void MultIO::print(std::ostream&) const
{

}

//
// LEGACY INTERFACE
//

int MultIO::iclosefdb(int *addr) {

}

int MultIO::iopenfdb(const char *name, int *addr, const char *mode, int name_len, int mode_len) {

}

int MultIO::iinitfdb(void) {

}

int MultIO::isetcommfdb(int *rank) {

}

int MultIO::isetrankfdb(int *addr, int *rank) {

}

int MultIO::iset_fdb_root(int *addr, const char *name, int name_len) {

}

int MultIO::ireadfdb(int *addr, void *data, int *words) {

}

int MultIO::iwritefdb(int *addr, void *data, int *words) {

}

int MultIO::iflushfdb(int *addr) {

}

int MultIO::isetfieldcountfdb(int *addr, int *all_ranks, int *this_rank) {

}

int MultIO::isetvalfdb(int *addr, const char *name, const char *value, int name_len, int value_len) {

}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

