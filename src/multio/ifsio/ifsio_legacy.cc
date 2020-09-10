/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <string>

#include "eckit/exception/Exceptions.h"

#include "multio/ifsio/ifsio.h"
#include "multio/ifsio/ifsio_internals.h"
#include "multio/ifsio/ifsio_legacy.h"


extern "C" {

    fortint iinitfdb_() {
        MULTIO_TRACE_FUNC();
        return 0;
    }

    fortint iinitfdb_vpp_(const char *name, int name_len) {
        std::string sname(name, name + name_len);
        MULTIO_TRACE_FUNC1(sname.c_str());
        return 0;
    }

    fortint iopenfdb_(const char *name, fortint *addr, const char *mode, int name_len, int mode_len) {
        std::string sname(name, name + name_len);
        std::string smode(mode, mode + mode_len);
        MULTIO_TRACE_FUNC2(sname.c_str(), smode.c_str());
        *addr = 0;
        return 0;
    }

    fortint iclosefdb_(fortint *addr) {
        MULTIO_TRACE_FUNC();
        return 0;
    }

    fortint iflushfdb_(const fortint *addr) {
        MULTIO_TRACE_FUNC();
        return imultio_flush_();
    }

    fortint iwritefdb_(const fortint *addr, const void *data, const fortint *words) {
        MULTIO_TRACE_FUNC();
        return imultio_write_(data, words);
    }

    fortint iset_fdb_root_(const fortint *addr, const char *name, int name_len) {
        std::string sname(name, name + name_len);
        MULTIO_TRACE_FUNC1(sname.c_str());
        return 0;
    }

    fortint isetvalfdb_(const fortint *addr, const char *name, const char *value, int name_len, int value_len) {
        std::string sname(name, name + name_len);
        std::string svalue(value, value + value_len);
        MULTIO_TRACE_FUNC2(sname.c_str(), svalue.c_str());
        return 0;
    }

    int isetcommfdb_(const fortint *comm) {
        MULTIO_TRACE_FUNC();
        return 0;
    }

    int isetrankfdb_(const fortint *addr, const fortint *rank) {
        MULTIO_TRACE_FUNC();
        return 0;
    }

    int isetfieldcountfdb_(const fortint *addr, const fortint *all_ranks, const fortint *this_rank) {
        MULTIO_TRACE_FUNC();
        return 0;
    }

    fortint ireadfdb_(const fortint *addr, void *data, fortint *words) {
        try {
            MULTIO_TRACE_FUNC();
            NOTIMP;
        } catch (std::exception &e) {
            return ifsio_handle_error(e);
        }
        return 0;
    }

} // extern C
