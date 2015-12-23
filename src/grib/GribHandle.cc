/*
 * (C) Copyright 1996-2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "grib_api.h"

#include "grib/GribHandle.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/parser/StringTools.h"

extern "C" {
    int grib_keys_iterator_get_long(grib_keys_iterator *kiter, long *v, size_t *len);
    int grib_keys_iterator_get_double(grib_keys_iterator *kiter, double *v, size_t *len);
    int grib_keys_iterator_get_string(grib_keys_iterator *kiter, char *v, size_t *len);
    int grib_keys_iterator_get_bytes(grib_keys_iterator *kiter, unsigned char *v, size_t *len);
    int grib_keys_iterator_get_native_type(grib_keys_iterator *kiter);
}

using namespace eckit;

namespace grib {

//----------------------------------------------------------------------------------------------------------------------

GribHandle::GribHandle( eckit::Stream& s )
{
    bool b, more;
    std::string key;
    std::string str;
    long ival;
    double dval;

    s >> more;
    while(more)
    {
        s >> key;

        s >> b;
        if(b) {
            s >> str;
            stringValues_[key] = str;
        }

        s >> b;
        if(b) {
            s >> ival;
            longValues_[key] = ival;
        }

        s >> b;
        if(b) {
            s >> dval;
            doubleValues_[key] = dval;
        }
        s >> more;
    }
}

GribHandle::GribHandle(const Buffer& buffer,size_t size)
{
    size_t len;
    const char *message = buffer;
    char val[80] = {0,};
    double d;
    long l;

    grib_handle *h = grib_handle_new_from_message(0,const_cast<char*>(message),size);
    ASSERT(h);

    char mars_str [] = "mars";
    grib_keys_iterator* ks = grib_keys_iterator_new(h,GRIB_KEYS_ITERATOR_ALL_KEYS, mars_str);
    ASSERT(ks);

    while(grib_keys_iterator_next(ks))
    {
        const char* name = grib_keys_iterator_get_name(ks);

        len = sizeof(val);
        if(grib_keys_iterator_get_string(ks,val,&len) == 0) stringValues_[name] = val;
        len = 1;
        if(grib_keys_iterator_get_double(ks,&d,&len) == 0)       doubleValues_[name] = d;
        len = 1;
        if(grib_keys_iterator_get_long(ks,&l,&len) == 0)         longValues_[name] = l;

    }

#if 0
    const char *extra[] = {"editionNumber", "table2Version", "indicatorOfParameter", 0};
    int i  = 0;

    while(extra[i]) {
        long value;
        if(grib_get_long(h,extra[i],&value) == 0)
            longValues_[std::string(extra[i])] = value;
        i++;
    }
#endif

    grib_keys_iterator_delete(ks);
    grib_handle_delete(h);

}

GribHandle::~GribHandle() {
}

std::string GribHandle::substitute(const std::string& pattern) const
{
	return StringTools::substitute(pattern,stringValues_);
}

void GribHandle::getValue(const std::string& key,double& value)
{
    std::map<std::string,double>::iterator j = doubleValues_.find(key);
    if(j == doubleValues_.end()) throw eckit::UserError(std::string("GribHandle::getDouble failed for [") + key + "]");
    value = (*j).second;
}

void GribHandle::getValue(const std::string& key,long& value)
{
    std::map<std::string,long>::iterator j = longValues_.find(key);
    if(j == longValues_.end()) throw eckit::UserError(std::string("GribHandle::getLong failed for [") + key + "]");
    value = (*j).second;
}

void GribHandle::getValue(const std::string& key,std::string& value)
{
    std::map<std::string,std::string>::iterator j = stringValues_.find(key);
    if(j == stringValues_.end()) throw eckit::UserError(std::string("GribHandle::getString failed for [") + key + "]");
    value = (*j).second;
}

//----------------------------------------------------------------------------------------------------------------------

} // namespace grib