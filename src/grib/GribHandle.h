/*
 * (C) Copyright 1996-2013 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef grib_GribHandle_H
#define grib_GribHandle_H

#include "eckit/io/Buffer.h"
#include "eckit/memory/NonCopyable.h"
#include "eckit/serialisation/Stream.h"

namespace grib {

//----------------------------------------------------------------------------------------------------------------------

class GribHandle : private eckit::NonCopyable {

public: // methods

    GribHandle( eckit::Stream& );
    GribHandle( const eckit::Buffer&, size_t );

	~GribHandle();

	void getValue(const std::string& name,double& value);
	void getValue(const std::string& name,long& value);
	void getValue(const std::string& name,std::string& value);

	std::string substitute(const std::string& pattern) const;

	void print( std::ostream& ) const;

	friend std::ostream& operator<<(std::ostream& s,const GribHandle& p) { p.print(s); return s; }

private: // members

	std::map<std::string,std::string> stringValues_;
	std::map<std::string,long>   longValues_;
	std::map<std::string,double> doubleValues_;

};

//----------------------------------------------------------------------------------------------------------------------

} // namespace grib

#endif
