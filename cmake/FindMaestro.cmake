# (C) Copyright 2020- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# Try to find libmaestro

# The following paths will be searched, both in the environment and as CMake variables:
#
#  MAESTRO_ROOT
#  MAESTRO_DIR
#  MAESTRO_PATH

# If found, the maestro target will be created.

find_path(MAESTRO_INCLUDE_DIR
    NAMES maestro-core/maestro.h
    HINTS
        ${MAESTRO_ROOT}
        ${MAESTRO_DIR}
        ${MAESTRO_PATH}
        ENV MAESTRO_ROOT
        ENV MAESTRO_DIR
        ENV MAESTRO_PATH
    PATH_SUFFIXES include
)

set( MAESTRO_INCLUDE_DIR ${MAESTRO_INCLUDE_DIR}/maestro-core )

find_library(MAESTRO_LIBRARY
    NAMES maestro
    HINTS
        ${MAESTRO_ROOT}
        ${MAESTRO_DIR}
        ${MAESTRO_PATH}
        ENV MAESTRO_ROOT
        ENV MAESTRO_DIR
        ENV MAESTRO_PATH
    PATH_SUFFIXES lib lib64
)

set( MAESTRO_LIBRARIES    ${MAESTRO_LIBRARY} )
set( MAESTRO_INCLUDE_DIRS ${MAESTRO_INCLUDE_DIR} )

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Maestro DEFAULT_MSG MAESTRO_LIBRARY MAESTRO_INCLUDE_DIR)

mark_as_advanced(MAESTRO_INCLUDE_DIR MAESTRO_LIBRARY)

if(Maestro_FOUND)
    add_library(maestro UNKNOWN IMPORTED GLOBAL)
    set_target_properties(maestro PROPERTIES
        IMPORTED_LOCATION ${MAESTRO_LIBRARY}
        INTERFACE_INCLUDE_DIRECTORIES ${MAESTRO_INCLUDE_DIR}
    )
endif()
