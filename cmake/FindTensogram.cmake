# (C) Copyright 2025- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# Try to find the Tensogram library (N-dimensional tensor message format)
#
# Tensogram is a Rust-core library with a C FFI layer and a C++ header-only wrapper.
# This module locates the pre-built static library and the required headers.
#
# The following paths will be searched, both in the environment and as CMake variables:
#
#   TENSOGRAM_ROOT
#   TENSOGRAM_DIR
#   TENSOGRAM_PATH
#
# If found, the tensogram::tensogram imported target will be created.
#
# Output variables:
#   tensogram_FOUND           - True if tensogram was found
#   TENSOGRAM_INCLUDE_DIRS    - Include directories (C++ wrapper + C FFI header)
#   TENSOGRAM_LIBRARIES       - Libraries to link against

# --- Locate the C++ header-only wrapper: tensogram.hpp ---

find_path(TENSOGRAM_CPP_INCLUDE_DIR
    NAMES tensogram.hpp
    HINTS
        ${TENSOGRAM_ROOT}
        ${TENSOGRAM_DIR}
        ${TENSOGRAM_PATH}
        ENV TENSOGRAM_ROOT
        ENV TENSOGRAM_DIR
        ENV TENSOGRAM_PATH
    PATH_SUFFIXES include
)

# --- Locate the C FFI header: tensogram.h ---
# This is typically in a different include path (crates/tensogram-ffi/)

find_path(TENSOGRAM_FFI_INCLUDE_DIR
    NAMES tensogram.h
    HINTS
        ${TENSOGRAM_ROOT}
        ${TENSOGRAM_DIR}
        ${TENSOGRAM_PATH}
        ENV TENSOGRAM_ROOT
        ENV TENSOGRAM_DIR
        ENV TENSOGRAM_PATH
    PATH_SUFFIXES include crates/tensogram-ffi
)

# --- Locate the Rust static library: libtensogram_ffi.a ---

find_library(TENSOGRAM_LIBRARY
    NAMES tensogram_ffi
    HINTS
        ${TENSOGRAM_ROOT}
        ${TENSOGRAM_DIR}
        ${TENSOGRAM_PATH}
        ENV TENSOGRAM_ROOT
        ENV TENSOGRAM_DIR
        ENV TENSOGRAM_PATH
    PATH_SUFFIXES lib lib64 target/release
)

# --- Aggregate results ---

set(TENSOGRAM_INCLUDE_DIRS ${TENSOGRAM_CPP_INCLUDE_DIR} ${TENSOGRAM_FFI_INCLUDE_DIR})
set(TENSOGRAM_LIBRARIES    ${TENSOGRAM_LIBRARY})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(tensogram
    DEFAULT_MSG
    TENSOGRAM_LIBRARY
    TENSOGRAM_CPP_INCLUDE_DIR
    TENSOGRAM_FFI_INCLUDE_DIR
)

mark_as_advanced(TENSOGRAM_CPP_INCLUDE_DIR TENSOGRAM_FFI_INCLUDE_DIR TENSOGRAM_LIBRARY)

# --- Create imported target ---

if(tensogram_FOUND AND NOT TARGET tensogram::tensogram)

    # The Rust static library (imported)
    add_library(tensogram_ffi STATIC IMPORTED GLOBAL)
    set_target_properties(tensogram_ffi PROPERTIES
        IMPORTED_LOCATION "${TENSOGRAM_LIBRARY}"
    )

    # Header-only C++ wrapper (INTERFACE) linking the Rust lib + platform libs
    add_library(tensogram::tensogram INTERFACE IMPORTED GLOBAL)
    set_target_properties(tensogram::tensogram PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${TENSOGRAM_CPP_INCLUDE_DIR};${TENSOGRAM_FFI_INCLUDE_DIR}"
    )
    target_link_libraries(tensogram::tensogram INTERFACE tensogram_ffi)

    # Platform-specific system libraries required by the Rust static library
    if(APPLE)
        target_link_libraries(tensogram::tensogram INTERFACE
            "-framework CoreFoundation"
            "-framework Security"
            "-framework SystemConfiguration"
            "-lc++"
            "-lm"
        )
    elseif(UNIX)
        target_link_libraries(tensogram::tensogram INTERFACE
            dl
            pthread
            m
            stdc++
        )
    endif()

endif()
