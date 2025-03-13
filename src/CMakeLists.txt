add_subdirectory( multio )

if ( BUILD_MULTIOM )
    if (NOT CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        message(STATUS "Building multiom")
        add_subdirectory( multiom )
        add_subdirectory( multio/action/encode-grib2 )
    else ()
        message(WARNING "Skipping multiom because the GNU Fortran compiler is too slow.")
    endif()
endif()

# Include api to defer dependencies
add_subdirectory( multio/api )
