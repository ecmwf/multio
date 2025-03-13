add_subdirectory( multio )

if ( BUILD_MULTIOM )
    add_subdirectory( multiom )
    add_subdirectory( multio/action/encode-grib2 )
endif()

# Include api to defer dependencies
add_subdirectory( multio/api )
