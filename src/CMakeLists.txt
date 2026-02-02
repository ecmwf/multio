add_subdirectory( multio )

if ( BUILD_MULTIOM )
    add_subdirectory( multiom )
endif()

# Include api to defer dependencies
add_subdirectory( multio/api )
