add_subdirectory( multio )

if ( BUILD_MULTIOM )
    add_subdirectory( multiom )
    add_subdirectory( multio/action/encode-mtg2 )
endif()

# Include api to defer dependencies
add_subdirectory( multio/api )
