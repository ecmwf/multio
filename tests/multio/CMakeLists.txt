if(HAVE_FDB5)

configure_file( etc/fdb/schema      ${CMAKE_BINARY_DIR}/multio/etc/fdb/schema      @ONLY )
configure_file( etc/fdb/config.yaml ${CMAKE_BINARY_DIR}/multio/etc/fdb/config.yaml @ONLY )

# This directory path is defined in ${CMAKE_BINARY_DIR}/multio/etc/fdb/config.yaml file
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/multio/tests/fdb/root )

endif(HAVE_FDB5)
