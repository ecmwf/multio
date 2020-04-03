if(HAVE_FDB5)

configure_file( etc/fdb/schema   ${CMAKE_BINARY_DIR}/multio/etc/fdb/schema    @ONLY )
configure_file( etc/fdb/roots    ${CMAKE_BINARY_DIR}/multio/etc/fdb/roots     @ONLY )
configure_file( etc/fdb/spaces   ${CMAKE_BINARY_DIR}/multio/etc/fdb/spaces    @ONLY )

# This directory path is defined in ${CMAKE_BINARY_DIR}/multio/etc/fdb/roots file
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/multio/tests/fdb/root )

endif(HAVE_FDB5)
