if(HAVE_FDB5)

configure_file( etc/fdb/schema      ${CMAKE_BINARY_DIR}/multio/etc/fdb/schema      @ONLY )
configure_file( etc/fdb/config.yaml ${CMAKE_BINARY_DIR}/multio/etc/fdb/config.yaml @ONLY )

# This directory path is defined in ${CMAKE_BINARY_DIR}/multio/etc/fdb/config.yaml file
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/multio/tests/fdb/root )

endif(HAVE_FDB5)

include_directories( ${ECKIT_INCLUDE_DIRS} )

if( CMAKE_SYSTEM_NAME MATCHES "Linux" AND CMAKE_CXX_COMPILER_ID MATCHES "GNU" )
    list( APPEND _link_libraries
        -Wl,--no-as-needed
    )
endif()

list( APPEND _link_libraries
    multio
    multio-action-sink
)

if ( HAVE_FDB5 )
    list( APPEND _link_libraries
        multio-fdb5
    )
endif()

ecbuild_add_test( TARGET    test_multio_datasink_factory
                  SOURCES   test_multio_datasink_factory.cc
                  LIBS      ${_link_libraries} )

ecbuild_add_test( TARGET    test_multio_multio
                  SOURCES   test_multio_multio.cc TestDataContent.cc TestDataContent.h
                  LIBS      ${_link_libraries} )

ecbuild_add_test( TARGET    test_multio_file_sink
                  SOURCES   test_multio_file_sink.cc TestDataContent.cc TestDataContent.h
                  LIBS      ${_link_libraries} )

ecbuild_add_test( TARGET    test_multio_encode_bitspervalue
                  SOURCES   test_multio_encode_bitspervalue.cc
                  LIBS      ${_link_libraries} )

ecbuild_add_test( TARGET    test_multio_confctx
                  SOURCES   test_multio_confctx.cc
                  LIBS      multio
                  ENVIRONMENT "MULTIO_SERVER_CONFIG_PATH=${CMAKE_SOURCE_DIR}/multio/tests/server/config"
)

ecbuild_add_test( TARGET    test_multio_capi
                  SOURCES   test_multio_capi.cc
                  LIBS      multio-api
                  ENVIRONMENT "MULTIO_SERVER_CONFIG_PATH=${CMAKE_SOURCE_DIR}/multio/tests/server/config"
)

ecbuild_add_test( TARGET    test_multio_fapi_general
                  SOURCES   test_multio_fapi_general.f90
                  CONDITION HAVE_FORTRAN
                  LIBS      multio-fapi
                  ENVIRONMENT "MULTIO_SERVER_CONFIG_PATH=${CMAKE_SOURCE_DIR}/multio/tests/server/config"
)
ecbuild_add_test( TARGET    test_multio_maestro
                  SOURCES   test_multio_maestro.cc
                  CONDITION HAVE_MAESTRO
                  LIBS      multio-maestro )

list( APPEND _test_environment
    FDB_HOME=${CMAKE_BINARY_DIR}/multio
    FDB_DEBUG=1
    MULTIO_DEBUG=1
)

list( APPEND _test_data
    "rd.vod.grib"
)

ecbuild_get_test_multidata( TARGET multio_get_test_data
                            DIRNAME fdb5/tests/fdb
                            NAMES ${_test_data} )

ecbuild_add_test( TARGET test_multio_ifs_plans
	          CONDITION HAVE_FDB5
                  COMMAND $<TARGET_FILE:multio-feed>
                  TEST_DEPENDS multio_get_test_data
                  ARGS ${_test_data}
                  ENVIRONMENT "${_test_environment}" "MULTIO_PLANS={ \"plans\": [{ \"name\": \"test plans fdb5\", \"actions\": [{ \"type\": \"sink\", \"sinks\": [{\"type\": \"fdb5\"}]}]}]}" )

ecbuild_add_test( TARGET test_multio_ifs_io
                  CONDITION HAVE_FDB5
                  COMMAND $<TARGET_FILE:multio-feed>
                  TEST_DEPENDS multio_get_test_data
                  ARGS ${_test_data}
                  ENVIRONMENT "${_test_environment}" )

ecbuild_add_test( TARGET test_multio_subtoc
                  CONDITION HAVE_FDB5
                  COMMAND $<TARGET_FILE:multio-feed>
                  TEST_DEPENDS multio_get_test_data
                  ARGS ${_test_data} --test-subtoc
                  ENVIRONMENT "${_test_environment}" )

ecbuild_add_test( TARGET test_multio_legacy_interface
                  CONDITION HAVE_FDB5 AND HAVE_LEGACY_IFSIO
                  COMMAND $<TARGET_FILE:multio-legacy>
                  TEST_DEPENDS multio_get_test_data
                  ARGS ${_test_data}
                  ENVIRONMENT "${_test_environment}" )
