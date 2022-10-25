include_directories( ${ECKIT_INCLUDE_DIRS} )

ecbuild_add_test( TARGET    test_multio_datasink_factory
                  SOURCES   test_multio_datasink_factory.cc
                  LIBS      multio )

ecbuild_add_test( TARGET    test_multio_multio
                  SOURCES   test_multio_multio.cc TestDataContent.cc TestDataContent.h
                  LIBS      multio )

ecbuild_add_test( TARGET    test_multio_file_sink
                  SOURCES   test_multio_file_sink.cc TestDataContent.cc TestDataContent.h
                  LIBS      multio )

ecbuild_add_test( TARGET    test_multio_encode_bitspervalue
                  SOURCES   test_multio_encode_bitspervalue.cc
                  LIBS      multio )

ecbuild_add_test( TARGET    test_multio_maestro
                  SOURCES   test_multio_maestro.cc
                  CONDITION HAVE_MAESTRO
                  LIBS      multio multio-maestro )

ecbuild_add_test( TARGET    test_multio_capi
                  SOURCES   test_multio_capi.cc
                  CONDITION HAVE_MULTIO_SERVER
                  LIBS      multio multio-server multio-api 
                  ENVIRONMENT "MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/server/config"
)

if( HAVE_FORTRAN )
    ecbuild_enable_fortran( REQUIRED MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/module)
    ecbuild_add_test( TARGET    test_multio_fapi_general
                      SOURCES   test_multio_fapi_general.f90
                      CONDITION HAVE_MULTIO_SERVER
                      LIBS      multio multio-server multio-api multio-fapi
                      ENVIRONMENT "MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/server/config"
    )
endif()

ecbuild_add_test( TARGET    test_multio_confctx
                  SOURCES   test_multio_confctx.cc
                  CONDITION HAVE_MULTIO_SERVER
                  LIBS      multio 
                  ENVIRONMENT "MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/server/config"
)


list( APPEND _test_environment
    FDB_HOME=${CMAKE_BINARY_DIR}/multio
    FDB_DEBUG=1
    MULTIO_DEBUG=1
)

list( APPEND _test_data
    "rd.vod.grib"
)

if( CMAKE_SYSTEM_NAME MATCHES "Linux" AND CMAKE_CXX_COMPILER_ID MATCHES "GNU" )
    list( APPEND _link_libraries
        -Wl,--no-as-needed
    )
endif()

list( APPEND _link_libraries
    multio
    multio-fdb5
    eckit_option
)

ecbuild_add_executable( TARGET    multio-legacy
                        SOURCES   multio-legacy.cc
                                  ${CMAKE_CURRENT_SOURCE_DIR}/../src/multio/tools/MultioTool.cc
                        CONDITION HAVE_FDB5 AND HAVE_LEGACY_IFSIO
                        LIBS      ${_link_libraries})

ecbuild_add_executable( TARGET    multio-legacy-hammer
                        SOURCES   multio-legacy-hammer.cc
                                  ${CMAKE_CURRENT_SOURCE_DIR}/../src/multio/tools/MultioTool.cc
                        CONDITION HAVE_FDB5 AND HAVE_LEGACY_IFSIO
                        LIBS      ${_link_libraries})

ecbuild_get_test_multidata( TARGET multio_get_test_data DIRNAME fdb5/tests/fdb NAMES ${_test_data} )

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

add_subdirectory(multio)
add_subdirectory(server)
