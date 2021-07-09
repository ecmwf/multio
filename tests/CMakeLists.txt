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

ecbuild_add_test( TARGET    test_multio_scoped_timer
                  SOURCES   test_multio_scoped_timer.cc
                  LIBS      multio )


list( APPEND _test_environment
    FDB_HOME=${CMAKE_BINARY_DIR}/multio
    FDB_DEBUG=1
    MULTIO_DEBUG=1
)

list( APPEND _test_data
  "rd.vod.grib"
)

ecbuild_add_executable( TARGET    multio-sink
                        SOURCES   multio-sink.cc ${CMAKE_CURRENT_SOURCE_DIR}/../src/multio/tools/MultioTool.cc
                        CONDITION HAVE_FDB5
                        LIBS      multio multio-fdb5 eckit_option)

ecbuild_add_executable( TARGET    multio-legacy
                        SOURCES   multio-legacy.cc ${CMAKE_CURRENT_SOURCE_DIR}/../src/multio/tools/MultioTool.cc
                        CONDITION HAVE_FDB5 AND HAVE_LEGACY_IFSIO
                        LIBS      multio multio-fdb5 eckit_option)

ecbuild_get_test_multidata( TARGET multio_get_test_data DIRNAME fdb5/tests/fdb NAMES ${_test_data} )

ecbuild_add_test( TARGET test_multio_ifs_io
                  CONDITION HAVE_FDB5
                  COMMAND $<TARGET_FILE:multio-sink>
                  TEST_DEPENDS multio_get_test_data
                  ARGS ${_test_data}
                  ENVIRONMENT "${_test_environment}" )

ecbuild_add_test( TARGET test_multio_subtoc
                  CONDITION HAVE_FDB5
                  COMMAND $<TARGET_FILE:multio-sink>
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
