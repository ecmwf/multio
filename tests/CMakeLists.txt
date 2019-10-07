include_directories( ${ECKIT_INCLUDE_DIRS} )
include_directories( ${ATLAS_INCLUDE_DIRS} )

ecbuild_add_test( TARGET    test_multio_datasink_factory
                  SOURCES   test_multio_datasink_factory.cc
                  LIBS      multio )

ecbuild_add_test( TARGET    test_multio_multio
                  SOURCES   test_multio_multio.cc
                  LIBS      multio )

ecbuild_add_test( TARGET    test_multio_file_sink
                  SOURCES   test_multio_file_sink.cc
                  LIBS      multio )

list( APPEND _test_environment
    FDB_DEBUG=1
    MULTIO_TRACE=1
)

list( APPEND _test_data
  "rd.vod.grib"
)

ecbuild_get_test_multidata( TARGET multio_get_test_data DIRNAME fdb5/tests/fdb NAMES ${_test_data} )

ecbuild_add_test( TARGET test_multio_ifs_io
                  CONDITION HAVE_FDB5
                  COMMAND $<TARGET_FILE:multio-sink>
                  TEST_DEPENDS multio_get_test_data
                  ARGS ${_test_data}
                  ENVIRONMENT "${_test_environment}" )

add_subdirectory(attic)
