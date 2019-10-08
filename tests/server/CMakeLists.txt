if(HAVE_MULTIO_SERVER)

list( APPEND _test_environment
    MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/configs/
)

ecbuild_add_test( TARGET test_multio_hammer_thread
                  COMMAND $<TARGET_FILE:multio-hammer>
                  ARGS --transport=thread --nbclients=5 --nbservers=3
                  ENVIRONMENT "${_test_environment}" )

ecbuild_add_test( TARGET test_multio_hammer_tcp
                  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/tcp_test.sh
                  ARGS $<TARGET_FILE:multio-hammer>
                  ENVIRONMENT "${_test_environment}" )

ecbuild_add_test( TARGET      test_multio_hammer_mpi
                  COMMAND     $<TARGET_FILE:multio-hammer>
                  ARGS        --transport=mpi --nbclients=5 --nbservers=3
                  MPI         8
                  ENVIRONMENT "${_test_environment}" )

list( APPEND _test_environment
    FDB_DEBUG=1
)

list( APPEND _server_test_data
  "single-field.grib"
)

ecbuild_get_test_multidata( TARGET multio_server_get_test_data NAMES ${_server_test_data} )

ecbuild_add_test( TARGET       test_multio_hammer_none
                  CONDITION    HAVE_FDB5
                  COMMAND      $<TARGET_FILE:multio-hammer>
                  TEST_DEPENDS multio_server_get_test_data
                  ARGS         ${_server_test_data} --transport=none
                  ENVIRONMENT  "${_test_environment}" )

endif(HAVE_MULTIO_SERVER)
