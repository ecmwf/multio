if(HAVE_MULTIO_SERVER)

list( APPEND _test_environment
    MULTIO_SERVER_PATH=${CMAKE_CURRENT_SOURCE_DIR}
)

ecbuild_add_test( TARGET test_multio_hammer_thread
                  COMMAND $<TARGET_FILE:multio-hammer>
                  ARGS --transport=thread --nbclients=5 --nbservers=3
                  ENVIRONMENT "${_test_environment}" )

ecbuild_add_test( TARGET      test_multio_hammer_mpi
                  COMMAND     $<TARGET_FILE:multio-hammer>
                  ARGS        --transport=mpi --nbclients=5 --nbservers=3
                  MPI         8
                  ENVIRONMENT "${_test_environment}" )

ecbuild_add_test( TARGET test_multio_hammer_tcp
                  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/tcp-launch.sh
                  ARGS $<TARGET_FILE:multio-hammer>
                  ENVIRONMENT "${_test_environment}" )

list( APPEND _test_environment
    FDB_DEBUG=1
    MULTIO_DEBUG=1
)

list( APPEND _hammer_test_data
  "single-field.grib"
)

ecbuild_get_test_multidata( TARGET multio_hammer_get_test_data NAMES ${_hammer_test_data} )

ecbuild_add_test( TARGET       test_multio_hammer_none
                  CONDITION    HAVE_FDB5
                  COMMAND      $<TARGET_FILE:multio-hammer>
                  TEST_DEPENDS multio_hammer_get_test_data
                  ARGS         ${_hammer_test_data} --transport=none
                  ENVIRONMENT  "${_test_environment}" )

list( APPEND _nemo_test_data
  "nemo.tar.gz"
)

ecbuild_get_test_multidata( EXTRACT TARGET multio_replay_get_test_data NAMES ${_nemo_test_data} )

ecbuild_add_test( TARGET       test_multio_replay_nemo
                  COMMAND      ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_DEPENDS multio_replay_get_test_data
                  ARGS         $<TARGET_FILE:multio-replay> $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT  "${_test_environment}" )

endif(HAVE_MULTIO_SERVER)
