if(HAVE_FDB5)

configure_file( etc/fdb/schema      ${CMAKE_BINARY_DIR}/multio/etc/fdb/schema      @ONLY )
configure_file( etc/fdb/config.yaml ${CMAKE_BINARY_DIR}/multio/etc/fdb/config.yaml @ONLY )

# This directory path is defined in ${CMAKE_BINARY_DIR}/multio/etc/fdb/config.yaml file
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/multio/tests/fdb/root )

endif(HAVE_FDB5)

# Test sinks


ecbuild_add_test( TARGET    test_multio_datasink_factory
                  SOURCES   test_multio_datasink_factory.cc
                  NO_AS_NEEDED
                  LIBS      multio-action-sink )

ecbuild_add_test( TARGET    test_multio_multio
                  SOURCES   test_multio_multio.cc TestDataContent.cc TestDataContent.h
                  NO_AS_NEEDED
                  LIBS      multio-action-sink )

ecbuild_add_test( TARGET    test_multio_file_sink
                  SOURCES   test_multio_file_sink.cc TestDataContent.cc TestDataContent.h
                  NO_AS_NEEDED
                  LIBS      multio-action-sink )

# Test bits-per-value

ecbuild_add_test( TARGET    test_multio_encode_bitspervalue
                  SOURCES   test_multio_encode_bitspervalue.cc
                  NO_AS_NEEDED
                  LIBS      multio-action-sink )

# Test config

ecbuild_add_test( TARGET    test_multio_conf
                  SOURCES   test_multio_conf.cc
                  LIBS      multio
                  ENVIRONMENT "MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/config" )

# Test Mask compression

ecbuild_add_test( TARGET    test_multio_mask_compression
                  SOURCES   test_multio_mask_compression.cc
                  LIBS      multio )


# Test Metadata

ecbuild_add_test( TARGET    test_multio_metadata
                  SOURCES   test_multio_metadata.cc
                  LIBS      multio )

# Test api

list( APPEND _api_test_environment
    CMAKE_BINARY_HOME=${CMAKE_BINARY_DIR}
    MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/config
)


ecbuild_add_test( TARGET    test_multio_capi
                  SOURCES   test_multio_capi.cc
                  LIBS      multio-api
                  ENVIRONMENT "${_api_test_environment}"
)

ecbuild_add_test( TARGET    test_multio_fapi_general
                  SOURCES   test_multio_fapi_general.f90
                  CONDITION HAVE_FORTRAN
                  LIBS      multio-fapi
                  ENVIRONMENT "MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/config"
)

# Test maestro

ecbuild_add_test( TARGET    test_multio_maestro
                  SOURCES   test_multio_maestro.cc
                  CONDITION HAVE_MAESTRO
                  LIBS      multio-maestro )

# Test api

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


list( APPEND _test_environment
    MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/config
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

list( APPEND _hammer_test_data
  "single-field.grib"
)

ecbuild_get_test_multidata( TARGET multio_hammer_get_test_data
                            DIRNAME multio/tests/server
                            NAMES ${_hammer_test_data} )

ecbuild_add_test( ENABLED      OFF
                  TARGET       test_multio_hammer_none
                  CONDITION    HAVE_FDB5
                  COMMAND      $<TARGET_FILE:multio-hammer>
                  TEST_DEPENDS multio_hammer_get_test_data
                  ARGS         ${_hammer_test_data} --transport=none
                  MPI          1
                  ENVIRONMENT  "${_test_environment}" )

list( APPEND _nemo_test_data
  "nemo.tar.gz"
)

ecbuild_get_test_multidata( EXTRACT TARGET multio_replay_get_test_data
                            DIRNAME multio/tests/server
                            NAMES ${_nemo_test_data}
)

ecbuild_add_test( TARGET       test_multio_replay_nemo_capi_initfilepath_empi
                  COMMAND      ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_DEPENDS multio_replay_get_test_data
                  ARGS         "$<TARGET_FILE:multio-replay-nemo-capi> --init-mpi-external=1 --config-file=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-server.yaml"
                               $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT  "${_test_environment}"
)

ecbuild_add_test( TARGET       test_multio_replay_nemo_capi_initdefault_empi
                  COMMAND      ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_DEPENDS multio_replay_get_test_data
                  ARGS         "$<TARGET_FILE:multio-replay-nemo-capi> --init-mpi-external=1"
                               $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT  "${_test_environment}"
)

ecbuild_add_test( TARGET       test_multio_replay_nemo_capi_initdefault_impi1
                  COMMAND      ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_DEPENDS multio_replay_get_test_data
                  ARGS         $<TARGET_FILE:multio-replay-nemo-capi>
                               $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT  "${_test_environment}"
)

ecbuild_add_test( TARGET       test_multio_replay_nemo_capi_initdefault_impi2
                  COMMAND      ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_DEPENDS multio_replay_get_test_data
                  ARGS         $<TARGET_FILE:multio-replay-nemo-capi>
                               $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT  "${_test_environment}"
                               "MULTIO_SERVER_CONFIG_7FILE=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-server-mpi-default-splitting.yaml"
)

ecbuild_add_test( TARGET       test_multio_replay_nemo_capi_initdefault_impi3
                  COMMAND      ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_DEPENDS multio_replay_get_test_data
                  ARGS         "$<TARGET_FILE:multio-replay-nemo-capi> --mpi-group=custom"
                               $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT  "${_test_environment}"
                               "MULTIO_SERVER_CONFIG_FILE=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-server-mpi-custom-splitting.yaml"
)

ecbuild_add_test( TARGET       test_multio_replay_nemo_capi_initdefault_passdown_mpi
                  COMMAND      ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_DEPENDS multio_replay_get_test_data
                  ARGS         "$<TARGET_FILE:multio-replay-nemo-capi> --pass-down-mpi-comm=1"
                               $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT  "${_test_environment}"
)

ecbuild_add_test( TARGET       test_multio_replay_nemo_fapi
                  CONDITION    HAVE_FORTRAN
                  COMMAND      ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_DEPENDS multio_replay_get_test_data
                  ARGS         $<TARGET_FILE:multio-replay-nemo-fapi>
                               $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT  "${_test_environment}"
)

ecbuild_add_test( TARGET       test_multio_replay_nemo_capi_masked
                  COMMAND      ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_DEPENDS multio_replay_get_test_data
                  ARGS         "$<TARGET_FILE:multio-replay-nemo-capi> --send-masks=1"
                               $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT  "${_test_environment}"
                               "MULTIO_SERVER_CONFIG_FILE=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-server-masked.yaml"
)

ecbuild_add_test( TARGET       test_multio_replay_nemo_capi_partial_agg
                  COMMAND      ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_DEPENDS multio_replay_get_test_data
                  ARGS         $<TARGET_FILE:multio-replay-nemo-capi-partial-agg>
                               $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT  "${_test_environment}"
                               "MULTIO_SERVER_CONFIG_FILE=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-server-test-partial-agg.yaml"
)


# Install files
add_subdirectory(config)
