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


ecbuild_add_test( TARGET    test_multio_metadata_parametrization
                  TEST_REQUIRES test_multio_metadata
                  SOURCES   test_multio_metadata_parametrization.cc
                  LIBS      multio )

ecbuild_add_test( TARGET    test_multio_metadata_mapping
                  TEST_REQUIRES test_multio_metadata
                  SOURCES   test_multio_metadata_mapping.cc
                  NO_AS_NEEDED
                  LIBS      multio multio-action-metadata-mapping multio-action-debug-sink multio-action-print
                  ENVIRONMENT "MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/config" )

ecbuild_add_test( TARGET    test_multio_select
                  SOURCES   test_multio_select.cc
                  LIBS      multio )


# Test Data modelling

ecbuild_add_test( TARGET    test_multio_datamod
                  TEST_REQUIRES test_multio_metadata
                  SOURCES   test_multio_datamod.cc
                  LIBS      multio )

ecbuild_add_test( TARGET    test_multio_datamod_containers
                  TEST_REQUIRES test_multio_metadata
                  SOURCES   test_multio_datamod_containers.cc
                  LIBS      multio )

ecbuild_add_test( TARGET    test_multio_datamod_models
                  TEST_REQUIRES test_multio_metadata
                  SOURCES   test_multio_datamod_models.cc
                  LIBS      multio )


# Test Config Parser

ecbuild_add_test( TARGET    test_multio_config_parser
                  SOURCES   test_multio_config_parser.cc
                  LIBS      multio )


# Test ring buffer

ecbuild_add_test( TARGET    test_multio_ringbuffer
                  SOURCES   test_multio_ringbuffer.cc
                  CONDITION HAVE_MULTIO_SERVER_MEMORY_PROFILE OR HAVE_MULTIO_CLIENT_MEMORY_PROFILE
                  LIBS      multio )

# Test tracing

ecbuild_add_test( TARGET    test_multio_tracing
                  SOURCES   test_multio_tracing.cc
                  CONDITION HAVE_MULTIO_SERVER_MEMORY_PROFILE OR HAVE_MULTIO_CLIENT_MEMORY_PROFILE
                  LIBS      multio )

# Test api

list( APPEND _api_test_environment
    CMAKE_BINARY_HOME=${CMAKE_BINARY_DIR}
    MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/config
)


ecbuild_add_test( TARGET      test_multio_capi
                  SOURCES     test_multio_capi.cc
                  LIBS        multio-api
                  ENVIRONMENT "${_api_test_environment}"
)

ecbuild_add_test( TARGET      test_multio_fapi_general
                  SOURCES     test_multio_fapi_general.f90
                  CONDITION   HAVE_FORTRAN
                  LIBS        multio-fapi
                  ENVIRONMENT "MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/config"
)

# Test maestro

ecbuild_add_test( TARGET    test_multio_maestro
                  SOURCES   test_multio_maestro.cc
                  CONDITION HAVE_MAESTRO
                  LIBS      multio-maestro )

if(HAVE_MULTIO_BUILD_TOOLS)

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
                            DIRNAME fdb5/test-data/tests/fdb
                            NAMES ${_test_data} )

ecbuild_add_test( TARGET test_multio_ifs_plans
                  CONDITION HAVE_FDB5
                  COMMAND $<TARGET_FILE:multio-feed>
                  TEST_REQUIRES multio_get_test_data
                  ARGS ${_test_data}
                  ENVIRONMENT "${_test_environment}" "MULTIO_PLANS={ \"plans\": [{ \"name\": \"test plans fdb5\", \"actions\": [{ \"type\": \"sink\", \"sinks\": [{\"type\": \"fdb5\"}]}]}]}" )

ecbuild_add_test( TARGET test_multio_ifs_io
                  CONDITION HAVE_FDB5
                  COMMAND $<TARGET_FILE:multio-feed>
                  TEST_REQUIRES multio_get_test_data test_multio_ifs_plans
                  ARGS ${_test_data}
                  ENVIRONMENT "${_test_environment}" )

ecbuild_add_test( TARGET test_multio_subtoc
                  CONDITION HAVE_FDB5
                  COMMAND $<TARGET_FILE:multio-feed>
                  TEST_REQUIRES multio_get_test_data test_multio_ifs_io
                  ARGS ${_test_data} --test-subtoc
                  ENVIRONMENT "${_test_environment}" )

ecbuild_add_test( TARGET test_multio_legacy_interface
                  CONDITION HAVE_FDB5 AND HAVE_LEGACY_IFSIO
                  COMMAND $<TARGET_FILE:multio-legacy>
                  TEST_REQUIRES multio_get_test_data test_multio_subtoc
                  ARGS ${_test_data}
                  ENVIRONMENT "${_test_environment}" )


list( APPEND _test_environment
    MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/config
)

ecbuild_add_test( TARGET test_multio_hammer_thread
                  COMMAND $<TARGET_FILE:multio-hammer>
                  ARGS --transport=thread --nbclients=5 --nbservers=3
                  ENVIRONMENT "${_test_environment}" )

ecbuild_add_test( TARGET test_multio_hammer_tcp
                  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/tcp-launch.sh
                  ARGS $<TARGET_FILE:multio-hammer>
                  TEST_REQUIRES test_multio_hammer_thread 
                  ENVIRONMENT "${_test_environment}" )

ecbuild_add_test( TARGET      test_multio_hammer_mpi
                  CONDITION   eckit_HAVE_MPI
                  COMMAND     $<TARGET_FILE:multio-hammer>
                  ARGS        --transport=mpi --nbclients=5 --nbservers=3
                  MPI         8
                  TEST_PROPERTIES RESOURCE_LOCK mpi
                  TEST_REQUIRES test_multio_hammer_tcp 
                  ENVIRONMENT "${_test_environment}" )


list( APPEND _hammer_test_data
  "single-field.grib"
)

ecbuild_get_test_multidata( TARGET multio_hammer_get_test_data
                            DIRNAME multio/test-data/tests/server
                            NAMES ${_hammer_test_data} )

ecbuild_add_test( ENABLED       OFF
                  TARGET        test_multio_hammer_none
                  CONDITION     HAVE_FDB5
                  COMMAND       $<TARGET_FILE:multio-hammer>
                  TEST_REQUIRES multio_hammer_get_test_data
                  ARGS          ${_hammer_test_data} --transport=none
                  ENVIRONMENT   "${_test_environment}" )



if (eckit_HAVE_MPI)

if (NOT DEFINED ${MPIEXEC_EXECUTABLE})
  set(MPIEXEC_EXECUTABLE mpiexec)
endif()

list( APPEND _nemo_test_data
  "nemo.tar.gz"
)

ecbuild_get_test_multidata( EXTRACT TARGET multio_replay_get_test_data
                            DIRNAME multio/test-data/tests/server
                            NAMES ${_nemo_test_data}
)

ecbuild_add_test( TARGET          test_multio_replay_nemo_capi_initfilepath_empi
                  COMMAND         ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_REQUIRES   multio_replay_get_test_data
                  ARGS            "$<TARGET_FILE:multio-replay-nemo-capi> --init-mpi-external=1 --config-file=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-server.yaml"
                                  $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT     "${_test_environment}"
                  TEST_PROPERTIES RESOURCE_LOCK mpi
)

ecbuild_add_test( TARGET          test_multio_replay_nemo_capi_initdefault_empi
                  COMMAND         ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_REQUIRES   multio_replay_get_test_data
                  ARGS            "$<TARGET_FILE:multio-replay-nemo-capi> --init-mpi-external=1"
                                  $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT     "${_test_environment}"
                  TEST_PROPERTIES RESOURCE_LOCK mpi
)

ecbuild_add_test( TARGET          test_multio_replay_nemo_capi_initdefault_impi1
                  COMMAND         ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_REQUIRES   multio_replay_get_test_data
                  ARGS            $<TARGET_FILE:multio-replay-nemo-capi>
                                  $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT     "${_test_environment}"
                  TEST_PROPERTIES RESOURCE_LOCK mpi
)

ecbuild_add_test( TARGET          test_multio_replay_nemo_capi_initdefault_impi2
                  COMMAND         ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_REQUIRES   multio_replay_get_test_data
                  ARGS            $<TARGET_FILE:multio-replay-nemo-capi>
                                  $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT     "${_test_environment}"
                                  "MULTIO_SERVER_CONFIG_7FILE=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-server-mpi-default-splitting.yaml"
                  TEST_PROPERTIES RESOURCE_LOCK mpi
)

ecbuild_add_test( TARGET          test_multio_replay_nemo_capi_initdefault_impi3
                  COMMAND         ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_REQUIRES   multio_replay_get_test_data
                  ARGS            "$<TARGET_FILE:multio-replay-nemo-capi> --mpi-group=custom"
                                  $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT     "${_test_environment}"
                                  "MULTIO_SERVER_CONFIG_FILE=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-server-mpi-custom-splitting.yaml"
                  TEST_PROPERTIES RESOURCE_LOCK mpi
)

ecbuild_add_test( TARGET          test_multio_replay_nemo_capi_initdefault_passdown_mpi
                  COMMAND         ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_REQUIRES   multio_replay_get_test_data
                  ARGS            "$<TARGET_FILE:multio-replay-nemo-capi> --pass-down-mpi-comm=1"
                                  $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT     "${_test_environment}"
                  TEST_PROPERTIES RESOURCE_LOCK mpi
)

ecbuild_add_test( TARGET          test_multio_replay_nemo_fapi
                  CONDITION       HAVE_FORTRAN
                  COMMAND         ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_REQUIRES   multio_replay_get_test_data
                  ARGS            $<TARGET_FILE:multio-replay-nemo-fapi>
                                  $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT     "${_test_environment}"
                  TEST_PROPERTIES RESOURCE_LOCK mpi
)

ecbuild_add_test( TARGET          test_multio_replay_nemo_capi_masked
                  COMMAND         ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_REQUIRES   multio_replay_get_test_data
                  ARGS            "$<TARGET_FILE:multio-replay-nemo-capi> --send-masks=1"
                                  $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT     "${_test_environment}"
                                  "MULTIO_SERVER_CONFIG_FILE=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-server-masked.yaml"
                  TEST_PROPERTIES RESOURCE_LOCK mpi
)

ecbuild_add_test( TARGET          test_multio_replay_nemo_capi_partial_agg
                  COMMAND         ${CMAKE_CURRENT_SOURCE_DIR}/replay-nemo.sh
                  TEST_REQUIRES   multio_replay_get_test_data
                  ARGS            $<TARGET_FILE:multio-replay-nemo-capi-partial-agg>
                                  $<TARGET_FILE:multio-probe> ${MPIEXEC_EXECUTABLE} ${MPI_ARGS}
                  ENVIRONMENT     "${_test_environment}"
                                  "MULTIO_SERVER_CONFIG_FILE=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-server-test-partial-agg.yaml"
                  TEST_PROPERTIES RESOURCE_LOCK mpi
)


# Synchronization

ecbuild_add_executable( TARGET    test_multio_synchronize
                        SOURCES   test_multio_synchronize.cc
                        LIBS      multio-api
                        NOINSTALL
)

ecbuild_add_test( TARGET          test_multio_synchronize_fail_no_mpi
                  COMMAND         ${CMAKE_CURRENT_BINARY_DIR}/test_multio_synchronize
                  ENVIRONMENT     "MULTIO_SERVER_CONFIG_FILE=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-simple-client-server.yaml"
                  TEST_PROPERTIES  WILL_FAIL TRUE
)

ecbuild_add_test( TARGET          test_multio_synchronize_fail_no_sync
                  COMMAND         ${CMAKE_CURRENT_SOURCE_DIR}/expect-timeout.sh
                  ARGS            2 ${MPIEXEC_EXECUTABLE} -n 3 ${CMAKE_CURRENT_BINARY_DIR}/test_multio_synchronize : -n 1 ${CMAKE_CURRENT_BINARY_DIR}/test_multio_synchronize --skip-sync : -n 4 ${CMAKE_BINARY_DIR}/bin/multio-probe --test
                  ENVIRONMENT     "MULTIO_SERVER_CONFIG_FILE=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-simple-client-server.yaml"
                                  ECKIT_MPI_INIT_THREAD=MPI_THREAD_MULTIPLE
                  TEST_PROPERTIES RESOURCE_LOCK mpi
)

ecbuild_add_test( TARGET          test_multio_synchronize_client_only
                  COMMAND         ${MPIEXEC_EXECUTABLE}
                  ARGS            -n 1 ${CMAKE_CURRENT_BINARY_DIR}/test_multio_synchronize
                  ENVIRONMENT     "MULTIO_SERVER_CONFIG_FILE=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-simple-client-server.yaml"
                                  ECKIT_MPI_INIT_THREAD=MPI_THREAD_MULTIPLE
                  TEST_PROPERTIES RESOURCE_LOCK mpi
                                  TIMEOUT       5
)

ecbuild_add_test( TARGET          test_multio_synchronize_2_clients
                  COMMAND         ${MPIEXEC_EXECUTABLE}
                  ARGS            -n 2 ${CMAKE_CURRENT_BINARY_DIR}/test_multio_synchronize
                  ENVIRONMENT     "MULTIO_SERVER_CONFIG_FILE=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-simple-client-server.yaml"
                                  ECKIT_MPI_INIT_THREAD=MPI_THREAD_MULTIPLE
                  TEST_PROPERTIES RESOURCE_LOCK mpi
                                  TIMEOUT       5
)

foreach (N_CLIENTS 1 4)
foreach (N_SERVERS 1 4)
ecbuild_add_test( TARGET          test_multio_synchronize_${N_CLIENTS}_clients_${N_SERVERS}_servers
                  COMMAND         ${MPIEXEC_EXECUTABLE}
                  ARGS            -n ${N_CLIENTS} ${CMAKE_CURRENT_BINARY_DIR}/test_multio_synchronize : -n ${N_SERVERS} ${CMAKE_BINARY_DIR}/bin/multio-probe --test
                  ENVIRONMENT     "MULTIO_SERVER_CONFIG_FILE=${CMAKE_CURRENT_SOURCE_DIR}/config/multio-simple-client-server.yaml"
                                  ECKIT_MPI_INIT_THREAD=MPI_THREAD_MULTIPLE
                  TEST_PROPERTIES RESOURCE_LOCK mpi
                                  TIMEOUT       5
)
endforeach()
endforeach()

endif (eckit_HAVE_MPI)


# Install files
add_subdirectory(config)

add_subdirectory(action)
add_subdirectory(mars2mars)

endif (HAVE_MULTIO_BUILD_TOOLS)
