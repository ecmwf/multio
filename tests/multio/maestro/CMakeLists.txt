
set ( PREFIX test_multio_maestro )

ecbuild_add_test(
    TARGET    ${PREFIX}_api
    SOURCES   test_maestro_api.cc
    LIBS      multio-maestro
)


# Maestro in sink action

list( APPEND _test_environment
    COMPONENT_NAME=MultIO_TEST_COMPONENT
    MSTRO_WORKFLOW_NAME=MultIO_TEST_WORKFLOW
    MULTIO_DEBUG=1
)

ecbuild_add_test(
    TARGET ${PREFIX}_sink_init
    SOURCES test_maestro_sink_init.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-sink
    ENVIRONMENT "${_test_environment}"
)

ecbuild_add_test(
    TARGET ${PREFIX}_sink_msg
    COMMAND multio-feed
    ARGS "${CMAKE_CURRENT_SOURCE_DIR}/../test.grib"
    ENVIRONMENT "${_test_environment}" "MULTIO_PLANS={ \"plans\": [{ \"name\": \"test plan maestro\", \"actions\": [{ \"type\": \"sink\", \"sinks\": [{\"type\": \"maestro\", \"ready-cdo\": false }]}]}]}"
)

# TODO: Add test with single consumer
