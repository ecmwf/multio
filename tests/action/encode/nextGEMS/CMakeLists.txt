list( APPEND _action_nextGEMS_test_templates
    "nextgems_test_templates.tar.gz"
)
list( APPEND _action_nextGEMS_test_input
    "nextgems_test_data.tar.gz"
)



set(TEST_NEXTGEMS_PATH ${multio_BINARY_DIR}/tests/action/encode/nextGEMS )

set(TEST_NEXTGEMS_OUTPUT_PATH ${TEST_NEXTGEMS_PATH}/output )

set(TEST_NEXTGEMS_TEMPLATES_PATH ${TEST_NEXTGEMS_PATH}/templates )
file(MAKE_DIRECTORY ${TEST_NEXTGEMS_TEMPLATES_PATH})
ecbuild_get_test_multidata( EXTRACT 
    TARGET multio_action_nextGEMS_get_test_templates 
    DIRNAME multio/tests/actions
    DIRLOCAL ${TEST_NEXTGEMS_TEMPLATES_PATH}
    NAMES ${_action_nextGEMS_test_templates} )

set(TEST_NEXTGEMS_INPUT_PATH ${TEST_NEXTGEMS_PATH}/input )
file(MAKE_DIRECTORY ${TEST_NEXTGEMS_INPUT_PATH})
ecbuild_get_test_multidata( EXTRACT 
    TARGET multio_action_nextGEMS_get_test_input 
    DIRNAME multio/tests/actions
    DIRLOCAL ${TEST_NEXTGEMS_INPUT_PATH}
    NAMES ${_action_nextGEMS_test_input} )

configure_file( testNextGEMS.yaml.in ${TEST_NEXTGEMS_PATH}/testNextGEMS.yaml @ONLY )

set(TEST_NEXTGEMS_SCRIPT ${TEST_NEXTGEMS_PATH}/multio_test_nextGEMS.sh )
configure_file( multio_test_nextGEMS.sh.in ${TEST_NEXTGEMS_SCRIPT} @ONLY )



list( APPEND _action_nextGEMS_test_dependencies
    multio_action_nextGEMS_get_test_templates 
    multio_action_nextGEMS_get_test_templates 
)
ecbuild_add_test( 
    TARGET test_multio_nextGEMS_encoding
    COMMAND ${TEST_NEXTGEMS_SCRIPT}
    TEST_DEPENDS ${_action_nextGEMS_test_dependencies}
)

