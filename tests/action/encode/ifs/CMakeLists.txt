list( APPEND _action_ifs_test_data
    "ifs_test_templates.tar.gz"
)

set(TEST_IFS_PATH ${multio_BINARY_DIR}/tests/action/encode/ifs )

set(TEST_IFS_TEMPLATES_PATH ${TEST_IFS_PATH}/templates )
set(TEST_IFS_TEST_TEMPLATES_PATH ${TEST_IFS_TEMPLATES_PATH}/ifs_test_templates )
file(MAKE_DIRECTORY ${TEST_IFS_TEMPLATES_PATH})
ecbuild_get_test_multidata(
    EXTRACT
    TARGET multio_action_ifs_get_test_data
    DIRNAME multio/tests/actions
    DIRLOCAL ${TEST_IFS_TEMPLATES_PATH}
    NAMES ${_action_ifs_test_data} )

configure_file( testIFSTestFC.yaml.in ${TEST_IFS_PATH}/testIFSTestFC.yaml @ONLY )
