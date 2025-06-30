
set(PREFIX multio_tests_action_encode-mtg2)

# Set IFS_INSTALL_DIR to the Multio source root directory.
file(REAL_PATH "../../../../" IFS_INSTALL_DIR BASE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

ecbuild_add_test(
    TARGET ${PREFIX}_initialize
    SOURCES test_initialize.cc
    NO_AS_NEEDED
    LIBS multio-action-encode-mtg2 multio-action-sink
    ENVIRONMENT "IFS_INSTALL_DIR=${IFS_INSTALL_DIR}"
)


# Test to reparse MULTIOM configs in c++
# Will be removed after C++ migration
ecbuild_add_test( TARGET    test_multio_encode_mtg2_conf
                  SOURCES   test_multio_encode_mtg2_conf.cc
                  LIBS      multio multio-action-encode-mtg2)


