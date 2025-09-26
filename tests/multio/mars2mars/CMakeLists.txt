set(PREFIX multio_tests_mars2mars)

# Set IFS_INSTALL_DIR to the Multio source root directory.
file(REAL_PATH "../../../../" IFS_INSTALL_DIR BASE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

ecbuild_add_test( TARGET    ${PREFIX}_mappings
                  SOURCES   test_multio_mars2mars_mappings.cc
                  LIBS      multio multio-mars2mars)

