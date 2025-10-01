
set(PREFIX multio_tests_mars2grib)

# Set IFS_INSTALL_DIR to the Multio source root directory.
file(REAL_PATH "../../../../" IFS_INSTALL_DIR BASE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

list( APPEND _mars2grib_test_data
  "mars-encoder-conf-test-aifs.tar.xz"
  "mars-encoder-conf-test-era6-5220.tar.xz"
)

ecbuild_get_test_multidata(
    EXTRACT TARGET   ${PREFIX}_get_data
    DIRNAME  multio/test-data/mars2grib
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES ${_mars2grib_test_data}
)


# Test to reparse MULTIOM configs in c++
# Will be removed after C++ migration
ecbuild_add_test( TARGET    ${PREFIX}_conf
                  SOURCES   test_multio_mars2grib_conf.cc
                  LIBS      multio multio-mars2grib)

ecbuild_add_test( TARGET    ${PREFIX}_infer_pdt
                  SOURCES   test_multio_mars2grib_infer_pdt.cc
                  LIBS      multio multio-mars2grib)

ecbuild_add_test( TARGET    ${PREFIX}_rules_gen
                  SOURCES   test_multio_mars2grib_rules_gen.cc
                  TEST_REQUIRES  ${PREFIX}_infer_pdt ${PREFIX}_conf ${PREFIX}_get_data
                  LIBS      multio multio-mars2grib)

ecbuild_add_test( TARGET    ${PREFIX}_encode
                  SOURCES   test_multio_mars2grib_encode.cc
                  TEST_REQUIRES  ${PREFIX}_rules_gen
                  LIBS      multio multio-mars2grib)

# TBD
ecbuild_add_test( TARGET    ${PREFIX}_setters
                  SOURCES   test_multio_mars2grib_setters.cc
                  TEST_REQUIRES  ${PREFIX}_rules_gen 
                  LIBS      multio multio-mars2grib)


