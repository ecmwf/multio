set(PREFIX ecom_extremesDT)


ecbuild_get_test_multidata(
  EXTRACT
  TARGET ${PREFIX}_get_test_data
  DIRNAME ecom/extremesDT/001
  DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
  NAMES
    "reference.tar.bz"
    "reproducer.tar.bz"
)

ecbuild_get_test_multidata(
  TARGET ${PREFIX}_get_test_cfg
  DIRNAME ecom/extremesDT/001
  DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
  NAMES
    "output-manager-config.yaml"
)

list( APPEND _arguments
  "-y output-manager-config.yaml"
  "-t GRIBX"
  "-i ${CMAKE_CURRENT_BINARY_DIR}/reproducer"
)

ecbuild_add_test(
  ENABLED       OFF
  TARGET        ${PREFIX}_${_test}_run
  TEST_REQUIRES ${PREFIX}_get_test_data ${PREFIX}_get_test_cfg
  COMMAND       secom
  ARGS          ${_arguments}
)

ecbuild_add_test(
  ENABLED       OFF
  TARGET        ${PREFIX}_${_test}_metadata
  TEST_REQUIRES ${PREFIX}_get_data ${PREFIX}_get_cache
  COMMAND       grib_compare
  ARGS          -H ${CMAKE_CURRENT_BINARY_DIR}/reference.grib ${CMAKE_CURRENT_BINARY_DIR}/allfields.00000001.grib
)
