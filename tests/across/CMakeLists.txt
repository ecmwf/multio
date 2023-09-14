set(PREFIX multio_tests_across_dissemination)


configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/single_request.diss    ${CMAKE_BINARY_DIR}/multio/tests/across/single_request.diss    COPYONLY )
configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/multiple_requests.diss ${CMAKE_BINARY_DIR}/multio/tests/across/multiple_requests.diss COPYONLY )


list( APPEND _across_test_data
  "regular_ll_pl_grib2.tmpl"
  "regular_ll_ml_grib2.tmpl"
  "regular_ll_sfc_grib2.tmpl"
  "multio_tests_across_dissemination_data.grib"
  "Reference_test-across-dissemination-multiple_0-ZZ1-M1.grib"
  "Reference_test-across-dissemination-multiple_1-ZZ1-M1.grib"
  "Reference_test-across-dissemination-single_0-ZZ1-M1.grib"
)

ecbuild_get_test_multidata(
    TARGET   ${PREFIX}_get_data
    DIRNAME  multio/tests/across
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES ${_across_test_data}
)

set(_feed multio_tests_across_dissemination_data.grib )

ecbuild_add_test(
    TARGET       ${PREFIX}_single_request_run
    TEST_DEPENDS ${PREFIX}_get_data
    COMMAND      multio-feed
    ARGS          --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/plan_single_request.yaml ${_feed}
)

ecbuild_add_test(
    TARGET       ${PREFIX}_single_request_check_metadata
    TEST_DEPENDS ${PREFIX}_single_request_run
    ARGS         -H Reference_test-across-dissemination-single_0-ZZ1-M1.grib test-across-dissemination-single/0000/test-across-dissemination-single_0000-ZZ1:M1.grib
    COMMAND      grib_compare
)

ecbuild_add_test(
    TARGET       ${PREFIX}_single_request_check_values
    TEST_DEPENDS ${PREFIX}_single_request_check_metadata
    ARGS         -P -T10 Reference_test-across-dissemination-single_0-ZZ1-M1.grib test-across-dissemination-single/0000/test-across-dissemination-single_0000-ZZ1:M1.grib
    COMMAND      grib_compare
)




ecbuild_add_test(
    TARGET       ${PREFIX}_multiple_request_run
    TEST_DEPENDS ${PREFIX}_get_data
    COMMAND      multio-feed
    ARGS          --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/plan_multiple_requests.yaml ${_feed}
)

ecbuild_add_test(
    TARGET       ${PREFIX}_multiple_request_check_metadata_0
    TEST_DEPENDS ${PREFIX}_multiple_request_run
    ARGS         -H Reference_test-across-dissemination-multiple_0-ZZ1-M1.grib test-across-dissemination-multiple/0000/test-across-dissemination-multiple_0000-ZZ1:M1.grib
    COMMAND      grib_compare
)

ecbuild_add_test(
    TARGET       ${PREFIX}_multiple_request_check_metadata_1
    TEST_DEPENDS ${PREFIX}_multiple_request_run
    ARGS         -H Reference_test-across-dissemination-multiple_0-ZZ1-M1.grib test-across-dissemination-multiple/0001/test-across-dissemination-multiple_0001-ZZ1:M1.grib
    COMMAND      grib_compare
)

ecbuild_add_test(
    TARGET       ${PREFIX}_multiple_request_check_values_0
    TEST_DEPENDS ${PREFIX}_multiple_request_check_metadata_0
    ARGS         -P -T10 Reference_test-across-dissemination-multiple_0-ZZ1-M1.grib test-across-dissemination-multiple/0000/test-across-dissemination-multiple_0000-ZZ1:M1.grib
    COMMAND      grib_compare
)

ecbuild_add_test(
    TARGET       ${PREFIX}_multiple_request_check_values_1
    TEST_DEPENDS ${PREFIX}_multiple_request_check_metadata_1
    ARGS         -P -T10 Reference_test-across-dissemination-multiple_0-ZZ1-M1.grib test-across-dissemination-multiple/0001/test-across-dissemination-multiple_0001-ZZ1:M1.grib
    COMMAND      grib_compare
)
