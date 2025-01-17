set(PREFIX multio_tests_action_interpolate_reduced_gg_to_HEALPix_32_nested)

ecbuild_get_test_multidata(
    TARGET   ${PREFIX}_get_data_interpolate
    DIRNAME  multio/tests/actions/interpolate
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES    "MARS_reduced_gg.grib"
)

ecbuild_get_test_multidata(
    TARGET   ${PREFIX}_get_data_healpix_ring2nest
    DIRNAME  multio/tests/actions/healpix_ring2nest
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES    "Reference_HEALPix_32_nested.grib"
             "HEALPix_ring2nest.atlas"
)

ecbuild_add_test(
    TARGET       ${PREFIX}_original
    TEST_DEPENDS ${PREFIX}_get_data_interpolate ${PREFIX}_get_data_healpix_ring2nest
    COMMAND      multio-feed
    ARGS         ${CMAKE_CURRENT_BINARY_DIR}/MARS_reduced_gg.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/reduced_gg_to_HEALPix_32_nested_original.yaml
)
set_tests_properties(${PREFIX}_original PROPERTIES
    FIXTURES_SETUP ${PREFIX}_original
)

ecbuild_add_test(
    TARGET       ${PREFIX}_direct
    TEST_DEPENDS ${PREFIX}_get_data_interpolate ${PREFIX}_get_data_healpix_ring2nest
    COMMAND      multio-feed
    ARGS         ${CMAKE_CURRENT_BINARY_DIR}/MARS_reduced_gg.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/reduced_gg_to_HEALPix_32_nested_direct.yaml
)
set_tests_properties(${PREFIX}_direct PROPERTIES
    FIXTURES_SETUP ${PREFIX}_direct
)

ecbuild_add_test(
    TARGET  ${PREFIX}_compare
    COMMAND grib_compare
    ARGS    MultIO_reduced_gg_to_HEALPix_32_nested_original.grib MultIO_reduced_gg_to_HEALPix_32_nested_direct.grib
)
set_tests_properties(${PREFIX}_compare PROPERTIES
    FIXTURES_REQUIRED "${PREFIX}_original;${PREFIX}_direct"
)
