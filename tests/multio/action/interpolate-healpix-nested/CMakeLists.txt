set(DATA_PREFIX multio_tests_action_interpolate_reduced_gg_to_HEALPix_nested)

ecbuild_get_test_multidata(
    TARGET   ${DATA_PREFIX}_get_data_interpolate
    DIRNAME  multio/tests/actions/interpolate
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES    "MARS_reduced_gg.grib"
)
set_tests_properties(${DATA_PREFIX}_get_data_interpolate PROPERTIES
    FIXTURES_SETUP ${DATA_PREFIX}_get_data_interpolate
)

ecbuild_get_test_multidata(
    TARGET   ${DATA_PREFIX}_get_data_healpix_ring2nest
    DIRNAME  multio/tests/actions/healpix_ring2nest
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES    "Reference_HEALPix_32_nested.grib"
             "Reference_HEALPix_1024_nested.grib"
             "HEALPix_ring2nest.atlas"
)
set_tests_properties(${DATA_PREFIX}_get_data_healpix_ring2nest PROPERTIES
    FIXTURES_SETUP ${DATA_PREFIX}_get_data_healpix_ring2nest
)

foreach (_n
    32
    1024
)
    set(PREFIX multio_tests_action_interpolate_reduced_gg_to_HEALPix_${_n}_nested)

    ecbuild_add_test(
        TARGET  ${PREFIX}_original
        COMMAND multio-feed
        ARGS    ${CMAKE_CURRENT_BINARY_DIR}/MARS_reduced_gg.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/reduced_gg_to_HEALPix_${_n}_nested_original.yaml
    )
    set_tests_properties(${PREFIX}_original PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_original
        FIXTURES_REQUIRED "${DATA_PREFIX}_get_data_interpolate;${DATA_PREFIX}_get_data_healpix_ring2nest"
    )

    ecbuild_add_test(
        TARGET  ${PREFIX}_direct
        COMMAND multio-feed
        ARGS    ${CMAKE_CURRENT_BINARY_DIR}/MARS_reduced_gg.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/reduced_gg_to_HEALPix_${_n}_nested_direct.yaml
    )
    set_tests_properties(${PREFIX}_direct PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_direct
        FIXTURES_REQUIRED "${DATA_PREFIX}_get_data_interpolate;${DATA_PREFIX}_get_data_healpix_ring2nest"
    )

    ecbuild_add_test(
        TARGET  ${PREFIX}_compare
        COMMAND grib_compare
        ARGS    MultIO_reduced_gg_to_HEALPix_${_n}_nested_original.grib MultIO_reduced_gg_to_HEALPix_${_n}_nested_direct.grib
    )
    set_tests_properties(${PREFIX}_compare PROPERTIES
        FIXTURES_REQUIRED "${PREFIX}_original;${PREFIX}_direct"
    )

endforeach()