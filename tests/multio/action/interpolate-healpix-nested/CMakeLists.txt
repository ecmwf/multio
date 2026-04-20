if(HAVE_MIR AND HAVE_MULTIO_BUILD_TOOLS)

set(DATA_PREFIX multio_tests_action_interpolate_reduced_gg_to_HEALPix_nested)

ecbuild_get_test_multidata(
    TARGET   ${DATA_PREFIX}_get_data_interpolate
    DIRNAME  multio/test-data/tests/actions/interpolate
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES    "MARS_reduced_gg.grib"
)

ecbuild_get_test_multidata(
    TARGET   ${DATA_PREFIX}_get_data_healpix_ring2nest
    DIRNAME  multio/test-data/tests/actions/healpix_ring2nest
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES    "Reference_HEALPix_32_nested.grib"
             "Reference_HEALPix_1024_nested.grib"
)

ecbuild_get_test_multidata(
    TARGET   ${DATA_PREFIX}_get_data_interpolate_healpix
    DIRNAME  multio/test-data/tests/actions/interpolate-healpix-nested
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES    "Reference_reduced_gg_to_HEALPix_32_nested.grib"
             "Reference_reduced_gg_to_HEALPix_1024_nested.grib"
             "HEALPix_ring2nest.atlas"
             "CORE2_ngrid_NSIDE128_0_ring.csv"
)

foreach (_n
    32
    1024
)
    set(PREFIX multio_tests_action_interpolate_reduced_gg_to_HEALPix_${_n}_nested)

    ecbuild_add_test(
        TARGET          ${PREFIX}_original
        TEST_REQUIRES   ${DATA_PREFIX}_get_data_interpolate ${DATA_PREFIX}_get_data_healpix_ring2nest ${DATA_PREFIX}_get_data_interpolate_healpix
        COMMAND         multio-feed
        ARGS            ${CMAKE_CURRENT_BINARY_DIR}/MARS_reduced_gg.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/reduced_gg_to_HEALPix_${_n}_nested_original.yaml
        TEST_PROPERTIES TEST_PRIORITY 1  # This test is very slow and should start as early as possible!
    )

    ecbuild_add_test(
        TARGET          ${PREFIX}_direct
        TEST_REQUIRES   ${DATA_PREFIX}_get_data_interpolate ${DATA_PREFIX}_get_data_healpix_ring2nest
        COMMAND         multio-feed
        ARGS            ${CMAKE_CURRENT_BINARY_DIR}/MARS_reduced_gg.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/reduced_gg_to_HEALPix_${_n}_nested_direct.yaml
        TEST_PROPERTIES TEST_PRIORITY 1  # This test is very slow and should start as early as possible!
    )

    ecbuild_add_test(
        TARGET        ${PREFIX}_compare_original
        TEST_REQUIRES ${PREFIX}_original ${PREFIX}_direct
        COMMAND       grib_compare
        ARGS          MultIO_reduced_gg_to_HEALPix_${_n}_nested_original.grib MultIO_reduced_gg_to_HEALPix_${_n}_nested_direct.grib
    )

    ecbuild_add_test(
        TARGET        ${PREFIX}_compare_reference
        TEST_REQUIRES ${DATA_PREFIX}_get_data_interpolate_healpix ${PREFIX}_direct
        COMMAND       grib_compare
        ARGS          Reference_reduced_gg_to_HEALPix_${_n}_nested.grib MultIO_reduced_gg_to_HEALPix_${_n}_nested_direct.grib
    )

endforeach()

endif()
