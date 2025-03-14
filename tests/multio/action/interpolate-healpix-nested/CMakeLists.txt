set(DATA_PREFIX multio_tests_action_interpolate_reduced_gg_to_HEALPix_nested)

ecbuild_get_test_multidata(
    TARGET   ${DATA_PREFIX}_get_data_interpolate
    DIRNAME  multio/tests/actions/interpolate
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES    "MARS_reduced_gg.grib"
)

ecbuild_get_test_multidata(
    TARGET   ${DATA_PREFIX}_get_data_healpix_ring2nest
    DIRNAME  multio/tests/actions/healpix_ring2nest
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES    "Reference_HEALPix_32_nested.grib"
             "Reference_HEALPix_1024_nested.grib"
)

ecbuild_get_test_multidata(
    TARGET   ${DATA_PREFIX}_get_data_interpolate_healpix
    DIRNAME  multio/tests/actions/interpolate-healpix-nested
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
        TARGET        ${PREFIX}_original
        TEST_REQUIRES ${DATA_PREFIX}_get_data_interpolate ${DATA_PREFIX}_get_data_healpix_ring2nest
        COMMAND       multio-feed
        ARGS          ${CMAKE_CURRENT_BINARY_DIR}/MARS_reduced_gg.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/reduced_gg_to_HEALPix_${_n}_nested_original.yaml
    )

    ecbuild_add_test(
        TARGET        ${PREFIX}_direct
        TEST_REQUIRES ${DATA_PREFIX}_get_data_interpolate ${DATA_PREFIX}_get_data_healpix_ring2nest
        COMMAND       multio-feed
        ARGS          ${CMAKE_CURRENT_BINARY_DIR}/MARS_reduced_gg.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/reduced_gg_to_HEALPix_${_n}_nested_direct.yaml
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


### FESOM to MIR Cache Generator

set(PREFIX multio_tests_fesom_cache_generator)

ecbuild_add_test(
    TARGET        ${PREFIX}_fesom_csv_ring2mat_ring
    TEST_REQUIRES ${DATA_PREFIX}_get_data_interpolate_healpix
    COMMAND       cache-generator-fesom-2-mir
    ARGS          --inputPath=${CMAKE_CURRENT_BINARY_DIR} --inputFile=CORE2_ngrid_NSIDE128_0_ring.csv --nCols=126858 --inputOrdering=ring --outputOrdering=ring
)

ecbuild_add_test(
    TARGET        ${PREFIX}_fesom_csv_ring2mat_nested
    TEST_REQUIRES ${DATA_PREFIX}_get_data_interpolate_healpix
    COMMAND       cache-generator-fesom-2-mir
    ARGS          --inputPath=${CMAKE_CURRENT_BINARY_DIR} --inputFile=CORE2_ngrid_NSIDE128_0_ring.csv --nCols=126858 --inputOrdering=ring --outputOrdering=nested
)

ecbuild_get_test_multidata(
    TARGET   ${DATA_PREFIX}_get_data_fesom
    DIRNAME  multio/tests/actions/interpolate-fesom
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES
        "fesom_CORE2_ngrid_feed_o2d.grib"
        "fesom_to_HEALPix32.tmpl"
)

ecbuild_add_test(
    TARGET        ${PREFIX}_original
    TEST_REQUIRES ${DATA_PREFIX}_get_data_fesom ${PREFIX}_fesom_csv_ring2mat_ring
    COMMAND       multio-feed
    ARGS          ${CMAKE_CURRENT_BINARY_DIR}/fesom_CORE2_ngrid_feed_o2d.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/fesom_CORE2_ngrid_to_HEALPix_128_nested_original.yaml
)

ecbuild_add_test(
    TARGET        ${PREFIX}_direct
    TEST_REQUIRES ${DATA_PREFIX}_get_data_fesom ${PREFIX}_fesom_csv_ring2mat_nested
    COMMAND       multio-feed
    ARGS          ${CMAKE_CURRENT_BINARY_DIR}/fesom_CORE2_ngrid_feed_o2d.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/fesom_CORE2_ngrid_to_HEALPix_128_nested_direct.yaml
)

ecbuild_add_test(
    TARGET        ${PREFIX}_compare
    TEST_REQUIRES ${PREFIX}_original;${PREFIX}_direct
    COMMAND       grib_compare
    ARGS          MultIO_fesom_CORE2_ngrid_to_HEALPix_128_nested_original.grib MultIO_fesom_CORE2_ngrid_to_HEALPix_128_nested_direct.grib
)
