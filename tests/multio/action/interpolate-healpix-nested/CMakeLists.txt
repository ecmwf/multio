if(HAVE_MIR AND HAVE_MULTIO_BUILD_TOOLS)

set(DATA_PREFIX multio_tests_action_interpolate_reduced_gg_to_HEALPix_nested)

ecbuild_get_test_multidata(
    TARGET   ${DATA_PREFIX}_get_data_interpolate
    DIRNAME  multio/tests/actions/interpolate
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES    "MARS_reduced_gg.grib"
)
if ( TEST ${DATA_PREFIX}_get_data_interpolate )
    set_tests_properties(${DATA_PREFIX}_get_data_interpolate PROPERTIES
        FIXTURES_SETUP ${DATA_PREFIX}_get_data_interpolate
    )
endif()

ecbuild_get_test_multidata(
    TARGET   ${DATA_PREFIX}_get_data_healpix_ring2nest
    DIRNAME  multio/tests/actions/healpix_ring2nest
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES    "Reference_HEALPix_32_nested.grib"
             "Reference_HEALPix_1024_nested.grib"
)
if ( TEST ${DATA_PREFIX}_get_data_healpix_ring2nest )
    set_tests_properties(${DATA_PREFIX}_get_data_healpix_ring2nest PROPERTIES
        FIXTURES_SETUP ${DATA_PREFIX}_get_data_healpix_ring2nest
    )
endif()

ecbuild_get_test_multidata(
    TARGET   ${DATA_PREFIX}_get_data_interpolate_healpix
    DIRNAME  multio/tests/actions/interpolate-healpix-nested
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES    "Reference_reduced_gg_to_HEALPix_32_nested.grib"
             "Reference_reduced_gg_to_HEALPix_1024_nested.grib"
             "HEALPix_ring2nest.atlas"
             "CORE2_ngrid_NSIDE128_0_ring.csv"
)
if ( TEST ${DATA_PREFIX}_get_data_interpolate_healpix )
    set_tests_properties(${DATA_PREFIX}_get_data_interpolate_healpix PROPERTIES
        FIXTURES_SETUP ${DATA_PREFIX}_get_data_interpolate_healpix
    )
endif()

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
    if ( TEST ${PREFIX}_original )
        set_tests_properties(${PREFIX}_original PROPERTIES
            FIXTURES_SETUP    ${PREFIX}_original
            FIXTURES_REQUIRED "${DATA_PREFIX}_get_data_interpolate;${DATA_PREFIX}_get_data_healpix_ring2nest"
        )
    endif()

    ecbuild_add_test(
        TARGET  ${PREFIX}_direct
        COMMAND multio-feed
        ARGS    ${CMAKE_CURRENT_BINARY_DIR}/MARS_reduced_gg.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/reduced_gg_to_HEALPix_${_n}_nested_direct.yaml
    )
    if ( TEST ${PREFIX}_direct )
        set_tests_properties(${PREFIX}_direct PROPERTIES
            FIXTURES_SETUP    ${PREFIX}_direct
            FIXTURES_REQUIRED "${DATA_PREFIX}_get_data_interpolate;${DATA_PREFIX}_get_data_healpix_ring2nest"
        )
    endif()

    ecbuild_add_test(
        TARGET  ${PREFIX}_compare_original
        COMMAND grib_compare
        ARGS    MultIO_reduced_gg_to_HEALPix_${_n}_nested_original.grib MultIO_reduced_gg_to_HEALPix_${_n}_nested_direct.grib
    )
    if ( TEST ${PREFIX}_compare_original )
        set_tests_properties(${PREFIX}_compare_original PROPERTIES
            FIXTURES_REQUIRED "${PREFIX}_original;${PREFIX}_direct"
        )
    endif()

    ecbuild_add_test(
        TARGET  ${PREFIX}_compare_reference
        COMMAND grib_compare
        ARGS    Reference_reduced_gg_to_HEALPix_${_n}_nested.grib MultIO_reduced_gg_to_HEALPix_${_n}_nested_direct.grib
    )
    if ( TEST ${PREFIX}_compare_reference )
        set_tests_properties(${PREFIX}_compare_reference PROPERTIES
            FIXTURES_REQUIRED "${DATA_PREFIX}_get_data_interpolate_healpix;${PREFIX}_direct"
        )
    endif()

endforeach()


### FESOM to MIR Cache Generator

set(PREFIX multio_tests_fesom_cache_generator)

ecbuild_add_test(
    TARGET  ${PREFIX}_fesom_csv_ring2mat_ring
    COMMAND cache-generator-fesom-2-mir
    ARGS    --inputPath=${CMAKE_CURRENT_BINARY_DIR} --inputFile=CORE2_ngrid_NSIDE128_0_ring.csv --nCols=126858 --inputOrdering=ring --outputOrdering=ring
)
if ( TEST ${PREFIX}_fesom_csv_ring2mat_ring )
    set_tests_properties(${PREFIX}_fesom_csv_ring2mat_ring PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_fesom_csv_ring2mat_ring
        FIXTURES_REQUIRED ${DATA_PREFIX}_get_data_interpolate_healpix
    )
endif()

ecbuild_add_test(
    TARGET  ${PREFIX}_fesom_csv_ring2mat_nested
    COMMAND cache-generator-fesom-2-mir
    ARGS    --inputPath=${CMAKE_CURRENT_BINARY_DIR} --inputFile=CORE2_ngrid_NSIDE128_0_ring.csv --nCols=126858 --inputOrdering=ring --outputOrdering=nested
)
if ( TEST ${PREFIX}_fesom_csv_ring2mat_nested )
    set_tests_properties(${PREFIX}_fesom_csv_ring2mat_nested PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_fesom_csv_ring2mat_nested
        FIXTURES_REQUIRED ${DATA_PREFIX}_get_data_interpolate_healpix
    )
endif()

ecbuild_get_test_multidata(
    TARGET   ${DATA_PREFIX}_get_data_fesom
    DIRNAME  multio/tests/actions/interpolate-fesom
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES
        "fesom_CORE2_ngrid_feed_o2d.grib"
        "fesom_to_HEALPix32.tmpl"
)

ecbuild_add_test(
    TARGET  ${PREFIX}_original
    COMMAND multio-feed
    ARGS    ${CMAKE_CURRENT_BINARY_DIR}/fesom_CORE2_ngrid_feed_o2d.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/fesom_CORE2_ngrid_to_HEALPix_128_nested_original.yaml
)
if ( TEST ${PREFIX}_original )
    set_tests_properties(${PREFIX}_original PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_original
        FIXTURES_REQUIRED "${DATA_PREFIX}_get_data_fesom;${PREFIX}_fesom_csv_ring2mat_ring"
    )
endif()

ecbuild_add_test(
    TARGET  ${PREFIX}_direct
    COMMAND multio-feed
    ARGS    ${CMAKE_CURRENT_BINARY_DIR}/fesom_CORE2_ngrid_feed_o2d.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/fesom_CORE2_ngrid_to_HEALPix_128_nested_direct.yaml
)
if ( TEST ${PREFIX}_direct )
    set_tests_properties(${PREFIX}_direct PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_direct
        FIXTURES_REQUIRED "${DATA_PREFIX}_get_data_fesom;${PREFIX}_fesom_csv_ring2mat_nested"
    )
endif()

ecbuild_add_test(
    TARGET  ${PREFIX}_compare
    COMMAND grib_compare
    ARGS    MultIO_fesom_CORE2_ngrid_to_HEALPix_128_nested_original.grib MultIO_fesom_CORE2_ngrid_to_HEALPix_128_nested_direct.grib
)
if ( TEST ${PREFIX}_compare )
    set_tests_properties(${PREFIX}_compare PROPERTIES
        FIXTURES_REQUIRED "${PREFIX}_original;${PREFIX}_direct"
    )
endif()

endif()
