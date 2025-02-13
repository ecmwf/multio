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
             "HEALPix_ring2nest.atlas"
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
