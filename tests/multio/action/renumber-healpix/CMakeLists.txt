if(HAVE_MULTIO_BUILD_TOOLS)

set(PREFIX multio_tests_action_healpix_ring2nest)


ecbuild_get_test_multidata(
    TARGET   ${PREFIX}_get_HEALPix_remapping_data
    DIRNAME  multio/tests/actions/healpix_ring2nest
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES
        "Feed_HEALPix_32_ring.grib"
        "Feed_HEALPix_1024_ring.grib"
        "Reference_HEALPix_32_nested.grib"
        "Reference_HEALPix_1024_nested.grib"
        "HEALPix_ring2nest.atlas" )



ecbuild_add_test(
    TARGET        ${PREFIX}_HEALPix_32_ring2nest_run
    TEST_REQUIRES ${PREFIX}_get_HEALPix_remapping_data
    COMMAND       multio-feed
    ARGS          --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/HEALPix_32_ring2nest.yaml Feed_HEALPix_32_ring.grib)

ecbuild_add_test(
    TARGET        ${PREFIX}_HEALPix_32_ring2nest_check_metadata
    TEST_REQUIRES ${PREFIX}_HEALPix_32_ring2nest_run
    COMMAND       grib_compare
    ARGS          -H Reference_HEALPix_32_nested.grib MultIO_HEALPix_32_nested.grib)

ecbuild_add_test(
    TARGET        ${PREFIX}_HEALPix_32_ring2nest_check_values
    TEST_REQUIRES ${PREFIX}_HEALPix_32_ring2nest_check_metadata
    COMMAND       grib_compare
    ARGS          -P -T10 Reference_HEALPix_32_nested.grib MultIO_HEALPix_32_nested.grib)



ecbuild_add_test(
    TARGET        ${PREFIX}_HEALPix_1024_ring2nest_run
    TEST_REQUIRES ${PREFIX}_get_HEALPix_remapping_data
    COMMAND       multio-feed
    ARGS          --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/HEALPix_1024_ring2nest.yaml Feed_HEALPix_1024_ring.grib)

ecbuild_add_test(
    TARGET        ${PREFIX}_HEALPix_1024_ring2nest_check_metadata
    TEST_REQUIRES ${PREFIX}_HEALPix_1024_ring2nest_run
    COMMAND       grib_compare
    ARGS          -H Reference_HEALPix_1024_nested.grib MultIO_HEALPix_1024_nested.grib)

ecbuild_add_test(
    TARGET        ${PREFIX}_HEALPix_1024_ring2nest_check_values
    TEST_REQUIRES ${PREFIX}_HEALPix_1024_ring2nest_check_metadata
    COMMAND       grib_compare
    ARGS          -P -T10 Reference_HEALPix_1024_nested.grib MultIO_HEALPix_1024_nested.grib)

endif()
