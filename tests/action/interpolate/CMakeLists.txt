if( HAVE_MIR )

set(PREFIX multio_tests_action_interpolate)

ecbuild_get_test_multidata(
    TARGET   ${PREFIX}_get_data
    DIRNAME  multio/tests/actions/interpolate
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES
        "MARS_reduced_gg.grib"
        "MARS_reduced_gg_to_grid_0.25_0.25.grib"
        "MARS_reduced_gg_to_grid_0.25_0.25_area.grib"
        "MARS_reduced_gg_to_grid_0.50_0.50.grib"
        "MARS_reduced_gg_to_grid_0.50_0.50_area.grib"
        "MARS_reduced_gg_to_grid_1.00_1.00.grib"
        "MARS_reduced_gg_to_grid_1.00_1.00_area.grib"
        "MARS_regular_ll.grib"
        "MARS_regular_ll_to_grid_3.00_6.00.grib"
        "MARS_regular_ll_to_grid_6.00_9.00.grib"
        "MARS_sh.grib"
        "MARS_sh_to_grid_0.25_0.25.grib"
        "MARS_sh_to_grid_0.25_0.25_area.grib"
        "MARS_sh_to_grid_0.50_0.50.grib"
        "MARS_sh_to_grid_0.50_0.50_area.grib"
        "MARS_sh_to_grid_1.00_1.00.grib"
        "MARS_sh_to_grid_1.00_1.00_area.grib")

foreach(_test
    reduced_gg_copy
    reduced_gg_encode_decode
    reduced_gg_to_grid_0.25_0.25
    reduced_gg_to_grid_0.25_0.25_area
    reduced_gg_to_grid_0.50_0.50
    reduced_gg_to_grid_0.50_0.50_area
    reduced_gg_to_grid_1.00_1.00
    reduced_gg_to_grid_1.00_1.00_area
    regular_ll_copy
    regular_ll_encode_decode
    regular_ll_to_grid_3.00_6.00
    regular_ll_to_grid_6.00_9.00
    sh_copy
    sh_encode_decode
    sh_to_grid_0.25_0.25
    sh_to_grid_0.25_0.25_area
    sh_to_grid_0.50_0.50
    sh_to_grid_0.50_0.50_area
    sh_to_grid_1.00_1.00
    sh_to_grid_1.00_1.00_area)
    set(_result MultIO_${_test}.grib)

    if(_test MATCHES "^reduced_gg.*")
        set(_feed "MARS_reduced_gg.grib")
    elseif(_test MATCHES "^regular_ll.*")
        set(_feed "MARS_regular_ll.grib")
    else()
        set(_feed "MARS_sh.grib")
    endif()

    if(_test MATCHES ".*_encode_decode" OR _test MATCHES ".*_copy")
        set(_reference ${_feed})
    else()
        set(_reference MARS_${_test}.grib)
    endif()

    ecbuild_add_test(
        TARGET       ${PREFIX}_${_test}_run
        TEST_DEPENDS ${PREFIX}_get_data
        ENVIRONMENT  ATLAS_ORCA_CACHING
        COMMAND      multio-feed
        ARGS          --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/${_test}.yaml ${_feed})

    ecbuild_add_test(
        TARGET       ${PREFIX}_${_test}_check_metadata
        TEST_DEPENDS ${PREFIX}_${_test}_run
        COMMAND      grib_compare
        ARGS         -H ${_reference} ${_result})

    if(_test MATCHES ".*to_grid_.*[^a]$")
        # FIXME ignore values at the pole
        ecbuild_add_test(
            TARGET       ${PREFIX}_${_test}_check_values
            TEST_DEPENDS ${PREFIX}_${_test}_check_metadata
            COMMAND      mir-compare
            ARGS         ${_reference} ${_result} --ignore-above-latitude=89.9 --ignore-below-latitude=-89.9 --absolute-error=0.0001)
    else()
        ecbuild_add_test(
            TARGET       ${PREFIX}_${_test}_check_values
            TEST_DEPENDS ${PREFIX}_${_test}_check_metadata
            COMMAND      grib_compare
            ARGS         -P -T10 ${_reference} ${_result})
    endif()
endforeach()




if ( HAVE_HEALPIX_EXAMPLES )

ecbuild_get_test_multidata(
    TARGET   ${PREFIX}_get_HEALPix_data
    DIRNAME  multio/tests/actions/interpolate
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES
        "Reference_reduced_gg_to_HEALPix_32.grib"
        "Reference_reduced_gg_to_HEALPix_1024.grib"
        "Reference_regular_ll_to_HEALPix_32.grib"
        "Reference_regular_ll_to_HEALPix_1024.grib"
        "Reference_reduced_gg_to_HEALPix_32_conservative.grib"
        "Reference_reduced_gg_to_HEALPix_1024_conservative.grib"
        "Reference_regular_ll_to_HEALPix_32_conservative.grib"
        "Reference_regular_ll_to_HEALPix_1024_conservative.grib"
        "Reference_sh_to_HEALPix_32.grib"
        "Reference_sh_to_HEALPix_1024.grib"
        "eORCA12_T.grib"
        "eORCA12_U.grib"
        "eORCA12_V.grib"
        "eORCA12_W.grib"
        "Reference_eORCA12_T_to_HEALPix_32.grib"
        "Reference_eORCA12_T_to_HEALPix_1024.grib"
        "Reference_eORCA12_U_to_HEALPix_32.grib"
        "Reference_eORCA12_U_to_HEALPix_1024.grib"
        "Reference_eORCA12_V_to_HEALPix_32.grib"
        "Reference_eORCA12_V_to_HEALPix_1024.grib"
        "Reference_eORCA12_W_to_HEALPix_32.grib"
        "Reference_eORCA12_W_to_HEALPix_1024.grib")

foreach(_test
    reduced_gg_to_HEALPix_32
    reduced_gg_to_HEALPix_1024
    regular_ll_to_HEALPix_32
    regular_ll_to_HEALPix_1024
    reduced_gg_to_HEALPix_32_conservative
    reduced_gg_to_HEALPix_1024_conservative
    regular_ll_to_HEALPix_32_conservative
    regular_ll_to_HEALPix_1024_conservative
    sh_to_HEALPix_32
    sh_to_HEALPix_1024
    eORCA12_T_to_HEALPix_32
    eORCA12_U_to_HEALPix_32
    eORCA12_V_to_HEALPix_32
    eORCA12_W_to_HEALPix_32
    eORCA12_T_to_HEALPix_1024
    eORCA12_U_to_HEALPix_1024
    eORCA12_V_to_HEALPix_1024
    eORCA12_W_to_HEALPix_1024)
    set(_result MultIO_${_test}.grib)

    if(_test MATCHES "^reduced_gg.*")
        set(_feed "MARS_reduced_gg.grib")
    elseif(_test MATCHES "^regular_ll.*")
        set(_feed "MARS_regular_ll.grib")
    elseif(_test MATCHES "^eORCA12_T.*")
        set(_feed "eORCA12_T.grib")
    elseif(_test MATCHES "^eORCA12_U.*")
        set(_feed "eORCA12_U.grib")
    elseif(_test MATCHES "^eORCA12_V.*")
        set(_feed "eORCA12_V.grib")
    elseif(_test MATCHES "^eORCA12_W.*")
        set(_feed "eORCA12_W.grib")
    else()
        set(_feed "MARS_sh.grib")
    endif()

    if(_test MATCHES ".*_encode_decode" OR _test MATCHES ".*_copy")
        set(_reference ${_feed})
    elseif( _test MATCHES ".*HEALPix.*" )
        set(_reference Reference_${_test}.grib)
    else()
        set(_reference MARS_${_test}.grib)
    endif()

    ecbuild_add_test(
        TARGET       ${PREFIX}_${_test}_run
        TEST_DEPENDS ${PREFIX}_get_data
        ENVIRONMENT  ATLAS_ORCA_CACHING
        COMMAND      multio-feed
        ARGS          --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/${_test}.yaml ${_feed})

    ecbuild_add_test(
        TARGET       ${PREFIX}_${_test}_check_metadata
        TEST_DEPENDS ${PREFIX}_${_test}_run
        COMMAND      grib_compare
        ARGS         -H ${_reference} ${_result})

    if(_test MATCHES ".*to_grid_.*[^a]$")
        # FIXME ignore values at the pole
        ecbuild_add_test(
            TARGET       ${PREFIX}_${_test}_check_values
            TEST_DEPENDS ${PREFIX}_${_test}_check_metadata
            COMMAND      mir-compare
            ARGS         ${_reference} ${_result} --ignore-above-latitude=89.9 --ignore-below-latitude=-89.9 --absolute-error=0.0001)
    else()
        ecbuild_add_test(
            TARGET       ${PREFIX}_${_test}_check_values
            TEST_DEPENDS ${PREFIX}_${_test}_check_metadata
            COMMAND      grib_compare
            ARGS         -P -T10 ${_reference} ${_result})
    endif()
endforeach()


endif()

endif( HAVE_MIR )
