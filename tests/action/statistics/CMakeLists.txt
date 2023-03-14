set(PREFIX multio_tests_action_statistics)

# Configure the action
list( APPEND _statistics_tests_names
average_1d_grib2
average_15d_grib2
average_1m_grib2
average_3m_grib2
)


list( APPEND _statistics_test_data
"statistics-test-data.grib"
"reduced_gg_pl_80_avg_grib2.tmpl"
"Reference_average_15d_grib2.grib"
"Reference_average_1d_grib2.grib"
"Reference_average_1m_grib2.grib"
"Reference_average_3m_grib2.grib"
)

# Some tests depend from mir interpolation
if( HAVE_MIR )
list ( APPEND _statistics_tests_names
average_1d_regrid_grib2
)
list( APPEND _statistics_test_data
"regular_ll_pl_grib2.tmpl"
"Reference_average_1d_regrid_grib2.grib"
)
endif( HAVE_MIR )


ecbuild_get_test_multidata(
    TARGET   ${PREFIX}_get_data
    DIRNAME  multio/tests/actions/statistics
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES ${_statistics_test_data}
    )

foreach(_test ${_statistics_tests_names} )

    set(_result    Result_${_test}.grib)
    set(_reference Reference_${_test}.grib)

    set(_feed statistics-test-data.grib)

    ecbuild_add_test(
        TARGET       ${PREFIX}_run_${_test}
        TEST_DEPENDS ${PREFIX}_get_data
        COMMAND      multio-feed
        ARGS          --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/${_test}.yaml ${_feed})

    ecbuild_add_test(
        TARGET       ${PREFIX}_check_metadata_${_test}
        TEST_DEPENDS ${PREFIX}_run_${_test}
        COMMAND      grib_compare
        ARGS         -H ${_reference} ${_result})

    ecbuild_add_test(
        TARGET       ${PREFIX}_check_values_${_test}
        TEST_DEPENDS ${PREFIX}_check_metadata_${_test}
        COMMAND      grib_compare
        ARGS         ${_reference} ${_result})

endforeach()

