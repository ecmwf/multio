
set(PREFIX multio_tests_action_statistics)

# Configure the action
list( APPEND _statistics_tests_names
    average_1d_grib2
    average_10d_grib2
    average_1m_grib2
)

list( APPEND _statistics_tests_kinds
    standard
    restart
)


# "standard_0_statistics_test_data.grib"
# "standard_1_statistics_test_data.grib"
# "restart_0_statistics_test_data.grib"
# "restart_1_statistics_test_data.grib"
list( APPEND _statistics_test_data
    "reduced_gg_pl_80_avg_grib2.tmpl"
    "standard_0_statistics_test_data.grib"
    "standard_1_statistics_test_data.grib"
    "standard_0-144_statistics_test_data.grib"
    "standard_144-288_statistics_test_data.grib"
    "Reference_standard_average_10d_grib2.grib"
    "Reference_standard_average_1d_grib2.grib"
    "Reference_standard_average_1m_grib2.grib"
    "Reference_standard_144-288_average_1m_grib2.grib"
    "restart_1_statistics_test_data.grib"
    "restart_0_statistics_test_data.grib"
    "Reference_restart_average_10d_grib2.grib"
    "Reference_restart_average_1d_grib2.grib"
    "Reference_restart_average_1m_grib2.grib"
)

# Some tests depend from mir interpolation
if( HAVE_MIR )
list ( APPEND _statistics_tests_names
    average_1d_regrid_2-1-10_grib2
)
list( APPEND _statistics_test_data
  "regular_ll_pl_grib2.tmpl"
  "Reference_standard_average_1d_regrid_2-1-10_grib2.grib"
  "Reference_restart_average_1d_regrid_2-1-10_grib2.grib"
)
endif( HAVE_MIR )


ecbuild_get_test_multidata(
    TARGET   ${PREFIX}_get_data
    DIRNAME  multio/tests/actions/new-statistics
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES ${_statistics_test_data}
)

foreach(_test ${_statistics_tests_names} )
    foreach(_kind ${_statistics_tests_kinds} )

        set(_result    Reference_${_kind}_${_test}.grib)

        foreach(_start "0" "1" )

            set(_reference Result_${_kind}_${_start}_${_test}.grib)
            set(_feed ${_kind}_${_start}_statistics_test_data.grib)


            if( (_test MATCHES "^.*regrid.*$") )

                if ( HAVE_MIR )
                    ecbuild_add_test(
                        TARGET       ${PREFIX}_run_${_kind}_${_start}_${_test}
                        TEST_DEPENDS ${PREFIX}_get_data
                        COMMAND      multio-feed
                        ARGS          --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/${_kind}_${_start}_${_test}.yaml ${_feed}
                    )

                    ecbuild_add_test(
                        TARGET       ${PREFIX}_check_metadata_${_kind}_${_start}_${_test}
                        TEST_DEPENDS ${PREFIX}_run_${_kind}_${_start}_${_test}
                        ARGS         -H ${_reference} ${_result}
                        COMMAND      grib_compare
                   )

                    ecbuild_add_test(
                        TARGET       ${PREFIX}_check_values_${_kind}_${_start}_${_test}
                        TEST_DEPENDS ${PREFIX}_check_metadata_${_kind}_${_start}_${_test}
                        COMMAND      mir-compare
                        ARGS         ${_reference} ${_result} --ignore-above-latitude=89.9 --ignore-below-latitude=-89.9
                    )
                endif()
            else()
                ecbuild_add_test(
                    TARGET       ${PREFIX}_run_${_kind}_${_start}_${_test}
                    TEST_DEPENDS ${PREFIX}_get_data
                    COMMAND      multio-feed
                    ARGS          --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/${_kind}_${_start}_${_test}.yaml ${_feed}
                )

                ecbuild_add_test(
                    TARGET       ${PREFIX}_check_metadata_${_kind}_${_start}_${_test}
                    TEST_DEPENDS ${PREFIX}_run_${_kind}_${_start}_${_test}
                    ARGS         -H ${_reference} ${_result}
                    COMMAND      grib_compare
                )

                ecbuild_add_test(
                    TARGET       ${PREFIX}_check_values_${_kind}_${_start}_${_test}
                    TEST_DEPENDS ${PREFIX}_check_metadata_${_kind}_${_start}_${_test}
                    COMMAND      grib_compare
                    ARGS         -P -T10 ${_reference} ${_result}
                )
            endif()

        endforeach()
    endforeach()
endforeach()


#
# Tests about the checkpointing needs to be reworked
add_subdirectory(restart)
