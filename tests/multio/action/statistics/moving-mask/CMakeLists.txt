
set(PREFIX multio_tests_action_statistics_moving_mask)


### Without moving masks

ecbuild_get_test_multidata(
    TARGET   ${PREFIX}_static_get_input_data
    DIRNAME  multio/tests/actions/new-statistics
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES
        reduced_gg_pl_80_avg_grib2.tmpl
        standard_0_statistics_test_data.grib
        Reference_standard_average_1d_grib2.grib
)
set_tests_properties(${PREFIX}_static_get_input_data PROPERTIES
    FIXTURES_SETUP ${PREFIX}_static_get_input_data
)

ecbuild_add_test(
    TARGET      ${PREFIX}_static_avg_1d_all_run
    ENVIRONMENT THRESHOLD=-1 RESULT_FILE=Result_avg_1d_all_static.grib
    COMMAND     multio-feed
    ARGS        ${CMAKE_CURRENT_BINARY_DIR}/standard_0_statistics_test_data.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/test-avg-static.yaml
)
set_tests_properties(${PREFIX}_static_avg_1d_all_run PROPERTIES
    FIXTURES_SETUP    ${PREFIX}_static_avg_1d_all_run
    FIXTURES_REQUIRED ${PREFIX}_static_get_input_data
)

ecbuild_add_test(
    TARGET  ${PREFIX}_static_avg_1d_all_check
    COMMAND grib_compare
    ARGS    Reference_standard_average_1d_grib2.grib Result_avg_1d_all_static.grib
)
set_tests_properties(${PREFIX}_static_avg_1d_all_check PROPERTIES
    FIXTURES_REQUIRED ${PREFIX}_static_avg_1d_all_run
)

ecbuild_add_test(
    TARGET      ${PREFIX}_static_avg_1d_any_run
    ENVIRONMENT THRESHOLD=1 RESULT_FILE=Result_avg_1d_any_static.grib
    COMMAND     multio-feed
    ARGS        ${CMAKE_CURRENT_BINARY_DIR}/standard_0_statistics_test_data.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/test-avg-static.yaml
)
set_tests_properties(${PREFIX}_static_avg_1d_any_run PROPERTIES
    FIXTURES_SETUP    ${PREFIX}_static_avg_1d_any_run
    FIXTURES_REQUIRED ${PREFIX}_static_get_input_data
)

ecbuild_add_test(
    TARGET  ${PREFIX}_static_avg_1d_any_check
    COMMAND grib_compare
    ARGS    Reference_standard_average_1d_grib2.grib Result_avg_1d_any_static.grib
)
set_tests_properties(${PREFIX}_static_avg_1d_any_check PROPERTIES
    FIXTURES_REQUIRED ${PREFIX}_static_avg_1d_any_run
)


ecbuild_get_test_multidata(
    TARGET   ${PREFIX}_get_input_data
    DIRNAME  multio/tests/actions/statistics-moving-mask
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES
        Original_moving_mask.grib
        Statistics_grib2.tmpl
)
set_tests_properties(${PREFIX}_get_input_data PROPERTIES
    FIXTURES_SETUP ${PREFIX}_get_input_data
)


foreach(_op avg max min)

    string(REPLACE "avg" "average" OPERATION "${_op}")
    string(REPLACE "max" "maximum" OPERATION "${OPERATION}")
    string(REPLACE "min" "minimum" OPERATION "${OPERATION}")

foreach(_th 10d all any)

    string(REPLACE   "d"   "" THRESHOLD "${_th}")
    string(REPLACE "all" "-1" THRESHOLD "${THRESHOLD}")
    string(REPLACE "any"  "1" THRESHOLD "${THRESHOLD}")

    ### Without restart
    set(RESULT_FILE Result_${_op}_1m_${_th}.grib)
    set(REFERENCE_FILE Reference_${_op}_1m_${_th}.grib)

    ecbuild_add_test(
        TARGET      ${PREFIX}_${_op}_1m_${_th}_run
        ENVIRONMENT OPERATION=${OPERATION} THRESHOLD=${THRESHOLD} RESULT_FILE=${RESULT_FILE} READ_RESTART=false WRITE_RESTART=false RESTART_ID=none
        COMMAND     multio-feed
        ARGS        ${CMAKE_CURRENT_BINARY_DIR}/Original_moving_mask.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/test-${_op}-moving-mask.yaml
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_run PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_${_op}_1m_${_th}_run
        FIXTURES_REQUIRED ${PREFIX}_get_input_data
    )

    ecbuild_get_test_multidata(
        TARGET   ${PREFIX}_${_op}_1m_${_th}_get_reference_data
        DIRNAME  multio/tests/actions/statistics-moving-mask
        DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
        NAMES    Reference_${_op}_1m_${_th}.grib
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_get_reference_data PROPERTIES
        FIXTURES_SETUP ${PREFIX}_${_op}_1m_${_th}_get_reference_data
    )

    ecbuild_add_test(
        TARGET  ${PREFIX}_${_op}_1m_${_th}_check_metadata
        COMMAND grib_compare
        ARGS    -H ${REFERENCE_FILE} ${RESULT_FILE}
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_check_metadata PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_${_op}_1m_${_th}_check_metadata
        FIXTURES_REQUIRED "${PREFIX}_${_op}_1m_${_th}_get_reference_data;${PREFIX}_${_op}_1m_${_th}_run"
    )

    ecbuild_add_test(
        TARGET  ${PREFIX}_${_op}_1m_${_th}_check_values
        COMMAND grib_compare
        ARGS    ${REFERENCE_FILE} ${RESULT_FILE}
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_check_values PROPERTIES
        FIXTURES_REQUIRED ${PREFIX}_${_op}_1m_${_th}_check_metadata
    )


    ### With restart
    set(RESULT_FILE Result_${_op}_1m_${_th}_restart.grib)

    ecbuild_add_test(
        TARGET  ${PREFIX}_${_op}_1m_${_th}_restart_clean_dir
        COMMAND ${CMAKE_COMMAND}
        ARGS    -E remove_directory ${CMAKE_CURRENT_BINARY_DIR}/Restart_${OPERATION}_${THRESHOLD}
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_restart_clean_dir PROPERTIES
        FIXTURES_SETUP ${PREFIX}_${_op}_1m_${_th}_restart_clean_dir
    )

    ecbuild_add_test(
        TARGET      ${PREFIX}_${_op}_1m_${_th}_restart_0-240_run
        ENVIRONMENT OPERATION=${OPERATION} THRESHOLD=${THRESHOLD} RESULT_FILE=${RESULT_FILE} READ_RESTART=false WRITE_RESTART=true RESTART_ID=none
        COMMAND     multio-feed
        ARGS        ${CMAKE_CURRENT_BINARY_DIR}/Original_moving_mask.grib --decode --stepRange=0-240 --plans=${CMAKE_CURRENT_SOURCE_DIR}/test-${_op}-moving-mask.yaml
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_restart_0-240_run PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_${_op}_1m_${_th}_restart_0-240_run
        FIXTURES_REQUIRED "${PREFIX}_get_input_data;${PREFIX}_${_op}_restart_clean_dir"
    )

    ecbuild_add_test(
        TARGET      ${PREFIX}_${_op}_1m_${_th}_restart_240-360_run
        ENVIRONMENT OPERATION=${OPERATION} THRESHOLD=${THRESHOLD} RESULT_FILE=${RESULT_FILE} READ_RESTART=true WRITE_RESTART=true RESTART_ID=latest
        COMMAND     multio-feed
        ARGS        ${CMAKE_CURRENT_BINARY_DIR}/Original_moving_mask.grib --decode --stepRange=240-360 --plans=${CMAKE_CURRENT_SOURCE_DIR}/test-${_op}-moving-mask.yaml
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_restart_240-360_run PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_${_op}_1m_${_th}_restart_240-360_run
        FIXTURES_REQUIRED ${PREFIX}_${_op}_1m_${_th}_restart_0-240_run
    )

    ecbuild_add_test(
        TARGET      ${PREFIX}_${_op}_1m_${_th}_restart_360-744_run
        ENVIRONMENT OPERATION=${OPERATION} THRESHOLD=${THRESHOLD} RESULT_FILE=${RESULT_FILE} READ_RESTART=true WRITE_RESTART=false RESTART_ID=latest
        COMMAND     multio-feed
        ARGS        ${CMAKE_CURRENT_BINARY_DIR}/Original_moving_mask.grib --decode --stepRange=360-744 --plans=${CMAKE_CURRENT_SOURCE_DIR}/test-${_op}-moving-mask.yaml
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_restart_360-744_run PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_${_op}_1m_${_th}_restart_360-744_run
        FIXTURES_REQUIRED ${PREFIX}_${_op}_1m_${_th}_restart_240-360_run
    )

    ecbuild_add_test(
        TARGET  ${PREFIX}_${_op}_1m_${_th}_restart_check_metadata
        COMMAND grib_compare
        ARGS    -H ${REFERENCE_FILE} ${RESULT_FILE}
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_restart_check_metadata PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_${_op}_1m_${_th}_restart_check_metadata
        FIXTURES_REQUIRED "${PREFIX}_${_op}_1m_${_th}_get_reference_data;${PREFIX}_${_op}_1m_${_th}_restart_360-744_run"
    )

    ecbuild_add_test(
        TARGET  ${PREFIX}_${_op}_1m_${_th}_restart_check_values
        COMMAND grib_compare
        ARGS    ${REFERENCE_FILE} ${RESULT_FILE}
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_restart_check_values PROPERTIES
        FIXTURES_REQUIRED ${PREFIX}_${_op}_1m_${_th}_restart_check_metadata
    )

endforeach()
endforeach()
