
set(PREFIX multio_tests_action_statistics_moving_mask)

ecbuild_get_test_multidata(
    TARGET   ${PREFIX}_get_input_data
    DIRNAME  multio/tests/actions/statistics-moving-mask
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES
        Original_6h.grib
        Statistics_grib2.tmpl
)
set_tests_properties(${PREFIX}_get_input_data PROPERTIES
    FIXTURES_SETUP ${PREFIX}_get_input_data
)

### Without restart
foreach(_op avg max min)
foreach(_th 10d all any)

    ecbuild_add_test(
        TARGET ${PREFIX}_${_op}_1m_${_th}_run
        COMMAND multio-feed
        ARGS ${CMAKE_CURRENT_BINARY_DIR}/Original_6h.grib --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/${_op}_1m_${_th}.yaml
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
        ARGS    -H Reference_${_op}_1m_${_th}.grib Result_${_op}_1m_${_th}.grib
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_check_metadata PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_${_op}_1m_${_th}_check_metadata
        FIXTURES_REQUIRED "${PREFIX}_${_op}_1m_${_th}_get_reference_data;${PREFIX}_${_op}_1m_${_th}_run"
    )

    ecbuild_add_test(
        TARGET  ${PREFIX}_${_op}_1m_${_th}_check_values
        COMMAND grib_compare
        ARGS    Reference_${_op}_1m_${_th}.grib Result_${_op}_1m_${_th}.grib
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_check_values PROPERTIES
        FIXTURES_REQUIRED ${PREFIX}_${_op}_1m_${_th}_check_metadata
    )

endforeach()
endforeach()


### With restart
foreach(_op avg min max)
foreach(_th 10d)

    ecbuild_add_test(
        TARGET  ${PREFIX}_${_op}_restart_clean_dir
        COMMAND ${CMAKE_COMMAND}
        ARGS    -E remove_directory ${CMAKE_CURRENT_BINARY_DIR}/${_op}MovingMaskRestart
    )
    set_tests_properties(${PREFIX}_${_op}_restart_clean_dir PROPERTIES
        FIXTURES_SETUP ${PREFIX}_restart_clean_dir
    )

    ecbuild_add_test(
        TARGET      ${PREFIX}_${_op}_1m_${_th}_restart_0-240_run
        ENVIRONMENT READ_RESTART=false WRITE_RESTART=true RESTART_ID=none
        COMMAND     multio-feed
        ARGS        ${CMAKE_CURRENT_BINARY_DIR}/Original_mm.grib --decode --stepRange=0-240 --plans=${CMAKE_CURRENT_SOURCE_DIR}/${_op}_1m_${_th}_restart.yaml
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_restart_0-240_run PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_${_op}_1m_${_th}_restart_0-240_run
        FIXTURES_REQUIRED "${PREFIX}_get_input_data;${PREFIX}_${_op}_restart_clean_dir"
    )

    ecbuild_add_test(
        TARGET      ${PREFIX}_${_op}_1m_${_th}_restart_240-360_run
        ENVIRONMENT READ_RESTART=true WRITE_RESTART=true RESTART_ID=latest
        COMMAND     multio-feed
        ARGS        ${CMAKE_CURRENT_BINARY_DIR}/Original_mm.grib --decode --stepRange=240-360 --plans=${CMAKE_CURRENT_SOURCE_DIR}/${_op}_1m_${_th}_restart.yaml
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_restart_240-360_run PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_${_op}_1m_${_th}_restart_240-360_run
        FIXTURES_REQUIRED ${PREFIX}_${_op}_1m_${_th}_restart_0-240_run
    )

    ecbuild_add_test(
        TARGET      ${PREFIX}_${_op}_1m_${_th}_restart_360-744_run
        ENVIRONMENT READ_RESTART=true WRITE_RESTART=false RESTART_ID=latest
        COMMAND     multio-feed
        ARGS        ${CMAKE_CURRENT_BINARY_DIR}/Original_mm.grib --decode --stepRange=360-744 --plans=${CMAKE_CURRENT_SOURCE_DIR}/${_op}_1m_${_th}_restart.yaml
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_restart_360-744_run PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_${_op}_1m_${_th}_restart_360-744_run
        FIXTURES_REQUIRED ${PREFIX}_${_op}_1m_${_th}_restart_240-360_run
    )

    ecbuild_add_test(
        TARGET  ${PREFIX}_${_op}_1m_${_th}_restart_check_metadata
        COMMAND grib_compare
        ARGS    -H Reference_${_op}_1m_${_th}.grib Result_${_op}_1m_${_th}_restart.grib
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_restart_check_metadata PROPERTIES
        FIXTURES_SETUP    ${PREFIX}_${_op}_1m_${_th}_restart_check_metadata
        FIXTURES_REQUIRED "${PREFIX}_${_op}_1m_${_th}_get_reference_data;${PREFIX}_${_op}_1m_${_th}_restart_360-744_run"
    )

    ecbuild_add_test(
        TARGET  ${PREFIX}_${_op}_1m_${_th}_restart_check_values
        COMMAND grib_compare
        ARGS    Reference_${_op}_1m_${_th}.grib Result_${_op}_1m_${_th}_restart.grib
    )
    set_tests_properties(${PREFIX}_${_op}_1m_${_th}_restart_check_values PROPERTIES
        FIXTURES_REQUIRED ${PREFIX}_${_op}_1m_${_th}_restart_check_metadata
    )

endforeach()
endforeach()
