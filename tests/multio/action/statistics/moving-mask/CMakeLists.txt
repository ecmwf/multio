
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
