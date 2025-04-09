
set(PREFIX multio_tests_action_statistics_restart)

#
# Restart data
list( APPEND _statistics_restart_test_data
    "reduced_gg_pl_80_avg_grib2.tmpl"
    "standard_0_statistics_test_data.grib"
    "Reference_chunks_0_of_11.grib"
    "Reference_chunks_1_of_11.grib"
    "Reference_chunks_2_of_11.grib"
    "Reference_chunks_3_of_11.grib"
    "Reference_chunks_4_of_11.grib"
    "Reference_chunks_5_of_11.grib"
    "Reference_chunks_6_of_11.grib"
    "Reference_chunks_7_of_11.grib"
    "Reference_chunks_8_of_11.grib"
    "Reference_chunks_9_of_11.grib"
    "Reference_chunks_10_of_11.grib"
    "Reference_chunks_11_of_11.grib"
    "Reference_single_chunk.grib"
)

ecbuild_get_test_multidata(
    TARGET   ${PREFIX}_get_data
    DIRNAME  multio/tests/actions/statistics-restart
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES ${_statistics_restart_test_data}
)

#
# Run step 0
set(READ_RESTART false)
set(WRITE_RESTART true)
set(RESTART_MAIN_FOLDER "NamesAreNotGuaranteedToBeUniqueAcrossPlans_0")
set(RESULT_FILE "Result_chunks_0_of_11.grib")
set(RESTART_ID "none")

ecbuild_add_test(
    TARGET  ${PREFIX}_clean_dir
    COMMAND ${CMAKE_COMMAND}
    ARGS    -E remove_directory ${CMAKE_CURRENT_BINARY_DIR}/${RESTART_MAIN_FOLDER}
)

ecbuild_add_test(
    TARGET        ${PREFIX}_run_0-24_new_restart
    TEST_REQUIRES ${PREFIX}_get_data ${PREFIX}_clean_dir
    CONDITION     HAVE_ECKIT_CODEC
    ENVIRONMENT   READ_RESTART=${READ_RESTART} WRITE_RESTART=${WRITE_RESTART} RESTART_MAIN_FOLDER=${RESTART_MAIN_FOLDER}  RESULT_FILE=${RESULT_FILE}  RESTART_ID=${RESTART_ID}
    COMMAND       multio-feed
    ARGS          --stepRange=0-24 --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/test-restart.yaml standard_0_statistics_test_data.grib
)

# Set the number of steps and loop through them
set(NUM_STEPS 11)
set( prevStepRange "0-24" )

# Loop from 1 to NUM_STEPS
foreach(i RANGE 1 ${NUM_STEPS})
    math(EXPR start "${i} * 24 + 1")
    math(EXPR j "${i} + 1")
    math(EXPR end "${j} * 24")

    set(stepRange "${start}-${end}")

    set(READ_RESTART true)
    set(WRITE_RESTART true)
    set(RESTART_MAIN_FOLDER "NamesAreNotGuaranteedToBeUniqueAcrossPlans_0")
    set(RESULT_FILE "Result_chunks_${i}_of_11.grib")
    set(RESTART_ID "latest")

    # Echo the multio-feed command
    message(STATUS "multio-feed --stepRange=${stepRange} --decode --plans=test-restart.yaml source/standard_0_statistics_test_data.grib")

    # Run the multio-feed command
    ecbuild_add_test(
        TARGET        ${PREFIX}_run_${stepRange}_new_restart
        TEST_REQUIRES ${PREFIX}_run_${prevStepRange}_new_restart
        CONDITION     HAVE_ECKIT_CODEC
        ENVIRONMENT   READ_RESTART=${READ_RESTART} WRITE_RESTART=${WRITE_RESTART} RESTART_MAIN_FOLDER=${RESTART_MAIN_FOLDER}  RESULT_FILE=${RESULT_FILE}  RESTART_ID=${RESTART_ID}
        COMMAND       multio-feed
        ARGS          --stepRange=${stepRange} --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/test-restart.yaml standard_0_statistics_test_data.grib
    )

    set( prevStepRange "${stepRange}" )

endforeach()

#
# Single chunk
set(READ_RESTART false)
set(WRITE_RESTART false)
set(RESTART_MAIN_FOLDER "none")
set(RESULT_FILE "Result_single_chunk.grib")
set(RESTART_ID "none")

ecbuild_add_test(
    TARGET        ${PREFIX}_run_single_chunk_new_restart
    TEST_REQUIRES ${PREFIX}_get_data ${PREFIX}_clean_dir
    CONDITION     HAVE_ECKIT_CODEC
    ENVIRONMENT   READ_RESTART=${READ_RESTART} WRITE_RESTART=${WRITE_RESTART} RESTART_MAIN_FOLDER=${RESTART_MAIN_FOLDER}  RESULT_FILE=${RESULT_FILE}  RESTART_ID=${RESTART_ID}
    COMMAND       multio-feed
    ARGS          --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/test-restart.yaml standard_0_statistics_test_data.grib
)

ecbuild_add_test(
    TARGET        ${PREFIX}_check_metadata_single_chunk_new_restart
    TEST_REQUIRES ${PREFIX}_run_${stepRange}_new_restart ${PREFIX}_run_single_chunk_new_restart
    ARGS          -H "Result_single_chunk.grib" "Reference_single_chunk.grib"
    COMMAND       grib_compare
)

ecbuild_add_test(
    TARGET        ${PREFIX}_check_values_single_chunk_new_restart
    TEST_REQUIRES ${PREFIX}_run_${stepRange}_new_restart ${PREFIX}_run_single_chunk_new_restart
    ARGS          -P -T10 "Result_single_chunk.grib" "Reference_single_chunk.grib"
    COMMAND       grib_compare
)

#
# Final comparison
ecbuild_add_test(
    TARGET        ${PREFIX}_check_metadata_singleVSrestart_chunk_new_restart
    TEST_REQUIRES ${PREFIX}_run_${stepRange}_new_restart ${PREFIX}_run_single_chunk_new_restart
    ARGS          -H "Result_chunks_11_of_11.grib" "Result_single_chunk.grib"
    COMMAND       grib_compare
)

ecbuild_add_test(
    TARGET        ${PREFIX}_check_values_singleVSrestart_chunk_new_restart
    TEST_REQUIRES ${PREFIX}_run_${stepRange}_new_restart ${PREFIX}_run_single_chunk_new_restart
    ARGS          -P -T10 "Result_chunks_11_of_11.grib" "Result_single_chunk.grib"
    COMMAND       grib_compare
)
