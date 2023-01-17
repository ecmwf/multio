if(HAVE_MIR)

#
# Local variables to help code readability
# ========================================

set(TEST_BUILD_PATH ${INTERPOLATE_BUILD_PATH}/${testcase} )


#
# This test case requires another grid
# ====================================

list( APPEND _interpolate_tests_grids_kind
    "Regularll"
)

#
# Configure and the tests files and setup the tests
# =================================================

foreach(grid ${_interpolate_tests_grids_kind})
    #
    # Create the configuration files
    configure_file( ${grid}.yaml.in ${TEST_BUILD_PATH}/${grid}.yaml @ONLY )
    #
    # lover the name of the grid to construct the test name in lower-case
    string( TOLOWER         ${grid}  gridname    )
    string( REPLACE "-" "_" testname ${testcase} )
    #
    # Definition of the name of thee testscase
    set( testCaseName ${INTERPOLATE_TEST_PREFIX}_run_${testname}_${gridname} )
    #
    # Set up the test case
    ecbuild_add_test(
        TARGET ${testCaseName}
        CONDITION HAVE_MIR
        COMMAND ${INTERPOLATE_EXE}
        ARGS ${testCaseName} ${grid} ${testcase} "none" "strict"
        TEST_DEPENDS ${_interpolate_tests_dependencies}
    )
    #
endforeach()


endif(HAVE_MIR)