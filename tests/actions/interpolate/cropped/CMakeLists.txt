if(HAVE_MIR)

#
# Local variables to help code readability
# ========================================

set(TEST_BUILD_PATH ${INTERPOLATE_BUILD_PATH}/${testcase} )


#
# Configure the tests files and setup the tests cases
# ===================================================

foreach(parser ${_interpolate_tests_parsers_kind})
    foreach(grid ${_interpolate_tests_grids_kind})
        foreach(spacing ${_interpolate_tests_grid_spacings})
            #
            # Create the configuration files
            configure_file( ${parser}/${grid}${spacing}x${spacing}.yaml.in ${TEST_BUILD_PATH}/${parser}/${grid}${spacing}x${spacing}.yaml @ONLY )
            #
            # lover the name of the grid to construct the test name in lower-case
            string( TOLOWER         ${grid}  gridname    )
            string( REPLACE "-" "_" testname ${testcase} )
            string( REPLACE "-" "_" parser_ ${parser} )
            #
            # Definition of the name of thee testscase
            set( testCaseName ${INTERPOLATE_TEST_PREFIX}_run_${testname}_${parser_}_${gridname}_${spacing}x${spacing} )
            #
            # Set up the test case
            ecbuild_add_test(
                TARGET ${testCaseName}
                CONDITION HAVE_MIR
                COMMAND ${INTERPOLATE_EXE}
                ARGS ${testCaseName} ${grid} ${testcase} ${parser} "strict" ${spacing}
                TEST_DEPENDS ${_interpolate_tests_dependencies}
            )
            #
        endforeach()
    endforeach()
endforeach()

endif(HAVE_MIR)