
set(PREFIX multio_tests_action_encode-mtg2)

# Set IFS_INSTALL_DIR to the Multio source root directory.
file(REAL_PATH "../../../../" IFS_INSTALL_DIR BASE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

ecbuild_add_test(
    TARGET ${PREFIX}_initialize
    SOURCES test_initialize.cc
    NO_AS_NEEDED
    LIBS multio-action-encode-mtg2 multio-action-sink
    ENVIRONMENT "IFS_INSTALL_DIR=${IFS_INSTALL_DIR}"
)


ecbuild_add_test(
    TARGET ${PREFIX}_with_sample_data
    SOURCES test_encode-mtg2_with_sample_data.cc
    NO_AS_NEEDED
    LIBS multio-action-encode-mtg2 multio-action-debug-sink
    ENVIRONMENT "IFS_INSTALL_DIR=${IFS_INSTALL_DIR}"
)


ecbuild_add_test(
    TARGET ${PREFIX}_cache_run
    SOURCES test_cache.cc
    NO_AS_NEEDED
    LIBS multio-action-encode-mtg2 multio-action-sink
    ENVIRONMENT "IFS_INSTALL_DIR=${IFS_INSTALL_DIR}"
)

ecbuild_add_test(
    TARGET        ${PREFIX}_cache_timespan_fs_compare
    TEST_REQUIRES ${PREFIX}_cache_run
    COMMAND       grib_compare
    ARGS          -f timespan-fs.grib timespan-fs-with-cache.grib
)

ecbuild_add_test(
    TARGET        ${PREFIX}_cache_timespan_3_6_compare
    TEST_REQUIRES ${PREFIX}_cache_run
    COMMAND       grib_compare
    ARGS          -f timespan-3-6.grib timespan-3-6-with-cache.grib
)
