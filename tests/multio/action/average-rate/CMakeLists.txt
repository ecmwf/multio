
set( PREFIX multio_tests_action_average-rate )

ecbuild_add_test(
    TARGET ${PREFIX}
    SOURCES AverageRate.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-average-rate
)
