
set( PREFIX multio_tests_action_statistics-mtg2_operation )

ecbuild_add_test(
    TARGET ${PREFIX}_average
    SOURCES Average.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-statistics-mtg2
)

ecbuild_add_test(
    TARGET ${PREFIX}_minimum
    SOURCES Minimum.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-statistics-mtg2
)

ecbuild_add_test(
    TARGET ${PREFIX}_maximum
    SOURCES Maximum.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-statistics-mtg2
)
