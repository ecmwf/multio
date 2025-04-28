
set( PREFIX multio_tests_action_statistics-mtg2 )

ecbuild_add_test(
    TARGET ${PREFIX}_single_flush
    SOURCES SingleFlush.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-statistics-mtg2
)

ecbuild_add_test(
    TARGET ${PREFIX}_single_field_single_flush
    SOURCES SingleFieldSingleFlush.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-statistics-mtg2
)
