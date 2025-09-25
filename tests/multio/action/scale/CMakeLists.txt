
set( PREFIX multio_tests_action_scale )

ecbuild_add_test(
    TARGET ${PREFIX}
    SOURCES Scale.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-scale
)
