
set( PREFIX multio_tests_action_mars2mars )

ecbuild_add_test(
    TARGET ${PREFIX}
    CONDITION metkit_HAVE_MARS2MARS
    SOURCES Mars2Mars.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-mars2mars
)

ecbuild_add_test(
    TARGET ${PREFIX}_encode
    CONDITION metkit_HAVE_MARS2MARS
    SOURCES Mars2MarsEncode.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-mars2mars multio-action-encode-mtg2 eccodes
)
