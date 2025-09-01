
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

ecbuild_add_test(
    TARGET ${PREFIX}_difference
    SOURCES Difference.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-statistics-mtg2
)

ecbuild_add_test(
    TARGET ${PREFIX}_inverse_difference
    SOURCES InverseDifference.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-statistics-mtg2
)

ecbuild_add_test(
    TARGET ${PREFIX}_accumulate
    SOURCES Accumulate.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-statistics-mtg2
)

ecbuild_add_test(
    TARGET ${PREFIX}_deaccumulate
    SOURCES DeAccumulate.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-statistics-mtg2
)

ecbuild_add_test(
    TARGET ${PREFIX}_flux_average
    SOURCES FluxAverage.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-statistics-mtg2
)

ecbuild_add_test(
    TARGET ${PREFIX}_fixed_window_flux_average
    SOURCES FixedWindowFluxAverage.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-statistics-mtg2
)

ecbuild_add_test(
    TARGET ${PREFIX}_instant
    SOURCES Instant.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-statistics-mtg2
)

ecbuild_add_test(
    TARGET ${PREFIX}_stddev
    SOURCES StdDev.cc
    NO_AS_NEEDED
    LIBS multio-action-debug-sink multio-action-statistics-mtg2
)
