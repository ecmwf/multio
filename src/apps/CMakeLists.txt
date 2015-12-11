ecbuild_add_executable(
    TARGET mx
    SOURCES mx.cc
    INCLUDES ${ECKIT_INCLUDE_DIRS}
    LIBS multio
)
