ecbuild_add_library(
    TARGET multio-mars2mars

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        Mars2MarsException.cc
        Mars2MarsException.h
        Rules.cc
        Rules.h
        rules/Matcher.h
        rules/ParamMatcher.h
        rules/Setter.h
        rules/Rule.h
        rules/Rule.cc

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
        eccodes
)
