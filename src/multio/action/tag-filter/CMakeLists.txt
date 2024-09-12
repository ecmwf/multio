
ecbuild_add_library(

    TARGET multio-action-tag-filter

    SOURCES
        TagFilter.cc
        TagFilter.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
