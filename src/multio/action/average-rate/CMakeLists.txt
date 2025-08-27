ecbuild_add_library(

    TARGET multio-action-average-rate

    TYPE SHARED  # Due to reliance on factory self registration this library cannot be static

    SOURCES
        AverageRate.cc
        AverageRate.h

    PUBLIC_LIBS
        multio
)
