ecbuild_add_library(

    TARGET multio-action-interpolate

    SOURCES
        Interpolate.cc
        Interpolate.h
        Parser.h

    PRIVATE_INCLUDES
      ${MIR_INCLUDE_DIRS}
      ${ECKIT_INCLUDE_DIRS}

    CONDITION
      HAVE_MIR

    PUBLIC_LIBS
        multio
        mir
)
