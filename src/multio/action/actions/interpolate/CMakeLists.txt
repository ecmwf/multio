ecbuild_add_library(

    TARGET multio-action-interpolate

    SOURCES
        Interpolate.cc
        Interpolate.h
        parser/InterpolateParser.h
        parser/InterpolateParserAtlas.h
        parser/InterpolateParserPureForwarding.h
        parser/InterpolateParserFactory.h



    PRIVATE_INCLUDES
      ${MIR_INCLUDE_DIRS}
      ${ECKIT_INCLUDE_DIRS}

    CONDITION
      HAVE_MIR

    PUBLIC_LIBS
        multio
        mir
)
