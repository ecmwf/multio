ecbuild_add_library(
    TARGET multio-mars2grib

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        Mars2GribException.cc
        Mars2GribException.h
        MarsCachedKeys.h
        Options.h
        EncoderCache.cc
        EncoderCache.h
        EncoderConf.h
        Rules.cc
        Rules.h
        multiom/MultIOMDict.cc
        multiom/MultIOMDict.h
        multiom/MultIOMRawEncoder.cc
        multiom/MultIOMRawEncoder.h
        rules/ParamMatcher.h
        rules/Matcher.h
        rules/Setter.h
        rules/Rule.h
        rules/Rule.cc
        sections/SectionSetter.cc
        sections/SectionSetter.h
        sections/SectionTypes.cc
        sections/SectionTypes.h
        sections/Level.cc
        sections/Level.h
        generated/InferPDT.h
        generated/InferPDTTest.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
        multiom-encoders
        eccodes
        atlas # To infer geo .... will be replaced by eckit-geo in future
)
