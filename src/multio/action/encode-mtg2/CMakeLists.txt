ecbuild_add_library(
    TARGET multio-action-encode-mtg2

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        EncodeMtg2.cc
        EncodeMtg2.h
        EncodeMtg2Exception.cc
        EncodeMtg2Exception.h
        AtlasGeoSetter.h
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
        multiom/MultIOMRules.cc
        multiom/MultIOMRules.h
        rules/Matcher.h
        rules/Setter.h
        rules/Rule.h
        generated/InferPDT.h
        generated/InferPDTTest.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
        multiom-encoders
        eccodes
        atlas
)
