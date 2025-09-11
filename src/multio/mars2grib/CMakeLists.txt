ecbuild_add_library(
    TARGET multio-mars2grib

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        Mars2GribException.cc
        Mars2GribException.h
        MarsCachedKeys.h
        Encoder.cc
        Encoder.h
        LegacyEncoderConf.h
        Grib2Layout.h
        Rules.cc
        Rules.h
        api/RawAPI.cc
        api/RawAPI.h
        api/StableAPI.cc
        api/StableAPI.h
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
        grib2/Geometry.cc
        grib2/Geometry.h
        grib2/Level.cc
        grib2/Level.h
        grib2/Satellite.cc
        grib2/Satellite.h
        grib2/DirFreq.cc
        grib2/DirFreq.h
        grib2/Time.cc
        grib2/Time.h
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
