ecbuild_add_library(

    TARGET multio-grib-to-mtg2

    TYPE SHARED

    SOURCES
        ../MultioTool.cc
        CodesHandleToEckitMessage.cc
        CodesHandleToEckitMessage.h
        GlobalContext.cc
        GlobalContext.h
        handleGribBoundaries.cc
        handleGribBoundaries.h
        ReaderContext.h
        OptionsUtils.cc
        OptionsUtils.h
        StageOutcomes.cc
        StageOutcomes.h
        Utils.cc
        Utils.h
        UnitOfWork.cc
        UnitOfWork.h
        WorkUnitLoadBalancer.cc
        WorkUnitLoadBalancer.h
        Sink.cc
        Sink.h
        Summary.cc
        Summary.h
        ProcessOneMessage.cc
        ProcessOneMessage.h
        ProcessOneUnitOfWork.cc
        ProcessOneUnitOfWork.h
        ProcessRankOwnedUnitsOfWork.cc
        ProcessRankOwnedUnitsOfWork.h
        stages/GribBasedFilter.cc
        stages/GribBasedFilter.h
        stages/Hacks.cc
        stages/Hacks.h
        stages/GribToMars.cc
        stages/GribToMars.h
        stages/MarsToMars.cc
        stages/MarsToMars.h
        stages/Overrides.cc
        stages/Overrides.h
        stages/MarsBasedFilter.cc
        stages/MarsBasedFilter.h
        stages/MarsToGrib.cc
        stages/MarsToGrib.h
        stages/PostEncodeValidation.cc
        stages/PostEncodeValidation.h
        stages/Grib2Fdb5.cc
        stages/Grib2Fdb5.h

    CONDITION
        HAVE_GRIB_TO_MTG2

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}
        ${METKIT_INCLUDE_DIRS}

    PUBLIC_LIBS
        multio
        multio-fdb5
        eckit
        metkit
        eccodes
)


ecbuild_add_library(

    TARGET multio-grib-to-mtg2-mpi

    TYPE SHARED

    SOURCES
        MpiUtils.cc
        MpiUtils.h
        OptionsUtilsMpi.cc
        MultioToolUtils.cc
        MultioToolUtils.h

    CONDITION
        HAVE_GRIB_TO_MTG2 AND eckit_HAVE_MPI

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}
        ${METKIT_INCLUDE_DIRS}

    PUBLIC_LIBS
        multio-grib-to-mtg2
        eckit
        eckit_mpi
)



ecbuild_add_executable(

    TARGET
        grib-to-mtg2-options-parser

    CONDITION
        HAVE_GRIB_TO_MTG2 AND HAVE_TESTS

    NOINSTALL

    SOURCES
        grib-to-mtg2-options-parser.cc

    NO_AS_NEEDED

    LIBS
        multio-grib-to-mtg2
)


ecbuild_add_executable(

    TARGET
        classify-mars2grib-testcases

    CONDITION
        HAVE_GRIB_TO_MTG2

    SOURCES
        classify-mars2grib-testcases.cc

    NO_AS_NEEDED

    LIBS
        multio-grib-to-mtg2
)


ecbuild_add_executable(

    TARGET
        grib-to-mtg2-data-distribution-test

    CONDITION
        HAVE_GRIB_TO_MTG2 AND HAVE_TESTS

    NOINSTALL

    SOURCES
        grib-to-mtg2-data-distribution-test.cc

    NO_AS_NEEDED

    LIBS
        multio-grib-to-mtg2
)


ecbuild_add_executable(

    TARGET
        grib-to-mtg2-sink-test

    CONDITION
        HAVE_GRIB_TO_MTG2 AND HAVE_TESTS

    NOINSTALL

    SOURCES
        grib-to-mtg2-sink-test.cc

    NO_AS_NEEDED

    LIBS
        multio-grib-to-mtg2
)


ecbuild_add_executable(

    TARGET
        distributed-grib-to-mtg2

    CONDITION
        HAVE_GRIB_TO_MTG2 AND eckit_HAVE_MPI

    SOURCES
        distributed-grib-to-mtg2.cc

    NO_AS_NEEDED

    LIBS
        multio-grib-to-mtg2-mpi
)


ecbuild_add_executable(

    TARGET
        scalar-grib-to-mtg2

    CONDITION
        HAVE_GRIB_TO_MTG2

    SOURCES
        scalar-grib-to-mtg2.cc

    NO_AS_NEEDED

    LIBS
        multio-grib-to-mtg2
)
