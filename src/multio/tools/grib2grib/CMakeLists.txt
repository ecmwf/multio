ecbuild_add_library(

    TARGET multio-grib2grib

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
        HAVE_GRIB1_TO_GRIB2

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

    TARGET multio-grib2grib-mpi

    TYPE SHARED

    SOURCES
        MpiUtils.cc
        MpiUtils.h
        OptionsUtilsMpi.cc
        MultioToolUtils.cc
        MultioToolUtils.h

    CONDITION
        HAVE_GRIB1_TO_GRIB2 AND eckit_HAVE_MPI

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}
        ${METKIT_INCLUDE_DIRS}

    PUBLIC_LIBS
        multio-grib2grib
        eckit
        eckit_mpi
)



ecbuild_add_executable(

    TARGET
        grib2grib-options-parser

    CONDITION
        HAVE_GRIB1_TO_GRIB2

    SOURCES
        grib2grib_options-parser.cc

    NO_AS_NEEDED

    LIBS
        multio-grib2grib
)


ecbuild_add_executable(

    TARGET
        grib2grib-data-distribution-test

    CONDITION
        HAVE_GRIB1_TO_GRIB2

    SOURCES
        grib2grib-data-distribution-test.cc

    NO_AS_NEEDED

    LIBS
        multio-grib2grib
)


ecbuild_add_executable(

    TARGET
        grib2grib-sink-test

    CONDITION
        HAVE_GRIB1_TO_GRIB2

    SOURCES
        grib2grib-sink-test.cc

    NO_AS_NEEDED

    LIBS
        multio-grib2grib
)


ecbuild_add_executable(

    TARGET
        distributed-grib-to-grib

    CONDITION
        HAVE_GRIB1_TO_GRIB2 AND eckit_HAVE_MPI

    SOURCES
        distributed-grib-to-grib.cc

    NO_AS_NEEDED

    LIBS
        multio-grib2grib-mpi
)
