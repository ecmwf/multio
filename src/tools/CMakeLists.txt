ecbuild_add_executable(

    TARGET multio-play-journal

    SOURCES
        multio-play-journal.cc

    INCLUDES
        ${MULTIO_INCLUDE_DIRS}
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        ${MULTIO_HERMES}
        ${MULTIO_FDB5}
        ${MULTIO_FDB4}
        multio
        metkit
)

ecbuild_add_executable(

    TARGET multio-read-journal

    SOURCES
        multio-read-journal.cc

    INCLUDES
        ${MULTIO_INCLUDE_DIRS}
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        multio
)

ecbuild_add_executable(

    NOINSTALL

    TARGET multx

    SOURCES
        multx.cc

    INCLUDES
        ${MULTIO_INCLUDE_DIRS}
        ${METKIT_INCLUDE_DIRS}
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        ${MULTIO_FDB5}
        ${MULTIO_FDB4}
        multio
        metkit
        eckit_option )

