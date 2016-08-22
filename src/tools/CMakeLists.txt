ecbuild_add_executable(

    TARGET playjournal

    SOURCES
        playjournal.cc

    INCLUDES
        ${MULTIO_INCLUDE_DIRS}
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        ${MULTIO_HERMES}
        ${MULTIO_FDB5}
        ${MULTIO_FDB4}
        multio
        gribpp
)

ecbuild_add_executable(

    TARGET readjournal

    SOURCES
        readjournal.cc

    INCLUDES
        ${MULTIO_INCLUDE_DIRS}
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        multio
)
