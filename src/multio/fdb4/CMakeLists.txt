ecbuild_add_library(

    TARGET multio-fdb4

    INSTALL_HEADERS LISTED

    HEADER_DESTINATION
        ${INSTALL_INCLUDE_DIR}/multio/fdb4

    SOURCES
        FDB4Sink.h
        FDB4Sink.cc

    PRIVATE_INCLUDES
        ${MULTIO_INCLUDE_DIRS}
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        multio fdb )
