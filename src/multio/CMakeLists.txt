### config headers

ecbuild_generate_config_headers( DESTINATION ${INSTALL_INCLUDE_DIR}/multio )

configure_file( multio_config.h.in   multio_config.h  )
configure_file( multio_version.h.in  multio_version.h )

install(FILES
            ${CMAKE_CURRENT_BINARY_DIR}/multio_config.h
            ${CMAKE_CURRENT_BINARY_DIR}/multio_version.h
        DESTINATION
            ${INSTALL_INCLUDE_DIR}/multio )

configure_file( multio_version.cc.in  multio_version.cc )

### multio sources

list( APPEND multio_srcs
    multio_version.cc
    DataSink.cc
    DataSink.h
    FileSink.h
    FileSink.cc
    MultIO.h
    MultIO.cc
    FDB4Sink.h
    FDB4Sink.cc
    Journal.h
    Journal.cc
    JournalRecord.h
    JournalRecord.cc
    JournalReader.h
    JournalReader.cc
    ifsio.h
    ifsio.cc )


### multio library

ecbuild_add_library(

    TARGET multio

    INSTALL_HEADERS LISTED

    HEADER_DESTINATION
        ${INSTALL_INCLUDE_DIR}/multio

    SOURCES
        ${multio_srcs}

    GENERATED
        multio_version.cc

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        eckit )
