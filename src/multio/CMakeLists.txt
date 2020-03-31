### config headers

ecbuild_generate_config_headers( DESTINATION ${INSTALL_INCLUDE_DIR}/multio )

configure_file( library/multio_config.h.in   library/multio_config.h  )
configure_file( library/multio_version.h.in  library/multio_version.h )

install(FILES
            ${CMAKE_CURRENT_BINARY_DIR}/library/multio_config.h
            ${CMAKE_CURRENT_BINARY_DIR}/library/multio_version.h
        DESTINATION
            ${INSTALL_INCLUDE_DIR}/multio/library )

configure_file( library/multio_version.cc.in  library/multio_version.cc )

### multio sources

list( APPEND multio_srcs
    action/Aggregation.cc
    action/Aggregation.h
    action/Encode.cc
    action/Encode.h
    action/Print.cc
    action/Print.h
    action/Select.cc
    action/Select.h
    action/Sink.cc
    action/Sink.h
    action/SingleFieldSink.cc
    action/SingleFieldSink.h
    action/Statistics.cc
    action/Statistics.h
    action/Null.cc
    action/Null.h
    action/Action.cc
    action/Action.h
    action/Plan.cc
    action/Plan.h
    ifsio/ifsio_internals.cc
    ifsio/ifsio.cc
    ifsio/ifsio.h
    library/LibMultio.cc
    library/LibMultio.h
    library/multio_version.cc
    domain/Domain.cc
    domain/Domain.h
    domain/Mappings.cc
    domain/Mappings.h
    message/Message.cc
    message/MessageContent.cc
    message/MessageHeader.cc
    message/Message.h
    message/Metadata.h
    message/Peer.cc
    message/Peer.h
    sink/DataSink.cc
    sink/DataSink.h
    sink/FileSink.cc
    sink/FileSink.h
    sink/IOStats.cc
    sink/IOStats.h
    sink/MultIO.cc
    sink/MultIO.h
    sink/Trigger.cc
    sink/Trigger.h
)

if( HAVE_LEGACY_IFSIO )
    list( APPEND multio_srcs
        ifsio_legacy.cc
        ifsio_legacy.h
    )
endif()

### multio library

ecbuild_add_library(

    TARGET multio

    INSTALL_HEADERS LISTED

    HEADER_DESTINATION
        ${INSTALL_INCLUDE_DIR}/multio

    SOURCES
        ${multio_srcs}

    GENERATED
        library/multio_version.cc

    PRIVATE_INCLUDES
      ${METKIT_INCLUDE_DIRS}
      ${ECKIT_INCLUDE_DIRS}

    LIBS
        metkit eckit )

add_subdirectory(fdb5)
add_subdirectory(attic)
add_subdirectory(server)
