### config headers

ecbuild_generate_config_headers( DESTINATION ${INSTALL_INCLUDE_DIR}/multio )

configure_file( multio_config.h.in  multio_config.h  )
configure_file( multio_version.h.in  multio_version.h )

install(FILES
            ${CMAKE_CURRENT_BINARY_DIR}/multio_config.h
            ${CMAKE_CURRENT_BINARY_DIR}/multio_version.h
        DESTINATION
            ${INSTALL_INCLUDE_DIR}/multio )

configure_file( multio_version.cc.in  multio_version.cc )

### multio sources

list( APPEND multio_library_srcs
    LibMultio.cc
    LibMultio.h
    multio_version.cc
)

list( APPEND multio_action_srcs
    action/Aggregation.cc
    action/Aggregation.h
    action/Encode.cc
    action/Encode.h
    action/GribEncoder.cc
    action/GribEncoder.h
    action/GridInfo.cc
    action/GridInfo.h
    action/Operation.cc
    action/Operation.h
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
    action/Period.cc
    action/Period.h
    action/Plan.cc
    action/Plan.h
    action/TemporalStatistics.cc
    action/TemporalStatistics.h
)

list( APPEND multio_ifsio_srcs
    ifsio/ifsio_internals.cc
    ifsio/ifsio.cc
    ifsio/ifsio.h
    ifsio/EncodeBitsPerValue.cc
    ifsio/EncodeBitsPerValue.h
)

if( HAVE_LEGACY_IFSIO )
    list( APPEND multio_ifsio_srcs
        ifsio/ifsio_legacy.cc
        ifsio/ifsio_legacy.h
    )
endif()

list( APPEND multio_domain_srcs
    domain/Domain.cc
    domain/Domain.h
    domain/Mappings.cc
    domain/Mappings.h
)

list( APPEND multio_message_srcs
    message/Message.cc
    message/MessageContent.cc
    message/MessageHeader.cc
    message/Message.h
    message/Metadata.cc
    message/Metadata.h
    message/Peer.cc
    message/Peer.h
)

list( APPEND multio_sink_srcs
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

list( APPEND multio_srcs
    ${multio_library_srcs}
    ${multio_action_srcs}
    ${multio_ifsio_srcs}
    ${multio_domain_srcs}
    ${multio_message_srcs}
    ${multio_sink_srcs}
)

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

    PUBLIC_INCLUDES
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/src>
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>

    PRIVATE_INCLUDES
      ${METKIT_INCLUDE_DIRS}
      ${ECKIT_INCLUDE_DIRS}

    PUBLIC_LIBS
        metkit eckit )

add_subdirectory(fdb5)
add_subdirectory(maestro)
add_subdirectory(server)
add_subdirectory(tools)
