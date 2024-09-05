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

list( APPEND multio_util_srcs
    util/Mars2GribMetadataSetter.h
    util/Mars2GribMetadataSetter.cc
    util/DateTime.cc
    util/DateTime.h
    util/Grib2TimeUnit.cc
    util/Grib2TimeUnit.h
    util/Environment.cc
    util/Environment.h
    util/FailureHandling.cc
    util/FailureHandling.h
    util/PrecisionTag.cc
    util/PrecisionTag.h
    util/Metadata.cc
    util/Metadata.h
    util/Substitution.cc
    util/Substitution.h
    util/BinaryUtils.h
    util/MioGribHandle.h
    util/MioGribHandle.cc
    util/Hash.h
    util/Hash.cc
    util/Timing.h
)

list( APPEND multio_config_srcs
    config/ComponentConfiguration.cc
    config/ComponentConfiguration.h
    config/PlanConfiguration.cc
    config/PlanConfiguration.h
    config/PathConfiguration.h
    config/MultioConfiguration.cc
    config/MultioConfiguration.h
    config/MetadataMappings.cc
    config/MetadataMappings.h
)


list( APPEND multio_action_srcs
    action/ActionStatistics.cc
    action/ActionStatistics.h
    action/Action.cc
    action/Action.h
    action/ChainedAction.cc
    action/ChainedAction.h
    action/Plan.cc
    action/Plan.h
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
    domain/Mask.cc
    domain/Mask.h
    domain/MaskCompression.cc
    domain/MaskCompression.h
)

list( APPEND multio_message_srcs
    message/BaseMetadata.h
    message/BaseMetadata.cc
    message/Message.cc
    message/MessageHeader.cc
    message/Message.h
    message/Metadata.h
    message/Metadata.cc
    message/MetadataValue.h
    message/MetadataValue.cc
    message/MetadataTypes.h
    message/MetadataTypes.cc
    message/Glossary.cc
    message/Glossary.h
    message/MetadataException.cc
    message/MetadataException.h
    message/MetadataMatcher.cc
    message/MetadataMatcher.h
    message/SharedMetadata.cc
    message/SharedMetadata.h
    message/SharedPayload.cc
    message/SharedPayload.h
    message/SharedPayloadException.cc
    message/SharedPayloadException.h
    message/Parametrization.cc
    message/Parametrization.h
    message/Peer.cc
    message/Peer.h
    message/MetadataMapping.cc
    message/MetadataMapping.h
    message/PrehashedKey.h
)

list( APPEND multio_server_srcs
    server/Dispatcher.cc
    server/Dispatcher.h
    server/Listener.cc
    server/Listener.h
    server/MultioClient.cc
    server/MultioClient.h
    server/MultioServer.cc
    server/MultioServer.h
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

list( APPEND multio_transport_srcs
    transport/ThreadTransport.cc
    transport/ThreadTransport.h
    transport/MpiCommSetup.cc
    transport/MpiCommSetup.h
    transport/MpiStream.cc
    transport/MpiStream.h
    transport/MpiTransport.cc
    transport/MpiTransport.h
    transport/TcpTransport.cc
    transport/TcpTransport.h
    transport/Transport.cc
    transport/Transport.h
    transport/TransportRegistry.cc
    transport/TransportRegistry.h
    transport/TransportStatistics.cc
    transport/TransportStatistics.h
    transport/StreamPool.cc
    transport/StreamPool.h
)

list( APPEND multio_srcs
    ${multio_library_srcs}
    ${multio_util_srcs}
    ${multio_config_srcs}
    ${multio_action_srcs}
    ${multio_ifsio_srcs}
    ${multio_domain_srcs}
    ${multio_message_srcs}
    ${multio_server_srcs}
    ${multio_sink_srcs}
    ${multio_transport_srcs}
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
        metkit eckit eckit_mpi)

add_subdirectory(fdb5)
add_subdirectory(maestro)
add_subdirectory(action)
add_subdirectory(api)
add_subdirectory(tools)

if( HAVE_FORTRAN AND ECBUILD_INSTALL_FORTRAN_MODULES )
    install( DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/${CMAKE_CFG_INTDIR}
             DESTINATION module/${PROJECT_NAME}
             COMPONENT modules )
endif()
