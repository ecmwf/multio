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
    DataSink.cc
    DataSink.h
    FileSink.cc
    FileSink.h
    IOStats.cc
    IOStats.h
    LibMultio.cc
    LibMultio.h
    MultIO.cc
    MultIO.h
    Trigger.cc
    Trigger.h
    ifsio_internals.cc
    ifsio.cc
    ifsio.h
    ifsio_legacy.cc
    ifsio_legacy.h
    multio_version.cc
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

    PRIVATE_INCLUDES
      ${METKIT_INCLUDE_DIRS}
      ${ECKIT_INCLUDE_DIRS}

    LIBS
        metkit eckit )

add_subdirectory(fdb5)
add_subdirectory(attic)
add_subdirectory(server)

ecbuild_add_executable( TARGET    multio-sink
                        SOURCES   tools/multio-sink.cc
                        CONDITION HAVE_FDB5
                        LIBS      multio multio-fdb5 eckit_option)
