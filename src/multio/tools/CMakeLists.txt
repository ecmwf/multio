
ecbuild_add_executable( TARGET    multio-hammer
                        SOURCES   multio-hammer.cc MultioTool.cc
                        LIBS      multio-api)

ecbuild_add_executable( TARGET      multio-replay-nemo-capi-initfilepath
                        SOURCES     multio-replay-nemo-capi.cc MultioTool.cc
                        LIBS        multio-api
                        DEFINITIONS "INIT_BY_FILEPATH" "EXPLICIT_MPI")

ecbuild_add_executable( TARGET      multio-replay-nemo-capi-initdefault-empi
                        SOURCES     multio-replay-nemo-capi.cc MultioTool.cc
                        LIBS        multio-api
                        DEFINITIONS "INIT_BY_ENV" "EXPLICIT_MPI")

ecbuild_add_executable( TARGET      multio-replay-nemo-capi-initdefault-impi
                        SOURCES     multio-replay-nemo-capi.cc MultioTool.cc
                        LIBS        multio-api
                        DEFINITIONS "INIT_BY_ENV" "IMPLICIT_MPI")

ecbuild_add_executable( TARGET      multio-replay-nemo-capi-initdefault-impi-custom
                        SOURCES     multio-replay-nemo-capi.cc MultioTool.cc
                        LIBS        multio-api
                        DEFINITIONS "INIT_BY_ENV" "IMPLICIT_MPI" "SPECIFIC_MPI_GROUP=custom")

ecbuild_add_executable( TARGET      multio-replay-nemo-capi-initmpi-empi
                        SOURCES     multio-replay-nemo-capi.cc MultioTool.cc
                        LIBS        multio-api
                        DEFINITIONS "INIT_BY_MPI" "EXPLICIT_MPI")

ecbuild_add_executable( TARGET      multio-replay-nemo-capi-initmpi-impi
                        SOURCES     multio-replay-nemo-capi.cc MultioTool.cc
                        LIBS        multio-api
                        DEFINITIONS "INIT_BY_MPI" "EXPLICIT_MPI")

ecbuild_add_executable( TARGET    multio-probe
                        SOURCES   multio-probe.cc MultioTool.cc
                        LIBS      multio-api)

ecbuild_add_executable( TARGET    multio-encode-ocean
                        SOURCES   multio-encode-ocean.cc MultioTool.cc
                        LIBS      multio-api)

ecbuild_add_executable( TARGET    multio-maestro-syphon
                        CONDITION HAVE_MAESTRO
                        SOURCES   multio-maestro-syphon.cc MultioTool.cc
                        LIBS      multio-maestro)

ecbuild_add_executable( TARGET    multio-pool-manager
                        CONDITION HAVE_MAESTRO
                        SOURCES   multio-pool-manager.cc MultioTool.cc
                        LIBS      multio-maestro)

ecbuild_add_executable( TARGET    multio-maestro-instigator
                        CONDITION HAVE_MAESTRO
                        SOURCES   multio-maestro-instigator.cc MultioTool.cc
                        LIBS      multio-maestro)


if( CMAKE_SYSTEM_NAME MATCHES "Linux" AND CMAKE_CXX_COMPILER_ID MATCHES "GNU" )
    list( APPEND _link_libraries
        -Wl,--no-as-needed
    )
endif()

list( APPEND _link_libraries
    multio-action-aggregate
    multio-action-encode
    multio-action-mask
    multio-action-metadata-mapping
    multio-action-null
    multio-action-print
    multio-action-select
    multio-action-single-field-sink
    multio-action-sink
    multio-action-statistics
    multio-action-transport
    eckit_option
)

if( HAVE_MIR )
    list( APPEND _link_libraries
        multio-action-interpolate
    )
endif()

if ( HAVE_FDB5 )
    list( APPEND _link_libraries
        multio-fdb5
    )
endif()

ecbuild_add_executable( TARGET    multio-feed
                        CONDITION HAVE_FDB5
                        SOURCES   multio-feed.cc MultioTool.cc
                        LIBS      ${_link_libraries})

# TODO ADD SOME FCKIT condition
ecbuild_add_executable( TARGET    multio-replay-nemo-fapi
                        CONDITION HAVE_FORTRAN
                        SOURCES   multio-replay-nemo-fapi.f90
                        LIBS      multio-fapi fckit MPI::MPI_Fortran)
