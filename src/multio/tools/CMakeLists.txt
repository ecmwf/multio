
list( APPEND _multio_server_tools
    "multio-hammer" "multio-probe" "multio-encode-ocean"
)

foreach( tool ${_multio_server_tools} )
    ecbuild_add_executable( TARGET ${tool}
                            SOURCES ${tool}.cc MultioTool.cc
                            LIBS multio-api )
endforeach()

list( APPEND _multio_maestro_tools
    "multio-maestro-syphon" "multio-pool-manager" "multio-maestro-instigator"
)

foreach( tool ${_multio_maestro_tools} )
    ecbuild_add_executable( TARGET ${tool}
                            CONDITION HAVE_MAESTRO
                            SOURCES ${tool}.cc MultioTool.cc
                            LIBS multio-maestro )
endforeach()

# multio tools for testing the legacy ifsio

ecbuild_add_executable( TARGET    multio-legacy
                        SOURCES   multio-legacy.cc MultioTool.cc
                        CONDITION HAVE_FDB5 AND HAVE_LEGACY_IFSIO
                        LIBS      multio )

ecbuild_add_executable( TARGET    multio-legacy-hammer
                        SOURCES   multio-legacy-hammer.cc MultioTool.cc
                        CONDITION HAVE_FDB5 AND HAVE_LEGACY_IFSIO
                        LIBS      multio )

# tools for testing the multio API

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

ecbuild_add_executable( TARGET    multio-feed
                        CONDITION HAVE_FDB5
                        SOURCES   multio-feed.cc MultioTool.cc
                        LIBS      multio-api)

# TODO ADD SOME FCKIT condition
ecbuild_add_executable( TARGET    multio-replay-nemo-fapi
                        CONDITION HAVE_FORTRAN
                        SOURCES   multio-replay-nemo-fapi.f90
                        LIBS      multio-fapi fckit MPI::MPI_Fortran)
