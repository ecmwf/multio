
list( APPEND _multio_server_tools
    "multio-hammer" "multio-probe" "multio-encode-ocean"
)

foreach( tool ${_multio_server_tools} )
    ecbuild_add_executable( TARGET ${tool}
                            SOURCES ${tool}.cc MultioTool.cc
                            LIBS multio-api )
endforeach()

list( APPEND _multio_maestro_tools
    "multio-pool-manager" "multio-maestro-instigator"
)

foreach( tool ${_multio_maestro_tools} )
    ecbuild_add_executable( TARGET ${tool}
                            CONDITION HAVE_MAESTRO
                            SOURCES ${tool}.cc MultioTool.cc
                            LIBS multio-maestro )
endforeach()

ecbuild_add_executable( TARGET multio-maestro-syphon
                        CONDITION HAVE_MAESTRO AND HAVE_PGEN
                        SOURCES multio-maestro-syphon.cc MultioTool.cc
                        LIBS multio-maestro pgen)


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

ecbuild_add_executable( TARGET      multio-replay-nemo-capi
                        SOURCES     multio-replay-nemo-capi.cc MultioTool.cc
                        LIBS        multio-api )

ecbuild_add_executable( TARGET    multio-feed
                        CONDITION HAVE_FDB5
                        SOURCES   multio-feed.cc MultioTool.cc
                        LIBS      multio-api )

# TODO ADD SOME FCKIT condition
ecbuild_add_executable( TARGET    multio-replay-nemo-fapi
                        CONDITION HAVE_FORTRAN
                        SOURCES   multio-replay-nemo-fapi.f90
                        LIBS      multio-fapi fckit MPI::MPI_Fortran )

# TODO: add atlas condition/dependency
ecbuild_add_executable( TARGET    multio-generate-grib-template
                        SOURCES   multio-generate-grib-template.cc MultioTool.cc
                        LIBS      multio atlas eccodes eckit )

ecbuild_add_executable( TARGET    multio-convert-trace-log
                        SOURCES   multio-convert-trace-log.cc MultioTool.cc
                        LIBS      multio eckit )

ecbuild_add_executable( TARGET      multio-replay-nemo-capi-partial-agg
                        SOURCES     multio-replay-nemo-capi-partial-agg.cc MultioTool.cc
                        LIBS        multio-api )
