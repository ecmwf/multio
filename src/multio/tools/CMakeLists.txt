
ecbuild_add_executable( TARGET    multio-hammer
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   multio-hammer.cc MultioTool.cc
                        LIBS      multio multio-server)

ecbuild_add_executable( TARGET    multio-replay
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   multio-replay.cc MultioTool.cc
                        LIBS      multio multio-server)

ecbuild_add_executable( TARGET    multio-replay-nemo-capi-initfilepath
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   multio-replay-nemo-capi.cc MultioTool.cc
                        LIBS      multio multio-server multio-api)
target_compile_definitions(multio-replay-nemo-capi-initfilepath PUBLIC "INIT_BY_FILEPATH" "EXPLICIT_MPI")

ecbuild_add_executable( TARGET    multio-replay-nemo-capi-initdefault-empi
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   multio-replay-nemo-capi.cc MultioTool.cc
                        LIBS      multio multio-server multio-api)
target_compile_definitions(multio-replay-nemo-capi-initdefault-empi PUBLIC "INIT_BY_ENV" "EXPLICIT_MPI")

ecbuild_add_executable( TARGET    multio-replay-nemo-capi-initdefault-impi
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   multio-replay-nemo-capi.cc MultioTool.cc
                        LIBS      multio multio-server multio-api)
target_compile_definitions(multio-replay-nemo-capi-initdefault-impi PUBLIC "INIT_BY_ENV" "IMPLICIT_MPI")

ecbuild_add_executable( TARGET    multio-replay-nemo-capi-initdefault-impi-custom
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   multio-replay-nemo-capi.cc MultioTool.cc
                        LIBS      multio multio-server multio-api)
target_compile_definitions(multio-replay-nemo-capi-initdefault-impi-custom PUBLIC "INIT_BY_ENV" "IMPLICIT_MPI" "SPECIFIC_MPI_GROUP=custom")

ecbuild_add_executable( TARGET    multio-replay-nemo-capi-initmpi-empi
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   multio-replay-nemo-capi.cc MultioTool.cc
                        LIBS      multio multio-server multio-api)
target_compile_definitions(multio-replay-nemo-capi-initmpi-empi PUBLIC "INIT_BY_MPI" "EXPLICIT_MPI")

ecbuild_add_executable( TARGET    multio-replay-nemo-capi-initmpi-impi
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   multio-replay-nemo-capi.cc MultioTool.cc
                        LIBS      multio multio-server multio-api)
target_compile_definitions(multio-replay-nemo-capi-initmpi-impi PUBLIC "INIT_BY_MPI" "IMPLICIT_MPI")


ecbuild_add_executable( TARGET    multio-probe
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   multio-probe.cc MultioTool.cc
                        LIBS      multio multio-server)

ecbuild_add_executable( TARGET    multio-encode-ocean
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   multio-encode-ocean.cc MultioTool.cc
                        LIBS      multio multio-server)

ecbuild_add_executable( TARGET    multio-maestro-syphon
                        CONDITION HAVE_MULTIO_SERVER AND HAVE_MAESTRO
                        SOURCES   multio-maestro-syphon.cc MultioTool.cc
                        LIBS      multio maestro multio-maestro)

ecbuild_add_executable( TARGET    multio-pool-manager
                        CONDITION HAVE_MULTIO_SERVER AND HAVE_MAESTRO
                        SOURCES   multio-pool-manager.cc MultioTool.cc
                        LIBS      multio maestro multio-maestro)

ecbuild_add_executable( TARGET    multio-maestro-instigator
                        CONDITION HAVE_MULTIO_SERVER AND HAVE_MAESTRO
                        SOURCES   multio-maestro-instigator.cc MultioTool.cc
                        LIBS      multio maestro multio-maestro)

if( HAVE_MULTIO_SERVER AND HAVE_FORTRAN )
    ecbuild_enable_fortran( REQUIRED MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/module)
    find_package(fckit)

    if( fckit_FOUND )
        # TODO ADD SOME FCKIT condition
        ecbuild_add_executable( TARGET    multio-replay-nemo-fapi
                                CONDITION HAVE_MULTIO_SERVER AND HAVE_FORTRAN 
                                SOURCES   multio-replay-nemo-fapi.f90
                                LIBS      multio multio-server multio-api multio-fapi fckit
        )
    ENDIF( fckit_FOUND )
endif()


# set( CMAKE_Fortran_MODULE_DIRECTORY ${MPI_Fortran_MODULE_DIR} )

# set_target_properties(multio-replay-nemo-fapi PROPERTIES COMPILE_FLAGS "${MPI_Fortran_COMPILE_FLAGS}")
# set_target_properties(multio-replay-nemo-fapi PROPERTIES LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}")
