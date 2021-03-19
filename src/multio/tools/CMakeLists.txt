
ecbuild_add_executable( TARGET    multio-hammer
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   multio-hammer.cc MultioTool.cc
                        LIBS      multio multio-server)

ecbuild_add_executable( TARGET    multio-replay
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   multio-replay.cc MultioTool.cc
                        LIBS      multio multio-server)

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
                        LIBS      multio maestro)
