
find_package( Maestro )

ecbuild_add_library(

    TARGET multio-maestro

    SOURCES
        MaestroSink.cc
        MaestroSink.h

    CONDITION
        HAVE_MAESTRO

    LIBS
        multio
        maestro
)
