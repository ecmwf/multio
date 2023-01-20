if(HAVE_MULTIO_SERVER)

add_subdirectory(ifs)
add_subdirectory(nextGEMS)

endif(HAVE_MULTIO_SERVER)

if( HAVE_MIR )
add_subdirectory(interpolate)
endif( HAVE_MIR )
