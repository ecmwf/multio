add_subdirectory(ifs)
add_subdirectory(nextGEMS)

if( HAVE_MIR )
add_subdirectory(interpolate)
endif( HAVE_MIR )
