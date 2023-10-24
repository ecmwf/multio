add_subdirectory(encode)

if( HAVE_MIR )
add_subdirectory(interpolate)
endif( HAVE_MIR )

add_subdirectory(statistics)
add_subdirectory(healpix-ring2nest)
