add_subdirectory( multio )

if (NOT CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
message(STATUS "Building multiom")
add_subdirectory( multiom )
else ()
message(WARNING "Skipping multiom because the GNU Fortran compiler is too slow.")
endif()
