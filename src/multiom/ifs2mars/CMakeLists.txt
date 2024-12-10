set(MULTIOM_IFS2MARS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + ifs2mars sources: ${MULTIOM_IFS2MARS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


set( MULTIOM_IFS2MARS_MAIN_SOURCES
  ${MULTIOM_IFS2MARS_DIR}/atm2mars_mod.F90
  ${MULTIOM_IFS2MARS_DIR}/wam2mars_mod.F90
)

# Collect source files in module2
set( MULTIOM_IFS2MARS_SOURCES
  ${MULTIOM_IFS2MARS_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in ifs2mars directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_IFS2MARS_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()