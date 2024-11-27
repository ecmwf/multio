set(MULTIOM_HOOKS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + hooks sources: ${MULTIOM_HOOKS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

#
# Add Subdirectories
add_subdirectory( options )
add_subdirectory( debug )
add_subdirectory( log )
add_subdirectory( trace )
add_subdirectory( paranoid_profile )

set( MULTIOM_HOOKS_MAIN_SOURCES
  ${MULTIOM_HOOKS_DIR}/hooks_mod.F90
)

# Collect source files in module2
set( MULTIOM_HOOKS_SOURCES
  ${MULTIOM_HOOKS_MAIN_SOURCES}
  ${MULTIOM_HOOKS_OPTIONS_SOURCES}
  ${MULTIOM_HOOKS_DEBUG_SOURCES}
  ${MULTIOM_HOOKS_LOG_SOURCES}
  ${MULTIOM_HOOKS_TRACE_SOURCES}
  ${MULTIOM_HOOKS_PARANOID_PROFILE_SOURCES}
  CACHE INTERNAL "List of all sources in hooks directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_HOOKS_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()