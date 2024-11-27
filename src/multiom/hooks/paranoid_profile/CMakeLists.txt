set(MULTIOM_HOOKS_PARANOID_PROFILE_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + hooks/paranoid_profile sources: ${MULTIOM_HOOKS_PARANOID_PROFILE_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

# Collect source files in module2
set( MULTIOM_HOOKS_PARANOID_PROFILE_SOURCES
  ${MULTIOM_HOOKS_PARANOID_PROFILE_DIR}/paranoid_profile_mod.F90
  CACHE INTERNAL "List of all sources in hooks/paranoid_profile directory"
)

# Optionally, message what has been appended for paranoid_profileging
foreach(source_file IN LISTS MULTIOM_HOOKS_PARANOID_PROFILE_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()