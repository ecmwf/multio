set(MULTIOM_HOOKS_OPTIONS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + hooks/options sources: ${MULTIOM_HOOKS_OPTIONS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

# Collect source files in module2
set( MULTIOM_HOOKS_OPTIONS_SOURCES
  ${MULTIOM_HOOKS_OPTIONS_DIR}/options_mod.F90
  CACHE INTERNAL "List of all sources in hooks/options directory"
)

# Optionally, message what has been appended for optionsging
foreach(source_file IN LISTS MULTIOM_HOOKS_OPTIONS_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()