set(MULTIOM_HOOKS_LOG_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + hooks/log sources: ${MULTIOM_HOOKS_LOG_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

# Collect source files in module2
set( MULTIOM_HOOKS_LOG_SOURCES
  ${MULTIOM_HOOKS_LOG_DIR}/log_mod.F90
  ${MULTIOM_HOOKS_LOG_DIR}/log_utils_mod.F90
  CACHE INTERNAL "List of all sources in hooks/log directory"
)

# Optionally, message what has been appended for logging
foreach(source_file IN LISTS MULTIOM_HOOKS_LOG_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()