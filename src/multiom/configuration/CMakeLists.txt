set(MULTIOM_CONFIGURATION_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + configuration sources: ${MULTIOM_CONFIGURATION_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

# Collect source files in module2
set( MULTIOM_CONFIGURATION_SOURCES
  ${MULTIOM_CONFIGURATION_DIR}/yaml_core_utils_mod.F90
  ${MULTIOM_CONFIGURATION_DIR}/configuration_utils_mod.F90
#  ${MULTIOM_CONFIGURATION_DIR}/command_line_arguments_mod.F90
  ${MULTIOM_CONFIGURATION_DIR}/command_arguments_tokenizer_mod.F90
  CACHE INTERNAL "List of all sources in configuration directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_CONFIGURATION_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()