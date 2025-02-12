set(MULTIOM_OPERATIONS_STRINGOP_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + operations_stringop sources: ${MULTIOM_OPERATIONS_STRINGOP_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

#
# Add Subdirectories

set( MULTIOM_OPERATIONS_STRINGOP_MAIN_SOURCES
  ${MULTIOM_OPERATIONS_STRINGOP_DIR}/stringop_base_mod.F90
  ${MULTIOM_OPERATIONS_STRINGOP_DIR}/assignment_msg_string_mod.F90
  ${MULTIOM_OPERATIONS_STRINGOP_DIR}/assignment_par_string_mod.F90
)

# Collect source files in module2
set( MULTIOM_OPERATIONS_STRINGOP_SOURCES
  ${MULTIOM_OPERATIONS_STRINGOP_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in operations_stringop directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_OPERATIONS_STRINGOP_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()