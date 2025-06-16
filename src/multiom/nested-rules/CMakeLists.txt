set(MULTIOM_NESTED_RULES_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + nested_rules sources: ${MULTIOM_NESTED_RULES_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


set( MULTIOM_NESTED_RULES_MAIN_SOURCES
  ${MULTIOM_NESTED_RULES_DIR}/nested_rules_tree_loader_mod.F90
  ${MULTIOM_NESTED_RULES_DIR}/nested_rules_linear_loader_mod.F90
  ${MULTIOM_NESTED_RULES_DIR}/nested_rule_mod.F90
  ${MULTIOM_NESTED_RULES_DIR}/nested_rules_factory_mod.F90
  ${MULTIOM_NESTED_RULES_DIR}/nested_rules_collection_mod.F90
)

# Collect source files in module2
set( MULTIOM_NESTED_RULES_SOURCES
  ${MULTIOM_NESTED_RULES_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in nested_rules directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_NESTED_RULES_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()