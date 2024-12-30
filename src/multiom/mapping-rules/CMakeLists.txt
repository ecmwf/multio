set(MULTIOM_MAPPING_RULES_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + mapping_rules sources: ${MULTIOM_MAPPING_RULES_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


set( MULTIOM_MAPPING_RULES_MAIN_SOURCES
  ${MULTIOM_MAPPING_RULES_DIR}/mapping_rule_mod.F90
  ${MULTIOM_MAPPING_RULES_DIR}/mapping_rules_factory_mod.F90
  ${MULTIOM_MAPPING_RULES_DIR}/mapping_rules_collection_mod.F90
  ${MULTIOM_MAPPING_RULES_DIR}/cached_mapper_collection_mod.F90
  ${MULTIOM_MAPPING_RULES_DIR}/cached_mapper_mod.F90
  ${MULTIOM_MAPPING_RULES_DIR}/mapping_cache_mod.F90
  ${MULTIOM_MAPPING_RULES_DIR}/mapping_utils_mod.F90
  # ${MULTIOM_MAPPING_RULES_DIR}/mapping_options_mod.F90
)

# Collect source files in module2
set( MULTIOM_MAPPING_RULES_SOURCES
  ${MULTIOM_MAPPING_RULES_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in mapping_rules directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_MAPPING_RULES_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()