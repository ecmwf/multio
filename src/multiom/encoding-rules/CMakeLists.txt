set(MULTIOM_ENCODING_RULES_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + encoding_rules sources: ${MULTIOM_ENCODING_RULES_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


set( MULTIOM_ENCODING_RULES_MAIN_SOURCES
  ${MULTIOM_ENCODING_RULES_DIR}/encoding_rules_tree_loader_mod.F90
  ${MULTIOM_ENCODING_RULES_DIR}/cached_encoder_collection_mod.F90
  ${MULTIOM_ENCODING_RULES_DIR}/cached_encoder_mod.F90
  ${MULTIOM_ENCODING_RULES_DIR}/encoding_rule_mod.F90
  ${MULTIOM_ENCODING_RULES_DIR}/encoding_rules_factory_mod.F90
  ${MULTIOM_ENCODING_RULES_DIR}/encoding_rules_collection_mod.F90
  ${MULTIOM_ENCODING_RULES_DIR}/encoding_info_mod.F90
  ${MULTIOM_ENCODING_RULES_DIR}/encoding_cache_mod.F90
  ${MULTIOM_ENCODING_RULES_DIR}/encoding_utils_mod.F90
  ${MULTIOM_ENCODING_RULES_DIR}/sample_mod.F90
)

# Collect source files in module2
set( MULTIOM_ENCODING_RULES_SOURCES
  ${MULTIOM_ENCODING_RULES_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in encoding_rules directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_ENCODING_RULES_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()