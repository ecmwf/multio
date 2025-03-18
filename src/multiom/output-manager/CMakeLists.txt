set(MULTIOM_OUTPUT_MANAGER_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + output-manager sources: ${MULTIOM_MAPPING_RULES_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


set( MULTIOM_OUTPUT_MANAGER_MAIN_SOURCES
  ${MULTIOM_OUTPUT_MANAGER_DIR}/output_manager_base_mod.F90
  ${MULTIOM_OUTPUT_MANAGER_DIR}/output_manager_factory_mod.F90
  ${MULTIOM_OUTPUT_MANAGER_DIR}/noop_mod.F90
  ${MULTIOM_OUTPUT_MANAGER_DIR}/dump_mod.F90
  ${MULTIOM_OUTPUT_MANAGER_DIR}/model_data2multio_mod.F90
  ${MULTIOM_OUTPUT_MANAGER_DIR}/grib_header2multio_mod.F90
)

# Collect source files in module2
set( MULTIOM_OUTPUT_MANAGER_SOURCES
  ${MULTIOM_OUTPUT_MANAGER_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in output-manager directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_OUTPUT_MANAGER_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()