set(MULTIOM_FILTERS_CUSTOM_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + filters_custom sources: ${MULTIOM_FILTERS_CUSTOM_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


# Collect source files in module2
set( MULTIOM_FILTERS_CUSTOM_SOURCES
  ${MULTIOM_FILTERS_CUSTOM_DIR}/filter_model_mod.F90
  ${MULTIOM_FILTERS_CUSTOM_DIR}/filter_is_ensemble_mod.F90
  ${MULTIOM_FILTERS_CUSTOM_DIR}/filter_is_chemical_mod.F90
  ${MULTIOM_FILTERS_CUSTOM_DIR}/filter_is_aerosol_mod.F90
  ${MULTIOM_FILTERS_CUSTOM_DIR}/filter_is_chemical_optical_mod.F90
  CACHE INTERNAL "List of all sources in filters/custom directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_FILTERS_CUSTOM_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()