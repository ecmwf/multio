set(MULTIOM_FILTERS_MARS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + filters_mars sources: ${MULTIOM_FILTERS_MARS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


# Collect source files in module2
set( MULTIOM_FILTERS_MARS_SOURCES
  ${MULTIOM_FILTERS_MARS_DIR}/filter_class_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_direction_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_expver_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_frequency_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_wavelength_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_levelist_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_anoffset_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_chem_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_step_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_number_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_levtype_mod.F90
  # ${MULTIOM_FILTERS_MARS_DIR}/filter_origin_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_param_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_repres_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_packing_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_stream_mod.F90
  ${MULTIOM_FILTERS_MARS_DIR}/filter_type_mod.F90
  CACHE INTERNAL "List of all sources in filters/mars directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_FILTERS_MARS_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()