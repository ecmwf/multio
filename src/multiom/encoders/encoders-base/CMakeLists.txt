set(MULTIOM_ENCODERS_BASE_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + encoders_base sources: ${MULTIOM_ENCODERS_BASE_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

# Collect source files in module2
set( MULTIOM_ENCODERS_BASE_MAIN_SOURCES
  ${MULTIOM_ENCODERS_BASE_DIR}/grib_section_base_mod.F90
  ${MULTIOM_ENCODERS_BASE_DIR}/grib_encoder_options_mod.F90
  ${MULTIOM_ENCODERS_BASE_DIR}/grib_encoder_register_mod.F90
  ${MULTIOM_ENCODERS_BASE_DIR}/grib_encoder_tunneling_mod.F90
  ${MULTIOM_ENCODERS_BASE_DIR}/grib_encoder_lazy_rules_mod.F90
  ${MULTIOM_ENCODERS_BASE_DIR}/mapping_options_mod.F90
  ${MULTIOM_ENCODERS_BASE_DIR}/filter_options_mod.F90
)

# Collect source files in module2
set( MULTIOM_ENCODERS_BASE_SOURCES
  ${MULTIOM_ENCODERS_BASE_MAIN_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SOURCES}
  CACHE INTERNAL "List of all sources in operations directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_ENCODERS_BASE_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()