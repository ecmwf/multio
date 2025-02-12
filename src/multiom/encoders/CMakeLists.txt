set(MULTIOM_ENCODERS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + encoders sources: ${MULTIOM_ENCODERS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

#
# Add Subdirectories
add_subdirectory(encoder-grib2)



# Collect source files in module2
set( MULTIOM_ENCODERS_MAIN_SOURCES
  #${MULTIOM_ENCODERS_DIR}/grib_section_base_mod.F90
  #${MULTIOM_ENCODERS_DIR}/grib_encoder_options_mod.F90
  ${MULTIOM_ENCODERS_DIR}/grib_encoder_factory_mod.F90
  #${MULTIOM_ENCODERS_DIR}/grib_encoder_register_mod.F90
  #${MULTIOM_ENCODERS_DIR}/grib_encoder_tunneling_mod.F90
  #${MULTIOM_ENCODERS_DIR}/grib_encoder_lazy_rules_mod.F90
)

# Collect source files in module2
set( MULTIOM_ENCODERS_SOURCES
  ${MULTIOM_ENCODERS_MAIN_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SOURCES}
  CACHE INTERNAL "List of all sources in operations directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_ENCODERS_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()