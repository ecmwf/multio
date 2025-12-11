set(MULTIOM_ENCODERS_GRIB2_SECTION1_REFERENCE_TIME_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + grib2 section1_REFERENCE_TIME sources: ${MULTIOM_ENCODERS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


# Collect source files in module2
set( MULTIOM_ENCODER_GRIB2_SECTION1_REFERENCE_TIME_MAIN_SOURCES
  ${MULTIOM_ENCODERS_GRIB2_SECTION1_REFERENCE_TIME_DIR}/grib2_section1_reference_time_default_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION1_REFERENCE_TIME_DIR}/grib2_section1_reference_time_custom_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION1_REFERENCE_TIME_DIR}/grib2_section1_reference_time_factory_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION1_REFERENCE_TIME_DIR}/grib2_significance_of_reference_time_map_mod.F90
)

# Collect source files in module2
set( MULTIOM_ENCODER_GRIB2_SECTION1_REFERENCE_TIME_SOURCES
  ${MULTIOM_ENCODER_GRIB2_SECTION1_REFERENCE_TIME_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in encoders directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_ENCODER_GRIB2_SECTION1_REFERENCE_TIME_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()
