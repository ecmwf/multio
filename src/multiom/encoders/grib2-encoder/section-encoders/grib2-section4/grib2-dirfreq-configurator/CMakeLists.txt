set(MULTIOM_ENCODERS_GRIB2_SECTION4_DIRFREQ_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + grib2 section4_dirfreq sources: ${MULTIOM_ENCODERS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


# Collect source files in module2
set( MULTIOM_ENCODER_GRIB2_SECTION4_DIRFREQ_MAIN_SOURCES
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIRFREQ_DIR}/grib2_section4_dirfreq_default_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIRFREQ_DIR}/grib2_section4_dirfreq_custom_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIRFREQ_DIR}/grib2_section4_dirfreq_factory_mod.F90
)

# Collect source files in module2
set( MULTIOM_ENCODER_GRIB2_SECTION4_DIRFREQ_SOURCES
  ${MULTIOM_ENCODER_GRIB2_SECTION4_DIRFREQ_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in encoders directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_ENCODER_GRIB2_SECTION4_DIRFREQ_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()