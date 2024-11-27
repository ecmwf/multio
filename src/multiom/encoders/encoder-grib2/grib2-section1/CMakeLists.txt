set(MULTIOM_ENCODERS_GRIB2_SECTION1_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + grib2 section1 sources: ${MULTIOM_ENCODERS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

#
# Add Subdirectories
add_subdirectory(grib2-origin-configurator)
add_subdirectory(grib2-tables-configurator)
add_subdirectory(grib2-data-type-configurator)
add_subdirectory(grib2-reference-time-configurator)

# Collect source files in module2
set( MULTIOM_ENCODER_GRIB2_SECTION1_MAIN_SOURCES
  ${MULTIOM_ENCODERS_GRIB2_SECTION1_DIR}/grib2_section1_000_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION1_DIR}/grib2_section1_factory_mod.F90
)

# Collect source files in module2
set( MULTIOM_ENCODER_GRIB2_SECTION1_SOURCES
  ${MULTIOM_ENCODER_GRIB2_SECTION1_MAIN_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION1_ORIGIN_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION1_TABLES_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION1_DATA_TYPE_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION1_REFERENCE_TIME_SOURCES}
  CACHE INTERNAL "List of all sources in encoders directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_ENCODER_GRIB2_SECTION1_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()