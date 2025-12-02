set(MULTIOM_ENCODERS_GRIB2_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + grib2 encoder sources: ${MULTIOM_ENCODERS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

#
# Add Subdirectories
add_subdirectory(grib2-section0)
add_subdirectory(grib2-section1)
add_subdirectory(grib2-section2)
add_subdirectory(grib2-section3)
add_subdirectory(grib2-section4)
add_subdirectory(grib2-section5)

# Collect source files in module2
set( MULTIOM_ENCODER_GRIB2_MAIN_SOURCES
  ${MULTIOM_ENCODERS_GRIB2_DIR}/grib2_encoder_mod.F90
)

# Collect source files in module2
set( MULTIOM_ENCODER_GRIB2_SOURCES
  ${MULTIOM_ENCODER_GRIB2_MAIN_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION0_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION1_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION2_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION3_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION4_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION5_SOURCES}
  CACHE INTERNAL "List of all sources in encoders directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_ENCODER_GRIB2_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()