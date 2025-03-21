set(MULTIOM_ENCODERS_GRIB2_SECTION4_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + grib2 section4 sources: ${MULTIOM_ENCODERS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

#
# Add Subdirectories
add_subdirectory(grib2-chem-configurator)
add_subdirectory(grib2-point-in-time-configurator)
add_subdirectory(grib2-statistics-configurator)
add_subdirectory(grib2-param-configurator)
add_subdirectory(grib2-level-configurator)
add_subdirectory(grib2-model-configurator)
add_subdirectory(grib2-ensemble-configurator)
add_subdirectory(grib2-dirfreq-configurator)
add_subdirectory(grib2-period-configurator)
add_subdirectory(grib2-satellite-configurator)
add_subdirectory(grib2-random-patterns-configurator)

# Collect source files in module2
set( MULTIOM_ENCODER_GRIB2_SECTION4_MAIN_SOURCES
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_000_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_001_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_008_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_011_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_032_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_033_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_040_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_041_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_042_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_043_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_044_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_045_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_046_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_047_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_048_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_049_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_050_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_086_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_087_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_099_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_100_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_103_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_104_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_105_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_107_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_108_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_109_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_143_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_factory_mod.F90
)

# Collect source files in module2
set( MULTIOM_ENCODER_GRIB2_SECTION4_SOURCES
  ${MULTIOM_ENCODER_GRIB2_SECTION4_MAIN_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION4_CHEM_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION4_POINT_IN_TIME_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION4_STATISTICS_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION4_PARAM_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION4_LEVEL_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION4_MODEL_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION4_ENSEMBLE_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION4_DIRFREQ_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION4_PERIOD_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION4_SATELLITE_SOURCES}
  ${MULTIOM_ENCODER_GRIB2_SECTION4_RANDOM_PATTERNS_SOURCES}
  CACHE INTERNAL "List of all sources in encoders directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_ENCODER_GRIB2_SECTION4_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()
