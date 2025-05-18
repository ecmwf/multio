set(MULTIOM_ENCODERS_GRIB2_SECTION4_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + grib2 section4 sources: ${MULTIOM_ENCODERS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

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
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_045_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_046_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_048_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_049_mod.F90
  # ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_050_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_060_mod.F90
  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_061_mod.F90
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
#  ${MULTIOM_ENCODERS_GRIB2_SECTION4_DIR}/grib2_section4_factory_mod.F90
)

# Collect source files in module2
set( MULTIOM_ENCODER_GRIB2_SECTION4_SOURCES
  ${MULTIOM_ENCODER_GRIB2_SECTION4_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in encoders directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_ENCODER_GRIB2_SECTION4_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()


#
# Add library
ecbuild_add_library(
  TARGET
    multiom-encoders-section4-configurator

  SOURCES
    ${MULTIOM_ENCODER_GRIB2_SECTION4_MAIN_SOURCES}

  DEFINITIONS
    ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  PRIVATE_INCLUDES
    $<BUILD_INTERFACE:${ECOM_INCLUDE_DIR}>

  PUBLIC_LIBS
    fckit
    eccodes
    eccodes_f90
    multiom-core
    multiom-dictionaries
    multiom-encoders-base
)
