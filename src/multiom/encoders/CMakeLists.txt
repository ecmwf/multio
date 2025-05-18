set(MULTIOM_ENCODERS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + encoders sources: ${MULTIOM_ENCODERS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

#
# Add Subdirectories
add_subdirectory(encoder-grib2)
add_subdirectory(grib2-section0)
add_subdirectory(grib2-section1)
add_subdirectory(grib2-section2)
add_subdirectory(grib2-section3)
add_subdirectory(grib2-section4)
add_subdirectory(grib2-section5)

add_subdirectory(grib2-chem-configurator)
add_subdirectory(grib2-data-type-configurator)
add_subdirectory(grib2-dirfreq-configurator)
add_subdirectory(grib2-ensemble-configurator)
add_subdirectory(grib2-level-configurator)
add_subdirectory(grib2-model-configurator)
add_subdirectory(grib2-origin-configurator)
add_subdirectory(grib2-param-configurator)
add_subdirectory(grib2-period-configurator)
add_subdirectory(grib2-point-in-time-configurator)
add_subdirectory(grib2-random-patterns-configurator)
add_subdirectory(grib2-reference-time-configurator)
add_subdirectory(grib2-satellite-configurator)
add_subdirectory(grib2-statistics-configurator)
add_subdirectory(grib2-tables-configurator)







# # Collect source files in module2
# set( MULTIOM_ENCODERS_MAIN_SOURCES
#   #${MULTIOM_ENCODERS_DIR}/grib_section_base_mod.F90
#   #${MULTIOM_ENCODERS_DIR}/grib_encoder_options_mod.F90
#   ${MULTIOM_ENCODERS_DIR}/grib_encoder_factory_mod.F90
#   #${MULTIOM_ENCODERS_DIR}/grib_encoder_register_mod.F90
#   #${MULTIOM_ENCODERS_DIR}/grib_encoder_tunneling_mod.F90
#   #${MULTIOM_ENCODERS_DIR}/grib_encoder_lazy_rules_mod.F90
# )
#
# # Collect source files in module2
# set( MULTIOM_ENCODERS_SOURCES
#   ${MULTIOM_ENCODERS_MAIN_SOURCES}
#   ${MULTIOM_ENCODER_GRIB2_SOURCES}
#   CACHE INTERNAL "List of all sources in operations directory"
# )
#
# # Optionally, message what has been appended for debugging
# foreach(source_file IN LISTS MULTIOM_ENCODERS_MAIN_SOURCES)
#
#     # Extract the filename (without the directory)
#     get_filename_component(filename ${source_file} NAME)
#
#     # Print the filename
#     message("${OFFSET}    - ${filename}")
# endforeach()