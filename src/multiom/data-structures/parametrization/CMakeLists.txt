set(MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + parametrization sources: ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

# Collect source files in data-structures
set( MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_MAIN_SOURCES
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}/parametrization_enumerators_mod.F90
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}/parametrization_mod.F90
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}/reduced_gg_map_mod.F90
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}/regular_ll_map_mod.F90
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}/spherical_harmonics_map_mod.F90
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}/data_representation_par_mod.F90
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}/geometry_par_mod.F90
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}/time_par_mod.F90
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}/bitmap_par_mod.F90
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}/ensemble_par_mod.F90
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}/analysis_par_mod.F90
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}/satellite_par_mod.F90
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}/level_par_mod.F90
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_DIR}/wave_par_mod.F90
)

# Collect source files in data-structures
set( MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_SOURCES
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in data-structures/mars directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()