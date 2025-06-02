set(MULTIOM_API_WRAPPER_DICTIONARIES_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + dictionaries wrapper sources: ${MULTIOM_COMMON_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

# Collect source files in module2
set( MULTIOM_API_WRAPPER_DICTIONARIES_MAIN_SOURCES
  ${MULTIOM_API_WRAPPER_DICTIONARIES_DIR}/mars_dictionary_wrapper_mod.F90
  ${MULTIOM_API_WRAPPER_DICTIONARIES_DIR}/parametrization_dictionary_wrapper_mod.F90
  ${MULTIOM_API_WRAPPER_DICTIONARIES_DIR}/reduced_gg_dictionary_wrapper_mod.F90
  ${MULTIOM_API_WRAPPER_DICTIONARIES_DIR}/regular_gg_dictionary_wrapper_mod.F90
  ${MULTIOM_API_WRAPPER_DICTIONARIES_DIR}/regular_ll_dictionary_wrapper_mod.F90
  ${MULTIOM_API_WRAPPER_DICTIONARIES_DIR}/healpix_dictionary_wrapper_mod.F90
  ${MULTIOM_API_WRAPPER_DICTIONARIES_DIR}/orca_dictionary_wrapper_mod.F90
  ${MULTIOM_API_WRAPPER_DICTIONARIES_DIR}/fesom_dictionary_wrapper_mod.F90
  ${MULTIOM_API_WRAPPER_DICTIONARIES_DIR}/spherical_harmonics_dictionary_wrapper_mod.F90
  ${MULTIOM_API_WRAPPER_DICTIONARIES_DIR}/api_options_dictionary_wrapper_mod.F90
)

# Collect source files in module2
set( MULTIOM_API_WRAPPER_DICTIONARIES_SOURCES
  ${MULTIOM_API_WRAPPER_DICTIONARIES_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in API directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_API_WRAPPER_DICTIONARIES_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()