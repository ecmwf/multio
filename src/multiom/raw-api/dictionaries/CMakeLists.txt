set(MULTIOM_RAWAPI_DICTIONARIES_DIR ${CMAKE_CURRENT_SOURCE_DIR})

add_subdirectory( c )

# Enter in the module
message("${OFFSET} + rawapi/dictionaries sources: ${MULTIOM_RAWAPI_DICTIONARIES_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


set( MULTIOM_RAWAPI_DICTIONARIES_MAIN_SOURCES
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/c/api_dictionaries.cc
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/dictionaries_api_mod.F90
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/options_dictionary_utils_mod.F90
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/options_dictionary_wrapper_mod.F90
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/mars_dictionary_utils_mod.F90
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/mars_dictionary_wrapper_mod.F90
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/par_dictionary_utils_mod.F90
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/par_dictionary_wrapper_mod.F90
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/reduced_gg_dictionary_utils_mod.F90
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/reduced_gg_dictionary_wrapper_mod.F90
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/regular_gg_dictionary_utils_mod.F90
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/regular_gg_dictionary_wrapper_mod.F90
  # ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/regular_ll_dictionary_utils_mod.F90
  # ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/regular_ll_dictionary_wrapper_mod.F90
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/healpix_dictionary_utils_mod.F90
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/healpix_dictionary_wrapper_mod.F90
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/sh_dictionary_utils_mod.F90
  ${MULTIOM_RAWAPI_DICTIONARIES_DIR}/sh_dictionary_wrapper_mod.F90
)

# Collect source files in module2
set( MULTIOM_RAWAPI_DICTIONARIES_SOURCES
  ${MULTIOM_RAWAPI_DICTIONARIES_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in rawapi/dictionaries directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_RAWAPI_DICTIONARIES_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()