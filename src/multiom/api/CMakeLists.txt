set(MULTIOM_API_DIR ${CMAKE_CURRENT_SOURCE_DIR})

add_subdirectory( c )

# Enter in the module
message("${OFFSET} + API sources: ${MULTIOM_API_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

# Collect source files in module2
set( MULTIOM_API_MAIN_SOURCES
  ${MULTIOM_API_DIR}/c/api.c
  ${MULTIOM_API_DIR}/api_mars_dictionary_utils_mod.F90
  ${MULTIOM_API_DIR}/api_par_dictionary_utils_mod.F90
  ${MULTIOM_API_DIR}/api_reduced_gg_dictionary_utils_mod.F90
  ${MULTIOM_API_DIR}/api_sh_dictionary_utils_mod.F90
  ${MULTIOM_API_DIR}/api_general_utils_mod.F90
  ${MULTIOM_API_DIR}/api_general_utils_c.c
  ${MULTIOM_API_DIR}/api_shared_data_mod.F90
  ${MULTIOM_API_DIR}/api_encoder_wrapper_mod.F90
  ${MULTIOM_API_DIR}/api_dictionary_wrapper_mod.F90
)

# Collect source files in module2
set( MULTIOM_API_SOURCES
  ${MULTIOM_API_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in API directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_API_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()