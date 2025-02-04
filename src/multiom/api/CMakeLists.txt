set(MULTIOM_API_DIR ${CMAKE_CURRENT_SOURCE_DIR})

add_subdirectory( c )

# Enter in the module
message("${OFFSET} + API sources: ${MULTIOM_API_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


# Create the templated map for containing the mars dictionary
set(TEMPLATE_USE_KEY  "USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG")
set(TEMPLATE_KEY_DUMMY_TYPE "INTEGER(KIND=C_LONG_LONG)")
set(TEMPLATE_KEY_TYPE "INTEGER(KIND=C_LONG_LONG)")
set(TEMPLATE_KEY_INITIAL_VALUE "0_INT64" )
set(TEMPLATE_KEY_FNAME "int64")
set(TEMPLATE_KEY_NAME "INT64")
set(TEMPLATE_USE_VALUE  "USE :: API_OPTIONS_DICTIONARY_UTILS_MOD, ONLY: API_OPTIONS_T")
set(TEMPLATE_VALUE_DUMMY_TYPE "TYPE(API_OPTIONS_T)")
set(TEMPLATE_VALUE_TYPE "TYPE(API_OPTIONS_T)")
set(TEMPLATE_VALUE_FNAME "opt_dict" )
set(TEMPLATE_VALUE_NAME "OPT_DICT" )
configure_file(  ${MULTIOM_TEMPLATES_DIR}/map_pkey_mod.F90 ${CMAKE_CURRENT_BINARY_DIR}/map_${TEMPLATE_KEY_FNAME}_${TEMPLATE_VALUE_FNAME}_mod.F90  )


# Collect source files in module2
set( MULTIOM_API_MAIN_SOURCES
  ${MULTIOM_API_DIR}/c/api.c
  ${MULTIOM_API_DIR}/api_options_dictionary_utils_mod.F90
  ${MULTIOM_API_DIR}/api_mars_dictionary_utils_mod.F90
  ${MULTIOM_API_DIR}/api_par_dictionary_utils_mod.F90
  ${MULTIOM_API_DIR}/api_reduced_gg_dictionary_utils_mod.F90
  ${MULTIOM_API_DIR}/api_sh_dictionary_utils_mod.F90
  ${MULTIOM_API_DIR}/api_general_utils_mod.F90
  ${MULTIOM_API_DIR}/api_general_utils_c.cc
  ${MULTIOM_API_DIR}/api_shared_data_mod.F90
  ${MULTIOM_API_DIR}/api_encoder_wrapper_mod.F90
  ${MULTIOM_API_DIR}/api_dictionary_wrapper_mod.F90
  ${MULTIOM_API_DIR}/api_extract_metadata_c.cc
  ${MULTIOM_API_DIR}/api_encode_c.cc
  ${CMAKE_CURRENT_BINARY_DIR}/map_int64_opt_dict_mod.F90
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
