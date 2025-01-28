set(MULTIOM_DATA_STRUCTURES_MARS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + mars sources: ${MULTIOM_DATA_STRUCTURES_MARS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

# Create the templated map for containing the mars dictionary
set(TEMPLATE_USE_KEY  "USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG")
set(TEMPLATE_KEY_DUMMY_TYPE "INTEGER(KIND=C_LONG_LONG)")
set(TEMPLATE_KEY_TYPE "INTEGER(KIND=C_LONG_LONG)")
set(TEMPLATE_KEY_INITIAL_VALUE "0_INT64" )
set(TEMPLATE_KEY_FNAME "int64")
set(TEMPLATE_KEY_NAME "INT64")
set(TEMPLATE_USE_VALUE  "USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T")
set(TEMPLATE_VALUE_DUMMY_TYPE "TYPE(FORTRAN_MESSAGE_T)")
set(TEMPLATE_VALUE_TYPE "TYPE(FORTRAN_MESSAGE_T)")
set(TEMPLATE_VALUE_FNAME "mars_dict" )
set(TEMPLATE_VALUE_NAME "MARS_DICT" )
configure_file(  ${MULTIOM_TEMPLATES_DIR}/map_pkey_mod.F90 ${CMAKE_CURRENT_BINARY_DIR}/map_${TEMPLATE_KEY_FNAME}_${TEMPLATE_VALUE_FNAME}_mod.F90  )


# Collect source files in data-structures
set( MULTIOM_DATA_STRUCTURES_MARS_MAIN_SOURCES
  ${MULTIOM_DATA_STRUCTURES_MARS_DIR}/fortran_message_enumerators_mod.F90
  ${MULTIOM_DATA_STRUCTURES_MARS_DIR}/fortran_message_mod.F90
  ${CMAKE_CURRENT_BINARY_DIR}/map_int64_mars_dict_mod.F90
)

# Collect source files in data-structures
set( MULTIOM_DATA_STRUCTURES_MARS_SOURCES
  ${MULTIOM_DATA_STRUCTURES_MARS_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in data-structures/mars directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_DATA_STRUCTURES_MARS_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()
