set(MULTIOM_IFS_ENCODERS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} +ifs_encoders sources: ${MULTIOM_IFS_ENCODERS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


set(TEMPLATE_USE_KEY  "USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG")
set(TEMPLATE_KEY_DUMMY_TYPE "INTEGER(KIND=C_LONG_LONG)")
set(TEMPLATE_KEY_TYPE "INTEGER(KIND=C_LONG_LONG)")
set(TEMPLATE_KEY_FNAME "int64")
set(TEMPLATE_KEY_NAME "INT64")
set(TEMPLATE_USE_VALUE  "USE :: MULTIOM_CACHED_ENCODER_MOD, ONLY: MULTIOM_CACHED_ENCODERS_T")
set(TEMPLATE_VALUE_DUMMY_TYPE "TYPE(MULTIOM_CACHED_ENCODERS_T)")
set(TEMPLATE_VALUE_TYPE "TYPE(MULTIOM_CACHED_ENCODERS_T)")
set(TEMPLATE_VALUE_FNAME "encoder" )
set(TEMPLATE_VALUE_NAME "ENCODER" )
configure_file(  ${MULTIOM_TEMPLATES_DIR}/map_pkey_mod.F90 ${CMAKE_CURRENT_BINARY_DIR}/map_${TEMPLATE_KEY_FNAME}_${TEMPLATE_VALUE_FNAME}_mod.F90  )


set( MULTIOM_IFS_ENCODERS_MAIN_SOURCES
  ${MULTIOM_IFS_ENCODERS_DIR}/multiom_cached_encoder_mod.F90
  ${MULTIOM_IFS_ENCODERS_DIR}/multiom_encoder_mod.F90
  ${CMAKE_CURRENT_BINARY_DIR}/map_int64_encoder_mod.F90
)

# Collect source files in module2
set( MULTIOM_IFS_ENCODERS_SOURCES
  ${MULTIOM_IFS_ENCODERS_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in ifs_encoders directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_IFS_ENCODERS_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()