set(MULTIOM_RAWAPI_ENCODERS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + RAWAPI_ENCODERS sources: ${MULTIOM_RAWAPI_ENCODERS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


set( MULTIOM_RAWAPI_ENCODERS_MAIN_SOURCES
  ${MULTIOM_RAWAPI_ENCODERS_DIR}/raw_encoders_api_mod.F90
  ${MULTIOM_RAWAPI_ENCODERS_DIR}/raw_encoder_wrapper_mod.F90
)

# Collect source files in module2
set( MULTIOM_RAWAPI_ENCODERS_SOURCES
  ${MULTIOM_RAWAPI_ENCODERS_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in RAWAPI_ENCODERS directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_RAWAPI_ENCODERS_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()