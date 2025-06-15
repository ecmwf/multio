set(MULTIOM_RAWAPI_COMMON_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + rawapi/common sources: ${MULTIOM_RAWAPI_COMMON_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


set( MULTIOM_RAWAPI_COMMON_MAIN_SOURCES
  ${MULTIOM_RAWAPI_COMMON_DIR}/api_f_c_wrapper_mod.F90
  ${MULTIOM_RAWAPI_COMMON_DIR}/api_f_c_wrapper_c.cc
)

# Collect source files in module2
set( MULTIOM_RAWAPI_COMMON_SOURCES
  ${MULTIOM_RAWAPI_COMMON_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in rawapi/common directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_RAWAPI_COMMON_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()