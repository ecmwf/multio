set(MULTIOM_COMMON_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + common sources: ${MULTIOM_COMMON_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

# Collect source files in module2
set( MULTIOM_COMMON_SOURCES
  ${MULTIOM_COMMON_DIR}/datakinds_def_mod.F90
  ${MULTIOM_COMMON_DIR}/enumerators_mod.F90
  ${MULTIOM_COMMON_DIR}/general_utils_mod.F90
  ${MULTIOM_COMMON_DIR}/datetime_utils_mod.F90
  ${MULTIOM_COMMON_DIR}/constants_mod.F90
  ${MULTIOM_COMMON_DIR}/grib_codes_mod.F90
  ${MULTIOM_COMMON_DIR}/sysinfo_c.c
  ${MULTIOM_COMMON_DIR}/sysinfo_mod.F90
  ${MULTIOM_COMMON_DIR}/profile_mod.F90
  ${MULTIOM_COMMON_DIR}/multio_utils_mod.F90
  ${MULTIOM_COMMON_DIR}/log_info_mod.F90
  CACHE INTERNAL "List of all sources in common directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_COMMON_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()
