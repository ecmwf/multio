set(MULTIOM_IFS_INTERFACE_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + ifs-iface sources: ${MULTIOM_MAPPING_RULES_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


set( MULTIOM_IFS_INTERFACE_MAIN_SOURCES
  ${MULTIOM_IFS_INTERFACE_DIR}/ifs_par_serializer_mod.F90
  ${MULTIOM_IFS_INTERFACE_DIR}/ifs_msg_mod.F90
  ${MULTIOM_IFS_INTERFACE_DIR}/ifs_par_mod.F90
  ${MULTIOM_IFS_INTERFACE_DIR}/ifs_toc_mod.F90
  ${MULTIOM_IFS_INTERFACE_DIR}/ifs_val_mod.F90
)

# Collect source files in module2
set( MULTIOM_IFS_INTERFACE_SOURCES
  ${MULTIOM_IFS_INTERFACE_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in ifs-interface directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_IFS_INTERFACE_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()