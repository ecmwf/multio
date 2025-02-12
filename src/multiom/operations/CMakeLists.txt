set(MULTIOM_OPERATIONS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + operations sources: ${MULTIOM_OPERATIONS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

#
# Add Subdirectories
add_subdirectory(floatop)
add_subdirectory(intop)
add_subdirectory(stringop)

# Main operations
set( MULTIOM_OPERATIONS_MAIN_SOURCES
  ${MULTIOM_OPERATIONS_DIR}/assignment_base_mod.F90
  ${MULTIOM_OPERATIONS_DIR}/assignment_factory_mod.F90
  ${MULTIOM_OPERATIONS_DIR}/assignment_copy_mod.F90
  ${MULTIOM_OPERATIONS_DIR}/assignment_composed_mod.F90
)

# Collect source files in module2
set( MULTIOM_OPERATIONS_SOURCES
  ${MULTIOM_OPERATIONS_MAIN_SOURCES}
  ${MULTIOM_OPERATIONS_FLOATOP_SOURCES}
  ${MULTIOM_OPERATIONS_INTOP_SOURCES}
  ${MULTIOM_OPERATIONS_STRINGOP_SOURCES}
  CACHE INTERNAL "List of all sources in operations directory"
)


#
# + BUILD OUTPUT MANAGER LIBRARY
# ======================================================================
# ecbuild_add_library(
#   TARGET  multiom-operations
#   SOURCES ${MULTIOM_OPERATIONS_SOURCES}
#
#   DEFINITIONS ${OUTPUT_MANAGER_BUILD_FLAVOUR}
#
#   PRIVATE_INCLUDES
#     ${CMAKE_BINARY_DIR}/include
#     ${OUTPUT_MANAGER_INCLUDE_PATH}
#
#   PUBLIC_LIBS
#     fckit
#     eccodes
#     eccodes_f90
#     multio-fapi
# )


# Module directory
# set_target_properties( multiom-operations PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module/multiom )
# target_include_directories( multiom-operations PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/module/multiom> )

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_OPERATIONS_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()