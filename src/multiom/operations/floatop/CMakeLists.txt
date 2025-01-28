set(MULTIOM_OPERATIONS_FLOATOP_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + operations_floatop sources: ${MULTIOM_OPERATIONS_FLOATOP_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

#
# Add Subdirectories

set( MULTIOM_OPERATIONS_FLOATOP_MAIN_SOURCES
  ${MULTIOM_OPERATIONS_FLOATOP_DIR}/floatop_base_mod.F90
  ${MULTIOM_OPERATIONS_FLOATOP_DIR}/floatop_function_call_mod.F90
  ${MULTIOM_OPERATIONS_FLOATOP_DIR}/floatop_binary_op_mod.F90
  ${MULTIOM_OPERATIONS_FLOATOP_DIR}/floatop_unary_op_mod.F90
  ${MULTIOM_OPERATIONS_FLOATOP_DIR}/floatop_constant_mod.F90
  ${MULTIOM_OPERATIONS_FLOATOP_DIR}/floatop_message_mod.F90
  ${MULTIOM_OPERATIONS_FLOATOP_DIR}/floatop_parametrization_mod.F90
  ${MULTIOM_OPERATIONS_FLOATOP_DIR}/floatop_factory_mod.F90
  ${MULTIOM_OPERATIONS_FLOATOP_DIR}/assignment_par_float_mod.F90
  ${MULTIOM_OPERATIONS_FLOATOP_DIR}/assignment_msg_float_mod.F90
)

# Collect source files in module2
set( MULTIOM_OPERATIONS_FLOATOP_SOURCES
  ${MULTIOM_OPERATIONS_FLOATOP_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in operations_floatop directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_OPERATIONS_FLOATOP_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()