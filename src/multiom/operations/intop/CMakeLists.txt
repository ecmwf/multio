set(MULTIOM_OPERATIONS_INTOP_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + operations_intop sources: ${MULTIOM_OPERATIONS_INTOP_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

#
# Add Subdirectories

set( MULTIOM_OPERATIONS_INTOP_MAIN_SOURCES
  ${MULTIOM_OPERATIONS_INTOP_DIR}/intop_base_mod.F90
  ${MULTIOM_OPERATIONS_INTOP_DIR}/intop_function_call_mod.F90
  ${MULTIOM_OPERATIONS_INTOP_DIR}/intop_binary_op_mod.F90
  ${MULTIOM_OPERATIONS_INTOP_DIR}/intop_unary_op_mod.F90
  ${MULTIOM_OPERATIONS_INTOP_DIR}/intop_constant_mod.F90
  ${MULTIOM_OPERATIONS_INTOP_DIR}/intop_enum_mod.F90
  ${MULTIOM_OPERATIONS_INTOP_DIR}/intop_message_mod.F90
  ${MULTIOM_OPERATIONS_INTOP_DIR}/intop_parametrization_mod.F90
  ${MULTIOM_OPERATIONS_INTOP_DIR}/intop_factory_mod.F90
  ${MULTIOM_OPERATIONS_INTOP_DIR}/assignment_msg_int_mod.F90
  ${MULTIOM_OPERATIONS_INTOP_DIR}/assignment_par_int_mod.F90
)

# Collect source files in module2
set( MULTIOM_OPERATIONS_INTOP_SOURCES
  ${MULTIOM_OPERATIONS_INTOP_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in operations_intop directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_OPERATIONS_INTOP_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()