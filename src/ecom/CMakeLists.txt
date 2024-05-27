if ( HAVE_OUTPUT_MANAGER )

# Update on new big features
set(OUTPUT_MANAGER_MAJOR 0)

# Interface changes
set(OUTPUT_MANAGER_MINOR 0)

# Implementation updates with constant interfaces
set(OUTPUT_MANAGER_PATCH 1)

# Use Fortran to get the compiler and compiler version
execute_process(
    COMMAND ${CMAKE_Fortran_COMPILER} --version
    OUTPUT_VARIABLE FORTRAN_COMPILER_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(REGEX REPLACE "\n" ";" lines "${FORTRAN_COMPILER_VERSION}")
list(GET lines 0 FORTRAN_COMPILER_INFO)

# Linker flags (remove trailing spaces)
string(REGEX REPLACE "^[ \t]+" "" FORTRAN_BUILD_FLAGS         "${IFS_Fortran_FLAGS}")
string(REGEX REPLACE "^[ \t]+" "" FORTRAN_EXE_LINKER_FLAGS    "${ECBUILD_EXE_LINKER_FLAGS}")
string(REGEX REPLACE "^[ \t]+" "" FORTRAN_SHARED_LINKER_FLAGS "${ECBUILD_SHARED_LINKER_FLAGS}")

# Get the git sha of the current commit
execute_process(
    COMMAND git rev-parse HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/ifs-source
    OUTPUT_VARIABLE GIT_SHA
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Get the current date and time
string(TIMESTAMP BUILD_DATE "%Y-%m-%d %H:%M:%S")

# Get the build type (e.g., Debug, Release)
if(CMAKE_BUILD_TYPE)
    set(BUILD_TYPE ${CMAKE_BUILD_TYPE})
else()
    set(BUILD_TYPE "Unknown")
endif()

# Get the hostname
execute_process(
    COMMAND hostname
    OUTPUT_VARIABLE HOSTNAME
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

#
# + CONFIGURE HEADERS
# ======================================================================
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/output_manager_version_mod.F90.in                               ${CMAKE_CURRENT_BINARY_DIR}/output_manager_version_mod.F90  )
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_utils.h.in         ${CMAKE_CURRENT_BINARY_DIR}/include/output_manager_preprocessor_utils.h  )
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_trace_utils.h.in   ${CMAKE_CURRENT_BINARY_DIR}/include/output_manager_preprocessor_trace_utils.h  )
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_logging_utils.h.in ${CMAKE_CURRENT_BINARY_DIR}/include/output_manager_preprocessor_logging_utils.h  )
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_errhdl_utils.h.in  ${CMAKE_CURRENT_BINARY_DIR}/include/output_manager_preprocessor_errhdl_utils.h  )


#
# Configure the include path (the variable is defiend and used in the parent scope)
list( APPEND OUTPUT_MANAGER_INCLUDE_PATH
    ${CMAKE_CURRENT_BINARY_DIR}/include/
)

set( OUTPUT_MANAGER_LIBRARIES
    ifs-output-manager
)

#
# + BUILD OUTPUT MANAGER CORE (this organisation comes from dep. problems in CMake)
# ======================================================================
list( APPEND output_manager_src
  ${CMAKE_CURRENT_BINARY_DIR}/output_manager_version_mod.F90
  core/om_data_kind_mod.F90
  core/om_data_types_mod.F90
  core/om_constants_mod.F90
  core/om_enumerators_mod.F90
  core/om_debug_mod.F90
  core/om_trace_mod.F90
  core/om_core_mod.F90
  core/grib_info_data_mod.F90
)


#
# + COMMON/UTILS
# ======================================================================

#
# Append source files for the raw message management
# --------------------------------------------------
list( APPEND output_manager_src
    common/om_general_utils_c.c
    common/om_general_utils_mod.F90
    common/om_multio_utils_v2_mod.F90
    common/om_profile_mod.F90
    common/datetime_utils_mod.F90
)

#
# + CONTAINERS
# ======================================================================

#
# Append source files for the raw message management
# --------------------------------------------------
list( APPEND output_manager_src
    containers/map_mod.F90
    containers/stack_mod.F90
    containers/circular_buffer_mod.F90
)

#
# + GRIB INFO
# ======================================================================

#
# Append source files for the raw message management
# --------------------------------------------------
list( APPEND output_manager_src
    grib_info/encoding_info_mod.F90
    grib_info/yaml_rules_mod.F90
    grib_info/grib_codes_mod.F90
)


#
# + ENCODERS
# ======================================================================
#
# Append source files for management of the encoders
# --------------------------------------------------
list( APPEND output_manager_src

    encoders/grib_encoder_factory_mod.F90
    encoders/grib_encoder_indexer_mod.F90
    encoders/grib_encoder_base_mod.F90
    encoders/grib_encoder_manager_mod.F90
)


#
# + MESSAGE
# ======================================================================
#
# Append source files for the mesage definition and management
# ------------------------------------------------------------
list( APPEND output_manager_src
    data/toc_utils_mod.F90
    data/msg_utils_mod.F90
    data/par_utils_mod.F90
    data/val_utils_mod.F90
)





#
# + METADATA
# ======================================================================

#
# Append metadata source files to the list
# ----------------------------------------
list( APPEND output_manager_src
    metadata/metadata_base_mod.F90
    metadata/multIO_metadata_v2_mod.F90
    metadata/grib_metadata_mod.F90
    metadata/metadata_factory_mod.F90
    metadata/metadata_tracer_mod.F90
)


#
# + ENCODERS
# ======================================================================

#
# Append output-manager source files to the list
# ----------------------------------------------
list( APPEND output_manager_src
    encoders/utils/runtime/time_encoders_mod.F90
    encoders/utils/assumptions/general_assumptions_mod.F90
    encoders/utils/assumptions/level_assumptions_mod.F90
    encoders/utils/assumptions/packaging_assumptions_mod.F90
    encoders/utils/assumptions/time_assumptions_mod.F90
    encoders/utils/preset/general_preset_utils_mod.F90
    encoders/utils/preset/geometry_preset_utils_mod.F90
    encoders/utils/preset/mars_preset_utils_mod.F90
    encoders/utils/preset/time_preset_utils_mod.F90
    encoders/utils/runtime/general_runtime_utils_mod.F90
    encoders/utils/runtime/compression_runtime_utils_mod.F90
    encoders/utils/runtime/parameters_runtime_utils_mod.F90
    encoders/utils/runtime/levels_runtime_utils_mod.F90
    encoders/utils/runtime/packaging_runtime_utils_mod.F90
    encoders/utils/runtime/time_runtime_utils_mod.F90
    encoders/grib1/grib_encoder_grib1_gg_p_mod.F90
    encoders/grib2/grib_encoder_grib2_gg_p_mod.F90
    encoders/grib1/grib_encoder_grib1_gg_t_mod.F90
    encoders/grib2/grib_encoder_grib2_gg_t_mod.F90
    encoders/grib1/grib_encoder_grib1_gg_v_mod.F90
    encoders/grib2/grib_encoder_grib2_gg_v_mod.F90
    encoders/grib1/grib_encoder_grib1_gg_s_mod.F90
    encoders/grib2/grib_encoder_grib2_gg_s_mod.F90
    encoders/grib1/grib_encoder_grib1_gg_m_mod.F90
    encoders/grib2/grib_encoder_grib2_gg_m_mod.F90
    encoders/grib1/grib_encoder_grib1_gg_wi_mod.F90
    encoders/grib2/grib_encoder_grib2_gg_wi_mod.F90
    encoders/grib1/grib_encoder_grib1_gg_ws_mod.F90
    encoders/grib2/grib_encoder_grib2_gg_ws_mod.F90
    encoders/grib1/grib_encoder_grib1_sh_p_mod.F90
    encoders/grib2/grib_encoder_grib2_sh_p_mod.F90
    encoders/grib1/grib_encoder_grib1_sh_t_mod.F90
    encoders/grib2/grib_encoder_grib2_sh_t_mod.F90
    encoders/grib1/grib_encoder_grib1_sh_v_mod.F90
    encoders/grib2/grib_encoder_grib2_sh_v_mod.F90
    encoders/grib1/grib_encoder_grib1_sh_s_mod.F90
    encoders/grib2/grib_encoder_grib2_sh_s_mod.F90
    encoders/grib1/grib_encoder_grib1_sh_m_mod.F90
    encoders/grib2/grib_encoder_grib2_sh_m_mod.F90
    encoders/grib1/grib_encoder_grib1_sh_wi_mod.F90
    encoders/grib2/grib_encoder_grib2_sh_wi_mod.F90
    encoders/grib1/grib_encoder_grib1_sh_ws_mod.F90
    encoders/grib2/grib_encoder_grib2_sh_ws_mod.F90
)



#
# + OUTPUT MANAGERS
# ======================================================================

#
# Append output-manager source files to the list
# ----------------------------------------------
list( APPEND output_manager_src
    flavours/output_manager_base_mod.F90
    flavours/noop_output_manager_mod.F90
    flavours/dump_output_manager_mod.F90
    flavours/gribx_binary_output_manager_mod.F90
    flavours/gribx2multio_bin_output_manager_mod.F90
    flavours/gribx2multio_raw_output_manager_mod.F90
    flavours/multio_raw_output_manager_mod.F90
    flavours/multio_no_enc_output_manager_v2_mod.F90
    flavours/output_manager_factory_mod.F90
)

if ( HAVE_OUTPUT_MANAGER_ENCODER_REPORT )
list( APPEND OUTPUT_MANAGER_BUILD_FLAVOUR "WITH_OUTPUT_MANAGER_ENCODING_REPORT" )
endif()


#
# + API INTERFACE
# ======================================================================
list( APPEND output_manager_src
    om_api_mod.F90
)


#
# + BUILD OUTPUT MANAGER LIBRARY
# ======================================================================
ecbuild_add_library(
  TARGET  ecom
  SOURCES ${output_manager_src}

  DEFINITIONS ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  PRIVATE_INCLUDES
    ${CMAKE_BINARY_DIR}/include
    ${OUTPUT_MANAGER_INCLUDE_PATH}

  PUBLIC_LIBS
    fckit
    eccodes
    eccodes_f90
    multio-fapi
)

# Module directory
set_target_properties( ecom PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module/ecom )
target_include_directories( ecom PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/module/ecom> )


if ( HAVE_OUTPUT_MANAGER_TOOL )
ecbuild_add_executable(

  TARGET ecom-feed

  SOURCES
    tool/om_tool_cfg_mod.F90
    tool/om_tool_utils_mod.F90
    tool/om_tool_prog.F90

  LIBS
    fckit
    ecom
    eccodes
    eccodes_f90
    multio-fapi

  LINKER_LANGUAGE Fortran

)

ecbuild_add_executable(

  TARGET ecom-parser

  SOURCES
    yaml/yaml_parser_prog.F90

  LIBS
    fckit
    ecom
    eccodes
    eccodes_f90
    multio-fapi

  LINKER_LANGUAGE Fortran

)
endif()


#set_target_properties( ecom PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module/ecom )
#target_include_directories( ecom PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/module/ecom> )

endif()
