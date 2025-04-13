set(MULTIOM_TOOLS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + tools sources: ${MULTIOM_TOOLS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


set( MULTIOM_TOOLS_MAIN_SOURCES
  ${MULTIOM_TOOLS_DIR}/command_arguments_parser_mod.F90
  ${MULTIOM_TOOLS_DIR}/command_arguments_tokenizer_mod.F90
  ${MULTIOM_TOOLS_DIR}/multiom_tools_utils_mod.F90
)

# Collect source files in module2
set( MULTIOM_TOOLS_SOURCES
  ${MULTIOM_TOOLS_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in tools directory"
)

# ======================================================================
ecbuild_add_library(
  TARGET  multiom-tools-utils
  SOURCES ${MULTIOM_TOOLS_SOURCES}

  DEFINITIONS ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  PRIVATE_INCLUDES
      $<BUILD_INTERFACE:${ECOM_INCLUDE_DIR}>

  PUBLIC_LIBS
    fckit
    eccodes
    eccodes_f90
    multio-fapi
    multiom
)


# Module directory
set_target_properties( multiom-tools-utils PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module/multiom )
target_include_directories( multiom-tools-utils PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/module/multiom> )


ecbuild_add_executable(

  TARGET multiom-cmd-args

  SOURCES
    ${MULTIOM_TOOLS_DIR}/multiom_cmd_args_prog.F90


  DEFINITIONS ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  INCLUDES
      $<BUILD_INTERFACE:${ECOM_INCLUDE_DIR}>

  LIBS
    fckit
    eccodes
    eccodes_f90
    multio-fapi
    multiom
    multiom-tools-utils

  LINKER_LANGUAGE Fortran

)


ecbuild_add_executable(

  TARGET multiom-nested-rules-reader

  SOURCES
    ${MULTIOM_TOOLS_DIR}/multiom_nested_rules_reader_prog.F90


  DEFINITIONS ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  INCLUDES
      $<BUILD_INTERFACE:${ECOM_INCLUDE_DIR}>

  LIBS
    fckit
    eccodes
    eccodes_f90
    multio-fapi
    multiom
    multiom-tools-utils

  LINKER_LANGUAGE Fortran

)


ecbuild_add_executable(

  TARGET multiom-feed

  CONDITION
    HAVE_MULTIO_BUILD_TOOLS

  SOURCES
    ${MULTIOM_TOOLS_DIR}/multiom_feed_prog.F90

  DEFINITIONS ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  INCLUDES
      $<BUILD_INTERFACE:${ECOM_INCLUDE_DIR}>

  LIBS
    fckit
    eccodes
    eccodes_f90
    multio-fapi
    multiom
    multiom-tools-utils

  LINKER_LANGUAGE Fortran

)


ecbuild_add_executable(

  TARGET multiom-list

  CONDITION
    HAVE_MULTIO_BUILD_TOOLS

  SOURCES
    ${MULTIOM_TOOLS_DIR}/multiom_list_prog.F90


  DEFINITIONS ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  INCLUDES
      $<BUILD_INTERFACE:${ECOM_INCLUDE_DIR}>

  LIBS
    fckit
    eccodes
    eccodes_f90
    multio-fapi
    multiom
    multiom-tools-utils

  LINKER_LANGUAGE Fortran

)



ecbuild_add_executable(

  TARGET multiom-params

  CONDITION
    HAVE_MULTIO_BUILD_TOOLS

  SOURCES
    ${MULTIOM_TOOLS_DIR}/multiom_par_prog.F90


  DEFINITIONS ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  INCLUDES
      $<BUILD_INTERFACE:${ECOM_INCLUDE_DIR}>

  LIBS
    fckit
    eccodes
    eccodes_f90
    multio-fapi
    multiom
    multiom-tools-utils

  LINKER_LANGUAGE Fortran

)



ecbuild_add_executable(

  TARGET
      grib1-to-grib2

  CONDITION
    HAVE_MULTIO_BUILD_TOOLS

  SOURCES
      multiom-mtg2.cc
      ../../multio/tools/MultioTool.cc

  NO_AS_NEEDED

  LIBS
      multio
      multiom
      eckit
      fckit
      metkit
      eccodes
      eccodes_f90
)
