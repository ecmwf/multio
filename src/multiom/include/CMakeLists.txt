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



# Path to the configured file
set(ECOM_INCLUDE_UTILS_FILE ${CMAKE_CURRENT_BINARY_DIR}/standard/output_manager_preprocessor_utils.h.in)

# Remove the file if it exists
if(EXISTS ${ECOM_VERSION_FILE})
    file(REMOVE ${ECOM_VERSION_FILE})
endif()

# Generate the file
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/output_manager_version_mod.F90.in
  ${ECOM_VERSION_FILE}
)




# Clean the directory if it exists
if(EXISTS ${ECOM_INCLUDE_DIR})
    file(REMOVE_RECURSE ${ECOM_INCLUDE_DIR})
endif()

# Create the directory
file(MAKE_DIRECTORY ${ECOM_INCLUDE_DIR})


#
# + CONFIGURE HEADERS
# ======================================================================
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_utils.h.in         ${ECOM_INCLUDE_DIR}/output_manager_preprocessor_utils.h  )
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_trace_utils.h.in   ${ECOM_INCLUDE_DIR}/output_manager_preprocessor_trace_utils.h  )
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_logging_utils.h.in ${ECOM_INCLUDE_DIR}/output_manager_preprocessor_logging_utils.h  )
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_errhdl_utils.h.in  ${ECOM_INCLUDE_DIR}/output_manager_preprocessor_errhdl_utils.h  )

endif()