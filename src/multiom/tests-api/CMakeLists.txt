if (HAVE_OUTPUT_MANAGER_TESTS AND HAVE_MULTIO_BUILD_TOOLS)

# Define the directory containing the test files
set(TEST_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS " ** mutliom test dir is: ${TEST_DIR}")


# Get all the files matching the pattern "test_*_prog.c"
file(GLOB TEST_FILES_C "${TEST_DIR}/test_*_prog.c")

# Loop over the files and create tests
foreach(TEST_FILE ${TEST_FILES_C})

  # Get the filename without the directory path
  get_filename_component(TEST_FILENAME ${TEST_FILE} NAME)

  # Remove the ".c" extension
  string(REPLACE "_prog.c" "" TEST_BASE ${TEST_FILENAME})

  # Create the test name by prepending "multiom_"
  set(TEST_NAME "multiom_${TEST_BASE}.x")


  # Loggin the test name and test file
  message(STATUS "    - Add test file: ${TEST_FILE}")
  message(STATUS "    - Add test base: ${TEST_BASE}")
  message(STATUS "    - Add test name: ${TEST_NAME}")

  ecbuild_add_executable(

    TARGET ${TEST_NAME}

    SOURCES
      ${TEST_FILE}


    DEFINITIONS ${OUTPUT_MANAGER_BUILD_FLAVOUR}

    INCLUDES
      $<BUILD_INTERFACE:${ECOM_INCLUDE_DIR}>
      $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/multio/src/multiom/api/c>

    LIBS
      fckit
      multiom
      eccodes
      eccodes_f90
      multio-fapi

  )

endforeach()

endif()
