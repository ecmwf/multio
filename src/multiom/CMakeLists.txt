#
# Configure the preprocessor file for version info
include( ${CMAKE_CURRENT_SOURCE_DIR}/cmake/ConfigurePreprocessorInfo.cmake )

#
# Add Subdirectories
add_subdirectory( core )
add_subdirectory( encoders )
add_subdirectory( ifs2mars )

#
# Building the tools
add_subdirectory( tools )






