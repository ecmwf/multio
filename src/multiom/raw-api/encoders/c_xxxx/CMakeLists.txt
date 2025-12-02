file(GLOB api_c_interface RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.h")


# Loop through each entry and add it as a subdirectory if it's a directory
foreach(rule ${encoding_rules})
    message( " Copy ${rule} to the build directory" )
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${api_c_interface}
                   ${CMAKE_CURRENT_BINARY_DIR}/${api_c_interface}
                   COPYONLY)
endforeach()


message( " Copy api_encoders.h to the build directory" )
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/api_encoders.h
               ${CMAKE_CURRENT_BINARY_DIR}/api_encoders.h
               COPYONLY)

install(
    FILES       api_encoders.h
    DESTINATION ${ECOM_INCLUDE_DIR}
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
