file(GLOB api_c_interface RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.h")


# Loop through each entry and add it as a subdirectory if it's a directory
foreach(rule ${encoding_rules})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${api_c_interface}
                   ${CMAKE_CURRENT_BINARY_DIR}/${api_c_interface}
                   COPYONLY)
endforeach()


install(
    FILES       ${api_c_interface}
    DESTINATION ${MULTIOM_CONFIG_DIR}/api/c/
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
