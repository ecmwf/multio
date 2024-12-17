file(GLOB mapping_rules RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.yaml")


# Loop through each entry and add it as a subdirectory if it's a directory
foreach(rule ${mapping_rules})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${rule}
                   ${CMAKE_CURRENT_BINARY_DIR}/${rule}
                   COPYONLY)
endforeach()


install(
    FILES       ${mapping_rules}
    DESTINATION ${MULTIOM_CONFIG_DIR}/${KNOWLEDGE_VERSION}/mappings
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
