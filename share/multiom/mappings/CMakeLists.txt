file(GLOB mapping_rules RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.yaml")

# Mapping rules used to fix wrong information coming from IFS
add_subdirectory(fix)

# Mapping rules used to inject information or to configure the encoding
add_subdirectory(rules)


# Loop through each entry and add it as a subdirectory if it's a directory
foreach(rule ${mapping_rules})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${rule}
                   ${CMAKE_CURRENT_BINARY_DIR}/${rule}
                   COPYONLY)
endforeach()


install(
    FILES       ${mapping_rules}
    DESTINATION ${MULTIOM_CONFIG_DIR}/mappings
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
