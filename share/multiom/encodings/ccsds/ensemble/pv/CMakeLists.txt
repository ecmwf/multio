
file(GLOB encoding_rules RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.yaml")






# Loop through each entry and add it as a subdirectory if it's a directory
foreach(rule ${encoding_rules})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${rule}
                   ${CMAKE_CURRENT_BINARY_DIR}/${rule}
                   COPYONLY)

    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${rule}
                   ${CMAKE_BINARY_DIR}/share/multiom/encodings/ccsds/ensemble/pv/${rule}
                   COPYONLY)
endforeach()


install(
    FILES       ${encoding_rules}
    DESTINATION ${MULTIOM_CONFIG_DIR}/encodings/ccsds/ensemble/pv
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
