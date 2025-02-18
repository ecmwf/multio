file(GLOB samples RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.tmpl")


# Loop through each entry and add it as a subdirectory if it's a directory
foreach(sample ${samples})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${sample}
                   ${CMAKE_CURRENT_BINARY_DIR}/${sample}
                   COPYONLY)
endforeach()


install(
    FILES       ${samples}
    DESTINATION ${MULTIOM_CONFIG_DIR}/samples
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
