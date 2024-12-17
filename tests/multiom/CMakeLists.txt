add_subdirectory(knowledge)

file(GLOB output_manager_main_file RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.yaml")


# Loop through each entry and add it as a subdirectory if it's a directory
foreach(ommf ${output_manager_main_file})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${ommf}
                   ${CMAKE_CURRENT_BINARY_DIR}/${ommf}
                   COPYONLY)
endforeach()


install(
    FILES       ${output_manager_main_file}
    DESTINATION ${MULTIOM_CONFIG_DIR}
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)


# Loop through each entry and add it as a subdirectory if it's a directory
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/run.sh
    ${CMAKE_BINARY_DIR}/bin/run.sh
    COPYONLY)

install(
    FILES       ${CMAKE_CURRENT_SOURCE_DIR}/run.sh
    DESTINATION bin
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)