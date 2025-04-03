file(GLOB multio_plans RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.yaml")


# Loop through each entry and add it as a subdirectory if it's a directory
foreach(plan ${multio_plans})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${plan}
                   ${CMAKE_CURRENT_BINARY_DIR}/${plan}
                   COPYONLY)

    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${plan}
                   ${CMAKE_BINARY_DIR}/share/multiom/plans/${plan}
                   COPYONLY)
endforeach()


install(
    FILES       ${multio_plans}
    DESTINATION ${MULTIOM_CONFIG_DIR}/plans
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
