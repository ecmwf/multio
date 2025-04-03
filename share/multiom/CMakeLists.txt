file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/mappings")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/plans")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/samples")

add_subdirectory(mappings)
add_subdirectory(encodings)
add_subdirectory(plans)
add_subdirectory(samples)

# Copy the output manager configuration files
file(GLOB output_manager_main_file RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.yaml")

# Loop through each entry and add it as a subdirectory if it's a directory
foreach(ommf ${output_manager_main_file})

    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${ommf}
                   ${CMAKE_CURRENT_BINARY_DIR}/${ommf}
                   COPYONLY)

    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${ommf}
                   ${CMAKE_BINARY_DIR}/share/multiom/${ommf}
                   COPYONLY)

    install(
        FILES       ${ommf}
        DESTINATION ${MULTIOM_CONFIG_DIR}
        PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)


endforeach()