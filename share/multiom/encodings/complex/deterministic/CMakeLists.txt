
file(GLOB encoding_rules RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.yaml")

file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/complex/deterministic/ml")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/complex/deterministic/pl")

add_subdirectory("ml")
add_subdirectory("pl")

# Loop through each entry and add it as a subdirectory if it's a directory
foreach(rule ${encoding_rules})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${rule}
                   ${CMAKE_CURRENT_BINARY_DIR}/${rule}
                   COPYONLY)

    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${rule}
                   ${CMAKE_BINARY_DIR}/share/multiom/encodings/complex/deterministic/${rule}
                   COPYONLY)
endforeach()


install(
    FILES       ${encoding_rules}
    DESTINATION ${MULTIOM_CONFIG_DIR}/encodings/complex/deterministic
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
