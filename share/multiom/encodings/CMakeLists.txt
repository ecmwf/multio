
file(GLOB encoding_rules RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.yaml")

file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/ccsds")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/complex")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/simple")

add_subdirectory("ccsds")
add_subdirectory("complex")
add_subdirectory("simple")


# Loop through each entry and add it as a subdirectory if it's a directory
foreach(rule ${encoding_rules})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${rule}
                   ${CMAKE_CURRENT_BINARY_DIR}/${rule}
                   COPYONLY)

    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${rule}
                   ${CMAKE_BINARY_DIR}/share/multiom/encodings/${rule}
                   COPYONLY)
endforeach()


install(
    FILES       ${encoding_rules}
    DESTINATION ${MULTIOM_CONFIG_DIR}/encodings
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
