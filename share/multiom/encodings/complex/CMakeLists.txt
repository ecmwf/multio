
file(GLOB encoding_rules RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.yaml")

file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/complex/deterministic")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/complex/ensemble")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/complex/reforecast_ensemble")

add_subdirectory("deterministic")
add_subdirectory("ensemble")
add_subdirectory("reforecast_ensemble")

# Loop through each entry and add it as a subdirectory if it's a directory
foreach(rule ${encoding_rules})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${rule}
                   ${CMAKE_CURRENT_BINARY_DIR}/${rule}
                   COPYONLY)

    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${rule}
                   ${CMAKE_BINARY_DIR}/share/multiom/encodings/complex/${rule}
                   COPYONLY)
endforeach()


install(
    FILES       ${encoding_rules}
    DESTINATION ${MULTIOM_CONFIG_DIR}/encodings/complex
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
