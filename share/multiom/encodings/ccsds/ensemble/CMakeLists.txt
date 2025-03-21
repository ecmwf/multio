
file(GLOB encoding_rules RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.yaml")

file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/ccsds/ensemble/al")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/ccsds/ensemble/hl")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/ccsds/ensemble/ml")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/ccsds/ensemble/pl")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/ccsds/ensemble/pt")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/ccsds/ensemble/pv")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/ccsds/ensemble/sfc")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/ccsds/ensemble/sol")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/ccsds/ensemble/al")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/ccsds/ensemble/pt")
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/multiom/encodings/ccsds/ensemble/pv")

add_subdirectory("al")
add_subdirectory("hl")
add_subdirectory("ml")
add_subdirectory("pl")
add_subdirectory("pt")
add_subdirectory("pv")
add_subdirectory("sfc")
add_subdirectory("sol")

# Loop through each entry and add it as a subdirectory if it's a directory
foreach(rule ${encoding_rules})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${rule}
                   ${CMAKE_CURRENT_BINARY_DIR}/${rule}
                   COPYONLY)

    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${rule}
                   ${CMAKE_BINARY_DIR}/share/multiom/encodings/ccsds/ensemble/${rule}
                   COPYONLY)
endforeach()


install(
    FILES       ${encoding_rules}
    DESTINATION ${MULTIOM_CONFIG_DIR}/encodings/ccsds/ensemble
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
