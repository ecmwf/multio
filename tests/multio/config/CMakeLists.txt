file(GLOB config_yaml_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.yaml")
file(GLOB config_tmpl_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.tmpl")

install(
    FILES       ${config_yaml_files}
    DESTINATION ${MULTIO_CONFIG_DIR}
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)


install(
    FILES       ${config_tmpl_files}
    DESTINATION ${MULTIO_CONFIG_DIR}
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)

add_subdirectory(metadata-mapping)
