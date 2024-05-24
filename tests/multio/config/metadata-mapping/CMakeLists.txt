
file(GLOB config_yaml_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.yaml")

install(
    FILES       ${config_yaml_files}
    DESTINATION ${MULTIO_CONFIG_DIR}/metadata-mapping
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
