configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/statistics_param_mappings.yml
    ${CMAKE_CURRENT_BINARY_DIR}/statistics_param_mappings.yml
    COPYONLY
)

install(
    FILES       statistics_param_mappings.yml
    DESTINATION ${MULTIO_SHARE_DIR}/multio/statistics-mtg2
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ
)
