file(GLOB config_tmpl_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.tmpl")

install(
    FILES       ${config_tmpl_files}
    DESTINATION ${MULTIO_CONFIG_DIR}/grib-samples/samples.O1280
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
