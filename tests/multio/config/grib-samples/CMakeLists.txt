
file(GLOB config_tmpl_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.tmpl")

install(
    FILES       ${config_tmpl_files}
    DESTINATION ${MULTIO_CONFIG_DIR}/grib-samples
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)

add_subdirectory(samples.O80)
add_subdirectory(samples.O400)
add_subdirectory(samples.O1280)
add_subdirectory(samples.O2560)
