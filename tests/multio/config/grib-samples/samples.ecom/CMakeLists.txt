file(GLOB ecom_samples_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.tmpl")

install(
    FILES       ${ecom_samples_files}
    DESTINATION ${MULTIO_CONFIG_DIR}/grib-samples/samples.ecom
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ
)
