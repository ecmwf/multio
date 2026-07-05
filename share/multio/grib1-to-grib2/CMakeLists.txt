# -----------------------------------------------------------------------------
# Configure helper scripts
# -----------------------------------------------------------------------------

set(DIST_GRIB1_TO_GRIB2_SCRIPT_IN
    ${CMAKE_CURRENT_SOURCE_DIR}/dist-grib1-to-grib2.sh.in
)

set(DIST_GRIB1_TO_GRIB2_SCRIPT_OUT
    ${CMAKE_CURRENT_BINARY_DIR}/dist-grib1-to-grib2.sh
)

set(SUBMIT_DIST_GRIB1_TO_GRIB2_SCRIPT_IN
    ${CMAKE_CURRENT_SOURCE_DIR}/submit-dist-grib1-to-grib2.sh.in
)

set(SUBMIT_DIST_GRIB1_TO_GRIB2_SCRIPT_OUT
    ${CMAKE_CURRENT_BINARY_DIR}/submit-dist-grib1-to-grib2.sh
)

configure_file(
    ${DIST_GRIB1_TO_GRIB2_SCRIPT_IN}
    ${DIST_GRIB1_TO_GRIB2_SCRIPT_OUT}
    @ONLY
)

configure_file(
    ${SUBMIT_DIST_GRIB1_TO_GRIB2_SCRIPT_IN}
    ${SUBMIT_DIST_GRIB1_TO_GRIB2_SCRIPT_OUT}
    @ONLY
)

# -----------------------------------------------------------------------------
# Install scripts into bin/
# -----------------------------------------------------------------------------

install(
    PROGRAMS
        ${DIST_GRIB1_TO_GRIB2_SCRIPT_OUT}
        ${SUBMIT_DIST_GRIB1_TO_GRIB2_SCRIPT_OUT}
        ${CMAKE_CURRENT_SOURCE_DIR}/grib1-to-grib2-gather-lists.sh
    DESTINATION ${CMAKE_INSTALL_BINDIR}
)

# -----------------------------------------------------------------------------
# Install reference options file into share/multio/grib1-to-grib2/
# -----------------------------------------------------------------------------

install(
    FILES ${CMAKE_CURRENT_SOURCE_DIR}/optionfile.yaml
    DESTINATION ${CMAKE_INSTALL_DATADIR}/multio/grib1-to-grib2
)
