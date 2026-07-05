# -----------------------------------------------------------------------------
# Configure helper scripts
# -----------------------------------------------------------------------------

set(BINDIR ${CMAKE_INSTALL_PREFIX}/bin)
set(SHAREDIR ${CMAKE_INSTALL_PREFIX}/share)


set(MTG2_CONVERT_SCRIPT_IN
    ${CMAKE_CURRENT_SOURCE_DIR}/mtg2-convert.sh.in
)

set(MTG2_CONVERT_SCRIPT_OUT
    ${CMAKE_CURRENT_BINARY_DIR}/mtg2-convert.sh
)

configure_file(
    ${MTG2_CONVERT_SCRIPT_IN}
    ${MTG2_CONVERT_SCRIPT_OUT}
    @ONLY
)

# -----------------------------------------------------------------------------
# Install scripts into bin/
# -----------------------------------------------------------------------------

install(
    PROGRAMS
        ${MTG2_CONVERT_SCRIPT_OUT}
        ${CMAKE_CURRENT_SOURCE_DIR}/mtg2-list.sh
    DESTINATION ${CMAKE_INSTALL_BINDIR}
)

# -----------------------------------------------------------------------------
# Install reference options file into share/multio/grib1-to-grib2/
# -----------------------------------------------------------------------------

install(
    FILES ${CMAKE_CURRENT_SOURCE_DIR}/optionfile.yaml
    DESTINATION ${CMAKE_INSTALL_DATADIR}/multio/grib1-to-grib2
)
