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
# Install reference options file into share/multio/grib-to-mtg2/
# -----------------------------------------------------------------------------

install(
    FILES
        ${CMAKE_CURRENT_SOURCE_DIR}/optionfile.yaml
        ${CMAKE_CURRENT_SOURCE_DIR}/optionfile-full.yaml
        ${CMAKE_CURRENT_SOURCE_DIR}/grib-to-mtg2-options-example.yaml
    DESTINATION ${CMAKE_INSTALL_DATADIR}/multio/grib-to-mtg2
)

# -----------------------------------------------------------------------------
# Deprecated: compatibility symlink share/multio/grib1-to-grib2 -> grib-to-mtg2
# for scripts that still use the old install path. Remove in a later release.
# -----------------------------------------------------------------------------

install(CODE "
    set(_mtg2_old_share \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/multio/grib1-to-grib2\")
    if(IS_SYMLINK \"\${_mtg2_old_share}\" OR NOT EXISTS \"\${_mtg2_old_share}\")
        file(REMOVE \"\${_mtg2_old_share}\")
        file(CREATE_LINK grib-to-mtg2 \"\${_mtg2_old_share}\" SYMBOLIC)
        message(STATUS \"Installing: \${_mtg2_old_share} -> grib-to-mtg2 (deprecated compatibility link)\")
    else()
        message(WARNING \"\${_mtg2_old_share} exists and is not a symlink: compatibility link not created\")
    endif()
")
