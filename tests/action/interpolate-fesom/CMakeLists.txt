set(PREFIX multio_tests_action_interpolate-fesom)

configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/fesom_CORE2_ngrid_mappings.yaml ${CMAKE_CURRENT_BINARY_DIR}/fesom_CORE2_ngrid_mappings.yaml )

ecbuild_get_test_multidata(
    TARGET   ${PREFIX}_get_data
    DIRNAME  multio/tests/actions/interpolate-fesom
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES
        "fesom_CORE2_mesh.atlas"
        "fesom_CORE2_ngrid_feed_o2d.grib"
        "fesom_CORE2_ngrid_feed_o3d.grib"
        "fesom_to_HEALPix32.tmpl"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_double_double_o2d_onthefly_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_double_double_o2d_precomputed_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_single_double_o2d_onthefly_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_single_double_o2d_precomputed_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_double_double_o3d_onthefly_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_double_double_o3d_precomputed_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_single_double_o3d_onthefly_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_single_double_o3d_precomputed_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_double_single_o2d_onthefly_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_double_single_o2d_precomputed_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_single_single_o2d_onthefly_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_single_single_o2d_precomputed_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_double_single_o3d_onthefly_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_double_single_o3d_precomputed_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_single_single_o3d_onthefly_cache.grib"
        "Reference_fesom_CORE2_ngrid_to_HEALPix_32_ring_single_single_o3d_precomputed_cache.grib"
)


ecbuild_get_test_multidata(
    TARGET   ${PREFIX}_get_cache
    DIRNAME  multio/tests/actions/interpolate-fesom/cache
    DIRLOCAL ${CMAKE_CURRENT_BINARY_DIR}
    NAMES
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00000000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00001000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00002000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00003000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00004000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00005000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00006000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00007000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00008000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00009000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00010000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00011000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00012000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00013000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00014000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00015000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00016000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00017000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00018000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00019000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00020000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00021000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00022000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00023000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00024000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00025000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00026000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00027000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00028000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00029000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00030000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00031000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00032000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00033000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00034000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00035000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00036000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00037000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00038000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00039000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00040000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00041000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00042000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00043000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00044000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00045000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00046000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_double_ring_00047000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00000000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00001000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00002000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00003000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00004000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00005000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00006000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00007000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00008000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00009000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00010000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00011000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00012000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00013000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00014000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00015000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00016000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00017000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00018000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00019000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00020000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00021000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00022000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00023000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00024000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00025000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00026000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00027000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00028000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00029000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00030000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00031000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00032000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00033000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00034000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00035000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00036000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00037000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00038000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00039000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00040000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00041000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00042000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00043000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00044000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00045000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00046000.atlas"
        "fesom_CORE2_ngrid_to_HEALPix_000032_single_ring_00047000.atlas"
)

foreach(_gridType   CORE2 )
    foreach(_gridDomain  ngrid )
        foreach(_cachePrecision   single double)
            foreach(_cacheMode   precomputed)
                foreach(_outputPrecision  single double)
                    foreach(_levelType   o2d o3d)

                        set(_test fesom_${_gridType}_${_gridDomain}_to_HEALPix_32_ring_${_cachePrecision}_${_outputPrecision}_${_levelType}_${_cacheMode}_cache)
                        set(_result MultIO_${_test}.grib)
                        set(_feed fesom_${_gridType}_${_gridDomain}_feed_${_levelType}.grib)
                        set(_reference Reference_${_test}.grib)


                        ecbuild_add_test(
                            TARGET       ${PREFIX}_${_test}_run
                            TEST_DEPENDS ${PREFIX}_get_data ${PREFIX}_get_cache
                            COMMAND      multio-feed
                            ARGS         --decode --plans=${CMAKE_CURRENT_SOURCE_DIR}/${_test}.yaml ${_feed})

                        ecbuild_add_test(
                            ENABLED      OFF
                            TARGET       ${PREFIX}_${_test}_check_metadata
                            TEST_DEPENDS ${PREFIX}_${_test}_run
                            COMMAND      grib_compare
                            ARGS         -H ${_reference} ${_result})

                        ecbuild_add_test(
                            ENABLED      OFF
                            TARGET       ${PREFIX}_${_test}_check_values
                            TEST_DEPENDS ${PREFIX}_${_test}_check_metadata
                            COMMAND      grib_compare
                            ARGS         -P -T10 ${_reference} ${_result})

                    endforeach()
                endforeach()
            endforeach()
        endforeach()
    endforeach()
endforeach()
