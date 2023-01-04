include_directories( ${ECKIT_INCLUDE_DIRS} )

if( CMAKE_SYSTEM_NAME MATCHES "Linux" AND CMAKE_CXX_COMPILER_ID MATCHES "GNU" )
    list( APPEND _link_libraries
        -Wl,--no-as-needed
    )
endif()

list( APPEND _link_libraries
    multio
    multio-action-aggregation
    multio-action-encode
    multio-action-mask
    multio-action-metadataMapping
    multio-action-null
    multio-action-print
    multio-action-select
    multio-action-singleFieldSink
    multio-action-sink
    multio-action-statistics
    multio-action-transport
    eckit_option
)

if ( HAVE_FDB5 )
    list( APPEND _link_libraries
        multio-fdb5
    )
endif()

if( HAVE_MIR )
    list( APPEND _link_libraries
        multio-action-interpolate
    )
endif()

ecbuild_add_test( TARGET    test_multio_datasink_factory
                  SOURCES   test_multio_datasink_factory.cc
                  LIBS      ${_link_libraries} )

ecbuild_add_test( TARGET    test_multio_multio
                  SOURCES   test_multio_multio.cc TestDataContent.cc TestDataContent.h
                  LIBS      ${_link_libraries} )

ecbuild_add_test( TARGET    test_multio_file_sink
                  SOURCES   test_multio_file_sink.cc TestDataContent.cc TestDataContent.h
                  LIBS      ${_link_libraries} )

ecbuild_add_test( TARGET    test_multio_encode_bitspervalue
                  SOURCES   test_multio_encode_bitspervalue.cc
                  LIBS      ${_link_libraries} )

ecbuild_add_test( TARGET    test_multio_confctx
                  SOURCES   test_multio_confctx.cc
                  LIBS      ${_link_libraries}
                  ENVIRONMENT "MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/server/config"
)

ecbuild_add_test( TARGET    test_multio_capi
                  SOURCES   test_multio_capi.cc
                  CONDITION HAVE_MULTIO_SERVER
                  LIBS      multio-api
                  ENVIRONMENT "MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/server/config"
)

ecbuild_add_test( TARGET    test_multio_fapi_general
                  SOURCES   test_multio_fapi_general.f90
                  CONDITION HAVE_MULTIO_SERVER AND HAVE_FORTRAN
                  LIBS      multio-fapi
                  ENVIRONMENT "MULTIO_SERVER_CONFIG_PATH=${CMAKE_CURRENT_SOURCE_DIR}/server/config"
)
ecbuild_add_test( TARGET    test_multio_maestro
                  SOURCES   test_multio_maestro.cc
                  CONDITION HAVE_MAESTRO
                  LIBS      multio-maestro )

list( APPEND _test_environment
    FDB_HOME=${CMAKE_BINARY_DIR}/multio
    FDB_DEBUG=1
    MULTIO_DEBUG=1
)

list( APPEND _test_data
    "rd.vod.grib"
)



ecbuild_add_executable( TARGET    multio-legacy
                        SOURCES   multio-legacy.cc
                                  ${CMAKE_CURRENT_SOURCE_DIR}/../src/multio/tools/MultioTool.cc
                        CONDITION HAVE_FDB5 AND HAVE_LEGACY_IFSIO AND HAVE_MIR
                        LIBS      ${_link_libraries})

ecbuild_add_executable( TARGET    multio-legacy-hammer
                        SOURCES   multio-legacy-hammer.cc
                                  ${CMAKE_CURRENT_SOURCE_DIR}/../src/multio/tools/MultioTool.cc
                        CONDITION HAVE_FDB5 AND HAVE_LEGACY_IFSIO AND HAVE_MIR
                        LIBS      ${_link_libraries})

ecbuild_get_test_multidata( TARGET multio_get_test_data DIRNAME fdb5/tests/fdb NAMES ${_test_data} )

ecbuild_add_test( TARGET test_multio_ifs_plans
	              CONDITION HAVE_FDB5 AND HAVE_MIR
                  COMMAND $<TARGET_FILE:multio-feed>
                  TEST_DEPENDS multio_get_test_data
                  ARGS ${_test_data}
                  ENVIRONMENT "${_test_environment}" "MULTIO_PLANS={ \"plans\": [{ \"name\": \"test plans fdb5\", \"actions\": [{ \"type\": \"sink\", \"sinks\": [{\"type\": \"fdb5\"}]}]}]}" )

ecbuild_add_test( TARGET test_multio_ifs_io
                  CONDITION HAVE_FDB5
                  COMMAND $<TARGET_FILE:multio-feed>
                  TEST_DEPENDS multio_get_test_data
                  ARGS ${_test_data}
                  ENVIRONMENT "${_test_environment}" )

ecbuild_add_test( TARGET test_multio_subtoc
                  CONDITION HAVE_FDB5
                  COMMAND $<TARGET_FILE:multio-feed>
                  TEST_DEPENDS multio_get_test_data
                  ARGS ${_test_data} --test-subtoc
                  ENVIRONMENT "${_test_environment}" )

ecbuild_add_test( TARGET test_multio_legacy_interface
                  CONDITION HAVE_FDB5 AND HAVE_LEGACY_IFSIO
                  COMMAND $<TARGET_FILE:multio-legacy>
                  TEST_DEPENDS multio_get_test_data
                  ARGS ${_test_data}
                  ENVIRONMENT "${_test_environment}" )





#
#
# Test interpolate action
list( APPEND _interpolate_test_data_reducedgg
   "mars-reducedgg-025x025-cropped.grib"
   "mars-reducedgg-025x025.grib"
   "mars-reducedgg-050x050-cropped.grib"
   "mars-reducedgg-050x050.grib"
   "mars-reducedgg-100x100-cropped.grib"
   "mars-reducedgg-100x100.grib"
   "mars-reducedgg-orig.grib"
)

list( APPEND _interpolate_test_data_spherical
   "mars-spherical-025x025-cropped.grib"
   "mars-spherical-025x025.grib"
   "mars-spherical-050x050-cropped.grib"
   "mars-spherical-050x050.grib"
   "mars-spherical-100x100-cropped.grib"
   "mars-spherical-100x100.grib"
   "mars-spherical-orig.grib"
)

ecbuild_get_test_multidata( 
    TARGET multio_get_interpolate_test_data_reducedgg  
    DIRNAME multio/tests/actions/interpolate/reducedgg-PL 
    DIRLOCAL ${CMAKE_BINARY_DIR}/multio/tests/interpolate/data/reducedgg-PL
    NAMES ${_interpolate_test_data_reducedgg}
    )

ecbuild_get_test_multidata( 
    TARGET multio_get_interpolate_test_data_spherical  
    DIRNAME multio/tests/actions/interpolate/spherical-PL 
    DIRLOCAL ${CMAKE_BINARY_DIR}/multio/tests/interpolate/data/spherical-PL
    NAMES ${_interpolate_test_data_spherical}
    )    

ecbuild_add_test( 
    TARGET test_multio_encodeDecode_reducedgg
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEncodeDecode/sanityCheckEncodeDecodeReducedGG.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test(            
    TARGET test_multio_encodeDecode_spherical
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEncodeDecode/sanityCheckEncodeDecodeSpherical.sh
    TEST_DEPENDS multio_get_interpolate_test_data_spherical
 )

ecbuild_add_test( 
    TARGET test_multio_encodeDecode_regularll
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEncodeDecode/sanityCheckEncodeDecodeRegularLL.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test( 
    TARGET test_multio_emptyInterpolation_reducedgg
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationReducedGG.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test( 
    TARGET test_multio_emptyInterpolation_atlas_reducedgg
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasReducedGG.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test( 
    TARGET test_multio_emptyInterpolation_regularll
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationRegularLL.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test( 
    TARGET test_multio_emptyInterpolation_atlas_regularll
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasRegularLL.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test( 
    TARGET test_multio_emptyInterpolation_spherical
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationSpherical.sh
    TEST_DEPENDS multio_get_interpolate_test_data_spherical
)

ecbuild_add_test( 
    TARGET test_multio_emptyInterpolation_atlas_spherical
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasSpherical.sh
    TEST_DEPENDS multio_get_interpolate_test_data_spherical
)



ecbuild_add_test( 
    TARGET test_multio_interpolateFullFrame_reducedgg_025x025
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameReducedGG_025x025.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test( 
    TARGET test_multio_interpolateFullFrame_reducedgg_050x050
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameReducedGG_050x050.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test( 
    TARGET test_multio_interpolateFullFrame_reducedgg_100x100
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameReducedGG_100x100.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test( 
    TARGET test_multio_interpolateFullFrame_spherical_025x025
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameSpherical_025x025.sh
    TEST_DEPENDS multio_get_interpolate_test_data_spherical
)

ecbuild_add_test( 
    TARGET test_multio_interpolateFullFrame_spherical_050x050
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameSpherical_050x050.sh
    TEST_DEPENDS multio_get_interpolate_test_data_spherical
)

ecbuild_add_test( 
    TARGET test_multio_interpolateFullFrame_spherical_100x100
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameSpherical_100x100.sh
    TEST_DEPENDS multio_get_interpolate_test_data_spherical
)

ecbuild_add_test( 
    TARGET test_multio_interpolateFullFrame_atlas_reducedgg_025x025
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasReducedGG_025x025.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test( 
    TARGET test_multio_interpolateFullFrame_atlas_reducedgg_050x050
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasReducedGG_050x050.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test( 
    TARGET test_multio_interpolateFullFrame_atlas_reducedgg_100x100
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasReducedGG_100x100.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test( 
    TARGET test_multio_interpolateFullFrame_atlas_spherical_025x025
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasSpherical_025x025.sh
    TEST_DEPENDS multio_get_interpolate_test_data_spherical
)

ecbuild_add_test( 
    TARGET test_multio_interpolateFullFrame_atlas_spherical_050x050
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasSpherical_050x050.sh
    TEST_DEPENDS multio_get_interpolate_test_data_spherical
)

ecbuild_add_test( 
    TARGET test_multio_interpolateFullFrame_atlas_spherical_100x100
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasSpherical_100x100.sh
    TEST_DEPENDS multio_get_interpolate_test_data_spherical
)

ecbuild_add_test( 
    TARGET test_multio_interpolateCropped_reducedgg_025x025
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedReducedGG_025x025.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test( 
    TARGET test_multio_interpolateCropped_reducedgg_050x050
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedReducedGG_050x050.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test( 
    TARGET test_multio_interpolateCropped_reducedgg_100x100
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedReducedGG_100x100.sh
    TEST_DEPENDS multio_get_interpolate_test_data_reducedgg
)

ecbuild_add_test( 
    TARGET test_multio_interpolateCropped_spherical_025x025
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedSpherical_025x025.sh
    TEST_DEPENDS multio_get_interpolate_test_data_spherical
)

ecbuild_add_test( 
    TARGET test_multio_interpolateCropped_spherical_050x050
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedSpherical_050x050.sh
    TEST_DEPENDS multio_get_interpolate_test_data_spherical
)

ecbuild_add_test( 
    TARGET test_multio_interpolateCropped_spherical_100x100
    CONDITION HAVE_MIR
    COMMAND ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedSpherical_100x100.sh
    TEST_DEPENDS multio_get_interpolate_test_data_spherical
)


add_subdirectory(multio)
add_subdirectory(server)
add_subdirectory(actions)
