if(HAVE_MIR)

#
# Local variables to help code readability
# ========================================
set(INTERPOLATE_BUILD_PATH ${multio_BINARY_DIR}/tests/actions/interpolate )
set(INTERPOLATE_TEST_PREFIX test_multio_interpolate )
set(INTERPOLATE_EXE ${CMAKE_BINARY_DIR}/bin/multio-interpolate.sh )
set(INTERPOLATE_SERVER_DATA_PATH multio/tests/actions/interpolate/data )

#
# Preparation of the working directories
# ======================================

#
# Creation of the directory subtree for interpolation data to be downloaded
file( MAKE_DIRECTORY ${INTERPOLATE_BUILD_PATH}/data )
file( MAKE_DIRECTORY ${INTERPOLATE_BUILD_PATH}/data/regularll )
file( MAKE_DIRECTORY ${INTERPOLATE_BUILD_PATH}/data/reducedgg )
file( MAKE_DIRECTORY ${INTERPOLATE_BUILD_PATH}/data/spherical )

#
# Definition of the data to be downloaded from the server
# =======================================================

#
# Definition of all the files related to regular spaced grids
list( APPEND _interpolate_test_data_regularll
   "MarsRegularllOrig.grib"
)

#
# Definition of all the files related to reduced gaussian grids
list( APPEND _interpolate_test_data_reducedgg
   "MarsCroppedReducedgg025x025.grib"
   "MarsCroppedReducedgg050x050.grib"
   "MarsCroppedReducedgg100x100.grib"
   "MarsFullFrameReducedgg025x025.grib"
   "MarsFullFrameReducedgg050x050.grib"
   "MarsFullFrameReducedgg100x100.grib"
   "MarsReducedggOrig.grib"
)

#
# Definition of all the files related to spectral grids
list( APPEND _interpolate_test_data_spherical
   "MarsCroppedSpherical025x025.grib"
   "MarsCroppedSpherical050x050.grib"
   "MarsCroppedSpherical100x100.grib"
   "MarsFullFrameSpherical025x025.grib"
   "MarsFullFrameSpherical050x050.grib"
   "MarsFullFrameSpherical100x100.grib"
   "MarsSphericalOrig.grib"
)

#
# Definition of the points per latitude file needed for O1280 atlas grid
list( APPEND _interpolate_test_data_grid_definitons
   "PlO1280.dat"
)

#
# Download test data
# ==================

set( regularllDataTargetName ${INTERPOLATE_TEST_PREFIX}_get_regularll_data )
set( reducedggDataTargetName ${INTERPOLATE_TEST_PREFIX}_get_reducedgg_data )
set( sphericalDataTargetName ${INTERPOLATE_TEST_PREFIX}_get_spherical_data )
set( pointsPerLatitudeDataTargetName ${INTERPOLATE_TEST_PREFIX}_get_points_per_latitude_data  )


#
# Dowload all the files related to reduced gaussian grids

ecbuild_get_test_multidata(
    TARGET ${regularllDataTargetName}
    DIRNAME ${INTERPOLATE_SERVER_DATA_PATH}/regularll
    DIRLOCAL ${INTERPOLATE_BUILD_PATH}/data/regularll
    NAMES ${_interpolate_test_data_regularll}
    )

 #
 # Dowload all the files related to reduced gaussian grids

 ecbuild_get_test_multidata(
     TARGET ${reducedggDataTargetName}
     DIRNAME ${INTERPOLATE_SERVER_DATA_PATH}/reducedgg
     DIRLOCAL ${INTERPOLATE_BUILD_PATH}/data/reducedgg
     NAMES ${_interpolate_test_data_reducedgg}
     )

 #
 # Download all the files related to spectral grids

 ecbuild_get_test_multidata(
     TARGET ${sphericalDataTargetName}
     DIRNAME ${INTERPOLATE_SERVER_DATA_PATH}/spherical
     DIRLOCAL ${INTERPOLATE_BUILD_PATH}/data/spherical
     NAMES ${_interpolate_test_data_spherical}
     )

 #
 # Dowload the points per latitude file needed for O1280 grid

 ecbuild_get_test_multidata(
     TARGET ${pointsPerLatitudeDataTargetName}
     DIRNAME ${INTERPOLATE_SERVER_DATA_PATH}
     DIRLOCAL ${INTERPOLATE_BUILD_PATH}/data
     NAMES ${_interpolate_test_data_grid_definitons}
     )


#
# Configure the interpolation helper and put it into the binary directory
# =======================================================================

configure_file( multio-interpolate.sh.in   ${CMAKE_BINARY_DIR}/bin/multio-interpolate.sh @ONLY )


#
# Definition of test rages
# ========================

list( APPEND _interpolate_tests_dependencies
  ${regularllDataTargetName}
  ${reducedggDataTargetName}
  ${sphericalDataTargetName}
  ${pointsPerLatitudeDataTargetName}
)

list( APPEND _interpolate_testcases
    "encode-decode"
    "empty"
    "full-frame"
    "cropped"
)

list( APPEND _interpolate_tests_parsers_kind
    "atlas"
    "pure-forwarding"
)

list( APPEND _interpolate_tests_grids_kind
    "Reducedgg"
    "Spherical"
)

list( APPEND _interpolate_tests_grid_spacings
    "100"
    "050"
    "025"
)


#
# Test definitions in the subdirectories
# ======================================
foreach( testcase ${_interpolate_testcases} )
    add_subdirectory(${testcase})
endforeach()

endif(HAVE_MIR)