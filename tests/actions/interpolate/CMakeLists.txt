if(HAVE_MIR)

# Creation of the main directory for interpolation tests
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/multio/tests/interpolate )

# Creation of the directory subtree for interpolation data to be downloaded
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/multio/tests/interpolate/data )
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/multio/tests/interpolate/data/reducedgg-PL )
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/multio/tests/interpolate/data/spherical-PL )

# Creation of the main directory subtree for test cases
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped )
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame )
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation )
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEncodeDecode )


# Create the configuration files needed to run the tests

#
# Sanity checks Encode/Decode
configure_file( sanityCheckEncodeDecode/sanityCheckEncodeDecodeReducedGG.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEncodeDecode/sanityCheckEncodeDecodeReducedGG.yaml @ONLY )
configure_file( sanityCheckEncodeDecode/sanityCheckEncodeDecodeReducedGG.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEncodeDecode/sanityCheckEncodeDecodeReducedGG.sh @ONLY )

configure_file( sanityCheckEncodeDecode/sanityCheckEncodeDecodeRegularLL.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEncodeDecode/sanityCheckEncodeDecodeRegularLL.yaml @ONLY )
configure_file( sanityCheckEncodeDecode/sanityCheckEncodeDecodeRegularLL.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEncodeDecode/sanityCheckEncodeDecodeRegularLL.sh @ONLY )

configure_file( sanityCheckEncodeDecode/sanityCheckEncodeDecodeSpherical.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEncodeDecode/sanityCheckEncodeDecodeSpherical.yaml @ONLY )
configure_file( sanityCheckEncodeDecode/sanityCheckEncodeDecodeSpherical.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEncodeDecode/sanityCheckEncodeDecodeSpherical.sh @ONLY )


#
# Sanity checks Empty Interpolation
configure_file( sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationReducedGG.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationReducedGG.yaml @ONLY )
configure_file( sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationReducedGG.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationReducedGG.sh @ONLY )
configure_file( sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasReducedGG.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasReducedGG.yaml @ONLY )
configure_file( sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasReducedGG.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasReducedGG.sh @ONLY )

configure_file( sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationRegularLL.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationRegularLL.yaml @ONLY )
configure_file( sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationRegularLL.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationRegularLL.sh @ONLY )
configure_file( sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasRegularLL.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasRegularLL.yaml @ONLY )
configure_file( sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasRegularLL.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasRegularLL.sh @ONLY )

configure_file( sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationSpherical.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationSpherical.yaml @ONLY )
configure_file( sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationSpherical.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationSpherical.sh @ONLY )
configure_file( sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasSpherical.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasSpherical.yaml @ONLY )
configure_file( sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasSpherical.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/sanityCheckEmptyInterpolation/sanityCheckEmptyInterpolationAtlasSpherical.sh @ONLY )


#
# Interpolate Full Frame
configure_file( interpolateFullFrame/interpolateFullFrameReducedGG_025x025.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameReducedGG_025x025.yaml @ONLY )
configure_file( interpolateFullFrame/interpolateFullFrameReducedGG_025x025.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameReducedGG_025x025.sh @ONLY )

configure_file( interpolateFullFrame/interpolateFullFrameReducedGG_050x050.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameReducedGG_050x050.yaml @ONLY )
configure_file( interpolateFullFrame/interpolateFullFrameReducedGG_050x050.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameReducedGG_050x050.sh @ONLY )

configure_file( interpolateFullFrame/interpolateFullFrameReducedGG_100x100.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameReducedGG_100x100.yaml @ONLY )
configure_file( interpolateFullFrame/interpolateFullFrameReducedGG_100x100.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameReducedGG_100x100.sh @ONLY )


configure_file( interpolateFullFrame/interpolateFullFrameSpherical_025x025.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameSpherical_025x025.yaml @ONLY )
configure_file( interpolateFullFrame/interpolateFullFrameSpherical_025x025.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameSpherical_025x025.sh @ONLY )

configure_file( interpolateFullFrame/interpolateFullFrameSpherical_050x050.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameSpherical_050x050.yaml @ONLY )
configure_file( interpolateFullFrame/interpolateFullFrameSpherical_050x050.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameSpherical_050x050.sh @ONLY )

configure_file( interpolateFullFrame/interpolateFullFrameSpherical_100x100.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameSpherical_100x100.yaml @ONLY )
configure_file( interpolateFullFrame/interpolateFullFrameSpherical_100x100.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameSpherical_100x100.sh @ONLY )



configure_file( interpolateFullFrame/interpolateFullFrameAtlasReducedGG_025x025.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasReducedGG_025x025.yaml @ONLY )
configure_file( interpolateFullFrame/interpolateFullFrameAtlasReducedGG_025x025.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasReducedGG_025x025.sh @ONLY )

configure_file( interpolateFullFrame/interpolateFullFrameAtlasReducedGG_050x050.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasReducedGG_050x050.yaml @ONLY )
configure_file( interpolateFullFrame/interpolateFullFrameAtlasReducedGG_050x050.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasReducedGG_050x050.sh @ONLY )

configure_file( interpolateFullFrame/interpolateFullFrameAtlasReducedGG_100x100.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasReducedGG_100x100.yaml @ONLY )
configure_file( interpolateFullFrame/interpolateFullFrameAtlasReducedGG_100x100.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasReducedGG_100x100.sh @ONLY )


configure_file( interpolateFullFrame/interpolateFullFrameAtlasSpherical_025x025.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasSpherical_025x025.yaml @ONLY )
configure_file( interpolateFullFrame/interpolateFullFrameAtlasSpherical_025x025.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasSpherical_025x025.sh @ONLY )

configure_file( interpolateFullFrame/interpolateFullFrameAtlasSpherical_050x050.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasSpherical_050x050.yaml @ONLY )
configure_file( interpolateFullFrame/interpolateFullFrameAtlasSpherical_050x050.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasSpherical_050x050.sh @ONLY )

configure_file( interpolateFullFrame/interpolateFullFrameAtlasSpherical_100x100.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasSpherical_100x100.yaml @ONLY )
configure_file( interpolateFullFrame/interpolateFullFrameAtlasSpherical_100x100.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateFullFrame/interpolateFullFrameAtlasSpherical_100x100.sh @ONLY )




#
# Interpolate Cropped
configure_file( interpolateCropped/interpolateCroppedReducedGG_025x025.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedReducedGG_025x025.yaml @ONLY )
configure_file( interpolateCropped/interpolateCroppedReducedGG_025x025.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedReducedGG_025x025.sh @ONLY )

configure_file( interpolateCropped/interpolateCroppedReducedGG_050x050.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedReducedGG_050x050.yaml @ONLY )
configure_file( interpolateCropped/interpolateCroppedReducedGG_050x050.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedReducedGG_050x050.sh @ONLY )

configure_file( interpolateCropped/interpolateCroppedReducedGG_100x100.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedReducedGG_100x100.yaml @ONLY )
configure_file( interpolateCropped/interpolateCroppedReducedGG_100x100.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedReducedGG_100x100.sh @ONLY )


configure_file( interpolateCropped/interpolateCroppedSpherical_025x025.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedSpherical_025x025.yaml @ONLY )
configure_file( interpolateCropped/interpolateCroppedSpherical_025x025.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedSpherical_025x025.sh @ONLY )

configure_file( interpolateCropped/interpolateCroppedSpherical_050x050.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedSpherical_050x050.yaml @ONLY )
configure_file( interpolateCropped/interpolateCroppedSpherical_050x050.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedSpherical_050x050.sh @ONLY )

configure_file( interpolateCropped/interpolateCroppedSpherical_100x100.yaml.in ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedSpherical_100x100.yaml @ONLY )
configure_file( interpolateCropped/interpolateCroppedSpherical_100x100.sh.in   ${CMAKE_BINARY_DIR}/multio/tests/interpolate/interpolateCropped/interpolateCroppedSpherical_100x100.sh @ONLY )






endif(HAVE_MIR)
