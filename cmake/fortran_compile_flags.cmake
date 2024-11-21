# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

if(CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
  set(autopromote_flags   "-sreal64")
  set(checkbounds_flags   "-Rb")
  set(fpe_flags           "-Ktrap=fp")
  set(initsnan_flags      "-ei")

  set(convert_flags       "")
  set(linelength_flags    "-N 1023")
  set(nofma_flags         "-h nofma")


elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(autopromote_flags   "-fdefault-real-8 -fdefault-double-8")
  #set(checkbounds_flags   "")
  set(checkbounds_flags   "-fcheck=bounds")
  set(fpe_flags           "-ffpe-trap=invalid,zero,overflow")
  set(initsnan_flags      "-finit-real=snan")

  set(convert_flags       "-fconvert=big-endian")
  set(linelength_flags    "-ffree-line-length-none")
  set(nofma_flags         "")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(autopromote_flags   "-real-size 64")
  #set(checkbounds_flags   "-check bounds")
  set(checkbounds_flags   "")
  set(initsnan_flags      "-init=snan")
  set(fpe_flags           "-fpe0")

  #set(convert_flags       "-convert big-endian")
  set(convert_flags       "")
  set(linelength_flags    "")
  set(nofma_flags         "-no-fma")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC")
  set(autopromote_flags   "-r8")
  set(fpe_flags           "-Ktrap=fp")
  set(checkbounds_flags   "-Mbounds")

  set(convert_flags       "-byteswapio")
  set(linelength_flags    "-Mfree -Mextend")
  set(nofma_flags         "")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Flang")
  set(autopromote_flags   "-fdefault-real-8")
  set(fpe_flags           "-ffp-exception-behavior=strict")

  set(convert_flags       "-fconvert=big-endian")
  set(linelength_flags    "-ffree-line-length-none")
  set(nofma_flags         "-fno-fma")
endif()

if( NOT HAVE_SINGLE_PRECISION )
  ecbuild_add_fortran_flags( "${autopromote_flags}"   NAME autopromote )
endif()

foreach( flag    fpe initsnan checkbounds convert linelength nofma )
  if( ${flag}_flags )
    ecbuild_add_fortran_flags( "${${flag}_flags}" NAME ${flag} )
  endif()
endforeach()

## Debug flags for NVHPC are applied selectively to sourcefiles in src/ecwam/CMakeLists.txt
if( CMAKE_BUILD_TYPE MATCHES "Debug" AND NOT CMAKE_Fortran_COMPILER_ID MATCHES PGI|NVHPC )
  if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    # In case '-check all' has been added, we need to remove the '-check arg_temp_created' warnings
    ecbuild_add_fortran_flags( "-check noarg_temp_created" NAME check_noarg_temp_created BUILD DEBUG ) # the BUILD DEBUG argument makes sure it is appended after '-check all'
  endif()
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  # if( NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 10 )
  #   ecbuild_add_fortran_flags( "-fallow-argument-mismatch" NAME argument_mismatch )
  # endif()
  ecbuild_add_fortran_flags( "-Wall -Wpedantic" NAME warnings )
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "Flang")
  # Linker complains of unknown arguments:
  #    warning: argument unused during compilation: '-fdefault-real-8' [-Wunused-command-line-argument]
  foreach( LINKER_FLAGS CMAKE_EXE_LINKER_FLAGS CMAKE_SHARED_LINKER_FLAGS CMAKE_STATIC_LINKER_FLAGS )
    set( ${LINKER_FLAGS} "${${LINKER_FLAGS}} -Wno-unused-command-line-argument")
  endforeach()
endif()
