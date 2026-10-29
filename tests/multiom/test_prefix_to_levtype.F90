! (C) Copyright 2026- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

! Checks the prefix -> levtype mapping used by ifs2mars. Surface-prefix fields with a non-zero
! level are multi-layer fields (soil, snow, sea ice) and map to levtype=sol, except satellite
! fields, whose level indexes the channel and which must stay levtype=sfc.
PROGRAM TEST_PREFIX_TO_LEVTYPE

  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: IPREFIX2ILEVTYPE
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_SURFACE_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_MODEL_LEVEL_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_WAVE_INT_E
  USE :: ENUMERATORS_MOD,   ONLY: LEVTYPE_SFC_E
  USE :: ENUMERATORS_MOD,   ONLY: LEVTYPE_SOL_E
  USE :: ENUMERATORS_MOD,   ONLY: LEVTYPE_ML_E
  USE :: GRIB_CODES_MOD,    ONLY: NGRBCLBT
  USE :: GRIB_CODES_MOD,    ONLY: NGRBCSBT
  USE :: GRIB_CODES_MOD,    ONLY: NGRBCDRFL
  USE :: GRIB_CODES_MOD,    ONLY: NGRBCRRFL

IMPLICIT NONE

  TYPE(HOOKS_T) :: HOOKS
  INTEGER(KIND=JPIB_K) :: NFAIL

  NFAIL = 0_JPIB_K
  CALL HOOKS%DEBUG_HOOK_%INIT( )

  ! Multi-layer surface fields: the level is the layer -> sol
  CALL CHECK( 'soil temperature (260360) layer 2',      PREFIX_SURFACE_E, 260360_JPIB_K, 2_JPIB_K, LEVTYPE_SOL_E )
  CALL CHECK( 'volumetric soil water (260199) layer 1', PREFIX_SURFACE_E, 260199_JPIB_K, 1_JPIB_K, LEVTYPE_SOL_E )
  CALL CHECK( 'sea ice temperature (262024) layer 4',   PREFIX_SURFACE_E, 262024_JPIB_K, 4_JPIB_K, LEVTYPE_SOL_E )
  CALL CHECK( 'snow depth (228141) layer 3',            PREFIX_SURFACE_E, 228141_JPIB_K, 3_JPIB_K, LEVTYPE_SOL_E )

  ! Single-level surface fields -> sfc
  CALL CHECK( '2m temperature (167) level 0',           PREFIX_SURFACE_E, 167_JPIB_K,    0_JPIB_K, LEVTYPE_SFC_E )
  CALL CHECK( 'soil temperature (260360) level 0',      PREFIX_SURFACE_E, 260360_JPIB_K, 0_JPIB_K, LEVTYPE_SFC_E )

  ! Satellite fields: the level is the channel index -> must stay sfc
  CALL CHECK( 'cloudy brightness temperature channel 3',    PREFIX_SURFACE_E, NGRBCLBT,  3_JPIB_K, LEVTYPE_SFC_E )
  CALL CHECK( 'clear-sky brightness temperature channel 1', PREFIX_SURFACE_E, NGRBCSBT,  1_JPIB_K, LEVTYPE_SFC_E )
  CALL CHECK( 'cloudy reflectance channel 7',              PREFIX_SURFACE_E, NGRBCDRFL, 7_JPIB_K, LEVTYPE_SFC_E )
  CALL CHECK( 'clear-sky reflectance channel 12',          PREFIX_SURFACE_E, NGRBCRRFL, 12_JPIB_K, LEVTYPE_SFC_E )

  ! Other prefixes are unaffected by the level
  CALL CHECK( 'model level 137',                       PREFIX_MODEL_LEVEL_E, 130_JPIB_K,    137_JPIB_K, LEVTYPE_ML_E )
  CALL CHECK( 'integrated wave parameter level 0',     PREFIX_WAVE_INT_E,    140229_JPIB_K, 0_JPIB_K,   LEVTYPE_SFC_E )

  CALL HOOKS%DEBUG_HOOK_%FREE( )

  IF ( NFAIL .NE. 0_JPIB_K ) THEN
    WRITE( OUTPUT_UNIT, '(A,I0,A)' ) 'FAILED: ', NFAIL, ' check(s) failed'
    ERROR STOP 1
  END IF
  WRITE( OUTPUT_UNIT, '(A)' ) 'All checks passed'

CONTAINS

  SUBROUTINE CHECK( NAME, IPREFIX, PARAM_ID, LEVEL, EXPECTED )
    CHARACTER(LEN=*),     INTENT(IN) :: NAME
    INTEGER(KIND=JPIB_K), INTENT(IN) :: IPREFIX
    INTEGER(KIND=JPIB_K), INTENT(IN) :: PARAM_ID
    INTEGER(KIND=JPIB_K), INTENT(IN) :: LEVEL
    INTEGER(KIND=JPIB_K), INTENT(IN) :: EXPECTED

    INTEGER(KIND=JPIB_K), PARAMETER :: REPRES = 0_JPIB_K
    INTEGER(KIND=JPIB_K) :: ILEVTYPE
    INTEGER(KIND=JPIB_K) :: RET

    ILEVTYPE = -1_JPIB_K
    RET = IPREFIX2ILEVTYPE( IPREFIX, PARAM_ID, LEVEL, REPRES, ILEVTYPE, HOOKS )

    IF ( RET .NE. 0_JPIB_K ) THEN
      NFAIL = NFAIL + 1_JPIB_K
      WRITE( OUTPUT_UNIT, '(A,A,A,I0)' ) '[FAIL] ', NAME, ': IPREFIX2ILEVTYPE returned ', RET
    ELSEIF ( ILEVTYPE .NE. EXPECTED ) THEN
      NFAIL = NFAIL + 1_JPIB_K
      WRITE( OUTPUT_UNIT, '(A,A,A,I0,A,I0)' ) '[FAIL] ', NAME, ': levtype ', ILEVTYPE, ' expected ', EXPECTED
    ELSE
      WRITE( OUTPUT_UNIT, '(A,A)' ) '[ OK ] ', NAME
    END IF
  END SUBROUTINE CHECK

END PROGRAM TEST_PREFIX_TO_LEVTYPE
