! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'ifs_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'IFS_UTILS_MOD'
MODULE IFS_UTILS_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

! Default visibility of the module
PRIVATE


CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IPREF2MSGTYPE'
PP_THREAD_SAFE FUNCTION IPREF2MSGTYPE( KPREF, MSGTYPE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: ATM_MSG_E
  USE :: ENUMERATORS_MOD,   ONLY: WAM_MSG_E

  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_MODEL_LEVEL_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_PRESSURE_LEVEL_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_VORTICITY_LEVEL_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_THETA_LEVEL_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_SURFACE_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_WAVE_INT_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_WAVE_SPEC_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: KPREF
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: MSGTYPE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PREFIX = 1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Extract the enum for message type from prefix
  SELECT CASE( KPREF )
  CASE ( PREFIX_MODEL_LEVEL_E, PREFIX_PRESSURE_LEVEL_E, PREFIX_VORTICITY_LEVEL_E, PREFIX_THETA_LEVEL_E, PREFIX_SURFACE_E  )
    MSGTYPE = ATM_MSG_E
  CASE ( PREFIX_WAVE_INT_E, PREFIX_WAVE_SPEC_E )
    MSGTYPE = WAM_MSG_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PREFIX )
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (ERRFLAG_UNKNOWN_PREFIX)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown prefix' )

    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )

    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION IPREF2MSGTYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CPREF2IPREF'
PP_THREAD_SAFE FUNCTION  CPREF2IPREF( CDPREF, IPREF, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_MODEL_LEVEL_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_PRESSURE_LEVEL_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_VORTICITY_LEVEL_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_THETA_LEVEL_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_SURFACE_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_WAVE_INT_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_WAVE_SPEC_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: CDPREF
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IPREF
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PREFIX = 1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Error handling TODO: This check is not ok
  ! PP_DEBUG_DEVELOP_COND_THROW( LEN(CDPREF).LT.7,  1 )

  IF(CDPREF(1:2) == 'SF' .OR. CDPREF(1:1) == 's' .OR. CDPREF(1:3) == 'sfc') THEN
    IPREF = PREFIX_SURFACE_E
  ELSEIF(CDPREF(1:2) == 'ML' .OR. CDPREF(1:1) == 'm') THEN
    IPREF = PREFIX_MODEL_LEVEL_E
  ELSEIF(CDPREF(1:2) == 'PL' .OR. CDPREF(1:1) == 'p') THEN
    IPREF = PREFIX_PRESSURE_LEVEL_E
  ELSEIF(CDPREF(1:2) == 'PV' .OR. CDPREF(1:1) == 'v') THEN
    IPREF = PREFIX_VORTICITY_LEVEL_E
  ELSEIF(CDPREF(1:2) == 'TH' .OR. CDPREF(1:1) == 't') THEN
    IPREF = PREFIX_THETA_LEVEL_E
  ELSEIF(CDPREF(1:6) == 'wv_int') THEN
    IPREF = PREFIX_WAVE_INT_E
  ELSEIF(CDPREF(1:7) == 'wv_spec') THEN
    IPREF = PREFIX_WAVE_SPEC_E
  ELSE
    PP_DEBUG_DEVELOP_THROW( ERRFLAG_UNKNOWN_PREFIX )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (ERRFLAG_UNKNOWN_PREFIX)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown prefix: '//TRIM(CDPREF) )

    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )

    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION CPREF2IPREF
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'IPREFIX2ILEVTYPE'
FUNCTION IPREFIX2ILEVTYPE( IPREFIX, PARAM_ID, LEVEL, REPRES, ILEVTYPE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GRIB_CODES_MOD,    ONLY: NGRBRSN
  USE :: GRIB_CODES_MOD,    ONLY: NGRBTSN
  USE :: GRIB_CODES_MOD,    ONLY: NGRBWSN
  USE :: GRIB_CODES_MOD,    ONLY: NGRBSD
  USE :: GRIB_CODES_MOD,    ONLY: NGRB100U
  USE :: GRIB_CODES_MOD,    ONLY: NGRB100V

  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_MODEL_LEVEL_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_PRESSURE_LEVEL_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_VORTICITY_LEVEL_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_THETA_LEVEL_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_SURFACE_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_WAVE_INT_E
  USE :: ENUMERATORS_MOD,   ONLY: PREFIX_WAVE_SPEC_E

  USE :: ENUMERATORS_MOD,   ONLY: LEVTYPE_HHL_E
  USE :: ENUMERATORS_MOD,   ONLY: LEVTYPE_HPL_E
  USE :: ENUMERATORS_MOD,   ONLY: LEVTYPE_HL_E
  USE :: ENUMERATORS_MOD,   ONLY: LEVTYPE_ML_E
  USE :: ENUMERATORS_MOD,   ONLY: LEVTYPE_O2D_E
  USE :: ENUMERATORS_MOD,   ONLY: LEVTYPE_O3D_E
  USE :: ENUMERATORS_MOD,   ONLY: LEVTYPE_PL_E
  USE :: ENUMERATORS_MOD,   ONLY: LEVTYPE_PT_E
  USE :: ENUMERATORS_MOD,   ONLY: LEVTYPE_PV_E
  USE :: ENUMERATORS_MOD,   ONLY: LEVTYPE_SFC_E
  USE :: ENUMERATORS_MOD,   ONLY: LEVTYPE_SOL_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IPREFIX
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PARAM_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: LEVEL
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: REPRES
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: ILEVTYPE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_LEVTYPE = 1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! NGRBRSN  - 33     - Snow density
  ! NGRBTSN  - 238    - Temperature of snow layer
  ! NGRBWSN  - 228038 - Snow liquid water (multi-layer)
  ! NGRBSD   - 228141 - Snow depth (multi-layer)

  SELECT CASE ( IPREFIX )

  CASE ( PREFIX_MODEL_LEVEL_E )
    SELECT CASE (PARAM_ID)
    CASE ( NGRB100U, NGRB100V )
      ILEVTYPE = LEVTYPE_HL_E
    CASE DEFAULT
      ILEVTYPE = LEVTYPE_ML_E
    END SELECT
  CASE ( PREFIX_PRESSURE_LEVEL_E )
    SELECT CASE (PARAM_ID)
    CASE ( NGRB100U, NGRB100V )
      ILEVTYPE = LEVTYPE_HL_E
    CASE DEFAULT
      ILEVTYPE = LEVTYPE_PL_E
    END SELECT
  CASE ( PREFIX_VORTICITY_LEVEL_E )
    ILEVTYPE = LEVTYPE_PV_E
  CASE ( PREFIX_THETA_LEVEL_E )
    ILEVTYPE = LEVTYPE_PT_E
  CASE ( PREFIX_SURFACE_E )
    SELECT CASE (PARAM_ID)
    CASE ( NGRBRSN, NGRBTSN, NGRBWSN, NGRBSD, 231027 )
      IF ( LEVEL .NE. 0 ) THEN
        ILEVTYPE = LEVTYPE_SOL_E
      ELSE
        ILEVTYPE = LEVTYPE_SFC_E
      END IF
    CASE DEFAULT
      ILEVTYPE = LEVTYPE_SFC_E
    END SELECT
  CASE ( PREFIX_WAVE_INT_E )
    ILEVTYPE = LEVTYPE_SFC_E
  CASE ( PREFIX_WAVE_SPEC_E )
    ILEVTYPE = LEVTYPE_SFC_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_LEVTYPE )
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()
    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNKNOWN_LEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown levtype' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION IPREFIX2ILEVTYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'CLEVTYPE2ILEVTYPE'
FUNCTION CLEVTYPE2ILEVTYPE( CLEVTYPE, ILEVTYPE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_HHL_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_HPL_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_HL_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_ML_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_O2D_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_O3D_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_PL_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_PT_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_PV_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_SFC_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_SOL_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: CLEVTYPE
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: ILEVTYPE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_LEVTYPE = 1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  SELECT CASE ( CLEVTYPE )

  CASE ( 'hhl' )
    ILEVTYPE = LEVTYPE_HHL_E
  CASE ( 'hpl' )
    ILEVTYPE = LEVTYPE_HPL_E
  CASE ( 'hl' )
    ILEVTYPE = LEVTYPE_HL_E
  CASE ( 'ml' )
    ILEVTYPE = LEVTYPE_ML_E
  CASE ( 'o2d' )
    ILEVTYPE = LEVTYPE_O2D_E
  CASE ( 'o3d' )
    ILEVTYPE = LEVTYPE_O3D_E
  CASE ( 'pl' )
    ILEVTYPE = LEVTYPE_PL_E
  CASE ( 'pt' )
    ILEVTYPE = LEVTYPE_PT_E
  CASE ( 'pv' )
    ILEVTYPE = LEVTYPE_PV_E
  CASE ( 'sfc' )
    ILEVTYPE = LEVTYPE_SFC_E
  CASE ( 'sol' )
    ILEVTYPE = LEVTYPE_SOL_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_LEVTYPE )
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()


    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNKNOWN_LEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown levtype' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION CLEVTYPE2ILEVTYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ILEVTYPE2CLEVTYPE'
FUNCTION ILEVTYPE2CLEVTYPE( ILEVTYPE, CLEVTYPE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_HHL_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_HPL_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_HL_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_ML_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_O2D_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_O3D_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_PL_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_PT_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_PV_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_SFC_E
  USE :: ENUMERATORS_MOD, ONLY: LEVTYPE_SOL_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ILEVTYPE
  CHARACTER(LEN=3),     INTENT(OUT)   :: CLEVTYPE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_LEVTYPE = 1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  SELECT CASE ( ILEVTYPE )

  CASE ( LEVTYPE_HHL_E )
    CLEVTYPE = 'hhl'
  CASE ( LEVTYPE_HPL_E )
    CLEVTYPE = 'hpl'
  CASE ( LEVTYPE_HL_E )
    CLEVTYPE = 'hl'
  CASE ( LEVTYPE_ML_E )
    CLEVTYPE = 'ml'
  CASE ( LEVTYPE_O2D_E )
    CLEVTYPE = 'o2d'
  CASE ( LEVTYPE_O3D_E )
    CLEVTYPE = 'o3d'
  CASE ( LEVTYPE_PL_E )
    CLEVTYPE = 'pl'
  CASE ( LEVTYPE_PT_E )
    CLEVTYPE = 'pt'
  CASE ( LEVTYPE_PV_E )
    CLEVTYPE = 'pv'
  CASE ( LEVTYPE_SFC_E )
    CLEVTYPE = 'sfc'
  CASE ( LEVTYPE_SOL_E )
    CLEVTYPE = 'sol'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_LEVTYPE )
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()


    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNKNOWN_LEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown levtype' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION ILEVTYPE2CLEVTYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'IREPRES2CREPRES'
FUNCTION IREPRES2CREPRES( IREPRES, CREPRES, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  USE :: ENUMERATORS_MOD, ONLY: REPRES_LATLONG_E
  USE :: ENUMERATORS_MOD, ONLY: REPRES_GAUSSIANGRID_E
  USE :: ENUMERATORS_MOD, ONLY: REPRES_SPHERICALHARMONICS_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IREPRES
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Function result
  CHARACTER(LEN=20) :: CREPRES

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_LEVTYPE = 1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  SELECT CASE ( IREPRES )
  CASE ( REPRES_LATLONG_E )
    CREPRES = 'latlon'
  CASE ( REPRES_GAUSSIANGRID_E )
    CREPRES = 'gridded'
  CASE ( REPRES_SPHERICALHARMONICS_E )
    CREPRES = 'spherical_harmonics'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_LEVTYPE )
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()


    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNKNOWN_LEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown levtype' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION IREPRES2CREPRES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE IFS_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME