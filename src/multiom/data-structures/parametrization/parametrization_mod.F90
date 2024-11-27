! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'parametrization_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'PARAMETRIZATION_MOD'
MODULE PARAMETRIZATION_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  USE :: GEOMETRY_PAR_MOD,   ONLY: GEOMETRY_PAR_T
  USE :: SATELLITE_PAR_MOD,  ONLY: SATELLITE_PAR_T
  USE :: WAVE_PAR_MOD,       ONLY: WAVE_PAR_T
  USE :: LEVEL_PAR_MOD,      ONLY: LEVEL_PAR_T
  USE :: ENSEMBLE_PAR_MOD,   ONLY: ENSEMBLE_PAR_T
  USE :: SRFLEV_PAR_MOD,     ONLY: SRFLEV_PAR_T

IMPLICIT NONE

!> Default visibility of the module
PRIVATE


  !> Collection of all information I am not able to fit into mars
  TYPE :: PARAMETRIZATION_T

    ! Tables version
    INTEGER(KIND=JPIB_K) :: TABLES_VERSION=UNDEF_PARAM_E

    ! Scalar parameters
    INTEGER(KIND=JPIB_K) :: INITIAL_STEP=UNDEF_PARAM_E
    INTEGER(KIND=JPIB_K) :: LENGTH_OF_TIME_STEP_IN_SECONDS=UNDEF_PARAM_E
    INTEGER(KIND=JPIB_K) :: LENGTH_OF_TIME_WINDOW_IN_ANALYSIS=UNDEF_PARAM_E
    INTEGER(KIND=JPIB_K) :: LENGTH_OF_TIME_RANGE_IN_SECONDS=UNDEF_PARAM_E
    INTEGER(KIND=JPIB_K) :: BITS_PER_VALUE=UNDEF_PARAM_E

    !> Scale factors for the values (used to change units)
    REAL(KIND=JPRD_K)    :: VALUES_SCALE_FACTOR=0.0_JPRD_K


    ! Configorations to be descussed and eventually to be integrated in the MARS keywords
    TYPE(GEOMETRY_PAR_T)  :: GEOMETRY
    TYPE(LEVEL_PAR_T)     :: LEVELS
    TYPE(ENSEMBLE_PAR_T)  :: ENSEMBLE
    TYPE(WAVE_PAR_T)      :: WAVE
    TYPE(SATELLITE_PAR_T) :: SATELLITE
    TYPE(SRFLEV_PAR_T)    :: SRFLEV

  CONTAINS

    !> Copy from another object
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE           => PARAMETRIZATION_FREE
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: COPY_FROM      => PARAMETRIZATION_COPY_FROM
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: READ_FROM_YAML => READ_PARAMETRIZATION_FROM_YAML

    !> Set fields by field ID
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT    => PARAMETRIZATION_SET_INT
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_STRING => PARAMETRIZATION_SET_STRING
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_FLOAT  => PARAMETRIZATION_SET_FLOAT
    GENERIC :: SET => SET_INT
    GENERIC :: SET => SET_STRING
    GENERIC :: SET => SET_FLOAT

    !> Set fields by field ID
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_INT    => PARAMETRIZATION_GET_INT
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_STRING => PARAMETRIZATION_GET_STRING
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_FLOAT  => PARAMETRIZATION_GET_FLOAT
    GENERIC :: GET => GET_INT
    GENERIC :: GET => GET_STRING
    GENERIC :: GET => GET_FLOAT

    !> print
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT  => PARAMETRIZATION_PRINT
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: TO_JSON => PARAMETRIZATION_TO_JSON

  END TYPE

  !> Whitelist of public symbols (types)
  PUBLIC :: PARAMETRIZATION_T

CONTAINS



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_FREE'
FUNCTION PARAMETRIZATION_FREE( THIS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(PARAMETRIZATION_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_FREE_GEOMETRY=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_FREE_LEVELS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_FREE_ENSEMBLE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_FREE_WAVE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_FREE_SATELLITE=5_JPIB_K

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

  ! Tables version
  THIS%TABLES_VERSION=UNDEF_PARAM_E
  THIS%INITIAL_STEP=UNDEF_PARAM_E
  THIS%LENGTH_OF_TIME_STEP_IN_SECONDS=UNDEF_PARAM_E
  THIS%LENGTH_OF_TIME_WINDOW_IN_ANALYSIS=UNDEF_PARAM_E
  THIS%LENGTH_OF_TIME_RANGE_IN_SECONDS=UNDEF_PARAM_E
  THIS%BITS_PER_VALUE=UNDEF_PARAM_E
  THIS%VALUES_SCALE_FACTOR=0.0_JPRD_K

  ! Free the geometry
  PP_TRYCALL(ERR_FLAG_FREE_GEOMETRY) THIS%GEOMETRY%FREE( HOOKS )

  ! Free the levels
  PP_TRYCALL(ERR_FLAG_FREE_LEVELS) THIS%LEVELS%FREE( HOOKS )

  ! Free the ensemble
  PP_TRYCALL(ERR_FLAG_FREE_ENSEMBLE) THIS%ENSEMBLE%FREE( HOOKS )

  ! Free the wave
  PP_TRYCALL(ERR_FLAG_FREE_WAVE) THIS%WAVE%FREE( HOOKS )

  ! Free the satellite
  PP_TRYCALL(ERR_FLAG_FREE_SATELLITE) THIS%SATELLITE%FREE( HOOKS )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE(ERR_FLAG_FREE_GEOMETRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free geometry' )
    CASE(ERR_FLAG_FREE_LEVELS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free levels' )
    CASE(ERR_FLAG_FREE_ENSEMBLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free ensemble' )
    CASE(ERR_FLAG_FREE_WAVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free wave' )
    CASE(ERR_FLAG_FREE_SATELLITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free satellite' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION PARAMETRIZATION_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_COPY_FROM'
FUNCTION PARAMETRIZATION_COPY_FROM( THIS, OTHER, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(PARAMETRIZATION_T), INTENT(INOUT) :: THIS
  CLASS(PARAMETRIZATION_T), INTENT(IN)    :: OTHER
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_COPY_LEVELS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_COPY_ENSEMBLE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_COPY_WAVE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_COPY_SATELLITE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_COPY_GEOMETRY=5_JPIB_K

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

  ! Copy the scale factors
  THIS%VALUES_SCALE_FACTOR = OTHER%VALUES_SCALE_FACTOR

  ! Copy the geometry
  THIS%TABLES_VERSION = OTHER%TABLES_VERSION
  THIS%INITIAL_STEP = OTHER%INITIAL_STEP
  THIS%LENGTH_OF_TIME_STEP_IN_SECONDS = OTHER%LENGTH_OF_TIME_STEP_IN_SECONDS
  THIS%LENGTH_OF_TIME_WINDOW_IN_ANALYSIS = OTHER%LENGTH_OF_TIME_WINDOW_IN_ANALYSIS
  THIS%LENGTH_OF_TIME_RANGE_IN_SECONDS = OTHER%LENGTH_OF_TIME_RANGE_IN_SECONDS
  THIS%BITS_PER_VALUE = OTHER%BITS_PER_VALUE
  THIS%VALUES_SCALE_FACTOR = OTHER%VALUES_SCALE_FACTOR

  ! Copy the levels
  PP_TRYCALL(ERR_FLAG_COPY_LEVELS) THIS%LEVELS%COPY_FROM( OTHER%LEVELS, HOOKS )

  ! Copy the ensemble
  PP_TRYCALL(ERR_FLAG_COPY_ENSEMBLE) THIS%ENSEMBLE%COPY_FROM( OTHER%ENSEMBLE, HOOKS )

  ! Copy the wave
  PP_TRYCALL(ERR_FLAG_COPY_WAVE) THIS%WAVE%COPY_FROM( OTHER%WAVE, HOOKS )

  ! Copy the satellite
  PP_TRYCALL(ERR_FLAG_COPY_SATELLITE) THIS%SATELLITE%COPY_FROM( OTHER%SATELLITE, HOOKS )

  ! Copy the geometry
  PP_TRYCALL(ERR_FLAG_COPY_GEOMETRY) THIS%GEOMETRY%COPY_FROM( OTHER%GEOMETRY, HOOKS )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE(ERR_FLAG_COPY_LEVELS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to copy levels' )
    CASE(ERR_FLAG_COPY_ENSEMBLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to copy ensemble' )
    CASE(ERR_FLAG_COPY_WAVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to copy wave' )
    CASE(ERR_FLAG_COPY_SATELLITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to copy satellite' )
    CASE(ERR_FLAG_COPY_GEOMETRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to copy geometry' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION PARAMETRIZATION_COPY_FROM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_PRINT'
FUNCTION PARAMETRIZATION_PRINT( THIS, UNIT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPRD_K
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD,        ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: N_PARINTFLDS
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: IPARINTFLDS2CPARINTFLDS
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: N_PARSTRFLDS
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: IPARSTRINGFLDS2CPARSTRINGFLDS
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: N_PARFLOATFLDS
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: IPARFLOATFLDS2CPARFLOATFLDS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(PARAMETRIZATION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local parameters
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: ITMP
  REAL(KIND=JPRD_K)    :: RTMP
  CHARACTER(LEN=16)    :: CKEY
  CHARACTER(LEN=8)     :: CTMP
  INTEGER(KIND=JPIB_K) :: WRITE_STAT

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IPARINTFLDS2CPARINTFLDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_INT=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IPARSTRINGFLDS2CPARSTRINGFLDS=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_STRING=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IPARFLOATFLDS2CPARFLOATFLDS=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_FLOAT=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IOSTATUS_NOT_ZERO=7_JPIB_K

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

  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) '** PARAMETRIZATION PRINT'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! Integer members
  IF ( N_PARINTFLDS .GT. 0 ) THEN
    WRITE(UNIT,*) '+ Integer members'
    DO I = 1, N_PARINTFLDS
      PP_TRYCALL(ERRFLAG_IPARINTFLDS2CPARINTFLDS) IPARINTFLDS2CPARINTFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_INT) THIS%GET_INT( I, ITMP, HOOKS )
      WRITE(UNIT,'(A3,A20,A3,I32)',IOSTAT=WRITE_STAT) ' - ', TRIM(ADJUSTL(CKEY)) ,' : ', ITMP
      PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )
    ENDDO
  ENDIF

  ! String members
  IF ( N_PARSTRFLDS .GT. 0 ) THEN
    WRITE(UNIT,*) '+ String members'
    DO I = 1, N_PARSTRFLDS
      PP_TRYCALL(ERRFLAG_IPARSTRINGFLDS2CPARSTRINGFLDS) IPARSTRINGFLDS2CPARSTRINGFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_STRING) THIS%GET_STRING( I, CTMP, HOOKS )
      WRITE(UNIT,'(A3,A20,A3,A8)',IOSTAT=WRITE_STAT) ' - ', TRIM(ADJUSTL(CKEY)) ,' : ', CTMP
      PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )
    ENDDO
  ENDIF

  ! Float members
  IF ( N_PARFLOATFLDS .GT. 0 ) THEN
    WRITE(UNIT,*) '+ Float members'
    DO I = 1, N_PARFLOATFLDS
      PP_TRYCALL(ERRFLAG_IPARFLOATFLDS2CPARFLOATFLDS) IPARFLOATFLDS2CPARFLOATFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_FLOAT) THIS%GET_FLOAT( I, RTMP, HOOKS )
      WRITE(UNIT,'(A3,A20,A3,F11.4)',IOSTAT=WRITE_STAT) ' - ', TRIM(ADJUSTL(CKEY)) ,' : ', RTMP
      PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )
    ENDDO
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE(ERRFLAG_IPARINTFLDS2CPARINTFLDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert integer field ID to string' )
    CASE(ERRFLAG_GET_INT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get integer field' )
    CASE(ERRFLAG_IPARSTRINGFLDS2CPARSTRINGFLDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert string field ID to string' )
    CASE(ERRFLAG_GET_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get string field' )
    CASE(ERRFLAG_IOSTATUS_NOT_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'IO status is not zero' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION PARAMETRIZATION_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_TO_JSON'
FUNCTION PARAMETRIZATION_TO_JSON( THIS, JSON, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPRD_K
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD,        ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: N_PARINTFLDS
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: IPARINTFLDS2CPARINTFLDS
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: N_PARSTRFLDS
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: IPARSTRINGFLDS2CPARSTRINGFLDS
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: N_PARFLOATFLDS
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: IPARFLOATFLDS2CPARFLOATFLDS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(PARAMETRIZATION_T),      INTENT(INOUT) :: THIS
  CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: JSON
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local parameters
  INTEGER(KIND=JPIB_K) :: L
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: SZ
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: N_FIELDS
  INTEGER(KIND=JPIB_K) :: ITMP
  REAL(KIND=JPRD_K)    :: RTMP
  CHARACTER(LEN=16)    :: CKEY
  CHARACTER(LEN=32)    :: CTMP
  CHARACTER(LEN=1024)  :: JSON_ITEM
  CHARACTER(LEN=1)     :: SEP
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  INTEGER(KIND=JPIB_K) :: ALLOC_STATE
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IPARINTFLDS2CPARINTFLDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_INT=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IPARSTRINGFLDS2CPARSTRINGFLDS=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_STRING=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IPARFLOATFLDS2CPARFLOATFLDS=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_FLOAT=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOC_ERROR=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOC_ERROR=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IOSTATUS_NOT_ZERO=9_JPIB_K

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

  CNT = 0
  N_FIELDS = N_PARINTFLDS + N_PARSTRFLDS + N_PARFLOATFLDS

  ! Count the number of characters needed for the JSON string
  SZ = 17
  LO = 1
  HI = LO + SZ - 1
  IF ( N_PARINTFLDS .GT. 0 ) THEN
    DO I = 1, N_PARINTFLDS

      CTMP=REPEAT(' ',32)
      CKEY=REPEAT(' ',16)
      PP_TRYCALL(ERRFLAG_IPARINTFLDS2CPARINTFLDS) IPARINTFLDS2CPARINTFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_INT) THIS%GET_INT( I, ITMP, HOOKS )
      WRITE(CTMP,*,IOSTAT=WRITE_STAT) ITMP
      SZ = 1 + LEN_TRIM(ADJUSTL(CKEY)) + 1 + LEN_TRIM(ADJUSTL(CTMP)) + 1
      LO = HI + 1
      HI = LO + SZ - 1
    ENDDO
  ENDIF

  ! String members
  IF ( N_PARSTRFLDS .GT. 0 ) THEN
    DO I = 1, N_PARSTRFLDS
      CTMP=REPEAT(' ',32)
      CKEY=REPEAT(' ',16)
      PP_TRYCALL(ERRFLAG_IPARSTRINGFLDS2CPARSTRINGFLDS) IPARSTRINGFLDS2CPARSTRINGFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_STRING) THIS%GET_STRING( I, CTMP, HOOKS )
      SZ = 1 + LEN_TRIM(ADJUSTL(CKEY)) + 1 + LEN_TRIM(ADJUSTL(CTMP)) + 1
      LO = HI + 1
      HI = LO + SZ - 1
    ENDDO
  ENDIF

  ! Float members
  IF ( N_PARFLOATFLDS .GT. 0 ) THEN
    DO I = 1, N_PARFLOATFLDS
      CTMP=REPEAT(' ',32)
      CKEY=REPEAT(' ',16)
      PP_TRYCALL(ERRFLAG_IPARFLOATFLDS2CPARFLOATFLDS) IPARFLOATFLDS2CPARFLOATFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_FLOAT) THIS%GET_FLOAT( I, RTMP, HOOKS )
      WRITE(CTMP,*,IOSTAT=WRITE_STAT) RTMP
      SZ = 1 + LEN_TRIM(ADJUSTL(CKEY)) + 1 + LEN_TRIM(ADJUSTL(CTMP)) + 1
      LO = HI + 1
      HI = LO + SZ - 1
    ENDDO
  ENDIF

  ! Close the JSON object
  ! Size equal to one for the closing bracket no null character is needed
  ! since it is alreday include in the string
  SZ = 1
  LO = HI + 1
  HI = LO + SZ - 1

  ! Free the json string
  IF ( ALLOCATED(JSON) ) THEN
    DEALLOCATE(JSON, STAT=DEALLOC_STATE, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATE .NE. 0, ERRFLAG_DEALLOC_ERROR )
  ENDIF

  ! Allocate the JSON string
  L = HI
  ALLOCATE(CHARACTER(LEN=L) :: JSON, STAT=ALLOC_STATE, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATE .NE. 0, ERRFLAG_ALLOC_ERROR )

  ! Fill the JSON string
  JSON = REPEAT(' ', L)
  SZ = 17
  LO = 1
  HI = LO + SZ - 1
  JSON(LO:HI) = 'parametrization={'
  IF ( N_PARINTFLDS .GT. 0 ) THEN
    DO I = 1, N_PARINTFLDS
      CNT = CNT + 1
      IF ( CNT .GE. N_FIELDS ) THEN
        SEP = ' '
      ELSE
        SEP = ','
      ENDIF
      CTMP=REPEAT(' ',32)
      CKEY=REPEAT(' ',16)
      PP_TRYCALL(ERRFLAG_IPARINTFLDS2CPARINTFLDS) IPARINTFLDS2CPARINTFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_INT) THIS%GET_INT( I, ITMP, HOOKS )
      WRITE(CTMP,*,IOSTAT=WRITE_STAT) ITMP
      SZ = 1 + LEN_TRIM(ADJUSTL(CKEY)) + 1 + LEN_TRIM(ADJUSTL(CTMP)) + 1
      LO = HI + 1
      HI = LO + SZ - 1
      JSON_ITEM = REPEAT(' ', 1024)
      WRITE(JSON_ITEM, '(A1,A,A1,A,A1)') ' ', TRIM(ADJUSTL(CKEY)) ,':', TRIM(ADJUSTL(CTMP)), SEP
      JSON(LO:HI) = TRIM(JSON_ITEM)
    ENDDO
  ENDIF

  ! String members
  IF ( N_PARSTRFLDS .GT. 0 ) THEN
    DO I = 1, N_PARSTRFLDS
      CNT = CNT + 1
      IF ( CNT .GE. N_FIELDS ) THEN
        SEP = ' '
      ELSE
        SEP = ','
      ENDIF
      CTMP=REPEAT(' ',32)
      CKEY=REPEAT(' ',16)
      PP_TRYCALL(ERRFLAG_IPARSTRINGFLDS2CPARSTRINGFLDS) IPARSTRINGFLDS2CPARSTRINGFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_STRING) THIS%GET_STRING( I, CTMP, HOOKS )
      SZ = 1 + LEN_TRIM(ADJUSTL(CKEY)) + 1 + LEN_TRIM(ADJUSTL(CTMP)) + 1
      LO = HI + 1
      HI = LO + SZ - 1
      JSON_ITEM = REPEAT(' ', 1024)
      WRITE(JSON_ITEM, '(A1,A,A1,A,A1)') ' ', TRIM(ADJUSTL(CKEY)) ,':', TRIM(ADJUSTL(CTMP)), SEP
      JSON(LO:HI) = TRIM(JSON_ITEM)
    ENDDO
  ENDIF

  ! Float members
  IF ( N_PARFLOATFLDS .GT. 0 ) THEN
    DO I = 1, N_PARFLOATFLDS
      CNT = CNT + 1
      IF ( CNT .GE. N_FIELDS ) THEN
        SEP = ' '
      ELSE
        SEP = ','
      ENDIF
      CTMP=REPEAT(' ',32)
      CKEY=REPEAT(' ',16)
      PP_TRYCALL(ERRFLAG_IPARFLOATFLDS2CPARFLOATFLDS) IPARFLOATFLDS2CPARFLOATFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_FLOAT) THIS%GET_FLOAT( I, RTMP, HOOKS )
      WRITE(CTMP,*,IOSTAT=WRITE_STAT) RTMP
      SZ = 1 + LEN_TRIM(ADJUSTL(CKEY)) + 1 + LEN_TRIM(ADJUSTL(CTMP)) + 1
      LO = HI + 1
      HI = LO + SZ - 1
      JSON_ITEM = REPEAT(' ', 1024)
      WRITE(JSON_ITEM, '(A1,A,A1,A,A1)') ' ', TRIM(ADJUSTL(CKEY)) ,':', TRIM(ADJUSTL(CTMP)), SEP
      JSON(LO:HI) = TRIM(JSON_ITEM)
    ENDDO
  ENDIF

  ! Close the JSON object
  LO = LEN_TRIM(JSON) + 1
  HI = LO + 1
  JSON(LO:HI) = ' }'

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE(ERRFLAG_IPARINTFLDS2CPARINTFLDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert integer field ID to string' )
    CASE(ERRFLAG_GET_INT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get integer field' )
    CASE(ERRFLAG_IPARSTRINGFLDS2CPARSTRINGFLDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert string field ID to string' )
    CASE(ERRFLAG_GET_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get string field' )
    CASE(ERRFLAG_IOSTATUS_NOT_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'IO status is not zero' )
    CASE(ERRFLAG_ALLOC_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Allocation error' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//ERRMSG )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATE )
      ENDIF
    CASE(ERRFLAG_DEALLOC_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Deallocation error' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//ERRMSG )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATE )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION PARAMETRIZATION_TO_JSON
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_SET_INT'
FUNCTION PARAMETRIZATION_SET_INT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: PARINTFLD_TABLES_VERSION_E
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: PARINTFLD_INITIAL_STEP_E
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: PARINTFLD_LENGTH_OF_TIME_STEP_IN_SECONDS_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(PARAMETRIZATION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PAR=1_JPIB_K

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

  ! Set the value based on the ID
  SELECT CASE ( ID )
  CASE ( PARINTFLD_TABLES_VERSION_E )
    THIS%TABLES_VERSION = VALUE
  CASE ( PARINTFLD_INITIAL_STEP_E )
    THIS%INITIAL_STEP = VALUE
  CASE ( PARINTFLD_LENGTH_OF_TIME_STEP_IN_SECONDS_E )
    THIS%LENGTH_OF_TIME_STEP_IN_SECONDS = VALUE
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PAR )
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE (ERRFLAG_UNKNOWN_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown parameter' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION PARAMETRIZATION_SET_INT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_SET_STRING'
FUNCTION PARAMETRIZATION_SET_STRING( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(PARAMETRIZATION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  CHARACTER(LEN=*),         INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PAR=1_JPIB_K

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

  ! Set the value based on the ID
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PAR )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE (ERRFLAG_UNKNOWN_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown parameter' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION PARAMETRIZATION_SET_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_SET_FLOAT'
FUNCTION PARAMETRIZATION_SET_FLOAT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: PARFLOATFLD_SCALEFACTOR_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(PARAMETRIZATION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  REAL(KIND=JPRD_K),        INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PAR=1_JPIB_K

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

  ! Set the value based on the ID
  SELECT CASE ( ID )
  CASE ( PARFLOATFLD_SCALEFACTOR_E )
    THIS%VALUES_SCALE_FACTOR = VALUE
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PAR )
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE (ERRFLAG_UNKNOWN_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown parameter' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION PARAMETRIZATION_SET_FLOAT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_GET_INT'
FUNCTION PARAMETRIZATION_GET_INT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: PARINTFLD_TABLES_VERSION_E
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: PARINTFLD_INITIAL_STEP_E
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: PARINTFLD_LENGTH_OF_TIME_STEP_IN_SECONDS_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(PARAMETRIZATION_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  INTEGER(KIND=JPIB_K),     INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PAR=1_JPIB_K

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

  ! Get the value based on the ID
  SELECT CASE ( ID )
  CASE ( PARINTFLD_TABLES_VERSION_E )
    VALUE = THIS%TABLES_VERSION
  CASE ( PARINTFLD_INITIAL_STEP_E )
    VALUE = THIS%INITIAL_STEP
  CASE ( PARINTFLD_LENGTH_OF_TIME_STEP_IN_SECONDS_E )
    VALUE = THIS%LENGTH_OF_TIME_STEP_IN_SECONDS
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PAR )
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE (ERRFLAG_UNKNOWN_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown parameter' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION PARAMETRIZATION_GET_INT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_GET_STRING'
FUNCTION PARAMETRIZATION_GET_STRING( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(PARAMETRIZATION_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  CHARACTER(LEN=4),         INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PAR=1_JPIB_K

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

  VALUE=REPEAT('*',4)

  PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PAR )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE (ERRFLAG_UNKNOWN_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown parameter' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION PARAMETRIZATION_GET_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARAMETRIZATION_GET_FLOAT'
FUNCTION PARAMETRIZATION_GET_FLOAT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: PARFLOATFLD_SCALEFACTOR_E


  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(PARAMETRIZATION_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  REAL(KIND=JPRD_K),        INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PAR=1_JPIB_K

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

  SELECT CASE ( ID )
  CASE ( PARFLOATFLD_SCALEFACTOR_E )
    VALUE = THIS%VALUES_SCALE_FACTOR
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PAR )
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE (ERRFLAG_UNKNOWN_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown parameter' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION PARAMETRIZATION_GET_FLOAT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_PARAMETRIZATION_FROM_YAML'
FUNCTION READ_PARAMETRIZATION_FROM_YAML( PARAMETRIZATION, CONFIG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPRD_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_FLOAT
  USE :: ENUMERATORS_MOD,     ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(PARAMETRIZATION_T),   INTENT(INOUT) :: PARAMETRIZATION
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CONFIG
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(YAML_CONFIGURATION_T) :: PARAMETRIZATION_CONFIGURATION
  LOGICAL :: HAS_PARAMETRIZATION
  LOGICAL :: HAS_KEY

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_GEOMETRY=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_LEVELS=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_ENSEMBLE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_WAVE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SATELLITE=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DELETE_CONFIGURATION=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SRFLEV=9_JPIB_K

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

  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CONFIG, 'parametrization', HAS_PARAMETRIZATION, HOOKS )


  !> Read representations
  IF ( HAS_PARAMETRIZATION  ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CONFIG, 'parametrization', PARAMETRIZATION_CONFIGURATION, HOOKS )

    !> Read the parametrization
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( PARAMETRIZATION_CONFIGURATION, 'tables-version', HAS_KEY, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_KEY, ERRFLAG_UNABLE_TO_READ_CFG )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( PARAMETRIZATION_CONFIGURATION, 'tables-version', PARAMETRIZATION%TABLES_VERSION, HOOKS )

    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( PARAMETRIZATION_CONFIGURATION, 'length-of-timestep-in-seconds', HAS_KEY, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_KEY, ERRFLAG_UNABLE_TO_READ_CFG )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( PARAMETRIZATION_CONFIGURATION, 'length-of-timestep-in-seconds', PARAMETRIZATION%LENGTH_OF_TIME_STEP_IN_SECONDS, HOOKS )

    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( PARAMETRIZATION_CONFIGURATION, 'initial-step', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( PARAMETRIZATION_CONFIGURATION, 'initial-step', PARAMETRIZATION%INITIAL_STEP, HOOKS )
    ELSE
      PARAMETRIZATION%INITIAL_STEP = UNDEF_PARAM_E
    ENDIF

    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( PARAMETRIZATION_CONFIGURATION, 'length-of-window-in-analysis', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( PARAMETRIZATION_CONFIGURATION, 'length-of-window-in-analysis', PARAMETRIZATION%LENGTH_OF_TIME_WINDOW_IN_ANALYSIS, HOOKS )
    ELSE
      PARAMETRIZATION%LENGTH_OF_TIME_WINDOW_IN_ANALYSIS = UNDEF_PARAM_E
    ENDIF

    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( PARAMETRIZATION_CONFIGURATION, 'overall-length-of-timerange', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( PARAMETRIZATION_CONFIGURATION, 'length-of-window-in-analysis', PARAMETRIZATION%LENGTH_OF_TIME_RANGE_IN_SECONDS, HOOKS )
    ELSE
      PARAMETRIZATION%LENGTH_OF_TIME_RANGE_IN_SECONDS = UNDEF_PARAM_E
    ENDIF

    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( PARAMETRIZATION_CONFIGURATION, 'bits-per-value', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( PARAMETRIZATION_CONFIGURATION, 'bits-per-value', PARAMETRIZATION%BITS_PER_VALUE, HOOKS )
    ELSE
      PARAMETRIZATION%BITS_PER_VALUE = 16_JPIB_K
    ENDIF

    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( PARAMETRIZATION_CONFIGURATION, 'values-scale-factor', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_FLOAT( PARAMETRIZATION_CONFIGURATION, 'values-scale-factor', PARAMETRIZATION%VALUES_SCALE_FACTOR, HOOKS )
    ELSE
      PARAMETRIZATION%VALUES_SCALE_FACTOR = 1.0_JPRD_K
    ENDIF

    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_GEOMETRY)  PARAMETRIZATION%GEOMETRY%READ_FROM_YAML( PARAMETRIZATION_CONFIGURATION, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_LEVELS)    PARAMETRIZATION%LEVELS%READ_FROM_YAML( PARAMETRIZATION_CONFIGURATION, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_ENSEMBLE)  PARAMETRIZATION%ENSEMBLE%READ_FROM_YAML( PARAMETRIZATION_CONFIGURATION, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_WAVE)      PARAMETRIZATION%WAVE%READ_FROM_YAML( PARAMETRIZATION_CONFIGURATION, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SATELLITE) PARAMETRIZATION%SATELLITE%READ_FROM_YAML( PARAMETRIZATION_CONFIGURATION, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SRFLEV) PARAMETRIZATION%SRFLEV%READ_FROM_YAML( PARAMETRIZATION_CONFIGURATION, HOOKS )

    !> Destroy the configuration object
    PP_TRYCALL(ERRFLAG_UNABLE_TO_DELETE_CONFIGURATION) YAML_DELETE_CONFIGURATION( PARAMETRIZATION_CONFIGURATION, HOOKS )

  ENDIF


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE(ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read parametrization configuration' )
    CASE(ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read subconfigurations' )
    CASE(ERRFLAG_UNABLE_TO_READ_GEOMETRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read geometry' )
    CASE(ERRFLAG_UNABLE_TO_READ_LEVELS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read levels' )
    CASE(ERRFLAG_UNABLE_TO_READ_ENSEMBLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read ensemble' )
    CASE(ERRFLAG_UNABLE_TO_READ_WAVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read wave' )
    CASE(ERRFLAG_UNABLE_TO_READ_SATELLITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read satellite' )
    CASE(ERRFLAG_UNABLE_TO_READ_SRFLEV)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read srflev' )
    CASE(ERRFLAG_UNABLE_TO_DELETE_CONFIGURATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to delete configuration' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION READ_PARAMETRIZATION_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE PARAMETRIZATION_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
