! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'fortran_message_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'FORTRAN_MESSAGE_MOD'
MODULE FORTRAN_MESSAGE_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

IMPLICIT NONE

!> Default visibiliity of the module
PRIVATE

!>
TYPE :: FORTRAN_MESSAGE_T

  ! Default visibility of the type
  ! PRIVATE

  !> General information
  INTEGER(KIND=JPIB_K) :: STREAM = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: TYPE = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: CLASS = UNDEF_PARAM_E
  CHARACTER(LEN=4)     :: EXPVER = REPEAT('*',4)
  INTEGER(KIND=JPIB_K) :: ORIGIN = UNDEF_PARAM_E  ! Centre
  INTEGER(KIND=JPIB_K) :: ANOFFSET = UNDEF_PARAM_E

  !> Packing information
  INTEGER(KIND=JPIB_K) :: PACKING = UNDEF_PARAM_E  ! parturbation number

  !> Ensemble information
  INTEGER(KIND=JPIB_K) :: NUMBER = UNDEF_PARAM_E  ! parturbation number

  !> Satellite information
  INTEGER(KIND=JPIB_K) :: IDENT = UNDEF_PARAM_E ! satellite identifier
  INTEGER(KIND=JPIB_K) :: INSTRUMENT = UNDEF_PARAM_E ! instrument identifier
  INTEGER(KIND=JPIB_K) :: CHANNEL = UNDEF_PARAM_E ! satellite channel number

  !> Field information
  INTEGER(KIND=JPIB_K) :: PARAM_TYPE = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: CHEM = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: PARAM = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: LEVTYPE = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: LEVELIST = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: DIRECTION = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: FREQUENCY = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: MODEL = UNDEF_PARAM_E !! Deprecated

  INTEGER(KIND=JPIB_K) :: DATE = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: TIME = UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: STEP = UNDEF_PARAM_E

  !> Grid information
  INTEGER(KIND=JPIB_K) :: REPRES = UNDEF_PARAM_E
  CHARACTER(LEN=8)     :: GRID = REPEAT('*',8)

CONTAINS

  !> Comparison operators
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT           => FORTRAN_MESSAGE_INIT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: COPY_FROM      => FORTRAN_MESSAGE_COPY_DATA_FROM
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SWAP_DATA      => FORTRAN_MESSAGE_SWAP_DATA
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: READ_FROM_YAML => FORTRAN_MESSAGE_READ_FROM_YAML
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: IS_EQUAL_TO    => FORTRAN_MESSAGE_EQUAL_TO
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: IS_LOWER_THAN  => FORTRAN_MESSAGE_LOWER_THAN
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE           => FORTRAN_MESSAGE_FREE

  !> Set fields by field ID/Key
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_ENUM_INT    => FORTRAN_MESSAGE_SET_ENUM_INT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_KEY_INT     => FORTRAN_MESSAGE_SET_KEY_INT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_ENUM_STRING => FORTRAN_MESSAGE_SET_ENUM_STRING
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_KEY_STRING  => FORTRAN_MESSAGE_SET_KEY_STRING
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_ENUM_FLOAT  => FORTRAN_MESSAGE_SET_ENUM_FLOAT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_KEY_FLOAT   => FORTRAN_MESSAGE_SET_KEY_FLOAT
  GENERIC :: SET => SET_ENUM_INT
  GENERIC :: SET => SET_KEY_INT
  GENERIC :: SET => SET_ENUM_STRING
  GENERIC :: SET => SET_KEY_STRING
  GENERIC :: SET => SET_ENUM_FLOAT
  GENERIC :: SET => SET_KEY_FLOAT

  !> Get fields by field ID/Key
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_ENUM_INT    => FORTRAN_MESSAGE_GET_ENUM_INT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_KEY_INT     => FORTRAN_MESSAGE_GET_KEY_INT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_ENUM_STRING => FORTRAN_MESSAGE_GET_ENUM_STRING
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_KEY_STRING  => FORTRAN_MESSAGE_GET_KEY_STRING
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_ENUM_FLOAT  => FORTRAN_MESSAGE_GET_ENUM_FLOAT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_KEY_FLOAT   => FORTRAN_MESSAGE_GET_KEY_FLOAT
  GENERIC :: GET => GET_ENUM_INT
  GENERIC :: GET => GET_KEY_INT
  GENERIC :: GET => GET_ENUM_STRING
  GENERIC :: GET => GET_KEY_STRING
  GENERIC :: GET => GET_ENUM_FLOAT
  GENERIC :: GET => GET_KEY_FLOAT



  !> Has fields by field ID/Key
  !! PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: HAS_ENUM_INT    => FORTRAN_MESSAGE_HAS_ENUM_INT
  !! PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: HAS_KEY_INT     => FORTRAN_MESSAGE_HAS_KEY_INT
  !! PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: HAS_ENUM_STRING => FORTRAN_MESSAGE_HAS_ENUM_STRING
  !! PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: HAS_KEY_STRING  => FORTRAN_MESSAGE_HAS_KEY_STRING
  !! PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: HAS_ENUM_FLOAT  => FORTRAN_MESSAGE_HAS_ENUM_FLOAT
  !! PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: HAS_KEY_FLOAT   => FORTRAN_MESSAGE_HAS_KEY_FLOAT
  !! GENERIC :: HAS => HAS_ENUM_INT
  !! GENERIC :: HAS => HAS_KEY_INT
  !! GENERIC :: HAS => HAS_ENUM_STRING
  !! GENERIC :: HAS => HAS_KEY_STRING
  !! GENERIC :: HAS => HAS_ENUM_FLOAT
  !! GENERIC :: HAS => HAS_KEY_FLOAT

  !> Set fields by field ID
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT => FORTRAN_MESSAGE_PRINT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: TO_JSON => FORTRAN_MESSAGE_TO_JSON

END TYPE

!> Whitelist of public symbols (types)
PUBLIC :: FORTRAN_MESSAGE_T

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_INIT'
FUNCTION FORTRAN_MESSAGE_INIT( THIS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T),     INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET


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

  !> Reset to default values
  THIS%STREAM     = UNDEF_PARAM_E
  THIS%TYPE       = UNDEF_PARAM_E
  THIS%CLASS      = UNDEF_PARAM_E
  THIS%EXPVER     = REPEAT('*',4)
  THIS%ORIGIN     = UNDEF_PARAM_E
  THIS%ANOFFSET   = UNDEF_PARAM_E
  THIS%NUMBER     = UNDEF_PARAM_E
  THIS%IDENT      = UNDEF_PARAM_E
  THIS%INSTRUMENT = UNDEF_PARAM_E
  THIS%CHANNEL    = UNDEF_PARAM_E
  THIS%PARAM_TYPE = UNDEF_PARAM_E
  THIS%CHEM       = UNDEF_PARAM_E
  THIS%PARAM      = UNDEF_PARAM_E
  THIS%LEVTYPE    = UNDEF_PARAM_E
  THIS%LEVELIST   = UNDEF_PARAM_E
  THIS%DIRECTION  = UNDEF_PARAM_E
  THIS%FREQUENCY  = UNDEF_PARAM_E
  THIS%MODEL      = UNDEF_PARAM_E
  THIS%REPRES     = UNDEF_PARAM_E
  THIS%DATE       = UNDEF_PARAM_E
  THIS%TIME       = UNDEF_PARAM_E
  THIS%STEP       = UNDEF_PARAM_E
  THIS%GRID       = REPEAT('*',8)


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
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_FREE'
FUNCTION FORTRAN_MESSAGE_FREE( THIS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T),     INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET


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

  !> Reset to default values
  THIS%STREAM     = UNDEF_PARAM_E
  THIS%TYPE       = UNDEF_PARAM_E
  THIS%CLASS      = UNDEF_PARAM_E
  THIS%EXPVER     = REPEAT('*',4)
  THIS%ORIGIN     = UNDEF_PARAM_E
  THIS%ANOFFSET   = UNDEF_PARAM_E
  THIS%NUMBER     = UNDEF_PARAM_E
  THIS%IDENT      = UNDEF_PARAM_E
  THIS%INSTRUMENT = UNDEF_PARAM_E
  THIS%CHANNEL    = UNDEF_PARAM_E
  THIS%PARAM_TYPE = UNDEF_PARAM_E
  THIS%CHEM       = UNDEF_PARAM_E
  THIS%PARAM      = UNDEF_PARAM_E
  THIS%LEVTYPE    = UNDEF_PARAM_E
  THIS%LEVELIST   = UNDEF_PARAM_E
  THIS%DIRECTION  = UNDEF_PARAM_E
  THIS%FREQUENCY  = UNDEF_PARAM_E
  THIS%MODEL      = UNDEF_PARAM_E
  THIS%REPRES     = UNDEF_PARAM_E
  THIS%DATE       = UNDEF_PARAM_E
  THIS%TIME       = UNDEF_PARAM_E
  THIS%STEP       = UNDEF_PARAM_E
  THIS%GRID       = REPEAT('*',8)


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
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_PRINT'
FUNCTION FORTRAN_MESSAGE_PRINT( THIS, UNIT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPRD_K
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD,        ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: ENUMERATORS_MOD,                 ONLY: UNDEF_PARAM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: N_MSGINTFLDS
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: IMSGINTFLDS2CMSGINTFLDS
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: N_MSGSTRFLDS
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: IMSGSTRINGFLDS2CMSGSTRINGFLDS
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: N_MSGFLOATFLDS
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: IMSGFLOATFLDS2CMSGFLOATFLDS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(INOUT) :: THIS
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
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IMSGINTFLDS2CMSGINTFLDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_INT=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IMSGSTRINGFLDS2CMSGSTRINGFLDS=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_STRING=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IMSGFLOATFLDS2CMSGFLOATFLDS=5_JPIB_K
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

  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) '** MESSAGE PRINT'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! Integer members
  IF ( N_MSGINTFLDS .GT. 0 ) THEN
    WRITE(UNIT,*) '+ Integer members'
    DO I = 1, N_MSGINTFLDS
      PP_TRYCALL(ERRFLAG_IMSGINTFLDS2CMSGINTFLDS) IMSGINTFLDS2CMSGINTFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_INT) THIS%GET_ENUM_INT( I, ITMP, HOOKS )
      IF ( ITMP .NE. UNDEF_PARAM_E ) THEN
        WRITE(UNIT,'(A3,A20,A3,I32)',IOSTAT=WRITE_STAT) ' - ', TRIM(ADJUSTL(CKEY)) ,' : ', ITMP
        PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )
      ENDIF
    ENDDO
  ENDIF

  ! String members
  IF ( N_MSGSTRFLDS .GT. 0 ) THEN
    WRITE(UNIT,*) '+ String members'
    DO I = 1, N_MSGSTRFLDS
      PP_TRYCALL(ERRFLAG_IMSGSTRINGFLDS2CMSGSTRINGFLDS) IMSGSTRINGFLDS2CMSGSTRINGFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_STRING) THIS%GET_ENUM_STRING( I, CTMP, HOOKS )
      WRITE(UNIT,'(A3,A20,A3,A8)',IOSTAT=WRITE_STAT) ' - ', TRIM(ADJUSTL(CKEY)) ,' : ', CTMP
      PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )
    ENDDO
  ENDIF

  ! Float members
  IF ( N_MSGFLOATFLDS .GT. 0 ) THEN
    WRITE(UNIT,*) '+ Float members'
    DO I = 1, N_MSGFLOATFLDS
      PP_TRYCALL(ERRFLAG_IMSGFLOATFLDS2CMSGFLOATFLDS) IMSGFLOATFLDS2CMSGFLOATFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_FLOAT) THIS%GET_ENUM_FLOAT( I, RTMP, HOOKS )
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
    CASE(ERRFLAG_IMSGINTFLDS2CMSGINTFLDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert integer field ID to string' )
    CASE(ERRFLAG_GET_INT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get integer field' )
    CASE(ERRFLAG_IMSGSTRINGFLDS2CMSGSTRINGFLDS)
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
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_TO_JSON'
FUNCTION FORTRAN_MESSAGE_TO_JSON( THIS, JSON, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPRD_K
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD,        ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: N_MSGINTFLDS
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: IMSGINTFLDS2CMSGINTFLDS
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: N_MSGSTRFLDS
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: IMSGSTRINGFLDS2CMSGSTRINGFLDS
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: N_MSGFLOATFLDS
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: IMSGFLOATFLDS2CMSGFLOATFLDS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T),      INTENT(INOUT) :: THIS
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
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IMSGINTFLDS2CMSGINTFLDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_INT=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IMSGSTRINGFLDS2CMSGSTRINGFLDS=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_STRING=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IMSGFLOATFLDS2CMSGFLOATFLDS=5_JPIB_K
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
  N_FIELDS = N_MSGINTFLDS + N_MSGSTRFLDS + N_MSGFLOATFLDS

  ! Count the number of characters needed for the JSON string
  SZ = 9
  LO = 1
  HI = LO + SZ - 1
  IF ( N_MSGINTFLDS .GT. 0 ) THEN
    DO I = 1, N_MSGINTFLDS

      CTMP=REPEAT(' ',32)
      CKEY=REPEAT(' ',16)
      PP_TRYCALL(ERRFLAG_IMSGINTFLDS2CMSGINTFLDS) IMSGINTFLDS2CMSGINTFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_INT) THIS%GET_ENUM_INT( I, ITMP, HOOKS )
      WRITE(CTMP,*,IOSTAT=WRITE_STAT) ITMP
      SZ = 1 + LEN_TRIM(ADJUSTL(CKEY)) + 1 + LEN_TRIM(ADJUSTL(CTMP)) + 1
      LO = HI + 1
      HI = LO + SZ - 1
    ENDDO
  ENDIF

  ! String members
  IF ( N_MSGSTRFLDS .GT. 0 ) THEN
    DO I = 1, N_MSGSTRFLDS
      CTMP=REPEAT(' ',32)
      CKEY=REPEAT(' ',16)
      PP_TRYCALL(ERRFLAG_IMSGSTRINGFLDS2CMSGSTRINGFLDS) IMSGSTRINGFLDS2CMSGSTRINGFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_STRING) THIS%GET_ENUM_STRING( I, CTMP, HOOKS )
      SZ = 1 + LEN_TRIM(ADJUSTL(CKEY)) + 1 + LEN_TRIM(ADJUSTL(CTMP)) + 1
      LO = HI + 1
      HI = LO + SZ - 1
    ENDDO
  ENDIF

  ! Float members
  IF ( N_MSGFLOATFLDS .GT. 0 ) THEN
    DO I = 1, N_MSGFLOATFLDS
      CTMP=REPEAT(' ',32)
      CKEY=REPEAT(' ',16)
      PP_TRYCALL(ERRFLAG_IMSGFLOATFLDS2CMSGFLOATFLDS) IMSGFLOATFLDS2CMSGFLOATFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_FLOAT) THIS%GET_ENUM_FLOAT( I, RTMP, HOOKS )
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
  SZ = 9
  LO = 1
  HI = LO + SZ - 1
  JSON(LO:HI) = 'message={'
  IF ( N_MSGINTFLDS .GT. 0 ) THEN
    DO I = 1, N_MSGINTFLDS
      CNT = CNT + 1
      IF ( CNT .GE. N_FIELDS ) THEN
        SEP = ' '
      ELSE
        SEP = ','
      ENDIF
      CTMP=REPEAT(' ',32)
      CKEY=REPEAT(' ',16)
      PP_TRYCALL(ERRFLAG_IMSGINTFLDS2CMSGINTFLDS) IMSGINTFLDS2CMSGINTFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_INT) THIS%GET_ENUM_INT( I, ITMP, HOOKS )
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
  IF ( N_MSGSTRFLDS .GT. 0 ) THEN
    DO I = 1, N_MSGSTRFLDS
      CNT = CNT + 1
      IF ( CNT .GE. N_FIELDS ) THEN
        SEP = ' '
      ELSE
        SEP = ','
      ENDIF
      CTMP=REPEAT(' ',32)
      CKEY=REPEAT(' ',16)
      PP_TRYCALL(ERRFLAG_IMSGSTRINGFLDS2CMSGSTRINGFLDS) IMSGSTRINGFLDS2CMSGSTRINGFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_STRING) THIS%GET_ENUM_STRING( I, CTMP, HOOKS )
      SZ = 1 + LEN_TRIM(ADJUSTL(CKEY)) + 1 + LEN_TRIM(ADJUSTL(CTMP)) + 1
      LO = HI + 1
      HI = LO + SZ - 1
      JSON_ITEM = REPEAT(' ', 1024)
      WRITE(JSON_ITEM, '(A1,A,A1,A,A1)') ' ', TRIM(ADJUSTL(CKEY)) ,':', TRIM(ADJUSTL(CTMP)), SEP
      JSON(LO:HI) = TRIM(JSON_ITEM)
    ENDDO
  ENDIF

  ! Float members
  IF ( N_MSGFLOATFLDS .GT. 0 ) THEN
    DO I = 1, N_MSGFLOATFLDS
      CNT = CNT + 1
      IF ( CNT .GE. N_FIELDS ) THEN
        SEP = ' '
      ELSE
        SEP = ','
      ENDIF
      CTMP=REPEAT(' ',32)
      CKEY=REPEAT(' ',16)
      PP_TRYCALL(ERRFLAG_IMSGFLOATFLDS2CMSGFLOATFLDS) IMSGFLOATFLDS2CMSGFLOATFLDS( I, CKEY, HOOKS )
      PP_TRYCALL(ERRFLAG_GET_FLOAT) THIS%GET_ENUM_FLOAT( I, RTMP, HOOKS )
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
    CASE(ERRFLAG_IMSGINTFLDS2CMSGINTFLDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert integer field ID to string' )
    CASE(ERRFLAG_GET_INT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get integer field' )
    CASE(ERRFLAG_IMSGSTRINGFLDS2CMSGSTRINGFLDS)
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
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_TO_JSON
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_COPY_DATA_FROM'
FUNCTION FORTRAN_MESSAGE_COPY_DATA_FROM( THIS, OTHER, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: OTHER
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET


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

  !> Reset to default values
  THIS%STREAM     = OTHER%STREAM
  THIS%TYPE       = OTHER%TYPE
  THIS%CLASS      = OTHER%CLASS
  THIS%EXPVER     = OTHER%EXPVER
  THIS%ORIGIN     = OTHER%ORIGIN
  THIS%ANOFFSET   = OTHER%ANOFFSET
  THIS%NUMBER     = OTHER%NUMBER
  THIS%IDENT      = OTHER%IDENT
  THIS%INSTRUMENT = OTHER%INSTRUMENT
  THIS%CHANNEL    = OTHER%CHANNEL
  THIS%PARAM_TYPE = OTHER%PARAM_TYPE
  THIS%CHEM       = OTHER%CHEM
  THIS%PARAM      = OTHER%PARAM
  THIS%LEVTYPE    = OTHER%LEVTYPE
  THIS%LEVELIST   = OTHER%LEVELIST
  THIS%DIRECTION  = OTHER%DIRECTION
  THIS%FREQUENCY  = OTHER%FREQUENCY
  THIS%MODEL      = OTHER%MODEL
  THIS%REPRES     = OTHER%REPRES
  THIS%DATE       = OTHER%DATE
  THIS%TIME       = OTHER%TIME
  THIS%STEP       = OTHER%STEP
  THIS%GRID       = OTHER%GRID
  THIS%PACKING    = OTHER%PACKING

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
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_COPY_DATA_FROM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_SWAP_DATA'
FUNCTION FORTRAN_MESSAGE_SWAP_DATA( THIS, OTHER, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,   ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(INOUT) :: OTHER
  TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: ITMP
  CHARACTER(LEN=4)     :: CTMP4
  CHARACTER(LEN=8)     :: CTMP8

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

  !> Swap data
  ITMP         = THIS%STREAM
  THIS%STREAM  = OTHER%STREAM
  OTHER%STREAM = ITMP

  ITMP       = THIS%TYPE
  THIS%TYPE  = OTHER%TYPE
  OTHER%TYPE = ITMP

  ITMP        = THIS%CLASS
  THIS%CLASS  = OTHER%CLASS
  OTHER%CLASS = ITMP

  CTMP4        = THIS%EXPVER
  THIS%EXPVER  = OTHER%EXPVER
  OTHER%EXPVER = CTMP4

  ITMP         = THIS%ORIGIN
  THIS%ORIGIN  = OTHER%ORIGIN
  OTHER%ORIGIN = ITMP

  ITMP           = THIS%ANOFFSET
  THIS%ANOFFSET  = OTHER%ANOFFSET
  OTHER%ANOFFSET = ITMP

  ITMP         = THIS%NUMBER
  THIS%NUMBER  = OTHER%NUMBER
  OTHER%NUMBER = ITMP

  ITMP        = THIS%IDENT
  THIS%IDENT  = OTHER%IDENT
  OTHER%IDENT = ITMP

  ITMP             = THIS%INSTRUMENT
  THIS%INSTRUMENT  = OTHER%INSTRUMENT
  OTHER%INSTRUMENT = ITMP

  ITMP          = THIS%CHANNEL
  THIS%CHANNEL  = OTHER%CHANNEL
  OTHER%CHANNEL = ITMP

  ITMP             = THIS%PARAM_TYPE
  THIS%PARAM_TYPE  = OTHER%PARAM_TYPE
  OTHER%PARAM_TYPE = ITMP

  ITMP       = THIS%CHEM
  THIS%CHEM  = OTHER%CHEM
  OTHER%CHEM = ITMP

  ITMP        = THIS%PARAM
  THIS%PARAM  = OTHER%PARAM
  OTHER%PARAM = ITMP

  ITMP          = THIS%LEVTYPE
  THIS%LEVTYPE  = OTHER%LEVTYPE
  OTHER%LEVTYPE = ITMP

  ITMP           = THIS%LEVELIST
  THIS%LEVELIST  = OTHER%LEVELIST
  OTHER%LEVELIST = ITMP

  ITMP            = THIS%DIRECTION
  THIS%DIRECTION  = OTHER%DIRECTION
  OTHER%DIRECTION = ITMP

  ITMP            = THIS%FREQUENCY
  THIS%FREQUENCY  = OTHER%FREQUENCY
  OTHER%FREQUENCY = ITMP

  ITMP        = THIS%MODEL
  THIS%MODEL  = OTHER%MODEL
  OTHER%MODEL =ITMP

  ITMP         = THIS%REPRES
  THIS%REPRES  = OTHER%REPRES
  OTHER%REPRES = ITMP

  ITMP       = THIS%DATE
  THIS%DATE  = OTHER%DATE
  OTHER%DATE = ITMP

  ITMP       = THIS%TIME
  THIS%TIME  = OTHER%TIME
  OTHER%TIME = ITMP

  ITMP       = THIS%STEP
  THIS%STEP  = OTHER%STEP
  OTHER%STEP = ITMP

  CTMP8      = THIS%GRID
  THIS%GRID  = OTHER%GRID
  OTHER%GRID = CTMP8

  ITMP          = THIS%PACKING
  THIS%PACKING  = OTHER%PACKING
  OTHER%PACKING = ITMP

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
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_SWAP_DATA
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_EQUAL_TO'
FUNCTION FORTRAN_MESSAGE_EQUAL_TO( THIS, OTHER, OPT, IS_EQUAL, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,   ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(IN)    :: THIS
  TYPE(FORTRAN_MESSAGE_T),  INTENT(IN)    :: OTHER
  TYPE(CACHE_OPTIONS_T),    INTENT(IN)    :: OPT
  LOGICAL,                  INTENT(OUT)   :: IS_EQUAL
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET


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

  !> Compare two messages
  IS_EQUAL = .TRUE.

  !IF ( OPT%CACHE_LOCAL_USE_INFO ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%STREAM .EQ. OTHER%STREAM )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%TYPE .EQ. OTHER%TYPE )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%CLASS .EQ. OTHER%CLASS )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%EXPVER .EQ. OTHER%EXPVER )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%ORIGIN .EQ. OTHER%ORIGIN )
  !ENDIF

  ! Information related to time should never  be compared
  !IF ( OPT%CACHE_TIME_RELATED_INFO ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%ANOFFSET .EQ. OTHER%ANOFFSET )
  !ENDIF

  !IF ( OPT%CACHE_SATELLITES_INFO ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%NUMBER .EQ. OTHER%NUMBER )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%IDENT .EQ. OTHER%IDENT )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%INSTRUMENT .EQ. OTHER%INSTRUMENT )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%CHANNEL .EQ. OTHER%CHANNEL )
  !ENDIF

  !IF ( OPT%CACHE_PRODUCT_DEFINITION_INFO ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%PARAM_TYPE .EQ. OTHER%PARAM_TYPE )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%CHEM .EQ. OTHER%CHEM )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%PARAM .EQ. OTHER%PARAM )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%LEVTYPE .EQ. OTHER%LEVTYPE )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%MODEL .EQ. OTHER%MODEL )
  !ENDIF

  !IF ( OPT%CACHE_TYPE_OF_LEVELS ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%LEVELIST .EQ. OTHER%LEVELIST )
  !ENDIF

  !IF ( OPT%CACHE_DIRECTION_FREQUENCY ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%DIRECTION .EQ. OTHER%DIRECTION )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%FREQUENCY .EQ. OTHER%FREQUENCY )
  !ENDIF


  ! Time information should never be cached
  !IF ( OPT%CACHE_TIME_RELATED_INFO ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%DATE .EQ. OTHER%DATE )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%TIME .EQ. OTHER%TIME )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%STEP .EQ. OTHER%STEP )
  !ENDIF

  !IF ( OPT%CACHE_GRID_DEFINITION_INFO ) THEN
    IS_EQUAL = IS_EQUAL .AND. ( THIS%REPRES .EQ. OTHER%REPRES )
    IS_EQUAL = IS_EQUAL .AND. ( THIS%GRID .EQ. OTHER%GRID )
  !ENDIF

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
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_EQUAL_TO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_LOWER_THAN'
FUNCTION FORTRAN_MESSAGE_LOWER_THAN( THIS, OTHER, OPT, IS_LOWER_THAN, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,   ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(IN)    :: THIS
  TYPE(FORTRAN_MESSAGE_T),  INTENT(IN)    :: OTHER
  TYPE(CACHE_OPTIONS_T),    INTENT(IN)    :: OPT
  LOGICAL,                  INTENT(OUT)   :: IS_LOWER_THAN
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET


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

  !> Compare two messages
  IS_LOWER_THAN = .TRUE.
  ! IF ( OPT%CACHE_LOCAL_USE_INFO ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%STREAM .LT. OTHER%STREAM )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%TYPE .LT. OTHER%TYPE )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%CLASS .LT. OTHER%CLASS )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%EXPVER .LT. OTHER%EXPVER )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%ORIGIN .LT. OTHER%ORIGIN )
  ! ENDIF


  ! Information related to time should never  be compared
  ! IF ( OPT%CACHE_TIME_RELATED_INFO ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%ANOFFSET .LT. OTHER%ANOFFSET )
  ! ENDIF

  ! IF ( OPT%CACHE_SATELLITES_INFO ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%NUMBER .LT. OTHER%NUMBER )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%IDENT .LT. OTHER%IDENT )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%INSTRUMENT .LT. OTHER%INSTRUMENT )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%CHANNEL .LT. OTHER%CHANNEL )
  ! ENDIF

  ! IF ( OPT%CACHE_PRODUCT_DEFINITION_INFO ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%PARAM_TYPE .LT. OTHER%PARAM_TYPE )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%CHEM .LT. OTHER%CHEM )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%PARAM .LT. OTHER%PARAM )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%LEVTYPE .LT. OTHER%LEVTYPE )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%MODEL .LT. OTHER%MODEL )
  ! ENDIF

  ! IF ( OPT%CACHE_TYPE_OF_LEVELS ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%LEVELIST .LT. OTHER%LEVELIST )
  ! ENDIF

  ! IF ( OPT%CACHE_DIRECTION_FREQUENCY ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%DIRECTION .LT. OTHER%DIRECTION )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%FREQUENCY .LT. OTHER%FREQUENCY )
  ! ENDIF

  ! Time information should never be cached!!!
  ! IF ( OPT%CACHE_TIME_RELATED_INFO ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%DATE .LT. OTHER%DATE )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%TIME .LT. OTHER%TIME )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%STEP .LT. OTHER%STEP )
  ! ENDIF

  ! IF ( OPT%CACHE_GRID_DEFINITION_INFO ) THEN
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%REPRES .LT. OTHER%REPRES )
    IS_LOWER_THAN = IS_LOWER_THAN .AND. ( THIS%GRID .LT. OTHER%GRID )
  ! ENDIF

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
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_LOWER_THAN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_SET_ENUM_INT'
FUNCTION FORTRAN_MESSAGE_SET_ENUM_INT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_STREAM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_TYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_CLASS_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_ORIGIN_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_ANOFFSET_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_NUMBER_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_IDENT_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_INSTRUMENT_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_CHANNEL_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_PARAM_TYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_CHEM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_PARAM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_LEVTYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_LEVELIST_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_DIRECTION_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_FREQUENCY_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_MODEL_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_REPRES_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_DATE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_TIME_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_STEP_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_PACKING_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K

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

  ! Set fields by field ID
  SELECT CASE (ID)
  CASE (MSGINTFLD_STREAM_E)
    THIS%STREAM  = VALUE
  CASE (MSGINTFLD_TYPE_E)
    THIS%TYPE = VALUE
  CASE (MSGINTFLD_CLASS_E)
    THIS%CLASS = VALUE
  CASE (MSGINTFLD_ORIGIN_E)
    THIS%ORIGIN = VALUE
  CASE (MSGINTFLD_ANOFFSET_E)
    THIS%ANOFFSET = VALUE
  CASE (MSGINTFLD_NUMBER_E)
    THIS%NUMBER = VALUE
  CASE (MSGINTFLD_IDENT_E)
    THIS%IDENT = VALUE
  CASE (MSGINTFLD_INSTRUMENT_E)
    THIS%INSTRUMENT = VALUE
  CASE (MSGINTFLD_CHANNEL_E)
    THIS%CHANNEL = VALUE
  CASE (MSGINTFLD_PARAM_TYPE_E)
    THIS%PARAM_TYPE = VALUE
  CASE (MSGINTFLD_CHEM_E)
    THIS%CHEM = VALUE
  CASE (MSGINTFLD_PARAM_E)
    THIS%PARAM = VALUE
  CASE (MSGINTFLD_LEVTYPE_E)
    THIS%LEVTYPE = VALUE
  CASE (MSGINTFLD_LEVELIST_E)
    THIS%LEVELIST = VALUE
  CASE (MSGINTFLD_DIRECTION_E)
    THIS%DIRECTION = VALUE
  CASE (MSGINTFLD_FREQUENCY_E)
    THIS%FREQUENCY = VALUE
  CASE (MSGINTFLD_MODEL_E)
    THIS%MODEL = VALUE
  CASE (MSGINTFLD_REPRES_E)
    THIS%REPRES = VALUE
  CASE (MSGINTFLD_DATE_E)
    THIS%DATE = VALUE
  CASE (MSGINTFLD_TIME_E)
    THIS%TIME = VALUE
  CASE (MSGINTFLD_STEP_E)
    THIS%STEP = VALUE
  CASE (MSGINTFLD_PACKING_E)
    THIS%PACKING = VALUE
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_FIELD_ID )
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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_SET_ENUM_INT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_SET_KEY_INT'
FUNCTION FORTRAN_MESSAGE_SET_KEY_INT( THIS, KEY, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: CMSGINTFLDS2IMSGINTFLDS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: ID

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_ENUM=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SET_VALUE=2_JPIB_K

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

  ! Set fields by field ID
  PP_TRYCALL(ERRFLAG_CONVERT_TO_ENUM) CMSGINTFLDS2IMSGINTFLDS( KEY, ID, HOOKS )
  PP_TRYCALL(ERRFLAG_SET_VALUE) THIS%SET_ENUM_INT( ID, VALUE, HOOKS )

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
    CASE (ERRFLAG_CONVERT_TO_ENUM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'convert to enum' )
    CASE (ERRFLAG_SET_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'set value' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_SET_KEY_INT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_SET_ENUM_STRING'
FUNCTION FORTRAN_MESSAGE_SET_ENUM_STRING( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGSTRFLD_GRID_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGSTRFLD_EXPVER_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  CHARACTER(LEN=*),         INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K


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

  !> Select the prefix
  SELECT CASE ( ID )
  CASE (MSGSTRFLD_GRID_E)
    THIS%GRID = VALUE
  CASE (MSGSTRFLD_EXPVER_E)
    THIS%EXPVER = VALUE
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_FIELD_ID )
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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_SET_ENUM_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_SET_KEY_STRING'
FUNCTION FORTRAN_MESSAGE_SET_KEY_STRING( THIS, KEY, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: CMSGSTRINGFLDS2IMSGSTRINGFLDS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  CHARACTER(LEN=*),         INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: ID

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_ENUM=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SET_VALUE=2_JPIB_K

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

  !> Select the prefix
  PP_TRYCALL(ERRFLAG_CONVERT_TO_ENUM) CMSGSTRINGFLDS2IMSGSTRINGFLDS( KEY, ID, HOOKS )
  PP_TRYCALL(ERRFLAG_SET_VALUE) THIS%SET_ENUM_STRING( ID, VALUE, HOOKS )

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
    CASE (ERRFLAG_CONVERT_TO_ENUM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'convert to enum' )
    CASE (ERRFLAG_SET_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'set value' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_SET_KEY_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_SET_ENUM_FLOAT'
FUNCTION FORTRAN_MESSAGE_SET_ENUM_FLOAT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  REAL(KIND=JPRD_K),        INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K

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

  PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_FIELD_ID )

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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_SET_ENUM_FLOAT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_SET_KEY_FLOAT'
FUNCTION FORTRAN_MESSAGE_SET_KEY_FLOAT( THIS, KEY, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  REAL(KIND=JPRD_K),        INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: ID

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K

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

  PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_FIELD_ID )

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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_SET_KEY_FLOAT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_GET_ENUM_INT'
FUNCTION FORTRAN_MESSAGE_GET_ENUM_INT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_STREAM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_TYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_CLASS_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_ORIGIN_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_ANOFFSET_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_NUMBER_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_IDENT_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_INSTRUMENT_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_CHANNEL_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_PARAM_TYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_CHEM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_PARAM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_LEVTYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_LEVELIST_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_DIRECTION_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_FREQUENCY_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_MODEL_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_REPRES_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_DATE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_TIME_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_STEP_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_PACKING_E
  USE :: ENUMERATORS_MOD,                 ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  INTEGER(KIND=JPIB_K),     INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUE=3_JPIB_K


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

  ! Initialize the output value
  VALUE = 0_JPIB_K

  ! Set fields by field ID
  SELECT CASE (ID)
  CASE (MSGINTFLD_STREAM_E)
    VALUE = THIS%STREAM
  CASE (MSGINTFLD_TYPE_E)
    VALUE = THIS%TYPE
  CASE (MSGINTFLD_CLASS_E)
    VALUE = THIS%CLASS
  CASE (MSGINTFLD_ORIGIN_E)
    VALUE = THIS%ORIGIN
  CASE (MSGINTFLD_ANOFFSET_E)
    VALUE = THIS%ANOFFSET
  CASE (MSGINTFLD_NUMBER_E)
    VALUE = THIS%NUMBER
  CASE (MSGINTFLD_IDENT_E)
    VALUE = THIS%IDENT
  CASE (MSGINTFLD_INSTRUMENT_E)
    VALUE = THIS%INSTRUMENT
  CASE (MSGINTFLD_CHANNEL_E)
    VALUE = THIS%CHANNEL
  CASE (MSGINTFLD_PARAM_TYPE_E)
    VALUE = THIS%PARAM_TYPE
  CASE (MSGINTFLD_CHEM_E)
    VALUE = THIS%CHEM
  CASE (MSGINTFLD_PARAM_E)
    VALUE = THIS%PARAM
  CASE (MSGINTFLD_LEVTYPE_E)
    VALUE = THIS%LEVTYPE
  CASE (MSGINTFLD_LEVELIST_E)
    VALUE = THIS%LEVELIST
  CASE (MSGINTFLD_DIRECTION_E)
    VALUE = THIS%DIRECTION
  CASE (MSGINTFLD_FREQUENCY_E)
    VALUE = THIS%FREQUENCY
  CASE (MSGINTFLD_MODEL_E)
    VALUE = THIS%MODEL
  CASE (MSGINTFLD_REPRES_E)
    VALUE = THIS%REPRES
  CASE (MSGINTFLD_DATE_E)
    VALUE = THIS%DATE
  CASE (MSGINTFLD_TIME_E)
    VALUE = THIS%TIME
  CASE (MSGINTFLD_STEP_E)
    VALUE = THIS%STEP
  CASE (MSGINTFLD_PACKING_E)
    VALUE = THIS%PACKING
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_FIELD_ID )
  END SELECT

  ! Check value
  PP_DEBUG_CRITICAL_COND_THROW( VALUE .EQ. UNDEF_PARAM_E, ERRFLAG_INVALID_VALUE )

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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
    CASE (ERRFLAG_INVALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid value' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_GET_ENUM_INT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_GET_KEY_INT'
FUNCTION FORTRAN_MESSAGE_GET_KEY_INT( THIS, KEY, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: CMSGINTFLDS2IMSGINTFLDS
  USE :: ENUMERATORS_MOD,                 ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(IN)    :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  INTEGER(KIND=JPIB_K),     INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: ID

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SET_VALUE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUE=3_JPIB_K

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

  ! Initialize the output value
  VALUE = 0_JPIB_K

  ! Get field by ID
  PP_TRYCALL(ERRFLAG_INVALID_FIELD_ID) CMSGINTFLDS2IMSGINTFLDS( KEY, ID, HOOKS )
  PP_TRYCALL(ERRFLAG_SET_VALUE) THIS%GET_ENUM_INT( ID, VALUE, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( VALUE .EQ. UNDEF_PARAM_E, ERRFLAG_INVALID_VALUE )

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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
    CASE (ERRFLAG_SET_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'set value' )
    CASE (ERRFLAG_INVALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid value' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_GET_KEY_INT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_HAS_ENUM_INT'
FUNCTION FORTRAN_MESSAGE_HAS_ENUM_INT( THIS, ID, HAS_FIELD, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: HAS_IMSGINTFLDS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  LOGICAL,                  INTENT(OUT)   :: HAS_FIELD
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUE=3_JPIB_K


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

  ! Check if the field is valid
  PP_TRYCALL(ERRFLAG_INVALID_FIELD_ID) HAS_IMSGINTFLDS( ID, HAS_FIELD, HOOKS )

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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
    CASE (ERRFLAG_INVALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid value' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_HAS_ENUM_INT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_GET_ENUM_STRING'
FUNCTION FORTRAN_MESSAGE_GET_ENUM_STRING( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGSTRFLD_GRID_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGSTRFLD_EXPVER_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  CHARACTER(LEN=8),         INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUE=2_JPIB_K

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

  !> Select the prefix
  SELECT CASE ( ID )
  CASE (MSGSTRFLD_GRID_E)
    VALUE = THIS%GRID
    PP_DEBUG_CRITICAL_COND_THROW( VALUE .EQ. '********', ERRFLAG_INVALID_VALUE )
  CASE (MSGSTRFLD_EXPVER_E)
    VALUE = THIS%EXPVER
    PP_DEBUG_CRITICAL_COND_THROW( VALUE .EQ. '****', ERRFLAG_INVALID_VALUE )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_FIELD_ID )
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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
    CASE (ERRFLAG_INVALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid value' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_GET_ENUM_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_GET_KEY_STRING'
FUNCTION FORTRAN_MESSAGE_GET_KEY_STRING( THIS, KEY, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: CMSGSTRINGFLDS2IMSGSTRINGFLDS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(IN)    :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  CHARACTER(LEN=8),         INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: ID

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SET_VALUE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUE=3_JPIB_K

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

  !> Select the prefix
  PP_TRYCALL(ERRFLAG_INVALID_FIELD_ID) CMSGSTRINGFLDS2IMSGSTRINGFLDS( KEY, ID, HOOKS )
  PP_TRYCALL(ERRFLAG_SET_VALUE) THIS%GET_ENUM_STRING( ID, VALUE, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( VALUE .EQ. '********', ERRFLAG_INVALID_VALUE )

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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
    CASE (ERRFLAG_SET_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'set value' )
    CASE (ERRFLAG_INVALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid value' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_GET_KEY_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_GET_ENUM_FLOAT'
FUNCTION FORTRAN_MESSAGE_GET_ENUM_FLOAT( THIS, ID, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: ID
  REAL(KIND=JPRD_K),        INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K

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

  VALUE = 0.0_JPRD_K

  PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_FIELD_ID )

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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_GET_ENUM_FLOAT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_GET_KEY_FLOAT'
FUNCTION FORTRAN_MESSAGE_GET_KEY_FLOAT( THIS, KEY, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T), INTENT(IN)    :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  REAL(KIND=JPRD_K),        INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD_ID=1_JPIB_K

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

  VALUE = 0.0_JPRD_K

  PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_FIELD_ID )

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
    CASE (ERRFLAG_INVALID_FIELD_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid field ID' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_GET_KEY_FLOAT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FORTRAN_MESSAGE_READ_FROM_YAML'
FUNCTION FORTRAN_MESSAGE_READ_FROM_YAML( MSG, CONFIG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: FUN_C2I_IF
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER_WITH_FILTER
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION
  USE :: ENUMERATORS_MOD,     ONLY: UNDEF_PARAM_E

  USE :: ENUMERATORS_MOD, ONLY: CSTREAM2ISTREAM
  USE :: ENUMERATORS_MOD, ONLY: CTYPE2ITYPE
  USE :: ENUMERATORS_MOD, ONLY: CCLASS2ICLASS
  USE :: ENUMERATORS_MOD, ONLY: CPACKING2IPACKING
  USE :: ENUMERATORS_MOD, ONLY: CLEVTYPE2ILEVTYPE
  USE :: ENUMERATORS_MOD, ONLY: CREPRES2IREPRES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FORTRAN_MESSAGE_T),   INTENT(INOUT) :: MSG
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CONFIG
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(YAML_CONFIGURATION_T) :: MSG_CONFIG
  LOGICAL :: HAS_MESSAGE
  LOGICAL :: HAS_KEY
  PROCEDURE(FUN_C2I_IF), POINTER :: P_FILTER
  CHARACTER(LEN=:), ALLOCATABLE :: CTMP
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_EXPVER=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_GRID=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DELETE_CONFIGURATION=6_JPIB_K

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CONFIG, 'message', HAS_MESSAGE, HOOKS )


  !> Read representations
  IF ( HAS_MESSAGE  ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CONFIG, 'message', MSG_CONFIG, HOOKS )

    !> Read the "stream"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'stream', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      P_FILTER => CSTREAM2ISTREAM
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER_WITH_FILTER( MSG_CONFIG, 'stream', MSG%STREAM, P_FILTER, HOOKS )
    ELSE
      MSG%STREAM = UNDEF_PARAM_E
    ENDIF

    !> Read the "type"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'type', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      P_FILTER => CTYPE2ITYPE
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER_WITH_FILTER( MSG_CONFIG, 'type', MSG%TYPE, P_FILTER, HOOKS )
    ELSE
      MSG%TYPE = UNDEF_PARAM_E
    ENDIF

    !> Read the "class"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'class', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      P_FILTER => CCLASS2ICLASS
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER_WITH_FILTER( MSG_CONFIG, 'class', MSG%CLASS, P_FILTER, HOOKS )
    ELSE
      MSG%CLASS = UNDEF_PARAM_E
    ENDIF

    !> Read the "expver"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'expver', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      IF ( ALLOCATED(CTMP) ) THEN
        DEALLOCATE(CTMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
        PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      ENDIF
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_STRING( MSG_CONFIG, 'expver', CTMP, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CTMP), ERRFLAG_UNABLE_TO_READ_CFG )
      PP_DEBUG_CRITICAL_COND_THROW( LEN(CTMP).GT.4, ERRFLAG_INVALID_EXPVER )
      MSG%EXPVER = CTMP
      IF ( ALLOCATED(CTMP) ) THEN
        DEALLOCATE(CTMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
        PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      ENDIF
    ELSE
      MSG%EXPVER = REPEAT('*',4)
    ENDIF

    !> Read the "origin"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'origin', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( MSG_CONFIG, 'origin', MSG%ORIGIN, HOOKS )
    ELSE
      MSG%ORIGIN = UNDEF_PARAM_E
    ENDIF

    !> Read the "anoffset"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'anoffset', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( MSG_CONFIG, 'anoffset', MSG%ANOFFSET, HOOKS )
    ELSE
      MSG%ANOFFSET = UNDEF_PARAM_E
    ENDIF

    !> Read the "packing"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'packing', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      P_FILTER => CPACKING2IPACKING
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER_WITH_FILTER( MSG_CONFIG, 'packing', MSG%PACKING, P_FILTER, HOOKS )
    ELSE
      MSG%PACKING = UNDEF_PARAM_E
    ENDIF

    !> Read the "number"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'number', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( MSG_CONFIG, 'number', MSG%NUMBER, HOOKS )
    ELSE
      MSG%NUMBER = UNDEF_PARAM_E
    ENDIF

    !> Read the "ident"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'ident', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( MSG_CONFIG, 'ident', MSG%IDENT, HOOKS )
    ELSE
      MSG%IDENT = UNDEF_PARAM_E
    ENDIF

    !> Read the "instrument"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'instrument', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( MSG_CONFIG, 'instrument', MSG%INSTRUMENT, HOOKS )
    ELSE
      MSG%INSTRUMENT = UNDEF_PARAM_E
    ENDIF

    !> Read the "channel"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'channel', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( MSG_CONFIG, 'channel', MSG%CHANNEL, HOOKS )
    ELSE
      MSG%CHANNEL = UNDEF_PARAM_E
    ENDIF

    !> Read the "param"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'param', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( MSG_CONFIG, 'param', MSG%PARAM, HOOKS )
    ELSE
      MSG%PARAM = UNDEF_PARAM_E
    ENDIF

    !> Read the "levtype"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'levtype', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      P_FILTER => CLEVTYPE2ILEVTYPE
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER_WITH_FILTER( MSG_CONFIG, 'levtype', MSG%LEVTYPE, P_FILTER, HOOKS )
    ELSE
      MSG%LEVTYPE = UNDEF_PARAM_E
    ENDIF

    !> Read the "levelist"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'levelist', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( MSG_CONFIG, 'levelist', MSG%LEVELIST, HOOKS )
    ELSE
      MSG%LEVELIST = UNDEF_PARAM_E
    ENDIF

    !> Read the "direction"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'direction', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( MSG_CONFIG, 'direction', MSG%DIRECTION, HOOKS )
    ELSE
      MSG%DIRECTION = UNDEF_PARAM_E
    ENDIF

    !> Read the "frequency"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'direction', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( MSG_CONFIG, 'direction', MSG%FREQUENCY, HOOKS )
    ELSE
      MSG%FREQUENCY = UNDEF_PARAM_E
    ENDIF

    !> Read the "repres"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'repres', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      P_FILTER => CREPRES2IREPRES
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER_WITH_FILTER( MSG_CONFIG, 'repres', MSG%REPRES, P_FILTER, HOOKS )
    ELSE
      MSG%REPRES = UNDEF_PARAM_E
    ENDIF

    !> Read the "date"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'date', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( MSG_CONFIG, 'date', MSG%DATE, HOOKS )
    ELSE
      MSG%DATE = UNDEF_PARAM_E
    ENDIF

    !> Read the "time"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'time', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( MSG_CONFIG, 'time', MSG%TIME, HOOKS )
    ELSE
      MSG%TIME = UNDEF_PARAM_E
    ENDIF

    !> Read the "step"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'step', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( MSG_CONFIG, 'step', MSG%STEP, HOOKS )
    ELSE
      MSG%STEP = UNDEF_PARAM_E
    ENDIF

    !> Read the "grid"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MSG_CONFIG, 'grid', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      IF ( ALLOCATED(CTMP) ) THEN
        DEALLOCATE(CTMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
        PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      ENDIF
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_STRING( MSG_CONFIG, 'grid', CTMP, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CTMP), ERRFLAG_UNABLE_TO_READ_CFG )
      PP_DEBUG_CRITICAL_COND_THROW( LEN(CTMP).GT.8, ERRFLAG_INVALID_GRID )
      MSG%GRID = CTMP
      IF ( ALLOCATED(CTMP) ) THEN
        DEALLOCATE(CTMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
        PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      ENDIF
    ELSE
      MSG%GRID = REPEAT('*',8)
    ENDIF

    !> Destroy the configuration object
    PP_TRYCALL(ERRFLAG_UNABLE_TO_DELETE_CONFIGURATION) YAML_DELETE_CONFIGURATION( MSG_CONFIG, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
    CASE (ERRFLAG_INVALID_EXPVER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid expver' )
    CASE (ERRFLAG_INVALID_GRID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid grid' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate memory' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfiguration' )
    CASE (ERRFLAG_UNABLE_TO_DELETE_CONFIGURATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to delete configuration' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FORTRAN_MESSAGE_READ_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE FORTRAN_MESSAGE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
