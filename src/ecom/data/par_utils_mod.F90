! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'par_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'PAR_UTILS_MOD'
MODULE PAR_UTILS_MOD

IMPLICIT NONE

PRIVATE

! Whitelist of public symbols
PUBLIC :: PAR_CREATE_NAME
PUBLIC :: PAR_WOPEN
PUBLIC :: PAR_ROPEN
PUBLIC :: PAR_CLOSE
PUBLIC :: PAR_PRINT
PUBLIC :: PAR_WRITE
PUBLIC :: PAR_READ

PUBLIC :: PAR_SERIALIZE
PUBLIC :: PAR_DESERIALIZE

CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PAR_CREATE_NAME'
SUBROUTINE PAR_CREATE_NAME( DIRECTORY, PROC_ID, PARFNAME )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)  :: DIRECTORY
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: PROC_ID
  CHARACTER(LEN=*),     INTENT(OUT) :: PARFNAME

  ! Local variables
  LOGICAL :: VALEXIST
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: M
  INTEGER(KIND=JPIB_K) :: STAT


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  N = LEN(PARFNAME)
  M = LEN_TRIM(DIRECTORY) + 17
  PP_DEBUG_CRITICAL_COND_THROW( M.GT.N, 1)

  ! Create the message name
  PARFNAME = REPEAT(' ',N)
  WRITE(PARFNAME,'(A,A,I8.8,A,I8.8,A)', IOSTAT=STAT) TRIM(ADJUSTL(DIRECTORY)), '/par_', PROC_ID, '.bin'
  PP_DEBUG_CRITICAL_COND_THROW(STAT.NE.0, 2)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Values file name variable too short' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create the par file name' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE PAR_CREATE_NAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PAR_WOPEN'
SUBROUTINE PAR_WOPEN( PARFNAME, PARUNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: PARFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: PARUNIT

  ! Local variables
  LOGICAL :: VALEXIST
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( FILE=TRIM(PARFNAME), EXIST=VALEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( VALEXIST, 1)

  ! Open the TOC file
  OPEN( NEWUNIT=PARUNIT, FILE=TRIM(PARFNAME), STATUS='REPLACE', ACCESS='STREAM', ACTION='WRITE', FORM='UNFORMATTED', IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Val file already exists' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to open par file' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE PAR_WOPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PAR_ROPEN'
SUBROUTINE PAR_ROPEN( PARFNAME, PARUNIT, BIG_ENDIAN_READ )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)  :: PARFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: PARUNIT
  LOGICAL,              INTENT(IN)  :: BIG_ENDIAN_READ

  ! Local variables
  LOGICAL :: PAREXIST
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( FILE=TRIM(PARFNAME), EXIST=PAREXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.PAREXIST, 1)

  ! Open the TOC file
  IF ( BIG_ENDIAN_READ ) THEN
    OPEN( NEWUNIT=PARUNIT, FILE=TRIM(PARFNAME), STATUS='OLD', ACCESS='STREAM', ACTION='READ', FORM='UNFORMATTED', CONVERT='BIG_ENDIAN', IOSTAT=STAT )
  ELSE
    OPEN( NEWUNIT=PARUNIT, FILE=TRIM(PARFNAME), STATUS='OLD', ACCESS='STREAM', ACTION='READ', FORM='UNFORMATTED', IOSTAT=STAT )
  ENDIF
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to find par file: '//TRIM(PARFNAME) )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to open par file: '//TRIM(PARFNAME) )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE PAR_ROPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PAR_CLOSE'
SUBROUTINE PAR_CLOSE( PARUNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: PARUNIT

  ! Local variables
  LOGICAL :: PAROPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=PARUNIT, OPENED=PAROPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.PAROPENED, 1)

  ! Open the TOC file
  CLOSE( UNIT=PARUNIT, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unit not connected to a par file' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error closing par file' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE PAR_CLOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PAR_SERIALIZE'
SUBROUTINE PAR_SERIALIZE( DATA_RAW, DATA_SERIAL )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_SERIAL_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),        INTENT(IN)  :: DATA_RAW
  TYPE(MODEL_PAR_SERIAL_T), INTENT(OUT) :: DATA_SERIAL

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Compute the size of the arrays
  CALL COMPUTE_SIZE( DATA_RAW, DATA_SERIAL%DIMS_)

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED( DATA_SERIAL%IBUF_ ), 1 )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED( DATA_SERIAL%RBUF_ ), 2 )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED( DATA_SERIAL%CBUF_ ), 3 )

  ! Allocate arrays
  ALLOCATE( DATA_SERIAL%IBUF_(DATA_SERIAL%DIMS_(1)), STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4 )
  ALLOCATE( DATA_SERIAL%RBUF_(DATA_SERIAL%DIMS_(2)), STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 5 )
  ALLOCATE( DATA_SERIAL%CBUF_(DATA_SERIAL%DIMS_(3)), STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 6 )

  ! Fill serialized arrays
  CALL SERIALIZE_INTEGER_PARAMS( DATA_RAW, DATA_SERIAL%IBUF_ )

  CALL SERIALIZE_REAL_PARAMS(    DATA_RAW, DATA_SERIAL%RBUF_ )

  CALL SERIALIZE_STRING_PARAMS(  DATA_RAW, DATA_SERIAL%CBUF_ )

  ! Paranoid check
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG)
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Integer buffer already allocated' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Real buffer already allocated' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'String buffer already allocated' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate integer buffer: '//TRIM(ADJUSTL(ERRMSG)) )
      DEALLOCATE(ERRMSG)
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate real buffer: '//TRIM(ADJUSTL(ERRMSG)) )
      DEALLOCATE(ERRMSG)
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate string buffer: '//TRIM(ADJUSTL(ERRMSG)) )
      DEALLOCATE(ERRMSG)
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE PAR_SERIALIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PAR_DESERIALIZE'
SUBROUTINE PAR_DESERIALIZE( DATA_SERIAL, DATA_RAW )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_SERIAL_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_SERIAL_T), INTENT(IN)  :: DATA_SERIAL
  TYPE(MODEL_PAR_T),        INTENT(OUT) :: DATA_RAW

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_SERIAL%IBUF_ ), 1 )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_SERIAL%RBUF_ ), 2 )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_SERIAL%CBUF_ ), 3 )

  ! Fill serialized arrays
  CALL DESERIALIZE_INTEGER_PARAMS( DATA_SERIAL%IBUF_, DATA_SERIAL%DIMS_, DATA_RAW )

  CALL DESERIALIZE_REAL_PARAMS(    DATA_SERIAL%RBUF_, DATA_SERIAL%DIMS_, DATA_RAW )

  CALL DESERIALIZE_STRING_PARAMS(  DATA_SERIAL%CBUF_, DATA_SERIAL%DIMS_, DATA_RAW )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Integer buffer not allocated' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Real buffer not allocated' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'String buffer not allocated' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE PAR_DESERIALIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'COMPUTE_SIZE'
SUBROUTINE COMPUTE_SIZE( DATA_RAW, SZ )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),                   INTENT(IN)  :: DATA_RAW
  INTEGER(KIND=JPIB_K), DIMENSION(44), INTENT(OUT) :: SZ


  ! Function result
  INTEGER(KIND=JPIB_K), PARAMETER :: N_LOGICAL_SCALAR=9
  INTEGER(KIND=JPIB_K), PARAMETER :: N_INTEGER_SCALAR=70
  INTEGER(KIND=JPIB_K), PARAMETER :: N_REAL_SCALAR=19
  INTEGER(KIND=JPIB_K), PARAMETER :: N_STRING_SCALAR=9

  INTEGER(KIND=JPIB_K), PARAMETER :: N_INTEGER_ARRAY=7
  INTEGER(KIND=JPIB_K), PARAMETER :: N_REAL_ARRAY=4
  INTEGER(KIND=JPIB_K), PARAMETER :: N_STRING_ARRAY=0

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  SZ(4) = N_LOGICAL_SCALAR+N_INTEGER_SCALAR
  SZ(5) = N_REAL_SCALAR
  SZ(6) = N_STRING_SCALAR

  SZ(7) = N_INTEGER_ARRAY
  SZ(8) = N_REAL_ARRAY
  SZ(9) = N_STRING_ARRAY

  ! NSFLEVS
  IF ( ALLOCATED(DATA_RAW%GEO_%NSFLEVS) ) THEN
    SZ(10) = SIZE(DATA_RAW%GEO_%NSFLEVS)
    SZ(11) = LBOUND(DATA_RAW%GEO_%NSFLEVS,1)
    SZ(12) = UBOUND(DATA_RAW%GEO_%NSFLEVS,1)
    SZ(13) = LBOUND(DATA_RAW%GEO_%NSFLEVS,2)
    SZ(14) = UBOUND(DATA_RAW%GEO_%NSFLEVS,2)
  ELSE
    SZ(10) = INT(0,JPIB_K)
    SZ(11) = INT(-99,JPIB_K)
    SZ(12) = INT(-99,JPIB_K)
    SZ(13) = INT(-99,JPIB_K)
    SZ(14) = INT(-99,JPIB_K)
  ENDIF


  ! ILOENG
  IF ( ALLOCATED(DATA_RAW%GEO_%ILOENG) ) THEN
    SZ(15) = SIZE(DATA_RAW%GEO_%ILOENG)
    SZ(16) = LBOUND(DATA_RAW%GEO_%ILOENG,1)
    SZ(17) = UBOUND(DATA_RAW%GEO_%ILOENG,1)
  ELSE
    SZ(15) = INT(0,JPIB_K)
    SZ(16) = INT(-99,JPIB_K)
    SZ(17) = INT(-99,JPIB_K)
  ENDIF


  ! NLONRGG
  IF ( ALLOCATED(DATA_RAW%WAM_%NLONRGG) ) THEN
    SZ(18) = SIZE(DATA_RAW%WAM_%NLONRGG)
    SZ(19) = LBOUND(DATA_RAW%WAM_%NLONRGG,1)
    SZ(20) = UBOUND(DATA_RAW%WAM_%NLONRGG,1)
  ELSE
    SZ(18) = INT(0,JPIB_K)
    SZ(19) = INT(-99,JPIB_K)
    SZ(20) = INT(-99,JPIB_K)
  ENDIF

  ! MSERIES
  IF ( ALLOCATED(DATA_RAW%SAT_%MSERIES) ) THEN
    SZ(21) = SIZE(DATA_RAW%SAT_%MSERIES)
    SZ(22) = LBOUND(DATA_RAW%SAT_%MSERIES,1)
    SZ(23) = UBOUND(DATA_RAW%SAT_%MSERIES,1)
  ELSE
    SZ(22) = INT(0,JPIB_K)
    SZ(22) = INT(-99,JPIB_K)
    SZ(23) = INT(-99,JPIB_K)
  ENDIF

  ! MSATID
  IF ( ALLOCATED(DATA_RAW%SAT_%MSATID) ) THEN
    SZ(24) = SIZE(DATA_RAW%SAT_%MSATID)
    SZ(25) = LBOUND(DATA_RAW%SAT_%MSATID,1)
    SZ(26) = UBOUND(DATA_RAW%SAT_%MSATID,1)
  ELSE
    SZ(24) = INT(0,JPIB_K)
    SZ(25) = INT(-99,JPIB_K)
    SZ(26) = INT(-99,JPIB_K)
  ENDIF

  ! MINST
  IF ( ALLOCATED(DATA_RAW%SAT_%MINST) ) THEN
    SZ(27) = SIZE(DATA_RAW%SAT_%MINST)
    SZ(28) = LBOUND(DATA_RAW%SAT_%MINST,1)
    SZ(29) = UBOUND(DATA_RAW%SAT_%MINST,1)
  ELSE
    SZ(17) = INT(0,JPIB_K)
    SZ(28) = INT(-99,JPIB_K)
    SZ(29) = INT(-99,JPIB_K)
  ENDIF

  ! MCHAN
  IF ( ALLOCATED(DATA_RAW%SAT_%MCHAN) ) THEN
    SZ(30) = SIZE(DATA_RAW%SAT_%MCHAN)
    SZ(31) = LBOUND(DATA_RAW%SAT_%MCHAN,1)
    SZ(32) = UBOUND(DATA_RAW%SAT_%MCHAN,1)
  ELSE
    SZ(30) = INT(0,JPIB_K)
    SZ(31) = INT(-99,JPIB_K)
    SZ(32) = INT(-99,JPIB_K)
  ENDIF


  ! ZVERT
  IF ( ALLOCATED(DATA_RAW%GEO_%ZVERT) ) THEN
    SZ(33) = SIZE(DATA_RAW%GEO_%ZVERT)
    SZ(34) = LBOUND(DATA_RAW%GEO_%ZVERT,1)
    SZ(35) = UBOUND(DATA_RAW%GEO_%ZVERT,1)
  ELSE
    SZ(33) = INT(0,JPIB_K)
    SZ(34) = INT(-99,JPIB_K)
    SZ(35) = INT(-99,JPIB_K)
  ENDIF

  ! FR
  IF ( ALLOCATED(DATA_RAW%WAM_%FR) ) THEN
    SZ(36) = SIZE(DATA_RAW%WAM_%FR)
    SZ(37) = LBOUND(DATA_RAW%WAM_%FR,1)
    SZ(38) = UBOUND(DATA_RAW%WAM_%FR,1)
  ELSE
    SZ(36) = INT(0,JPIB_K)
    SZ(37) = INT(-99,JPIB_K)
    SZ(38) = INT(-99,JPIB_K)
  ENDIF

  ! TH
  IF ( ALLOCATED(DATA_RAW%WAM_%TH) ) THEN
    SZ(39) = SIZE(DATA_RAW%WAM_%TH)
    SZ(40) = LBOUND(DATA_RAW%WAM_%TH,1)
    SZ(41) = UBOUND(DATA_RAW%WAM_%TH,1)
  ELSE
    SZ(39) = INT(0,JPIB_K)
    SZ(40) = INT(-99,JPIB_K)
    SZ(41) = INT(-99,JPIB_K)
  ENDIF

  ! RCWM
  IF ( ALLOCATED(DATA_RAW%SAT_%RCWN) ) THEN
    SZ(42) = SIZE(DATA_RAW%SAT_%RCWN)
    SZ(43) = LBOUND(DATA_RAW%SAT_%RCWN,1)
    SZ(44) = UBOUND(DATA_RAW%SAT_%RCWN,1)
  ELSE
    SZ(42) = INT(0,JPIB_K)
    SZ(43) = INT(-99,JPIB_K)
    SZ(44) = INT(-99,JPIB_K)
  ENDIF

  ! Total size of the arrys to be sent
  SZ(1) = SZ(4) + SZ(10) + SZ(15) + SZ(18) + SZ(21) + SZ(24) + SZ(27) + SZ(30)
  SZ(2) = SZ(5) + SZ(33) + SZ(36) + SZ(39) + SZ(42)
  SZ(3) = SZ(6)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE COMPUTE_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SERIALIZE_INTEGER_PARAMS'
SUBROUTINE SERIALIZE_INTEGER_PARAMS( DATA_RAW, BUF )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),                  INTENT(IN)  :: DATA_RAW
  INTEGER(KIND=JPIB_K), DIMENSION(:), INTENT(OUT) :: BUF

  ! Local variables
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_RAW%GEO_%ILOENG ), 2 )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_RAW%GEO_%NSFLEVS ), 3 )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_RAW%WAM_%NLONRGG ), 4 )

  ! Initialize the buffer
  DO I = 1, SIZE(BUF)
    BUF(I) = -999999_JPIB_K
  ENDDO

  ! Fill the buffer
  BUF( 1) = INT( DATA_RAW%SIM_%IGRIB2_TABLES_VERSION_LATEST, JPIB_K )
  BUF( 2) = INT( DATA_RAW%SIM_%NCYCLE, JPIB_K )
  BUF( 3) = INT( DATA_RAW%SIM_%NLOCGRB, JPIB_K )
  BUF( 4) = INT( DATA_RAW%SIM_%NBITSSHLNSP, JPIB_K )
  BUF( 5) = INT( DATA_RAW%SIM_%NBITSEXPR, JPIB_K )
  BUF( 6) = INT( DATA_RAW%SIM_%NLEG, JPIB_K )
  BUF( 7) = INT( DATA_RAW%SIM_%NTOTENS, JPIB_K )
  BUF( 8) = INT( DATA_RAW%SIM_%NENSFNB, JPIB_K )
  BUF( 9) = INT( DATA_RAW%SIM_%NWINOFF, JPIB_K )
  BUF(10) = INT( DATA_RAW%SIM_%NJDIAG, JPIB_K )
  BUF(11) = INT( DATA_RAW%SIM_%NJDOMAI, JPIB_K )
  BUF(12) = INT( DATA_RAW%SIM_%NJITER, JPIB_K )
  BUF(13) = INT( DATA_RAW%SIM_%NSTREAM, JPIB_K )
  BUF(14) = INT( DATA_RAW%SIM_%NSYSTEM, JPIB_K )
  BUF(15) = INT( DATA_RAW%SIM_%NMETHOD, JPIB_K )
  BUF(16) = INT( DATA_RAW%SIM_%NREFERENCE, JPIB_K )
  BUF(17) = INT( DATA_RAW%SIM_%NCONSENSUS, JPIB_K )
  BUF(18) = INT( DATA_RAW%SIM_%NDWD, JPIB_K )
  BUF(19) = INT( DATA_RAW%SIM_%NMFR, JPIB_K )
  BUF(20) = INT( DATA_RAW%SIM_%NNCEP, JPIB_K )
  BUF(21) = INT( DATA_RAW%SIM_%NUKM, JPIB_K )
  BUF(22) = INT( DATA_RAW%SIM_%NINDAT, JPIB_K )
  BUF(23) = INT( DATA_RAW%SIM_%NSSSSS, JPIB_K )
  BUF(24) = INT( DATA_RAW%SIM_%NCONF, JPIB_K )
  BUF(25) = INT( DATA_RAW%SIM_%NSTEPINI, JPIB_K )
  BUF(26) = INT( DATA_RAW%SIM_%NFCHO_TRUNC_INI, JPIB_K )
  BUF(27) = INT( DATA_RAW%GEO_%JPMXLE, JPIB_K )
  BUF(28) = INT( DATA_RAW%GEO_%JPMXGL, JPIB_K )
  BUF(29) = INT( DATA_RAW%GEO_%ISMAX, JPIB_K )
  BUF(30) = INT( DATA_RAW%GEO_%ILATS, JPIB_K )
  BUF(31) = INT( DATA_RAW%GEO_%ILONS, JPIB_K )
  BUF(32) = INT( DATA_RAW%GEO_%IDGNH, JPIB_K )
  BUF(33) = INT( DATA_RAW%GEO_%IFLEV, JPIB_K )
  BUF(34) = INT( DATA_RAW%GEO_%NHTYP, JPIB_K )
  BUF(35) = INT( DATA_RAW%GEO_%NSTTYP, JPIB_K )
  BUF(36) = INT( DATA_RAW%WAM_%NGX, JPIB_K )
  BUF(37) = INT( DATA_RAW%WAM_%NGY, JPIB_K )
  BUF(38) = INT( DATA_RAW%WAM_%NANG, JPIB_K )
  BUF(39) = INT( DATA_RAW%WAM_%NFRE_RED, JPIB_K )
  BUF(40) = INT( DATA_RAW%WAM_%IMDLGRBID_G, JPIB_K )
  BUF(41) = INT( DATA_RAW%WAM_%IMDLGRBID_M, JPIB_K )
  BUF(42) = INT( DATA_RAW%WAM_%NDATE_TIME_WINDOW_END, JPIB_K )
  BUF(43) = INT( DATA_RAW%WAM_%NWINOFF, JPIB_K )
  BUF(44) = INT( DATA_RAW%WAM_%NGRIB_VERSION, JPIB_K )
  BUF(45) = INT( DATA_RAW%WAM_%NTENCODE, JPIB_K )
  BUF(46) = INT( DATA_RAW%WAM_%NGRBRESI, JPIB_K )
  BUF(47) = INT( DATA_RAW%WAM_%NGRBRESS, JPIB_K )
  BUF(48) = INT( DATA_RAW%WAM_%IRGG, JPIB_K )
  BUF(49) = INT( DATA_RAW%WAM_%IQGAUSS, JPIB_K )
  BUF(50) = INT( DATA_RAW%WAM_%NENSFNB, JPIB_K )
  BUF(51) = INT( DATA_RAW%WAM_%NTOTENS, JPIB_K )
  BUF(52) = INT( DATA_RAW%WAM_%NSYSNB, JPIB_K )
  BUF(53) = INT( DATA_RAW%WAM_%NMETNB, JPIB_K )
  BUF(54) = INT( DATA_RAW%WAM_%ISTREAM, JPIB_K )
  BUF(55) = INT( DATA_RAW%WAM_%NLOCGRB, JPIB_K )
  BUF(56) = INT( DATA_RAW%WAM_%NCONSENSUS, JPIB_K )
  BUF(57) = INT( DATA_RAW%WAM_%NDWD, JPIB_K )
  BUF(58) = INT( DATA_RAW%WAM_%NMFR, JPIB_K )
  BUF(59) = INT( DATA_RAW%WAM_%NNCEP, JPIB_K )
  BUF(60) = INT( DATA_RAW%WAM_%NUKM, JPIB_K )
  BUF(61) = INT( DATA_RAW%WAM_%IREFDATE, JPIB_K )
  BUF(62) = INT( DATA_RAW%WAM_%NSPEC2TAB, JPIB_K )
  BUF(63) = INT( DATA_RAW%WAM_%NSPEC2TMPD, JPIB_K )
  BUF(64) = INT( DATA_RAW%WAM_%NSPEC2TMPP, JPIB_K )
  BUF(65) = INT( DATA_RAW%WAM_%NTRG2TMPD, JPIB_K )
  BUF(66) = INT( DATA_RAW%WAM_%NTRG2TMPP, JPIB_K )
  BUF(67) = INT( DATA_RAW%WAM_%ITMIN, JPIB_K )
  BUF(68) = INT( DATA_RAW%WAM_%ITMAX, JPIB_K )
  BUF(69) = INT( DATA_RAW%SAT_%NSATSIM, JPIB_K )
  BUF(70) = INT( DATA_RAW%SIM_%NPROC_IO, JPIB_K )
  BUF(71) = L2I( DATA_RAW%SIM_%LPPSTEPS )
  BUF(72) = L2I( DATA_RAW%SIM_%LOBSC1 )
  BUF(73) = L2I( DATA_RAW%SIM_%LVAREPS )
  BUF(74) = L2I( DATA_RAW%SIM_%LDMCC04 )
  BUF(75) = L2I( DATA_RAW%WAM_%LWCOUSAMEGRID )
  BUF(76) = L2I( DATA_RAW%WAM_%LGRHDIFS )
  BUF(77) = L2I( DATA_RAW%WAM_%LNEWLVTP )
  BUF(78) = L2I( DATA_RAW%WAM_%LPADPOLES )
  BUF(79) = L2I( DATA_RAW%WAM_%LL_GRID_SIMPLE_MATRIX )
  CNT = 79

  IF ( ALLOCATED(DATA_RAW%GEO_%ILOENG) ) THEN
    DO I = LBOUND(DATA_RAW%GEO_%ILOENG,1), UBOUND(DATA_RAW%GEO_%ILOENG,1)
      CNT = CNT + 1
      BUF(CNT) = INT( DATA_RAW%GEO_%ILOENG(I), JPIB_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%GEO_%NSFLEVS) ) THEN
    DO I = LBOUND(DATA_RAW%GEO_%NSFLEVS,1), UBOUND(DATA_RAW%GEO_%NSFLEVS,1)
      DO J = LBOUND(DATA_RAW%GEO_%NSFLEVS,2), UBOUND(DATA_RAW%GEO_%NSFLEVS,2)
        CNT = CNT + 1
        BUF(CNT) = INT( DATA_RAW%GEO_%NSFLEVS(I,J), JPIB_K )
      ENDDO
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%WAM_%NLONRGG) ) THEN
    DO I = LBOUND(DATA_RAW%WAM_%NLONRGG,1), UBOUND(DATA_RAW%WAM_%NLONRGG,1)
      CNT = CNT + 1
      BUF(CNT) = INT( DATA_RAW%WAM_%NLONRGG(I), JPIB_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%SAT_%MSERIES) ) THEN
    DO I = LBOUND(DATA_RAW%SAT_%MSERIES,1), UBOUND(DATA_RAW%SAT_%MSERIES,1)
      CNT = CNT + 1
      BUF(CNT) = INT( DATA_RAW%SAT_%MSERIES(I), JPIB_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%SAT_%MSATID) ) THEN
    DO I = LBOUND(DATA_RAW%SAT_%MSATID,1), UBOUND(DATA_RAW%SAT_%MSATID,1)
      CNT = CNT + 1
      BUF(CNT) = INT( DATA_RAW%SAT_%MSATID(I), JPIB_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%SAT_%MINST) ) THEN
    DO I = LBOUND(DATA_RAW%SAT_%MINST,1), UBOUND(DATA_RAW%SAT_%MINST,1)
      CNT = CNT + 1
      BUF(CNT) = INT( DATA_RAW%SAT_%MINST(I), JPIB_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%SAT_%MCHAN) ) THEN
    DO I = LBOUND(DATA_RAW%SAT_%MCHAN,1), UBOUND(DATA_RAW%SAT_%MCHAN,1)
      CNT = CNT + 1
      BUF(CNT) = INT( DATA_RAW%SAT_%MCHAN(I), JPIB_K )
    ENDDO
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

  ! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Wrong size of the buffer' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'ILOENG array not allocated' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'NSFLEVS array not allocated' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'NLONRGG array not allocated' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE SERIALIZE_INTEGER_PARAMS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SERIALIZE_REAL_PARAMS'
SUBROUTINE SERIALIZE_REAL_PARAMS( DATA_RAW, BUF )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPRD_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)  :: DATA_RAW
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(OUT) :: BUF

  ! Local variables
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_RAW%GEO_%ZVERT ), 2 )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_RAW%WAM_%FR ), 3 )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_RAW%WAM_%TH ), 4 )

  ! Initialize the buffer
  DO I = 1, SIZE(BUF)
    BUF(I) = -999999.9999_JPRD_K
  ENDDO

  ! Fill the buffer
  BUF( 1) = REAL( DATA_RAW%SIM_%RTIMST, JPRD_K )
  BUF( 2) = REAL( DATA_RAW%SIM_%TSTEP, JPRD_K )
  BUF( 3) = REAL( DATA_RAW%GEO_%ZNLAT, JPRD_K )
  BUF( 4) = REAL( DATA_RAW%GEO_%ZSLAT, JPRD_K )
  BUF( 5) = REAL( DATA_RAW%GEO_%RMUCEN, JPRD_K )
  BUF( 6) = REAL( DATA_RAW%GEO_%RLOCEN, JPRD_K )
  BUF( 7) = REAL( DATA_RAW%GEO_%RSTRET, JPRD_K )
  BUF( 8) = REAL( DATA_RAW%WAM_%PPMISS, JPRD_K )
  BUF( 9) = REAL( DATA_RAW%WAM_%PPEPS, JPRD_K )
  BUF(10) = REAL( DATA_RAW%WAM_%PPREC, JPRD_K )
  BUF(11) = REAL( DATA_RAW%WAM_%PPRESOL, JPRD_K )
  BUF(12) = REAL( DATA_RAW%WAM_%PPMIN_RESET, JPRD_K )
  BUF(13) = REAL( DATA_RAW%WAM_%AMOWEP, JPRD_K )
  BUF(14) = REAL( DATA_RAW%WAM_%AMOSOP, JPRD_K )
  BUF(15) = REAL( DATA_RAW%WAM_%AMOEAP, JPRD_K )
  BUF(16) = REAL( DATA_RAW%WAM_%AMONOP, JPRD_K )
  BUF(17) = REAL( DATA_RAW%WAM_%XDELLA, JPRD_K )
  BUF(18) = REAL( DATA_RAW%WAM_%XDELLO, JPRD_K )
  BUF(19) = REAL( DATA_RAW%WAM_%ZMISS, JPRD_K )
  CNT = 19

  IF ( ALLOCATED(DATA_RAW%GEO_%ZVERT) ) THEN
    DO I = LBOUND(DATA_RAW%GEO_%ZVERT,1), UBOUND(DATA_RAW%GEO_%ZVERT,1)
      CNT = CNT + 1
      BUF(CNT) = REAL( DATA_RAW%GEO_%ZVERT(I), JPRD_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%WAM_%FR) ) THEN
    DO I = LBOUND(DATA_RAW%WAM_%FR,1), UBOUND(DATA_RAW%WAM_%FR,1)
      CNT = CNT + 1
      BUF(CNT) = REAL( DATA_RAW%WAM_%FR(I), JPRD_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%WAM_%TH) ) THEN
    DO I = LBOUND(DATA_RAW%WAM_%TH,1), UBOUND(DATA_RAW%WAM_%TH,1)
      CNT = CNT + 1
      BUF(CNT) = REAL( DATA_RAW%WAM_%TH(I), JPRD_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%SAT_%RCWN) ) THEN
    DO I = LBOUND(DATA_RAW%SAT_%RCWN,1), UBOUND(DATA_RAW%SAT_%RCWN,1)
      CNT = CNT + 1
      BUF(CNT) = REAL( DATA_RAW%SAT_%RCWN(I), JPRD_K )
    ENDDO
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

  ! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Wrong size of the buffer' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'ZVERT array not allocated' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'FR array not allocated' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'TH array not allocated' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE SERIALIZE_REAL_PARAMS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SERIALIZE_STRING_PARAMS'
SUBROUTINE SERIALIZE_STRING_PARAMS( DATA_RAW, BUF )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPRD_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)  :: DATA_RAW
  CHARACTER(LEN=16), DIMENSION(:), INTENT(OUT) :: BUF

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialize the buffer
  DO I = 1, SIZE(BUF)
    BUF(I) = REPEAT(' ',16)
  ENDDO

  ! Fill the buffer
  BUF( 1)(1:2)  = DATA_RAW%SIM_%CTYPE(1:2)
  BUF( 2)(1:2)  = DATA_RAW%SIM_%CFCLASS(1:2)
  BUF( 3)(1:16) = DATA_RAW%SIM_%CNMEXP(1:16)
  BUF( 4)(1:1)  = DATA_RAW%WAM_%CLDOMAIN(1:1)
  BUF( 5)(1:1)  = DATA_RAW%WAM_%HOPERI(1:1)
  BUF( 6)(1:1)  = DATA_RAW%WAM_%HOPERS(1:1)
  BUF( 7)(1:2)  = DATA_RAW%WAM_%MARSTYPE(1:2)
  BUF( 8)(1:2)  = DATA_RAW%WAM_%YCLASS(1:2)
  BUF( 9)(1:3)  = DATA_RAW%WAM_%YEXPVER(1:3)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

  ! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Wrong size of the buffer' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE SERIALIZE_STRING_PARAMS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'DESERIALIZE_INTEGER_PARAMS'
SUBROUTINE DESERIALIZE_INTEGER_PARAMS( BUF, SZ, DATA_RAW )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), DIMENSION(:),  INTENT(IN)    :: BUF
  INTEGER(KIND=JPIB_K), DIMENSION(44), INTENT(IN)    :: SZ
  TYPE(MODEL_PAR_T),                   INTENT(INOUT) :: DATA_RAW

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: LO1
  INTEGER(KIND=JPIB_K) :: LO2
  INTEGER(KIND=JPIB_K) :: HI1
  INTEGER(KIND=JPIB_K) :: HI2

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Free memory if allocated
  IF ( ALLOCATED(DATA_RAW%GEO_%ILOENG) ) THEN
    DEALLOCATE(DATA_RAW%GEO_%ILOENG)
  ENDIF
  IF ( ALLOCATED(DATA_RAW%GEO_%NSFLEVS) ) THEN
    DEALLOCATE(DATA_RAW%GEO_%NSFLEVS)
  ENDIF
  IF ( ALLOCATED(DATA_RAW%WAM_%NLONRGG) ) THEN
    DEALLOCATE(DATA_RAW%WAM_%NLONRGG)
  ENDIF

  ! Fill the buffer
  DATA_RAW%SIM_%IGRIB2_TABLES_VERSION_LATEST = INT( BUF(1),  JPIB_K )
  DATA_RAW%SIM_%NCYCLE                       = INT( BUF(2),  JPIB_K )
  DATA_RAW%SIM_%NLOCGRB                      = INT( BUF(3),  JPIB_K )
  DATA_RAW%SIM_%NBITSSHLNSP                  = INT( BUF(4),  JPIB_K )
  DATA_RAW%SIM_%NBITSEXPR                    = INT( BUF(5),  JPIB_K )
  DATA_RAW%SIM_%NLEG                         = INT( BUF(6),  JPIB_K )
  DATA_RAW%SIM_%NTOTENS                      = INT( BUF(7),  JPIB_K )
  DATA_RAW%SIM_%NENSFNB                      = INT( BUF(8),  JPIB_K )
  DATA_RAW%SIM_%NWINOFF                      = INT( BUF(9),  JPIB_K )
  DATA_RAW%SIM_%NJDIAG                       = INT( BUF(10), JPIB_K )
  DATA_RAW%SIM_%NJDOMAI                      = INT( BUF(11), JPIB_K )
  DATA_RAW%SIM_%NJITER                       = INT( BUF(12), JPIB_K )
  DATA_RAW%SIM_%NSTREAM                      = INT( BUF(13), JPIB_K )
  DATA_RAW%SIM_%NSYSTEM                      = INT( BUF(14), JPIB_K )
  DATA_RAW%SIM_%NMETHOD                      = INT( BUF(15), JPIB_K )
  DATA_RAW%SIM_%NREFERENCE                   = INT( BUF(16), JPIB_K )
  DATA_RAW%SIM_%NCONSENSUS                   = INT( BUF(17), JPIB_K )
  DATA_RAW%SIM_%NDWD                         = INT( BUF(18), JPIB_K )
  DATA_RAW%SIM_%NMFR                         = INT( BUF(19), JPIB_K )
  DATA_RAW%SIM_%NNCEP                        = INT( BUF(20), JPIB_K )
  DATA_RAW%SIM_%NUKM                         = INT( BUF(21), JPIB_K )
  DATA_RAW%SIM_%NINDAT                       = INT( BUF(22), JPIB_K )
  DATA_RAW%SIM_%NSSSSS                       = INT( BUF(23), JPIB_K )
  DATA_RAW%SIM_%NCONF                        = INT( BUF(24), JPIB_K )
  DATA_RAW%SIM_%NSTEPINI                     = INT( BUF(25), JPIB_K )
  DATA_RAW%SIM_%NFCHO_TRUNC_INI              = INT( BUF(26), JPIB_K )
  DATA_RAW%GEO_%JPMXLE                       = INT( BUF(27), JPIB_K )
  DATA_RAW%GEO_%JPMXGL                       = INT( BUF(28), JPIB_K )
  DATA_RAW%GEO_%ISMAX                        = INT( BUF(29), JPIB_K )
  DATA_RAW%GEO_%ILATS                        = INT( BUF(30), JPIB_K )
  DATA_RAW%GEO_%ILONS                        = INT( BUF(31), JPIB_K )
  DATA_RAW%GEO_%IDGNH                        = INT( BUF(32), JPIB_K )
  DATA_RAW%GEO_%IFLEV                        = INT( BUF(33), JPIB_K )
  DATA_RAW%GEO_%NHTYP                        = INT( BUF(34), JPIB_K )
  DATA_RAW%GEO_%NSTTYP                       = INT( BUF(35), JPIB_K )
  DATA_RAW%WAM_%NGX                          = INT( BUF(36), JPIB_K )
  DATA_RAW%WAM_%NGY                          = INT( BUF(37), JPIB_K )
  DATA_RAW%WAM_%NANG                         = INT( BUF(38), JPIB_K )
  DATA_RAW%WAM_%NFRE_RED                     = INT( BUF(39), JPIB_K )
  DATA_RAW%WAM_%IMDLGRBID_G                  = INT( BUF(40), JPIB_K )
  DATA_RAW%WAM_%IMDLGRBID_M                  = INT( BUF(41), JPIB_K )
  DATA_RAW%WAM_%NDATE_TIME_WINDOW_END        = INT( BUF(42), JPIB_K )
  DATA_RAW%WAM_%NWINOFF                      = INT( BUF(43), JPIB_K )
  DATA_RAW%WAM_%NGRIB_VERSION                = INT( BUF(44), JPIB_K )
  DATA_RAW%WAM_%NTENCODE                     = INT( BUF(45), JPIB_K )
  DATA_RAW%WAM_%NGRBRESI                     = INT( BUF(46), JPIB_K )
  DATA_RAW%WAM_%NGRBRESS                     = INT( BUF(47), JPIB_K )
  DATA_RAW%WAM_%IRGG                         = INT( BUF(48), JPIB_K )
  DATA_RAW%WAM_%IQGAUSS                      = INT( BUF(49), JPIB_K )
  DATA_RAW%WAM_%NENSFNB                      = INT( BUF(50), JPIB_K )
  DATA_RAW%WAM_%NTOTENS                      = INT( BUF(51), JPIB_K )
  DATA_RAW%WAM_%NSYSNB                       = INT( BUF(52), JPIB_K )
  DATA_RAW%WAM_%NMETNB                       = INT( BUF(53), JPIB_K )
  DATA_RAW%WAM_%ISTREAM                      = INT( BUF(54), JPIB_K )
  DATA_RAW%WAM_%NLOCGRB                      = INT( BUF(55), JPIB_K )
  DATA_RAW%WAM_%NCONSENSUS                   = INT( BUF(56), JPIB_K )
  DATA_RAW%WAM_%NDWD                         = INT( BUF(57), JPIB_K )
  DATA_RAW%WAM_%NMFR                         = INT( BUF(58), JPIB_K )
  DATA_RAW%WAM_%NNCEP                        = INT( BUF(59), JPIB_K )
  DATA_RAW%WAM_%NUKM                         = INT( BUF(60), JPIB_K )
  DATA_RAW%WAM_%IREFDATE                     = INT( BUF(61), JPIB_K )
  DATA_RAW%WAM_%NSPEC2TAB                    = INT( BUF(62), JPIB_K )
  DATA_RAW%WAM_%NSPEC2TMPD                   = INT( BUF(63), JPIB_K )
  DATA_RAW%WAM_%NSPEC2TMPP                   = INT( BUF(64), JPIB_K )
  DATA_RAW%WAM_%NTRG2TMPD                    = INT( BUF(65), JPIB_K )
  DATA_RAW%WAM_%NTRG2TMPP                    = INT( BUF(66), JPIB_K )
  DATA_RAW%WAM_%ITMIN                        = INT( BUF(67), JPIB_K )
  DATA_RAW%WAM_%ITMAX                        = INT( BUF(68), JPIB_K )
  DATA_RAW%SAT_%NSATSIM                      = INT( BUF(69), JPIB_K )
  DATA_RAW%SIM_%NPROC_IO                     = INT( BUF(70), JPIB_K )
  DATA_RAW%SIM_%LPPSTEPS                     = I2L( BUF(71) )
  DATA_RAW%SIM_%LOBSC1                       = I2L( BUF(72) )
  DATA_RAW%SIM_%LVAREPS                      = I2L( BUF(73) )
  DATA_RAW%SIM_%LDMCC04                      = I2L( BUF(74) )
  DATA_RAW%WAM_%LWCOUSAMEGRID                = I2L( BUF(75) )
  DATA_RAW%WAM_%LGRHDIFS                     = I2L( BUF(76) )
  DATA_RAW%WAM_%LNEWLVTP                     = I2L( BUF(77) )
  DATA_RAW%WAM_%LPADPOLES                    = I2L( BUF(78) )
  DATA_RAW%WAM_%LL_GRID_SIMPLE_MATRIX        = I2L( BUF(79) )
  CNT = 79

  ! NSFLEVS
  IF ( SZ(10) .GT. 0 ) THEN
    LO1 = SZ(11)
    HI1 = SZ(12)
    LO2 = SZ(13)
    HI2 = SZ(14)
    ALLOCATE(DATA_RAW%GEO_%NSFLEVS(LO1:HI1,LO2:HI2), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2 )
    DO I = LO1, HI1
      DO J = LO2, HI2
        CNT = CNT + 1
        DATA_RAW%GEO_%NSFLEVS(I,J) = INT( BUF(CNT), JPIB_K )
      ENDDO
    ENDDO
  ENDIF

  ! ILOENG
  IF ( SZ(15) .GT. 0 ) THEN
    LO1 = SZ(16)
    HI1 = SZ(17)
    ALLOCATE(DATA_RAW%GEO_%ILOENG(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2 )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%GEO_%ILOENG(I) = INT( BUF(CNT), JPIB_K )
    ENDDO
  ENDIF

  ! NLONRGG
  IF ( SZ(18) .GT. 0 ) THEN
    LO1 = SZ(19)
    HI1 = SZ(20)
    ALLOCATE(DATA_RAW%WAM_%NLONRGG(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2 )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%WAM_%NLONRGG(I) = INT( BUF(CNT), JPIB_K )
    ENDDO
  ENDIF

  ! MSERIES
  IF ( SZ(21) .GT. 0 ) THEN
    LO1 = SZ(22)
    HI1 = SZ(23)
    ALLOCATE(DATA_RAW%SAT_%MSERIES(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2 )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%SAT_%MSERIES(I) = INT( BUF(CNT), JPIB_K )
    ENDDO
  ENDIF

  ! MSATID
  IF ( SZ(24) .GT. 0 ) THEN
    LO1 = SZ(25)
    HI1 = SZ(26)
    ALLOCATE(DATA_RAW%SAT_%MSATID(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2 )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%SAT_%MSATID(I) = INT( BUF(CNT), JPIB_K )
    ENDDO
  ENDIF

  ! MINST
  IF ( SZ(27) .GT. 0 ) THEN
    LO1 = SZ(28)
    HI1 = SZ(29)
    ALLOCATE(DATA_RAW%SAT_%MINST(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2 )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%SAT_%MINST(I) = INT( BUF(CNT), JPIB_K )
    ENDDO
  ENDIF

  ! MCHAN
  IF ( SZ(30) .GT. 0 ) THEN
    LO1 = SZ(31)
    HI1 = SZ(32)
    ALLOCATE(DATA_RAW%SAT_%MCHAN(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2 )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%SAT_%MCHAN(I) = INT( BUF(CNT), JPIB_K )
    ENDDO
  ENDIF

  ! Paranoid checks
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG)
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

  ! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (2)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate integer array'//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate integer array' )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE DESERIALIZE_INTEGER_PARAMS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'DESERIALIZE_REAL_PARAMS'
SUBROUTINE DESERIALIZE_REAL_PARAMS( BUF, SZ, DATA_RAW )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPRD_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  REAL(KIND=JPRD_K),    DIMENSION(:),  INTENT(IN)    :: BUF
  INTEGER(KIND=JPIB_K), DIMENSION(44), INTENT(IN)    :: SZ
  TYPE(MODEL_PAR_T),                   INTENT(INOUT) :: DATA_RAW

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: LO1
  INTEGER(KIND=JPIB_K) :: HI1

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Free memory if allocated
  IF ( ALLOCATED(DATA_RAW%GEO_%ZVERT) ) THEN
    DEALLOCATE(DATA_RAW%GEO_%ZVERT)
  ENDIF
  IF ( ALLOCATED(DATA_RAW%WAM_%FR) ) THEN
    DEALLOCATE(DATA_RAW%WAM_%FR)
  ENDIF
  IF ( ALLOCATED(DATA_RAW%WAM_%TH) ) THEN
    DEALLOCATE(DATA_RAW%WAM_%TH)
  ENDIF

  ! Fill the buffer
  DATA_RAW%SIM_%RTIMST      = REAL( BUF(1), JPRD_K )
  DATA_RAW%SIM_%TSTEP       = REAL( BUF(2), JPRD_K )
  DATA_RAW%GEO_%ZNLAT       = REAL( BUF(3), JPRD_K )
  DATA_RAW%GEO_%ZSLAT       = REAL( BUF(4), JPRD_K )
  DATA_RAW%GEO_%RMUCEN      = REAL( BUF(5), JPRD_K )
  DATA_RAW%GEO_%RLOCEN      = REAL( BUF(6), JPRD_K )
  DATA_RAW%GEO_%RSTRET      = REAL( BUF(7), JPRD_K )
  DATA_RAW%WAM_%PPMISS      = REAL( BUF(8), JPRD_K )
  DATA_RAW%WAM_%PPEPS       = REAL( BUF(9), JPRD_K )
  DATA_RAW%WAM_%PPREC       = REAL( BUF(10), JPRD_K )
  DATA_RAW%WAM_%PPRESOL     = REAL( BUF(11), JPRD_K )
  DATA_RAW%WAM_%PPMIN_RESET = REAL( BUF(12), JPRD_K )
  DATA_RAW%WAM_%AMOWEP      = REAL( BUF(13), JPRD_K )
  DATA_RAW%WAM_%AMOSOP      = REAL( BUF(14), JPRD_K )
  DATA_RAW%WAM_%AMOEAP      = REAL( BUF(15), JPRD_K )
  DATA_RAW%WAM_%AMONOP      = REAL( BUF(16), JPRD_K )
  DATA_RAW%WAM_%XDELLA      = REAL( BUF(17), JPRD_K )
  DATA_RAW%WAM_%XDELLO      = REAL( BUF(18), JPRD_K )
  DATA_RAW%WAM_%ZMISS       = REAL( BUF(19), JPRD_K )
  CNT = 19

  ! ZVERT
  IF ( SZ(33) .GT. 0 ) THEN
    LO1 = SZ(34)
    HI1 = SZ(35)
    ALLOCATE(DATA_RAW%GEO_%ZVERT(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2 )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%GEO_%ZVERT(I) = REAL( BUF(CNT), JPRD_K )
    ENDDO
  ENDIF

  ! FR
  IF ( SZ(36) .GT. 0 ) THEN
    LO1 = SZ(37)
    HI1 = SZ(38)
    ALLOCATE(DATA_RAW%WAM_%FR(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2 )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%WAM_%FR(I) = REAL( BUF(CNT), JPRD_K )
    ENDDO
  ENDIF

  ! TH
  IF ( SZ(39) .GT. 0 ) THEN
    LO1 = SZ(40)
    HI1 = SZ(41)
    ALLOCATE(DATA_RAW%WAM_%TH(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2 )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%WAM_%TH(I) = REAL( BUF(CNT), JPRD_K )
    ENDDO
  ENDIF

  ! TH
  IF ( SZ(42) .GT. 0 ) THEN
    LO1 = SZ(43)
    HI1 = SZ(44)
    ALLOCATE(DATA_RAW%SAT_%RCWN(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2 )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%SAT_%RCWN(I) = REAL( BUF(CNT), JPRD_K )
    ENDDO
  ENDIF

  ! Paranoid checks
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG)
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

  ! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Invalid bounds for "ZVERT"' )
    CASE (2)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate "ZVERT"'//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate "ZVERT"' )
      ENDIF
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Invalid bounds along direction 1 for "FR"' )
    CASE (4)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate "FR"'//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate "FR"' )
      ENDIF
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Invalid bounds for "TH"' )
    CASE (6)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate "TH"'//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate "TH"' )
      ENDIF
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Wrong size of "DATA_RAW"' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE DESERIALIZE_REAL_PARAMS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'DESERIALIZE_STRING_PARAMS'
SUBROUTINE DESERIALIZE_STRING_PARAMS( BUF, SZ, DATA_RAW )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=16),    DIMENSION(:),  INTENT(IN)    :: BUF
  INTEGER(KIND=JPIB_K), DIMENSION(44), INTENT(IN)    :: SZ
  TYPE(MODEL_PAR_T),                   INTENT(INOUT) :: DATA_RAW

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Fill the buffer
  DATA_RAW%SIM_%CTYPE(1:2)    = BUF( 1)(1:2)
  DATA_RAW%SIM_%CFCLASS(1:2)  = BUF( 2)(1:2)
  DATA_RAW%SIM_%CNMEXP(1:16)  = BUF( 3)(1:16)
  DATA_RAW%WAM_%CLDOMAIN(1:1) = BUF( 4)(1:1)
  DATA_RAW%WAM_%HOPERI(1:1)   = BUF( 5)(1:1)
  DATA_RAW%WAM_%HOPERS(1:1)   = BUF( 6)(1:1)
  DATA_RAW%WAM_%MARSTYPE(1:2) = BUF( 7)(1:2)
  DATA_RAW%WAM_%YCLASS(1:2)   = BUF( 8)(1:2)
  DATA_RAW%WAM_%YEXPVER(1:3)  = BUF( 9)(1:3)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

  ! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Wrong size of "DATA_RAW"' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE DESERIALIZE_STRING_PARAMS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PAR_WRITE'
SUBROUTINE PAR_WRITE( DATA, UNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),    INTENT(IN) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  CALL WRITE_SIM_PAR( DATA%SIM_, UNIT )
  CALL WRITE_GEO_PAR( DATA%GEO_, UNIT )
  CALL WRITE_WAM_PAR( DATA%WAM_, UNIT )
  CALL WRITE_SAT_PAR( DATA%SAT_, UNIT )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PAR_WRITE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PAR_PRINT'
SUBROUTINE PAR_PRINT( DATA, UNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),    INTENT(IN) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  CALL PRINT_SIM_PAR( DATA%SIM_, UNIT )
  CALL PRINT_GEO_PAR( DATA%GEO_, UNIT )
  CALL PRINT_WAM_PAR( DATA%WAM_, UNIT )
  CALL PRINT_SAT_PAR( DATA%SAT_, UNIT )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PAR_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PAR_READ'
SUBROUTINE PAR_READ( DATA, UNIT, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),    INTENT(INOUT) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: UNIT
  LOGICAL,              INTENT(IN)  :: VERBOSE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Call read procedures
  CALL READ_SIM_PAR( DATA%SIM_, UNIT, VERBOSE )
  CALL READ_GEO_PAR( DATA%GEO_, UNIT, VERBOSE )
  CALL READ_WAM_PAR( DATA%WAM_, UNIT, VERBOSE )
  CALL READ_SAT_PAR( DATA%SAT_, UNIT, VERBOSE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PAR_READ
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PRINT_SIM_PAR'
SUBROUTINE PRINT_SIM_PAR( DATA, UNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: SIM_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(SIM_PAR_T),      INTENT(IN) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  WRITE(UNIT,*) ' '
  WRITE(UNIT,*) ' SIMULATION PARAMETERS'
  WRITE(UNIT,*) ' ---------------------'
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%IGRIB2_TABLES_VERSION_LATEST :: ', DATA%IGRIB2_TABLES_VERSION_LATEST
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NCYCLE...................... :: ', DATA%NCYCLE
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NLOCGRB..................... :: ', DATA%NLOCGRB
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NBITSSHLNSP................. :: ', DATA%NBITSSHLNSP
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NBITSEXPR................... :: ', DATA%NBITSEXPR
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%CTYPE....................... :: ', DATA%CTYPE
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%CFCLASS..................... :: ', DATA%CFCLASS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NLEG........................ :: ', DATA%NLEG
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NTOTENS..................... :: ', DATA%NTOTENS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NENSFNB..................... :: ', DATA%NENSFNB
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NWINOFF..................... :: ', DATA%NWINOFF
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NJDIAG...................... :: ', DATA%NJDIAG
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NJDOMAI..................... :: ', DATA%NJDOMAI
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NJITER...................... :: ', DATA%NJITER
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NSTREAM..................... :: ', DATA%NSTREAM
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NSYSTEM..................... :: ', DATA%NSYSTEM
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NMETHOD..................... :: ', DATA%NMETHOD
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NREFERENCE.................. :: ', DATA%NREFERENCE
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NCONSENSUS.................. :: ', DATA%NCONSENSUS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NDWD........................ :: ', DATA%NDWD
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NMFR........................ :: ', DATA%NMFR
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NNCEP....................... :: ', DATA%NNCEP
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NUKM........................ :: ', DATA%NUKM
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NINDAT...................... :: ', DATA%NINDAT
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NSSSSS...................... :: ', DATA%NSSSSS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%RTIMST...................... :: ', DATA%RTIMST
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NCONF....................... :: ', DATA%NCONF
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%LPPSTEPS.................... :: ', DATA%LPPSTEPS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%TSTEP....................... :: ', DATA%TSTEP
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NSTEPINI.................... :: ', DATA%NSTEPINI
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%LOBSC1...................... :: ', DATA%LOBSC1
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%CNMEXP...................... :: ', DATA%CNMEXP
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%LVAREPS..................... :: ', DATA%LVAREPS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NFCHO_TRUNC_INI............. :: ', DATA%NFCHO_TRUNC_INI
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%LDMCC04..................... :: ', DATA%LDMCC04
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NPROC_IO.................... :: ', DATA%NPROC_IO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PRINT_SIM_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'WRITE_SIM_PAR'
SUBROUTINE WRITE_SIM_PAR(DATA, UNIT )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: SIM_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(SIM_PAR_T),      INTENT(IN) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IGRIB2_TABLES_VERSION_LATEST, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NCYCLE, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NLOCGRB, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NBITSSHLNSP, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 32 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NBITSEXPR, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 33 )

  WRITE(UNIT, IOSTAT=STAT) DATA%CTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4 )

  WRITE(UNIT, IOSTAT=STAT) DATA%CFCLASS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 5 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NLEG, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 6 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NTOTENS, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NENSFNB, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 8 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NWINOFF, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 9 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NJDIAG, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 10 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NJDOMAI, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 11 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NJITER, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 12 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSTREAM, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 13 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSYSTEM, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 14 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NMETHOD, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 15 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NREFERENCE, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 16 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NCONSENSUS, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 17 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NDWD, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 18 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NMFR, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 19 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NNCEP, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 20 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NUKM, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 21 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NINDAT, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 22 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSSSSS, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 23 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%RTIMST, REAL64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 24 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NCONF, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 25 )



  WRITE(UNIT, IOSTAT=STAT) DATA%LPPSTEPS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 34 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%TSTEP, REAL64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 35 )


  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSTEPINI, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 26 )

  WRITE(UNIT, IOSTAT=STAT) DATA%LOBSC1
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 27 )

  WRITE(UNIT, IOSTAT=STAT) DATA%CNMEXP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 28 )

  WRITE(UNIT, IOSTAT=STAT) DATA%LVAREPS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 29 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NFCHO_TRUNC_INI, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 30 )

  WRITE(UNIT, IOSTAT=STAT) DATA%LDMCC04
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 31 )

  WRITE(UNIT, IOSTAT=STAT) DATA%NPROC_IO
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 36 )



  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IGRIB2_TABLES_VERSION_LATEST\"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NCYCLE\"' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NLOCGRB\"' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%CTYPE\"' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%CFCLASS\"' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NLEG\"' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NTOTENS\"' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NENSFNB\"' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NWINOFF\"' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NJDIAG\"' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NJDOMAI\"' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NJITER\"' )
    CASE (13)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NSTREAM\"' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NSYSTEM\"' )
    CASE (15)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NMETHOD\"' )
    CASE (16)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NREFERENCE\"' )
    CASE (17)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NCONSENSUS\"' )
    CASE (18)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NDWD\"' )
    CASE (19)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NMFR\"' )
    CASE (20)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NNCEP\"' )
    CASE (21)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NUKM\"' )
    CASE (22)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NINDAT\"' )
    CASE (23)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NSSSSS\"' )
    CASE (24)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%RTIMST\"' )
    CASE (25)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NCONF\"' )
    CASE (26)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NSTEPINI\"' )
    CASE (27)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%LOBSC1\"' )
    CASE (28)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%CNMEXP\"' )
    CASE (29)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%LVAREPS\"' )
    CASE (30)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NFCHO_TRUNC_INI\"' )
    CASE (31)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%LDMCC04\"' )
    CASE (32)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NBITSSHLNSP\"' )
    CASE (33)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NBITSEXPR\"' )
    CASE (34)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%LPPSTEPS\"' )
    CASE (35)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%TSTEP\"' )
    CASE (36)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NPROC_IO\"' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE WRITE_SIM_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PRINT_SAT_PAR'
SUBROUTINE PRINT_SAT_PAR( DATA, UNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: SAT_PAR_T
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(SAT_PAR_T),      INTENT(IN) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  WRITE(UNIT,*) ' '
  WRITE(UNIT,*) ' SATELLITES PARAMETERS'
  WRITE(UNIT,*) ' ---------------------'

  WRITE(UNIT,*) ' + ALLOCATED(SATELLITES_PARAMS%MSERIES).............. :: ', ALLOCATED(DATA%MSERIES)
  IF ( ALLOCATED(DATA%MSERIES) ) THEN
    WRITE(UNIT,*) ' + LBOUND(SATELLITES_PARAMS%MSERIES,1)............... :: ', LBOUND(DATA%MSERIES,1)
    WRITE(UNIT,*) ' + UBOUND(SATELLITES_PARAMS%MSERIES,1)............... :: ', UBOUND(DATA%MSERIES,1)
    DO I = LBOUND(DATA%MSERIES,1), UBOUND(DATA%MSERIES,1)
      WRITE(UNIT,'(A,I6,A,F11.4)') ' + SATELLITES_PARAMS%MSERIES(',I,').............. :: ', DATA%MSERIES(I)
    ENDDO
  ENDIF

  WRITE(UNIT,*) ' + ALLOCATED(SATELLITES_PARAMS%MSATID).............. :: ', ALLOCATED(DATA%MSATID)
  IF ( ALLOCATED(DATA%MSATID) ) THEN
    WRITE(UNIT,*) ' + LBOUND(SATELLITES_PARAMS%MSATID,1)............... :: ', LBOUND(DATA%MSATID,1)
    WRITE(UNIT,*) ' + UBOUND(SATELLITES_PARAMS%MSATID,1)............... :: ', UBOUND(DATA%MSATID,1)
    DO I = LBOUND(DATA%MSATID,1), UBOUND(DATA%MSATID,1)
      WRITE(UNIT,'(A,I6,A,F11.4)') ' + SATELLITES_PARAMS%MSATID(',I,').............. :: ', DATA%MSATID(I)
    ENDDO
  ENDIF

  WRITE(UNIT,*) ' + ALLOCATED(SATELLITES_PARAMS%MINST).............. :: ', ALLOCATED(DATA%MINST)
  IF ( ALLOCATED(DATA%MINST) ) THEN
    WRITE(UNIT,*) ' + LBOUND(SATELLITES_PARAMS%MINST,1)............... :: ', LBOUND(DATA%MINST,1)
    WRITE(UNIT,*) ' + UBOUND(SATELLITES_PARAMS%MINST,1)............... :: ', UBOUND(DATA%MINST,1)
    DO I = LBOUND(DATA%MINST,1), UBOUND(DATA%MINST,1)
      WRITE(UNIT,'(A,I6,A,F11.4)') ' + SATELLITES_PARAMS%MINST(',I,').............. :: ', DATA%MINST(I)
    ENDDO
  ENDIF

  WRITE(UNIT,*) ' + ALLOCATED(SATELLITES_PARAMS%MCHAN).............. :: ', ALLOCATED(DATA%MCHAN)
  IF ( ALLOCATED(DATA%MCHAN) ) THEN
    WRITE(UNIT,*) ' + LBOUND(SATELLITES_PARAMS%MCHAN,1)............... :: ', LBOUND(DATA%MCHAN,1)
    WRITE(UNIT,*) ' + UBOUND(SATELLITES_PARAMS%MCHAN,1)............... :: ', UBOUND(DATA%MCHAN,1)
    DO I = LBOUND(DATA%MCHAN,1), UBOUND(DATA%MCHAN,1)
      WRITE(UNIT,'(A,I6,A,F11.4)') ' + SATELLITES_PARAMS%MCHAN(',I,').............. :: ', DATA%MCHAN(I)
    ENDDO
  ENDIF

  WRITE(UNIT,*) ' + ALLOCATED(SATELLITES_PARAMS%RCWN).............. :: ', ALLOCATED(DATA%RCWN)
  IF ( ALLOCATED(DATA%RCWN) ) THEN
    WRITE(UNIT,*) ' + LBOUND(SATELLITES_PARAMS%RCWN,1)............... :: ', LBOUND(DATA%RCWN,1)
    WRITE(UNIT,*) ' + UBOUND(SATELLITES_PARAMS%RCWN,1)............... :: ', UBOUND(DATA%RCWN,1)
    DO I = LBOUND(DATA%RCWN,1), UBOUND(DATA%RCWN,1)
      WRITE(UNIT,'(A,I6,A,F11.4)') ' + SATELLITES_PARAMS%RCWN(',I,').............. :: ', DATA%RCWN(I)
    ENDDO
  ENDIF



  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PRINT_SAT_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PRINT_GEO_PAR'
SUBROUTINE PRINT_GEO_PAR( DATA, UNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: GEO_PAR_T
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GEO_PAR_T),      INTENT(IN) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  WRITE(UNIT,*) ' '
  WRITE(UNIT,*) ' GEOMETRY PARAMETERS'
  WRITE(UNIT,*) ' -------------------'
  WRITE(UNIT,*) ' + GEOMETRY_PARAMS%JPMXLE........................ :: ', DATA%JPMXLE
  WRITE(UNIT,*) ' + GEOMETRY_PARAMS%JPMXGL........................ :: ', DATA%JPMXGL
  WRITE(UNIT,*) ' + GEOMETRY_PARAMS%ISMAX......................... :: ', DATA%ISMAX
  WRITE(UNIT,*) ' + GEOMETRY_PARAMS%ILATS......................... :: ', DATA%ILATS
  WRITE(UNIT,*) ' + GEOMETRY_PARAMS%ILONS......................... :: ', DATA%ILONS
  WRITE(UNIT,*) ' + GEOMETRY_PARAMS%IDGNH......................... :: ', DATA%IDGNH
  WRITE(UNIT,*) ' + GEOMETRY_PARAMS%ZNLAT......................... :: ', DATA%ZNLAT
  WRITE(UNIT,*) ' + GEOMETRY_PARAMS%ZSLAT......................... :: ', DATA%ZSLAT
  WRITE(UNIT,*) ' + GEOMETRY_PARAMS%IFLEV......................... :: ', DATA%IFLEV
  WRITE(UNIT,*) ' + ALLOCATED(GEOMETRY_PARAMS%ZVERT).............. :: ', ALLOCATED(DATA%ZVERT)

  IF ( ALLOCATED(DATA%ZVERT) ) THEN
    WRITE(UNIT,*) ' + LBOUND(GEOMETRY_PARAMS%ZVERT,1)............... :: ', LBOUND(DATA%ZVERT,1)
    WRITE(UNIT,*) ' + UBOUND(GEOMETRY_PARAMS%ZVERT,1)............... :: ', UBOUND(DATA%ZVERT,1)
    DO I = LBOUND(DATA%ZVERT,1), UBOUND(DATA%ZVERT,1)
      WRITE(UNIT,'(A,I6,A,F11.4)') ' + GEOMETRY_PARAMS%ZVERT(',I,').............. :: ', DATA%ZVERT(I)
    ENDDO
  ENDIF


  WRITE(UNIT,*) ' + GEOMETRY_PARAMS%NHTYP......................... :: ', DATA%NHTYP
  WRITE(UNIT,*) ' + GEOMETRY_PARAMS%NSTTYP........................ :: ', DATA%NSTTYP
  WRITE(UNIT,*) ' + GEOMETRY_PARAMS%RMUCEN........................ :: ', DATA%RMUCEN
  WRITE(UNIT,*) ' + GEOMETRY_PARAMS%RLOCEN........................ :: ', DATA%RLOCEN
  WRITE(UNIT,*) ' + GEOMETRY_PARAMS%RSTRET........................ :: ', DATA%RSTRET
  WRITE(UNIT,*) ' + ALLOCATED(GEOMETRY_PARAMS%ILOENG)............. :: ', ALLOCATED(DATA%ILOENG)

  IF ( ALLOCATED(DATA%ILOENG) ) THEN
    WRITE(UNIT,*) ' + LBOUND(GEOMETRY_PARAMS%ILOENG,1).............. :: ', LBOUND(DATA%ILOENG,1)
    WRITE(UNIT,*) ' + UBOUND(GEOMETRY_PARAMS%ILOENG,1).............. :: ', UBOUND(DATA%ILOENG,1)
    DO I = LBOUND(DATA%ILOENG,1), UBOUND(DATA%ILOENG,1)
      WRITE(UNIT,'(A,I6,A,I8)') ' +     GEOMETRY_PARAMS%ILOENG(',I ,').. : ', DATA%ILOENG(I)
    ENDDO
  ENDIF

  WRITE(UNIT,*) ' + ALLOCATED(GEOMETRY_PARAMS%NSFLEVS)............ :: ', ALLOCATED(DATA%NSFLEVS)
  IF ( ALLOCATED(DATA%NSFLEVS) ) THEN
    WRITE(UNIT,*) ' + LBOUND(GEOMETRY_PARAMS%NSFLEVS,1)............. :: ', LBOUND(DATA%NSFLEVS,1)
    WRITE(UNIT,*) ' + UBOUND(GEOMETRY_PARAMS%NSFLEVS,1)............. :: ', UBOUND(DATA%NSFLEVS,1)
    WRITE(UNIT,*) ' + LBOUND(GEOMETRY_PARAMS%NSFLEVS,2)............. :: ', LBOUND(DATA%NSFLEVS,2)
    WRITE(UNIT,*) ' + UBOUND(GEOMETRY_PARAMS%NSFLEVS,2)............. :: ', UBOUND(DATA%NSFLEVS,2)

    DO I = LBOUND(DATA%NSFLEVS,1), UBOUND(DATA%NSFLEVS,1)
      DO J = LBOUND(DATA%NSFLEVS,2), UBOUND(DATA%NSFLEVS,2)

        WRITE(UNIT,'(A,I3,A,I3,A,I8)') ' + GEOMETRY_PARAMS%NSFLEVS(',I,',', J,')....... :: ', DATA%NSFLEVS(I,J)
      ENDDO
    ENDDO
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PRINT_GEO_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'WRITE_SAT_PAR'
SUBROUTINE WRITE_SAT_PAR(DATA, UNIT )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: SAT_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(SAT_PAR_T),      INTENT(IN) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%MSERIES)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )
  IF ( ALLOCATED(DATA%MSERIES) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%MSERIES,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%MSERIES,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )
    DO I = LBOUND(DATA%MSERIES,1), UBOUND(DATA%MSERIES,1)
      WRITE(UNIT, IOSTAT=STAT) INT( DATA%MSERIES(I), INT64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4 )
    ENDDO
  ENDIF

  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%MSATID)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 5 )
  IF ( ALLOCATED(DATA%MSATID) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%MSATID,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 6 )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%MSATID,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )
    DO I = LBOUND(DATA%MSATID,1), UBOUND(DATA%MSATID,1)
      WRITE(UNIT, IOSTAT=STAT) INT( DATA%MSATID(I), INT64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 8 )
    ENDDO
  ENDIF

  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%MINST)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 9 )
  IF ( ALLOCATED(DATA%MINST) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%MINST,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 10 )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%MINST,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 11 )
    DO I = LBOUND(DATA%MINST,1), UBOUND(DATA%MINST,1)
      WRITE(UNIT, IOSTAT=STAT) INT( DATA%MINST(I), INT64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 12 )
    ENDDO
  ENDIF

  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%MCHAN)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 13 )
  IF ( ALLOCATED(DATA%MCHAN) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%MCHAN,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 14 )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%MCHAN,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 15 )
    DO I = LBOUND(DATA%MCHAN,1), UBOUND(DATA%MCHAN,1)
      WRITE(UNIT, IOSTAT=STAT) INT( DATA%MCHAN(I), INT64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 16 )
    ENDDO
  ENDIF

  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%RCWN)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 17 )
  IF ( ALLOCATED(DATA%RCWN) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%RCWN,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 18 )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%RCWN,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 19 )
    DO I = LBOUND(DATA%RCWN,1), UBOUND(DATA%RCWN,1)
      WRITE(UNIT, IOSTAT=STAT) REAL( DATA%RCWN(I), REAL64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 20 )
    ENDDO
  ENDIF


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=32) :: TMP1

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write allocation status for of: \"DATA%MSERIES\"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write lbound of: \"DATA%MSERIES\"' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write ubound of: \"DATA%MSERIES\"' )
    CASE (4)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%MSERIES('//TRIM(ADJUSTL(TMP1))//')\"' )

    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write allocation status for of: \"DATA%MSATID\"' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write lbound of: \"DATA%MSATID\"' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write ubound of: \"DATA%MSATID\"' )
    CASE (8)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%MSATID('//TRIM(ADJUSTL(TMP1))//')\"' )

    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write allocation status for of: \"DATA%MINST\"' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write lbound of: \"DATA%MINST\"' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write ubound of: \"DATA%MINST\"' )
    CASE (12)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%MINST('//TRIM(ADJUSTL(TMP1))//')\"' )

    CASE (13)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write allocation status for of: \"DATA%MCHAN\"' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write lbound of: \"DATA%MCHAN\"' )
    CASE (15)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write ubound of: \"DATA%MCHAN\"' )
    CASE (16)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%MCHAN('//TRIM(ADJUSTL(TMP1))//')\"' )


    CASE (17)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write allocation status for of: \"DATA%RCWN\"' )
    CASE (18)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write lbound of: \"DATA%RCWN\"' )
    CASE (19)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write ubound of: \"DATA%RCWN\"' )
    CASE (20)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%RCWN('//TRIM(ADJUSTL(TMP1))//')\"' )

    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE WRITE_SAT_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'WRITE_GEO_PAR'
SUBROUTINE WRITE_GEO_PAR(DATA, UNIT )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: GEO_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GEO_PAR_T),      INTENT(IN) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%JPMXLE, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%JPMXGL, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ISMAX, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ILATS, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ILONS, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 5 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IDGNH, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 6 )


  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%ZNLAT, REAL64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%ZSLAT, REAL64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 8 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IFLEV, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 9 )



  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%ZVERT)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 10 )

  IF ( ALLOCATED(DATA%ZVERT) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%ZVERT,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 11 )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%ZVERT,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 12 )
    DO I = LBOUND(DATA%ZVERT,1), UBOUND(DATA%ZVERT,1)
      WRITE(UNIT, IOSTAT=STAT) REAL( DATA%ZVERT(I), REAL64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 13 )
    ENDDO
  ENDIF


  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NHTYP, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 14 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSTTYP, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 15 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%RMUCEN, REAL64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 16 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%RLOCEN, REAL64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 17 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%RSTRET, REAL64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 18 )


  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%ILOENG)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 19 )

  IF ( ALLOCATED(DATA%ILOENG) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%ILOENG,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 20 )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%ILOENG,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 21 )
    DO I = LBOUND(DATA%ILOENG,1), UBOUND(DATA%ILOENG,1)
      WRITE(UNIT, IOSTAT=STAT) INT( DATA%ILOENG(I), INT64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 22 )
    ENDDO
  ENDIF



  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%NSFLEVS)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 23 )

  IF ( ALLOCATED(DATA%NSFLEVS) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%NSFLEVS,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 24 )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%NSFLEVS,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 25 )
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%NSFLEVS,2), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 26 )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%NSFLEVS,2), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 27 )

    DO I = LBOUND(DATA%NSFLEVS,1), UBOUND(DATA%NSFLEVS,1)
      DO J = LBOUND(DATA%NSFLEVS,2), UBOUND(DATA%NSFLEVS,2)
        WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSFLEVS(I,J), INT64)
        PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 28 )
      ENDDO
    ENDDO
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=32) :: TMP1
    CHARACTER(LEN=32) :: TMP2

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%JPMXLE\"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%JPMXGL\"' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ISMAX\"' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ILATS\"' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ILONS\"' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IDGNH\"' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ZNLAT\"' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ZSLAT\"' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IFLEV\"' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write allocation status of: \"DATA%ZVERT\"' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write lbound of: \"DATA%ZVERT\"' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write ubound of: \"DATA%ZVERT\"' )
    CASE (13)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ZVERT('//TRIM(ADJUSTL(TMP1))//')\"' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NHTYP\"' )
    CASE (15)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NSTTYP\"' )
    CASE (16)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%RMUCEN\"' )
    CASE (17)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%RLOCEN\"' )
    CASE (18)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%RSTRET\"' )
    CASE (19)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write allocation status of: \"DATA%ILOENG\"' )
    CASE (20)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write lbound of: \"DATA%ILOENG\"' )
    CASE (21)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write ubound of: \"DATA%ILOENG\"' )
    CASE (22)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ILOENG('//TRIM(ADJUSTL(TMP1))//')\"' )
    CASE (23)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write allocation status of: \"DATA%NSFLEVS\"' )
    CASE (24)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write lbound(1) of: \"DATA%NSFLEVS\"' )
    CASE (25)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write ubound(1) of: \"DATA%NSFLEVS\"' )
    CASE (26)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write lbound(2) of: \"DATA%NSFLEVS\"' )
    CASE (27)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write ubound(2) of: \"DATA%NSFLEVS\"' )
    CASE (28)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I10)') I
      TMP2 = REPEAT(' ',32)
      WRITE(TMP2,'(I10)') J
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NSFLEVS('//TRIM(ADJUSTL(TMP1))//':'//TRIM(ADJUSTL(TMP2))//')\"' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE WRITE_GEO_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PRINT_WAM_PAR'
SUBROUTINE PRINT_WAM_PAR( DATA, UNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: WAM_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(WAM_PAR_T),      INTENT(IN) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  WRITE(UNIT,*) ' '
  WRITE(UNIT,*) ' WAM PARAMETERS'
  WRITE(UNIT,*) ' --------------'
  WRITE(UNIT,*) ' + ALLOCATED(DATA%FR)............................ :: ', ALLOCATED(DATA%FR)

  IF ( ALLOCATED(DATA%FR) ) THEN
    WRITE(UNIT,*) ' + LBOUND(SIMULATION_PARAMS%FR,1)................ :: ', LBOUND(DATA%FR,1)
    WRITE(UNIT,*) ' + UBOUND(SIMULATION_PARAMS%FR,1)................ :: ', UBOUND(DATA%FR,1)
    DO I = LBOUND(DATA%FR,1), UBOUND(DATA%FR,1)
      WRITE(UNIT,'(A,I6,A,F11.4)') ' + SIMULATION_PARAMS%FR(',I,')............... :: ', DATA%FR(I)
    ENDDO
  ENDIF


  WRITE(UNIT,*) ALLOCATED(DATA%TH)

  IF ( ALLOCATED(DATA%TH) ) THEN
    WRITE(UNIT,*) ' + LBOUND(SIMULATION_PARAMS%TH,1)................ :: ', LBOUND(DATA%TH,1)
    WRITE(UNIT,*) ' + UBOUND(SIMULATION_PARAMS%TH,1)................ :: ', UBOUND(DATA%TH,1)
    DO I = LBOUND(DATA%TH,1), UBOUND(DATA%TH,1)
      WRITE(UNIT,'(A,I6,A,F11.4)') ' + SIMULATION_PARAMS%TH(', I,')..................... : ', DATA%TH(I)
    ENDDO
  ENDIF

  WRITE(UNIT,*) ' + SIMULATION_PARAMS%LWCOUSAMEGRID............... :: ', DATA%LWCOUSAMEGRID
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%CLDOMAIN.................... :: ', DATA%CLDOMAIN
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NGX......................... :: ', DATA%NGX
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NGY......................... :: ', DATA%NGY
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NANG........................ :: ', DATA%NANG
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NFRE_RED.................... :: ', DATA%NFRE_RED
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%IMDLGRBID_G................. :: ', DATA%IMDLGRBID_G
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%IMDLGRBID_M................. :: ', DATA%IMDLGRBID_M
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NDATE_TIME_WINDOW_END....... :: ', DATA%NDATE_TIME_WINDOW_END
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NWINOFF..................... :: ', DATA%NWINOFF
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NGRIB_VERSION............... :: ', DATA%NGRIB_VERSION
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NTENCODE.................... :: ', DATA%NTENCODE
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NGRBRESI.................... :: ', DATA%NGRBRESI
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NGRBRESS.................... :: ', DATA%NGRBRESS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%PPMISS...................... :: ', DATA%PPMISS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%PPEPS....................... :: ', DATA%PPEPS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%PPREC....................... :: ', DATA%PPREC
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%PPRESOL..................... :: ', DATA%PPRESOL
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%PPMIN_RESET................. :: ', DATA%PPMIN_RESET
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%HOPERI...................... :: ', DATA%HOPERI
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%HOPERS...................... :: ', DATA%HOPERS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%LGRHDIFS.................... :: ', DATA%LGRHDIFS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%LNEWLVTP.................... :: ', DATA%LNEWLVTP
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%LPADPOLES................... :: ', DATA%LPADPOLES
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%LL_GRID_SIMPLE_MATRIX....... :: ', DATA%LL_GRID_SIMPLE_MATRIX
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%IRGG........................ :: ', DATA%IRGG
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%IQGAUSS..................... :: ', DATA%IQGAUSS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%AMOWEP...................... :: ', DATA%AMOWEP
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%AMOSOP...................... :: ', DATA%AMOSOP
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%AMOEAP...................... :: ', DATA%AMOEAP
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%AMONOP...................... :: ', DATA%AMONOP
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%XDELLA...................... :: ', DATA%XDELLA
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%XDELLO...................... :: ', DATA%XDELLO
  WRITE(UNIT,*) ' + ALLOCATED(SIMULATION_PARAMS%NLONRGG).......... :: ', ALLOCATED(DATA%NLONRGG)

  IF ( ALLOCATED(DATA%NLONRGG) ) THEN
    WRITE(UNIT,*) ' + LBOUND(SIMULATION_PARAMS%NLONRGG,1)........... :: ', LBOUND(DATA%NLONRGG,1)
    WRITE(UNIT,*) ' + UBOUND(SIMULATION_PARAMS%NLONRGG,1)........... :: ', UBOUND(DATA%NLONRGG,1)
    DO I = LBOUND(DATA%NLONRGG,1), UBOUND(DATA%NLONRGG,1)
      WRITE(UNIT,'(A,I6,A,I8)') ' + SIMULATION_PARAMS%NLONRGG(',I,').......... :: ', DATA%NLONRGG(I)
    ENDDO
  ENDIF

  WRITE(UNIT,*) ' + SIMULATION_PARAMS%ZMISS....................... :: ', DATA%ZMISS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NENSFNB..................... :: ', DATA%NENSFNB
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NTOTENS..................... :: ', DATA%NTOTENS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NSYSNB...................... :: ', DATA%NSYSNB
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NMETNB...................... :: ', DATA%NMETNB
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%ISTREAM..................... :: ', DATA%ISTREAM
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NLOCGRB..................... :: ', DATA%NLOCGRB
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NCONSENSUS.................. :: ', DATA%NCONSENSUS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NDWD........................ :: ', DATA%NDWD
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NMFR........................ :: ', DATA%NMFR
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NNCEP....................... :: ', DATA%NNCEP
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NUKM........................ :: ', DATA%NUKM
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%IREFDATE.................... :: ', DATA%IREFDATE
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%MARSTYPE.................... :: ', DATA%MARSTYPE
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%YCLASS...................... :: ', DATA%YCLASS
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%YEXPVER..................... :: ', DATA%YEXPVER
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NSPEC2TAB................... :: ', DATA%NSPEC2TAB
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NSPEC2TMPD.................. :: ', DATA%NSPEC2TMPD
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NSPEC2TMPP.................. :: ', DATA%NSPEC2TMPP
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NTRG2TMPD................... :: ', DATA%NTRG2TMPD
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%NTRG2TMPP................... :: ', DATA%NTRG2TMPP
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%ITMIN....................... :: ', DATA%ITMIN
  WRITE(UNIT,*) ' + SIMULATION_PARAMS%ITMAX....................... :: ', DATA%ITMAX

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PRINT_WAM_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'WRITE_WAM_PAR'
SUBROUTINE WRITE_WAM_PAR(DATA, UNIT )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: WAM_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(WAM_PAR_T),      INTENT(IN) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%FR)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )

  IF ( ALLOCATED(DATA%FR) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%FR,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%FR,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )
    DO I = LBOUND(DATA%FR,1), UBOUND(DATA%FR,1)
      WRITE(UNIT, IOSTAT=STAT) REAL( DATA%FR(I), REAL64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4 )
    ENDDO
  ENDIF


  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%TH)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 5 )

  IF ( ALLOCATED(DATA%TH) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%TH,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 6 )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%TH,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )
    DO I = LBOUND(DATA%TH,1), UBOUND(DATA%TH,1)
      WRITE(UNIT, IOSTAT=STAT) REAL( DATA%TH(I), REAL64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 8 )
    ENDDO
  ENDIF

  WRITE(UNIT, IOSTAT=STAT) DATA%LWCOUSAMEGRID
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 9 )

  WRITE(UNIT, IOSTAT=STAT) DATA%CLDOMAIN
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 10 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NGX, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 11 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NGY, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 12 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NANG, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 13 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NFRE_RED, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 14 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IMDLGRBID_G, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 15 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IMDLGRBID_M, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 16 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NDATE_TIME_WINDOW_END, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 17 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NWINOFF, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 18 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NGRIB_VERSION, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 19 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NTENCODE, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 20 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NGRBRESI, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 21 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NGRBRESS, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 22 )



  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%PPMISS, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 23 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%PPEPS, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 24 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%PPREC, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 25 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%PPRESOL, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 26 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%PPMIN_RESET, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 27 )


  WRITE(UNIT, IOSTAT=STAT) DATA%HOPERI
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 28 )

  WRITE(UNIT, IOSTAT=STAT) DATA%HOPERS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 29 )

  WRITE(UNIT, IOSTAT=STAT) DATA%LGRHDIFS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 30 )

  WRITE(UNIT, IOSTAT=STAT) DATA%LNEWLVTP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 31 )

  WRITE(UNIT, IOSTAT=STAT) DATA%LPADPOLES
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 32 )

  WRITE(UNIT, IOSTAT=STAT) DATA%LL_GRID_SIMPLE_MATRIX
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 33 )




  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IRGG, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 34 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IQGAUSS, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 35 )


  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%AMOWEP, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 36 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%AMOSOP, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 37 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%AMOEAP, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 38 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%AMONOP, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 39 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%XDELLA, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 40 )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%XDELLO, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 41 )


  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%NLONRGG)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 42 )

  IF ( ALLOCATED(DATA%NLONRGG) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%NLONRGG,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 43 )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%NLONRGG,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 44 )
    DO I = LBOUND(DATA%NLONRGG,1), UBOUND(DATA%NLONRGG,1)
      WRITE(UNIT, IOSTAT=STAT) INT( DATA%NLONRGG(I), INT64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 45 )
    ENDDO
  ENDIF

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%ZMISS, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 46 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NENSFNB, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 47 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NTOTENS, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 48 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSYSNB, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 49 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NMETNB, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 50 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ISTREAM, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 51 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NLOCGRB, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 52 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NCONSENSUS, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 53 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NDWD, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 54 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NMFR, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 55 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NNCEP, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 56 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NUKM, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 57 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IREFDATE, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 58 )


  WRITE(UNIT, IOSTAT=STAT) DATA%MARSTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 59 )

  WRITE(UNIT, IOSTAT=STAT) DATA%YCLASS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 60 )

  WRITE(UNIT, IOSTAT=STAT) DATA%YEXPVER
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 61 )



  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSPEC2TAB, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 62 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSPEC2TMPD, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 63 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSPEC2TMPP, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 64 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NTRG2TMPD, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 65 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NTRG2TMPP, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 66 )


  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ITMIN, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 67 )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ITMAX, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 68 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=32) :: TMP1

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write allocation status: \"DATA%FR\"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write lbound: \"DATA%FR\"' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write ubound: \"DATA%FR\"' )
    CASE (4)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%FR('//TRIM(ADJUSTL(TMP1))//')\"' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write allocation status: \"DATA%TH\"' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write lbound: \"DATA%TH\"' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write ubound: \"DATA%TH\"' )
    CASE (8)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%TH('//TRIM(ADJUSTL(TMP1))//')\"' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%LWCOUSAMEGRID\"' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%CLDOMAIN\"' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NGX\"' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NGY\"' )
    CASE (13)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NANG\"' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NFRE_RED\"' )
    CASE (15)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IMDLGRBID_G\"' )
    CASE (16)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IMDLGRBID_M\"' )
    CASE (17)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NDATE_TIME_WINDOW_END\"' )
    CASE (18)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NWINOFF\"' )
    CASE (19)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NGRIB_VERSION\"' )
    CASE (20)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NTENCODE\"' )
    CASE (21)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NGRBRESI\"' )
    CASE (22)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NGRBRESS\"' )
    CASE (23)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%PPMISS\"' )
    CASE (24)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%PPEPS\"' )
    CASE (25)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%PPREC\"' )
    CASE (26)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%PPRESOL\"' )
    CASE (27)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%PPMIN_RESET\"' )
    CASE (28)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%HOPERI\"' )
    CASE (29)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%HOPERS\"' )
    CASE (30)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%LGRHDIFS\"' )
    CASE (31)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%LNEWLVTP\"' )
    CASE (32)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%LPADPOLES\"' )
    CASE (33)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%LL_GRID_SIMPLE_MATRIX\"' )
    CASE (34)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IRGG\"' )
    CASE (35)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IQGAUSS\"' )
    CASE (36)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%AMOWEP\"' )
    CASE (37)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%AMOSOP\"' )
    CASE (38)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%AMOEAP\"' )
    CASE (39)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%AMONOP\"' )
    CASE (40)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%XDELLA\"' )
    CASE (41)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%XDELLO\"' )
    CASE (42)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write allocation status: \"DATA%NLONRGG\"' )
    CASE (43)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write lboud: \"DATA%NLONRGG\"' )
    CASE (44)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write ubound: \"DATA%NLONRGG\"' )
    CASE (45)
      TMP1 = REPEAT(' ',32)
      WRITE(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NLONRGG('//TRIM(ADJUSTL(TMP1))//')\"' )
    CASE (46)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ZMISS\"' )
    CASE (47)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NENSFNB\"' )
    CASE (48)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NTOTENS\"' )
    CASE (49)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NSYSNB\"' )
    CASE (50)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NMETNB\"' )
    CASE (51)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ISTREAM\"' )
    CASE (52)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NLOCGRB\"' )
    CASE (53)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NCONSENSUS\"' )
    CASE (54)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NDWD\"' )
    CASE (55)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NMFR\"' )
    CASE (56)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NNCEP\"' )
    CASE (57)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NUKM\"' )
    CASE (58)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%IREFDATE\"' )
    CASE (59)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%MARSTYPE\"' )
    CASE (60)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%YCLASS\"' )
    CASE (61)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%YEXPVER\"' )
    CASE (62)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NSPEC2TAB\"' )
    CASE (63)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NSPEC2TMPD\"' )
    CASE (64)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NSPEC2TMPP\"' )
    CASE (65)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NTRG2TMPD\"' )
    CASE (66)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%NTRG2TMPP\"' )
    CASE (67)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ITMIN\"' )
    CASE (68)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write: \"DATA%ITMAX\"' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE WRITE_WAM_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_SIM_PAR'
SUBROUTINE READ_SIM_PAR( DATA, UNIT, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: SIM_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(SIM_PAR_T),      INTENT(INOUT) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  LOGICAL,              INTENT(IN)    :: VERBOSE

  ! Local variables
  INTEGER(KIND=INT64)  :: ITMP
  REAL(KIND=REAL64)    :: RTMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  READ(UNIT, IOSTAT=STAT) ITMP
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%TABLE_VERSION_LATEST: ', ITMP, STAT
  ENDIF
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )
  DATA%IGRIB2_TABLES_VERSION_LATEST = INT( ITMP, KIND(DATA%IGRIB2_TABLES_VERSION_LATEST))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NCYCLE: ', ITMP, STAT
  ENDIF
  DATA%NCYCLE = INT( ITMP, KIND(DATA%NCYCLE))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NLOCGRB: ', ITMP, STAT
  ENDIF
  DATA%NLOCGRB = INT( ITMP, KIND(DATA%NLOCGRB))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 32 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%:NBITSSHLNSP ', ITMP, STAT
  ENDIF
  DATA%NBITSSHLNSP = INT( ITMP, KIND(DATA%NBITSSHLNSP))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 33 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%:NBITSEXPR ', ITMP, STAT
  ENDIF
  DATA%NBITSEXPR = INT( ITMP, KIND(DATA%NBITSEXPR))



  READ(UNIT, IOSTAT=STAT) DATA%CTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%CTYPE: ',  DATA%CTYPE, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%CFCLASS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 5 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%CFCLASS: ', DATA%CFCLASS, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 6 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NLEG: ', ITMP, STAT
  ENDIF
  DATA%NLEG = INT( ITMP, KIND(DATA%NLEG))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NTOTENS: ', ITMP, STAT
  ENDIF
  DATA%NTOTENS = INT( ITMP, KIND(DATA%NTOTENS))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 8 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NENSFNB: ', ITMP, STAT
  ENDIF
  DATA%NENSFNB = INT( ITMP, KIND(DATA%NENSFNB))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 9 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NWINOFF: ', ITMP, STAT
  ENDIF
  DATA%NWINOFF = INT( ITMP, KIND(DATA%NWINOFF))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 10 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NJDIAG: ', ITMP, STAT
  ENDIF
  DATA%NJDIAG = INT( ITMP, KIND(DATA%NJDIAG))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 11 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NJDOMAI: ', ITMP, STAT
  ENDIF
  DATA%NJDOMAI = INT( ITMP, KIND(DATA%NJDOMAI))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 12 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NJITER: ', ITMP, STAT
  ENDIF
  DATA%NJITER = INT( ITMP, KIND(DATA%NJITER))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 13 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NSTREAM: ', ITMP, STAT
  ENDIF
  DATA%NSTREAM = INT( ITMP, KIND(DATA%NSTREAM))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 14 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NSYSTEM: ', ITMP, STAT
  ENDIF
  DATA%NSYSTEM = INT( ITMP, KIND(DATA%NSYSTEM))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 15 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NMETHOD: ', ITMP, STAT
  ENDIF
  DATA%NMETHOD = INT( ITMP, KIND(DATA%NMETHOD))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 16 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NREFERENCE: ', ITMP, STAT
  ENDIF
  DATA%NREFERENCE = INT( ITMP, KIND(DATA%NREFERENCE))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 17 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NCONSENSUS: ', ITMP, STAT
  ENDIF
  DATA%NCONSENSUS = INT( ITMP, KIND(DATA%NCONSENSUS))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 18 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NDWD: ', ITMP, STAT
  ENDIF
  DATA%NDWD = INT( ITMP, KIND(DATA%NDWD))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 19 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NMFR: ', ITMP, STAT
  ENDIF
  DATA%NMFR = INT( ITMP, KIND(DATA%NMFR))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 20 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NNCEP: ', ITMP, STAT
  ENDIF
  DATA%NNCEP = INT( ITMP, KIND(DATA%NNCEP))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 21 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NUKM: ', ITMP, STAT
  ENDIF
  DATA%NUKM = INT( ITMP, KIND(DATA%NUKM))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 22 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NINDAT: ', ITMP, STAT
  ENDIF
  DATA%NINDAT = INT( ITMP, KIND(DATA%NINDAT))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 23 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NSSSSS: ', ITMP, STAT
  ENDIF
  DATA%NSSSSS = INT( ITMP, KIND(DATA%NSSSSS))

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 24 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%RTIMST: ', RTMP, STAT
  ENDIF
  DATA%RTIMST = REAL( RTMP, KIND(DATA%RTIMST))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 25 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NCONF: ', ITMP, STAT
  ENDIF
  DATA%NCONF = INT( ITMP, KIND(DATA%NCONF))

  READ(UNIT, IOSTAT=STAT) DATA%LPPSTEPS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 34 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%LPPSTEPS: ', DATA%LPPSTEPS, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 35 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%TSTEP: ', RTMP, STAT
  ENDIF
  DATA%TSTEP = REAL( RTMP, KIND(DATA%TSTEP) )


  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 26 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NSTEPINI: ', ITMP, STAT
  ENDIF
  DATA%NSTEPINI = INT( ITMP, KIND(DATA%NSTEPINI))

  READ(UNIT, IOSTAT=STAT) DATA%LOBSC1
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 27 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%LOBSC1: ', DATA%LOBSC1, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%CNMEXP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 28 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%CNMEXP: ', DATA%CNMEXP, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%LVAREPS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 29 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%LVAREPS: ', DATA%LVAREPS, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 30 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NFCHO_TRUNC_INI: ', ITMP, STAT
  ENDIF
  DATA%NFCHO_TRUNC_INI = INT( ITMP, KIND(DATA%NFCHO_TRUNC_INI))

  READ(UNIT, IOSTAT=STAT) DATA%LDMCC04
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 31 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%LDMCC04: ', DATA%LDMCC04, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 36 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NPROC_IO: ', ITMP, STAT
  ENDIF
  DATA%NPROC_IO = INT( ITMP, KIND(DATA%NPROC_IO))

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%IGRIB2_TABLES_VERSION_LATEST\"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NCYCLE\"' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NLOCGRB\"' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%CTYPE\"' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%CFCLASS\"' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NLEG\"' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NTOTENS\"' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NENSFNB\"' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NWINOFF\"' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NJDIAG\"' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NJDOMAI\"' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NJITER\"' )
    CASE (13)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NSTREAM\"' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NSYSTEM\"' )
    CASE (15)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NMETHOD\"' )
    CASE (16)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NREFERENCE\"' )
    CASE (17)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NCONSENSUS\"' )
    CASE (18)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NDWD\"' )
    CASE (19)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NMFR\"' )
    CASE (20)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NNCEP\"' )
    CASE (21)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NUKM\"' )
    CASE (22)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NINDAT\"' )
    CASE (23)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NSSSSS\"' )
    CASE (24)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%RTIMST\"' )
    CASE (25)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NCONF\"' )
    CASE (26)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NSTEPINI\"' )
    CASE (27)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%LOBSC1\"' )
    CASE (28)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%CNMEXP\"' )
    CASE (29)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%LVAREPS\"' )
    CASE (30)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NFCHO_TRUNC_INI\"' )
    CASE (31)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%LDMCC04\"' )
    CASE (32)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NBITSSHLNSP\"' )
    CASE (33)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NBITSEXPR\"' )
    CASE (34)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%LPPSTEPS\"' )
    CASE (35)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%TSTEP\"' )
    CASE (36)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to Read: \"DATA%NPROC_IO\"' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT



    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! READ the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE READ_SIM_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_GEO_PAR'
SUBROUTINE READ_GEO_PAR( DATA, UNIT, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: GEO_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GEO_PAR_T),      INTENT(INOUT) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  LOGICAL,              INTENT(IN)    :: VERBOSE

  ! Local variables
  REAL(KIND=REAL64)    :: RTMP
  INTEGER(KIND=INT64)  :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=INT64) :: LB1
  INTEGER(KIND=INT64) :: LB2
  INTEGER(KIND=INT64) :: UB1
  INTEGER(KIND=INT64) :: UB2
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  LOGICAL :: IS_ZVERT_ALLOCATED
  LOGICAL :: IS_ILOENG_ALLOCATED
  LOGICAL :: IS_NSFLEVS_ALLOCATED

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( ALLOCATED(DATA%ZVERT) ) THEN
    DEALLOCATE(DATA%ZVERT)
  ENDIF
  IF ( ALLOCATED(DATA%ILOENG) ) THEN
    DEALLOCATE(DATA%ILOENG)
  ENDIF
  IF ( ALLOCATED(DATA%NSFLEVS) ) THEN
    DEALLOCATE(DATA%NSFLEVS)
  ENDIF

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%JPMXLE: ', ITMP, STAT
  ENDIF
  DATA%JPMXLE = INT( ITMP, KIND(DATA%JPMXLE))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%JPMXGL: ', ITMP, STAT
  ENDIF
  DATA%JPMXGL = INT( ITMP, KIND(DATA%JPMXGL))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%ISMAX: ', ITMP, STAT
  ENDIF
  DATA%ISMAX = INT( ITMP, KIND(DATA%ISMAX))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%ILATS: ', ITMP, STAT
  ENDIF
  DATA%ILATS = INT( ITMP, KIND(DATA%ILATS))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 5 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%ILONS: ', ITMP, STAT
  ENDIF
  DATA%ILONS = INT( ITMP, KIND(DATA%ILONS))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 6 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%IDGNH: ', ITMP, STAT
  ENDIF
  DATA%IDGNH = INT( ITMP, KIND(DATA%IDGNH))

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%ZNLAT: ', RTMP, STAT
  ENDIF
  DATA%ZNLAT = REAL( RTMP, KIND(DATA%ZNLAT))

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 8 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%ZSLAT: ', RTMP, STAT
  ENDIF
  DATA%ZSLAT = REAL( RTMP, KIND(DATA%ZSLAT))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 9 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%IFLEV: ', ITMP, STAT
  ENDIF
  DATA%IFLEV = INT( ITMP, KIND(DATA%IFLEV))

  READ(UNIT, IOSTAT=STAT) IS_ZVERT_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 10 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%IS_ZVERT_ALLOCATED: ', IS_ZVERT_ALLOCATED, STAT
  ENDIF

  IF ( IS_ZVERT_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 11 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 12 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%UB1: ', UB1, STAT
    ENDIF
    ALLOCATE( DATA%ZVERT(LB1:UB1), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 29 )
    DO I = LBOUND(DATA%ZVERT,1), UBOUND(DATA%ZVERT,1)
      READ(UNIT, IOSTAT=STAT) RTMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 13 )
      IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%GEO_%ZVERT(', I ,'): ', RTMP, STAT
      ENDIF
      DATA%ZVERT(I) = REAL( RTMP, KIND(DATA%ZVERT(I)))
    ENDDO
  ENDIF

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 14 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%NHTYP: ', ITMP, STAT
  ENDIF
  DATA%NHTYP = INT( ITMP, KIND(DATA%NHTYP))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 15 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%NSTTYP: ', ITMP, STAT
  ENDIF
  DATA%NSTTYP = INT( ITMP, KIND(DATA%NSTTYP))

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 16 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%RMUCEN: ', RTMP, STAT
  ENDIF
  DATA%RMUCEN = REAL( RTMP, KIND(DATA%RMUCEN))

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 17 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%RLOCEN: ', RTMP, STAT
  ENDIF
  DATA%RLOCEN = REAL( RTMP, KIND(DATA%RLOCEN))

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 18 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%RSTRET: ', RTMP, STAT
  ENDIF
  DATA%RSTRET = REAL( RTMP, KIND(DATA%RSTRET))


  READ(UNIT, IOSTAT=STAT) IS_ILOENG_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 19 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%IS_ILOENG_ALLOCATED: ', IS_ILOENG_ALLOCATED, STAT
  ENDIF

  IF ( IS_ILOENG_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 20 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 21 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%ITMP: ', ITMP, STAT
    ENDIF
    ALLOCATE( DATA%ILOENG(LB1:UB1), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 30 )
    DO I = LBOUND(DATA%ILOENG,1), UBOUND(DATA%ILOENG,1)
      READ(UNIT, IOSTAT=STAT) ITMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 22 )
      IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%GEO_%ILOENG(', I ,'): ', ITMP, STAT
      ENDIF
      DATA%ILOENG(I) = INT( ITMP, KIND(DATA%ILOENG(I)))
    ENDDO
  ENDIF

  READ(UNIT, IOSTAT=STAT) IS_NSFLEVS_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 23 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%IS_NSFLEVS_ALLOCATED: ', IS_NSFLEVS_ALLOCATED, STAT
  ENDIF

  IF ( IS_NSFLEVS_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 24 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 25 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%UB1: ', UB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) LB2
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 26 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%LB2: ', LB2, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB2
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 27 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%UB2: ', UB2, STAT
    ENDIF
    ALLOCATE( DATA%NSFLEVS(LB1:UB1, LB2:UB2 ), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 31 )
    DO I = LBOUND(DATA%NSFLEVS,1), UBOUND(DATA%NSFLEVS,1)
      DO J = LBOUND(DATA%NSFLEVS,2), UBOUND(DATA%NSFLEVS,2)
        READ(UNIT, IOSTAT=STAT) ITMP
        PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 28 )
        IF ( VERBOSE ) THEN
          WRITE(*,*) ' + PAR%GEO_%NSFLEVS(', I,', ', J,'): ', ITMP, STAT
        ENDIF
        DATA%NSFLEVS(I,J) = INT( ITMP, KIND(DATA%NSFLEVS(I,J)))
      ENDDO
    ENDDO
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=32) :: TMP1
    CHARACTER(LEN=32) :: TMP2

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%JPMXLE\"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%JPMXGL\"' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%ISMAX\"' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%ILATS\"' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%ILONS\"' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%IDGNH\"' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%ZNLAT\"' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%ZSLAT\"' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%IFLEV\"' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read allocation status of: \"DATA%ZVERT\"' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lbound of: \"DATA%ZVERT\"' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read ubound of: \"DATA%ZVERT\"' )
    CASE (13)
      TMP1 = REPEAT(' ',32)
      READ(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%ZVERT('//TRIM(ADJUSTL(TMP1))//')\"' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NHTYP\"' )
    CASE (15)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NSTTYP\"' )
    CASE (16)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%RMUCEN\"' )
    CASE (17)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%RLOCEN\"' )
    CASE (18)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%RSTRET\"' )
    CASE (19)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read allocation status of: \"DATA%ILOENG\"' )
    CASE (20)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lbound of: \"DATA%ILOENG\"' )
    CASE (21)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read ubound of: \"DATA%ILOENG\"' )
    CASE (22)
      TMP1 = REPEAT(' ',32)
      READ(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%ILOENG('//TRIM(ADJUSTL(TMP1))//')\"' )
    CASE (23)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read allocation status of: \"DATA%NSFLEVS\"' )
    CASE (24)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lbound(1) of: \"DATA%NSFLEVS\"' )
    CASE (25)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read ubound(1) of: \"DATA%NSFLEVS\"' )
    CASE (26)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lbound(2) of: \"DATA%NSFLEVS\"' )
    CASE (27)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read ubound(2) of: \"DATA%NSFLEVS\"' )
    CASE (28)
      TMP1 = REPEAT(' ',32)
      READ(TMP1,'(I10)') I
      TMP2 = REPEAT(' ',32)
      READ(TMP2,'(I10)') J
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NSFLEVS('//TRIM(ADJUSTL(TMP1))//':'//TRIM(ADJUSTL(TMP2))//')\"' )
    CASE (29)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate: \"DATA%ZVERT\"' )
    CASE (30)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate: \"DATA%ILOENG\"' )
    CASE (31)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate: \"DATA%NSFLEVS\"' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE READ_GEO_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE







#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_SAT_PAR'
SUBROUTINE READ_SAT_PAR( DATA, UNIT, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: SAT_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(SAT_PAR_T),      INTENT(INOUT) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  LOGICAL,              INTENT(IN)    :: VERBOSE

  ! Local variables
  REAL(KIND=REAL64)    :: RTMP
  INTEGER(KIND=INT64)  :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=INT64) :: LB1
  INTEGER(KIND=INT64) :: UB1
  INTEGER(KIND=JPIB_K) :: I
  LOGICAL :: IS_MSERIES_ALLOCATED
  LOGICAL :: IS_MSATID_ALLOCATED
  LOGICAL :: IS_MINST_ALLOCATED
  LOGICAL :: IS_MCHAN_ALLOCATED
  LOGICAL :: IS_RCWN_ALLOCATED


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( ALLOCATED(DATA%MSERIES) ) THEN
    DEALLOCATE(DATA%MSERIES)
  ENDIF
  IF ( ALLOCATED(DATA%MSATID) ) THEN
    DEALLOCATE(DATA%MSATID)
  ENDIF
  IF ( ALLOCATED(DATA%MINST) ) THEN
    DEALLOCATE(DATA%MINST)
  ENDIF
  IF ( ALLOCATED(DATA%MCHAN) ) THEN
    DEALLOCATE(DATA%MCHAN)
  ENDIF
  IF ( ALLOCATED(DATA%RCWN) ) THEN
    DEALLOCATE(DATA%RCWN)
  ENDIF

  READ(UNIT, IOSTAT=STAT) IS_MSERIES_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SAT_%IS_MSERIES_ALLOCATED: ', IS_MSERIES_ALLOCATED, STAT
  ENDIF
  IF ( IS_MSERIES_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%UB1: ', UB1, STAT
    ENDIF
    ALLOCATE( DATA%MSERIES(LB1:UB1), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4 )
    DO I = LBOUND(DATA%MSERIES,1), UBOUND(DATA%MSERIES,1)
      READ(UNIT, IOSTAT=STAT) ITMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 5 )
      IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%GEO_%MSERIES(', I,'): ', ITMP, STAT
      ENDIF
      DATA%MSERIES(I) = INT( ITMP, KIND(DATA%MSERIES(I)))
    ENDDO
  ENDIF


  READ(UNIT, IOSTAT=STAT) IS_MSATID_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 6 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SAT_%IS_MSATID_ALLOCATED: ', IS_MSATID_ALLOCATED, STAT
  ENDIF
  IF ( IS_MSATID_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 8 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%UB1: ', UB1, STAT
    ENDIF
    ALLOCATE( DATA%MSATID(LB1:UB1), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 9 )
    DO I = LBOUND(DATA%MSATID,1), UBOUND(DATA%MSATID,1)
      READ(UNIT, IOSTAT=STAT) ITMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 10 )
      IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%GEO_%MSATID(', I,'): ', ITMP, STAT
      ENDIF
      DATA%MSATID(I) = INT( ITMP, KIND(DATA%MSATID(I)))
    ENDDO
  ENDIF


  READ(UNIT, IOSTAT=STAT) IS_MINST_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 11 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SAT_%IS_MINST_ALLOCATED: ', IS_MINST_ALLOCATED, STAT
  ENDIF
  IF ( IS_MINST_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 12 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 13 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%UB1: ', UB1, STAT
    ENDIF
    ALLOCATE( DATA%MINST(LB1:UB1), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 14 )
    DO I = LBOUND(DATA%MINST,1), UBOUND(DATA%MINST,1)
      READ(UNIT, IOSTAT=STAT) ITMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 15 )
      IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%GEO_%MINST(', I,'): ', ITMP, STAT
      ENDIF
      DATA%MINST(I) = INT( ITMP, KIND(DATA%MINST(I)))
    ENDDO
  ENDIF


  READ(UNIT, IOSTAT=STAT) IS_MCHAN_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 16 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SAT_%IS_MCHAN_ALLOCATED: ', IS_MCHAN_ALLOCATED, STAT
  ENDIF
  IF ( IS_MCHAN_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 17 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 18 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%UB1: ', UB1, STAT
    ENDIF
    ALLOCATE( DATA%MCHAN(LB1:UB1), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 19 )
    DO I = LBOUND(DATA%MCHAN,1), UBOUND(DATA%MCHAN,1)
      READ(UNIT, IOSTAT=STAT) ITMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 20 )
      IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%GEO_%MCHAN(', I,'): ', ITMP, STAT
      ENDIF
      DATA%MCHAN(I) = INT( ITMP, KIND(DATA%MCHAN(I)))
    ENDDO
  ENDIF


  READ(UNIT, IOSTAT=STAT) IS_RCWN_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 21 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SAT_%IS_RCWN_ALLOCATED: ', IS_RCWN_ALLOCATED, STAT
  ENDIF
  IF ( IS_RCWN_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 22 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 23 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%UB1: ', UB1, STAT
    ENDIF
    ALLOCATE( DATA%RCWN(LB1:UB1), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 24 )
    DO I = LBOUND(DATA%RCWN,1), UBOUND(DATA%RCWN,1)
      READ(UNIT, IOSTAT=STAT) RTMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 25 )
      IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%GEO_%RCWN(', I,'): ', ITMP, STAT
      ENDIF
      DATA%RCWN(I) = REAL( RTMP, KIND(DATA%RCWN(I)))
    ENDDO
  ENDIF
  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=32) :: TMP1
    CHARACTER(LEN=32) :: TMP2

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read allocation status of: \"DATA%MSERIES\"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lbound of: \"DATA%MSERIES\"' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read ubound of: \"DATA%MSERIES\"' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate data: \"DATA%MSERIES\"' )
    CASE (5)
      TMP1 = REPEAT(' ',32)
      READ(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%MSERIES('//TRIM(ADJUSTL(TMP1))//')\"' )

    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read allocation status of: \"DATA%MSATID\"' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lbound of: \"DATA%MSATID\"' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read ubound of: \"DATA%MSATID\"' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate data: \"DATA%MSATID\"' )
    CASE (10)
      TMP1 = REPEAT(' ',32)
      READ(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%MSATID('//TRIM(ADJUSTL(TMP1))//')\"' )

    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read allocation status of: \"DATA%MINST\"' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lbound of: \"DATA%MINST\"' )
    CASE (13)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read ubound of: \"DATA%MINST\"' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate data: \"DATA%MINST\"' )
    CASE (15)
      TMP1 = REPEAT(' ',32)
      READ(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%MINST('//TRIM(ADJUSTL(TMP1))//')\"' )

    CASE (16)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read allocation status of: \"DATA%MCHAN\"' )
    CASE (17)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lbound of: \"DATA%MCHAN\"' )
    CASE (18)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read ubound of: \"DATA%MCHAN\"' )
    CASE (19)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate data: \"DATA%MCHAN\"' )
    CASE (20)
      TMP1 = REPEAT(' ',32)
      READ(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%MCHAN('//TRIM(ADJUSTL(TMP1))//')\"' )

    CASE (21)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read allocation status of: \"DATA%RCWN\"' )
    CASE (22)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lbound of: \"DATA%RCWN\"' )
    CASE (23)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read ubound of: \"DATA%RCWN\"' )
    CASE (24)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate data: \"DATA%RCWN\"' )
    CASE (25)
      TMP1 = REPEAT(' ',32)
      READ(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%RCWN('//TRIM(ADJUSTL(TMP1))//')\"' )

    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE READ_SAT_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'READ_WAM_PAR'
SUBROUTINE READ_WAM_PAR( DATA, UNIT, VERBOSE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: WAM_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(WAM_PAR_T),      INTENT(INOUT) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  LOGICAL,              INTENT(IN)    :: VERBOSE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: I
  REAL(KIND=REAL64)   :: RTMP
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=INT64) :: ILO
  INTEGER(KIND=INT64) :: IHI
  LOGICAL :: IS_ALLOCATED_FR
  LOGICAL :: IS_ALLOCATED_TH
  LOGICAL :: IS_ALLOCATED_NLONRGG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  READ(UNIT, IOSTAT=STAT) IS_ALLOCATED_FR
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IS_ALLOCATED_FR ', IS_ALLOCATED_FR, STAT
  ENDIF

  IF ( IS_ALLOCATED_FR ) THEN
    READ(UNIT, IOSTAT=STAT) ILO
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%WAM_%ILO: ', ILO, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) IHI
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )
    IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%WAM_%IHI: ', IHI, STAT
    ENDIF
    IF ( ALLOCATED(DATA%FR) ) THEN
      DEALLOCATE(DATA%FR)
    ENDIF
    ALLOCATE( DATA%FR(ILO:IHI), STAT=STAT )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 62 )
    DO I = LBOUND(DATA%FR,1), UBOUND(DATA%FR,1)
      READ(UNIT, IOSTAT=STAT) RTMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 4 )
      IF ( VERBOSE ) THEN
        WRITE(*,'(A,I8,A,F11.4,I8)') ' + PAR%WAM_%FR(', I, ')= ', RTMP, STAT
      ENDIF
      DATA%FR(I) = REAL( RTMP, KIND(DATA%FR(I)))
    ENDDO
  ENDIF


  READ(UNIT, IOSTAT=STAT) IS_ALLOCATED_TH
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 5 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IS_ALLOCATED_TH ', IS_ALLOCATED_TH, STAT
  ENDIF

  IF ( IS_ALLOCATED_TH ) THEN
    READ(UNIT, IOSTAT=STAT) ILO
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 6 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%WAM_%ILO: ', ILO, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) IHI
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 7 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%WAM_%IHI: ', IHI, STAT
    ENDIF
    IF ( ALLOCATED(DATA%TH) ) THEN
      DEALLOCATE(DATA%TH)
    ENDIF
    ALLOCATE( DATA%TH(ILO:IHI), STAT=STAT )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 63 )
    DO I = LBOUND(DATA%TH,1), UBOUND(DATA%TH,1)
      READ(UNIT, IOSTAT=STAT) RTMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 8 )
      IF ( VERBOSE ) THEN
        WRITE(*,'(A,I8,A,F11.4,I8)') ' + PAR%WAM_%TH(', I, ')= ', RTMP, STAT
      ENDIF
      DATA%TH(I) = REAL( RTMP, KIND(DATA%TH(I)) )
    ENDDO
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%LWCOUSAMEGRID
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 9 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%LWCOUSAMEGRID: ', DATA%LWCOUSAMEGRID, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%CLDOMAIN
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 10 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%CLDOMAIN: ', DATA%CLDOMAIN, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 11 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 10 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NGX: ', ITMP, STAT
  ENDIF
  DATA%NGX = INT( ITMP, KIND(DATA%NGX) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 12 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NGY: ', ITMP, STAT
  ENDIF
  DATA%NGY = INT( ITMP, KIND(DATA%NGY) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 13 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NANG: ', ITMP, STAT
  ENDIF
  DATA%NANG = INT( ITMP, KIND(DATA%NANG) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 14 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NFRE_RED: ', ITMP, STAT
  ENDIF
  DATA%NFRE_RED = INT( ITMP, KIND(DATA%NFRE_RED) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 15 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IMDLGRBID_G: ', ITMP, STAT
  ENDIF
  DATA%IMDLGRBID_G = INT( ITMP, KIND(DATA%IMDLGRBID_G) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 16 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IMDLGRBID_M: ', ITMP, STAT
  ENDIF
  DATA%IMDLGRBID_M = INT( ITMP, KIND(DATA%IMDLGRBID_M) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 17 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NDATE_TIME_WINDOW_END: ', ITMP, STAT
  ENDIF
  DATA%NDATE_TIME_WINDOW_END = INT( ITMP, KIND(DATA%NDATE_TIME_WINDOW_END) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 18 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NWINOFF: ', ITMP, STAT
  ENDIF
  DATA%NWINOFF = INT( ITMP, KIND(DATA%NWINOFF) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 19 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NGRIB_VERSION: ', ITMP, STAT
  ENDIF
  DATA%NGRIB_VERSION = INT( ITMP, KIND(DATA%NGRIB_VERSION) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 20 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NTENCODE: ', ITMP, STAT
  ENDIF
  DATA%NTENCODE = INT( ITMP, KIND(DATA%NTENCODE) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 21 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NGRBRESI: ', ITMP, STAT
  ENDIF
  DATA%NGRBRESI = INT( ITMP, KIND(DATA%NGRBRESI) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 22 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NGRBRESS: ', ITMP, STAT
  ENDIF
  DATA%NGRBRESS = INT( ITMP, KIND(DATA%NGRBRESS) )


  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 23 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%PPMISS: ', RTMP, STAT
  ENDIF
  DATA%PPMISS = REAL( RTMP, KIND(DATA%PPMISS) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 24 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%PPEPS: ', RTMP, STAT
  ENDIF
  DATA%PPEPS = REAL( RTMP, KIND(DATA%PPEPS) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 25 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%PPREC: ', RTMP, STAT
  ENDIF
  DATA%PPREC = REAL( RTMP, KIND(DATA%PPREC) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 26 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%PPRESOL: ', RTMP, STAT
  ENDIF
  DATA%PPRESOL = REAL( RTMP, KIND(DATA%PPRESOL) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 27 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%PPMIN_RESET: ', RTMP, STAT
  ENDIF
  DATA%PPMIN_RESET = REAL( RTMP, KIND(DATA%PPMIN_RESET) )


  READ(UNIT, IOSTAT=STAT) DATA%HOPERI
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 28 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%HOPERI: ', DATA%HOPERI, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%HOPERS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 29 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%HOPERS: ', DATA%HOPERS, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%LGRHDIFS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 30 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%LGRHDIFS: ', DATA%LGRHDIFS, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%LNEWLVTP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 31 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%LNEWLVTP: ', DATA%LNEWLVTP, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%LPADPOLES
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 32 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%LPADPOLES: ', DATA%LPADPOLES, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%LL_GRID_SIMPLE_MATRIX
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 33 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%LL_GRID_SIMPLE_MATRIX: ', DATA%LL_GRID_SIMPLE_MATRIX, STAT
  ENDIF




  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 34 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IRGG: ', ITMP, STAT
  ENDIF
  DATA%IRGG = INT( ITMP, KIND(DATA%IRGG) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 35 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IQGAUSS: ', ITMP, STAT
  ENDIF
  DATA%IQGAUSS = INT( ITMP, KIND(DATA%IQGAUSS) )


  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 36 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%AMOWEP: ', ITMP, STAT
  ENDIF
  DATA%AMOWEP = REAL( RTMP, KIND(DATA%AMOWEP) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 37 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%AMOSOP: ', RTMP, STAT
  ENDIF
  DATA%AMOSOP = REAL( RTMP, KIND(DATA%AMOSOP) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 38 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%AMOEAP: ', RTMP, STAT
  ENDIF
  DATA%AMOEAP = REAL( RTMP, KIND(DATA%AMOEAP) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 39 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%AMONOP: ', RTMP, STAT
  ENDIF
  DATA%AMONOP = REAL( RTMP, KIND(DATA%AMONOP) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 40 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%XDELLA: ', RTMP, STAT
  ENDIF
  DATA%XDELLA = REAL( RTMP, KIND(DATA%XDELLA) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 41 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%XDELLO: ', RTMP, STAT
  ENDIF
  DATA%XDELLO = REAL( RTMP, KIND(DATA%XDELLO) )



  READ(UNIT, IOSTAT=STAT) IS_ALLOCATED_NLONRGG
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 42 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IS_ALLOCATED_NLONRGG: ', IS_ALLOCATED_NLONRGG, STAT
  ENDIF

  IF ( IS_ALLOCATED_NLONRGG ) THEN
    READ(UNIT, IOSTAT=STAT) ILO
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 43 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%WAM_%ILO: ', ILO, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) IHI
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 44 )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%WAM_%IHI: ', IHI, STAT
    ENDIF
    IF ( ALLOCATED(DATA%NLONRGG) ) THEN
      DEALLOCATE(DATA%NLONRGG)
    ENDIF
    ALLOCATE( DATA%NLONRGG(ILO:IHI), STAT=STAT )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 64 )
    DO I = LBOUND(DATA%NLONRGG,1), UBOUND(DATA%NLONRGG,1)
      READ(UNIT, IOSTAT=STAT) ITMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 45 )
      IF ( VERBOSE ) THEN
        WRITE(*,'(A,I8,A,F11.4,I8)') ' + PAR%WAM_%NLONRGG(', I ,') = ', RTMP, STAT
      ENDIF
      DATA%NLONRGG(I) = INT( ITMP, KIND(DATA%NLONRGG(I)))
    ENDDO
  ENDIF

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 46 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%ZMISS: ', RTMP, STAT
  ENDIF
  DATA%ZMISS = REAL( RTMP, KIND(DATA%ZMISS) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 47 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NENSFNB: ', ITMP, STAT
  ENDIF
  DATA%NENSFNB = INT( ITMP, KIND(DATA%NENSFNB) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 48 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NTOTENS: ', ITMP, STAT
  ENDIF
  DATA%NTOTENS = INT( ITMP, KIND(DATA%NTOTENS) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 49 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NSYSNB: ', ITMP, STAT
  ENDIF
  DATA%NSYSNB = INT( ITMP, KIND(DATA%NSYSNB) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 50 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NMETNB: ', ITMP, STAT
  ENDIF
  DATA%NMETNB = INT( ITMP, KIND(DATA%NMETNB) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 51 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%ISTREAM: ', ITMP, STAT
  ENDIF
  DATA%ISTREAM = INT( ITMP, KIND(DATA%ISTREAM) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 52 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NLOCGRB: ', ITMP, STAT
  ENDIF
  DATA%NLOCGRB = INT( ITMP, KIND(DATA%NLOCGRB) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 53 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NCONSENSUS: ', ITMP, STAT
  ENDIF
  DATA%NCONSENSUS = INT( ITMP, KIND(DATA%NCONSENSUS) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 54 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NDWD: ', ITMP, STAT
  ENDIF
  DATA%NDWD = INT( ITMP, KIND(DATA%NDWD) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 55 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NMFR: ', ITMP, STAT
  ENDIF
  DATA%NMFR = INT( ITMP, KIND(DATA%NMFR) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 56 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NNCEP: ', ITMP, STAT
  ENDIF
  DATA%NNCEP = INT( ITMP, KIND(DATA%NNCEP) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 57 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NUKM: ', ITMP, STAT
  ENDIF
  DATA%NUKM = INT( ITMP, KIND(DATA%NUKM) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 58 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IREFDATE: ', ITMP, STAT
  ENDIF
  DATA%IREFDATE = INT( ITMP, KIND(DATA%IREFDATE) )

  READ(UNIT, IOSTAT=STAT) DATA%MARSTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 59 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%MARSTYPE: ', DATA%MARSTYPE, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%YCLASS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 60 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%YCLASS: ', DATA%YCLASS, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%YEXPVER
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 61 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%YEXPVER: ', DATA%YEXPVER, STAT
  ENDIF



  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 65 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NSPEC2TAB: ', ITMP, STAT
  ENDIF
  DATA%NSPEC2TAB = INT( ITMP, KIND(DATA%NSPEC2TAB) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 66 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NSPEC2TMPD: ', ITMP, STAT
  ENDIF
  DATA%NSPEC2TMPD = INT( ITMP, KIND(DATA%NSPEC2TMPD) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 67 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NSPEC2TMPP: ', ITMP, STAT
  ENDIF
  DATA%NSPEC2TMPP = INT( ITMP, KIND(DATA%NSPEC2TMPP) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 68 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NTRG2TMPD: ', ITMP, STAT
  ENDIF
  DATA%NTRG2TMPD = INT( ITMP, KIND(DATA%NTRG2TMPD) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 69 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NTRG2TMPP: ', ITMP, STAT
  ENDIF
  DATA%NTRG2TMPP = INT( ITMP, KIND(DATA%NTRG2TMPP) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 70 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%ITMIN: ', ITMP, STAT
  ENDIF
  DATA%ITMIN = INT( ITMP, KIND(DATA%ITMIN) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 71 )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%ITMAX: ', ITMP, STAT
  ENDIF
  DATA%ITMAX = INT( ITMP, KIND(DATA%ITMAX) )


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=32) :: TMP1

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read allocation status: \"DATA%FR\"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lbound: \"DATA%FR\"' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read ubound: \"DATA%FR\"' )
    CASE (4)
      TMP1 = REPEAT(' ',32)
      READ(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%FR('//TRIM(ADJUSTL(TMP1))//')\"' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read allocation status: \"DATA%TH\"' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lbound: \"DATA%TH\"' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read ubound: \"DATA%TH\"' )
    CASE (8)
      TMP1 = REPEAT(' ',32)
      READ(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%TH('//TRIM(ADJUSTL(TMP1))//')\"' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%LWCOUSAMEGRID\"' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%CLDOMAIN\"' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NGX\"' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NGY\"' )
    CASE (13)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NANG\"' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NFRE_RED\"' )
    CASE (15)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%IMDLGRBID_G\"' )
    CASE (16)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%IMDLGRBID_M\"' )
    CASE (17)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NDATE_TIME_WINDOW_END\"' )
    CASE (18)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NWINOFF\"' )
    CASE (19)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NGRIB_VERSION\"' )
    CASE (20)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NTENCODE\"' )
    CASE (21)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NGRBRESI\"' )
    CASE (22)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NGRBRESS\"' )
    CASE (23)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%PPMISS\"' )
    CASE (24)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%PPEPS\"' )
    CASE (25)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%PPREC\"' )
    CASE (26)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%PPRESOL\"' )
    CASE (27)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%PPMIN_RESET\"' )
    CASE (28)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%HOPERI\"' )
    CASE (29)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%HOPERS\"' )
    CASE (30)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%LGRHDIFS\"' )
    CASE (31)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%LNEWLVTP\"' )
    CASE (32)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%LPADPOLES\"' )
    CASE (33)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%LL_GRID_SIMPLE_MATRIX\"' )
    CASE (34)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%IRGG\"' )
    CASE (35)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%IQGAUSS\"' )
    CASE (36)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%AMOWEP\"' )
    CASE (37)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%AMOSOP\"' )
    CASE (38)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%AMOEAP\"' )
    CASE (39)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%AMONOP\"' )
    CASE (40)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%XDELLA\"' )
    CASE (41)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%XDELLO\"' )
    CASE (42)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read allocation status: \"DATA%NLONRGG\"' )
    CASE (43)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lboud: \"DATA%NLONRGG\"' )
    CASE (44)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read ubound: \"DATA%NLONRGG\"' )
    CASE (45)
      TMP1 = REPEAT(' ',32)
      READ(TMP1,'(I10)') I
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NLONRGG('//TRIM(ADJUSTL(TMP1))//')\"' )
    CASE (46)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%ZMISS\"' )
    CASE (47)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NENSFNB\"' )
    CASE (48)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NTOTENS\"' )
    CASE (49)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NSYSNB\"' )
    CASE (50)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NMETNB\"' )
    CASE (51)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%ISTREAM\"' )
    CASE (52)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NLOCGRB\"' )
    CASE (53)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NCONSENSUS\"' )
    CASE (54)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NDWD\"' )
    CASE (55)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NMFR\"' )
    CASE (56)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NNCEP\"' )
    CASE (57)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NUKM\"' )
    CASE (58)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%IREFDATE\"' )
    CASE (59)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%MARSTYPE\"' )
    CASE (60)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%YCLASS\"' )
    CASE (61)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%YEXPVER\"' )
    CASE (62)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate: \"DATA%FR\"' )
    CASE (63)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate: \"DATA%TH\"' )
    CASE (64)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate: \"DATA%NLONRGG\"' )
    CASE (65)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NSPEC2TAB\"' )
    CASE (66)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NSPEC2TMPD\"' )
    CASE (67)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NSPEC2TMPP\"' )
    CASE (68)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NTRG2TMPD\"' )
    CASE (69)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%NTRG2TMPP\"' )
    CASE (70)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%IMDLGRBID_G\"' )
    CASE (71)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read: \"DATA%IMDLGRBID_M\"' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE READ_WAM_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'L2I'
FUNCTION L2I( L ) RESULT(I)
  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
IMPLICIT NONE
LOGICAL, INTENT(IN) :: L
INTEGER(KIND=JPIB_K) :: I
IF ( L ) THEN
  I = 1_JPIB_K
ELSE
  I = 0_JPIB_K
ENDIF
END FUNCTION L2I
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'I2L'
FUNCTION I2L( I ) RESULT(L)
  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
IMPLICIT NONE
INTEGER(KIND=JPIB_K), INTENT(IN) :: I
LOGICAL :: L
IF ( I.EQ.0 ) THEN
  L = .FALSE.
ELSE
  L = .TRUE.
ENDIF
END FUNCTION I2L
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE PAR_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
