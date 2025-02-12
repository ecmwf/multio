!> @file om_values_mod.F90
!>
!> @brief Definition of a message for the output manager
!>
!> This module defines the message used by the output manager
!> and some utitlities to handle these messages
!>
!> @author Mirco Valentini
!> @date February 10, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


! Definition of the module
#define PP_FILE_NAME 'ifs_val_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'IFS_VAL_MOD'
MODULE IFS_VAL_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: VAL_CREATE_NAME
PUBLIC :: VAL_WOPEN
PUBLIC :: VAL_ROPEN
PUBLIC :: VAL_CLOSE
PUBLIC :: VAL_WRITE_SP
PUBLIC :: VAL_WRITE_DP
PUBLIC :: VAL_READ_SP
PUBLIC :: VAL_READ_DP
PUBLIC :: VAL_GENERATE_SP
PUBLIC :: VAL_GENERATE_DP

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'VAL_CREATE_NAME'
PP_THREAD_SAFE FUNCTION VAL_CREATE_NAME( DIRECTORY, MSG_ID, PROC_ID, VALFNAME, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: DIRECTORY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: MSG_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PROC_ID
  CHARACTER(LEN=*),     INTENT(OUT)   :: VALFNAME
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: VALEXIST
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: M
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_SIZE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_FAILED=2_JPIB_K

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

  ! Error handling
  N = LEN(VALFNAME)
  M = LEN_TRIM(DIRECTORY) + 26
  PP_DEBUG_CRITICAL_COND_THROW( M.GT.N, ERRFLAG_WRONG_SIZE)

  ! Create the message name
  VALFNAME = REPEAT(' ',N)
  WRITE(VALFNAME,'(A,A,I8.8,A,I8.8,A)', IOSTAT=STAT) TRIM(ADJUSTL(DIRECTORY)), '/val_', MSG_ID, '_', PROC_ID, '.bin'
  PP_DEBUG_CRITICAL_COND_THROW(STAT.NE.0, ERRFLAG_WRITE_FAILED)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_WRONG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The file name is too short' )
    CASE (ERRFLAG_WRITE_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The file name could not be written' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION VAL_CREATE_NAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION '
#define PP_PROCEDURE_NAME 'VAL_WOPEN'
PP_THREAD_SAFE FUNCTION  VAL_WOPEN( VALFNAME, VALUNIT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: VALFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: VALUNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: VALEXIST
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_NOT_FOUND=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_OPEN_FAILED=2_JPIB_K

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

  ! Check if the file is opened
  INQUIRE( FILE=TRIM(VALFNAME), EXIST=VALEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( VALEXIST, ERRFLAG_FILE_NOT_FOUND)

  ! Open the TOC file
  OPEN( NEWUNIT=VALUNIT, FILE=TRIM(VALFNAME), STATUS='REPLACE', &
&       ACCESS='STREAM', ACTION='WRITE', FORM='UNFORMATTED', IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_FILE_OPEN_FAILED)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_FILE_NOT_FOUND)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The file does not exist' )
    CASE (ERRFLAG_FILE_OPEN_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The file could not be opened' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION  VAL_WOPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION '
#define PP_PROCEDURE_NAME 'VAL_ROPEN'
PP_THREAD_SAFE FUNCTION  VAL_ROPEN( VALFNAME, VALUNIT, BIG_ENDIAN_READ, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: VALFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: VALUNIT
  LOGICAL,              INTENT(IN)    :: BIG_ENDIAN_READ
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: VALEXIST
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_NOT_FOUND=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_OPEN_FAILED=2_JPIB_K

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

  ! Check if the file is opened
  INQUIRE( FILE=TRIM(VALFNAME), EXIST=VALEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.VALEXIST, ERRFLAG_FILE_NOT_FOUND)

  ! Open the TOC file
  IF ( BIG_ENDIAN_READ ) THEN
    OPEN( NEWUNIT=VALUNIT, FILE=TRIM(VALFNAME), STATUS='OLD', &
&         ACCESS='STREAM', ACTION='READ', FORM='UNFORMATTED', CONVERT='BIG_ENDIAN', IOSTAT=STAT )
  ELSE
    OPEN( NEWUNIT=VALUNIT, FILE=TRIM(VALFNAME), STATUS='OLD', &
&         ACCESS='STREAM', ACTION='READ', FORM='UNFORMATTED', IOSTAT=STAT )
  ENDIF
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_FILE_OPEN_FAILED)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_FILE_NOT_FOUND)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The file does not exist' )
    CASE (ERRFLAG_FILE_OPEN_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The file could not be opened' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION  VAL_ROPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION '
#define PP_PROCEDURE_NAME 'VAL_CLOSE'
PP_THREAD_SAFE FUNCTION  VAL_CLOSE( VALUNIT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VALUNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: VALOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_NOT_OPENED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_CLOSE_FAILED=2_JPIB_K

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

  ! Check if the file is opened
  INQUIRE( UNIT=VALUNIT, OPENED=VALOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.VALOPENED, ERRFLAG_FILE_NOT_OPENED)

  ! Open the TOC file
  CLOSE( UNIT=VALUNIT, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_FILE_CLOSE_FAILED)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_FILE_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The file is not opened' )
    CASE (ERRFLAG_FILE_CLOSE_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The file could not be closed' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION  VAL_CLOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION '
#define PP_PROCEDURE_NAME 'VAL_WRITE_SP'
PP_THREAD_SAFE FUNCTION  VAL_WRITE_SP( VALUNIT, VALUES, WRITE_POS, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRM_K
  USE :: ENUMERATORS_MOD,   ONLY: VALUES_SP_E
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),            INTENT(IN)    :: VALUNIT
  REAL(KIND=JPRM_K), DIMENSION(:), INTENT(IN)    :: VALUES
  INTEGER(KIND=INT64),             INTENT(OUT)   :: WRITE_POS
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: VALOPENED
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local parameters
  CHARACTER(LEN=*), PARAMETER :: GUARD='GUARD_SP'

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_NOT_OPENED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_POSITION=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_FAILED=3_JPIB_K

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

  ! Check if the file is opened
  INQUIRE( UNIT=VALUNIT, OPENED=VALOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.VALOPENED, ERRFLAG_FILE_NOT_OPENED)

  ! Get the position in the file
  INQUIRE( VALUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_GET_POSITION)

  ! Write the current step of the simulation and trigger restart
  WRITE( VALUNIT, IOSTAT=STAT ) INT( VALUES_SP_E, INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_WRITE_FAILED)
  WRITE( VALUNIT, IOSTAT=STAT ) INT( LBOUND(VALUES,1), INT64 )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_WRITE_FAILED)
  WRITE( VALUNIT, IOSTAT=STAT ) INT( UBOUND(VALUES,1), INT64 )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_WRITE_FAILED)

  ! Write the current step
  DO I = LBOUND(VALUES,1), UBOUND(VALUES,1)
    WRITE( VALUNIT, IOSTAT=STAT ) REAL(VALUES(I), REAL32)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_WRITE_FAILED)
  ENDDO

  ! Write guards
  WRITE( VALUNIT, IOSTAT=STAT ) GUARD
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_WRITE_FAILED)

  ! Perform a flush just to be sure
  FLUSH( VALUNIT )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_FILE_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The file is not opened' )
    CASE (ERRFLAG_UNABLE_TO_GET_POSITION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the position in the file' )
    CASE (ERRFLAG_WRITE_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the values' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION  VAL_WRITE_SP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION '
#define PP_PROCEDURE_NAME 'VAL_READ_SP'
PP_THREAD_SAFE FUNCTION  VAL_READ_SP( VALUNIT, READ_POS, VALUES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRM_K
  USE :: ENUMERATORS_MOD,   ONLY: VALUES_SP_E
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),            INTENT(IN)    :: VALUNIT
  INTEGER(KIND=JPIB_K),            INTENT(IN)    :: READ_POS
  REAL(KIND=JPRM_K), DIMENSION(:), INTENT(OUT)   :: VALUES
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: VALOPENED
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=INT64)  :: PRECISION
  INTEGER(KIND=INT64)  :: LO
  INTEGER(KIND=INT64)  :: HI
  REAL(KIND=REAL32)    :: RTMP
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=8)     :: GUARD

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_NOT_OPENED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_PRECISION=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_LO=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_HI=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_GUARD=6_JPIB_K

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

  ! Check if the file is opened
  INQUIRE( UNIT=VALUNIT, OPENED=VALOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.VALOPENED, ERRFLAG_FILE_NOT_OPENED)

  ! Write the current step of the simulation and trigger restart
  READ( VALUNIT, POS=READ_POS, IOSTAT=STAT ) PRECISION
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  PP_DEBUG_CRITICAL_COND_THROW( PRECISION.NE.VALUES_SP_E, ERRFLAG_WRONG_PRECISION )
  READ( VALUNIT, IOSTAT=STAT ) LO
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  READ( VALUNIT, IOSTAT=STAT ) HI
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)

  ! Check memory size/status
  PP_DEBUG_CRITICAL_COND_THROW( LBOUND(VALUES,1).NE.LO, ERRFLAG_WRONG_LO)
  PP_DEBUG_CRITICAL_COND_THROW( UBOUND(VALUES,1).NE.HI, ERRFLAG_WRONG_HI)

  ! Write the current step
  DO I = LO, HI
    READ( VALUNIT, IOSTAT=STAT ) RTMP
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
    VALUES(I) = REAL( RTMP, KIND(VALUES(I)) )
  ENDDO

  ! Read the guard
  READ( VALUNIT, IOSTAT=STAT ) GUARD
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  PP_DEBUG_CRITICAL_COND_THROW( GUARD.NE.'GUARD_SP', ERRFLAG_WRONG_GUARD)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_FILE_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The file is not opened' )
    CASE (ERRFLAG_UNABLE_TO_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the values' )
    CASE (ERRFLAG_WRONG_PRECISION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong precision' )
    CASE (ERRFLAG_WRONG_LO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong lower bound' )
    CASE (ERRFLAG_WRONG_HI)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong upper bound' )
    CASE (ERRFLAG_WRONG_GUARD)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong guard' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Got: "'//TRIM(ADJUSTL(GUARD))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Expected: "GUARD_SP"' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION  VAL_READ_SP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION '
#define PP_PROCEDURE_NAME 'VAL_WRITE_DP'
PP_THREAD_SAFE FUNCTION  VAL_WRITE_DP( VALUNIT, VALUES, WRITE_POS, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: ENUMERATORS_MOD,   ONLY: VALUES_DP_E
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),            INTENT(IN)    :: VALUNIT
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(IN)    :: VALUES
  INTEGER(KIND=INT64),             INTENT(OUT)   :: WRITE_POS
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: VALOPENED
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local parameters
  CHARACTER(LEN=*), PARAMETER :: GUARD='GUARD_DP'

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_NOT_OPENED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_POSITION=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_FAILED=3_JPIB_K

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

  ! Check if the file is opened
  INQUIRE( UNIT=VALUNIT, OPENED=VALOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.VALOPENED, ERRFLAG_FILE_NOT_OPENED)

  ! Get the position in the file
  INQUIRE( VALUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_GET_POSITION)

  ! Write the current step of the simulation and trigger restart
  WRITE( VALUNIT, IOSTAT=STAT ) INT( VALUES_DP_E, INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_WRITE_FAILED)
  WRITE( VALUNIT, IOSTAT=STAT ) INT( LBOUND(VALUES,1), INT64 )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_WRITE_FAILED)
  WRITE( VALUNIT, IOSTAT=STAT ) INT( UBOUND(VALUES,1), INT64 )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_WRITE_FAILED)

  ! Write the current step
  DO I = LBOUND(VALUES,1), UBOUND(VALUES,1)
    WRITE( VALUNIT, IOSTAT=STAT ) REAL(VALUES(I), REAL64)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_WRITE_FAILED)
  ENDDO

  ! Write guards
  WRITE( VALUNIT, IOSTAT=STAT ) GUARD
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_WRITE_FAILED)

  ! Perform a flush just to be sure
  FLUSH( VALUNIT )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_FILE_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The file is not opened' )
    CASE (ERRFLAG_UNABLE_TO_GET_POSITION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the position in the file' )
    CASE (ERRFLAG_WRITE_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the values' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION  VAL_WRITE_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION '
#define PP_PROCEDURE_NAME 'VAL_READ_DP'
PP_THREAD_SAFE FUNCTION  VAL_READ_DP( VALUNIT, READ_POS, VALUES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: ENUMERATORS_MOD,   ONLY: VALUES_DP_E
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),            INTENT(IN)    :: VALUNIT
  INTEGER(KIND=JPIB_K),            INTENT(IN)    :: READ_POS
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(OUT)   :: VALUES
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET


  ! Local variables
  LOGICAL :: VALOPENED
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=INT64)  :: PRECISION
  INTEGER(KIND=INT64)  :: LO
  INTEGER(KIND=INT64)  :: HI
  REAL(KIND=REAL64)    :: RTMP
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=8)     :: GUARD

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_NOT_OPENED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_PRECISION=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_LO=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_HI=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_GUARD=6_JPIB_K

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

  ! Check if the file is opened
  INQUIRE( UNIT=VALUNIT, OPENED=VALOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.VALOPENED, ERRFLAG_FILE_NOT_OPENED)

  ! Write the current step of the simulation and trigger restart
  READ( VALUNIT, POS=READ_POS, IOSTAT=STAT ) PRECISION
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  PP_DEBUG_CRITICAL_COND_THROW( PRECISION.NE.VALUES_DP_E, ERRFLAG_WRONG_PRECISION )
  READ( VALUNIT, IOSTAT=STAT ) LO
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  READ( VALUNIT, IOSTAT=STAT ) HI
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)

  ! Check memory size/status
  PP_DEBUG_CRITICAL_COND_THROW( LBOUND(VALUES,1).NE.LO, ERRFLAG_WRONG_LO)
  PP_DEBUG_CRITICAL_COND_THROW( UBOUND(VALUES,1).NE.HI, ERRFLAG_WRONG_HI)

  ! Write the current step
  DO I = LO, HI
    READ( VALUNIT, IOSTAT=STAT ) RTMP
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
    VALUES(I) = REAL( RTMP, KIND(VALUES(I)) )
  ENDDO

  ! Read the guard
  READ( VALUNIT, IOSTAT=STAT ) GUARD
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  PP_DEBUG_CRITICAL_COND_THROW( GUARD.NE.'GUARD_DP', ERRFLAG_WRONG_GUARD)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_FILE_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The file is not opened' )
    CASE (ERRFLAG_UNABLE_TO_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the values' )
    CASE (ERRFLAG_WRONG_PRECISION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong precision' )
    CASE (ERRFLAG_WRONG_LO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong lower bound' )
    CASE (ERRFLAG_WRONG_HI)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong upper bound' )
    CASE (ERRFLAG_WRONG_GUARD)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong guard' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Got: "'//TRIM(ADJUSTL(GUARD))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Expected: "GUARD_DP"' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION  VAL_READ_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION '
#define PP_PROCEDURE_NAME 'VAL_GENERATE_SP'
PP_THREAD_SAFE FUNCTION  VAL_GENERATE_SP( NVALUES, MINVAL, MAXVAL, AVGVAL, NUNDEF, XUNDEF, VALUES, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),            INTENT(IN)    :: NVALUES
  REAL(KIND=JPRD_K),               INTENT(IN)    :: MINVAL
  REAL(KIND=JPRD_K),               INTENT(IN)    :: MAXVAL
  REAL(KIND=JPRD_K),               INTENT(IN)    :: AVGVAL
  INTEGER(KIND=JPIB_K),            INTENT(IN)    :: NUNDEF
  REAL(KIND=JPRD_K),               INTENT(IN)    :: XUNDEF
  REAL(KIND=JPRM_K), DIMENSION(:), INTENT(OUT)   :: VALUES
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
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

  VALUES = 1.0_JPRM_K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION  VAL_GENERATE_SP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION '
#define PP_PROCEDURE_NAME 'VAL_GENERATE_DP'
PP_THREAD_SAFE FUNCTION  VAL_GENERATE_DP( NVALUES, MINVAL, MAXVAL, AVGVAL, NUNDEF, XUNDEF, VALUES, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
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

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),            INTENT(IN)    :: NVALUES
  REAL(KIND=JPRD_K),               INTENT(IN)    :: MINVAL
  REAL(KIND=JPRD_K),               INTENT(IN)    :: MAXVAL
  REAL(KIND=JPRD_K),               INTENT(IN)    :: AVGVAL
  INTEGER(KIND=JPIB_K),            INTENT(IN)    :: NUNDEF
  REAL(KIND=JPRD_K),               INTENT(IN)    :: XUNDEF
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(OUT)   :: VALUES
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
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

  VALUES = 1.0_JPRD_K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION  VAL_GENERATE_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE IFS_VAL_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME