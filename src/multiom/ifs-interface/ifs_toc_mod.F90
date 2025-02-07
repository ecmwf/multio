! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'ifs_toc_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'IFS_TOC_MOD'
MODULE IFS_TOC_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

! Default visibility of the module
PRIVATE

! Enumerators
INTEGER(KIND=JPIB_K), PARAMETER :: UNDEF_E=-1
INTEGER(KIND=JPIB_K), PARAMETER :: SIM_INIT_E=0

INTEGER(KIND=JPIB_K), PARAMETER :: ATM_FIELD_E=1
INTEGER(KIND=JPIB_K), PARAMETER :: WAM_FIELD_E=2

INTEGER(KIND=JPIB_K), PARAMETER :: FLUSH_STEP_E=3
INTEGER(KIND=JPIB_K), PARAMETER :: FLUSH_STEP_RST_E=4
INTEGER(KIND=JPIB_K), PARAMETER :: FLUSH_STEP_LAST_E=5

INTEGER(KIND=JPIB_K), PARAMETER :: SIM_END_E=6


TYPE :: TOC_ENTRY_BASE_T
  INTEGER(KIND=JPIB_K) :: TYPE_
  INTEGER(KIND=JPIB_K) :: CLOCK_RATE_
END TYPE

TYPE, EXTENDS(TOC_ENTRY_BASE_T) :: TOC_SIM_INIT_T
  INTEGER(KIND=JPIB_K) :: CLOCK_COUNT_
END TYPE

TYPE, EXTENDS(TOC_ENTRY_BASE_T) :: TOC_ATM_FIELD_T
  INTEGER(KIND=JPIB_K) :: PARAM_ID_
  INTEGER(KIND=JPIB_K) :: U_ID_
  INTEGER(KIND=JPIB_K) :: STEP_ID_
  INTEGER(KIND=JPIB_K) :: PROC_ID_
  INTEGER(KIND=JPIB_K) :: REPRES_ID_
  INTEGER(KIND=JPIB_K) :: PREFIX_ID_
  INTEGER(KIND=JPIB_K) :: MSG_ADDR_
  INTEGER(KIND=JPIB_K) :: VAL_TYPE_
  INTEGER(KIND=JPIB_K) :: VAL_LB_
  INTEGER(KIND=JPIB_K) :: VAL_UB_
  INTEGER(KIND=JPIB_K) :: VAL_ADDR_
  INTEGER(KIND=JPIB_K) :: CLOCK_COUNT_
  INTEGER(KIND=JPIB_K) :: MSG_ID_
END TYPE

TYPE, EXTENDS(TOC_ENTRY_BASE_T) :: TOC_WAM_FIELD_T
  INTEGER(KIND=JPIB_K) :: PARAM_ID_
  INTEGER(KIND=JPIB_K) :: U_ID_
  INTEGER(KIND=JPIB_K) :: STEP_ID_
  INTEGER(KIND=JPIB_K) :: PROC_ID_
  INTEGER(KIND=JPIB_K) :: REPRES_ID_
  INTEGER(KIND=JPIB_K) :: PREFIX_ID_
  INTEGER(KIND=JPIB_K) :: MSG_ADDR_
  INTEGER(KIND=JPIB_K) :: VAL_TYPE_
  INTEGER(KIND=JPIB_K) :: VAL_LB_
  INTEGER(KIND=JPIB_K) :: VAL_UB_
  INTEGER(KIND=JPIB_K) :: VAL_ADDR_
  INTEGER(KIND=JPIB_K) :: CLOCK_COUNT_
  INTEGER(KIND=JPIB_K) :: MSG_ID_
END TYPE

TYPE, EXTENDS(TOC_ENTRY_BASE_T) :: TOC_FLUSH_STEP_T
  INTEGER(KIND=JPIB_K) :: STEP_
  INTEGER(KIND=JPIB_K) :: CLOCK_COUNT_
END TYPE

TYPE, EXTENDS(TOC_ENTRY_BASE_T) :: TOC_FLUSH_STEP_RST_T
  INTEGER(KIND=JPIB_K) :: STEP_
  INTEGER(KIND=JPIB_K) :: CLOCK_COUNT_
END TYPE

TYPE, EXTENDS(TOC_ENTRY_BASE_T) :: TOC_FLUSH_LAST_STEP_T
  INTEGER(KIND=JPIB_K) :: STEP_
  INTEGER(KIND=JPIB_K) :: CLOCK_COUNT_
END TYPE

TYPE, EXTENDS(TOC_ENTRY_BASE_T) :: TOC_SIM_END_T
  INTEGER(KIND=JPIB_K) :: CLOCK_COUNT_
END TYPE

TYPE :: TOC_CONTAINER_T
  CLASS(TOC_ENTRY_BASE_T), POINTER :: ENTRY_ => NULL()
END TYPE

! Whitelist of public symbols (dataypes)
PUBLIC :: TOC_ENTRY_BASE_T
PUBLIC :: TOC_SIM_INIT_T
PUBLIC :: TOC_ATM_FIELD_T
PUBLIC :: TOC_WAM_FIELD_T
PUBLIC :: TOC_FLUSH_STEP_T
PUBLIC :: TOC_FLUSH_STEP_RST_T
PUBLIC :: TOC_FLUSH_LAST_STEP_T
PUBLIC :: TOC_SIM_END_T
PUBLIC :: TOC_CONTAINER_T


!  Whitelist of public symbols (procedures)
PUBLIC :: TOC_CREATE_NAME
PUBLIC :: TOC_WOPEN
PUBLIC :: TOC_CLOSE
PUBLIC :: TOC_WRITE_FLUSH_BEGIN_OF_SIMULATION
PUBLIC :: TOC_WRITE_ATM
PUBLIC :: TOC_WRITE_WAM
PUBLIC :: TOC_WRITE_FLUSH_STEP
PUBLIC :: TOC_WRITE_FLUSH_STEP_AND_RESTART
PUBLIC :: TOC_WRITE_FLUSH_LAST_STEP
PUBLIC :: TOC_WRITE_FLUSH_END_OF_SIMULATION
PUBLIC :: TOC_READ
PUBLIC :: TOC_READ_ALL
PUBLIC :: TOC_FREE

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_CREATE_NAME'
PP_THREAD_SAFE FUNCTION TOC_CREATE_NAME( DIRECTORY, PROC_ID, TOCFNAME, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PROC_ID
  CHARACTER(LEN=*),     INTENT(OUT)   :: TOCFNAME
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: MSGEXIST
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: M
  INTEGER(KIND=JPIB_K) :: STAT

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_NAME_TOO_SHORT = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_FILE_NAME = 2_JPIB_K

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
  N = LEN(TOCFNAME)
  M = LEN_TRIM(DIRECTORY) + 17
  PP_DEBUG_CRITICAL_COND_THROW( M.GT.N, ERRFLAG_FILE_NAME_TOO_SHORT)

  ! Create the message name
  TOCFNAME = REPEAT(' ',N)
  WRITE(TOCFNAME,'(A,A,I8.8,A)', IOSTAT=STAT) TRIM(ADJUSTL(DIRECTORY)), '/toc_', PROC_ID, '.bin'
  PP_DEBUG_CRITICAL_COND_THROW(STAT.NE.0, ERRFLAG_UNABLE_TO_CREATE_FILE_NAME)

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
    CASE (ERRFLAG_FILE_NAME_TOO_SHORT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Values file name variable too short' )
    CASE (ERRFLAG_UNABLE_TO_CREATE_FILE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the msg file name' )
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

END FUNCTION TOC_CREATE_NAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_WOPEN'
PP_THREAD_SAFE FUNCTION TOC_WOPEN( TOCFNAME, TOCUNIT, WRITE_POS, TOC_COUNTER, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  CHARACTER(LEN=*),     INTENT(IN)    :: TOCFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCEXIST
  INTEGER(KIND=JPIB_K) :: STAT

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PAR_FILE_ALREADY_EXISTS = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN_PAR_FILE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_HEADER = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INQUIRE_CURRENT_POSITION = 4_JPIB_K

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
  INQUIRE( FILE=TRIM(TOCFNAME), EXIST=TOCEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( TOCEXIST, ERRFLAG_PAR_FILE_ALREADY_EXISTS)

  ! Open the TOC file
  OPEN( NEWUNIT=TOCUNIT, FILE=TRIM(TOCFNAME), STATUS='REPLACE', &
&       ACCESS='STREAM', ACTION='WRITE', FORM='UNFORMATTED', IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_OPEN_PAR_FILE)

  ! Make spaces for the tocsize
  TOC_COUNTER = 0
  WRITE(TOCUNIT, IOSTAT=STAT) INT(TOC_COUNTER,KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_HEADER)

  ! Get the position inthe file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_INQUIRE_CURRENT_POSITION)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
    CASE (ERRFLAG_PAR_FILE_ALREADY_EXISTS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file already exists' )
    CASE (ERRFLAG_UNABLE_TO_OPEN_PAR_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open toc file' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_HEADER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write first element in the toc file' )
    CASE (ERRFLAG_UNABLE_TO_INQUIRE_CURRENT_POSITION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position in the toc file' )
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

END FUNCTION TOC_WOPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_ROPEN'
PP_THREAD_SAFE FUNCTION TOC_ROPEN( TOCFNAME, TOCUNIT, BIG_ENDIAN_READ, NENTRIES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  CHARACTER(LEN=*),     INTENT(IN)    :: TOCFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: TOCUNIT
  LOGICAL,              INTENT(IN)    :: BIG_ENDIAN_READ
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: NENTRIES
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCEXIST
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FIND_TOC_FILE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN_TOC_FILE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_TOC_ENTRIES = 3_JPIB_K

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
  INQUIRE( FILE=TRIM(TOCFNAME), EXIST=TOCEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCEXIST, ERRFLAG_UNABLE_TO_FIND_TOC_FILE)

  ! Open the TOC file
  IF ( BIG_ENDIAN_READ ) THEN
    OPEN( NEWUNIT=TOCUNIT, FILE=TRIM(TOCFNAME), STATUS='OLD', ACCESS='STREAM', &
&         ACTION='READ', FORM='UNFORMATTED', CONVERT='BIG_ENDIAN', IOSTAT=STAT )
  ELSE
    OPEN( NEWUNIT=TOCUNIT, FILE=TRIM(TOCFNAME), STATUS='OLD', ACCESS='STREAM', &
&         ACTION='READ', FORM='UNFORMATTED', IOSTAT=STAT )
  ENDIF
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_OPEN_TOC_FILE)

  ! Make spaces for the tocsize
  NENTRIES = 0
  READ(TOCUNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_TOC_ENTRIES)
  NENTRIES = INT( ITMP, KIND=KIND(NENTRIES) )

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
    CASE (ERRFLAG_UNABLE_TO_FIND_TOC_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find toc file: '//TRIM(TOCFNAME) )
    CASE (ERRFLAG_UNABLE_TO_OPEN_TOC_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open toc file: '//TRIM(TOCFNAME) )
    CASE (ERRFLAG_UNABLE_TO_READ_TOC_ENTRIES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the number of toc entries' )
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

END FUNCTION TOC_ROPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_READ'
PP_THREAD_SAFE FUNCTION TOC_READ( TOCFNAME, TOC, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),                                 INTENT(IN)    :: TOCFNAME
  TYPE(TOC_CONTAINER_T), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: TOC
  LOGICAL,                                          INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                                          INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: TOCUNIT
  INTEGER(KIND=JPIB_K) :: NENTRIES
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: ENTRY_TYPE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_ALREADY_ALLOCATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NENTRIES_LOWER_OR_EQUAL_TO_ZERO = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_TOC = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN_TOC = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_NEXT_ENTRY = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_FILE = 6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(TOC), ERRFLAG_TOC_ALREADY_ALLOCATED)

  ! Open the toc file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_OPEN_TOC) TOC_ROPEN( TOCFNAME, TOCUNIT, BIG_ENDIAN_READ, NENTRIES, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( NENTRIES.LE.0, ERRFLAG_NENTRIES_LOWER_OR_EQUAL_TO_ZERO)

  ! Allocate the table of contents
  ALLOCATE( TOC(NENTRIES), STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE_TOC)

  ! Read all the entries
  DO I = 1, NENTRIES
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NEXT_ENTRY) TOC_READ_NEXT_ENTRY_TOC( TOCUNIT, ENTRY_TYPE, TOC(I)%ENTRY_, HOOKS )
  ENDDO

  ! Close the toc file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_FILE) TOC_CLOSE( TOCUNIT, HOOKS )

  ! Deallocate the error message if allocated
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG,STAT=STAT)
  ENDIF

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
    CASE (ERRFLAG_TOC_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc already allocated' )
    CASE (ERRFLAG_NENTRIES_LOWER_OR_EQUAL_TO_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nentries in toc lower of equal to 0' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_TOC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate toc' )
    CASE (ERRFLAG_UNABLE_TO_OPEN_TOC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open toc' )
    CASE (ERRFLAG_UNABLE_TO_GET_NEXT_ENTRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get next entry in toc' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call toc file' )
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

END FUNCTION TOC_READ
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_CLOSE'
PP_THREAD_SAFE FUNCTION TOC_CLOSE( TOCUNIT, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN) :: TOCUNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CLOSE_FILE = 2_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

  ! Close the toc file and flush it
  CLOSE( TOCUNIT, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_CLOSE_FILE)

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
    CASE (ERRFLAG_TOC_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
    CASE (ERRFLAG_UNABLE_TO_CLOSE_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close the file' )
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

END FUNCTION TOC_CLOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_FREE'
PP_THREAD_SAFE FUNCTION TOC_FREE( TOC, HOOKS ) RESULT(RET)

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
  TYPE(TOC_CONTAINER_T), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: TOC
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_ENTRY = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_TOC = 2_JPIB_K

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

  IF ( ALLOCATED(TOC) ) THEN
    DO I = 1, SIZE(TOC)
      IF ( ASSOCIATED(TOC(I)%ENTRY_) ) THEN
        DEALLOCATE( TOC(I)%ENTRY_, STAT=STAT )
        PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE_ENTRY)
        NULLIFY(TOC(I)%ENTRY_)
      ENDIF
    ENDDO
    DEALLOCATE( TOC, STAT=STAT )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE_TOC)
  ENDIF

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
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE_ENTRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate a toc entry' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE_TOC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate toc' )
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

END FUNCTION TOC_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_READ_ALL'
PP_THREAD_SAFE FUNCTION TOC_READ_ALL( DIRECTORY, TOC, NPROCS, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),                                 INTENT(IN)    :: DIRECTORY
  TYPE(TOC_CONTAINER_T), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: TOC
  INTEGER(KIND=JPIB_K),                             INTENT(IN)    :: NPROCS
  LOGICAL,                                          INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                                          INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=1024) :: FULLDIR
  CHARACTER(LEN=1024) :: TOCFNAME
  INTEGER(KIND=JPIB_K), DIMENSION(NPROCS) :: TOCUNIT
  INTEGER(KIND=JPIB_K), DIMENSION(NPROCS) :: NENTRIES
  INTEGER(KIND=JPIB_K) :: TOTNENTRIES
  INTEGER(KIND=JPIB_K) :: PROCID
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: ENTRY_TYPE
  CLASS(TOC_ENTRY_BASE_T), POINTER :: TMP_ENTRY => NULL()
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_FILE_NAME = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN_TOC_FILE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_ALREADY_ALLOCATED = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NENTRIES_LOWER_OR_EQUAL_TO_ZERO = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_TOC = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_NEXT_ENTRY = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_ENTRY = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CLOSE_TOC = 8_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(TOC), ERRFLAG_TOC_ALREADY_ALLOCATED)

  ! Open the toc file
  TOTNENTRIES = 0
  DO PROCID = 1, NPROCS
    FULLDIR = REPEAT(' ',1024)
    WRITE(FULLDIR,'(A,A,I6.6,A)',IOSTAT=STAT) TRIM(ADJUSTL(DIRECTORY)), '/io_serv.', PROCID, '.d'
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_CREATE_FILE_NAME)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CREATE_FILE_NAME) TOC_CREATE_NAME( TRIM(FULLDIR), PROCID, TOCFNAME, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_OPEN_TOC_FILE) TOC_ROPEN( TOCFNAME, TOCUNIT(PROCID), BIG_ENDIAN_READ, NENTRIES(PROCID), HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( NENTRIES(PROCID).LE.0, ERRFLAG_NENTRIES_LOWER_OR_EQUAL_TO_ZERO)
    TOTNENTRIES = TOTNENTRIES + NENTRIES(PROCID)
  ENDDO

  ! Allocate the table of contents
  ALLOCATE( TOC(TOTNENTRIES), STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE_TOC)

  ! Read all the entries
  CNT = 0
  OuterLoop: DO
    ProcLoop: DO PROCID = 1, NPROCS
      InnerLoop: DO

        PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NEXT_ENTRY) TOC_READ_NEXT_ENTRY_TOC( TOCUNIT(PROCID), ENTRY_TYPE, TMP_ENTRY, HOOKS )

        SELECT CASE ( ENTRY_TYPE )

        CASE ( SIM_INIT_E, FLUSH_STEP_E, FLUSH_STEP_RST_E, FLUSH_STEP_LAST_E )

          IF ( PROCID .EQ. NPROCS ) THEN
            CNT = CNT + 1
            TOC(CNT)%ENTRY_ => TMP_ENTRY
            NULLIFY(TMP_ENTRY)
          ELSE
            IF ( ASSOCIATED(TMP_ENTRY) ) THEN
              DEALLOCATE(TMP_ENTRY)
              NULLIFY(TMP_ENTRY)
            ENDIF
          ENDIF
          EXIT InnerLoop

        CASE ( ATM_FIELD_E, WAM_FIELD_E )

          CNT = CNT + 1
          TOC(CNT)%ENTRY_ => TMP_ENTRY
          NULLIFY(TMP_ENTRY)

        CASE ( SIM_END_E )

          IF ( PROCID .EQ. NPROCS ) THEN
            CNT = CNT + 1
            TOC(CNT)%ENTRY_ => TMP_ENTRY
            NULLIFY(TMP_ENTRY)
          ELSE
            IF ( ASSOCIATED(TMP_ENTRY) ) THEN
              DEALLOCATE(TMP_ENTRY)
              NULLIFY(TMP_ENTRY)
            ENDIF
          ENDIF
          IF ( PROCID .EQ. NPROCS ) THEN
            EXIT OuterLoop
          ELSE
            EXIT InnerLoop
          ENDIF

        CASE DEFAULT

          PP_DEBUG_CRITICAL_THROW(ERRFLAG_UNKNOWN_ENTRY)

        END SELECT

      ENDDO InnerLoop
    ENDDO ProcLoop
  ENDDO OuterLoop

  ! Close the toc file
  DO PROCID = 1, NPROCS
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CLOSE_TOC) TOC_CLOSE( TOCUNIT(PROCID), HOOKS )
  ENDDO

  ! Deallocate the error message if allocated
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG,STAT=STAT)
  ENDIF


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
    CASE (ERRFLAG_UNABLE_TO_CREATE_FILE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the file name' )
    CASE (ERRFLAG_UNABLE_TO_OPEN_TOC_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the toc file' )
    CASE (ERRFLAG_TOC_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc already allocated' )
    CASE (ERRFLAG_NENTRIES_LOWER_OR_EQUAL_TO_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nentries in toc lower of equal to 0' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_TOC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate toc' )
    CASE (ERRFLAG_UNABLE_TO_GET_NEXT_ENTRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get next entry in toc' )
    CASE (ERRFLAG_UNKNOWN_ENTRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown entry' )
    CASE (ERRFLAG_UNABLE_TO_CLOSE_TOC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close the toc file' )
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

END FUNCTION TOC_READ_ALL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_READ_FLUSH_BEGIN_OF_SIMULATION'
PP_THREAD_SAFE FUNCTION  TOC_READ_FLUSH_BEGIN_OF_SIMULATION( TOCUNIT, TOC_ENTRY, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: TOCUNIT
  CLASS(TOC_SIM_INIT_T), INTENT(OUT)   :: TOC_ENTRY
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Initialise the toc entry
  TOC_ENTRY%CLOCK_COUNT_ = 0

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_TOC_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
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

END FUNCTION TOC_READ_FLUSH_BEGIN_OF_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WRITE_FLUSH_BEGIN_OF_SIMULATION'
PP_THREAD_SAFE FUNCTION TOC_WRITE_FLUSH_BEGIN_OF_SIMULATION( TOCUNIT, WRITE_POS, TOC_COUNTER, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER = 4_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Write the beginning of the simulation
  WRITE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT ) INT(SIM_INIT_E, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)

  ! Get the position inthe file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION)

  ! Write the beginning of the simulation
  TOC_COUNTER = TOC_COUNTER + 1
  WRITE(TOCUNIT, POS=1, IOSTAT=STAT ) INT( TOC_COUNTER, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
      CASE (ERRFLAG_TOC_NOT_OPENED)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
      CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the toc entry in the file' )
      CASE (ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position in the toc file' )
      CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position n the toc' )
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

END FUNCTION TOC_WRITE_FLUSH_BEGIN_OF_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_READ_ATM'
PP_THREAD_SAFE FUNCTION TOC_READ_ATM( TOCUNIT, TOC_ENTRY, HOOKS ) RESULT(RET)


  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=JPIB_K),   INTENT(IN)  :: TOCUNIT
  CLASS(TOC_ATM_FIELD_T), INTENT(OUT) :: TOC_ENTRY
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ = 2_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Read the wam informations
  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%PARAM_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%PARAM_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%U_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%U_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%STEP_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%STEP_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%PROC_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%PROC_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%REPRES_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%REPRES_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%PREFIX_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%PREFIX_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%MSG_ADDR_ = INT( ITMP, KIND=KIND(TOC_ENTRY%MSG_ADDR_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%VAL_TYPE_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_TYPE_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%VAL_LB_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_LB_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%VAL_UB_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_UB_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%VAL_ADDR_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_ADDR_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%MSG_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%MSG_ID_) )

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
      CASE (ERRFLAG_TOC_NOT_OPENED)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
      CASE (ERRFLAG_UNABLE_TO_READ)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read atmosphere entry' )
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

END FUNCTION TOC_READ_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WRITE_ATM_TO_TOC'
PP_THREAD_SAFE FUNCTION TOC_WRITE_ATM( TOCUNIT, PARAM_ID, U_ID, STEP_ID, PROC_ID, &
& REPRES_ID, PREFIX_ID, MSG_ADDR, VAL_TYPE, VAL_LB, VAL_UB, VAL_ADDR, &
& MSG_ID, WRITE_POS, TOC_COUNTER, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PARAM_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: U_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PROC_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: REPRES_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PREFIX_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: MSG_ADDR
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_TYPE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_LB
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_UB
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_ADDR
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: MSG_ID
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER = 4_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Write the current step of the simulation and trigger restart
  WRITE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT ) INT(ATM_FIELD_E,KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)

  FLUSH( TOCUNIT )

  ! Write the current step
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( PARAM_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( U_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( STEP_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( PROC_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( REPRES_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( PREFIX_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( MSG_ADDR, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_TYPE, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_LB, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_UB, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_ADDR, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( MSG_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)

  ! Get the position in the file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION)

  ! Update the number of tocs in the toc file header
  TOC_COUNTER = TOC_COUNTER + 1
  WRITE(TOCUNIT, POS=1, IOSTAT=STAT ) INT(TOC_COUNTER, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
    CASE (ERRFLAG_TOC_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the toc entry in the file' )
    CASE (ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position in the toc file' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position n the toc' )
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

END FUNCTION TOC_WRITE_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_READ_WAM'
PP_THREAD_SAFE FUNCTION TOC_READ_WAM( TOCUNIT, TOC_ENTRY, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=JPIB_K),   INTENT(IN)    :: TOCUNIT
  CLASS(TOC_WAM_FIELD_T), INTENT(OUT)   :: TOC_ENTRY
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ = 2_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Read the wam informations
  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%PARAM_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%PARAM_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%U_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%U_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%STEP_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%STEP_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%PROC_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%PROC_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%REPRES_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%REPRES_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%PREFIX_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%PREFIX_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%MSG_ADDR_ = INT( ITMP, KIND=KIND(TOC_ENTRY%MSG_ADDR_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%VAL_TYPE_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_TYPE_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%VAL_LB_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_LB_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%VAL_UB_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_UB_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%VAL_ADDR_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_ADDR_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%MSG_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%MSG_ID_) )

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
    CASE (ERRFLAG_TOC_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
    CASE (ERRFLAG_UNABLE_TO_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read wam from toc file' )
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

END FUNCTION TOC_READ_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_WRITE_WAM'
PP_THREAD_SAFE FUNCTION TOC_WRITE_WAM( TOCUNIT, PARAM_ID, U_ID, STEP_ID, PROC_ID, &
& REPRES_ID, PREFIX_ID, MSG_ADDR, VAL_TYPE, VAL_LB, VAL_UB, VAL_ADDR, &
& MSG_ID, WRITE_POS, TOC_COUNTER, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PARAM_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: U_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PROC_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: REPRES_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PREFIX_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: MSG_ADDR
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_TYPE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_LB
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_UB
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_ADDR
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: MSG_ID
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER = 4_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Write the current step of the simulation and trigger restart
  WRITE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT ) INT(WAM_FIELD_E,INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)

  FLUSH( TOCUNIT )

  ! Write the current step
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( PARAM_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( U_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( STEP_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( PROC_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( REPRES_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( PREFIX_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( MSG_ADDR, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_TYPE, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_LB, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_UB, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_ADDR, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( MSG_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)

  ! Get the position in the file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION)

  ! Update the number of tocs in the toc file header
  TOC_COUNTER = TOC_COUNTER + 1
  WRITE(TOCUNIT, POS=1, IOSTAT=STAT ) INT(TOC_COUNTER, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
    CASE (ERRFLAG_TOC_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the toc entry in the file' )
    CASE (ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position in the toc file' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position n the toc' )
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

END FUNCTION TOC_WRITE_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_READ_FLUSH_STEP'
PP_THREAD_SAFE FUNCTION TOC_READ_FLUSH_STEP( TOCUNIT, TOC_ENTRY, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=JPIB_K),    INTENT(IN)    :: TOCUNIT
  CLASS(TOC_FLUSH_STEP_T), INTENT(OUT)   :: TOC_ENTRY
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ = 2_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Read the last step
  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%STEP_ = INT( ITMP, KIND=KIND(TOC_ENTRY%STEP_) )

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
    CASE (ERRFLAG_TOC_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
    CASE (ERRFLAG_UNABLE_TO_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read step' )
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

END FUNCTION TOC_READ_FLUSH_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_WRITE_FLUSH_STEP'
PP_THREAD_SAFE FUNCTION TOC_WRITE_FLUSH_STEP( TOCUNIT, ISTEP, WRITE_POS, TOC_COUNTER, HOOKS ) RESULT(RET)
  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ISTEP
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER = 4_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Write the current step of the simulation and trigger restart
  WRITE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT ) INT(FLUSH_STEP_E,KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)

  ! Write the current step
  WRITE( TOCUNIT, IOSTAT=STAT ) INT(ISTEP,KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)

  ! Get the position in the file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION)

  ! Update the number of tocs in the toc file header
  TOC_COUNTER = TOC_COUNTER + 1
  WRITE(TOCUNIT, POS=1, IOSTAT=STAT ) INT(TOC_COUNTER, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
    CASE (ERRFLAG_TOC_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the toc entry in the file' )
    CASE (ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position in the toc file' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position n the toc' )
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

END FUNCTION TOC_WRITE_FLUSH_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_READ_FLUSH_STEP_AND_RESTART'
PP_THREAD_SAFE FUNCTION TOC_READ_FLUSH_STEP_AND_RESTART( TOCUNIT, TOC_ENTRY, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=JPIB_K),        INTENT(IN)    :: TOCUNIT
  CLASS(TOC_FLUSH_STEP_RST_T), INTENT(OUT)   :: TOC_ENTRY
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ = 2_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Read the last step
  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)
  TOC_ENTRY%STEP_ = INT( ITMP, KIND=KIND(TOC_ENTRY%STEP_) )

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
      CASE (ERRFLAG_TOC_NOT_OPENED)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
      CASE (ERRFLAG_UNABLE_TO_READ)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read step' )
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

END FUNCTION TOC_READ_FLUSH_STEP_AND_RESTART
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WRITE_FLUSH_STEP_AND_RESTART'
PP_THREAD_SAFE FUNCTION TOC_WRITE_FLUSH_STEP_AND_RESTART( TOCUNIT, ISTEP, WRITE_POS, TOC_COUNTER, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ISTEP
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER = 4_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Write the current step of the simulation and trigger restart
  WRITE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT ) INT( FLUSH_STEP_RST_E, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)

  ! Write the current step
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( ISTEP, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)

  ! Get the position in the file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION)

  ! Update the number of tocs in the toc file header
  TOC_COUNTER = TOC_COUNTER + 1
  WRITE(TOCUNIT, POS=1, IOSTAT=STAT ) INT(TOC_COUNTER, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
    CASE (ERRFLAG_TOC_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the toc entry in the file' )
    CASE (ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position in the toc file' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position n the toc' )
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

END FUNCTION TOC_WRITE_FLUSH_STEP_AND_RESTART
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_READ_FLUSH_LAST_STEP'
PP_THREAD_SAFE FUNCTION  TOC_READ_FLUSH_LAST_STEP( TOCUNIT, TOC_ENTRY, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: TOCUNIT
  CLASS(TOC_FLUSH_LAST_STEP_T), INTENT(OUT)   :: TOC_ENTRY
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ = 2_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Read the last step
  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  TOC_ENTRY%STEP_ = INT( ITMP, KIND=KIND(TOC_ENTRY%STEP_) )


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
    CASE (ERRFLAG_TOC_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
    CASE (ERRFLAG_UNABLE_TO_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read step' )
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

END FUNCTION TOC_READ_FLUSH_LAST_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_WRITE_FLUSH_LAST_STEP'
PP_THREAD_SAFE FUNCTION TOC_WRITE_FLUSH_LAST_STEP( TOCUNIT, ISTEP, WRITE_POS, TOC_COUNTER, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ISTEP
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER = 4_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Write the last step of the simulation
  WRITE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT ) INT( FLUSH_STEP_LAST_E, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)

  ! Write the last step
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( ISTEP, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION)

  ! Get the position in the file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)

  ! Update the number of tocs in the toc file header
  TOC_COUNTER = TOC_COUNTER + 1
  WRITE(TOCUNIT, POS=1, IOSTAT=STAT ) INT(TOC_COUNTER, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
    CASE (ERRFLAG_TOC_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the toc entry in the file' )
    CASE (ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position in the toc file' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position n the toc' )
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

END FUNCTION TOC_WRITE_FLUSH_LAST_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_READ_FLUSH_END_OF_SIMULATION'
PP_THREAD_SAFE FUNCTION  TOC_READ_FLUSH_END_OF_SIMULATION( TOCUNIT, TOC_ENTRY, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  CLASS(TOC_SIM_END_T), INTENT(OUT)   :: TOC_ENTRY
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  TOC_ENTRY%CLOCK_COUNT_ = 1

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
      CASE (ERRFLAG_TOC_NOT_OPENED)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
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

END FUNCTION TOC_READ_FLUSH_END_OF_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_WRITE_FLUSH_END_OF_SIMULATION'
PP_THREAD_SAFE FUNCTION TOC_WRITE_FLUSH_END_OF_SIMULATION( TOCUNIT, WRITE_POS, TOC_COUNTER, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER = 4_JPIB_K

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
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Write the end of the simulation
  WRITE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT ) INT(SIM_END_E,KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)

  ! Get the position inthe file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION)

  ! Update the number of tocs in the toc file header
  TOC_COUNTER = TOC_COUNTER + 1
  WRITE(TOCUNIT, POS=1, IOSTAT=STAT ) INT(TOC_COUNTER, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
    CASE (ERRFLAG_TOC_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_ENTRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the toc entry in the file' )
    CASE (ERRFLAG_UNABLE_TO_INQUIRE_TOC_POSITION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position in the toc file' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_TOC_COUNTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire the current position n the toc' )
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

END FUNCTION TOC_WRITE_FLUSH_END_OF_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_READ_NEXT_ENTRY_TOC'
PP_THREAD_SAFE FUNCTION TOC_READ_NEXT_ENTRY_TOC( TOCUNIT, NEXT_ENTRY_E, NEXT_ENTRY, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=JPIB_K),             INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K),             INTENT(OUT)   :: NEXT_ENTRY_E
  CLASS(TOC_ENTRY_BASE_T), POINTER, INTENT(INOUT) :: NEXT_ENTRY
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_NOT_OPENED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_TYPE = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_TYPE = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FLUSH_BEGIN_OF_SIMULATION = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_ATM = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_WAM = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FLUSH_STEP = 10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FLUSH_STEP_AND_RESTART = 11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FLUSH_LAST_STEP = 12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FLUSH_END_OF_SIMULATION = 13_JPIB_K


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

  ! Check the status of the next entry
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(NEXT_ENTRY), ERRFLAG_ALREADY_ASSOCIATED )

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, ERRFLAG_TOC_NOT_OPENED)

  ! Make spaces for the tocsize
  NEXT_ENTRY_E = UNDEF_E
  READ(TOCUNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ)
  NEXT_ENTRY_E = INT( ITMP, KIND=KIND(NEXT_ENTRY_E) )

  ! Depending on the value of the next entry, associate the proper entry and call the specialised readed
  SELECT CASE(NEXT_ENTRY_E)

  CASE (SIM_INIT_E) ! 0
    ! WRITE(*,*) ' + Read SIM_INIT_E'
    ALLOCATE( TOC_SIM_INIT_T::NEXT_ENTRY, STAT=STAT, ERRMSG=ERRMSG  )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE)
    NEXT_ENTRY%TYPE_ = SIM_INIT_E
    SELECT TYPE ( A => NEXT_ENTRY )
    CLASS IS ( TOC_SIM_INIT_T )
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_FLUSH_BEGIN_OF_SIMULATION) TOC_READ_FLUSH_BEGIN_OF_SIMULATION( TOCUNIT, A, HOOKS )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNABLE_TO_ALLOCATE)
    END SELECT

  CASE (ATM_FIELD_E) ! 1
    ! WRITE(*,*) ' + Read ATM_FIELD_E'
    ALLOCATE( TOC_ATM_FIELD_T::NEXT_ENTRY, STAT=STAT, ERRMSG=ERRMSG  )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE)
    NEXT_ENTRY%TYPE_ = ATM_FIELD_E
    SELECT TYPE ( A => NEXT_ENTRY )
    CLASS IS ( TOC_ATM_FIELD_T )
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_ATM) TOC_READ_ATM( TOCUNIT, A, HOOKS )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNABLE_TO_ALLOCATE)
    END SELECT

  CASE (WAM_FIELD_E) ! 2
    ! WRITE(*,*) ' + Read WAM_FIELD_E'
    ALLOCATE( TOC_WAM_FIELD_T::NEXT_ENTRY, STAT=STAT, ERRMSG=ERRMSG  )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE)
    NEXT_ENTRY%TYPE_ = WAM_FIELD_E
    SELECT TYPE ( A => NEXT_ENTRY )
    CLASS IS ( TOC_WAM_FIELD_T )
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_WAM) TOC_READ_WAM( TOCUNIT, A, HOOKS )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNABLE_TO_ALLOCATE)
    END SELECT

  CASE (FLUSH_STEP_E) ! 3
    ! WRITE(*,*) ' + Read FLUSH_STEP_E'
    ALLOCATE( TOC_FLUSH_STEP_T::NEXT_ENTRY, STAT=STAT, ERRMSG=ERRMSG  )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE)
    NEXT_ENTRY%TYPE_ = FLUSH_STEP_E
    SELECT TYPE ( A => NEXT_ENTRY )
    CLASS IS ( TOC_FLUSH_STEP_T )
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_FLUSH_STEP) TOC_READ_FLUSH_STEP( TOCUNIT, A, HOOKS )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNABLE_TO_ALLOCATE)
    END SELECT

  CASE (FLUSH_STEP_RST_E) ! 4
    ! WRITE(*,*) ' + Read FLUSH_STEP_RST_E'
    ALLOCATE( TOC_FLUSH_STEP_RST_T::NEXT_ENTRY, STAT=STAT, ERRMSG=ERRMSG  )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE)
    NEXT_ENTRY%TYPE_ = FLUSH_STEP_RST_E
    SELECT TYPE ( A => NEXT_ENTRY )
    CLASS IS ( TOC_FLUSH_STEP_RST_T )
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_FLUSH_STEP_AND_RESTART) TOC_READ_FLUSH_STEP_AND_RESTART( TOCUNIT, A, HOOKS )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNABLE_TO_ALLOCATE)
    END SELECT

  CASE (FLUSH_STEP_LAST_E) ! 5
    ! WRITE(*,*) ' + Read FLUSH_STEP_LAST_E'
    ALLOCATE( TOC_FLUSH_LAST_STEP_T::NEXT_ENTRY, STAT=STAT, ERRMSG=ERRMSG  )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE)
    NEXT_ENTRY%TYPE_ = FLUSH_STEP_LAST_E
    SELECT TYPE ( A => NEXT_ENTRY )
    CLASS IS ( TOC_FLUSH_LAST_STEP_T )
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_FLUSH_LAST_STEP) TOC_READ_FLUSH_LAST_STEP( TOCUNIT, A, HOOKS )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNABLE_TO_ALLOCATE)
    END SELECT

  CASE (SIM_END_E) ! 6
    ! WRITE(*,*) ' + Read SIM_END_E'
    ALLOCATE( TOC_SIM_END_T::NEXT_ENTRY, STAT=STAT, ERRMSG=ERRMSG  )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE)
    NEXT_ENTRY%TYPE_ = SIM_END_E
    SELECT TYPE ( A => NEXT_ENTRY )
    CLASS IS ( TOC_SIM_END_T )
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_FLUSH_END_OF_SIMULATION) TOC_READ_FLUSH_END_OF_SIMULATION( TOCUNIT, A, HOOKS )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNABLE_TO_ALLOCATE)
    END SELECT

  CASE (UNDEF_E) ! -1

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNABLE_TO_READ_TYPE )

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNABLE_TO_READ_TYPE )

  END SELECT

  ! Deallocate the error message if allocated
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG, STAT=STAT)
  ENDIF

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
    CASE (ERRFLAG_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The next entry is already associated' )
    CASE (ERRFLAG_TOC_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Toc file not opened or not exists' )
    CASE (ERRFLAG_UNABLE_TO_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the next entry type' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate the next entry' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=STAT)
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_READ_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the next entry type' )
    CASE (ERRFLAG_UNABLE_TO_READ_FLUSH_BEGIN_OF_SIMULATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the next entry of type FLUSH_BEGIN_OF_SIMULATION' )
    CASE (ERRFLAG_UNABLE_TO_READ_ATM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the next entry of type ATM_FIELD' )
    CASE (ERRFLAG_UNABLE_TO_READ_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the next entry of type WAM_FIELD' )
    CASE (ERRFLAG_UNABLE_TO_READ_FLUSH_STEP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the next entry of type FLUSH_STEP' )
    CASE (ERRFLAG_UNABLE_TO_READ_FLUSH_STEP_AND_RESTART)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the next entry of type FLUSH_STEP_AND_RESTART' )
    CASE (ERRFLAG_UNABLE_TO_READ_FLUSH_LAST_STEP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the next entry of type FLUSH_LAST_STEP' )
    CASE (ERRFLAG_UNABLE_TO_READ_FLUSH_END_OF_SIMULATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the next entry of type FLUSH_END_OF_SIMULATION' )
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

END FUNCTION TOC_READ_NEXT_ENTRY_TOC
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE IFS_TOC_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
