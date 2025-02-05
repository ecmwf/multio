! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'multiom_tools_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MULTIOM_TOOLS_UTILS_MOD'
MODULE MULTIOM_TOOLS_UTILS_MOD

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

IMPLICIT NONE

! Default visibility
PRIVATE

! Temporary values for the read functions
REAL(KIND=REAL32), TARGET, DIMENSION(:), ALLOCATABLE :: GVALUES_SP
REAL(KIND=REAL64), TARGET, DIMENSION(:), ALLOCATABLE :: GVALUES_DP

! Verbosity of the hooks
LOGICAL :: HOOK_VERBOSE=.FALSE.

! Whitelist of public symbols
PUBLIC :: DR_HOOK_DEFAULT8
PUBLIC :: PREPARE_OUTPUT_MANAGER_EC
PUBLIC :: READ_ATM_MESSAGE
PUBLIC :: READ_WAM_MESSAGE
PUBLIC :: READ_VAL_SP
PUBLIC :: READ_VAL_DP
PUBLIC :: HOOK_VERBOSE

CONTAINS

SUBROUTINE DR_HOOK_DEFAULT8( CDNAME, KSWITCH, PKEY )
  USE :: MULTIOM_API, ONLY: JPIM_K
  USE :: MULTIOM_API, ONLY: JPTR_K
IMPLICIT NONE
  CHARACTER(LEN=*),     INTENT(IN)    :: CDNAME
  INTEGER(KIND=JPIM_K), INTENT(IN)    :: KSWITCH
  REAL(KIND=JPTR_K),    INTENT(INOUT) :: PKEY
  IF ( HOOK_VERBOSE ) THEN
    IF ( KSWITCH .EQ. 0 ) THEN
      WRITE(*,*) 'ENTER: ', TRIM(CDNAME)
    ELSE
      WRITE(*,*) 'EXIT: ', TRIM(CDNAME)
    ENDIF
  ENDIF

END SUBROUTINE DR_HOOK_DEFAULT8


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PREPARE_OUTPUT_MANAGER_EC'
PP_THREAD_SAFE FUNCTION PREPARE_OUTPUT_MANAGER_EC( DIRECTORY, PROC_ID, YDTOPO, YDOMP, &
&    BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: MODEL_PAR_T
  USE :: MULTIOM_API, ONLY: PROC_TOPO_T
  USE :: MULTIOM_API, ONLY: PAR_CREATE_NAME
  USE :: MULTIOM_API, ONLY: PAR_ROPEN
  USE :: MULTIOM_API, ONLY: PAR_READ
  USE :: MULTIOM_API, ONLY: PAR_CLOSE
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T

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
  TYPE(PROC_TOPO_T),    INTENT(INOUT) :: YDTOPO
  TYPE(MODEL_PAR_T),    INTENT(INOUT) :: YDOMP
  LOGICAL,              INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,              INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=1024)  :: LOC_DIR
  CHARACTER(LEN=1024)  :: PARFNAME
  INTEGER(KIND=JPIB_K) :: PARUNIT
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_CREATE = 2
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_ROPEN = 3
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_READ = 4
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_CLOSE = 5

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

  ! Fill the topology object
  YDTOPO%NCOMM_W1IO = 1
  YDTOPO%NCOMM_WR = 1
  YDTOPO%NCOMM_IO = 1
  YDTOPO%MYPROC_WRIO = 1
  YDTOPO%MYPROC_IO = 1
  YDTOPO%MYPROC_WR = 1
  YDTOPO%NPROC_IO = 1
  YDTOPO%LIO_SERVER = .TRUE.
  YDTOPO%LIO_CLIENT = .FALSE.

  ! Read the parameters
  LOC_DIR = REPEAT( ' ', 1024 )
  WRITE(LOC_DIR,'(A,A,I6.6,A)', IOSTAT=WRITE_STATUS) TRIM(DIRECTORY), '/io_serv.', PROC_ID, '.d'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  ! Read the parameters file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_CREATE) PAR_CREATE_NAME( TRIM(LOC_DIR), PROC_ID, PARFNAME, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_ROPEN) PAR_ROPEN( PARFNAME, PARUNIT, BIG_ENDIAN_READ, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_READ) PAR_READ( YDOMP, PARUNIT, VERBOSE, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_CLOSE) PAR_CLOSE( PARUNIT, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_WRITE_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write to string' )
    CASE(ERRFLAG_UNABLE_TO_PAR_CREATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create parameter file' )
    CASE(ERRFLAG_UNABLE_TO_PAR_ROPEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open parameter file' )
    CASE(ERRFLAG_UNABLE_TO_PAR_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read parameter file' )
    CASE(ERRFLAG_UNABLE_TO_PAR_CLOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close parameter file' )
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

END FUNCTION PREPARE_OUTPUT_MANAGER_EC
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_ATM_MESSAGE'
PP_THREAD_SAFE FUNCTION READ_ATM_MESSAGE( DIRECTORY, PROC_ID, MSG_ID, ADDR, YDMSG, &
&  BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: MSG_CREATE_NAME
  USE :: MULTIOM_API, ONLY: MSG_ROPEN
  USE :: MULTIOM_API, ONLY: MSG_READ_ATM
  USE :: MULTIOM_API, ONLY: MSG_CLOSE
  USE :: MULTIOM_API, ONLY: OM_ATM_MSG_T
  USE :: MULTIOM_API, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)  :: DIRECTORY
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: PROC_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: MSG_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: ADDR
  TYPE(OM_ATM_MSG_T),   INTENT(OUT) :: YDMSG
  LOGICAL,              INTENT(IN)  :: BIG_ENDIAN_READ
  LOGICAL,              INTENT(IN)  :: VERBOSE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=32)    :: CFMT
  CHARACTER(LEN=32)    :: CINT
  CHARACTER(LEN=1024)  :: LOC_DIR
  CHARACTER(LEN=1024)  :: MSGFNAME
  INTEGER(KIND=JPIB_K) :: MSGUNIT
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MSG_CREATE_NAME = 2
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MSG_ROPEN = 3
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MSG_READ_ATM = 4
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MSG_CLOSE = 5

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

  ! Read the message
  IF ( VERBOSE ) THEN
    CFMT=REPEAT(' ',32)
    CINT=REPEAT(' ',32)
  ENDIF

  LOC_DIR=REPEAT( ' ', 1024 )
  WRITE(LOC_DIR,'(A,A,I6.6,A)', IOSTAT=WRITE_STATUS) TRIM(DIRECTORY), '/io_serv.', PROC_ID, '.d'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )


  PP_TRYCALL(ERRFLAG_UNABLE_TO_MSG_CREATE_NAME) MSG_CREATE_NAME( TRIM(LOC_DIR), MSG_ID, PROC_ID, MSGFNAME, HOOKS )

  IF ( VERBOSE ) THEN
    WRITE(CINT,'(I10)', IOSTAT=WRITE_STATUS) LEN_TRIM(MSGFNAME)
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

    WRITE(CFMT,'(A)', IOSTAT=WRITE_STATUS) '(A'//TRIM(ADJUSTL(CINT))//')'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

    WRITE(*,CFMT, IOSTAT=WRITE_STATUS) TRIM(MSGFNAME)
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  ENDIF

  PP_TRYCALL(ERRFLAG_UNABLE_TO_MSG_ROPEN) MSG_ROPEN( MSGFNAME, MSGUNIT, BIG_ENDIAN_READ, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MSG_READ_ATM) MSG_READ_ATM( MSGUNIT, YDMSG, ADDR, VERBOSE, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MSG_CLOSE) MSG_CLOSE( MSGUNIT, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_WRITE_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write to string' )
    CASE(ERRFLAG_UNABLE_TO_MSG_CREATE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create message file' )
    CASE(ERRFLAG_UNABLE_TO_MSG_ROPEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open message file' )
    CASE(ERRFLAG_UNABLE_TO_MSG_READ_ATM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read message file' )
    CASE(ERRFLAG_UNABLE_TO_MSG_CLOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close message file' )
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

END FUNCTION READ_ATM_MESSAGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_WAM_MESSAGE'
PP_THREAD_SAFE FUNCTION READ_WAM_MESSAGE( DIRECTORY, PROC_ID, MSG_ID, ADDR, YDMSG, &
& BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: MSG_CREATE_NAME
  USE :: MULTIOM_API, ONLY: MSG_ROPEN
  USE :: MULTIOM_API, ONLY: MSG_READ_WAM
  USE :: MULTIOM_API, ONLY: MSG_CLOSE
  USE :: MULTIOM_API, ONLY: OM_WAM_MSG_T
  USE :: MULTIOM_API, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)  :: DIRECTORY
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: PROC_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: MSG_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: ADDR
  TYPE(OM_WAM_MSG_T),   INTENT(OUT) :: YDMSG
  LOGICAL,              INTENT(IN)  :: BIG_ENDIAN_READ
  LOGICAL,              INTENT(IN)  :: VERBOSE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=32)    :: CFMT
  CHARACTER(LEN=32)    :: CINT
  CHARACTER(LEN=1024)  :: LOC_DIR
  CHARACTER(LEN=1024)  :: MSGFNAME
  INTEGER(KIND=JPIB_K) :: MSGUNIT
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MSG_CREATE_NAME = 2
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MSG_ROPEN = 3
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MSG_READ_WAM = 4
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MSG_CLOSE = 5

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

  ! Read the message
  IF ( VERBOSE ) THEN
    CFMT=REPEAT(' ',32)
    CINT=REPEAT(' ',32)
  ENDIF

  LOC_DIR = REPEAT( ' ', 1024 )
  WRITE(LOC_DIR,'(A,A,I6.6,A)', IOSTAT=WRITE_STATUS) TRIM(DIRECTORY), '/io_serv.', PROC_ID, '.d'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  PP_TRYCALL(ERRFLAG_UNABLE_TO_MSG_CREATE_NAME) MSG_CREATE_NAME( TRIM(LOC_DIR), MSG_ID, PROC_ID, MSGFNAME, HOOKS )

  IF ( VERBOSE ) THEN
    WRITE(CINT,'(I10)', IOSTAT=WRITE_STATUS) LEN_TRIM(MSGFNAME)
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

    WRITE(CFMT,'(A)', IOSTAT=WRITE_STATUS) '(A'//TRIM(ADJUSTL(CINT))//')'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

    WRITE(*,CFMT, IOSTAT=WRITE_STATUS) TRIM(MSGFNAME)
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  ENDIF

  PP_TRYCALL(ERRFLAG_UNABLE_TO_MSG_ROPEN)  MSG_ROPEN( MSGFNAME, MSGUNIT, BIG_ENDIAN_READ, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MSG_READ_WAM)  MSG_READ_WAM( MSGUNIT, YDMSG, ADDR, VERBOSE, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MSG_CLOSE)  MSG_CLOSE( MSGUNIT, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_WRITE_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write to string' )
    CASE(ERRFLAG_UNABLE_TO_MSG_CREATE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create message file' )
    CASE(ERRFLAG_UNABLE_TO_MSG_ROPEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open message file' )
    CASE(ERRFLAG_UNABLE_TO_MSG_READ_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read message file' )
    CASE(ERRFLAG_UNABLE_TO_MSG_CLOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close message file' )
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

END FUNCTION READ_WAM_MESSAGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_VAL_SP'
PP_THREAD_SAFE FUNCTION READ_VAL_SP( DIRECTORY, PROC_ID, MSG_ID, ADDR, &
& LB, UB, VALUES, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: JPRM_K
  USE :: MULTIOM_API, ONLY: OM_WAM_MSG_T
  USE :: MULTIOM_API, ONLY: VAL_CREATE_NAME
  USE :: MULTIOM_API, ONLY: VAL_ROPEN
  USE :: MULTIOM_API, ONLY: VAL_READ_SP
  USE :: MULTIOM_API, ONLY: VAL_GENERATE_SP
  USE :: MULTIOM_API, ONLY: VAL_CLOSE
  USE :: MULTIOM_API, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                         INTENT(IN)    :: DIRECTORY
  INTEGER(KIND=JPIB_K),                     INTENT(IN)    :: PROC_ID
  INTEGER(KIND=JPIB_K),                     INTENT(IN)    :: MSG_ID
  INTEGER(KIND=JPIB_K),                     INTENT(IN)    :: ADDR
  INTEGER(KIND=JPIB_K),                     INTENT(IN)    :: LB
  INTEGER(KIND=JPIB_K),                     INTENT(IN)    :: UB
  REAL(KIND=JPRM_K), DIMENSION(:), POINTER, INTENT(INOUT) :: VALUES
  LOGICAL,                                  INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                                  INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=1024)  :: LOC_DIR
  CHARACTER(LEN=1024)  :: VALFNAME
  INTEGER(KIND=JPIB_K) :: SZ
  INTEGER(KIND=JPIB_K) :: VALUNIT
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: NUNDEF
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS
  REAL(KIND=REAL64) :: MINVAL
  REAL(KIND=REAL64) :: MAXVAL
  REAL(KIND=REAL64) :: AVGVAL
  REAL(KIND=REAL64) :: XUNDEF
  LOGICAL :: VALEX

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_VAL_CREATE_NAME = 2
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_VAL_ROPEN = 3
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_VAL_READ_SP = 4
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_VAL_CLOSE = 5
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_VAL_GENERATE_SP = 6
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_MEMORY = 7
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_MEMORY = 8

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

  N = UB-LB+1
  IF ( .NOT. ALLOCATED(GVALUES_SP) ) THEN
    ALLOCATE( GVALUES_SP(N), STAT=ALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_MEMORY )
  ELSE
    IF ( SIZE(GVALUES_SP) .LT. N ) THEN
      DEALLOCATE(GVALUES_SP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_MEMORY )
      ALLOCATE( GVALUES_SP(N), STAT=ALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_MEMORY )
    ENDIF
  ENDIF
  VALUES(LB:UB) => GVALUES_SP(1:N)

  ! Read the message
  LOC_DIR = REPEAT( ' ', 1024 )
  WRITE(LOC_DIR,'(A,A,I6.6,A)', IOSTAT=WRITE_STATUS) TRIM(DIRECTORY), '/io_serv.', PROC_ID, '.d'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  PP_TRYCALL(ERRFLAG_UNABLE_TO_VAL_CREATE_NAME) VAL_CREATE_NAME( TRIM(LOC_DIR), MSG_ID, PROC_ID, VALFNAME, HOOKS )

  INQUIRE( FILE=TRIM(VALFNAME), EXIST=VALEX )

  IF ( .NOT.VALEX .OR. ADDR.LT.0 ) THEN
    SZ = SIZE(VALUES)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_VAL_GENERATE_SP) VAL_GENERATE_SP( SZ, MINVAL, MAXVAL, AVGVAL, NUNDEF, XUNDEF, VALUES, HOOKS )
  ELSE
    PP_TRYCALL(ERRFLAG_UNABLE_TO_VAL_ROPEN) VAL_ROPEN( VALFNAME, VALUNIT, BIG_ENDIAN_READ, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_VAL_READ_SP) VAL_READ_SP( VALUNIT, ADDR, VALUES, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_VAL_CLOSE) VAL_CLOSE( VALUNIT, HOOKS )
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
    CASE(ERRFLAG_UNABLE_TO_WRITE_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write to string' )
    CASE(ERRFLAG_UNABLE_TO_VAL_CREATE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create value file' )
    CASE(ERRFLAG_UNABLE_TO_VAL_ROPEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open value file' )
    CASE(ERRFLAG_UNABLE_TO_VAL_READ_SP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read value file' )
    CASE(ERRFLAG_UNABLE_TO_VAL_CLOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close value file' )
    CASE(ERRFLAG_UNABLE_TO_VAL_GENERATE_SP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to generate value file' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE_MEMORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate memory' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STAT)
      ENDIF
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE_MEMORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate memory' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STAT)
      ENDIF
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

END FUNCTION READ_VAL_SP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_VAL_DP'
PP_THREAD_SAFE FUNCTION READ_VAL_DP( DIRECTORY, PROC_ID, MSG_ID, ADDR, &
& LB, UB, VALUES, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: JPRM_K
  USE :: MULTIOM_API, ONLY: JPRD_K
  USE :: MULTIOM_API, ONLY: OM_WAM_MSG_T
  USE :: MULTIOM_API, ONLY: VAL_CREATE_NAME
  USE :: MULTIOM_API, ONLY: VAL_ROPEN
  USE :: MULTIOM_API, ONLY: VAL_READ_DP
  USE :: MULTIOM_API, ONLY: VAL_GENERATE_DP
  USE :: MULTIOM_API, ONLY: VAL_CLOSE
  USE :: MULTIOM_API, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                         INTENT(IN)    :: DIRECTORY
  INTEGER(KIND=JPIB_K),                     INTENT(IN)    :: PROC_ID
  INTEGER(KIND=JPIB_K),                     INTENT(IN)    :: MSG_ID
  INTEGER(KIND=JPIB_K),                     INTENT(IN)    :: ADDR
  INTEGER(KIND=JPIB_K),                     INTENT(IN)    :: LB
  INTEGER(KIND=JPIB_K),                     INTENT(IN)    :: UB
  REAL(KIND=JPRD_K), DIMENSION(:), POINTER, INTENT(INOUT) :: VALUES
  LOGICAL,                                  INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                                  INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=1024)  :: LOC_DIR
  CHARACTER(LEN=1024)  :: VALFNAME
  INTEGER(KIND=JPIB_K) :: SZ
  INTEGER(KIND=JPIB_K) :: VALUNIT
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: NUNDEF
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS
  REAL(KIND=REAL64) :: MINVAL
  REAL(KIND=REAL64) :: MAXVAL
  REAL(KIND=REAL64) :: AVGVAL
  REAL(KIND=REAL64) :: XUNDEF
  LOGICAL :: VALEX

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_VAL_CREATE_NAME = 2
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_VAL_ROPEN = 3
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_VAL_READ_DP = 4
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_VAL_CLOSE = 5
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_VAL_GENERATE_DP = 6
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_MEMORY = 7
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_MEMORY = 8

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

  N = UB-LB+1
  IF ( .NOT. ALLOCATED(GVALUES_DP) ) THEN
    ALLOCATE( GVALUES_DP(N), STAT=ALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_MEMORY )
  ELSE
    IF ( SIZE(GVALUES_DP) .LT. N ) THEN
      DEALLOCATE(GVALUES_DP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_MEMORY )
      ALLOCATE( GVALUES_DP(N), STAT=ALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_MEMORY )
    ENDIF
  ENDIF
  VALUES(LB:UB) => GVALUES_DP(1:N)

  ! Read the message
  LOC_DIR = REPEAT( ' ', 1024 )
  WRITE(LOC_DIR,'(A,A,I6.6,A)', IOSTAT=WRITE_STATUS) TRIM(DIRECTORY), '/io_serv.', PROC_ID, '.d'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  PP_TRYCALL(ERRFLAG_UNABLE_TO_VAL_CREATE_NAME) VAL_CREATE_NAME( TRIM(LOC_DIR), MSG_ID, PROC_ID, VALFNAME, HOOKS )

  INQUIRE( FILE=TRIM(VALFNAME), EXIST=VALEX )

  IF ( .NOT.VALEX .OR. ADDR.LT.0 ) THEN
    SZ = SIZE(VALUES)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_VAL_GENERATE_DP) VAL_GENERATE_DP( SZ, MINVAL, MAXVAL, AVGVAL, NUNDEF, XUNDEF, VALUES, HOOKS )
  ELSE
    PP_TRYCALL(ERRFLAG_UNABLE_TO_VAL_ROPEN) VAL_ROPEN( VALFNAME, VALUNIT, BIG_ENDIAN_READ, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_VAL_READ_DP) VAL_READ_DP( VALUNIT, ADDR, VALUES, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_VAL_CLOSE) VAL_CLOSE( VALUNIT, HOOKS )
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
    CASE(ERRFLAG_UNABLE_TO_WRITE_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write to string' )
    CASE(ERRFLAG_UNABLE_TO_VAL_CREATE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create value file' )
    CASE(ERRFLAG_UNABLE_TO_VAL_ROPEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open value file' )
    CASE(ERRFLAG_UNABLE_TO_VAL_READ_DP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read value file' )
    CASE(ERRFLAG_UNABLE_TO_VAL_CLOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close value file' )
    CASE(ERRFLAG_UNABLE_TO_VAL_GENERATE_DP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to generate value file' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE_MEMORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate memory' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STAT)
      ENDIF
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE_MEMORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate memory' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STAT)
      ENDIF
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

END FUNCTION READ_VAL_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE MULTIOM_TOOLS_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME