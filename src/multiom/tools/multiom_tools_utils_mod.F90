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

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K

IMPLICIT NONE

! Default visibility
PRIVATE

! Temporary values for the read functions
REAL(KIND=REAL32), TARGET, DIMENSION(:), ALLOCATABLE :: GVALUES_SP
REAL(KIND=REAL64), TARGET, DIMENSION(:), ALLOCATABLE :: GVALUES_DP

! Verbosity of the hooks
LOGICAL :: HOOK_VERBOSE=.FALSE.

! Format of the print
INTEGER(KIND=JPIB_K), PARAMETER :: CELL_SIZE = 16_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: NCELLS = 8_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SEP_SIZE = 3_JPIB_K
CHARACTER(LEN=*), PARAMETER :: PRINT_FORMAT = '(1x," | ",A," | ",A," | ",A," | ",A," | ",A," |" ,A," | ",A," | ",A," | ")'

! Whitelist of public symbols (Procedures)

! Utils
PUBLIC :: DR_HOOK_DEFAULT8
PUBLIC :: HOOK_VERBOSE


! Initialization
PUBLIC :: INITIALIZE_OUTPUT_MANAGER
PUBLIC :: INITIALIZE_TABLE_OF_CONTENTS
PUBLIC :: SHOW_PARAMS
PUBLIC :: CHECK_DATA_ENDIANNES

! Print
PUBLIC :: PRINT_HEADER
PUBLIC :: PRINT_SIMULATION_INIT
PUBLIC :: PRINT_ATM_MESSAGE
PUBLIC :: PRINT_WAM_MESSAGE
PUBLIC :: PRINT_FLUSH
PUBLIC :: PRINT_FLUSH_AND_RESTART
PUBLIC :: PRINT_FLUSH_LAST_STEP
PUBLIC :: PRINT_SIMULATION_END
PUBLIC :: PRINT_FOOTER

! Encode
PUBLIC :: ENCODE_HEADER
PUBLIC :: ENCODE_SIMULATION_INIT
PUBLIC :: ENCODE_ATM_MESSAGE
PUBLIC :: ENCODE_WAM_MESSAGE
PUBLIC :: ENCODE_FLUSH
PUBLIC :: ENCODE_FLUSH_AND_RESTART
PUBLIC :: ENCODE_FLUSH_LAST_STEP
PUBLIC :: ENCODE_SIMULATION_END
PUBLIC :: ENCODE_FOOTER

! Read
PUBLIC :: READ_ATM_MESSAGE
PUBLIC :: READ_WAM_MESSAGE
PUBLIC :: READ_VAL_SP
PUBLIC :: READ_VAL_DP

! Finalization
PUBLIC :: FINALIZE_OUTPUT_MANAGER
PUBLIC :: FINALIZE_TABLE_OF_CONTENTS

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

PURE FUNCTION NORMALIZE_STRING( STRING ) RESULT(RET)
IMPLICIT NONE
CHARACTER(LEN=*), INTENT(IN) :: STRING
CHARACTER(LEN=CELL_SIZE) :: RET
CHARACTER(LEN=CELL_SIZE) :: STRING_COPY
INTEGER :: N

STRING_COPY = STRING(1:MIN(LEN(STRING),CELL_SIZE))
N = LEN_TRIM(STRING_COPY)
RET = REPEAT(' ',CELL_SIZE)

IF ( N .LT. CELL_SIZE ) THEN
  RET = REPEAT( ' ', (CELL_SIZE-N)/2 ) // TRIM(ADJUSTL(STRING_COPY))
ELSE
  RET = STRING_COPY
ENDIF

END FUNCTION NORMALIZE_STRING


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CHECK_DATA_ENDIANNES'
PP_THREAD_SAFE FUNCTION CHECK_DATA_ENDIANNES( CMDARGS, BIG_ENDIAN_READ, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: TOC_CONTAINER_T
  USE :: MULTIOM_API, ONLY: MODEL_PAR_T
  USE :: MULTIOM_API, ONLY: PROC_TOPO_T
  USE :: MULTIOM_API, ONLY: PAR_PRINT
  USE :: MULTIOM_API, ONLY: PAR_CREATE_NAME
  USE :: MULTIOM_API, ONLY: PAR_GET_ENDIANNES
  USE :: MULTIOM_API, ONLY: PAR_ROPEN
  USE :: MULTIOM_API, ONLY: PAR_READ
  USE :: MULTIOM_API, ONLY: PAR_CLOSE
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: MULTIOM_API, ONLY: MAKE_OUTPUT_MANAGER
  USE :: MULTIOM_API, ONLY: TOC_READ_LIST

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T), INTENT(IN)    :: CMDARGS
  LOGICAL,                   INTENT(OUT)   :: BIG_ENDIAN_READ
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=1024)  :: LOC_DIR
  CHARACTER(LEN=1024)  :: PARFNAME
  CHARACTER(LEN=1024)  :: DUMP_PATH
  INTEGER(KIND=JPIB_K) :: PROC_ID
  INTEGER(KIND=JPIB_K) :: PARUNIT
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_CREATE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_DUMP_DIRECTORY = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_READ_BIG_ENDIAN = 4_JPIB_K

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

  ! Initialization
  LOC_DIR = REPEAT( ' ', 1024 )
  DUMP_PATH = REPEAT( ' ', 1024 )
  PARFNAME = REPEAT( ' ', 1024 )
  PROC_ID = 1_JPIB_K
  BIG_ENDIAN_READ = .FALSE.

  ! Read command line arguments
  PP_TRYCALL(ERRFLAG_GET_DUMP_DIRECTORY) CMDARGS%GET_DUMP_PATH( DUMP_PATH, HOOKS )

  ! Read the parameters (Parameters are always read from the first processor)
  WRITE(LOC_DIR,'(A,A,I6.6,A)', IOSTAT=WRITE_STATUS) TRIM(ADJUSTL(DUMP_PATH)), '/io_serv.', PROC_ID,'.d'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  ! Read the parameters file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_CREATE) PAR_CREATE_NAME( TRIM(ADJUSTL(LOC_DIR)), PROC_ID, PARFNAME, HOOKS )
  PP_TRYCALL(ERRFLAG_GET_READ_BIG_ENDIAN) PAR_GET_ENDIANNES( PARFNAME, BIG_ENDIAN_READ, HOOKS )

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
    CASE(ERRFLAG_GET_DUMP_DIRECTORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get dump directory' )
    CASE(ERRFLAG_GET_READ_BIG_ENDIAN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get read big endian' )
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

END FUNCTION CHECK_DATA_ENDIANNES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SHOW_PARAMS'
PP_THREAD_SAFE FUNCTION SHOW_PARAMS( CMDARGS, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: MODEL_PAR_T
  USE :: MULTIOM_API, ONLY: PAR_PRINT
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
  TYPE(COMMAND_LINE_ARGS_T), INTENT(IN)    :: CMDARGS
  LOGICAL,                   INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                   INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(MODEL_PAR_T) :: PARAMETERS
  CHARACTER(LEN=1024)  :: LOC_DIR
  CHARACTER(LEN=1024)  :: PARFNAME
  CHARACTER(LEN=1024)  :: DUMP_PATH
  INTEGER(KIND=JPIB_K) :: PROC_ID
  INTEGER(KIND=JPIB_K) :: PARUNIT
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_CREATE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_ROPEN = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_READ = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_CLOSE = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_DUMP_DIRECTORY = 6_JPIB_K

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


  ! Read command line arguments
  PP_TRYCALL(ERRFLAG_GET_DUMP_DIRECTORY) CMDARGS%GET_DUMP_PATH( DUMP_PATH, HOOKS )

  ! Read the parameters (Parameters are always read from the first processor)
  PROC_ID = 1_JPIB_K
  LOC_DIR = REPEAT( ' ', 1024 )
  WRITE(LOC_DIR,'(A,A,I6.6,A)', IOSTAT=WRITE_STATUS) TRIM(ADJUSTL(DUMP_PATH)), '/io_serv.', PROC_ID,'.d'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  ! Read the parameters file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_CREATE) PAR_CREATE_NAME( TRIM(ADJUSTL(LOC_DIR)), PROC_ID, PARFNAME, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_ROPEN) PAR_ROPEN( PARFNAME, PARUNIT, BIG_ENDIAN_READ, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_READ) PAR_READ( PARAMETERS, PARUNIT, VERBOSE, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_CLOSE) PAR_CLOSE( PARUNIT, HOOKS )

  ! WRITE(*,*) 'PARFNAME: ', TRIM(ADJUSTL(PARFNAME)), BIG_ENDIAN_READ
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_CREATE) PAR_PRINT( PARAMETERS, 6_JPIB_K, HOOKS )

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
    CASE(ERRFLAG_GET_DUMP_DIRECTORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get dump directory' )
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

END FUNCTION SHOW_PARAMS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INITIALIZE_OUTPUT_MANAGER'
PP_THREAD_SAFE FUNCTION INITIALIZE_OUTPUT_MANAGER( CMDARGS, TOPOLOGY, PARAMETERS, OUTPUT_MANAGER, TOC, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: TOC_CONTAINER_T
  USE :: MULTIOM_API, ONLY: MODEL_PAR_T
  USE :: MULTIOM_API, ONLY: PROC_TOPO_T
  USE :: MULTIOM_API, ONLY: PAR_PRINT
  USE :: MULTIOM_API, ONLY: PAR_CREATE_NAME
  USE :: MULTIOM_API, ONLY: PAR_ROPEN
  USE :: MULTIOM_API, ONLY: PAR_READ
  USE :: MULTIOM_API, ONLY: PAR_CLOSE
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: MULTIOM_API, ONLY: MAKE_OUTPUT_MANAGER
  USE :: MULTIOM_API, ONLY: TOC_READ_LIST

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T),                        INTENT(IN)    :: CMDARGS
  TYPE(PROC_TOPO_T),                                INTENT(INOUT) :: TOPOLOGY
  TYPE(MODEL_PAR_T),                                INTENT(INOUT) :: PARAMETERS
  CLASS(OUTPUT_MANAGER_BASE_A), POINTER,            INTENT(INOUT) :: OUTPUT_MANAGER
  TYPE(TOC_CONTAINER_T), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: TOC
  LOGICAL,                                          INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                                          INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=1024)  :: LOC_DIR
  CHARACTER(LEN=1024)  :: PARFNAME
  CHARACTER(LEN=1024)  :: DUMP_PATH
  CHARACTER(LEN=1024)  :: CONFIGURATION_FILE
  CHARACTER(LEN=1024)  :: OUTPUT_MANAGER_TYPE
  INTEGER(KIND=JPIB_K) :: NUMBER_OF_PROCESSORS
  INTEGER(KIND=JPIB_K) :: PROC_ID
  INTEGER(KIND=JPIB_K) :: PARUNIT
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: PROCESSOR_LIST

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_CREATE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_ROPEN = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_READ = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_CLOSE = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_DUMP_DIRECTORY = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_CONFIGURATION_FILE = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_OUTPUTMANAGER_TYPE = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAKE_OUTPUT_MANAGER = 10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_ALL_TOC = 11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_PROCESSOR_LIST = 12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_PROCESSOR_LIST = 13_JPIB_K

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

  ! Initialization
  LOC_DIR = REPEAT( ' ', 1024 )
  PARFNAME = REPEAT( ' ', 1024 )
  DUMP_PATH = REPEAT( ' ', 1024 )
  CONFIGURATION_FILE = REPEAT( ' ', 1024 )
  OUTPUT_MANAGER_TYPE = REPEAT( ' ', 1024 )
  NUMBER_OF_PROCESSORS = -999_JPIB_K

  ! Fill the topology object
  TOPOLOGY%NCOMM_W1IO = 1_JPIB_K
  TOPOLOGY%NCOMM_WR = 1_JPIB_K
  TOPOLOGY%NCOMM_IO = 1_JPIB_K
  TOPOLOGY%MYPROC_WRIO = 1_JPIB_K
  TOPOLOGY%MYPROC_IO = 1_JPIB_K
  TOPOLOGY%MYPROC_WR = 1_JPIB_K
  TOPOLOGY%NPROC_IO = 1_JPIB_K
  TOPOLOGY%LIO_SERVER = .TRUE.
  TOPOLOGY%LIO_CLIENT = .FALSE.
  PROC_ID = 1_JPIB_K

  ! Read command line arguments
  PP_TRYCALL(ERRFLAG_GET_DUMP_DIRECTORY) CMDARGS%GET_DUMP_PATH( DUMP_PATH, HOOKS )
  PP_TRYCALL(ERRFLAG_GET_CONFIGURATION_FILE) CMDARGS%GET_CONFIGURATION_FILE( CONFIGURATION_FILE, HOOKS )
  PP_TRYCALL(ERRFLAG_GET_OUTPUTMANAGER_TYPE) CMDARGS%GET_TYPE( OUTPUT_MANAGER_TYPE, HOOKS )

  ! Read the parameters (Parameters are always read from the first processor)
  WRITE(LOC_DIR,'(A,A,I6.6,A)', IOSTAT=WRITE_STATUS) TRIM(ADJUSTL(DUMP_PATH)), '/io_serv.', PROC_ID,'.d'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  ! Read the parameters file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_CREATE) PAR_CREATE_NAME( TRIM(ADJUSTL(LOC_DIR)), PROC_ID, PARFNAME, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_ROPEN) PAR_ROPEN( PARFNAME, PARUNIT, BIG_ENDIAN_READ, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_READ) PAR_READ( PARAMETERS, PARUNIT, VERBOSE, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_CLOSE) PAR_CLOSE( PARUNIT, HOOKS )

  ! Get the total number of processors
  NUMBER_OF_PROCESSORS = PARAMETERS%SIM_%NPROC_IO

  ! Get the processor list from the command line arguments
  PP_TRYCALL(ERRFLAG_GET_PROCESSOR_LIST) CMDARGS%GET_PROCESSOR_LIST( PROCESSOR_LIST, NUMBER_OF_PROCESSORS, HOOKS )

  ! Create the output manager
  PP_TRYCALL(ERRFLAG_MAKE_OUTPUT_MANAGER) MAKE_OUTPUT_MANAGER( OUTPUT_MANAGER_TYPE, TOPOLOGY, PARAMETERS, CONFIGURATION_FILE, OUTPUT_MANAGER, HOOKS )

  ! Load table of contents
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_ALL_TOC) TOC_READ_LIST( TRIM(ADJUSTL(DUMP_PATH)), TOC, PROCESSOR_LIST, BIG_ENDIAN_READ, VERBOSE, HOOKS )

  ! Free processor list
  DEALLOCATE( PROCESSOR_LIST, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_PROCESSOR_LIST )

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
    CASE(ERRFLAG_GET_DUMP_DIRECTORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get dump directory' )
    CASE(ERRFLAG_GET_CONFIGURATION_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get configuration file' )
    CASE(ERRFLAG_GET_OUTPUTMANAGER_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get output manager type' )
    CASE(ERRFLAG_MAKE_OUTPUT_MANAGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to make output manager' )
    CASE(ERRFLAG_UNABLE_TO_READ_ALL_TOC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read all table of contents' )
    CASE(ERRFLAG_GET_PROCESSOR_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get processor list' )
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE_PROCESSOR_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate processor list' )
      IF ( ALLOCATED(ERRMSG)) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS )
      END IF
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

END FUNCTION INITIALIZE_OUTPUT_MANAGER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FINALIZE_OUTPUT_MANAGER'
PP_THREAD_SAFE FUNCTION FINALIZE_OUTPUT_MANAGER( CMDARGS, OUTPUT_MANAGER, TOC, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: TOC_CONTAINER_T
  USE :: MULTIOM_API, ONLY: TOC_FREE
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: MULTIOM_API, ONLY: DESTROY_OUTPUT_MANAGER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T),                        INTENT(IN)    :: CMDARGS
  CLASS(OUTPUT_MANAGER_BASE_A), POINTER,            INTENT(INOUT) :: OUTPUT_MANAGER
  TYPE(TOC_CONTAINER_T), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: TOC
  LOGICAL,                                          INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                                          INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DESTROY_OUTPUT_MANAGER = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_TABLE_OF_CONTENTS = 2_JPIB_K

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

  ! Create the output manager
  PP_TRYCALL(ERRFLAG_DESTROY_OUTPUT_MANAGER) DESTROY_OUTPUT_MANAGER( OUTPUT_MANAGER, HOOKS )

  ! Fee table of contents
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DESTROY_TABLE_OF_CONTENTS) TOC_FREE( TOC, HOOKS )

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
    CASE(ERRFLAG_DESTROY_OUTPUT_MANAGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to destroy output manager' )
    CASE(ERRFLAG_UNABLE_TO_DESTROY_TABLE_OF_CONTENTS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to destroy table of contents' )
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

END FUNCTION FINALIZE_OUTPUT_MANAGER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INITIALIZE_TABLE_OF_CONTENTS'
PP_THREAD_SAFE FUNCTION INITIALIZE_TABLE_OF_CONTENTS( CMDARGS, TOC, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: MODEL_PAR_T
  USE :: MULTIOM_API, ONLY: PROC_TOPO_T
  USE :: MULTIOM_API, ONLY: PAR_CREATE_NAME
  USE :: MULTIOM_API, ONLY: PAR_ROPEN
  USE :: MULTIOM_API, ONLY: PAR_READ
  USE :: MULTIOM_API, ONLY: PAR_CLOSE
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: TOC_CONTAINER_T
  USE :: MULTIOM_API, ONLY: TOC_READ_LIST

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T),                        INTENT(IN)    :: CMDARGS
  TYPE(TOC_CONTAINER_T), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: TOC
  LOGICAL,                                          INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                                          INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(MODEL_PAR_T)    :: PARAMETERS
  CHARACTER(LEN=1024)  :: LOC_DIR
  CHARACTER(LEN=1024)  :: PARFNAME
  CHARACTER(LEN=1024)  :: DUMP_PATH
  INTEGER(KIND=JPIB_K) :: NUMBER_OF_PROCESSORS
  INTEGER(KIND=JPIB_K) :: PROC_ID
  INTEGER(KIND=JPIB_K) :: PARUNIT
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: PROCESSOR_LIST

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_CREATE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_ROPEN = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_READ = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PAR_CLOSE = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_DUMP_DIRECTORY = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_PROCESSOR_LIST = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_ALL_TOC = 10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_PROCESSOR_LIST = 11_JPIB_K

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
  LOC_DIR = REPEAT( ' ', 1024 )
  PARFNAME = REPEAT( ' ', 1024 )
  DUMP_PATH = REPEAT( ' ', 1024 )
  PROC_ID = 1_JPIB_K

  ! Read command line arguments
  PP_TRYCALL(ERRFLAG_GET_DUMP_DIRECTORY) CMDARGS%GET_DUMP_PATH( DUMP_PATH, HOOKS )

  ! Read the parameters (Parameters are always read from the first processor)
  WRITE(LOC_DIR,'(A,A,I6.6,A)', IOSTAT=WRITE_STATUS) TRIM(ADJUSTL(DUMP_PATH)), '/io_serv.', PROC_ID,'.d'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  ! Read the parameters file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_CREATE) PAR_CREATE_NAME( TRIM(ADJUSTL(LOC_DIR)), PROC_ID, PARFNAME, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_ROPEN) PAR_ROPEN( PARFNAME, PARUNIT, BIG_ENDIAN_READ, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_READ) PAR_READ( PARAMETERS, PARUNIT, VERBOSE, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PAR_CLOSE) PAR_CLOSE( PARUNIT, HOOKS )

  ! Get the total number of processors
  NUMBER_OF_PROCESSORS = PARAMETERS%SIM_%NPROC_IO

  ! Get the processor list from the command line arguments
  PP_TRYCALL(ERRFLAG_GET_PROCESSOR_LIST) CMDARGS%GET_PROCESSOR_LIST( PROCESSOR_LIST, NUMBER_OF_PROCESSORS, HOOKS )

  ! Load table of contents
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_ALL_TOC) TOC_READ_LIST( TRIM(ADJUSTL(DUMP_PATH)), TOC, PROCESSOR_LIST, BIG_ENDIAN_READ, VERBOSE, HOOKS )

  ! Free processor list
  DEALLOCATE( PROCESSOR_LIST, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_PROCESSOR_LIST )

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
    CASE(ERRFLAG_GET_DUMP_DIRECTORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get dump directory' )
    CASE(ERRFLAG_UNABLE_TO_READ_ALL_TOC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read all table of contents' )
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE_PROCESSOR_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate processor list' )
      IF ( ALLOCATED(ERRMSG)) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS )
      END IF
    CASE(ERRFLAG_GET_PROCESSOR_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get processor list' )
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

END FUNCTION INITIALIZE_TABLE_OF_CONTENTS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FINALIZE_TABLE_OF_CONTENTS'
PP_THREAD_SAFE FUNCTION FINALIZE_TABLE_OF_CONTENTS( CMDARGS, TOC, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: TOC_CONTAINER_T
  USE :: MULTIOM_API, ONLY: TOC_FREE
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
  TYPE(COMMAND_LINE_ARGS_T),                        INTENT(IN)    :: CMDARGS
  TYPE(TOC_CONTAINER_T), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: TOC
  LOGICAL,                                          INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                                          INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_TABLE_OF_CONTENTS = 2_JPIB_K

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

  ! Fee table of contents
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DESTROY_TABLE_OF_CONTENTS) TOC_FREE( TOC, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_DESTROY_TABLE_OF_CONTENTS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to destroy table of contents' )
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

END FUNCTION FINALIZE_TABLE_OF_CONTENTS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_HEADER_PVT'
PP_THREAD_SAFE FUNCTION PRINT_HEADER_PVT( CMDARGS, OUTPUT_UNIT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
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
  TYPE(COMMAND_LINE_ARGS_T), INTENT(IN)    :: CMDARGS
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: OUTPUT_UNIT
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STAT

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K

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

  ! Print the header separator
  WRITE( UNIT=OUTPUT_UNIT, FMT='(1x,A)', IOSTAT=WRITE_STAT ) REPEAT('-', NCELLS*CELL_SIZE+(NCELLS+1)*SEP_SIZE)
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  ! Print the header
  WRITE( UNIT=OUTPUT_UNIT, FMT=PRINT_FORMAT, IOSTAT=WRITE_STAT ) &
& NORMALIZE_STRING( 'solver' ) , &
& NORMALIZE_STRING( 'param' ) , &
& NORMALIZE_STRING( 'repres' ) , &
& NORMALIZE_STRING( 'levtype' ) , &
& NORMALIZE_STRING( 'levelist' ) , &
& NORMALIZE_STRING( 'frequency' ) , &
& NORMALIZE_STRING( 'direction' ) , &
& NORMALIZE_STRING( 'step' )
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  ! Print the header separator
  WRITE( UNIT=OUTPUT_UNIT, FMT='(1x,A)', IOSTAT=WRITE_STAT ) REPEAT('-', NCELLS*CELL_SIZE+(NCELLS+1)*SEP_SIZE)
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

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

END FUNCTION PRINT_HEADER_PVT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_HEADER'
PP_THREAD_SAFE FUNCTION PRINT_HEADER( CMDARGS, OUTPUT_UNIT, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
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
  TYPE(COMMAND_LINE_ARGS_T), INTENT(IN)    :: CMDARGS
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: OUTPUT_UNIT
  LOGICAL,                   INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                   INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STAT

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K

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

  ! Print the header separator
  WRITE( UNIT=OUTPUT_UNIT, FMT=PRINT_FORMAT, IOSTAT=WRITE_STAT ) 'multio output manager'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

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

END FUNCTION PRINT_HEADER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_SIMULATION_INIT'
PP_THREAD_SAFE FUNCTION PRINT_SIMULATION_INIT( CMDARGS, OUTPUT_UNIT, TOC_ENTRY, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: TOC_SIM_INIT_T
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
  TYPE(COMMAND_LINE_ARGS_T), INTENT(IN)    :: CMDARGS
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: OUTPUT_UNIT
  TYPE(TOC_SIM_INIT_T),      INTENT(IN)    :: TOC_ENTRY
  LOGICAL,                   INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                   INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STAT

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K

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

  ! Print the header
  WRITE( UNIT=OUTPUT_UNIT, FMT='(A)', IOSTAT=WRITE_STAT ) ''
  WRITE( UNIT=OUTPUT_UNIT, FMT='(1x,A)', IOSTAT=WRITE_STAT ) 'Begin of simulation'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  ! Print the header separator
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_TO_STRING) PRINT_HEADER_PVT( CMDARGS, OUTPUT_UNIT, HOOKS )

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

END FUNCTION PRINT_SIMULATION_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_ATM_MESSAGE'
PP_THREAD_SAFE FUNCTION PRINT_ATM_MESSAGE( CMDARGS, OUTPUT_UNIT, TOC_ENTRY, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: TOC_ATM_FIELD_T
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: OM_ATM_MSG_T
  USE :: ENUMERATORS_MOD, ONLY: IREPRES2CREPRES
  USE :: ENUMERATORS_MOD, ONLY: IPREFIX2ILEVTYPE
  USE :: ENUMERATORS_MOD, ONLY: ILEVTYPE2CLEVTYPE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T), INTENT(IN)    :: CMDARGS
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: OUTPUT_UNIT
  TYPE(TOC_ATM_FIELD_T),     INTENT(IN)    :: TOC_ENTRY
  LOGICAL,                   INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                   INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(OM_ATM_MSG_T)   :: MESSAGE
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  INTEGER(KIND=JPIB_K) :: ILEVTYPE
  CHARACTER(LEN=128)   :: CREPRES
  CHARACTER(LEN=128)   :: CLEVTYPE
  CHARACTER(LEN=32)    :: CPARAM
  CHARACTER(LEN=32)    :: CSTEP
  CHARACTER(LEN=32)    :: CLEVEL
  CHARACTER(LEN=1024)  :: DUMP_PATH
  LOGICAL :: MATCH

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_REPRES = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_ILEVTYPE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_CLEVTYPE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_DUMP_DIRECTORY = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_ATM_MESSAGE = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH = 8_JPIB_K

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

  ! Read command line arguments
  PP_TRYCALL(ERRFLAG_GET_DUMP_DIRECTORY) CMDARGS%GET_DUMP_PATH( DUMP_PATH, HOOKS )

  ! Read the next message to be processed
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_ATM_MESSAGE)   READ_ATM_MESSAGE( TRIM(ADJUSTL(DUMP_PATH)), &
&       TOC_ENTRY%PROC_ID_, TOC_ENTRY%MSG_ID_, TOC_ENTRY%MSG_ADDR_, &
&        MESSAGE, BIG_ENDIAN_READ, VERBOSE, HOOKS   )

  ! Check if the message matches teh filters
  PP_TRYCALL(ERRFLAG_MATCH) CMDARGS%MATCH( &
&    TOC_ENTRY%PARAM_ID_, &
&    TOC_ENTRY%REPRES_ID_, &
&    TOC_ENTRY%PREFIX_ID_, &
&    MESSAGE%ILEVG_, &
&    TOC_ENTRY%STEP_ID_, &
&    MATCH, &
&    HOOKS )


  IF ( MATCH ) THEN

    ! Initialization of local variables
    CREPRES = REPEAT(' ',128)
    CLEVTYPE = REPEAT(' ',128)

    ! Convert to enumerators to string
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_REPRES) IREPRES2CREPRES(TOC_ENTRY%REPRES_ID_, CREPRES, HOOKS)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_ILEVTYPE) IPREFIX2ILEVTYPE( TOC_ENTRY%PREFIX_ID_, &
&                TOC_ENTRY%PARAM_ID_, TOC_ENTRY%U_ID_,TOC_ENTRY%REPRES_ID_, ILEVTYPE, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_CLEVTYPE) ILEVTYPE2CLEVTYPE( ILEVTYPE, CLEVTYPE, HOOKS )

    ! Convert integers to string
    CPARAM = REPEAT(' ',32)
    WRITE(CPARAM,'(I32)', IOSTAT=WRITE_STAT) TOC_ENTRY%PARAM_ID_
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

    CSTEP = REPEAT(' ',32)
    WRITE(CSTEP,'(I32)', IOSTAT=WRITE_STAT) TOC_ENTRY%STEP_ID_
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

    CLEVEL = REPEAT(' ',32)
    WRITE(CLEVEL,'(I32)', IOSTAT=WRITE_STAT) MESSAGE%ILEVG_
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

    ! Print the header
    WRITE( UNIT=OUTPUT_UNIT, FMT=PRINT_FORMAT, IOSTAT=WRITE_STAT ) &
&   NORMALIZE_STRING( 'atm.' ) , &
&   NORMALIZE_STRING( TRIM(ADJUSTL(CPARAM)) ) , &
&   NORMALIZE_STRING( TRIM(ADJUSTL(CREPRES)) ) , &
&   NORMALIZE_STRING( TRIM(ADJUSTL(CLEVTYPE)) ) , &
&   NORMALIZE_STRING( TRIM(ADJUSTL(CLEVEL)) ) , &
&   NORMALIZE_STRING( TRIM(ADJUSTL('not-defined')) ) , &
&   NORMALIZE_STRING( TRIM(ADJUSTL('not-defined')) ) , &
&   NORMALIZE_STRING( TRIM(ADJUSTL(CSTEP)) )
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )
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
    CASE(ERRFLAG_UNABLE_TO_CONVERT_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert representation' )
    CASE(ERRFLAG_UNABLE_TO_CONVERT_ILEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert ilevtype' )
    CASE(ERRFLAG_UNABLE_TO_CONVERT_CLEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert clevtype' )
    CASE(ERRFLAG_GET_DUMP_DIRECTORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get dump directory' )
    CASE(ERRFLAG_UNABLE_TO_READ_ATM_MESSAGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read atm message' )
    CASE(ERRFLAG_MATCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to match' )
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

END FUNCTION PRINT_ATM_MESSAGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_WAM_MESSAGE'
PP_THREAD_SAFE FUNCTION PRINT_WAM_MESSAGE( CMDARGS, OUTPUT_UNIT, TOC_ENTRY, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: TOC_WAM_FIELD_T
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: OM_WAM_MSG_T
  USE :: ENUMERATORS_MOD, ONLY: IREPRES2CREPRES
  USE :: ENUMERATORS_MOD, ONLY: IPREFIX2ILEVTYPE
  USE :: ENUMERATORS_MOD, ONLY: ILEVTYPE2CLEVTYPE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T), INTENT(IN)    :: CMDARGS
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: OUTPUT_UNIT
  TYPE(TOC_WAM_FIELD_T),     INTENT(IN)    :: TOC_ENTRY
  LOGICAL,                   INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                   INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(OM_WAM_MSG_T)   :: MESSAGE
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  INTEGER(KIND=JPIB_K) :: ILEVTYPE
  CHARACTER(LEN=128)   :: CREPRES
  CHARACTER(LEN=128)   :: CLEVTYPE
  CHARACTER(LEN=32)    :: CPARAM
  CHARACTER(LEN=32)    :: CSTEP
  CHARACTER(LEN=32)    :: CLEVEL
  CHARACTER(LEN=32)    :: CFREQ
  CHARACTER(LEN=32)    :: CANGLE
  CHARACTER(LEN=1024)  :: DUMP_PATH
  LOGICAL :: MATCH

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_REPRES = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_ILEVTYPE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_CLEVTYPE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_DUMP_DIRECTORY = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_ATM_MESSAGE = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH = 8_JPIB_K

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

  ! Read command line arguments
  PP_TRYCALL(ERRFLAG_GET_DUMP_DIRECTORY) CMDARGS%GET_DUMP_PATH( DUMP_PATH, HOOKS )

  ! Read the next message to be processed
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_ATM_MESSAGE)   READ_WAM_MESSAGE( TRIM(ADJUSTL(DUMP_PATH)), &
&       TOC_ENTRY%PROC_ID_, TOC_ENTRY%MSG_ID_, TOC_ENTRY%MSG_ADDR_, &
&        MESSAGE, BIG_ENDIAN_READ, VERBOSE, HOOKS   )

  IF ( TOC_ENTRY%PARAM_ID_ .EQ. 140250 ) THEN
    ! Check if the message matches teh filters
    PP_TRYCALL(ERRFLAG_MATCH) CMDARGS%MATCH( &
&      TOC_ENTRY%PARAM_ID_, &
&      TOC_ENTRY%REPRES_ID_, &
&      TOC_ENTRY%PREFIX_ID_, &
&      MESSAGE%IFREQ, &
&      MESSAGE%IANGLE, &
&      TOC_ENTRY%STEP_ID_, &
&      MATCH, &
&      HOOKS )
  ELSE
    ! Check if the message matches teh filters
    PP_TRYCALL(ERRFLAG_MATCH) CMDARGS%MATCH( &
&      TOC_ENTRY%PARAM_ID_, &
&      TOC_ENTRY%REPRES_ID_, &
&      TOC_ENTRY%PREFIX_ID_, &
&      MESSAGE%KLEV, &
&      TOC_ENTRY%STEP_ID_, &
&      MATCH, &
&      HOOKS )
  ENDIF


  IF ( MATCH ) THEN

    ! Initialization of local variables
    CREPRES = REPEAT(' ',128)
    CLEVTYPE = REPEAT(' ',128)

    ! Convert to enumerators to string
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_REPRES) IREPRES2CREPRES(TOC_ENTRY%REPRES_ID_, CREPRES, HOOKS)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_ILEVTYPE) IPREFIX2ILEVTYPE( TOC_ENTRY%PREFIX_ID_, &
&                TOC_ENTRY%PARAM_ID_, TOC_ENTRY%U_ID_,TOC_ENTRY%REPRES_ID_, ILEVTYPE, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_CLEVTYPE) ILEVTYPE2CLEVTYPE( ILEVTYPE, CLEVTYPE, HOOKS )

    ! Convert integers to string
    CPARAM = REPEAT(' ',32)
    WRITE(CPARAM,'(I32)', IOSTAT=WRITE_STAT) TOC_ENTRY%PARAM_ID_
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

    CSTEP = REPEAT(' ',32)
    WRITE(CSTEP,'(I32)', IOSTAT=WRITE_STAT) TOC_ENTRY%STEP_ID_
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

    IF ( TOC_ENTRY%PARAM_ID_ .EQ. 140250 ) THEN
      ! Convert to string
      CLEVEL = REPEAT(' ',32)
      WRITE(CLEVEL,'(I32)', IOSTAT=WRITE_STAT) MESSAGE%KLEV
      PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

      ! Print the header
      WRITE( UNIT=OUTPUT_UNIT, FMT=PRINT_FORMAT, IOSTAT=WRITE_STAT ) &
&     NORMALIZE_STRING( 'wam.' ) , &
&     NORMALIZE_STRING( TRIM(ADJUSTL(CPARAM)) ) , &
&     NORMALIZE_STRING( TRIM(ADJUSTL(CREPRES)) ) , &
&     NORMALIZE_STRING( TRIM(ADJUSTL(CLEVTYPE)) ) , &
&     NORMALIZE_STRING( TRIM(ADJUSTL(CLEVEL)) ) , &
&     NORMALIZE_STRING( TRIM(ADJUSTL('not-defined')) ) , &
&     NORMALIZE_STRING( TRIM(ADJUSTL('not-defined')) ) , &
&     NORMALIZE_STRING( TRIM(ADJUSTL(CSTEP)) )
      PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )
    ELSE
      ! Convert to string
      CFREQ = REPEAT(' ',32)
      WRITE(CFREQ,'(I32)', IOSTAT=WRITE_STAT) MESSAGE%IFREQ
      PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )
      ! Convert to string
      CANGLE = REPEAT(' ',32)
      WRITE(CANGLE,'(I32)', IOSTAT=WRITE_STAT) MESSAGE%IANGLE
      PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )
      ! Print the header
      WRITE( UNIT=OUTPUT_UNIT, FMT=PRINT_FORMAT, IOSTAT=WRITE_STAT ) &
&     NORMALIZE_STRING( 'wam.' ) , &
&     NORMALIZE_STRING( TRIM(ADJUSTL(CPARAM)) ) , &
&     NORMALIZE_STRING( TRIM(ADJUSTL(CREPRES)) ) , &
&     NORMALIZE_STRING( TRIM(ADJUSTL(CLEVTYPE)) ) , &
&     NORMALIZE_STRING( TRIM(ADJUSTL('not-defined')) ) , &
&     NORMALIZE_STRING( TRIM(ADJUSTL(CFREQ)) ) , &
&     NORMALIZE_STRING( TRIM(ADJUSTL(CANGLE)) ) , &
&     NORMALIZE_STRING( TRIM(ADJUSTL(CSTEP)) )
      PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )
    ENDIF
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
    CASE(ERRFLAG_UNABLE_TO_CONVERT_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert representation' )
    CASE(ERRFLAG_UNABLE_TO_CONVERT_ILEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert ilevtype' )
    CASE(ERRFLAG_UNABLE_TO_CONVERT_CLEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert clevtype' )
    CASE(ERRFLAG_GET_DUMP_DIRECTORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get dump directory' )
    CASE(ERRFLAG_UNABLE_TO_READ_ATM_MESSAGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read atm message' )
    CASE(ERRFLAG_MATCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to match' )
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

END FUNCTION PRINT_WAM_MESSAGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_FLUSH'
PP_THREAD_SAFE FUNCTION PRINT_FLUSH( CMDARGS, OUTPUT_UNIT, TOC_ENTRY, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: TOC_FLUSH_STEP_T
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
  TYPE(COMMAND_LINE_ARGS_T), INTENT(IN)    :: CMDARGS
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: OUTPUT_UNIT
  TYPE(TOC_FLUSH_STEP_T),    INTENT(IN)    :: TOC_ENTRY
  LOGICAL,                   INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                   INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  CHARACTER(LEN=32) :: CSTEP

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K

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

  ! Convert step to string
  CSTEP = REPEAT(' ',32)
  WRITE(CSTEP,'(I32)') TOC_ENTRY%STEP_


  ! Print the header separator
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_TO_STRING) PRINT_HEADER_PVT( CMDARGS, OUTPUT_UNIT, HOOKS )
  WRITE( UNIT=OUTPUT_UNIT, FMT='(1x,A)', IOSTAT=WRITE_STAT ) 'Flush step: '//TRIM(ADJUSTL(CSTEP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )
  WRITE( UNIT=OUTPUT_UNIT, FMT='(A)', IOSTAT=WRITE_STAT ) ' '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )
  WRITE( UNIT=OUTPUT_UNIT, FMT='(A)', IOSTAT=WRITE_STAT ) ' '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  ! Print the header separator
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_TO_STRING) PRINT_HEADER_PVT( CMDARGS, OUTPUT_UNIT, HOOKS )


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

END FUNCTION PRINT_FLUSH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_FLUSH_AND_RESTART'
PP_THREAD_SAFE FUNCTION PRINT_FLUSH_AND_RESTART( CMDARGS, OUTPUT_UNIT, TOC_ENTRY, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: TOC_FLUSH_STEP_RST_T
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
  TYPE(COMMAND_LINE_ARGS_T),  INTENT(IN)    :: CMDARGS
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: OUTPUT_UNIT
  TYPE(TOC_FLUSH_STEP_RST_T), INTENT(IN)    :: TOC_ENTRY
  LOGICAL,                    INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                    INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  CHARACTER(LEN=32) :: CSTEP

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K

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

  ! Convert step to string
  CSTEP = REPEAT(' ',32)
  WRITE(CSTEP,'(I32)') TOC_ENTRY%STEP_

  ! Print the header separator
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_TO_STRING) PRINT_HEADER_PVT( CMDARGS, OUTPUT_UNIT, HOOKS )
  WRITE( UNIT=OUTPUT_UNIT, FMT='(1x,A)', IOSTAT=WRITE_STAT ) 'Flush step and restart: '//TRIM(ADJUSTL(CSTEP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )
  WRITE( UNIT=OUTPUT_UNIT, FMT='(A)', IOSTAT=WRITE_STAT ) ' '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )
  WRITE( UNIT=OUTPUT_UNIT, FMT='(A)', IOSTAT=WRITE_STAT ) ' '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

  ! Print the header separator
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_TO_STRING) PRINT_HEADER_PVT( CMDARGS, OUTPUT_UNIT, HOOKS )



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

END FUNCTION PRINT_FLUSH_AND_RESTART
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_FLUSH_LAST_STEP'
PP_THREAD_SAFE FUNCTION PRINT_FLUSH_LAST_STEP( CMDARGS, OUTPUT_UNIT, TOC_ENTRY, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: TOC_FLUSH_LAST_STEP_T
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
  TYPE(COMMAND_LINE_ARGS_T),   INTENT(IN)    :: CMDARGS
  INTEGER(KIND=JPIB_K),        INTENT(IN)    :: OUTPUT_UNIT
  TYPE(TOC_FLUSH_LAST_STEP_T), INTENT(IN)    :: TOC_ENTRY
  LOGICAL,                     INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                     INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STAT

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K

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


  ! Print the header separator
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_TO_STRING) PRINT_HEADER_PVT( CMDARGS, OUTPUT_UNIT, HOOKS )
  WRITE( UNIT=OUTPUT_UNIT, FMT='(1x,A)', IOSTAT=WRITE_STAT ) 'Flush last step: '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )
  WRITE( UNIT=OUTPUT_UNIT, FMT='(A)', IOSTAT=WRITE_STAT ) ' '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )
  WRITE( UNIT=OUTPUT_UNIT, FMT='(A)', IOSTAT=WRITE_STAT ) ' '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

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

END FUNCTION PRINT_FLUSH_LAST_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_SIMULATION_END'
PP_THREAD_SAFE FUNCTION PRINT_SIMULATION_END( CMDARGS, OUTPUT_UNIT, TOC_ENTRY, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: TOC_SIM_END_T
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
  TYPE(COMMAND_LINE_ARGS_T), INTENT(IN)    :: CMDARGS
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: OUTPUT_UNIT
  TYPE(TOC_SIM_END_T),       INTENT(IN)    :: TOC_ENTRY
  LOGICAL,                   INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                   INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STAT

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K

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

  ! Print the header
  WRITE( UNIT=OUTPUT_UNIT, FMT='(A)', IOSTAT=WRITE_STAT ) ' '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )
  WRITE( UNIT=OUTPUT_UNIT, FMT='(A)', IOSTAT=WRITE_STAT ) ' '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )
  WRITE( UNIT=OUTPUT_UNIT, FMT='(1x,A)', IOSTAT=WRITE_STAT ) 'End Of simulation'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )


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

END FUNCTION PRINT_SIMULATION_END
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_FOOTER'
PP_THREAD_SAFE FUNCTION PRINT_FOOTER( CMDARGS, OUTPUT_UNIT, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
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
  TYPE(COMMAND_LINE_ARGS_T), INTENT(IN)    :: CMDARGS
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: OUTPUT_UNIT
  LOGICAL,                   INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                   INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STAT

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K

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

  ! Print the header separator
  WRITE( UNIT=OUTPUT_UNIT, FMT='(1x,A)', IOSTAT=WRITE_STAT ) REPEAT('-', NCELLS*CELL_SIZE+(NCELLS+1)*SEP_SIZE)
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

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

END FUNCTION PRINT_FOOTER
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
#define PP_PROCEDURE_NAME 'ENCODE_HEADER'
PP_THREAD_SAFE FUNCTION ENCODE_HEADER( CMDARGS, OUTPUT_MANAGER, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: OUTPUT_MANAGER_BASE_A
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
  TYPE(COMMAND_LINE_ARGS_T),             INTENT(IN)    :: CMDARGS
  CLASS(OUTPUT_MANAGER_BASE_A), POINTER, INTENT(INOUT) :: OUTPUT_MANAGER
  LOGICAL,                               INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                               INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

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

END FUNCTION ENCODE_HEADER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_SIMULATION_INIT'
PP_THREAD_SAFE FUNCTION ENCODE_SIMULATION_INIT( CMDARGS, OUTPUT_MANAGER, TOC_ENTRY, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: MULTIOM_API, ONLY: TOC_SIM_INIT_T
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
  TYPE(COMMAND_LINE_ARGS_T),             INTENT(IN)    :: CMDARGS
  CLASS(OUTPUT_MANAGER_BASE_A), POINTER, INTENT(INOUT) :: OUTPUT_MANAGER
  TYPE(TOC_SIM_INIT_T),                  INTENT(IN)    :: TOC_ENTRY
  LOGICAL,                               INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                               INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

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

END FUNCTION ENCODE_SIMULATION_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_ATM_MESSAGE'
PP_THREAD_SAFE FUNCTION ENCODE_ATM_MESSAGE( CMDARGS, OUTPUT_MANAGER, TOC_ENTRY, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: MULTIOM_API, ONLY: TOC_ATM_FIELD_T
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: OM_ATM_MSG_T
  USE :: ENUMERATORS_MOD, ONLY: IREPRES2CREPRES
  USE :: ENUMERATORS_MOD, ONLY: IPREFIX2ILEVTYPE
  USE :: ENUMERATORS_MOD, ONLY: ILEVTYPE2CLEVTYPE
  USE :: ENUMERATORS_MOD, ONLY: VALUES_SP_E
  USE :: ENUMERATORS_MOD, ONLY: VALUES_DP_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T),             INTENT(IN)    :: CMDARGS
  CLASS(OUTPUT_MANAGER_BASE_A), POINTER, INTENT(INOUT) :: OUTPUT_MANAGER
  TYPE(TOC_ATM_FIELD_T),                 INTENT(IN)    :: TOC_ENTRY
  LOGICAL,                               INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                               INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(OM_ATM_MSG_T)   :: MESSAGE
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  INTEGER(KIND=JPIB_K) :: ILEVTYPE
  CHARACTER(LEN=128)   :: CREPRES
  CHARACTER(LEN=128)   :: CLEVTYPE
  CHARACTER(LEN=32)    :: CPARAM
  CHARACTER(LEN=32)    :: CSTEP
  CHARACTER(LEN=32)    :: CLEVEL
  CHARACTER(LEN=1024)  :: DUMP_PATH
  LOGICAL :: MATCH
  LOGICAL :: SKIP_VALUES
  REAL(KIND=REAL32), POINTER, DIMENSION(:) :: VALUES_SP
  REAL(KIND=REAL64), POINTER, DIMENSION(:) :: VALUES_DP

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_REPRES = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_ILEVTYPE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_CLEVTYPE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_DUMP_DIRECTORY = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_ATM_MESSAGE = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_SKIP_VALUES = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_VAL_SP = 10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_ATM_SP = 11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_VAL_DP = 12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_ATM_DP = 13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_VAL_TYPE = 14_JPIB_K

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

  ! Read command line arguments
  PP_TRYCALL(ERRFLAG_GET_DUMP_DIRECTORY) CMDARGS%GET_DUMP_PATH( DUMP_PATH, HOOKS )
  PP_TRYCALL(ERRFLAG_GET_SKIP_VALUES) CMDARGS%GET_SKIP_VALUES( SKIP_VALUES, HOOKS )

  ! Read the next message to be processed
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_ATM_MESSAGE) READ_ATM_MESSAGE( TRIM(ADJUSTL(DUMP_PATH)), &
&      TOC_ENTRY%PROC_ID_, TOC_ENTRY%MSG_ID_, TOC_ENTRY%MSG_ADDR_, &
&       MESSAGE, BIG_ENDIAN_READ, VERBOSE, HOOKS   )

  ! Check if the message matches teh filters
  PP_TRYCALL(ERRFLAG_MATCH) CMDARGS%MATCH( &
&    TOC_ENTRY%PARAM_ID_, &
&    TOC_ENTRY%REPRES_ID_, &
&    TOC_ENTRY%PREFIX_ID_, &
&    MESSAGE%ILEVG_, &
&    TOC_ENTRY%STEP_ID_, &
&    MATCH, &
& HOOKS )


  IF ( MATCH ) THEN

    SELECT CASE ( TOC_ENTRY%VAL_TYPE_ )
    CASE ( VALUES_SP_E )

      ! Read the values
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VAL_SP) READ_VAL_SP( TRIM(ADJUSTL(DUMP_PATH)), SKIP_VALUES, &
&            TOC_ENTRY%PROC_ID_, TOC_ENTRY%MSG_ID_, TOC_ENTRY%VAL_ADDR_, &
&            TOC_ENTRY%VAL_LB_, TOC_ENTRY%VAL_UB_, VALUES_SP, BIG_ENDIAN_READ, VERBOSE, HOOKS )

      ! Write data to the output manager
      PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_ATM_SP) OUTPUT_MANAGER%WRITE_ATM_SP( MESSAGE, VALUES_SP, HOOKS )

    CASE ( VALUES_DP_E )

      ! Rdad the values
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VAL_DP) READ_VAL_DP( TRIM(ADJUSTL(DUMP_PATH)), SKIP_VALUES, &
&            TOC_ENTRY%PROC_ID_, TOC_ENTRY%MSG_ID_, TOC_ENTRY%VAL_ADDR_, &
&            TOC_ENTRY%VAL_LB_, TOC_ENTRY%VAL_UB_, VALUES_DP, BIG_ENDIAN_READ, VERBOSE, HOOKS )

      ! Write data to the output manager
      PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_ATM_DP) OUTPUT_MANAGER%WRITE_ATM_DP( MESSAGE, VALUES_DP, HOOKS )

    CASE DEFAULT

      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_VAL_TYPE )

    END SELECT


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
    CASE(ERRFLAG_UNABLE_TO_CONVERT_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert representation' )
    CASE(ERRFLAG_UNABLE_TO_CONVERT_ILEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert ilevtype' )
    CASE(ERRFLAG_UNABLE_TO_CONVERT_CLEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert clevtype' )
    CASE(ERRFLAG_GET_DUMP_DIRECTORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get dump directory' )
    CASE(ERRFLAG_UNABLE_TO_READ_ATM_MESSAGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read atm message' )
    CASE(ERRFLAG_MATCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to match' )
    CASE(ERRFLAG_GET_SKIP_VALUES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get skip values' )
    CASE(ERRFLAG_UNABLE_TO_READ_VAL_SP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read val sp' )
    CASE(ERRFLAG_UNABLE_TO_WRITE_ATM_SP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write atm sp' )
    CASE(ERRFLAG_UNABLE_TO_READ_VAL_DP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read val dp' )
    CASE(ERRFLAG_UNABLE_TO_WRITE_ATM_DP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write atm dp' )
    CASE(ERRFLAG_UNKNOWN_VAL_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown val type' )
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

END FUNCTION ENCODE_ATM_MESSAGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_WAM_MESSAGE'
PP_THREAD_SAFE FUNCTION ENCODE_WAM_MESSAGE( CMDARGS, OUTPUT_MANAGER, TOC_ENTRY, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: MULTIOM_API, ONLY: TOC_WAM_FIELD_T
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: OM_WAM_MSG_T
  USE :: ENUMERATORS_MOD, ONLY: IREPRES2CREPRES
  USE :: ENUMERATORS_MOD, ONLY: IPREFIX2ILEVTYPE
  USE :: ENUMERATORS_MOD, ONLY: ILEVTYPE2CLEVTYPE
  USE :: ENUMERATORS_MOD, ONLY: VALUES_SP_E
  USE :: ENUMERATORS_MOD, ONLY: VALUES_DP_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T),             INTENT(IN)    :: CMDARGS
  CLASS(OUTPUT_MANAGER_BASE_A), POINTER, INTENT(INOUT) :: OUTPUT_MANAGER
  TYPE(TOC_WAM_FIELD_T),                 INTENT(IN)    :: TOC_ENTRY
  LOGICAL,                               INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                               INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(OM_WAM_MSG_T)   :: MESSAGE
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  INTEGER(KIND=JPIB_K) :: ILEVTYPE
  CHARACTER(LEN=128)   :: CREPRES
  CHARACTER(LEN=128)   :: CLEVTYPE
  CHARACTER(LEN=32)    :: CPARAM
  CHARACTER(LEN=32)    :: CSTEP
  CHARACTER(LEN=32)    :: CLEVEL
  CHARACTER(LEN=32)    :: CFREQ
  CHARACTER(LEN=32)    :: CANGLE
  CHARACTER(LEN=1024)  :: DUMP_PATH
  LOGICAL :: MATCH
  LOGICAL :: SKIP_VALUES
  REAL(KIND=REAL32), POINTER, DIMENSION(:) :: VALUES_SP
  REAL(KIND=REAL64), POINTER, DIMENSION(:) :: VALUES_DP

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_TO_STRING = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_REPRES = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_ILEVTYPE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_CLEVTYPE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_DUMP_DIRECTORY = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_WAM_MESSAGE = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_SKIP_VALUES = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_VAL_SP = 10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_WAM_SP = 11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_VAL_DP = 12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_WAM_DP = 13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_VAL_TYPE = 14_JPIB_K

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

  ! Read command line arguments
  PP_TRYCALL(ERRFLAG_GET_DUMP_DIRECTORY) CMDARGS%GET_DUMP_PATH( DUMP_PATH, HOOKS )
  PP_TRYCALL(ERRFLAG_GET_SKIP_VALUES) CMDARGS%GET_SKIP_VALUES( SKIP_VALUES, HOOKS )

  ! Read the next message to be processed
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_WAM_MESSAGE)   READ_WAM_MESSAGE( TRIM(ADJUSTL(DUMP_PATH)), &
&       TOC_ENTRY%PROC_ID_, TOC_ENTRY%MSG_ID_, TOC_ENTRY%MSG_ADDR_, &
&        MESSAGE, BIG_ENDIAN_READ, VERBOSE, HOOKS   )

  IF ( TOC_ENTRY%PARAM_ID_ .EQ. 140250 ) THEN
    ! Check if the message matches teh filters
    PP_TRYCALL(ERRFLAG_MATCH) CMDARGS%MATCH( &
&      TOC_ENTRY%PARAM_ID_, &
&      TOC_ENTRY%REPRES_ID_, &
&      TOC_ENTRY%PREFIX_ID_, &
&      MESSAGE%IFREQ, &
&      MESSAGE%IANGLE, &
&      TOC_ENTRY%STEP_ID_, &
&      MATCH, &
&      HOOKS )
  ELSE
    ! Check if the message matches teh filters
    PP_TRYCALL(ERRFLAG_MATCH) CMDARGS%MATCH( &
&      TOC_ENTRY%PARAM_ID_, &
&      TOC_ENTRY%REPRES_ID_, &
&      TOC_ENTRY%PREFIX_ID_, &
&      MESSAGE%KLEV, &
&      TOC_ENTRY%STEP_ID_, &
&      MATCH, &
&      HOOKS )
  ENDIF


  IF ( MATCH ) THEN

    ! Check that the message file exist
    SELECT CASE ( TOC_ENTRY%VAL_TYPE_ )
    CASE ( VALUES_SP_E )

      ! Read the values
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VAL_SP) READ_VAL_SP( TRIM(ADJUSTL(DUMP_PATH)), SKIP_VALUES, &
&          TOC_ENTRY%PROC_ID_, TOC_ENTRY%MSG_ID_, TOC_ENTRY%VAL_ADDR_, &
&          TOC_ENTRY%VAL_LB_, TOC_ENTRY%VAL_UB_, VALUES_SP, BIG_ENDIAN_READ, VERBOSE, HOOKS )

      ! Write data to the output manager
      PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_WAM_SP) OUTPUT_MANAGER%WRITE_WAM_SP( MESSAGE, VALUES_SP, HOOKS )

    CASE ( VALUES_DP_E )

      ! Rdad the values
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VAL_DP) READ_VAL_DP( TRIM(ADJUSTL(DUMP_PATH)), SKIP_VALUES, &
&          TOC_ENTRY%PROC_ID_, TOC_ENTRY%MSG_ID_, TOC_ENTRY%VAL_ADDR_, &
&          TOC_ENTRY%VAL_LB_, TOC_ENTRY%VAL_UB_, VALUES_DP, BIG_ENDIAN_READ, VERBOSE, HOOKS )

      ! Write data to the output manager
      PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_WAM_DP) OUTPUT_MANAGER%WRITE_WAM_DP( MESSAGE, VALUES_DP, HOOKS )

    CASE DEFAULT

      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_VAL_TYPE )

    END SELECT

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
    CASE(ERRFLAG_UNABLE_TO_CONVERT_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert representation' )
    CASE(ERRFLAG_UNABLE_TO_CONVERT_ILEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert ilevtype' )
    CASE(ERRFLAG_UNABLE_TO_CONVERT_CLEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert clevtype' )
    CASE(ERRFLAG_GET_DUMP_DIRECTORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get dump directory' )
    CASE(ERRFLAG_UNABLE_TO_READ_WAM_MESSAGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read wam message' )
    CASE(ERRFLAG_MATCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to match' )
    CASE(ERRFLAG_GET_SKIP_VALUES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get skip values' )
    CASE(ERRFLAG_UNABLE_TO_READ_VAL_SP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read val sp' )
    CASE(ERRFLAG_UNABLE_TO_WRITE_WAM_SP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write wam sp' )
    CASE(ERRFLAG_UNABLE_TO_READ_VAL_DP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read val dp' )
    CASE(ERRFLAG_UNABLE_TO_WRITE_WAM_DP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write wam dp' )
    CASE(ERRFLAG_UNKNOWN_VAL_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown val type' )
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

END FUNCTION ENCODE_WAM_MESSAGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_FLUSH'
PP_THREAD_SAFE FUNCTION ENCODE_FLUSH( CMDARGS, OUTPUT_MANAGER, TOC_ENTRY, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: MULTIOM_API, ONLY: TOC_FLUSH_STEP_T
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
  TYPE(COMMAND_LINE_ARGS_T),             INTENT(IN)    :: CMDARGS
  CLASS(OUTPUT_MANAGER_BASE_A), POINTER, INTENT(INOUT) :: OUTPUT_MANAGER
  TYPE(TOC_FLUSH_STEP_T),                INTENT(IN)    :: TOC_ENTRY
  LOGICAL,                               INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                               INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FLUSH_STEP = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FLUSH_STEP_AND_RESTART = 2_JPIB_K

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

  ! Send the flush or replace the flush with a step and restart flush
  IF ( CMDARGS%REPLACE_FLUSH_STEP_AND_RESTART ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FLUSH_STEP_AND_RESTART) OUTPUT_MANAGER%FLUSH_STEP_AND_TRIGGER_RESTART( TOC_ENTRY%STEP_, HOOKS )
  ELSE
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FLUSH_STEP) OUTPUT_MANAGER%FLUSH_STEP( TOC_ENTRY%STEP_, HOOKS )
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
    CASE(ERRFLAG_UNABLE_TO_FLUSH_STEP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to flush step' )
    CASE(ERRFLAG_UNABLE_TO_FLUSH_STEP_AND_RESTART)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to flush step and restart (overwriting flush step)' )
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

END FUNCTION ENCODE_FLUSH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_FLUSH_AND_RESTART'
PP_THREAD_SAFE FUNCTION ENCODE_FLUSH_AND_RESTART( CMDARGS, OUTPUT_MANAGER, TOC_ENTRY, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: MULTIOM_API, ONLY: TOC_FLUSH_STEP_RST_T
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
  TYPE(COMMAND_LINE_ARGS_T),             INTENT(IN)    :: CMDARGS
  CLASS(OUTPUT_MANAGER_BASE_A), POINTER, INTENT(INOUT) :: OUTPUT_MANAGER
  TYPE(TOC_FLUSH_STEP_RST_T),            INTENT(IN)    :: TOC_ENTRY
  LOGICAL,                               INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                               INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FLUSH_STEP_AND_RESTART = 1_JPIB_K

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

  ! Flush step and trigger restart
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FLUSH_STEP_AND_RESTART) OUTPUT_MANAGER%FLUSH_STEP_AND_TRIGGER_RESTART( TOC_ENTRY%STEP_, HOOKS )


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
    CASE(ERRFLAG_UNABLE_TO_FLUSH_STEP_AND_RESTART)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to flush step and restart' )
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

END FUNCTION ENCODE_FLUSH_AND_RESTART
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_FLUSH_LAST_STEP'
PP_THREAD_SAFE FUNCTION ENCODE_FLUSH_LAST_STEP( CMDARGS, OUTPUT_MANAGER, TOC_ENTRY, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: MULTIOM_API, ONLY: TOC_FLUSH_LAST_STEP_T
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
  TYPE(COMMAND_LINE_ARGS_T),             INTENT(IN)    :: CMDARGS
  CLASS(OUTPUT_MANAGER_BASE_A), POINTER, INTENT(INOUT) :: OUTPUT_MANAGER
  TYPE(TOC_FLUSH_LAST_STEP_T),           INTENT(IN)    :: TOC_ENTRY
  LOGICAL,                               INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                               INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LAST_STEP = 1_JPIB_K

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

  ! Flush last step
  IF ( .NOT. ( CMDARGS%SKIP_FLUSH_LAST_STEP ) ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LAST_STEP) OUTPUT_MANAGER%FLUSH_LAST_STEP( TOC_ENTRY%STEP_, HOOKS )
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
    CASE(ERRFLAG_UNABLE_TO_LAST_STEP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to last step' )
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

END FUNCTION ENCODE_FLUSH_LAST_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_SIMULATION_END'
PP_THREAD_SAFE FUNCTION ENCODE_SIMULATION_END( CMDARGS, OUTPUT_MANAGER, TOC_ENTRY, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: MULTIOM_API, ONLY: TOC_SIM_END_T
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
  TYPE(COMMAND_LINE_ARGS_T),             INTENT(IN)    :: CMDARGS
  CLASS(OUTPUT_MANAGER_BASE_A), POINTER, INTENT(INOUT) :: OUTPUT_MANAGER
  TYPE(TOC_SIM_END_T),                   INTENT(IN)    :: TOC_ENTRY
  LOGICAL,                               INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                               INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

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

END FUNCTION ENCODE_SIMULATION_END
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_FOOTER'
PP_THREAD_SAFE FUNCTION ENCODE_FOOTER( CMDARGS, OUTPUT_MANAGER, BIG_ENDIAN_READ, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: MULTIOM_API, ONLY: OUTPUT_MANAGER_BASE_A
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
  TYPE(COMMAND_LINE_ARGS_T),             INTENT(IN)    :: CMDARGS
  CLASS(OUTPUT_MANAGER_BASE_A), POINTER, INTENT(INOUT) :: OUTPUT_MANAGER
  LOGICAL,                               INTENT(IN)    :: BIG_ENDIAN_READ
  LOGICAL,                               INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

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

END FUNCTION ENCODE_FOOTER
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
PP_THREAD_SAFE FUNCTION READ_VAL_SP( DIRECTORY, SKIP_VAL, PROC_ID, MSG_ID, ADDR, &
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
  LOGICAL,                                  INTENT(IN)    :: SKIP_VAL
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

  IF ( .NOT. SKIP_VAL ) THEN
    ! Read the message
    LOC_DIR = REPEAT( ' ', 1024 )
    WRITE(LOC_DIR,'(A,A,I6.6,A)', IOSTAT=WRITE_STATUS) TRIM(DIRECTORY), '/io_serv.', PROC_ID, '.d'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

    PP_TRYCALL(ERRFLAG_UNABLE_TO_VAL_CREATE_NAME) VAL_CREATE_NAME( TRIM(LOC_DIR), MSG_ID, PROC_ID, VALFNAME, HOOKS )

    INQUIRE( FILE=TRIM(VALFNAME), EXIST=VALEX )
  ELSE
    VALEX = .FALSE.
  ENDIF

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
PP_THREAD_SAFE FUNCTION READ_VAL_DP( DIRECTORY, SKIP_VAL, PROC_ID, MSG_ID, ADDR, &
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
  LOGICAL,                                  INTENT(IN)    :: SKIP_VAL
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

  IF ( .NOT. SKIP_VAL ) THEN
    ! Read the message
    LOC_DIR = REPEAT( ' ', 1024 )
    WRITE(LOC_DIR,'(A,A,I6.6,A)', IOSTAT=WRITE_STATUS) TRIM(DIRECTORY), '/io_serv.', PROC_ID, '.d'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_TO_STRING )

    PP_TRYCALL(ERRFLAG_UNABLE_TO_VAL_CREATE_NAME) VAL_CREATE_NAME( TRIM(LOC_DIR), MSG_ID, PROC_ID, VALFNAME, HOOKS )

    INQUIRE( FILE=TRIM(VALFNAME), EXIST=VALEX )
  ELSE
    VALEX = .FALSE.
  ENDIF

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
