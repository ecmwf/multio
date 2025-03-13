! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

#define PP_FILE_NAME 'multiom_cmd_args_prog.F90'
#define PP_SECTION_TYPE 'PROGRAM'
#define PP_SECTION_NAME 'MULTIOM_CMD_ARGS_PROG'
#define PP_PROCEDURE_TYPE 'PROGRAM'
#define PP_PROCEDURE_NAME 'MULTIOM_CMD_ARGS_PROG'
PROGRAM MULTIOM_CMD_ARGS_PROG

  ! Symbolds imported from intrinsic modules
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: COMMAND_ARGUMENTS_PARSER_MOD, ONLY: COMMAND_LINE_ARGS_T

IMPLICIT NONE

  ! Local variables
  TYPE(HOOKS_T) :: HOOKS
  TYPE(COMMAND_LINE_ARGS_T) :: CMDARGS

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INIT_CMDARGS = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_CMDARGS = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PRINT_CMDARGS = 3_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialize the hooks
  CALL HOOKS%DEBUG_HOOK_%INIT( )

  ! Initialize the command arguments tokenizer
  PP_TRYCALL(ERRFLAG_INIT_CMDARGS) CMDARGS%INIT( HOOKS )

  ! Print tokenized command line arguments
  PP_TRYCALL(ERRFLAG_PRINT_CMDARGS) CMDARGS%PRINT( 6_JPIB_K, HOOKS )

  ! Initialize the command arguments tokenizer
  PP_TRYCALL(ERRFLAG_FREE_CMDARGS) CMDARGS%FREE( HOOKS )

  !> Be sure we don't have any memory leaks
  CALL HOOKS%DEBUG_HOOK_%FREE( )

  !> Exit point (on success)
  STOP 0

! Error handler
PP_ERROR_HANDLER

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_INIT_CMDARGS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error initializing the command arguments tokenizer' )
    CASE (ERRFLAG_FREE_CMDARGS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error freeing the command arguments tokenizer' )
    CASE (ERRFLAG_PRINT_CMDARGS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error printing command line arguments' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Print the error stack
    CALL HOOKS%DEBUG_HOOK_%PRINT_ERROR_STACK( 6_JPIB_K )

    ! Free the error stack
    CALL HOOKS%DEBUG_HOOK_%FREE( )

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  STOP 1

END PROGRAM MULTIOM_CMD_ARGS_PROG
#undef PP_FILE_NAME
#undef PP_SECTION_TYPE
#undef PP_SECTION_NAME
#undef PP_PROCEDURE_TYPE
#undef PP_PROCEDURE_NAME