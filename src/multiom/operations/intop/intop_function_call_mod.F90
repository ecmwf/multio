!>
!> @file intop_function_call_mod.F90
!>
!> @brief Module containing definitions and procedures for function_calleter operations.
!>
!> This module defines the `INTOP_FUNCTION_CALL_T` type, along with its associated
!> procedures and helper functions that facilitate the creation, management, and
!> utilization of function_calleter operations within the system. FUNCTION_CALLeter operations allow for
!> complex operationing operations by combining multiple nested operations.
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'intop_function_call_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'INTOP_FUNCTION_CALL_MOD'
MODULE INTOP_FUNCTION_CALL_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: INTOP_BASE_MOD,    ONLY: INTOP_BASE_A
  USE :: INTOP_BASE_MOD,    ONLY: INTOP_CONTAINER_T

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!>
!> This derived type extends `INTOP_BASE_A` and is used for operationing based on function_calleters.
!> It supports matching, ignoring, or applying thresholds to specific function_calleters, and can
!> also utilize a keyset to perform nested operationing.
!>
TYPE, EXTENDS(INTOP_BASE_A) :: INTOP_FUNCTION_CALL_T

  !> Default visibility of the type.
  PRIVATE

  !> @brief Type of the operation (e.g., function_call|sub|mul|div|pow|unary_neg|function_call).
  INTEGER(KIND=JPIB_K) :: FUNCTION_CALL_TYPE_=-99_JPIB_K
  INTEGER(KIND=JPIB_K) :: FUNCTION_CALL_NARGS_=-99_JPIB_K

  !> First operand
  TYPE(INTOP_CONTAINER_T), POINTER, DIMENSION(:) :: OPERANDS_ => NULL()
  INTEGER(KIND=JPIB_K), POINTER, DIMENSION(:) :: WORKSPACE_ => NULL()


CONTAINS

  !> @brief Initializes the operation function_calleter type.
  !> @details This procedure sets up the `INTOP_FUNCTION_CALL_T` type, initializing its components.
  !>
  !> @function_call [in] this The instance of `INTOP_FUNCTION_CALL_T` to initialize.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT => INTOP_FUNCTION_CALL_INIT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET  => INTOP_FUNCTION_CALL_GET_OPERANDS

  !> @brief Matches a condition against the operation.
  !> @details This procedure checks whether a given condition matches the criteria defined by the operation.
  !>
  !> @function_call [in] this The instance of `INTOP_FUNCTION_CALL_T`.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: EVAL => INTOP_FUNCTION_CALL_EVALUATE

  !> @brief Prints the operation's details for debugging or logging.
  !> @details Outputs the operation's configuration and current state.
  !>
  !> @function_call [in] this The instance of `INTOP_FUNCTION_CALL_T` to print.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT => INTOP_FUNCTION_CALL_PRINT

  !> @brief Frees resources allocated for the operation.
  !> @details Cleans up the `INTOP_FUNCTION_CALL_T` type, deallocating any resources used by the operation.
  !>
  !> @function_call [in] this The instance of `INTOP_FUNCTION_CALL_T` to free.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE  => INTOP_FUNCTION_CALL_FREE

END TYPE



!> Whitlist of public symbols
PUBLIC :: INTOP_FUNCTION_CALL_T

CONTAINS


!>
!> @brief Initializes the `INTOP_FUNCTION_CALL_T` operation from a YAML configuration.
!>
!> This function initializes the `INTOP_FUNCTION_CALL_T` type based on the provided YAML
!> configuration (`CFG`) and applies any hooks specified in `HOOKS`. The function
!> reads the necessary function_calleters from the configuration and sets up the operation structure.
!>
!> @function_call [inout] THIS The operation object (`INTOP_FUNCTION_CALL_T`) that will be initialized.
!> @function_call [in]    CFG  The YAML configuration object containing the operation settings.
!> @function_call [in]    OPT The generic options to be used to initialize the operation.
!> @function_call [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during initialization.
!>
!> @return Integer error code (`RET`) indicating the success or failure of the initialization.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_HAS_KEY
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_GET_SUBCONFIGURATIONS
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_GET_CONFIGURATIONS_SIZE
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_GET_CONFIGURATION_BY_ID
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_READ_STRING
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_READ_INTEGER
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_READ_INTEGER_KEYSET_WITH_RANGES
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_DELETE_CONFIGURATION
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_DELETE_CONFIGURATIONS
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INTOP_FUNCTION_CALL_INIT'
FUNCTION INTOP_FUNCTION_CALL_INIT( THIS, CFG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,       ONLY: CINTOPFUNCCALL2IINTOPFUNCCALL
  USE :: YAML_CORE_UTILS_MOD,   ONLY: FUN_C2I_IF
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_READ_INTEGER_WITH_FILTER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_FUNCTION_CALL_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  PROCEDURE(FUN_C2I_IF), POINTER :: P_CINTOPFUNCCALL2IINTOPFUNCCALL
  LOGICAL :: HAS_OPERATION
  INTEGER(KIND=JPIB_K) :: ALLOC_STATE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_OPERATION = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INTOP_UNDEFINED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_OPERANDS = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_WORKSPACE = 4_JPIB_K

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION ) YAML_CONFIGURATION_HAS_KEY( CFG, 'operation', HAS_OPERATION, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_OPERATION, ERRFLAG_INTOP_UNDEFINED )

  !> Read all the subconfigurations
  P_CINTOPFUNCCALL2IINTOPFUNCCALL => CINTOPFUNCCALL2IINTOPFUNCCALL
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION) YAML_READ_INTEGER_WITH_FILTER( CFG, 'operation', THIS%FUNCTION_CALL_TYPE_, P_CINTOPFUNCCALL2IINTOPFUNCCALL, HOOKS )

  !> Initialize the operands
  THIS%FUNCTION_CALL_NARGS_ = MOD(THIS%FUNCTION_CALL_TYPE_,1000)

  !> Allocate the operands
  ALLOCATE( THIS%OPERANDS_(THIS%FUNCTION_CALL_NARGS_), STAT=ALLOC_STATE, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATE .NE. 0, ERRFLAG_UNABLE_TO_ALLOC_OPERANDS )

  !> Allocate the operandworkspace
  ALLOCATE( THIS%WORKSPACE_(THIS%FUNCTION_CALL_NARGS_), STAT=ALLOC_STATE, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATE .NE. 0, ERRFLAG_UNABLE_TO_ALLOC_WORKSPACE )

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
    CASE (ERRFLAG_INTOP_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'undefined integer binary operation' )
    CASE (ERRFLAG_UNABLE_TO_READ_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read operation' )
    CASE (ERRFLAG_UNABLE_TO_ALLOC_OPERANDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate operands' )
    CASE (ERRFLAG_UNABLE_TO_ALLOC_WORKSPACE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate workspace' )
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

END FUNCTION INTOP_FUNCTION_CALL_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Matches a operation function_calleter with a message and function_calleter.
!>
!> This function checks whether the provided message (`MSG`) and function_calleter (`PAR`)
!> match the operation's criteria. If a match is found, the `MATCH` flag is set to `.TRUE.`;
!> otherwise, it is set to `.FALSE.`. Hooks can be applied during the matching process
!> to allow additional processing.
!>
!> @function_call [inout] THIS  The operation object (`INTOP_FUNCTION_CALL_T`) used for matching.
!> @function_call [in]    MSG   The message (`FORTRAN_MESSAGE_T`) that is checked against the operation.
!> @function_call [in]    PAR   The function_calleter object (`FUNCTION_CALLETRIZATION_T`) used in the matching process.
!> @function_call [out]   MATCH Logical flag indicating whether the message and function_calleter match the operation's criteria.
!> @function_call [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during matching.
!>
!> @return Integer error code (`RET`) indicating success or failure of the matching process.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] FUNCTION_CALLETRIZATION_MOD::FUNCTION_CALLETRIZATION_T
!> @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INTOP_FUNCTION_CALL_EVALUATE'
FUNCTION INTOP_FUNCTION_CALL_EVALUATE( THIS, MSG, PAR, RESULT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,     ONLY: INTOP_FUNCTION_CALL_ABS_E
  USE :: ENUMERATORS_MOD,     ONLY: INTOP_FUNCTION_CALL_SIGN_E
  USE :: ENUMERATORS_MOD,     ONLY: INTOP_FUNCTION_CALL_STEPL_E
  USE :: ENUMERATORS_MOD,     ONLY: INTOP_FUNCTION_CALL_STEPR_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_FUNCTION_CALL_T), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),  INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),  INTENT(IN)    :: PAR
  INTEGER(KIND=JPIB_K),     INTENT(OUT)   :: RESULT
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_UNDEFINED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_OPERATION = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_NESTED_EVAL = 5_JPIB_K

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

  !> Check if the operation has operands
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED( THIS%OPERANDS_ ), ERRFLAG_OPERATION_UNDEFINED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED( THIS%WORKSPACE_ ), ERRFLAG_OPERATION_UNDEFINED )

  !> Evaluate nested operations
  DO I = 1, THIS%FUNCTION_CALL_NARGS_
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_NESTED_EVAL) THIS%OPERANDS_(I)%OPERATION_%EVAL( MSG, PAR, THIS%WORKSPACE_(I), HOOKS )
  END DO

  !> Evaluate the nested operations
  SELECT CASE( THIS%FUNCTION_CALL_TYPE_ )

  CASE( INTOP_FUNCTION_CALL_ABS_E )

    RESULT = ABS( THIS%WORKSPACE_(1) )


  CASE( INTOP_FUNCTION_CALL_SIGN_E )

    RESULT = SIGN( 1_JPIB_K,THIS%WORKSPACE_(1) )


  CASE( INTOP_FUNCTION_CALL_STEPR_E )

    IF ( THIS%WORKSPACE_(1) .GE. 0) THEN
      RESULT = 1_JPIB_K
    ELSE
      RESULT = 0_JPIB_K
    END IF

  CASE( INTOP_FUNCTION_CALL_STEPL_E )

    IF ( THIS%WORKSPACE_(1) .GT. 0) THEN
      RESULT = 1_JPIB_K
    ELSE
      RESULT = 0_JPIB_K
    END IF

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_OPERATION )

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
    CASE (ERRFLAG_OPERATION_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'operation undefined' )
    CASE (ERRFLAG_UNKNOWN_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown operation' )
    CASE (ERRFLAG_UNABLE_TO_CALL_NESTED_EVAL)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call nested eval' )
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

END FUNCTION INTOP_FUNCTION_CALL_EVALUATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INTOP_FUNCTION_CALL_GET_OPERANDS'
FUNCTION INTOP_FUNCTION_CALL_GET_OPERANDS( THIS, OPERANDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: INTOP_BASE_MOD,    ONLY: INTOP_CONTAINER_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_FUNCTION_CALL_T),                   INTENT(INOUT) :: THIS
  TYPE(INTOP_CONTAINER_T), POINTER, DIMENSION(:), INTENT(INOUT) :: OPERANDS
  TYPE(HOOKS_T),                                  INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=16) :: INTOP_TYPE_STR

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_OPERAND = 1_JPIB_K

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

  ! Get the nested operands
  OPERANDS => THIS%OPERANDS_

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
    CASE (ERRFLAG_UNKNOWN_OPERAND)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown operand' )
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

END FUNCTION INTOP_FUNCTION_CALL_GET_OPERANDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Prints the operation's configuration and details to the specified output unit.
!>
!> This function prints the details of the operation to the output specified by the `UNIT`.
!> An `OFFSET` can be applied to format the output, and `HOOKS` can be used for additional
!> callbacks during the printing process. The printed output includes details about
!> the operation's current configuration.
!>
!> @function_call [inout] THIS    The operation object (`INTOP_FUNCTION_CALL_T`) whose details are to be printed.
!> @function_call [in]    UNIT    The output unit (file or console) where the operation's details will be printed.
!> @function_call [in]    OFFSET  The offset applied to the printed output for formatting purposes.
!> @function_call [inout] HOOKS   Structure (`HOOKS_T`) used for additional hooks or callbacks during printing.
!>
!> @return Integer error code (`RET`) indicating success or failure of the print operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!> @dependency [PROCEDURE] DESTROY_OPERATION (*to be implemented)
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INTOP_FUNCTION_CALL_PRINT'
FUNCTION INTOP_FUNCTION_CALL_PRINT( THIS, UNIT, OFFSET, HOOKS, SEPARATOR ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: IINTOPFUNCCALL2CINTOPFUNCCALL

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_FUNCTION_CALL_T),   INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: SEPARATOR

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=16) :: INTOP_TYPE_STR
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: I

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_OPERATION = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERAND_NOT_ASSOCIATED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WORKSPACE_NOT_ASSOCIATED = 3_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED( THIS%OPERANDS_ ), ERRFLAG_OPERAND_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED( THIS%WORKSPACE_ ), ERRFLAG_WORKSPACE_NOT_ASSOCIATED )

  !> Convert the operation type to string
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_OPERATION) IINTOPFUNCCALL2CINTOPFUNCCALL( THIS%FUNCTION_CALL_TYPE_, INTOP_TYPE_STR, HOOKS )

  !> Print the function_call operation
  N = SIZE(THIS%OPERANDS_)
  WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'function_call::'//TRIM(ADJUSTL(INTOP_TYPE_STR))//'('
  IF ( N .GT. 1 ) THEN
    DO I = 1, N-1
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_OPERATION) THIS%OPERANDS_(I)%OPERATION_%PRINT( UNIT, OFFSET+2, HOOKS, SEPARATOR=', ...' )
    END DO
  ENDIF
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_OPERATION) THIS%OPERANDS_(N)%OPERATION_%PRINT( UNIT, OFFSET+2, HOOKS )
  IF ( PRESENT(SEPARATOR) ) THEN
    WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//')'//TRIM(ADJUSTL(SEPARATOR))
  ELSE
    WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//')'
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
    CASE (ERRFLAG_OPERAND_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'operand not associated' )
    CASE (ERRFLAG_WORKSPACE_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'workspace not associated' )
    CASE (ERRFLAG_UNABLE_TO_CONVERT_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert operation' )
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

END FUNCTION INTOP_FUNCTION_CALL_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees the memory and resources associated with the operation.
!>
!> This function deallocates the memory and resources used by the operation object.
!> It ensures proper cleanup of nested structures and any associated hooks.
!> After this function is called, the operation object should no longer be used.
!>
!> @function_call [inout] THIS   The operation object (`INTOP_FUNCTION_CALL_T`) whose resources are to be freed.
!> @function_call [inout] HOOKS  Structure (`HOOKS_T`) used for additional hooks or callbacks during the deallocation process.
!>
!> @return Integer error code (`RET`) indicating success or failure of the free operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!> @dependency [PROCEDURE] DESTROY_OPERATION (*to be implemented)
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INTOP_FUNCTION_CALL_FREE'
FUNCTION INTOP_FUNCTION_CALL_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(INTOP_FUNCTION_CALL_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERANDS_NOT_ASSOCIATE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WORKSPACE_NOT_ASSOCIATED = 3_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED( THIS%OPERANDS_ ), ERRFLAG_OPERANDS_NOT_ASSOCIATE )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED( THIS%WORKSPACE_ ), ERRFLAG_WORKSPACE_NOT_ASSOCIATED )

  !> Deallocate the operands
  DEALLOCATE( THIS%OPERANDS_, STAT=DEALLOC_STATE, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATE .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )

  !> Deallocate the workspace
  DEALLOCATE( THIS%WORKSPACE_, STAT=DEALLOC_STATE, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATE .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )

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
    CASE (ERRFLAG_OPERANDS_NOT_ASSOCIATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'operands not associated' )
    CASE (ERRFLAG_WORKSPACE_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'workspace not associated' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate' )
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

END FUNCTION INTOP_FUNCTION_CALL_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE INTOP_FUNCTION_CALL_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME