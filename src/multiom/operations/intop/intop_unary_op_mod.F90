!>
!> @file intop_unary_op_mod.F90
!>
!> @brief Module containing definitions and procedures for unary_opeter operations.
!>
!> This module defines the `INTOP_UNARY_OP_T` type, along with its associated
!> procedures and helper functions that facilitate the creation, management, and
!> utilization of unary_opeter operations within the system. UNARY_OPeter operations allow for
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


#define PP_FILE_NAME 'intop_unary_op_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'INTOP_UNARY_OP_MOD'
MODULE INTOP_UNARY_OP_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: INTOP_BASE_MOD,    ONLY: INTOP_BASE_A

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!>
!> @brief A type representing a operation with support for nested operations and matching logic.
!>
!> This derived type extends `INTOP_BASE_A` and is used for operationing based on unary_opeters.
!> It supports matching, ignoring, or applying thresholds to specific unary_opeters, and can
!> also utilize a keyset to perform nested operationing.
!>
TYPE, EXTENDS(INTOP_BASE_A) :: INTOP_UNARY_OP_T

  !> Default visibility of the type.
  PRIVATE

  !> @brief Type of the operation (e.g., unary_op|sub|mul|div|pow|unary_neg|function_call).
  INTEGER(KIND=JPIB_K) :: UNARY_OP_TYPE_=-99_JPIB_K

  !> First operand
  CLASS(INTOP_BASE_A), POINTER :: OPERAND_ => NULL()

CONTAINS

  !> @brief Initializes the operation unary_opeter type.
  !> @details This procedure sets up the `INTOP_UNARY_OP_T` type, initializing its components.
  !>
  !> @unary_op [in] this The instance of `INTOP_UNARY_OP_T` to initialize.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT  => INTOP_UNARY_OP_INIT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET   => INTOP_UNARY_OP_SET_OPERAND
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET   => INTOP_UNARY_OP_GET_OPERAND

  !> @brief Matches a condition against the operation.
  !> @details This procedure checks whether a given condition matches the criteria defined by the operation.
  !>
  !> @unary_op [in] this The instance of `INTOP_UNARY_OP_T`.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: EVAL => INTOP_UNARY_OP_EVALUATE

  !> @brief Prints the operation's details for debugging or logging.
  !> @details Outputs the operation's configuration and current state.
  !>
  !> @unary_op [in] this The instance of `INTOP_UNARY_OP_T` to print.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT => INTOP_UNARY_OP_PRINT

  !> @brief Frees resources allocated for the operation.
  !> @details Cleans up the `INTOP_UNARY_OP_T` type, deallocating any resources used by the operation.
  !>
  !> @unary_op [in] this The instance of `INTOP_UNARY_OP_T` to free.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE  => INTOP_UNARY_OP_FREE

END TYPE



!> Whitlist of public symbols
PUBLIC :: INTOP_UNARY_OP_T

CONTAINS


!>
!> @brief Initializes the `INTOP_UNARY_OP_T` operation from a YAML configuration.
!>
!> This function initializes the `INTOP_UNARY_OP_T` type based on the provided YAML
!> configuration (`CFG`) and applies any hooks specified in `HOOKS`. The function
!> reads the necessary unary_opeters from the configuration and sets up the operation structure.
!>
!> @unary_op [inout] THIS The operation object (`INTOP_UNARY_OP_T`) that will be initialized.
!> @unary_op [in]    CFG  The YAML configuration object containing the operation settings.
!> @unary_op [in]    OPT The generic options to be used to initialize the operation.
!> @unary_op [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during initialization.
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
#define PP_PROCEDURE_NAME 'INTOP_UNARY_OP_INIT'
FUNCTION INTOP_UNARY_OP_INIT( THIS, CFG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,       ONLY: CINTOPUNARY2IINTOPUNARY
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
  CLASS(INTOP_UNARY_OP_T),   INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  PROCEDURE(FUN_C2I_IF), POINTER :: P_CINTOPUNARY2IINTOPUNARY
  LOGICAL :: HAS_OPERATION

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_OPERATION = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INTOP_UNDEFINED = 2_JPIB_K

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
  P_CINTOPUNARY2IINTOPUNARY => CINTOPUNARY2IINTOPUNARY
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION) YAML_READ_INTEGER_WITH_FILTER( CFG, 'operation', THIS%UNARY_OP_TYPE_, P_CINTOPUNARY2IINTOPUNARY, HOOKS )

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'undefined integer unary operation' )
    CASE (ERRFLAG_UNABLE_TO_READ_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read operation' )
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

END FUNCTION INTOP_UNARY_OP_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Matches a operation unary_opeter with a message and unary_opeter.
!>
!> This function checks whether the provided message (`MSG`) and unary_opeter (`PAR`)
!> match the operation's criteria. If a match is found, the `MATCH` flag is set to `.TRUE.`;
!> otherwise, it is set to `.FALSE.`. Hooks can be applied during the matching process
!> to allow additional processing.
!>
!> @unary_op [inout] THIS  The operation object (`INTOP_UNARY_OP_T`) used for matching.
!> @unary_op [in]    MSG   The message (`FORTRAN_MESSAGE_T`) that is checked against the operation.
!> @unary_op [in]    PAR   The unary_opeter object (`UNARY_OPETRIZATION_T`) used in the matching process.
!> @unary_op [out]   MATCH Logical flag indicating whether the message and unary_opeter match the operation's criteria.
!> @unary_op [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during matching.
!>
!> @return Integer error code (`RET`) indicating success or failure of the matching process.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] UNARY_OPETRIZATION_MOD::UNARY_OPETRIZATION_T
!> @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INTOP_UNARY_OP_EVALUATE'
FUNCTION INTOP_UNARY_OP_EVALUATE( THIS, MSG, PAR, RESULT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,     ONLY: INTOP_UNARY_NEG_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_UNARY_OP_T), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),  INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),  INTENT(IN)    :: PAR
  INTEGER(KIND=JPIB_K),     INTENT(OUT)   :: RESULT
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: OP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION1_UNDEFINED = 1_JPIB_K
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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED( THIS%OPERAND_ ), ERRFLAG_OPERATION1_UNDEFINED )

  !> Evaluate nested operations
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_NESTED_EVAL) THIS%OPERAND_%EVAL( MSG, PAR, OP, HOOKS )

  !> Evaluate the nested operations
  SELECT CASE( THIS%UNARY_OP_TYPE_ )

  CASE( INTOP_UNARY_NEG_E )

    RESULT = -1 * OP

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
    CASE (ERRFLAG_OPERATION1_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'operation 1 undefined' )
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
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION INTOP_UNARY_OP_EVALUATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INTOP_UNARY_OP_SET_OPERAND'
FUNCTION INTOP_UNARY_OP_SET_OPERAND( THIS, OPERAND, HOOKS ) RESULT(RET)

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
  CLASS(INTOP_UNARY_OP_T),      INTENT(INOUT) :: THIS
  CLASS(INTOP_BASE_A), POINTER, INTENT(INOUT) :: OPERAND
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=16) :: INTOP_TYPE_STR

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

  ! Set the nested operand
  THIS%OPERAND_ => OPERAND

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

END FUNCTION INTOP_UNARY_OP_SET_OPERAND
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INTOP_UNARY_OP_GET_OPERAND'
FUNCTION INTOP_UNARY_OP_GET_OPERAND( THIS, OPERAND, HOOKS ) RESULT(RET)

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
  CLASS(INTOP_UNARY_OP_T),     INTENT(INOUT) :: THIS
  CLASS(INTOP_BASE_A), POINTER, INTENT(INOUT) :: OPERAND
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=16) :: INTOP_TYPE_STR

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

  ! Get the nested operand
  OPERAND => THIS%OPERAND_
  THIS%OPERAND_ => NULL()

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

END FUNCTION INTOP_UNARY_OP_GET_OPERAND
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
!> @unary_op [inout] THIS    The operation object (`INTOP_UNARY_OP_T`) whose details are to be printed.
!> @unary_op [in]    UNIT    The output unit (file or console) where the operation's details will be printed.
!> @unary_op [in]    OFFSET  The offset applied to the printed output for formatting purposes.
!> @unary_op [inout] HOOKS   Structure (`HOOKS_T`) used for additional hooks or callbacks during printing.
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
#define PP_PROCEDURE_NAME 'INTOP_UNARY_OP_PRINT'
FUNCTION INTOP_UNARY_OP_PRINT( THIS, UNIT, OFFSET, HOOKS, SEPARATOR ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: IINTOPUNARY2CINTOPUNARY

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_UNARY_OP_T),   INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: SEPARATOR

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=16) :: INTOP_TYPE_STR

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_OPERATION = 1_JPIB_K

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

  !> Convert the operation type to string
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_OPERATION) IINTOPUNARY2CINTOPUNARY( THIS%UNARY_OP_TYPE_, INTOP_TYPE_STR, HOOKS )

  !> Print the unary_op operation
  WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'unary_op::'//TRIM(ADJUSTL(INTOP_TYPE_STR))//'('
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_OPERATION) THIS%OPERAND_%PRINT( UNIT, OFFSET+2, HOOKS )
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
    CASE (ERRFLAG_UNABLE_TO_CONVERT_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert operation' )
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

END FUNCTION INTOP_UNARY_OP_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees the memory and resources associated with the operation.
!>
!> This function deallocates the memory and resources used by the operation object.
!> It ensures proper cleanup of nested structures and any associated hooks.
!> After this function is called, the operation object should no longer be used.
!>
!> @unary_op [inout] THIS   The operation object (`INTOP_UNARY_OP_T`) whose resources are to be freed.
!> @unary_op [inout] HOOKS  Structure (`HOOKS_T`) used for additional hooks or callbacks during the deallocation process.
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
#define PP_PROCEDURE_NAME 'INTOP_UNARY_OP_FREE'
FUNCTION INTOP_UNARY_OP_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(INTOP_UNARY_OP_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

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

END FUNCTION INTOP_UNARY_OP_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE INTOP_UNARY_OP_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME