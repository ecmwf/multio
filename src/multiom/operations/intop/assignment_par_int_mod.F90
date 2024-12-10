!>
!> @file assignment_par_int_mod.F90
!>
!> @brief Module containing definitions and procedures for variableeter intops.
!>
!> This module defines the `ASSIGNMENT_PAR_INT_T` type, along with its associated
!> procedures and helper functions that facilitate the creation, management, and
!> utilization of variableeter intops within the system. VARIABLEeter intops allow for
!> complex intoping intops by combining multiple nested intops.
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'assignment_par_int_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ASSIGNMENT_PAR_INT_MOD'
MODULE ASSIGNMENT_PAR_INT_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: ASSIGNMENT_BASE_MOD, ONLY: ASSIGNMENT_BASE_A
  USE :: INTOP_BASE_MOD,      ONLY: INTOP_BASE_A

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!>
!> @brief A type representing a intop with support for nested intops and matching logic.
!>
!> This derived type extends `INTOP_BASE_A` and is used for intoping based on variableeters.
!> It supports matching, ignoring, or applying thresholds to specific variableeters, and can
!> also utilize a keyset to perform nested intoping.
!>
TYPE, EXTENDS(ASSIGNMENT_BASE_A) :: ASSIGNMENT_PAR_INT_T

  !> Default visibility of the type.
  PRIVATE

  !> Variable to store
  INTEGER(KIND=JPIB_K) :: VARIABLE_ID_ = -99_JPIB_K

  CLASS(INTOP_BASE_A), POINTER :: INTOP_ => NULL()

CONTAINS

  !> @brief Initializes the intop variableeter type.
  !> @details This procedure sets up the `ASSIGNMENT_PAR_INT_T` type, initializing its components.
  !>
  !> @variable [in] this The instance of `ASSIGNMENT_PAR_INT_T` to initialize.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT  => ASSIGNMENT_PAR_INT_INIT

  !> @brief Matches a condition against the intop.
  !> @details This procedure checks whether a given condition matches the criteria defined by the intop.
  !>
  !> @variable [in] this The instance of `ASSIGNMENT_PAR_INT_T`.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: EVAL => ASSIGNMENT_PAR_INT_EVAL

  !> @brief Prints the intop's details for debugging or logging.
  !> @details Outputs the intop's configuration and current state.
  !>
  !> @variable [in] this The instance of `ASSIGNMENT_PAR_INT_T` to print.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT => ASSIGNMENT_PAR_INT_PRINT

  !> @brief Frees resources allocated for the intop.
  !> @details Cleans up the `ASSIGNMENT_PAR_INT_T` type, deallocating any resources used by the intop.
  !>
  !> @variable [in] this The instance of `ASSIGNMENT_PAR_INT_T` to free.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE  => ASSIGNMENT_PAR_INT_FREE

END TYPE



!> Whitlist of public symbols
PUBLIC :: ASSIGNMENT_PAR_INT_T

CONTAINS


!>
!> @brief Initializes the `ASSIGNMENT_PAR_INT_T` intop from a YAML configuration.
!>
!> This function initializes the `ASSIGNMENT_PAR_INT_T` type based on the provided YAML
!> configuration (`CFG`) and applies any hooks specified in `HOOKS`. The function
!> reads the necessary variableeters from the configuration and sets up the intop structure.
!>
!> @variable [inout] THIS The intop object (`ASSIGNMENT_PAR_INT_T`) that will be initialized.
!> @variable [in]    CFG  The YAML configuration object containing the intop settings.
!> @variable [in]    OPT The generic options to be used to initialize the intop.
!> @variable [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during initialization.
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
#define PP_PROCEDURE_NAME 'ASSIGNMENT_PAR_INT_INIT'
FUNCTION ASSIGNMENT_PAR_INT_INIT( THIS, CFG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,             ONLY: FUN_C2I_IF
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_READ_INTEGER_WITH_FILTER
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_READ_STRING
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_DELETE_CONFIGURATION
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: CPARINTFLDS2IPARINTFLDS
  USE :: INTOP_FACTORY_MOD,               ONLY: MAKE_INTOP

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ASSIGNMENT_PAR_INT_T),            INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(YAML_CONFIGURATION_T) :: EXPRESSION_CONFIGURATION
  PROCEDURE(FUN_C2I_IF), POINTER :: P_CPARINTFLDS2IPARINTFLDS
  LOGICAL :: HAS_NAME
  LOGICAL :: HAS_TYPE
  LOGICAL :: HAS_EXPRESSION
  CHARACTER(LEN=:), ALLOCATABLE :: OP_TYPE
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG


  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_INTOP=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TARGET_UNDEFINED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NAME_UNDEFINED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SOURCE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXPRESSION_UNDEFINED=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAKE_INTOP=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INTOP_DEALLOCATION_ERROR=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXPRESSION_TYPE_UNDEFINED=13_JPIB_K

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

  !> Read the variable source
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_INTOP ) YAML_CONFIGURATION_HAS_KEY( CFG, 'name', HAS_NAME, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_NAME, ERRFLAG_NAME_UNDEFINED )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_INTOP ) YAML_CONFIGURATION_HAS_KEY( CFG, 'expression', HAS_EXPRESSION, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_EXPRESSION, ERRFLAG_EXPRESSION_UNDEFINED )

  !> Read the variable name
  P_CPARINTFLDS2IPARINTFLDS => CPARINTFLDS2IPARINTFLDS
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SOURCE) YAML_READ_INTEGER_WITH_FILTER( CFG, 'name', THIS%VARIABLE_ID_, P_CPARINTFLDS2IPARINTFLDS, HOOKS )

  !> Read the expression
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, 'expression', EXPRESSION_CONFIGURATION, HOOKS )

  !> Read the type of the first operation in the expression
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_INTOP ) YAML_CONFIGURATION_HAS_KEY( EXPRESSION_CONFIGURATION, 'type', HAS_TYPE, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_TYPE, ERRFLAG_EXPRESSION_TYPE_UNDEFINED )

  !> Read the first operation in the expression
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_INTOP ) YAML_READ_STRING( EXPRESSION_CONFIGURATION, 'type', OP_TYPE, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(OP_TYPE), ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ )

  !> Initialize the intop
  PP_TRYCALL(ERRFLAG_MAKE_INTOP) MAKE_INTOP( THIS%INTOP_, OP_TYPE, EXPRESSION_CONFIGURATION, HOOKS )

  !> Deallocate the operation type
  DEALLOCATE(OP_TYPE, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE )

  !> Destroy the configuration object
  PP_TRYCALL( ERRFLAG_INTOP_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( EXPRESSION_CONFIGURATION, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_READ_INTOP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read intop' )
    CASE (ERRFLAG_TARGET_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'source undefined' )
    CASE (ERRFLAG_NAME_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'name undefined' )
    CASE (ERRFLAG_UNABLE_TO_READ_SOURCE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read operation' )
    CASE (ERRFLAG_EXPRESSION_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'expression undefined' )
    CASE (ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfiguration' )
    CASE (ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'type not allocated after read' )
    CASE (ERRFLAG_MAKE_INTOP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make intop' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate type' )
    CASE (ERRFLAG_INTOP_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate the intop' )
    CASE (ERRFLAG_EXPRESSION_TYPE_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'expression type undefined' )
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

END FUNCTION ASSIGNMENT_PAR_INT_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Matches a intop variableeter with a message and variableeter.
!>
!> This function checks whether the provided message (`MSG`) and variableeter (`PAR`)
!> match the intop's criteria. If a match is found, the `MATCH` flag is set to `.TRUE.`;
!> otherwise, it is set to `.FALSE.`. Hooks can be applied during the matching process
!> to allow additional processing.
!>
!> @variable [inout] THIS  The intop object (`ASSIGNMENT_PAR_INT_T`) used for matching.
!> @variable [in]    MSG   The message (`FORTRAN_MESSAGE_T`) that is checked against the intop.
!> @variable [in]    PAR   The variableeter object (`VARIABLEETRIZATION_T`) used in the matching process.
!> @variable [out]   MATCH Logical flag indicating whether the message and variableeter match the intop's criteria.
!> @variable [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during matching.
!>
!> @return Integer error code (`RET`) indicating success or failure of the matching process.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] VARIABLEETRIZATION_MOD::VARIABLEETRIZATION_T
!> @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ASSIGNMENT_PAR_INT_EVAL'
FUNCTION ASSIGNMENT_PAR_INT_EVAL( THIS, IN_MSG, IN_PAR, OUT_MSG, OUT_PAR, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ASSIGNMENT_PAR_INT_T),         INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T), INTENT(IN)    :: IN_MSG
  TYPE(PARAMETRIZATION_T), INTENT(IN)    :: IN_PAR
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: OUT_MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: OUT_PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: RESULT

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EVALUATE_EXPRESSION=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_STORE_RESULT=2_JPIB_K

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

  ! Compute the result
  PP_TRYCALL(ERRFLAG_UNABLE_TO_EVALUATE_EXPRESSION) THIS%INTOP_%EVAL( IN_MSG, IN_PAR, RESULT, HOOKS )

  ! Pu the result in the outut message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_STORE_RESULT) OUT_PAR%SET( THIS%VARIABLE_ID_, RESULT, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_EVALUATE_EXPRESSION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to evaluate expression' )
    CASE (ERRFLAG_UNABLE_TO_STORE_RESULT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to store result' )
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

END FUNCTION ASSIGNMENT_PAR_INT_EVAL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Prints the intop's configuration and details to the specified output unit.
!>
!> This function prints the details of the intop to the output specified by the `UNIT`.
!> An `OFFSET` can be applied to format the output, and `HOOKS` can be used for additional
!> callbacks during the printing process. The printed output includes details about
!> the intop's current configuration.
!>
!> @variable [inout] THIS    The intop object (`ASSIGNMENT_PAR_INT_T`) whose details are to be printed.
!> @variable [in]    UNIT    The output unit (file or console) where the intop's details will be printed.
!> @variable [in]    OFFSET  The offset applied to the printed output for formatting purposes.
!> @variable [inout] HOOKS   Structure (`HOOKS_T`) used for additional hooks or callbacks during printing.
!>
!> @return Integer error code (`RET`) indicating success or failure of the print intop.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!> @dependency [PROCEDURE] DESTROY_INTOP (*to be implemented)
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ASSIGNMENT_PAR_INT_PRINT'
FUNCTION ASSIGNMENT_PAR_INT_PRINT( THIS, UNIT, OFFSET, HOOKS, SEPARATOR ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: IPARINTFLDS2CPARINTFLDS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ASSIGNMENT_PAR_INT_T),    INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: SEPARATOR

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=64) :: VARIABLE_NAME

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_ENUM_TO_STRING = 1_JPIB_K

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

  !> Initialize local variables
  VARIABLE_NAME = REPEAT(' ',64)

  !> Convert IDs to names
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_ENUM_TO_STRING) IPARINTFLDS2CPARINTFLDS( THIS%VARIABLE_ID_, VARIABLE_NAME, HOOKS )

  !> Print the variable intop
  IF ( PRESENT(SEPARATOR) ) THEN
    WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'assign to :: '//'par:'//TRIM(ADJUSTL(VARIABLE_NAME))//TRIM(ADJUSTL(SEPARATOR))
  ELSE
    WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'assign to :: '//'par:'//TRIM(ADJUSTL(VARIABLE_NAME))
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
    CASE (ERRFLAG_UNABLE_TO_CONVERT_ENUM_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert enum to string' )
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

END FUNCTION ASSIGNMENT_PAR_INT_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees the memory and resources associated with the intop.
!>
!> This function deallocates the memory and resources used by the intop object.
!> It ensures proper cleanup of nested structures and any associated hooks.
!> After this function is called, the intop object should no longer be used.
!>
!> @variable [inout] THIS   The intop object (`ASSIGNMENT_PAR_INT_T`) whose resources are to be freed.
!> @variable [inout] HOOKS  Structure (`HOOKS_T`) used for additional hooks or callbacks during the deallocation process.
!>
!> @return Integer error code (`RET`) indicating success or failure of the free intop.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!> @dependency [PROCEDURE] DESTROY_INTOP (*to be implemented)
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ASSIGNMENT_PAR_INT_FREE'
FUNCTION ASSIGNMENT_PAR_INT_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(ASSIGNMENT_PAR_INT_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

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

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION ASSIGNMENT_PAR_INT_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE ASSIGNMENT_PAR_INT_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME