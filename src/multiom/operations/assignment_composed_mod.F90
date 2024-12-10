!>
!> @file assignment_composed_mod.F90
!>
!> @brief Module containing definitions and procedures for variableeter intops.
!>
!> This module defines the `ASSIGNMENT_COMPOSED_T` type, along with its associated
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


#define PP_FILE_NAME 'assignment_composed_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ASSIGNMENT_COMPOSED_MOD'
MODULE ASSIGNMENT_COMPOSED_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: ASSIGNMENT_BASE_MOD, ONLY: ASSIGNMENT_BASE_A
  USE :: ASSIGNMENT_BASE_MOD, ONLY: ASSIGNMENT_CONTAINER_T

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
TYPE, EXTENDS(ASSIGNMENT_BASE_A) :: ASSIGNMENT_COMPOSED_T

  !> Default visibility of the type.
  PRIVATE

  !> Decide the ecution criteria
  LOGICAL :: CHAINED_EXECUTION_ = -99_JPIB_K

  !> The nested assignemnts
  TYPE(ASSIGNMENT_CONTAINER_T), POINTER, DIMENSION(:) :: NESTED_ASSIGNMENTS_ => NULL()

CONTAINS

  !> @brief Initializes the intop variableeter type.
  !> @details This procedure sets up the `ASSIGNMENT_COMPOSED_T` type, initializing its components.
  !>
  !> @variable [in] this The instance of `ASSIGNMENT_COMPOSED_T` to initialize.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT  => ASSIGNMENT_COMPOSED_INIT

  !> @brief Matches a condition against the intop.
  !> @details This procedure checks whether a given condition matches the criteria defined by the intop.
  !>
  !> @variable [in] this The instance of `ASSIGNMENT_COMPOSED_T`.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: EVAL => ASSIGNMENT_COMPOSED_EVAL

  !> @brief Prints the intop's details for debugging or logging.
  !> @details Outputs the intop's configuration and current state.
  !>
  !> @variable [in] this The instance of `ASSIGNMENT_COMPOSED_T` to print.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT => ASSIGNMENT_COMPOSED_PRINT

  !> @brief Frees resources allocated for the intop.
  !> @details Cleans up the `ASSIGNMENT_COMPOSED_T` type, deallocating any resources used by the intop.
  !>
  !> @variable [in] this The instance of `ASSIGNMENT_COMPOSED_T` to free.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE  => ASSIGNMENT_COMPOSED_FREE



  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: ALLOCATE_NESTED_ASSIGNMENTS => ASSIGNMENT_COMPOSED_ALLOCATE_NESTED_ASSIGNMENTS
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_NESTED_ASSIGNMENTS =>ASSIGNMENT_COMPOSED_GET_NESTED_ASSIGNMENTS

END TYPE



!> Whitlist of public symbols
PUBLIC :: ASSIGNMENT_COMPOSED_T

CONTAINS


!>
!> @brief Initializes the `ASSIGNMENT_COMPOSED_T` intop from a YAML configuration.
!>
!> This function initializes the `ASSIGNMENT_COMPOSED_T` type based on the provided YAML
!> configuration (`CFG`) and applies any hooks specified in `HOOKS`. The function
!> reads the necessary variableeters from the configuration and sets up the intop structure.
!>
!> @variable [inout] THIS The intop object (`ASSIGNMENT_COMPOSED_T`) that will be initialized.
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
#define PP_PROCEDURE_NAME 'ASSIGNMENT_COMPOSED_INIT'
FUNCTION ASSIGNMENT_COMPOSED_INIT( THIS, CFG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_READ_LOGICAL

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ASSIGNMENT_COMPOSED_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_CHAINED

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CHAINED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHAINED_UNDEFINED = 2_JPIB_K

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

  !> Read the variable chained
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CHAINED) YAML_CONFIGURATION_HAS_KEY( CFG, 'chained', HAS_CHAINED, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_CHAINED, ERRFLAG_CHAINED_UNDEFINED )

  !> Read the variable chained
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CHAINED) YAML_READ_LOGICAL( CFG, 'chained', THIS%CHAINED_EXECUTION_, HOOKS )

  !> Initialize the nested array of assignments
  THIS%NESTED_ASSIGNMENTS_ => NULL()

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
    CASE (ERRFLAG_UNABLE_TO_READ_CHAINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read chained' )
    CASE (ERRFLAG_CHAINED_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'chained undefined' )
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

END FUNCTION ASSIGNMENT_COMPOSED_INIT
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
!> @variable [inout] THIS  The intop object (`ASSIGNMENT_COMPOSED_T`) used for matching.
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
#define PP_PROCEDURE_NAME 'ASSIGNMENT_COMPOSED_EVAL'
FUNCTION ASSIGNMENT_COMPOSED_EVAL( THIS, IN_MSG, IN_PAR, OUT_MSG, OUT_PAR, HOOKS ) RESULT(RET)

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
  CLASS(ASSIGNMENT_COMPOSED_T),         INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T), INTENT(IN)    :: IN_MSG
  TYPE(PARAMETRIZATION_T), INTENT(IN)    :: IN_PAR
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: OUT_MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: OUT_PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  TYPE(FORTRAN_MESSAGE_T) :: CURR_MSG
  TYPE(PARAMETRIZATION_T) :: CURR_PAR

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NESTED_ASSIGNMENT_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EVALUATE_NESTED_ASSIGNMENT=2_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%NESTED_ASSIGNMENTS_), ERRFLAG_NESTED_ASSIGNMENT_NOT_ASSOCIATED )

  !> Compose assignments
  IF ( THIS%CHAINED_EXECUTION_ ) THEN
    CURR_MSG = IN_MSG
    CURR_PAR = IN_PAR
    !> Evaluate the nested assignments
    DO I = 1, SIZE(THIS%NESTED_ASSIGNMENTS_)
      PP_TRYCALL(ERRFLAG_UNABLE_TO_EVALUATE_NESTED_ASSIGNMENT) THIS%NESTED_ASSIGNMENTS_(I)%ASSIGNMENT_%EVAL( CURR_MSG, CURR_PAR, OUT_MSG, OUT_PAR, HOOKS )
      CURR_MSG = OUT_MSG
      CURR_PAR = OUT_PAR
    END DO
  ELSE
    !> Evaluate the nested assignments
    DO I = 1, SIZE(THIS%NESTED_ASSIGNMENTS_)
      PP_TRYCALL(ERRFLAG_UNABLE_TO_EVALUATE_NESTED_ASSIGNMENT) THIS%NESTED_ASSIGNMENTS_(I)%ASSIGNMENT_%EVAL( IN_MSG, IN_PAR, OUT_MSG, OUT_PAR, HOOKS )
    END DO
  END IF

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
    CASE (ERRFLAG_NESTED_ASSIGNMENT_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'nested assignment not associated' )
    CASE (ERRFLAG_UNABLE_TO_EVALUATE_NESTED_ASSIGNMENT)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to evaluate nested assignment' )
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

END FUNCTION ASSIGNMENT_COMPOSED_EVAL
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
!> @variable [inout] THIS    The intop object (`ASSIGNMENT_COMPOSED_T`) whose details are to be printed.
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
#define PP_PROCEDURE_NAME 'ASSIGNMENT_COMPOSED_PRINT'
FUNCTION ASSIGNMENT_COMPOSED_PRINT( THIS, UNIT, OFFSET, HOOKS, SEPARATOR ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ASSIGNMENT_COMPOSED_T),    INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: SEPARATOR

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NESTED_PRINT = 2_JPIB_K

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

  !> Convert IDs to names
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%NESTED_ASSIGNMENTS_), ERRFLAG_NOT_ASSOCIATED )

  IF ( THIS%CHAINED_EXECUTION_ ) THEN
    WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'chained_nested_assignemnt( '
  ELSE
    WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'nested_assignemnt( '
  ENDIF

  IF ( SIZE(THIS%NESTED_ASSIGNMENTS_) .GT. 1) THEN
    DO I = 1, SIZE(THIS%NESTED_ASSIGNMENTS_)-1
      PP_TRYCALL(ERRFLAG_NESTED_PRINT) THIS%NESTED_ASSIGNMENTS_(I)%ASSIGNMENT_%PRINT( UNIT, OFFSET+2, HOOKS, ' , ...' )
    END DO
    PP_TRYCALL(ERRFLAG_NESTED_PRINT) THIS%NESTED_ASSIGNMENTS_(I)%ASSIGNMENT_%PRINT( UNIT, OFFSET+2, HOOKS )
  ENDIF

  !> Print the variable intop
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
    CASE (ERRFLAG_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'nested assignments are not associated' )
    CASE (ERRFLAG_NESTED_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to print nested assignments' )
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

END FUNCTION ASSIGNMENT_COMPOSED_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees the memory and resources associated with the intop.
!>
!> This function deallocates the memory and resources used by the intop object.
!> It ensures proper cleanup of nested structures and any associated hooks.
!> After this function is called, the intop object should no longer be used.
!>
!> @variable [inout] THIS   The intop object (`ASSIGNMENT_COMPOSED_T`) whose resources are to be freed.
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
#define PP_PROCEDURE_NAME 'ASSIGNMENT_COMPOSED_FREE'
FUNCTION ASSIGNMENT_COMPOSED_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(ASSIGNMENT_COMPOSED_T), INTENT(INOUT) :: THIS
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

END FUNCTION ASSIGNMENT_COMPOSED_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ASSIGNMENT_COMPOSED_ALLOCATE_NESTED_ASSIGNMENTS'
FUNCTION ASSIGNMENT_COMPOSED_ALLOCATE_NESTED_ASSIGNMENTS( THIS, N_ASSIGNMENTS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ASSIGNMENT_COMPOSED_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: N_ASSIGNMENTS
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: ALLOC_STATE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_SIZE = 3_JPIB_K

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

  !> Read the variable chained
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%NESTED_ASSIGNMENTS_), ERRFLAG_ALREADY_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( N_ASSIGNMENTS .LE. 0, ERRFLAG_WRONG_SIZE )

  !> Allocate the nested assignments
  ALLOCATE( THIS%NESTED_ASSIGNMENTS_(N_ASSIGNMENTS), STAT=ALLOC_STATE, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATE /= 0, ERRFLAG_UNABLE_TO_ALLOCATE )

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
    CASE (ERRFLAG_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'already associated' )
    CASE (ERRFLAG_WRONG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'allocation size lower than 0' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate' )
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

END FUNCTION ASSIGNMENT_COMPOSED_ALLOCATE_NESTED_ASSIGNMENTS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ASSIGNMENT_COMPOSED_GET_NESTED_ASSIGNMENTS'
FUNCTION ASSIGNMENT_COMPOSED_GET_NESTED_ASSIGNMENTS( THIS, NESTED_ASSIGNMENTS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: FILTER_BASE_MOD,   ONLY: FILTER_CONTAINER_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ASSIGNMENT_COMPOSED_T),                         INTENT(INOUT) :: THIS
   TYPE(ASSIGNMENT_CONTAINER_T), POINTER, DIMENSION(:), INTENT(OUT)   :: NESTED_ASSIGNMENTS
  TYPE(HOOKS_T),                                        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ASSOCIATED = 1_JPIB_K

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

  !> Read the variable chained
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%NESTED_ASSIGNMENTS_), ERRFLAG_NOT_ASSOCIATED )

  !> Associate the nested assignemtsn to the output
  NESTED_ASSIGNMENTS => THIS%NESTED_ASSIGNMENTS_

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
    CASE (ERRFLAG_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'not associated' )
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

END FUNCTION ASSIGNMENT_COMPOSED_GET_NESTED_ASSIGNMENTS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE ASSIGNMENT_COMPOSED_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME