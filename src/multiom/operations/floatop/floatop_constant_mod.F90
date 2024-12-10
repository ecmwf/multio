!>
!> @file floatop_constant_mod.F90
!>
!> @brief Module containing definitions and procedures for constanteter operations.
!>
!> This module defines the `FLOATOP_CONSTANT_T` type, along with its associated
!> procedures and helper functions that facilitate the creation, management, and
!> utilization of constanteter operations within the system. CONSTANTeter operations allow for
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


#define PP_FILE_NAME 'floatop_constant_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'FLOATOP_CONSTANT_MOD'
MODULE FLOATOP_CONSTANT_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: FLOATOP_BASE_MOD,    ONLY: FLOATOP_BASE_A

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!>
!> @brief A type representing a operation with support for nested operations and matching logic.
!>
!> This derived type extends `FLOATOP_BASE_A` and is used for operationing based on constanteters.
!> It supports matching, ignoring, or applying thresholds to specific constanteters, and can
!> also utilize a keyset to perform nested operationing.
!>
TYPE, EXTENDS(FLOATOP_BASE_A) :: FLOATOP_CONSTANT_T

  !> Default visibility of the type.
  PRIVATE

  !> @brief Type of the operation (e.g., constant|sub|mul|div|pow|unary_neg|function_call).
  INTEGER(KIND=JPIB_K) :: CONSTANT_=-99_JPIB_K

CONTAINS

  !> @brief Initializes the operation constanteter type.
  !> @details This procedure sets up the `FLOATOP_CONSTANT_T` type, initializing its components.
  !>
  !> @constant [in] this The instance of `FLOATOP_CONSTANT_T` to initialize.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT  => FLOATOP_CONSTANT_INIT

  !> @brief Matches a condition against the operation.
  !> @details This procedure checks whether a given condition matches the criteria defined by the operation.
  !>
  !> @constant [in] this The instance of `FLOATOP_CONSTANT_T`.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: EVAL => FLOATOP_CONSTANT_EVAL

  !> @brief Prints the operation's details for debugging or logging.
  !> @details Outputs the operation's configuration and current state.
  !>
  !> @constant [in] this The instance of `FLOATOP_CONSTANT_T` to print.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT => FLOATOP_CONSTANT_PRINT

  !> @brief Frees resources allocated for the operation.
  !> @details Cleans up the `FLOATOP_CONSTANT_T` type, deallocating any resources used by the operation.
  !>
  !> @constant [in] this The instance of `FLOATOP_CONSTANT_T` to free.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE  => FLOATOP_CONSTANT_FREE

END TYPE



!> Whitlist of public symbols
PUBLIC :: FLOATOP_CONSTANT_T

CONTAINS


!>
!> @brief Initializes the `FLOATOP_CONSTANT_T` operation from a YAML configuration.
!>
!> This function initializes the `FLOATOP_CONSTANT_T` type based on the provided YAML
!> configuration (`CFG`) and applies any hooks specified in `HOOKS`. The function
!> reads the necessary constanteters from the configuration and sets up the operation structure.
!>
!> @constant [inout] THIS The operation object (`FLOATOP_CONSTANT_T`) that will be initialized.
!> @constant [in]    CFG  The YAML configuration object containing the operation settings.
!> @constant [in]    OPT The generic options to be used to initialize the operation.
!> @constant [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during initialization.
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
#define PP_PROCEDURE_NAME 'FLOATOP_CONSTANT_INIT'
FUNCTION FLOATOP_CONSTANT_INIT( THIS, CFG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_READ_STRING_WITH_ENV_EXPANSION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FLOATOP_CONSTANT_T),  INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_VALUE
  LOGICAL :: IS_INTEGER
  CHARACTER(LEN=:), ALLOCATABLE :: CTMP
  INTEGER(KIND=JPIB_K) :: READF_STATE
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_OPERATION = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_UNDEFINED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_INTEGER = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FLOAT = 7_JPIB_K

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

  !> Read the constant
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION ) YAML_CONFIGURATION_HAS_KEY( CFG, 'value', HAS_VALUE, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_VALUE, ERRFLAG_VALUE_UNDEFINED )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION) YAML_READ_STRING_WITH_ENV_EXPANSION( CFG, 'value', CTMP, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CTMP), ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ )
  READ(CTMP,*,IOSTAT=READF_STATE) THIS%CONSTANT_
  PP_DEBUG_CRITICAL_COND_THROW( READF_STATE .NE. 0, ERRFLAG_UNABLE_TO_READ_FLOAT )
  DEALLOCATE(CTMP, STAT=DEALLOC_STATE, ERRMSG=ERRMSG)
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
    CASE (ERRFLAG_UNABLE_TO_READ_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read operation' )
    CASE (ERRFLAG_VALUE_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'value undefined' )
    CASE (ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'type not allocated after read' )
    CASE (ERRFLAG_UNABLE_TO_CONVERT_TO_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to integer' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate' )
    CASE (ERRFLAG_UNABLE_TO_READ_FLOAT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read floating point number' )
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

END FUNCTION FLOATOP_CONSTANT_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Matches a operation constanteter with a message and constanteter.
!>
!> This function checks whether the provided message (`MSG`) and constanteter (`PAR`)
!> match the operation's criteria. If a match is found, the `MATCH` flag is set to `.TRUE.`;
!> otherwise, it is set to `.FALSE.`. Hooks can be applied during the matching process
!> to allow additional processing.
!>
!> @constant [inout] THIS  The operation object (`FLOATOP_CONSTANT_T`) used for matching.
!> @constant [in]    MSG   The message (`FORTRAN_MESSAGE_T`) that is checked against the operation.
!> @constant [in]    PAR   The constanteter object (`CONSTANTETRIZATION_T`) used in the matching process.
!> @constant [out]   MATCH Logical flag indicating whether the message and constanteter match the operation's criteria.
!> @constant [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during matching.
!>
!> @return Integer error code (`RET`) indicating success or failure of the matching process.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] CONSTANTETRIZATION_MOD::CONSTANTETRIZATION_T
!> @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FLOATOP_CONSTANT_EVAL'
FUNCTION FLOATOP_CONSTANT_EVAL( THIS, MSG, PAR, RESULT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPRD_K
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
  CLASS(FLOATOP_CONSTANT_T), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),   INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),   INTENT(IN)    :: PAR
  REAL(KIND=JPRD_K),         INTENT(OUT)   :: RESULT
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

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

  ! Return the result of the operation
  RESULT = THIS%CONSTANT_

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION FLOATOP_CONSTANT_EVAL
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
!> @constant [inout] THIS    The operation object (`FLOATOP_CONSTANT_T`) whose details are to be printed.
!> @constant [in]    UNIT    The output unit (file or console) where the operation's details will be printed.
!> @constant [in]    OFFSET  The offset applied to the printed output for formatting purposes.
!> @constant [inout] HOOKS   Structure (`HOOKS_T`) used for additional hooks or callbacks during printing.
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
#define PP_PROCEDURE_NAME 'FLOATOP_CONSTANT_PRINT'
FUNCTION FLOATOP_CONSTANT_PRINT( THIS, UNIT, OFFSET, HOOKS, SEPARATOR ) RESULT(RET)

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
  CLASS(FLOATOP_CONSTANT_T),  INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: SEPARATOR

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=32) :: TMP

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

  TMP = REPEAT( ' ', 32 )
  WRITE(TMP,*) THIS%CONSTANT_

  !> Print the constant operation
  IF ( PRESENT(SEPARATOR) ) THEN
    WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'constant :: '//TRIM(ADJUSTL(TMP))//TRIM(ADJUSTL(SEPARATOR))
  ELSE
    WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'constant :: '//TRIM(ADJUSTL(TMP))
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

END FUNCTION FLOATOP_CONSTANT_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees the memory and resources associated with the operation.
!>
!> This function deallocates the memory and resources used by the operation object.
!> It ensures proper cleanup of nested structures and any associated hooks.
!> After this function is called, the operation object should no longer be used.
!>
!> @constant [inout] THIS   The operation object (`FLOATOP_CONSTANT_T`) whose resources are to be freed.
!> @constant [inout] HOOKS  Structure (`HOOKS_T`) used for additional hooks or callbacks during the deallocation process.
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
#define PP_PROCEDURE_NAME 'FLOATOP_CONSTANT_FREE'
FUNCTION FLOATOP_CONSTANT_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(FLOATOP_CONSTANT_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

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

END FUNCTION FLOATOP_CONSTANT_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE FLOATOP_CONSTANT_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME