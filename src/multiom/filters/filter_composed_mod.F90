!>
!> @file filter_composed_mod.F90
!>
!> @brief Module containing definitions and procedures for composed filters.
!>
!> This module defines the `FILTER_COMPOSED_T` type, along with its associated
!> procedures and helper functions that facilitate the creation, management, and
!> utilization of composed filters within the system. Composed filters allow for
!> complex filtering operations by combining multiple nested filters.
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'filter_composed_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'FILTER_COMPOSED_MOD'
MODULE FILTER_COMPOSED_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: FILTER_BASE_MOD,   ONLY: FILTER_BASE_A
  USE :: FILTER_BASE_MOD,   ONLY: FILTER_CONTAINER_T

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!>
!> @brief Defines a composed filter that can aggregate multiple nested filters.
!>
!> The `FILTER_COMPOSED_T` type extends the abstract class `FILTER_BASE_A`
!> and provides functionality to manage a collection of nested filters. It supports
!> various logical operations on these nested filters, enabling complex filtering
!> logic to be constructed from simpler components.
!>
!> @section Properties
!>   - `OPERATION_TYPE_`: Specifies the type of operation to be performed on the
!>     nested filters. The possible values are:
!>       - `ALL`: All conditions must be satisfied.
!>       - `ANY`: At least one condition must be satisfied.
!>       - `NONE`: No conditions should be satisfied.
!>       - `ONE`: Exactly one condition must be satisfied.
!>   - `FILTERS_`: An array of pointers to nested filters contained within this
!>     composed filter.
!>   - `CONDITIONS_`: An array of logical pointers that i used as a temporary storage
!>      for the results of the nested filters match evaluation.
!>
!> @section interface
!>   - Provides several procedures to manage and utilize nested filters:
!>     - `INIT`: Initializes the composed filter.
!>     - `MATCH`: Evaluates the conditions against the nested filters.
!>     - `FREE`: Deallocates resources associated with the composed filter.
!>     - `ALLOCATE_NESTED_FILTERS`: Allocates memory for the nested filters.
!>     - `GET_NESTED_FILTERS`: Retrieves the array of nested filters.
!>
!> @section dependencies
!>   - @dependency [TYPE] FILTER_BASE_A
!>   - @dependency [TYPE] FILTER_CONTAINER_T
!>
TYPE, EXTENDS(FILTER_BASE_A) :: FILTER_COMPOSED_T

  !> Default visibility of the type
  PRIVATE

  !> ALL, ANY, NONE, ONE
  INTEGER(KIND=JPIB_K) :: OPERATION_TYPE_

  !> Nested filters
  TYPE(FILTER_CONTAINER_T), POINTER, DIMENSION(:) :: FILTERS_ => NULL()
  LOGICAL, POINTER, DIMENSION(:) :: CONDITIONS_ => NULL()

CONTAINS

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT  => FILTER_COMPOSED_INIT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: MATCH => FILTER_COMPOSED_MATCH
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT => FILTER_COMPOSED_PRINT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE  => FILTER_COMPOSED_FREE

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: ALLOCATE_NESTED_FILTERS => FILTER_COMPOSED_ALLOCATE_FILTERS
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_NESTED_FILTERS => FILTER_COMPOSED_GET_FILTERS

END TYPE

!> Whitlist of public symbols
PUBLIC :: FILTER_COMPOSED_T

CONTAINS


!>
!> @brief Initializes the composed filter.
!>
!> This thread-safe function initializes the composed filter instance by
!> allocating necessary resources and setting its initial state based on
!> the provided configuration. It also configures debugging, logging, and
!> tracing mechanisms through the `HOOKS` parameter.
!>
!> @param [inout] THIS The instance of the composed filter to be initialized.
!> @param [in]    CFG The YAML configuration object containing parameters for
!>                    initializing the composed filter.
!> @param [in]    OPT The generic options to be used to initialize the filter.
!> @param [inout] HOOKS A structure used for debugging, tracing, and logging
!>                       purposes, allowing for enhanced monitoring of the
!>                       initialization process.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [READ] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_COMPOSED_INIT'
PP_THREAD_SAFE FUNCTION FILTER_COMPOSED_INIT( THIS, CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: FILTER_OPTIONS_MOD,  ONLY: FILTER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER_WITH_FILTER
  USE :: YAML_CORE_UTILS_MOD, ONLY: FUN_C2I_IF
  USE :: ENUMERATORS_MOD,     ONLY: FLT_COMPOSE_COP2IOP

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FILTER_COMPOSED_T),   INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  TYPE(FILTER_OPTIONS_T),     INTENT(IN)    :: OPT
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  PROCEDURE(FUN_C2I_IF), POINTER :: P_FLT_COMPOSE_COP2IOP
  LOGICAL :: HAS_OPERATION

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILTERS_ALREADY_ALLOCATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONDITIONS_ALREADY_ALLOCATED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_OPERATION = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_UNDEFINED = 4_JPIB_K

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

  ! Check if the filters are already allocated
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%FILTERS_), ERRFLAG_FILTERS_ALREADY_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%CONDITIONS_), ERRFLAG_CONDITIONS_ALREADY_ALLOCATED )

  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION ) YAML_CONFIGURATION_HAS_KEY( CFG, 'operation', HAS_OPERATION, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_OPERATION, ERRFLAG_OPERATION_UNDEFINED )

  !> Read all the subconfigurations
  P_FLT_COMPOSE_COP2IOP => FLT_COMPOSE_COP2IOP
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION) YAML_READ_INTEGER_WITH_FILTER( CFG, 'operation', THIS%OPERATION_TYPE_, P_FLT_COMPOSE_COP2IOP, HOOKS )

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
    CASE (ERRFLAG_FILTERS_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'filters are already allocated' )
    CASE (ERRFLAG_CONDITIONS_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'conditions are already allocated' )
    CASE (ERRFLAG_UNABLE_TO_READ_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read operation' )
    CASE (ERRFLAG_OPERATION_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'operation is undefined' )
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

END FUNCTION FILTER_COMPOSED_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Evaluates the composed filter against a given message and parameterization.
!>
!> This thread-safe function checks if the provided message (`MSG`) matches the
!> criteria defined by the composed filter instance. It uses the parameterization
!> object (`PAR`) for additional matching logic and stores the result in the
!> `MATCH` output parameter. The function also supports debugging and tracing
!> through the `HOOKS` parameter.
!>
!> @param [inout] THIS The instance of the composed filter being evaluated.
!> @param [in] MSG The message object containing data to be evaluated against
!>                 the filter's criteria.
!> @param [in] PAR The parameterization object providing additional context for
!>                 the evaluation process.
!> @param [out] MATCH Logical flag that will be set to `.TRUE.` if the message
!>                   matches the filter criteria, or `.FALSE.` otherwise.
!> @param [inout] HOOKS A structure for debugging, tracing, or logging purposes
!>                       that allows for monitoring the evaluation process.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [READ] PARAMETRIZATION_MOD::PARAMETRIZATION_T
!>   - @dependency [READ] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!>   - @dependency [READ] HOOKS_MOD::HOOKS_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_COMPOSED_MATCH'
PP_THREAD_SAFE FUNCTION FILTER_COMPOSED_MATCH( THIS, MSG, PAR, MATCH, HOOKS ) RESULT(RET)

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
  CLASS(FILTER_COMPOSED_T), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),  INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),  INTENT(IN)    :: PAR
  LOGICAL,                  INTENT(OUT)   :: MATCH
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EVALUTE_NESTED_FILTER = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EVALUATE_COMPOSER = 2_JPIB_K

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

  !> Evaluate the nested filters
  NestedFiltersEvaluationLoop: DO I = 1, SIZE(THIS%FILTERS_)
    PP_TRYCALL(ERRFLAG_EVALUTE_NESTED_FILTER) THIS%FILTERS_(I)%FILTER_%MATCH( MSG,PAR, THIS%CONDITIONS_(I), HOOKS )
  ENDDO NestedFiltersEvaluationLoop

  !> Compose the nested filters
  PP_TRYCALL(ERRFLAG_EVALUATE_COMPOSER) FILTER_COMPOSED_COMPOSE( THIS%OPERATION_TYPE_, THIS%CONDITIONS_, MATCH, HOOKS )

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
    CASE (ERRFLAG_EVALUTE_NESTED_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to evaluate nested filter' )
    CASE (ERRFLAG_EVALUATE_COMPOSER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to evaluate composer' )
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

END FUNCTION FILTER_COMPOSED_MATCH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Prints the details of a composed filter to the specified output unit.
!>
!> This thread-safe function outputs the current state and details of the composed
!> filter represented by `THIS` to the designated output unit. It includes an
!> optional offset for formatting. The printing process utilizes hooks for
!> debugging, logging, and tracing purposes.
!>
!> @param [inout] THIS The composed filter object whose details are to be printed.
!> @param [in] UNIT The output unit (file or console) where the filter details
!>                  will be printed.
!> @param [in] OFFSET The offset used for formatting the printed output.
!> @param [inout] HOOKS A structure for debugging, tracing, or logging purposes
!>                       that facilitates monitoring the print process.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [READ] HOOKS_MOD::HOOKS_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_COMPOSED_PRINT'
PP_THREAD_SAFE FUNCTION FILTER_COMPOSED_PRINT( THIS, UNIT, OFFSET, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: FLT_COMPOSE_IOP2COP

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FILTER_COMPOSED_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=16) :: OPERATION_TYPE_STR
  INTEGER(KIND=JPIB_K) :: I

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_OPERATION = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_NESTED_OPERATION = 2_JPIB_K

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_OPERATION) FLT_COMPOSE_IOP2COP( THIS%OPERATION_TYPE_, OPERATION_TYPE_STR, HOOKS )

  !> Print the composed filter
  WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'COMPOSED FILTER: '//TRIM(ADJUSTL(OPERATION_TYPE_STR))//'('
  DO I = 1, SIZE(THIS%FILTERS_)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_NESTED_OPERATION) THIS%FILTERS_(I)%FILTER_%PRINT( UNIT, OFFSET+2, HOOKS )
  ENDDO
  WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//')'

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert operation type to string' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_NESTED_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to print nested operation' )
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

END FUNCTION FILTER_COMPOSED_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees the resources associated with the composed filter instance.
!>
!> This thread-safe function releases any dynamically allocated resources held by
!> the `THIS` filter composed instance. It ensures that all nested filters are
!> properly deallocated to prevent memory leaks. The function can utilize hooks for
!> debugging, logging, and tracing purposes through the `HOOKS` parameter.
!>
!> @param [inout] THIS The instance of the composed filter whose resources are to
!>                     be freed.
!> @param [inout] HOOKS A structure for debugging, tracing, or logging purposes
!>                       that facilitates monitoring the resource deallocation process.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [WRITE] HOOKS_MOD::HOOKS_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_COMPOSED_FREE'
PP_THREAD_SAFE FUNCTION FILTER_COMPOSED_FREE( THIS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  ! TODO: USE :: FILTERS_FACTORY,   ONLY: DESTROY_FILTER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FILTER_COMPOSED_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILTERS_NOT_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONDITIONS_NOT_ASSOCIATED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NESTED_FILTER_NOT_ASSOCIATED = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DESTROY_NESTED_FILTER = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_FILTERS = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_CONDITIONS = 6_JPIB_K

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

  ! Check if the nested filters are associated
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED( THIS%FILTERS_ ), ERRFLAG_FILTERS_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED( THIS%CONDITIONS_ ), ERRFLAG_CONDITIONS_NOT_ASSOCIATED )

  ! Destroy containers
  DEALLOCATE( THIS%FILTERS_, STAT=DEALLOC_STATE, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATE .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_FILTERS )

  ! Destry support array
  DEALLOCATE( THIS%CONDITIONS_, STAT=DEALLOC_STATE, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATE .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_CONDITIONS )

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
    CASE (ERRFLAG_FILTERS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'nested filters are not associated' )
    CASE (ERRFLAG_CONDITIONS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'conditions are not associated' )
    CASE (ERRFLAG_NESTED_FILTER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'nested filter is not associated' )
    CASE (ERRFLAG_DESTROY_NESTED_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to destroy nested filter' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE_FILTERS)
      IF (ALLOCATED(ERRMSG)) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate filters: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate filters'  )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE_CONDITIONS)
      IF (ALLOCATED(ERRMSG)) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME('Unable to deallocate coinsitions: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate conditions' )
      ENDIF
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

END FUNCTION FILTER_COMPOSED_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Retrieves the nested filters contained within the composed filter instance.
!>
!> This thread-safe function populates the `NESTED_FILTERS` array with the pointers
!> to the nested filters that are part of the composed filter instance identified by
!> `THIS`. It allows users to access and manage the individual filters for further
!> processing. The function also utilizes hooks for debugging, logging, and tracing
!> through the `HOOKS` parameter.
!>
!> @param [inout] THIS The instance of the composed filter from which nested filters
!>                     are to be retrieved.
!> @param [out] NESTED_FILTERS An array of pointers to the nested filters within
!>                              the composed filter instance. This array will be
!>                              populated by the function.
!> @param [inout] HOOKS A structure for debugging, tracing, or logging purposes
!>                       that facilitates monitoring the retrieval process of nested
!>                       filters.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [READ] HOOKS_MOD::HOOKS_T
!>   - @dependency [READ] FILTER_BASE_MOD::FILTER_CONTAINER_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_COMPOSED_GET_FILTERS'
PP_THREAD_SAFE FUNCTION FILTER_COMPOSED_GET_FILTERS( THIS, NESTED_FILTERS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: FILTER_BASE_MOD,   ONLY: FILTER_CONTAINER_T
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FILTER_COMPOSED_T),                        INTENT(INOUT) :: THIS
  TYPE(FILTER_CONTAINER_T), DIMENSION(:), POINTER, INTENT(OUT)   :: NESTED_FILTERS
  TYPE(HOOKS_T),                                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILTERS_NOT_ASSOCIATED = 1_JPIB_K

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

  ! Check if the nested filters are associated
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED( THIS%FILTERS_ ), ERRFLAG_FILTERS_NOT_ASSOCIATED )

  !> REturn the nested filters
  NESTED_FILTERS => THIS%FILTERS_

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
    CASE (ERRFLAG_FILTERS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'nested filters are not associated' )
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

END FUNCTION FILTER_COMPOSED_GET_FILTERS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Allocates memory for the specified number of nested filters within the composed filter instance.
!>
!> This thread-safe function allocates memory for `N_FILTERS` nested filters
!> in the composed filter instance represented by `THIS`. It initializes the
!> internal structure to hold the nested filters and uses hooks for debugging,
!> logging, and tracing purposes.
!>
!> Allocation has not been done in the constructor/initialization, in order to avoid
!> circular dependencies between the composed filter and the nested filters.
!> For this reason allocation of the nested filters is delegated to the (recursive) factory.
!>
!> @param [inout] THIS The instance of the composed filter in which the nested
!>                     filters will be allocated.
!> @param [in] N_FILTERS The number of nested filters to allocate within the
!>                       composed filter instance.
!> @param [inout] HOOKS A structure for debugging, tracing, or logging purposes
!>                       that facilitates monitoring the allocation process of nested
!>                       filters.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [READ] HOOKS_MOD::HOOKS_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_COMPOSED_ALLOCATE_FILTERS'
PP_THREAD_SAFE FUNCTION FILTER_COMPOSED_ALLOCATE_FILTERS( THIS, N_FILTERS, HOOKS ) RESULT(RET)

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
  CLASS(FILTER_COMPOSED_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),     INTENT(IN)    :: N_FILTERS
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: ALLOC_STATE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILTERS_ALREADY_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONDITIONS_ALREADY_ASSOCIATED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_FILTERS = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_CONDITIONS = 4_JPIB_K

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

  ! Check if the nested filters are associated
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED( THIS%FILTERS_ ), ERRFLAG_FILTERS_ALREADY_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED( THIS%CONDITIONS_ ), ERRFLAG_CONDITIONS_ALREADY_ASSOCIATED )

  !> Allocate the nested filters
  ALLOCATE( THIS%FILTERS_(N_FILTERS), STAT=ALLOC_STATE, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATE .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_FILTERS )

  ALLOCATE( THIS%CONDITIONS_(N_FILTERS), STAT=ALLOC_STATE, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATE .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_CONDITIONS )

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
    CASE (ERRFLAG_FILTERS_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'nested filters are already associated' )
    CASE (ERRFLAG_CONDITIONS_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'conditions are already associated' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_FILTERS)
      IF (ALLOCATED(ERRMSG)) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate filters: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate filters'  )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_CONDITIONS)
      IF (ALLOCATED(ERRMSG)) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate conditions: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate conditions'  )
      ENDIF
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

END FUNCTION FILTER_COMPOSED_ALLOCATE_FILTERS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Composes a logical match result based on the specified operation and conditions.
!>
!> This thread-safe function evaluates the logical `CONDITIONS` using the
!> specified `OPERATION_TYPE` (`OP`) and determines the overall match result
!> (`MATCH`). It utilizes hooks for debugging, logging, and tracing the
!> composition process.
!>
!> @param [in] OP The operation type to be applied, indicating how to
!>                combine the logical conditions (e.g., ALL, ANY, NONE, ONE).
!> @param [in] CONDITIONS An array of logical values that represent the conditions
!>                        to be evaluated.
!> @param [out] MATCH The resulting logical value indicating the outcome
!>                    of the composition based on the operation and conditions.
!> @param [inout] HOOKS A structure for debugging, tracing, or logging purposes
!>                       that facilitates monitoring the composition process.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [READ] HOOKS_MOD::HOOKS_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_COMPOSED_COMPOSE'
PP_THREAD_SAFE FUNCTION FILTER_COMPOSED_COMPOSE( OP, CONDITIONS, MATCH, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: FLT_COMPOSE_ALL_E
  USE :: ENUMERATORS_MOD,   ONLY: FLT_COMPOSE_ANY_E
  USE :: ENUMERATORS_MOD,   ONLY: FLT_COMPOSE_NONE_E
  USE :: ENUMERATORS_MOD,   ONLY: FLT_COMPOSE_ONE_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: OP
  LOGICAL, DIMENSION(:), INTENT(IN)    :: CONDITIONS
  LOGICAL,               INTENT(OUT)   :: MATCH
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: CNT

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_OPERATION = 1_JPIB_K

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

  ! Initialize output variable
  MATCH = .FALSE.

  ! Comput number of matches
  N = SIZE(CONDITIONS)
  CNT = 0
  DO I = 1, N
    IF( CONDITIONS(I) ) THEN
      CNT = CNT + 1
    END IF
  ENDDO

  ! Apply operation
  SELECT CASE( OP )

  CASE( FLT_COMPOSE_ALL_E )

    MATCH = (CNT .EQ. N)

  CASE( FLT_COMPOSE_ANY_E )

    MATCH = (CNT .GT. 0)

  CASE( FLT_COMPOSE_NONE_E )

    MATCH = (CNT .EQ. 0)

  CASE( FLT_COMPOSE_ONE_E )

    MATCH = (CNT .EQ. 1)

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
    CASE (ERRFLAG_UNKNOWN_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown operation' )
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

END FUNCTION FILTER_COMPOSED_COMPOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE FILTER_COMPOSED_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME