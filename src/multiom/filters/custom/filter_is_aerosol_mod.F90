!>
!> @file filter_is_aerosol_mod.F90
!>
!> @brief Module containing definitions and procedures for is_aerosol filters.
!>
!> This module defines the `FILTER_IS_AEROSOL_T` type, along with its associated
!> procedures and helper functions that facilitate the creation, management, and
!> utilization of is_aerosol filters within the system. Is_aerosol filters allow for
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


#define PP_FILE_NAME 'filter_is_aerosol_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'FILTER_IS_AEROSOL_MOD'
MODULE FILTER_IS_AEROSOL_MOD

  ! Symbols imported from other modules within the project.
  USE :: FILTER_BASE_MOD, ONLY: FILTER_BASE_A

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!>
!> @brief Type definition for an aerosol filter.
!>
!> The `FILTER_IS_AEROSOL_T` type extends the base filter type `FILTER_BASE_A` and is designed
!> to represent aerosol filters within the system. This type encapsulates the necessary procedures
!> for initialization, matching, printing, and freeing resources associated with aerosol filters.
!>
TYPE, EXTENDS(FILTER_BASE_A) :: FILTER_IS_AEROSOL_T

  !> Default visibility of the type
  PRIVATE

CONTAINS

  !> Initializes the aerosol filter with default values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT  => FILTER_IS_AEROSOL_INIT

  !> Compares the aerosol filter against provided criteria to determine a match.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: MATCH => FILTER_IS_AEROSOL_MATCH

  !> Outputs the details of the aerosol filter to the specified output.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT => FILTER_IS_AEROSOL_PRINT

  !> Frees the resources allocated for the aerosol filter to prevent memory leaks.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE  => FILTER_IS_AEROSOL_FREE


END TYPE

!> Whitlist of public symbols
PUBLIC :: FILTER_IS_AEROSOL_T

CONTAINS


!>
!> @brief Initializes the aerosol filter with the provided configuration.
!>
!> This function initializes the `FILTER_IS_AEROSOL_T` instance (`THIS`) using the specified
!> YAML configuration object (`CFG`). It sets up the necessary hooks and prepares the
!> filter for use. This function ensures that the aerosol filter is correctly configured
!> before any operations are performed on it.
!>
!> @param [inout] THIS The aerosol filter instance to be initialized.
!> @param [in]    CFG The YAML configuration object containing initialization parameters.
!> @param [in]    OPT The generic options to be used to initialize the filter.
!> @param [inout] HOOKS The hooks that may be utilized during the initialization process.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_IS_AEROSOL_INIT'
FUNCTION FILTER_IS_AEROSOL_INIT( THIS, CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: FILTER_OPTIONS_MOD,  ONLY: FILTER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FILTER_IS_AEROSOL_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  TYPE(FILTER_OPTIONS_T),     INTENT(IN)    :: OPT
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

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
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FILTER_IS_AEROSOL_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Compares the aerosol filter parameters against provided values.
!>
!> This function checks whether the parameters defined in the `FILTER_IS_AEROSOL_T` instance
!> (`THIS`) match the specified values in `PAR`. It utilizes the `FORTRAN_MESSAGE_T` object
!> (`MSG`) to report any discrepancies. The result of the comparison is stored in the
!> logical variable `MATCH`, which indicates whether the parameters match.
!>
!> @param [inout] THIS The aerosol filter instance whose parameters are to be matched.
!> @param [in] MSG The message object used for reporting discrepancies.
!> @param [in] PAR The parameterization structure containing the values to be matched.
!> @param [out] MATCH Logical flag that will be set to `.TRUE.` if parameters match,
!>                     and `.FALSE.` otherwise.
!> @param [inout] HOOKS The hooks that may be utilized during the matching process.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_IS_AEROSOL_MATCH'
FUNCTION FILTER_IS_AEROSOL_MATCH( THIS, MSG, PAR, MATCH, HOOKS ) RESULT(RET)

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
  CLASS(FILTER_IS_AEROSOL_T), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),     INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),     INTENT(IN)    :: PAR
  LOGICAL,                     INTENT(OUT)   :: MATCH
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS

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

  !> TODO: Some kind of logic here
  MATCH = .FALSE.

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
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FILTER_IS_AEROSOL_MATCH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Prints the details of the aerosol filter instance.
!>
!> This function outputs the properties and current state of the `FILTER_IS_AEROSOL_T`
!> instance (`THIS`) to a specified output unit. The `OFFSET` parameter allows for
!> indentation or formatting in the output, providing a clearer presentation of the
!> aerosol filter's attributes. The `HOOKS` parameter may be utilized to trigger any
!> associated hooks during the printing process.
!>
!> @param [inout] THIS The aerosol filter instance whose details are to be printed.
!> @param [in] UNIT The output unit to which the details will be printed.
!> @param [in] OFFSET An integer value used to format the output, allowing for
!>                     indentation or spacing.
!> @param [inout] HOOKS The hooks that may be invoked during the printing process.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_IS_AEROSOL_PRINT'
FUNCTION FILTER_IS_AEROSOL_PRINT( THIS, UNIT, OFFSET, HOOKS ) RESULT(RET)

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
  CLASS(FILTER_IS_AEROSOL_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),        INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),        INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS

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

  !> Print the param filter
  WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'IS_AEROSOL FILTER '

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
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FILTER_IS_AEROSOL_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees resources associated with the aerosol filter instance.
!>
!> This function deallocates any memory or resources that are currently being used by
!> the `FILTER_IS_AEROSOL_T` instance (`THIS`). It ensures that all associated
!> hooks are also properly released or reset. This function is essential for
!> managing memory effectively and preventing leaks within the application.
!>
!> @param [inout] THIS The aerosol filter instance from which resources will be freed.
!> @param [inout] HOOKS The hooks that may need to be reset or released during the
!>                       freeing process.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_IS_AEROSOL_FREE'
FUNCTION FILTER_IS_AEROSOL_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(FILTER_IS_AEROSOL_T), INTENT(INOUT) :: THIS
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
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FILTER_IS_AEROSOL_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE FILTER_IS_AEROSOL_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME