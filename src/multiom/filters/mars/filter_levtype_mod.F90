!>
!> @file filter_direction_mod.F90
!>
!> @brief Module containing definitions and procedures for levtype filters.
!>
!> This module defines the `FILTER_LEVTYPE_T` type, along with its associated
!> procedures and helper functions that facilitate the creation, management, and
!> utilization of levtype filters within the system. Levtype filters allow for
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


#define PP_FILE_NAME 'filter_levtype_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'FILTER_LEVTYPE_MOD'
MODULE FILTER_LEVTYPE_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: FILTER_BASE_MOD,   ONLY: FILTER_BASE_A
  USE :: KEYSET_INT64_MOD,  ONLY: KEYSET_INT64_T

IMPLICIT NONE

!> Default visibility of the module
PRIVATE


!>
!> @brief Filter type for managing level types (LEVTYPE).
!>
!> This derived type `FILTER_LEVTYPE_T` is used to manage level-type-specific filtering.
!> It extends the base filter type `FILTER_BASE_A` and allows operations such as
!> initialization, matching, printing, and freeing of level type filters.
!>
TYPE, EXTENDS(FILTER_BASE_A) :: FILTER_LEVTYPE_T

  ! Default visibility of the type
  PRIVATE

  !> Specifies the type of filter operation (e.g., MATCH, IGNORE, THRESHOLD).
  INTEGER(KIND=JPIB_K) :: FILTER_TYPE_

  !> Indicates whether to use a keyset for the level types.
  LOGICAL :: USE_KEYSET_

  !> Stores the level type to filter by.
  INTEGER(KIND=JPIB_K) :: LEVTYPE_

  !> Holds the keyset of level types for filtering.
  TYPE(KEYSET_INT64_T) :: LEVTYPES_

CONTAINS

  !> Initializes the level type filter.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT  => FILTER_LEVTYPE_INIT

  !> Matches the filter with the input data based on level type.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: MATCH => FILTER_LEVTYPE_MATCH

  !> Prints the filter’s current state and configuration.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT => FILTER_LEVTYPE_PRINT

  !> Frees the resources allocated by the filter.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE  => FILTER_LEVTYPE_FREE

END TYPE

!> Whitlist of public symbols
PUBLIC :: FILTER_LEVTYPE_T

CONTAINS


!>
!> @brief Initializes a level type filter (`FILTER_LEVTYPE_T`) from a YAML configuration.
!>
!> This function initializes the `FILTER_LEVTYPE_T` object (`THIS`) by reading relevant
!> settings from the provided YAML configuration (`CFG`). The function ensures that all
!> necessary parameters are properly set and that any allocated memory is handled. It also
!> checks for specific keys within the YAML configuration and reads integer values, arrays,
!> or filters as needed. Debugging, logging, and tracing functionalities can be enabled if
!> configured.
!>
!> The `HOOKS` object may be used for additional processing or interactions with external
!> systems during the initialization process.
!>
!> @section interface
!> @param [inout] THIS The filter object of type `FILTER_LEVTYPE_T` to be initialized.
!> @param [in] CFG The YAML configuration object containing the initialization data.
!> @param [in]    OPT The generic options to be used to initialize the filter.
!> @param [inout] HOOKS An optional hooks object for external interaction.
!>
!> @return Integer error code (`RET`) indicating success or failure of the initialization.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_HAS_KEY
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_GET_SUBCONFIGURATIONS
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_GET_CONFIGURATIONS_SIZE
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_GET_CONFIGURATION_BY_ID
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_READ_INTEGER_WITH_FILTER
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_READ_INTEGER_ARRAY_WITH_FILTER
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_DELETE_CONFIGURATION
!> @dependency [PROCEDURE] YAML_CORE_UTILS_MOD::YAML_DELETE_CONFIGURATIONS
!> @dependency [PROCEDURE] ENUMERATORS_MOD::CLEVTYPE2ILEVTYPE
!> @dependency [PROCEDURE] ENUMERATORS_MOD::FLT_INT_COP2IOP
!> @dependency [PROCEDURE] ENUMERATORS_MOD::FLT_INT_MATCH_E
!> @dependency [PROCEDURE] ENUMERATORS_MOD::FLT_INT_IGNORE_E
!> @dependency [PROCEDURE] ENUMERATORS_MOD::FLT_INT_GT_E
!> @dependency [PROCEDURE] ENUMERATORS_MOD::FLT_INT_GE_E
!> @dependency [PROCEDURE] ENUMERATORS_MOD::FLT_INT_LE_E
!> @dependency [PROCEDURE] ENUMERATORS_MOD::FLT_INT_LT_E
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see FILTER_LEVTYPE_T, YAML_CONFIGURATION_T, HOOKS_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_LEVTYPE_INIT'
FUNCTION FILTER_LEVTYPE_INIT( THIS, CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: FILTER_OPTIONS_MOD,  ONLY: FILTER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATIONS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_CONFIGURATIONS_SIZE
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_CONFIGURATION_BY_ID
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER_WITH_FILTER
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER_KEYSET_WITH_FILTER
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD, ONLY: FUN_C2I_IF
  USE :: ENUMERATORS_MOD,     ONLY: CLEVTYPE2ILEVTYPE
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_COP2IOP
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_MATCH_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_IGNORE_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_GT_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_GE_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_LE_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_LT_E
  ! TODO: USE :: FILTERS_FACTORY,          ONLY: MAKE_FILTER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FILTER_LEVTYPE_T),    INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  TYPE(FILTER_OPTIONS_T),     INTENT(IN)    :: OPT
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  PROCEDURE(FUN_C2I_IF), POINTER :: P_FLT_INT_COP2IOP
  PROCEDURE(FUN_C2I_IF), POINTER :: P_CLEVTYPE2ILEVTYPE
  LOGICAL :: HAS_OPERATION
  LOGICAL :: HAS_VALUE
  LOGICAL :: HAS_VALUES
  INTEGER(KIND=JPIB_K) :: TMP

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_UNDEFINED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_TYPE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_OPERATION = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_VALUE = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_VALUES = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UDEFINED_TYPE = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INIT_KEYSET = 8_JPIB_K

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

  !> Initialize the nested filters
  PP_TRYCALL(ERRFLAG_INIT_KEYSET) THIS%LEVTYPES_%INIT( HOOKS )
  THIS%USE_KEYSET_ = .FALSE.

  !> Associate the filter procedure
  P_CLEVTYPE2ILEVTYPE => CLEVTYPE2ILEVTYPE

  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION ) YAML_CONFIGURATION_HAS_KEY( CFG, 'operation', HAS_OPERATION, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_OPERATION, ERRFLAG_OPERATION_UNDEFINED )

  !> Read all the subconfigurations
  P_FLT_INT_COP2IOP => FLT_INT_COP2IOP
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION) YAML_READ_INTEGER_WITH_FILTER( CFG, 'operation', THIS%FILTER_TYPE_, P_FLT_INT_COP2IOP, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%FILTER_TYPE_ .EQ. FLT_INT_GT_E, ERRFLAG_INVALID_TYPE )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%FILTER_TYPE_ .EQ. FLT_INT_GE_E, ERRFLAG_INVALID_TYPE )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%FILTER_TYPE_ .EQ. FLT_INT_LE_E, ERRFLAG_INVALID_TYPE )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%FILTER_TYPE_ .EQ. FLT_INT_LT_E, ERRFLAG_INVALID_TYPE )


  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'value', HAS_VALUE, HOOKS )

  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'values', HAS_VALUES, HOOKS )


  !> Read the paramID to be used in the filter
  IF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_MATCH_E .AND. HAS_VALUES .AND. .NOT.HAS_VALUE ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VALUES) YAML_READ_INTEGER_KEYSET_WITH_FILTER( CFG, 'values', THIS%LEVTYPES_, P_CLEVTYPE2ILEVTYPE, HOOKS )
    THIS%USE_KEYSET_ = .TRUE.

  ELSEIF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_MATCH_E .AND. HAS_VALUE .AND. .NOT.HAS_VALUES ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VALUE) YAML_READ_INTEGER_WITH_FILTER( CFG, 'value',THIS%LEVTYPE_, P_CLEVTYPE2ILEVTYPE, HOOKS )

  ELSEIF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_IGNORE_E .AND. HAS_VALUES .AND. .NOT.HAS_VALUE )THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VALUES) YAML_READ_INTEGER_KEYSET_WITH_FILTER( CFG, 'values', THIS%LEVTYPES_, P_CLEVTYPE2ILEVTYPE, HOOKS )
    THIS%USE_KEYSET_ = .TRUE.

  ELSEIF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_IGNORE_E .AND. HAS_VALUE .AND. .NOT.HAS_VALUES ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VALUE) YAML_READ_INTEGER_WITH_FILTER( CFG, 'value', THIS%LEVTYPE_, P_CLEVTYPE2ILEVTYPE, HOOKS )

  ELSE

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UDEFINED_TYPE )

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
    CASE (ERRFLAG_UNABLE_TO_READ_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read operation' )
    CASE (ERRFLAG_OPERATION_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'operation undefined' )
    CASE (ERRFLAG_INVALID_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid operation type' )
    CASE (ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read cfg' )
    CASE (ERRFLAG_UNABLE_TO_READ_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read value' )
    CASE (ERRFLAG_UNABLE_TO_READ_VALUES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read values' )
    CASE (ERRFLAG_UDEFINED_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'undefined type' )
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

END FUNCTION FILTER_LEVTYPE_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Matches a given parameterization (`PAR`) against the filter level type (`FILTER_LEVTYPE_T`).
!>
!> This function checks if the given parameterization (`PAR`) matches the current level type filter (`THIS`)
!> using specific matching rules. The result of the match is stored in the logical flag `MATCH`.
!> The function operates in conjunction with hooks (`HOOKS`) for additional operations, and logging, tracing,
!> and debugging can be enabled.
!>
!> The function also considers various enumerated match types (e.g., `FLT_INT_MATCH_E`, `FLT_INT_IGNORE_E`)
!> to decide the match result.
!>
!> @section interface
!> @param [inout] THIS The level type filter object (`FILTER_LEVTYPE_T`) for the match check.
!> @param [in] MSG A message object (`FORTRAN_MESSAGE_T`) used during the match operation.
!> @param [in] PAR The parameterization object (`PARAMETRIZATION_T`) to be matched against the filter.
!> @param [out] MATCH Logical flag set to `.TRUE.` if the parameterization matches, `.FALSE.` otherwise.
!> @param [inout] HOOKS Optional hooks object used for external operations during the match.
!>
!> @return Integer error code (`RET`) indicating success or failure of the match operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] PARAMETRIZATION_MOD::PARAMETRIZATION_T
!> @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!> @dependency [PROCEDURE] ENUMERATORS_MOD::FLT_INT_MATCH_E
!> @dependency [PROCEDURE] ENUMERATORS_MOD::FLT_INT_IGNORE_E
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see FILTER_LEVTYPE_T, PARAMETRIZATION_T, FORTRAN_MESSAGE_T, HOOKS_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_LEVTYPE_MATCH'
FUNCTION FILTER_LEVTYPE_MATCH( THIS, MSG, PAR, MATCH, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_MATCH_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_IGNORE_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FILTER_LEVTYPE_T),    INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),  INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),  INTENT(IN)    :: PAR
  LOGICAL,                  INTENT(OUT)   :: MATCH
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_OPERATION = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_KEYSET_MATCH = 2_JPIB_K

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
  SELECT CASE( THIS%FILTER_TYPE_ )

  CASE( FLT_INT_MATCH_E )

    IF ( THIS%USE_KEYSET_ ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_KEYSET_MATCH) THIS%LEVTYPES_%MATCH( MSG%LEVTYPE, MATCH, HOOKS )
    ELSE
      MATCH = (THIS%LEVTYPE_ .EQ. MSG%LEVTYPE)
    ENDIF

  CASE( FLT_INT_IGNORE_E )

    IF ( THIS%USE_KEYSET_ ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_KEYSET_MATCH) THIS%LEVTYPES_%MATCH( MSG%LEVTYPE, MATCH, HOOKS )
      MATCH = .NOT.MATCH
    ELSE
      MATCH = .NOT.(THIS%LEVTYPE_ .EQ. MSG%LEVTYPE)
    ENDIF

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to evaluate nested filter' )
    CASE (ERRFLAG_UNABLE_TO_CALL_KEYSET_MATCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call keyset match' )
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

END FUNCTION FILTER_LEVTYPE_MATCH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Prints the details of a level type filter (`FILTER_LEVTYPE_T`) to the specified output unit.
!>
!> This function outputs the internal state and properties of the level type filter object (`THIS`)
!> to a specified output unit (`UNIT`). The `OFFSET` parameter allows for indentation in the output,
!> facilitating better readability. The function may utilize hooks (`HOOKS`) for additional output
!> functionalities, such as custom logging or formatting. Debugging, logging, and tracing features
!> can be enabled using preprocessor directives.
!>
!> @section interface
!> @param [inout] THIS The level type filter object to be printed.
!> @param [in] UNIT The output unit number (e.g., 6 for standard output).
!> @param [in] OFFSET The indentation offset for formatted output.
!> @param [inout] HOOKS Optional hooks object for additional operations during printing.
!>
!> @return Integer error code (`RET`) indicating success or failure of the print operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see FILTER_LEVTYPE_T, HOOKS_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_LEVTYPE_PRINT'
FUNCTION FILTER_LEVTYPE_PRINT( THIS, UNIT, OFFSET, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: FLT_INT_IOP2COP

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FILTER_LEVTYPE_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),    INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),    INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=16) :: OPERATION_TYPE_STR

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_OPERATION) FLT_INT_IOP2COP( THIS%FILTER_TYPE_, OPERATION_TYPE_STR, HOOKS )

  !> Print the param filter
  WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'LEVTYPE FILTER: '//TRIM(ADJUSTL(OPERATION_TYPE_STR))

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
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FILTER_LEVTYPE_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Releases resources allocated for a level type filter (`FILTER_LEVTYPE_T`).
!>
!> This function frees the memory and resources associated with the level type filter object (`THIS`).
!> It is essential to call this function when the filter is no longer needed to avoid memory leaks.
!> The function can utilize hooks (`HOOKS`) for additional cleanup or logging functionalities.
!>
!> @section interface
!> @param [inout] THIS The level type filter object to be freed.
!> @param [inout] HOOKS Optional hooks object for additional operations during freeing.
!>
!> @return Integer error code (`RET`) indicating success or failure of the free operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see FILTER_LEVTYPE_T, HOOKS_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_LEVTYPE_FREE'
FUNCTION FILTER_LEVTYPE_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(FILTER_LEVTYPE_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILTERS_NOT_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_KEYSET = 2_JPIB_K

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

  ! Free keyset
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_KEYSET) THIS%LEVTYPES_%FREE( HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_FREE_KEYSET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free keyset'  )
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

END FUNCTION FILTER_LEVTYPE_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE FILTER_LEVTYPE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME