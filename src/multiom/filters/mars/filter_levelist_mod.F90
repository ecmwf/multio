!>
!> @file filter_level_mod.F90
!>
!> @brief Module containing definitions and procedures for level filters.
!>
!> This module defines the `FILTER_LEVELIST_T` type, along with its associated
!> procedures and helper functions that facilitate the creation, management, and
!> utilization of level filters within the system. Level filters allow for
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


#define PP_FILE_NAME 'filter_levelist_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'FILTER_LEVELIST_MOD'
MODULE FILTER_LEVELIST_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: FILTER_BASE_MOD,   ONLY: FILTER_BASE_A
  USE :: KEYSET_INT64_MOD,  ONLY: KEYSET_INT64_T

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!>
!> @brief A type representing a filter with support for nested filters and matching logic.
!>
!> This derived type extends `FILTER_BASE_A` and is used for filtering based on parameters.
!> It supports matching, ignoring, or applying thresholds to specific parameters, and can
!> also utilize a keyset to perform nested filtering.
!>
TYPE, EXTENDS(FILTER_BASE_A) :: FILTER_LEVELIST_T

  !> Default visibility of the type.
  PRIVATE

  !> @brief Type of the filter (e.g., MATCH, IGNORE, GT, GE, LE, LT).
  !> @details Defines the mode in which the filter operates. It can be configured to either match
  !> specific parameters, ignore them, or apply a threshold condition (gt, ge, le, lt).
  INTEGER(KIND=JPIB_K) :: FILTER_TYPE_

  !> @brief Indicator of whether a keyset is used for nested filtering.
  !> @details If `.TRUE.`, the `LEVELISTS_` keyset is used for filtering specific parameter IDs.
  LOGICAL :: USE_KEYSET_

  !> @brief The ID of the parameter for which the filter is applied.
  !> @details This is the identifier for the parameter being filtered. It is used
  !> when `USE_KEYSET_` is `.FALSE.`.
  INTEGER(KIND=JPIB_K) :: LEVELIST_

  !> @brief A keyset of parameter IDs used for nested filtering.
  !> @details A keyset that contains a list of parameter IDs for which the filter applies.
  !> This is used when `USE_KEYSET_` is `.TRUE.`.
  TYPE(KEYSET_INT64_T) :: LEVELISTS_

CONTAINS

  !> @brief Initializes the filter parameter type.
  !> @details This procedure sets up the `FILTER_LEVELIST_T` type, initializing its components.
  !>
  !> @param [in] this The instance of `FILTER_LEVELIST_T` to initialize.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT  => FILTER_LEVELIST_INIT

  !> @brief Matches a condition against the filter.
  !> @details This procedure checks whether a given condition matches the criteria defined by the filter.
  !>
  !> @param [in] this The instance of `FILTER_LEVELIST_T`.
  !> @param [in] input The input to be checked against the filter criteria.
  !> @return Logical result of the match operation (`.TRUE.` if match, `.FALSE.` otherwise).
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: MATCH => FILTER_LEVELIST_MATCH

  !> @brief Prints the filter's details for debugging or logging.
  !> @details Outputs the filter's configuration and current state.
  !>
  !> @param [in] this The instance of `FILTER_LEVELIST_T` to print.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT => FILTER_LEVELIST_PRINT

  !> @brief Frees resources allocated for the filter.
  !> @details Cleans up the `FILTER_LEVELIST_T` type, deallocating any resources used by the filter.
  !>
  !> @param [in] this The instance of `FILTER_LEVELIST_T` to free.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE  => FILTER_LEVELIST_FREE

END TYPE



!> Whitlist of public symbols
PUBLIC :: FILTER_LEVELIST_T

CONTAINS


!>
!> @brief Initializes the `FILTER_LEVELIST_T` filter from a YAML configuration.
!>
!> This function initializes the `FILTER_LEVELIST_T` type based on the provided YAML
!> configuration (`CFG`) and applies any hooks specified in `HOOKS`. The function
!> reads the necessary parameters from the configuration and sets up the filter structure.
!>
!> @param [inout] THIS The filter object (`FILTER_LEVELIST_T`) that will be initialized.
!> @param [in]    CFG  The YAML configuration object containing the filter settings.
!> @param [in]    OPT The generic options to be used to initialize the filter.
!> @param [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during initialization.
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
#define PP_PROCEDURE_NAME 'FILTER_LEVELIST_INIT'
FUNCTION FILTER_LEVELIST_INIT( THIS, CFG, OPT, HOOKS ) RESULT(RET)

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
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER_KEYSET_WITH_RANGES
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD, ONLY: FUN_C2I_IF
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_COP2IOP
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_MATCH_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_IGNORE_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_GT_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_GE_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_LE_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_LT_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_HAS_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_LACKS_E
  USE :: ENUMERATORS_MOD,     ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FILTER_LEVELIST_T),      INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  TYPE(FILTER_OPTIONS_T),     INTENT(IN)    :: OPT
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  PROCEDURE(FUN_C2I_IF), POINTER :: P_FLT_INT_COP2IOP
  LOGICAL :: HAS_OPERATION
  LOGICAL :: HAS_VALUE
  LOGICAL :: HAS_VALUES
  LOGICAL :: HAS_TRESHOLD
  INTEGER(KIND=JPIB_K) :: TMP
  LOGICAL :: MATCH

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_OPERATION = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_UNDEFINED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_VALUE = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_VALUES = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_UNDEFINED = 10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_UNDEFINED = 11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UDEFINED_TYPE = 12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INIT_KEYSET = 13_JPIB_K

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
  PP_TRYCALL(ERRFLAG_INIT_KEYSET) THIS%LEVELISTS_%INIT( HOOKS )
  THIS%USE_KEYSET_ = .FALSE.

  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION ) YAML_CONFIGURATION_HAS_KEY( CFG, 'operation', HAS_OPERATION, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_OPERATION, ERRFLAG_OPERATION_UNDEFINED )

  !> Read all the subconfigurations
  P_FLT_INT_COP2IOP => FLT_INT_COP2IOP
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION) YAML_READ_INTEGER_WITH_FILTER( CFG, 'operation', THIS%FILTER_TYPE_, P_FLT_INT_COP2IOP, HOOKS )

  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'treshold', HAS_TRESHOLD, HOOKS )

  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'value', HAS_VALUE, HOOKS )

  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'values', HAS_VALUES, HOOKS )


  !> Read the level to be used in the filter
  IF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_GT_E .AND. HAS_TRESHOLD .AND. &
&          .NOT.HAS_VALUE .AND. .NOT. HAS_VALUES ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VALUE) YAML_READ_INTEGER( CFG, 'treshold', TMP, HOOKS )
    THIS%LEVELIST_ = TMP

  ELSEIF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_GT_E .AND. HAS_TRESHOLD .AND. &
&          .NOT.HAS_VALUE .AND. .NOT.HAS_VALUES ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VALUE) YAML_READ_INTEGER( CFG, 'treshold', TMP, HOOKS )
    THIS%LEVELIST_ = TMP

  ELSEIF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_GE_E .AND. HAS_TRESHOLD .AND. &
&          .NOT.HAS_VALUE .AND. .NOT.HAS_VALUES ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VALUE) YAML_READ_INTEGER( CFG, 'treshold', TMP, HOOKS )
    THIS%LEVELIST_ = TMP

  ELSEIF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_LT_E .AND. HAS_TRESHOLD .AND. &
&          .NOT.HAS_VALUE .AND. .NOT.HAS_VALUES ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VALUE) YAML_READ_INTEGER( CFG, 'treshold', TMP, HOOKS )
    THIS%LEVELIST_ = TMP

  ELSEIF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_LE_E .AND. HAS_TRESHOLD .AND. &
&          .NOT.HAS_VALUE .AND. .NOT.HAS_VALUES ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VALUE) YAML_READ_INTEGER( CFG, 'treshold', TMP, HOOKS )
    THIS%LEVELIST_ = TMP

  ELSEIF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_MATCH_E .AND. HAS_VALUES  .AND. &
&          .NOT.HAS_TRESHOLD .AND. .NOT.HAS_VALUE ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VALUES) YAML_READ_INTEGER_KEYSET_WITH_RANGES( CFG, 'values', THIS%LEVELISTS_, HOOKS )
    THIS%USE_KEYSET_ = .TRUE.

  ELSEIF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_MATCH_E .AND. HAS_VALUE  .AND. &
&          .NOT.HAS_TRESHOLD .AND. .NOT.HAS_VALUES ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VALUE) YAML_READ_INTEGER( CFG, 'value', TMP, HOOKS )
    THIS%LEVELIST_ = TMP

  ELSEIF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_IGNORE_E .AND. HAS_VALUES  .AND. &
&          .NOT.HAS_TRESHOLD .AND. .NOT.HAS_VALUE )THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VALUES) YAML_READ_INTEGER_KEYSET_WITH_RANGES( CFG, 'values', THIS%LEVELISTS_, HOOKS )
    THIS%USE_KEYSET_ = .TRUE.

  ELSEIF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_IGNORE_E .AND. HAS_VALUE  .AND. &
&          .NOT.HAS_TRESHOLD .AND. .NOT.HAS_VALUES ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VALUE) YAML_READ_INTEGER( CFG, 'value', TMP, HOOKS )
    THIS%LEVELIST_ = TMP

  ELSEIF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_HAS_E .AND. .NOT.HAS_VALUE  .AND. &
&          .NOT.HAS_TRESHOLD .AND. .NOT.HAS_VALUES ) THEN

    THIS%LEVELIST_ = UNDEF_PARAM_E


  ELSEIF ( THIS%FILTER_TYPE_ .EQ. FLT_INT_LACKS_E .AND. .NOT.HAS_VALUE  .AND. &
&          .NOT.HAS_TRESHOLD .AND. .NOT.HAS_VALUES ) THEN

    THIS%LEVELIST_ = UNDEF_PARAM_E

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
    CASE (ERRFLAG_INIT_KEYSET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to initialize keyset' )
    CASE (ERRFLAG_UNABLE_TO_READ_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read operation' )
    CASE (ERRFLAG_OPERATION_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'operation undefined' )
    CASE (ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read cfg' )
    CASE (ERRFLAG_UNABLE_TO_READ_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read value' )
    CASE (ERRFLAG_UNABLE_TO_READ_VALUES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read values' )
    CASE (ERRFLAG_VALUE_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'value undefined' )
    CASE (ERRFLAG_VALUES_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'values undefined' )
    CASE (ERRFLAG_UDEFINED_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'undefined type' )
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

END FUNCTION FILTER_LEVELIST_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Matches a filter parameter with a message and parameter.
!>
!> This function checks whether the provided message (`MSG`) and parameter (`PAR`)
!> match the filter's criteria. If a match is found, the `MATCH` flag is set to `.TRUE.`;
!> otherwise, it is set to `.FALSE.`. Hooks can be applied during the matching process
!> to allow additional processing.
!>
!> @param [inout] THIS  The filter object (`FILTER_LEVELIST_T`) used for matching.
!> @param [in]    MSG   The message (`FORTRAN_MESSAGE_T`) that is checked against the filter.
!> @param [in]    PAR   The parameter object (`PARAMETRIZATION_T`) used in the matching process.
!> @param [out]   MATCH Logical flag indicating whether the message and parameter match the filter's criteria.
!> @param [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during matching.
!>
!> @return Integer error code (`RET`) indicating success or failure of the matching process.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] PARAMETRIZATION_MOD::PARAMETRIZATION_T
!> @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_LEVELIST_MATCH'
FUNCTION FILTER_LEVELIST_MATCH( THIS, MSG, PAR, MATCH, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_MATCH_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_IGNORE_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_GT_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_GE_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_LE_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_LT_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_HAS_E
  USE :: ENUMERATORS_MOD,     ONLY: FLT_INT_LACKS_E
  USE :: ENUMERATORS_MOD,     ONLY: UNDEF_PARAM_E
  ! USE :: ENUMERATORS_MOD,     ONLY: LEVTYPE_PL_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FILTER_LEVELIST_T), INTENT(INOUT) :: THIS
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
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FILTER = 3_JPIB_K

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

  ! Error handling
  ! PP_DEBUG_CRITICAL_COND_THROW( MSG%LEVTYPE .NE. LEVTYPE_PL_E, ERRFLAG_INVALID_FILTER )

  !> Match the filter
  IF ( MSG%LEVELIST .EQ. UNDEF_PARAM_E ) THEN

    MATCH = .FALSE.

  ELSE

    !> Evaluate the operation
    SELECT CASE( THIS%FILTER_TYPE_ )

    CASE( FLT_INT_MATCH_E )

      IF ( THIS%USE_KEYSET_ ) THEN
        PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_KEYSET_MATCH) THIS%LEVELISTS_%MATCH( MSG%LEVELIST, MATCH, HOOKS )
      ELSE
        MATCH = (THIS%LEVELIST_ .EQ. MSG%LEVELIST)
      ENDIF

    CASE( FLT_INT_IGNORE_E )

      IF ( THIS%USE_KEYSET_ ) THEN
        PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_KEYSET_MATCH) THIS%LEVELISTS_%MATCH( MSG%LEVELIST, MATCH, HOOKS )
        MATCH = .NOT.MATCH
      ELSE
        MATCH = .NOT.(THIS%LEVELIST_ .EQ. MSG%LEVELIST)
      ENDIF

    CASE( FLT_INT_GT_E )

      MATCH = (MSG%LEVELIST .GT. THIS%LEVELIST_ )

    CASE( FLT_INT_GE_E )

      MATCH = (MSG%LEVELIST .GE. THIS%LEVELIST_ )

    CASE( FLT_INT_LE_E )

      MATCH = (MSG%LEVELIST .LE. THIS%LEVELIST_ )

    CASE( FLT_INT_LT_E )

      MATCH = (MSG%LEVELIST .LT. THIS%LEVELIST_ )

    CASE( FLT_INT_HAS_E )

      MATCH = .NOT.(MSG%LEVELIST .EQ. THIS%LEVELIST_ )

    CASE( FLT_INT_LACKS_E )

      MATCH = (MSG%LEVELIST .EQ. THIS%LEVELIST_ )

    CASE DEFAULT

      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_OPERATION )

    END SELECT

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
    CASE (ERRFLAG_UNKNOWN_OPERATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to evaluate nested filter' )
    CASE (ERRFLAG_UNABLE_TO_CALL_KEYSET_MATCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call keyset match' )
!    CASE (ERRFLAG_INVALID_FILTER)
!      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid filter' )
!      PP_DEBUG_PUSH_MSG_TO_FRAME( '"levelist" filter can be used only with "levtype"="pl"' )
!      PP_DEBUG_PUSH_MSG_TO_FRAME( 'this constraint has been added to keep the cache small!!!!' )
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

END FUNCTION FILTER_LEVELIST_MATCH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Prints the filter's configuration and details to the specified output unit.
!>
!> This function prints the details of the filter to the output specified by the `UNIT`.
!> An `OFFSET` can be applied to format the output, and `HOOKS` can be used for additional
!> callbacks during the printing process. The printed output includes details about
!> the filter's current configuration.
!>
!> @param [inout] THIS    The filter object (`FILTER_LEVELIST_T`) whose details are to be printed.
!> @param [in]    UNIT    The output unit (file or console) where the filter's details will be printed.
!> @param [in]    OFFSET  The offset applied to the printed output for formatting purposes.
!> @param [inout] HOOKS   Structure (`HOOKS_T`) used for additional hooks or callbacks during printing.
!>
!> @return Integer error code (`RET`) indicating success or failure of the print operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!> @dependency [PROCEDURE] DESTROY_FILTER (*to be implemented)
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_LEVELIST_PRINT'
FUNCTION FILTER_LEVELIST_PRINT( THIS, UNIT, OFFSET, HOOKS ) RESULT(RET)

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
  CLASS(FILTER_LEVELIST_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

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
  WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'LEVELIST FILTER: '//TRIM(ADJUSTL(OPERATION_TYPE_STR))

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

END FUNCTION FILTER_LEVELIST_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees the memory and resources associated with the filter.
!>
!> This function deallocates the memory and resources used by the filter object.
!> It ensures proper cleanup of nested structures and any associated hooks.
!> After this function is called, the filter object should no longer be used.
!>
!> @param [inout] THIS   The filter object (`FILTER_LEVELIST_T`) whose resources are to be freed.
!> @param [inout] HOOKS  Structure (`HOOKS_T`) used for additional hooks or callbacks during the deallocation process.
!>
!> @return Integer error code (`RET`) indicating success or failure of the free operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!> @dependency [PROCEDURE] DESTROY_FILTER (*to be implemented)
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILTER_LEVELIST_FREE'
FUNCTION FILTER_LEVELIST_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(FILTER_LEVELIST_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_KEYSET = 1_JPIB_K

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_KEYSET) THIS%LEVELISTS_%FREE( HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_FREE_KEYSET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free keyset' )
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

END FUNCTION FILTER_LEVELIST_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE FILTER_LEVELIST_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME