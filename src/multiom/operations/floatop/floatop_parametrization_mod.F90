!>
!> @file floatop_parametrization_mod.F90
!>
!> @brief Module containing definitions and procedures for parametrizationeter floatops.
!>
!> This module defines the `FLOATOP_PARAMETRIZATION_T` type, along with its associated
!> procedures and helper functions that facilitate the creation, management, and
!> utilization of parametrizationeter floatops within the system. PARAMETRIZATIONeter floatops allow for
!> complex floatoping floatops by combining multiple nested floatops.
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'floatop_parametrization_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'FLOATOP_PARAMETRIZATION_MOD'
MODULE FLOATOP_PARAMETRIZATION_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPIB_K
  USE :: FLOATOP_BASE_MOD, ONLY: FLOATOP_BASE_A

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!>
!> @brief A type representing a floatop with support for nested floatops and matching logic.
!>
!> This derived type extends `FLOATOP_BASE_A` and is used for floatoping based on parametrizationeters.
!> It supports matching, ignoring, or applying thresholds to specific parametrizationeters, and can
!> also utilize a keyset to perform nested floatoping.
!>
TYPE, EXTENDS(FLOATOP_BASE_A) :: FLOATOP_PARAMETRIZATION_T

  !> Default visibility of the type.
  PRIVATE

  INTEGER(KIND=JPIB_K) :: PARAMETRIZATION_ID_ = -99_JPIB_K

CONTAINS

  !> @brief Initializes the floatop parametrizationeter type.
  !> @details This procedure sets up the `FLOATOP_PARAMETRIZATION_T` type, initializing its components.
  !>
  !> @parametrization [in] this The instance of `FLOATOP_PARAMETRIZATION_T` to initialize.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT  => FLOATOP_PARAMETRIZATION_INIT

  !> @brief Matches a condition against the floatop.
  !> @details This procedure checks whether a given condition matches the criteria defined by the floatop.
  !>
  !> @parametrization [in] this The instance of `FLOATOP_PARAMETRIZATION_T`.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: EVAL => FLOATOP_PARAMETRIZATION_EVAL

  !> @brief Prints the floatop's details for debugging or logging.
  !> @details Outputs the floatop's configuration and current state.
  !>
  !> @parametrization [in] this The instance of `FLOATOP_PARAMETRIZATION_T` to print.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT => FLOATOP_PARAMETRIZATION_PRINT

  !> @brief Frees resources allocated for the floatop.
  !> @details Cleans up the `FLOATOP_PARAMETRIZATION_T` type, deallocating any resources used by the floatop.
  !>
  !> @parametrization [in] this The instance of `FLOATOP_PARAMETRIZATION_T` to free.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE  => FLOATOP_PARAMETRIZATION_FREE

END TYPE



!> Whitlist of public symbols
PUBLIC :: FLOATOP_PARAMETRIZATION_T

CONTAINS


!>
!> @brief Initializes the `FLOATOP_PARAMETRIZATION_T` floatop from a YAML configuration.
!>
!> This function initializes the `FLOATOP_PARAMETRIZATION_T` type based on the provided YAML
!> configuration (`CFG`) and applies any hooks specified in `HOOKS`. The function
!> reads the necessary parametrizationeters from the configuration and sets up the floatop structure.
!>
!> @parametrization [inout] THIS The floatop object (`FLOATOP_PARAMETRIZATION_T`) that will be initialized.
!> @parametrization [in]    CFG  The YAML configuration object containing the floatop settings.
!> @parametrization [in]    OPT The generic options to be used to initialize the floatop.
!> @parametrization [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during initialization.
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
#define PP_PROCEDURE_NAME 'FLOATOP_PARAMETRIZATION_INIT'
FUNCTION FLOATOP_PARAMETRIZATION_INIT( THIS, CFG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,             ONLY: FUN_C2I_IF
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_READ_INTEGER_WITH_FILTER
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: CPARINTFLDS2IPARINTFLDS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FLOATOP_PARAMETRIZATION_T),    INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local parametrizations
  PROCEDURE(FUN_C2I_IF), POINTER :: P_CPARINTFLDS2IPARINTFLDS
  LOGICAL :: HAS_SOURCE
  LOGICAL :: HAS_NAME


  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FLOATOP=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NAME_UNDEFINED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FIELD=4_JPIB_K


  ! Local parametrizations declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local parametrizations declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local parametrizations declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  !> Read the parametrization source
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_FLOATOP ) YAML_CONFIGURATION_HAS_KEY( CFG, 'name', HAS_NAME, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_NAME, ERRFLAG_NAME_UNDEFINED )

  !> Read the parametrization name
  P_CPARINTFLDS2IPARINTFLDS => CPARINTFLDS2IPARINTFLDS
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_FIELD) YAML_READ_INTEGER_WITH_FILTER( CFG, 'name', THIS%PARAMETRIZATION_ID_, P_CPARINTFLDS2IPARINTFLDS, HOOKS )

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

    ! Error handling parametrizations
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_READ_FLOATOP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read floatop' )
    CASE (ERRFLAG_NAME_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'name undefined' )
    CASE (ERRFLAG_UNABLE_TO_READ_FIELD)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read operation' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error parametrization and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FLOATOP_PARAMETRIZATION_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Matches a floatop parametrizationeter with a parametrization and parametrizationeter.
!>
!> This function checks whether the provided parametrization (`MSG`) and parametrizationeter (`PAR`)
!> match the floatop's criteria. If a match is found, the `MATCH` flag is set to `.TRUE.`;
!> otherwise, it is set to `.FALSE.`. Hooks can be applied during the matching process
!> to allow additional processing.
!>
!> @parametrization [inout] THIS  The floatop object (`FLOATOP_PARAMETRIZATION_T`) used for matching.
!> @parametrization [in]    MSG   The parametrization (`FORTRAN_MESSAGE_T`) that is checked against the floatop.
!> @parametrization [in]    PAR   The parametrizationeter object (`PARAMETRIZATIONETRIZATION_T`) used in the matching process.
!> @parametrization [out]   MATCH Logical flag indicating whether the parametrization and parametrizationeter match the floatop's criteria.
!> @parametrization [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during matching.
!>
!> @return Integer error code (`RET`) indicating success or failure of the matching process.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] PARAMETRIZATIONETRIZATION_MOD::PARAMETRIZATIONETRIZATION_T
!> @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FLOATOP_PARAMETRIZATION_EVAL'
FUNCTION FLOATOP_PARAMETRIZATION_EVAL( THIS, MSG, PAR, RESULT, HOOKS ) RESULT(RET)

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
  CLASS(FLOATOP_PARAMETRIZATION_T), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),          INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),          INTENT(IN)    :: PAR
  REAL(KIND=JPRD_K),                INTENT(OUT)   :: RESULT
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_PARAMETRIZATION_FIELD=1_JPIB_K

  ! Local parametrizations declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local parametrizations declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local parametrizations declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Return the result of the floatop
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_PARAMETRIZATION_FIELD) PAR%GET( THIS%PARAMETRIZATION_ID_, RESULT, HOOKS )

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

    ! Error handling parametrizations
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_GET_PARAMETRIZATION_FIELD)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get parametrization field' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error parametrization and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FLOATOP_PARAMETRIZATION_EVAL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Prints the floatop's configuration and details to the specified output unit.
!>
!> This function prints the details of the floatop to the output specified by the `UNIT`.
!> An `OFFSET` can be applied to format the output, and `HOOKS` can be used for additional
!> callbacks during the printing process. The printed output includes details about
!> the floatop's current configuration.
!>
!> @parametrization [inout] THIS    The floatop object (`FLOATOP_PARAMETRIZATION_T`) whose details are to be printed.
!> @parametrization [in]    UNIT    The output unit (file or console) where the floatop's details will be printed.
!> @parametrization [in]    OFFSET  The offset applied to the printed output for formatting purposes.
!> @parametrization [inout] HOOKS   Structure (`HOOKS_T`) used for additional hooks or callbacks during printing.
!>
!> @return Integer error code (`RET`) indicating success or failure of the print floatop.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!> @dependency [PROCEDURE] DESTROY_FLOATOP (*to be implemented)
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FLOATOP_PARAMETRIZATION_PRINT'
FUNCTION FLOATOP_PARAMETRIZATION_PRINT( THIS, UNIT, OFFSET, HOOKS, SEPARATOR ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: PARAMETRIZATION_ENUMERATORS_MOD, ONLY: IPARFLOATFLDS2CPARFLOATFLDS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FLOATOP_PARAMETRIZATION_T),    INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: SEPARATOR

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local parametrizations
  CHARACTER(LEN=64) :: PARAMETRIZATION_NAME

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_ENUM_TO_STRING = 1_JPIB_K

  ! Local parametrizations declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local parametrizations declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local parametrizations declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  !> Initialize local parametrizations
  PARAMETRIZATION_NAME = REPEAT(' ',64)

  !> Convert IDs to names
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_ENUM_TO_STRING) IPARFLOATFLDS2CPARFLOATFLDS( THIS%PARAMETRIZATION_ID_, PARAMETRIZATION_NAME, HOOKS )

  !> Print the parametrization floatop
  IF ( PRESENT(SEPARATOR) ) THEN
    WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'parametrization :: par:'//TRIM(ADJUSTL(PARAMETRIZATION_NAME))//TRIM(ADJUSTL(SEPARATOR))
  ELSE
    WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'parametrization :: par:'//TRIM(ADJUSTL(PARAMETRIZATION_NAME))
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

    ! Error handling parametrizations
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

    ! Write the error parametrization and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION FLOATOP_PARAMETRIZATION_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees the memory and resources associated with the floatop.
!>
!> This function deallocates the memory and resources used by the floatop object.
!> It ensures proper cleanup of nested structures and any associated hooks.
!> After this function is called, the floatop object should no longer be used.
!>
!> @parametrization [inout] THIS   The floatop object (`FLOATOP_PARAMETRIZATION_T`) whose resources are to be freed.
!> @parametrization [inout] HOOKS  Structure (`HOOKS_T`) used for additional hooks or callbacks during the deallocation process.
!>
!> @return Integer error code (`RET`) indicating success or failure of the free floatop.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!> @dependency [PROCEDURE] DESTROY_FLOATOP (*to be implemented)
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FLOATOP_PARAMETRIZATION_FREE'
FUNCTION FLOATOP_PARAMETRIZATION_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(FLOATOP_PARAMETRIZATION_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local parametrizations declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local parametrizations declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local parametrizations declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION FLOATOP_PARAMETRIZATION_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE FLOATOP_PARAMETRIZATION_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME