!>
!> @file intop_message_mod.F90
!>
!> @brief Module containing definitions and procedures for messageeter intops.
!>
!> This module defines the `INTOP_MESSAGE_T` type, along with its associated
!> procedures and helper functions that facilitate the creation, management, and
!> utilization of messageeter intops within the system. MESSAGEeter intops allow for
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


#define PP_FILE_NAME 'intop_message_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'INTOP_MESSAGE_MOD'
MODULE INTOP_MESSAGE_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPIB_K
  USE :: INTOP_BASE_MOD, ONLY: INTOP_BASE_A

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!>
!> @brief A type representing a intop with support for nested intops and matching logic.
!>
!> This derived type extends `INTOP_BASE_A` and is used for intoping based on messageeters.
!> It supports matching, ignoring, or applying thresholds to specific messageeters, and can
!> also utilize a keyset to perform nested intoping.
!>
TYPE, EXTENDS(INTOP_BASE_A) :: INTOP_MESSAGE_T

  !> Default visibility of the type.
  PRIVATE

  INTEGER(KIND=JPIB_K) :: MESSAGE_ID_ = -99_JPIB_K

CONTAINS

  !> @brief Initializes the intop messageeter type.
  !> @details This procedure sets up the `INTOP_MESSAGE_T` type, initializing its components.
  !>
  !> @message [in] this The instance of `INTOP_MESSAGE_T` to initialize.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT  => INTOP_MESSAGE_INIT

  !> @brief Matches a condition against the intop.
  !> @details This procedure checks whether a given condition matches the criteria defined by the intop.
  !>
  !> @message [in] this The instance of `INTOP_MESSAGE_T`.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: EVAL => INTOP_MESSAGE_EVAL

  !> @brief Prints the intop's details for debugging or logging.
  !> @details Outputs the intop's configuration and current state.
  !>
  !> @message [in] this The instance of `INTOP_MESSAGE_T` to print.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT => INTOP_MESSAGE_PRINT

  !> @brief Frees resources allocated for the intop.
  !> @details Cleans up the `INTOP_MESSAGE_T` type, deallocating any resources used by the intop.
  !>
  !> @message [in] this The instance of `INTOP_MESSAGE_T` to free.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE  => INTOP_MESSAGE_FREE

END TYPE



!> Whitlist of public symbols
PUBLIC :: INTOP_MESSAGE_T

CONTAINS


!>
!> @brief Initializes the `INTOP_MESSAGE_T` intop from a YAML configuration.
!>
!> This function initializes the `INTOP_MESSAGE_T` type based on the provided YAML
!> configuration (`CFG`) and applies any hooks specified in `HOOKS`. The function
!> reads the necessary messageeters from the configuration and sets up the intop structure.
!>
!> @message [inout] THIS The intop object (`INTOP_MESSAGE_T`) that will be initialized.
!> @message [in]    CFG  The YAML configuration object containing the intop settings.
!> @message [in]    OPT The generic options to be used to initialize the intop.
!> @message [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during initialization.
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
#define PP_PROCEDURE_NAME 'INTOP_MESSAGE_INIT'
FUNCTION INTOP_MESSAGE_INIT( THIS, CFG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,             ONLY: FUN_C2I_IF
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_READ_INTEGER_WITH_FILTER
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: CMSGINTFLDS2IMSGINTFLDS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_MESSAGE_T),    INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local messages
  PROCEDURE(FUN_C2I_IF), POINTER :: P_CDATASTRUCT2IDATASTRUCT
  PROCEDURE(FUN_C2I_IF), POINTER :: P_CMSGINTFLDS2IMSGINTFLDS
  LOGICAL :: HAS_SOURCE
  LOGICAL :: HAS_NAME


  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_INTOP=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NAME_UNDEFINED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FIELD=4_JPIB_K


  ! Local messages declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local messages declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local messages declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  !> Read the message source
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_INTOP ) YAML_CONFIGURATION_HAS_KEY( CFG, 'name', HAS_NAME, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_NAME, ERRFLAG_NAME_UNDEFINED )

  !> Read the message name
  P_CMSGINTFLDS2IMSGINTFLDS => CMSGINTFLDS2IMSGINTFLDS
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_FIELD) YAML_READ_INTEGER_WITH_FILTER( CFG, 'name', THIS%MESSAGE_ID_, P_CMSGINTFLDS2IMSGINTFLDS, HOOKS )

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

    ! Error handling messages
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_READ_INTOP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read intop' )
    CASE (ERRFLAG_NAME_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'name undefined' )
    CASE (ERRFLAG_UNABLE_TO_READ_FIELD)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read operation' )
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

END FUNCTION INTOP_MESSAGE_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Matches a intop messageeter with a message and messageeter.
!>
!> This function checks whether the provided message (`MSG`) and messageeter (`PAR`)
!> match the intop's criteria. If a match is found, the `MATCH` flag is set to `.TRUE.`;
!> otherwise, it is set to `.FALSE.`. Hooks can be applied during the matching process
!> to allow additional processing.
!>
!> @message [inout] THIS  The intop object (`INTOP_MESSAGE_T`) used for matching.
!> @message [in]    MSG   The message (`FORTRAN_MESSAGE_T`) that is checked against the intop.
!> @message [in]    PAR   The messageeter object (`MESSAGEETRIZATION_T`) used in the matching process.
!> @message [out]   MATCH Logical flag indicating whether the message and messageeter match the intop's criteria.
!> @message [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during matching.
!>
!> @return Integer error code (`RET`) indicating success or failure of the matching process.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] MESSAGEETRIZATION_MOD::MESSAGEETRIZATION_T
!> @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INTOP_MESSAGE_EVAL'
FUNCTION INTOP_MESSAGE_EVAL( THIS, MSG, PAR, RESULT, HOOKS ) RESULT(RET)

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
  CLASS(INTOP_MESSAGE_T), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T), INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(IN)    :: PAR
  INTEGER(KIND=JPIB_K),    INTENT(OUT)   :: RESULT
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_MESSAGE_FIELD=1_JPIB_K

  ! Local messages declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local messages declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local messages declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Return the result of the intop
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_MESSAGE_FIELD) MSG%GET( THIS%MESSAGE_ID_, RESULT, HOOKS )

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

    ! Error handling messages
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_GET_MESSAGE_FIELD)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get message field' )
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

END FUNCTION INTOP_MESSAGE_EVAL
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
!> @message [inout] THIS    The intop object (`INTOP_MESSAGE_T`) whose details are to be printed.
!> @message [in]    UNIT    The output unit (file or console) where the intop's details will be printed.
!> @message [in]    OFFSET  The offset applied to the printed output for formatting purposes.
!> @message [inout] HOOKS   Structure (`HOOKS_T`) used for additional hooks or callbacks during printing.
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
#define PP_PROCEDURE_NAME 'INTOP_MESSAGE_PRINT'
FUNCTION INTOP_MESSAGE_PRINT( THIS, UNIT, OFFSET, HOOKS, SEPARATOR ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: IMSGINTFLDS2CMSGINTFLDS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_MESSAGE_T),    INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: SEPARATOR

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local messages
  CHARACTER(LEN=16) :: MESSAGE_NAME

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_ENUM_TO_STRING = 1_JPIB_K

  ! Local messages declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local messages declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local messages declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  !> Initialize local messages
  MESSAGE_NAME = REPEAT(' ',16)

  !> Convert IDs to names
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_ENUM_TO_STRING) IMSGINTFLDS2CMSGINTFLDS( THIS%MESSAGE_ID_, MESSAGE_NAME, HOOKS )

  !> Print the message intop
  IF ( PRESENT(SEPARATOR) ) THEN
    WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'message :: msg:'//TRIM(ADJUSTL(MESSAGE_NAME))//TRIM(ADJUSTL(SEPARATOR))
  ELSE
    WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'message :: msg:'//TRIM(ADJUSTL(MESSAGE_NAME))
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

    ! Error handling messages
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
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION INTOP_MESSAGE_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees the memory and resources associated with the intop.
!>
!> This function deallocates the memory and resources used by the intop object.
!> It ensures proper cleanup of nested structures and any associated hooks.
!> After this function is called, the intop object should no longer be used.
!>
!> @message [inout] THIS   The intop object (`INTOP_MESSAGE_T`) whose resources are to be freed.
!> @message [inout] HOOKS  Structure (`HOOKS_T`) used for additional hooks or callbacks during the deallocation process.
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
#define PP_PROCEDURE_NAME 'INTOP_MESSAGE_FREE'
FUNCTION INTOP_MESSAGE_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(INTOP_MESSAGE_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local messages declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local messages declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local messages declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION INTOP_MESSAGE_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE INTOP_MESSAGE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME