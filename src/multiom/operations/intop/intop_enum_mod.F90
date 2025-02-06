!>
!> @file intop_enum_mod.F90
!>
!> @brief Module containing definitions and procedures for enumeter operations.
!>
!> This module defines the `INTOP_ENUM_T` type, along with its associated
!> procedures and helper functions that facilitate the creation, management, and
!> utilization of enumeter operations within the system. ENUMeter operations allow for
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


#define PP_FILE_NAME 'intop_enum_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'INTOP_ENUM_MOD'
MODULE INTOP_ENUM_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: INTOP_BASE_MOD,    ONLY: INTOP_BASE_A

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!>
!> @brief A type representing a operation with support for nested operations and matching logic.
!>
!> This derived type extends `INTOP_BASE_A` and is used for operationing based on enumeters.
!> It supports matching, ignoring, or applying thresholds to specific enumeters, and can
!> also utilize a keyset to perform nested operationing.
!>
TYPE, EXTENDS(INTOP_BASE_A) :: INTOP_ENUM_T

  !> Default visibility of the type.
  PRIVATE

  !> Enumerators for field and value
  INTEGER(KIND=JPIB_K) :: FIELD_ID_=-99_JPIB_K
  INTEGER(KIND=JPIB_K) :: VALUE_ID_=-99_JPIB_K

CONTAINS

  !> @brief Initializes the operation enumeter type.
  !> @details This procedure sets up the `INTOP_ENUM_T` type, initializing its components.
  !>
  !> @enum [in] this The instance of `INTOP_ENUM_T` to initialize.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT  => INTOP_ENUM_INIT

  !> @brief Matches a condition against the operation.
  !> @details This procedure checks whether a given condition matches the criteria defined by the operation.
  !>
  !> @enum [in] this The instance of `INTOP_ENUM_T`.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: EVAL => INTOP_ENUM_EVAL

  !> @brief Prints the operation's details for debugging or logging.
  !> @details Outputs the operation's configuration and current state.
  !>
  !> @enum [in] this The instance of `INTOP_ENUM_T` to print.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT => INTOP_ENUM_PRINT

  !> @brief Frees resources allocated for the operation.
  !> @details Cleans up the `INTOP_ENUM_T` type, deallocating any resources used by the operation.
  !>
  !> @enum [in] this The instance of `INTOP_ENUM_T` to free.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE  => INTOP_ENUM_FREE

END TYPE



!> Whitlist of public symbols
PUBLIC :: INTOP_ENUM_T

CONTAINS


!>
!> @brief Initializes the `INTOP_ENUM_T` operation from a YAML configuration.
!>
!> This function initializes the `INTOP_ENUM_T` type based on the provided YAML
!> configuration (`CFG`) and applies any hooks specified in `HOOKS`. The function
!> reads the necessary enumeters from the configuration and sets up the operation structure.
!>
!> @enum [inout] THIS The operation object (`INTOP_ENUM_T`) that will be initialized.
!> @enum [in]    CFG  The YAML configuration object containing the operation settings.
!> @enum [in]    OPT The generic options to be used to initialize the operation.
!> @enum [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during initialization.
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
#define PP_PROCEDURE_NAME 'INTOP_ENUM_INIT'
FUNCTION INTOP_ENUM_INIT( THIS, CFG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_READ_STRING
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_READ_STRING_WITH_ENV_EXPANSION
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_IS_INTEGER
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_TO_INTEGER
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: CMSGINTFLDS2IMSGINTFLDS
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_STREAM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_TYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_CLASS_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_LEVTYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_REPRES_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_PACKING_E
  USE :: ENUMERATORS_MOD, ONLY: CSTREAM2ISTREAM
  USE :: ENUMERATORS_MOD, ONLY: CTYPE2ITYPE
  USE :: ENUMERATORS_MOD, ONLY: CCLASS2ICLASS
  USE :: ENUMERATORS_MOD, ONLY: CPACKING2IPACKING
  USE :: ENUMERATORS_MOD, ONLY: CREPRES2IREPRES
  USE :: ENUMERATORS_MOD, ONLY: CLEVTYPE2ILEVTYPE
  USE :: ENUMERATORS_MOD, ONLY: CSINK2ISINK

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_ENUM_T),    INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_VALUE
  LOGICAL :: HAS_NAME
  LOGICAL :: IS_INTEGER
  CHARACTER(LEN=:), ALLOCATABLE :: CTMP
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: FIELD_ID

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_OPERATION = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_UNDEFINED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_INTEGER = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INTEGER = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_ENUM = 8_JPIB_K


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

  !> Read the enum
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION ) YAML_CONFIGURATION_HAS_KEY( CFG, 'name', HAS_NAME, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_NAME, ERRFLAG_VALUE_UNDEFINED )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION) YAML_READ_STRING( CFG, 'name', CTMP, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CTMP), ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ )

  ! Handle the sink as a special case
  IF ( TRIM(ADJUSTL(CTMP)) .EQ. 'sink' ) THEN
    DEALLOCATE(CTMP, STAT=DEALLOC_STATE, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATE .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )

    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION ) YAML_CONFIGURATION_HAS_KEY( CFG, 'value', HAS_VALUE, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_VALUE, ERRFLAG_VALUE_UNDEFINED )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION) YAML_READ_STRING_WITH_ENV_EXPANSION( CFG, 'value', CTMP, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CTMP), ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ )

    ! Negative for the parametrization
    THIS%FIELD_ID_ = -1_JPIB_K
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_ENUM) CSINK2ISINK( CTMP, THIS%VALUE_ID_, HOOKS )

    ! Deallocate the value
    DEALLOCATE(CTMP, STAT=DEALLOC_STATE, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATE .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )

  ELSE
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_ENUM) CMSGINTFLDS2IMSGINTFLDS( CTMP, THIS%FIELD_ID_, HOOKS )
    DEALLOCATE(CTMP, STAT=DEALLOC_STATE, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATE .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )


    ! TODO: Add the sink type as enumerator.
    !> Read the enum
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION ) YAML_CONFIGURATION_HAS_KEY( CFG, 'value', HAS_VALUE, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_VALUE, ERRFLAG_VALUE_UNDEFINED )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_OPERATION) YAML_READ_STRING_WITH_ENV_EXPANSION( CFG, 'value', CTMP, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CTMP), ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ )

    SELECT CASE (THIS%FIELD_ID_)
    CASE (MSGINTFLD_STREAM_E)
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_ENUM) CSTREAM2ISTREAM( CTMP, THIS%VALUE_ID_, HOOKS )
    CASE (MSGINTFLD_TYPE_E)
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_ENUM) CTYPE2ITYPE( CTMP, THIS%VALUE_ID_, HOOKS )
    CASE (MSGINTFLD_CLASS_E)
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_ENUM) CCLASS2ICLASS( CTMP, THIS%VALUE_ID_, HOOKS )
    CASE (MSGINTFLD_LEVTYPE_E)
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_ENUM) CLEVTYPE2ILEVTYPE( CTMP, THIS%VALUE_ID_, HOOKS )
    CASE (MSGINTFLD_REPRES_E)
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_ENUM) CREPRES2IREPRES( CTMP, THIS%VALUE_ID_, HOOKS )
    CASE (MSGINTFLD_PACKING_E)
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_ENUM) CPACKING2IPACKING( CTMP, THIS%VALUE_ID_, HOOKS )
    CASE DEFAULT
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNABLE_TO_CONVERT_TO_ENUM )
    END SELECT

    ! Deallocate the value
    DEALLOCATE(CTMP, STAT=DEALLOC_STATE, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATE .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )

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
    CASE (ERRFLAG_VALUE_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'value undefined' )
    CASE (ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'type not allocated after read' )
    CASE (ERRFLAG_UNABLE_TO_CONVERT_TO_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to integer' )
    CASE (ERRFLAG_NOT_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'not integer' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate' )
    CASE (ERRFLAG_UNABLE_TO_CONVERT_TO_ENUM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to enum' )
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

END FUNCTION INTOP_ENUM_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Matches a operation enumeter with a message and enumeter.
!>
!> This function checks whether the provided message (`MSG`) and enumeter (`PAR`)
!> match the operation's criteria. If a match is found, the `MATCH` flag is set to `.TRUE.`;
!> otherwise, it is set to `.FALSE.`. Hooks can be applied during the matching process
!> to allow additional processing.
!>
!> @enum [inout] THIS  The operation object (`INTOP_ENUM_T`) used for matching.
!> @enum [in]    MSG   The message (`FORTRAN_MESSAGE_T`) that is checked against the operation.
!> @enum [in]    PAR   The enumeter object (`ENUMETRIZATION_T`) used in the matching process.
!> @enum [out]   MATCH Logical flag indicating whether the message and enumeter match the operation's criteria.
!> @enum [inout] HOOKS A structure (`HOOKS_T`) used for additional hooks or callbacks during matching.
!>
!> @return Integer error code (`RET`) indicating success or failure of the matching process.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section local dependencies
!> @dependency [TYPE] DATAKINDS_DEF_MOD::JPIB_K
!> @dependency [TYPE] ENUMETRIZATION_MOD::ENUMETRIZATION_T
!> @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!> @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!> @dependency [*] PP_DEBUG_USE_VARS::*
!> @dependency [*] PP_LOG_USE_VARS::*
!> @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INTOP_ENUM_EVAL'
FUNCTION INTOP_ENUM_EVAL( THIS, MSG, PAR, RESULT, HOOKS ) RESULT(RET)

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
  CLASS(INTOP_ENUM_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T), INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(IN)    :: PAR
  INTEGER(KIND=JPIB_K),    INTENT(OUT)   :: RESULT
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

  ! Return the result of the operation
  RESULT = THIS%VALUE_ID_

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION INTOP_ENUM_EVAL
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
!> @enum [inout] THIS    The operation object (`INTOP_ENUM_T`) whose details are to be printed.
!> @enum [in]    UNIT    The output unit (file or console) where the operation's details will be printed.
!> @enum [in]    OFFSET  The offset applied to the printed output for formatting purposes.
!> @enum [inout] HOOKS   Structure (`HOOKS_T`) used for additional hooks or callbacks during printing.
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
#define PP_PROCEDURE_NAME 'INTOP_ENUM_PRINT'
FUNCTION INTOP_ENUM_PRINT( THIS, UNIT, OFFSET, HOOKS, SEPARATOR ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: IMSGINTFLDS2CMSGINTFLDS
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_STREAM_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_TYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_CLASS_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_LEVTYPE_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_REPRES_E
  USE :: FORTRAN_MESSAGE_ENUMERATORS_MOD, ONLY: MSGINTFLD_PACKING_E
  USE :: ENUMERATORS_MOD, ONLY: ISTREAM2CSTREAM
  USE :: ENUMERATORS_MOD, ONLY: ITYPE2CTYPE
  USE :: ENUMERATORS_MOD, ONLY: ICLASS2CCLASS
  USE :: ENUMERATORS_MOD, ONLY: IPACKING2CPACKING
  USE :: ENUMERATORS_MOD, ONLY: IREPRES2CREPRES
  USE :: ENUMERATORS_MOD, ONLY: ILEVTYPE2CLEVTYPE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_ENUM_T),        INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: SEPARATOR

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=16) :: FIELD_NAME
  CHARACTER(LEN=32) :: FIELD_VALUE
  CHARACTER(LEN=32) :: TMP

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_NAME = 1_JPIB_K

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


  ! Extract field name
  FIELD_NAME = REPEAT( ' ', 16 )
  PP_TRYCALL(ERRFLAG_CONVERT_TO_NAME) IMSGINTFLDS2CMSGINTFLDS( THIS%FIELD_ID_, FIELD_NAME, HOOKS )

  ! Extract field value
  FIELD_VALUE = REPEAT( ' ', 32 )
  SELECT CASE (THIS%FIELD_ID_)
  CASE (MSGINTFLD_STREAM_E)
    PP_TRYCALL(ERRFLAG_CONVERT_TO_NAME) ISTREAM2CSTREAM( THIS%VALUE_ID_, FIELD_VALUE, HOOKS )
  CASE (MSGINTFLD_TYPE_E)
    PP_TRYCALL(ERRFLAG_CONVERT_TO_NAME) ITYPE2CTYPE( THIS%VALUE_ID_, FIELD_VALUE, HOOKS )
  CASE (MSGINTFLD_CLASS_E)
    PP_TRYCALL(ERRFLAG_CONVERT_TO_NAME) ICLASS2CCLASS( THIS%VALUE_ID_, FIELD_VALUE, HOOKS )
  CASE (MSGINTFLD_LEVTYPE_E)
    PP_TRYCALL(ERRFLAG_CONVERT_TO_NAME) ILEVTYPE2CLEVTYPE( THIS%VALUE_ID_, FIELD_VALUE, HOOKS )
  CASE (MSGINTFLD_REPRES_E)
    PP_TRYCALL(ERRFLAG_CONVERT_TO_NAME) IREPRES2CREPRES( THIS%VALUE_ID_, FIELD_VALUE, HOOKS )
  CASE (MSGINTFLD_PACKING_E)
    PP_TRYCALL(ERRFLAG_CONVERT_TO_NAME) IPACKING2CPACKING( THIS%VALUE_ID_, FIELD_VALUE, HOOKS )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_CONVERT_TO_NAME )
  END SELECT

  !> Print the enum operation
  IF ( PRESENT(SEPARATOR) ) THEN
    WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'enum :: "'//TRIM(ADJUSTL(FIELD_NAME))//'":"'//TRIM(ADJUSTL(FIELD_VALUE))//'"'//TRIM(ADJUSTL(SEPARATOR))
  ELSE
    WRITE(UNIT,'(A)') REPEAT(' ',OFFSET)//'enum :: "'//TRIM(ADJUSTL(FIELD_NAME))//'":"'//TRIM(ADJUSTL(FIELD_VALUE))//'"'
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
    CASE (ERRFLAG_CONVERT_TO_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert enum to name' )
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

END FUNCTION INTOP_ENUM_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees the memory and resources associated with the operation.
!>
!> This function deallocates the memory and resources used by the operation object.
!> It ensures proper cleanup of nested structures and any associated hooks.
!> After this function is called, the operation object should no longer be used.
!>
!> @enum [inout] THIS   The operation object (`INTOP_ENUM_T`) whose resources are to be freed.
!> @enum [inout] HOOKS  Structure (`HOOKS_T`) used for additional hooks or callbacks during the deallocation process.
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
#define PP_PROCEDURE_NAME 'INTOP_ENUM_FREE'
FUNCTION INTOP_ENUM_FREE( THIS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_ENUM_T), INTENT(INOUT) :: THIS
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

  ! Reset members
  THIS%FIELD_ID_ = UNDEF_PARAM_E
  THIS%VALUE_ID_ = UNDEF_PARAM_E

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

END FUNCTION INTOP_ENUM_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE INTOP_ENUM_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME