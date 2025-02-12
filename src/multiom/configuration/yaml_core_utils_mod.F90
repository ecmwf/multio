!>
!> @file yaml_core_utils_mod.f90
!>
!> @brief Provides utility routines for handling YAML configurations.
!>
!> This module contains various routines for managing YAML configurations, including reading,
!> creating, and manipulating configurations from files or memory. It supports functionalities such
!> as retrieving, deleting, and checking configurations, as well as handling errors and debugging.
!>
!> @section Public DataTypes
!> The module defines the following types:
!>  - @ref YAML_CONFIGURATION_T
!>  - @ref YAML_CONFIGURATIONS_T
!>
!> @section Public Interfaces
!> The module includes the following interfaces:
!>  - @ref FUN_C2I_IF
!>
!> @section Public Routines
!> The module includes the following procedures:
!>   - @ref YAML_NEW_CONFIGURATION_FROM_FILE
!>   - @ref YAML_DELETE_CONFIGURATION
!>   - @ref YAML_DELETE_CONFIGURATIONS
!>   - @ref YAML_GET_SUBCONFIGURATION
!>   - @ref YAML_GET_SUBCONFIGURATIONS
!>   - @ref YAML_GET_CONFIGURATIONS_SIZE
!>   - @ref YAML_GET_CONFIGURATION_BY_ID
!>   - @ref YAML_CONFIGURATION_HAS_KEY
!>   - @ref YAML_READ_STRING_ARRAY
!>   - @ref YAML_READ_INTEGER_ARRAY_WITH_FILTER
!>   - @ref YAML_READ_INTEGER_ARRAY_WITH_RANGES
!>   - @ref YAML_READ_INTEGER_ARRAY
!>   - @ref YAML_READ_FLOAT
!>   - @ref YAML_READ_INTEGER
!>   - @ref YAML_READ_STRING
!>   - @ref YAML_READ_LOGICAL
!>
!> @section Private Routines
!> The module includes the following procedures:
!>   - @ref READ_INTEGER_PATTERNS
!>   - @ref READ_INTEGER
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>


! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'yaml_core_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'YAML_CORE_UTILS_MOD'
MODULE YAML_CORE_UTILS_MOD

  !> Simbols imported from external libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

IMPLICIT NONE

!> Default symbols visibility
PRIVATE

!>
!> @class main object representing a YAML configuration
TYPE :: YAML_CONFIGURATION_T

  !> @brief Flag used to check if the configuration is allocated of just a reference
  LOGICAL :: IS_ALLOCATED_ = .FALSE.

  !> @brief Pointer to the actual configuration object
  TYPE(FCKIT_CONFIGURATION), POINTER :: CFG_ => NULL()

ENDTYPE


!>
!> @class object representing an array of YAML configurations
TYPE :: YAML_CONFIGURATIONS_T

  !> @brief Pointer to the array of configurations
  TYPE(FCKIT_CONFIGURATION), DIMENSION(:), ALLOCATABLE :: CFGS_
ENDTYPE


!>
!> Interface for the function converting a characters to an integer
!> To be used for example when reading anumerators by name
INTERFACE
  PP_THREAD_SAFE FUNCTION FUN_C2I_IF( CHAR, I, HOOKS ) RESULT( RET )

    ! Symbols imported from other modules within the project.
    USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
    USE :: HOOKS_MOD,         ONLY: HOOKS_T

    ! Symbols imported by the preprocessor for debugging purposes
    PP_DEBUG_USE_VARS

    ! Symbols imported by the preprocessor for logging purposes
    PP_LOG_USE_VARS

    ! Symbols imported by the preprocessor for tracing purposes
    PP_TRACE_USE_VARS

  IMPLICIT NONE

    ! Dummy arguments
    CHARACTER(LEN=*),     INTENT(IN)    :: CHAR
    INTEGER(KIND=JPIB_K), INTENT(OUT)   :: I
    TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS
    INTEGER(KIND=JPIB_K) :: RET
  END FUNCTION FUN_C2I_IF
END INTERFACE


!>
!> Public symbols visibility (datatypes)
PUBLIC :: YAML_CONFIGURATION_T
PUBLIC :: YAML_CONFIGURATIONS_T

!> Public symbols visibility (interfaces)
PUBLIC :: FUN_C2I_IF

!> Public symbols visibility (subroutines)
PUBLIC :: YAML_NEW_CONFIGURATION_FROM_FILE
PUBLIC :: YAML_DELETE_CONFIGURATION
PUBLIC :: YAML_DELETE_CONFIGURATIONS
PUBLIC :: YAML_GET_SUBCONFIGURATION
PUBLIC :: YAML_GET_SUBCONFIGURATIONS
PUBLIC :: YAML_GET_CONFIGURATIONS_SIZE
PUBLIC :: YAML_GET_CONFIGURATION_BY_ID
PUBLIC :: YAML_CONFIGURATION_HAS_KEY
PUBLIC :: YAML_READ_STRING_ARRAY
PUBLIC :: YAML_READ_INTEGER_ARRAY_WITH_RANGES
PUBLIC :: YAML_READ_INTEGER_ARRAY_WITH_FILTER
PUBLIC :: YAML_READ_INTEGER_KEYSET_WITH_RANGES
PUBLIC :: YAML_READ_INTEGER_KEYSET_WITH_FILTER
PUBLIC :: YAML_READ_INTEGER_ARRAY
PUBLIC :: YAML_READ_FLOAT_ARRAY
PUBLIC :: YAML_READ_FLOAT
PUBLIC :: YAML_READ_INTEGER
PUBLIC :: YAML_READ_INTEGER_WITH_FILTER
PUBLIC :: YAML_READ_STRING
PUBLIC :: YAML_READ_STRING_WITH_ENV_EXPANSION
PUBLIC :: YAML_READ_LOGICAL


CONTAINS


!>
!> @brief Creates a new YAML configuration from a file.
!>
!> This function reads a YAML file specified by `YAML_FILE_NAME` and creates a new YAML configuration,
!> which is then returned in the `VALUE` argument.
!>
!> @section interface
!> @param [in] YAML_FILE_NAME The name of the YAML file from which the configuration will be read.
!> @param [out] VALUE The new YAML configuration object created from the file.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating the result of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE] YAML_CONFIGURATION_T
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection external dependencies
!>   - @dependency [PROCEDURE] FCKIT_PATHNAME_MODULE::FCKIT_PATHNAME
!>   - @dependency [PROCEDURE] FCKIT_CONFIGURATION_MODULE::FCKIT_YAMLCONFIGURATION
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_NEW_CONFIGURATION_FROM_FILE'
PP_THREAD_SAFE FUNCTION YAML_NEW_CONFIGURATION_FROM_FILE( YAML_FILE_NAME, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
  USE  :: FCKIT_PATHNAME_MODULE,      ONLY: FCKIT_PATHNAME
  USE  :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_YAMLCONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),           INTENT(IN)    :: YAML_FILE_NAME
  TYPE(YAML_CONFIGURATION_T), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: FILE_EXISTS
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_ALLOCATED_FLAG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_YAML_FILE_NOT_PRESENT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_ALLOCATE_ERROR=4_JPIB_K

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

  ! Erro handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(VALUE%CFG_), 1 )
  PP_DEBUG_CRITICAL_COND_THROW( VALUE%IS_ALLOCATED_, 2 )

  ! Check if the file exsts
  INQUIRE( FILE=TRIM(YAML_FILE_NAME), EXIST=FILE_EXISTS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.FILE_EXISTS, 1 )

  ! Allocate the configuration
  ALLOCATE( VALUE%CFG_, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, 1 )
  VALUE%IS_ALLOCATED_ = .TRUE.

  ! Load the configuration
!$omp critical(FCKIT_YAMLCONFIGURATION)
  VALUE%CFG_ = FCKIT_YAMLCONFIGURATION( FCKIT_PATHNAME( TRIM(YAML_FILE_NAME) ) )
!$omp end critical(FCKIT_YAMLCONFIGURATION)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration already allocated' )
    CASE (ERRFLAG_CFG_ALLOCATED_FLAG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'IS_ALLOCATED flag inconsistent with pointer allocation status' )
    CASE (ERRFLAG_YAML_FILE_NOT_PRESENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find the YAML file to be read: '//TRIM(ADJUSTL(YAML_FILE_NAME)) )
    CASE (ERRFLAG_CFG_ALLOCATE_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating configuration' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating configuration: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_NEW_CONFIGURATION_FROM_FILE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Deletes a configuration from a YAML configuration object.
!>
!> @section interface
!> @param [inout] CFG The YAML configuration object from which the configuration will be deleted.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating the result of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE] YAML_CONFIGURATION_T
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see YAML_DELETE_CONFIGURATIONS
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_DELETE_CONFIGURATION'
PP_THREAD_SAFE FUNCTION YAML_DELETE_CONFIGURATION( CFG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(INOUT) :: CFG
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOC_CONFIGURATION=1_JPIB_K

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

  IF ( ASSOCIATED(CFG%CFG_) ) THEN
    IF ( CFG%IS_ALLOCATED_ ) THEN
!$omp critical(FCKIT_YAMLCONFIGURATION)
      CALL CFG%CFG_%FINAL()
!$omp end critical(FCKIT_YAMLCONFIGURATION)
      DEALLOCATE( CFG%CFG_, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOC_CONFIGURATION )
      NULLIFY( CFG%CFG_ )
    ELSE
      NULLIFY( CFG%CFG_ )
    ENDIF
    CFG%IS_ALLOCATED_ = .FALSE.
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_DEALLOC_CONFIGURATION)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating CONFIGURATIONS' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating CONFIGURATIONS: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_DELETE_CONFIGURATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Deletes configurations from a YAML configurations object.
!>
!> @section interface
!> @param [inout] CFG The YAML configurations object from which configurations will be deleted.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating the result of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE] YAML_CONFIGURATIONS_T
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection external dependencies
!>   - @dependency [PROCEDURE] FCKIT_CONFIGURATION_MODULE::DEALLOCATE_FCKIT_CONFIGURATION
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see YAML_DELETE_CONFIGURATION
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_DELETE_CONFIGURATIONS'
PP_THREAD_SAFE FUNCTION YAML_DELETE_CONFIGURATIONS( CFG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: DEALLOCATE_FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATIONS_T), INTENT(INOUT) :: CFG
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS

  ! Function return value
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

  IF ( ALLOCATED(CFG%CFGS_) ) THEN
!$omp critical(FCKIT_YAMLCONFIGURATION)
    CALL DEALLOCATE_FCKIT_CONFIGURATION( CFG%CFGS_ )
!$omp end critical(FCKIT_YAMLCONFIGURATION)
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_DELETE_CONFIGURATIONS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Retrieves a sub-configuration from a YAML configuration.
!>
!> This function extracts a specific sub-configuration from the provided YAML configuration object (`CFG`)
!> based on the given `KEY`. The extracted sub-configuration is stored in `VALUE`.
!>
!> @section interface
!> @param [in] CFG The main YAML configuration from which the sub-configuration is extracted.
!> @param [out] KEY The key identifying the sub-configuration within `CFG`.
!> @param [out] VALUE The sub-configuration extracted from `CFG` corresponding to the `KEY`.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE] YAML_CONFIGURATION_T
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_GET_SUBCONFIGURATION'
PP_THREAD_SAFE FUNCTION YAML_GET_SUBCONFIGURATION( CFG, KEY, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  CHARACTER(LEN=*),           INTENT(IN)    :: KEY
  TYPE(YAML_CONFIGURATION_T), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: EX
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_ALLOCATE_ERROR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SUBCCONFIGURATION=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(VALUE%CFG_), ERRFLAG_CFG_ALREADY_ALLOCATED )

  ! Initialize the output object
  VALUE%CFG_ => NULL()
  VALUE%IS_ALLOCATED_ = .FALSE.

  ! Allocate the configuration
  ALLOCATE( VALUE%CFG_, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_CFG_ALLOCATE_ERROR )

  ! Read the string
!$omp critical(FCKIT_YAMLCONFIGURATION)
  EX = CFG%CFG_%GET( KEY, VALUE%CFG_ )
!$omp end critical(FCKIT_YAMLCONFIGURATION)

  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_UNABLE_TO_SUBCCONFIGURATION )
  VALUE%IS_ALLOCATED_ = .TRUE.

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration already associated' )
    CASE (ERRFLAG_CFG_ALLOCATE_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating configuration' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating configuration: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_SUBCCONFIGURATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read subconfiguration' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_GET_SUBCONFIGURATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Retrieves sub-configurations from a YAML configuration.
!>
!> This function extracts sub-configurations from the provided YAML configuration object (`CFG`),
!> based on the specified `KEY`. The extracted sub-configurations are stored in `VALUE`.
!>
!> @section interface
!> @param [in] CFG The main YAML configuration from which sub-configurations are extracted.
!> @param [out] KEY The key used to identify the sub-configurations within `CFG`.
!> @param [out] VALUE The sub-configurations extracted from `CFG` based on the `KEY`.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE] YAML_CONFIGURATION_T
!>   - @dependency [TYPE] YAML_CONFIGURATIONS_T
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_GET_SUBCONFIGURATIONS'
PP_THREAD_SAFE FUNCTION YAML_GET_SUBCONFIGURATIONS( CFG, KEY, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),  INTENT(IN)    :: CFG
  CHARACTER(LEN=*),            INTENT(IN)    :: KEY
  TYPE(YAML_CONFIGURATIONS_T), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: EX

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFGS_ALREADY_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCONFIGURATIONS=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(VALUE%CFGS_), ERRFLAG_CFGS_ALREADY_ALLOCATED )

  ! Read the string
!$omp critical(FCKIT_YAMLCONFIGURATION)
  EX = CFG%CFG_%GET( KEY, VALUE%CFGS_ )
!$omp end critical(FCKIT_YAMLCONFIGURATION)

  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_UNABLE_TO_READ_SUBCONFIGURATIONS )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration not allocated' )
    CASE (ERRFLAG_CFGS_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration already allocated' )
    CASE (ERRFLAG_UNABLE_TO_READ_SUBCONFIGURATIONS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read subconfigurations' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_GET_SUBCONFIGURATIONS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Retrieves the size of the YAML configurations array.
!>
!> This function calculates the size of the given array of YAML configurations (`CFG`)
!> and returns the result in `CFG_SIZE`.
!>
!> @section interface
!> @param [inout] CFG The array of YAML configurations whose size is being calculated.
!> @param [out] CFG_SIZE The size of the YAML configurations array.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating the outcome of the function.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE] YAML_CONFIGURATIONS_T
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @section Error codes:
!>   - `ERRFLAG_CFG_ARRAY_EMPTY` (301): The configurations array is empty.
!>   - `ERRFLAG_CFG_ARRAY_CORRUPT` (302): The configurations array is corrupted or invalid.
!>
!> @see YAML_GET_CONFIGURATION_BY_ID
!>

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_GET_CONFIGURATIONS_SIZE'
PP_THREAD_SAFE FUNCTION YAML_GET_CONFIGURATIONS_SIZE( CFG, CFG_SIZE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATIONS_T), INTENT(IN)    :: CFG
  INTEGER(KIND=JPIB_K),        INTENT(OUT)   :: CFG_SIZE
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFGS_NOT_ALLOCATED=1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CFG%CFGS_), ERRFLAG_CFGS_NOT_ALLOCATED )

  ! Get the configuration size
  CFG_SIZE = SIZE(CFG%CFGS_)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFGS_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration not allocated' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_GET_CONFIGURATIONS_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Retrieves a specific YAML configuration by its identifier.
!>
!> This subroutine retrieves a YAML configuration from a list of configurations (`CFG`)
!> using a specified configuration ID (`CFG_ID`). The retrieved configuration is stored
!> in `CURR_CFG`.
!>
!> @section interface
!> @param [inout] CFG The list of YAML configurations, from which one configuration is selected.
!> @param [out] CFG_ID The identifier of the YAML configuration to retrieve.
!> @param [out] CURR_CFG The YAML configuration that is retrieved based on `CFG_ID`.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating the outcome of the subroutine.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this subroutine:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE] YAML_CONFIGURATION_T
!>   - @dependency [TYPE] YAML_CONFIGURATIONS_T
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see YAML_GET_CONFIGURATIONS_SIZE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_GET_CONFIGURATION_BY_ID'
PP_THREAD_SAFE FUNCTION YAML_GET_CONFIGURATION_BY_ID( CFG, CFG_ID, CURR_CFG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATIONS_T), TARGET, INTENT(IN)    :: CFG
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: CFG_ID
  TYPE(YAML_CONFIGURATION_T),          INTENT(OUT)   :: CURR_CFG
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFGS_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS_UB=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS_LB=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CFG%CFGS_), ERRFLAG_CFGS_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( CFG_ID.GT.SIZE(CFG%CFGS_), ERRFLAG_OUT_OF_BOUNDS_UB )
  PP_DEBUG_CRITICAL_COND_THROW( CFG_ID.LT.1, ERRFLAG_OUT_OF_BOUNDS_LB )

  ! Initialize the output object
  CURR_CFG%CFG_ => NULL()
  CURR_CFG%IS_ALLOCATED_ = .FALSE.

  ! Get the configuration
  CURR_CFG%CFG_ => CFG%CFGS_(CFG_ID)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFGS_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration not allocated' )
    CASE (ERRFLAG_OUT_OF_BOUNDS_UB)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Id out of bounds, bigger than the upper bound' )
    CASE (ERRFLAG_OUT_OF_BOUNDS_LB)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Id out of bounds, lower than 1' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_GET_CONFIGURATION_BY_ID
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Checks if a specified key exists in a YAML configuration.
!>
!> This function checks if a given key (`KEY`) exists in the YAML configuration object (`CFG`).
!> It returns a logical flag (`HAS_KEY`) indicating whether the key was found.
!>
!> @section interface
!> @param [in] CFG The YAML configuration object in which the key is searched.
!> @param [in] KEY The key to search for in the YAML configuration.
!> @param [out] HAS_KEY Logical flag that indicates whether the key exists (`.TRUE.` if it exists).
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating the outcome of the function.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE] YAML_CONFIGURATION_T
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_CONFIGURATION_HAS_KEY'
PP_THREAD_SAFE FUNCTION YAML_CONFIGURATION_HAS_KEY( CFG, KEY, HAS_KEY, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  CHARACTER(LEN=*),           INTENT(IN)    :: KEY
  LOGICAL,                    INTENT(OUT)   :: HAS_KEY
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )

  ! Read the string
!$omp critical(FCKIT_YAMLCONFIGURATION)
  HAS_KEY = CFG%CFG_%HAS( KEY )
!$omp end critical(FCKIT_YAMLCONFIGURATION)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration not allocated' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_CONFIGURATION_HAS_KEY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads a string array from a YAML configuration.
!>
!> This function reads a string array from a provided YAML configuration object (`CFG`)
!> using the specified key (`KEY`). The `VALUE` array is populated with the parsed strings.
!> If an error occurs during the process, the function returns an error code.
!>
!> @section interface
!> @param [in] CFG The YAML configuration object from which the string array is read.
!> @param [in] KEY The key corresponding to the string array in the YAML configuration.
!> @param [out] VALUE The string array to be populated with the parsed values.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating the outcome of the function.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE] YAML_CONFIGURATION_T
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see YAML_READ_FLOAT
!> @see YAML_READ_INTEGER
!> @see YAML_READ_LOGICAL
!> @see YAML_READ_STRING_ARRAY
!> @see YAML_READ_INTEGER_ARRAY
!> @see YAML_READ_INTEGER_ARRAY_WITH_RANGES
!> @see YAML_READ_INTEGER_ARRAY_WITH_FILTER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_READ_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION YAML_READ_STRING_ARRAY( CFG, KEY, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),                   INTENT(IN)    :: CFG
  CHARACTER(LEN=*),                             INTENT(IN)    :: KEY
  CHARACTER(LEN=:), ALLOCATABLE,  DIMENSION(:), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),                                INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: EX

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_ARRAY_ALSREADY_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_STRING_ARRAY=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(VALUE), ERRFLAG_STRING_ARRAY_ALSREADY_ALLOCATED )

  ! Read the string
!$omp critical(FCKIT_YAMLCONFIGURATION)
  EX = CFG%CFG_%GET( KEY, VALUE )
!$omp end critical(FCKIT_YAMLCONFIGURATION)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_UNABLE_TO_READ_STRING_ARRAY )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration not allocated' )
    CASE (ERRFLAG_STRING_ARRAY_ALSREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output string already allocated' )
    CASE (ERRFLAG_UNABLE_TO_READ_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read string array' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_READ_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads an integer array from a YAML configuration with an applied filter.
!>
!> This function reads an integer array from a provided YAML configuration object (`CFG`)
!> using the specified key (`KEY`). The `VALUE` array is populated with the parsed integers,
!> applying the given filter (`FILTER`) to each element. The function returns an error code
!> indicating success or failure.
!>
!> @section interface
!> @param [in] CFG The YAML configuration object from which the integer array is read.
!> @param [in] KEY The key corresponding to the integer array in the YAML configuration.
!> @param [out] VALUE The integer array to be populated with the parsed values.
!> @param [in] FILTER A function pointer that applies a filter to each element of the array.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating the outcome of the function.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE]      YAML_CONFIGURATION_T
!>   - @dependency [INTERFACE] FUN_C2I_IF
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see YAML_READ_STRING
!> @see YAML_READ_FLOAT
!> @see YAML_READ_INTEGER
!> @see YAML_READ_LOGICAL
!> @see YAML_READ_STRING_ARRAY
!> @see YAML_READ_INTEGER_ARRAY
!> @see YAML_READ_INTEGER_ARRAY_WITH_RANGES
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_READ_INTEGER_ARRAY_WITH_FILTER'
PP_THREAD_SAFE FUNCTION YAML_READ_INTEGER_ARRAY_WITH_FILTER( CFG, KEY, VALUE, FILTER, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),                       INTENT(IN)    :: CFG
  CHARACTER(LEN=*),                                 INTENT(IN)    :: KEY
  INTEGER(KIND=JPIB_K), ALLOCATABLE,  DIMENSION(:), INTENT(OUT)   :: VALUE
  PROCEDURE(FUN_C2I_IF), POINTER,                   INTENT(IN)    :: FILTER
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  CHARACTER(LEN=:), ALLOCATABLE, DIMENSION(:) :: ATMP
  INTEGER(KIND=JPIB_K) :: VALUE_SIZE
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_ALREADY_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_ARRAY_NOT_ALLOCATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_ARRAY_SIZE_LT_1=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_FILTER=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_READ_STRING_ARRAY=8_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(VALUE),          ERRFLAG_VALUE_ALREADY_ALLOCATED )

  ! Read the paramId as a string array
  PP_TRYCALL(ERRFLAG_ERROR_CALL_READ_STRING_ARRAY) YAML_READ_STRING_ARRAY( CFG, KEY, ATMP, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(ATMP), ERRFLAG_STRING_ARRAY_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(ATMP).LT.1, ERRFLAG_STRING_ARRAY_SIZE_LT_1 )

  ! Allocate the paramId array
  VALUE_SIZE = SIZE(ATMP)
  ALLOCATE( VALUE(VALUE_SIZE), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  ! Compute the size of the paramId array
  ParamIdFIlterSizeLoop: DO I = 1, VALUE_SIZE

    ! Filter the value
    PP_TRYCALL(ERRFLAG_CALL_FILTER) FILTER(ATMP(I), VALUE(I), HOOKS )

  ENDDO ParamIdFIlterSizeLoop

  ! Deallocate temporary memory
  IF ( ALLOCATED(ATMP) ) THEN
    DEALLOCATE( ATMP, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'YAML configuration not allocated' )
    CASE (ERRFLAG_VALUE_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'VALUE already allocated' )
    CASE (ERRFLAG_STRING_ARRAY_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array not allocated after read' )
    CASE (ERRFLAG_STRING_ARRAY_SIZE_LT_1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array is empty' )
    CASE (ERRFLAG_ALLOCATION_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating VALUES' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating VALUES: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_CALL_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling filter' )
    CASE (ERRFLAG_DEALLOCATION_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating ATMP' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating ATMP: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_ERROR_CALL_READ_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling YAML_READ_STRING_ARRAY' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_READ_INTEGER_ARRAY_WITH_FILTER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_READ_INTEGER_KEYSET_WITH_FILTER'
PP_THREAD_SAFE FUNCTION YAML_READ_INTEGER_KEYSET_WITH_FILTER( CFG, KEY, KEYSET, FILTER, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsics modules
   USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: KEYSET_INT64_MOD,  ONLY: KEYSET_INT64_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),     INTENT(IN)    :: CFG
  CHARACTER(LEN=*),               INTENT(IN)    :: KEY
  TYPE(KEYSET_INT64_T),           INTENT(INOUT) :: KEYSET
  PROCEDURE(FUN_C2I_IF), POINTER, INTENT(IN)    :: FILTER
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: TMP
  CHARACTER(LEN=:), ALLOCATABLE, DIMENSION(:) :: ATMP
  INTEGER(KIND=JPIB_K) :: VALUE_SIZE
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_ARRAY_NOT_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_ARRAY_SIZE_LT_1=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_KEYSET_PUSH=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_FILTER=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_READ_STRING_ARRAY=7_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )

  ! Read the paramId as a string array
  PP_TRYCALL(ERRFLAG_ERROR_CALL_READ_STRING_ARRAY) YAML_READ_STRING_ARRAY( CFG, KEY, ATMP, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(ATMP), ERRFLAG_STRING_ARRAY_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(ATMP).LT.1, ERRFLAG_STRING_ARRAY_SIZE_LT_1 )

  ! Allocate the paramId array
  VALUE_SIZE = SIZE(ATMP)

  ! Compute the size of the paramId array
  ParamIdFIlterSizeLoop: DO I = 1, VALUE_SIZE

    ! Filter the value
    PP_TRYCALL(ERRFLAG_CALL_FILTER) FILTER(ATMP(I), TMP, HOOKS )

    ! Push the value to the keyset
    PP_TRYCALL(ERRFLAG_ERROR_CALL_KEYSET_PUSH) KEYSET%INSERT( INT(TMP,KIND=INT64), HOOKS )

  ENDDO ParamIdFIlterSizeLoop

  ! Deallocate temporary memory
  IF ( ALLOCATED(ATMP) ) THEN
    DEALLOCATE( ATMP, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'YAML configuration not allocated' )
    CASE (ERRFLAG_STRING_ARRAY_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array not allocated after read' )
    CASE (ERRFLAG_STRING_ARRAY_SIZE_LT_1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array is empty' )
    CASE (ERRFLAG_CALL_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling filter' )
    CASE (ERRFLAG_ERROR_CALL_KEYSET_PUSH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error pushing value to keyset' )
    CASE (ERRFLAG_DEALLOCATION_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating ATMP' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating ATMP: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_ERROR_CALL_READ_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling YAML_READ_STRING_ARRAY' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_READ_INTEGER_KEYSET_WITH_FILTER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads an integer from a YAML configuration with an applied filter.
!>
!> This function reads an integer from a provided YAML configuration object (`CFG`)
!> using the specified key (`KEY`). The `VALUE` is populated with the parsed integer,
!> applying the given filter (`FILTER`) to each element. The function returns an error code
!> indicating success or failure.
!>
!> @section interface
!> @param [in] CFG The YAML configuration object from which the integer is read.
!> @param [in] KEY The key corresponding to the integer in the YAML configuration.
!> @param [out] VALUE The integer to be populated with the parsed values.
!> @param [in] FILTER A function pointer that applies a filter to the value.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating the outcome of the function.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE]      YAML_CONFIGURATION_T
!>   - @dependency [INTERFACE] FUN_C2I_IF
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see YAML_READ_STRING
!> @see YAML_READ_FLOAT
!> @see YAML_READ_INTEGER
!> @see YAML_READ_LOGICAL
!> @see YAML_READ_STRING_ARRAY
!> @see YAML_READ_INTEGER_ARRAY
!> @see YAML_READ_INTEGER_ARRAY_WITH_RANGES
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_READ_INTEGER_WITH_FILTER'
PP_THREAD_SAFE FUNCTION YAML_READ_INTEGER_WITH_FILTER( CFG, KEY, VALUE, FILTER, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),     INTENT(IN)    :: CFG
  CHARACTER(LEN=*),               INTENT(IN)    :: KEY
  INTEGER(KIND=JPIB_K),           INTENT(OUT)   :: VALUE
  PROCEDURE(FUN_C2I_IF), POINTER, INTENT(IN)    :: FILTER
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: ATMP
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_ARRAY_NOT_ALLOCATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_FILTER=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_READ_STRING_ARRAY=8_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )

  ! Read the paramId as a string array
  PP_TRYCALL(ERRFLAG_ERROR_CALL_READ_STRING_ARRAY) YAML_READ_STRING( CFG, KEY, ATMP, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(ATMP), ERRFLAG_STRING_ARRAY_NOT_ALLOCATED )

  ! Filter the value
  PP_TRYCALL(ERRFLAG_CALL_FILTER) FILTER(ATMP, VALUE, HOOKS )

  ! Deallocate temporary memory
  IF ( ALLOCATED(ATMP) ) THEN
    DEALLOCATE( ATMP, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'YAML configuration not allocated' )
    CASE (ERRFLAG_STRING_ARRAY_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array not allocated after read' )
    CASE (ERRFLAG_CALL_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling filter' )
    CASE (ERRFLAG_DEALLOCATION_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating ATMP' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating ATMP: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_ERROR_CALL_READ_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling YAML_READ_STRING_ARRAY' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_READ_INTEGER_WITH_FILTER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads an integer array with ranges from a YAML configuration.
!>
!> This function reads an integer array from a provided YAML configuration object (`CFG`)
!> using the specified key (`KEY`). The `VALUE` array is populated with the parsed integers,
!> supporting ranges in the YAML configuration. The function returns an error code indicating
!> success or failure of the operation. If verbose mode is enabled, additional output is generated
!> for debugging purposes.
!>
!> @section interface
!> @param [in] CFG The YAML configuration object from which the integer array is read.
!> @param [in] KEY The key corresponding to the integer array in the YAML configuration.
!> @param [out] VALUE The integer array to be populated with the parsed values, supporting ranges.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating the outcome of the function.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE]      YAML_CONFIGURATION_T
!>   - @dependency [PROCEDURE] YAML_READ_STRING_ARRAY
!>   - @dependency [PROCEDURE] STRING_IS_INTEGER
!>   - @dependency [PROCEDURE] STRING_IS_INTEGER_RANGE
!>   - @dependency [PROCEDURE] STRING_IS_INTEGER_RANGE_BY
!>   - @dependency [PROCEDURE] STRING_TO_INTEGER
!>   - @dependency [PROCEDURE] STRING_TO_INTEGER_RANGE
!>   - @dependency [PROCEDURE] STRING_TO_INTEGER_RANGE_BY
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see YAML_READ_STRING_ARRAY
!> @see YAML_READ_INTEGER_ARRAY
!> @see YAML_READ_INTEGER_ARRAY_WITH_FILTER
!> @see YAML_READ_STRING
!> @see YAML_READ_FLOAT
!> @see YAML_READ_LOGICAL
!> @see YAML_READ_INTEGER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_READ_INTEGER_ARRAY_WITH_RANGES'
PP_THREAD_SAFE FUNCTION YAML_READ_INTEGER_ARRAY_WITH_RANGES( CFG, KEY, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_IS_INTEGER
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_IS_INTEGER_RANGE
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_IS_INTEGER_RANGE_BY
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_TO_INTEGER
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_TO_INTEGER_RANGE
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_TO_INTEGER_RANGE_BY

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),                       INTENT(IN)    :: CFG
  CHARACTER(LEN=*),                                 INTENT(IN)    :: KEY
  INTEGER(KIND=JPIB_K), ALLOCATABLE,  DIMENSION(:), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE, DIMENSION(:) :: ATMP
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  LOGICAL :: IS_INTEGER
  LOGICAL :: IS_INTEGER_RANGE
  LOGICAL :: IS_INTEGER_RANGE_BY
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  INTEGER(KIND=JPIB_K) :: BY
  INTEGER(KIND=JPIB_K) :: VALUE_SIZE
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG


  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_ARRAY_NOT_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_ARRAY_SIZE_LT_1=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_1=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_2=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_3=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_4=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_5=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_OUT_OF_BOUNDS=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUS_SIZE_LT_1=14_JPIB_K

  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_READ_STRING_ARRAY=15_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_IS_INTEGER=16_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE=17_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE_BY=18_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_TO_INTEGER=19_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE=20_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE_BY=21_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )

  ! Read the paramId as a string array
  PP_TRYCALL(ERRFLAG_ERROR_CALL_READ_STRING_ARRAY) YAML_READ_STRING_ARRAY( CFG, KEY, ATMP, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(ATMP), ERRFLAG_STRING_ARRAY_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(ATMP).LT.1, ERRFLAG_STRING_ARRAY_SIZE_LT_1 )

  ! Compute the size of the paramId array
  VALUE_SIZE = 0_JPIB_K
  ParamIdFIlterSizeLoop: DO I = 1, SIZE(ATMP)

    ! Check if the value is an integer
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER) STRING_IS_INTEGER( ATMP(I), IS_INTEGER, HOOKS )
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE) STRING_IS_INTEGER_RANGE( ATMP(I), IS_INTEGER_RANGE, HOOKS )
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE_BY) STRING_IS_INTEGER_RANGE_BY( ATMP(I), IS_INTEGER_RANGE_BY, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INTEGER .AND. .NOT.IS_INTEGER_RANGE .AND. .NOT.IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_1 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_2 )

    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE, ERRFLAG_INVALID_STRING_IN_ARRAY_3 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_4 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER_RANGE .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_5 )

    ! Update the total size of the paramId array
    IF ( IS_INTEGER ) THEN
      VALUE_SIZE = VALUE_SIZE + 1
    ELSE IF ( IS_INTEGER_RANGE ) THEN
      PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE) STRING_TO_INTEGER_RANGE( ATMP(I), LO, HI, HOOKS )
      VALUE_SIZE = VALUE_SIZE + HI - LO + 1
    ELSE IF ( IS_INTEGER_RANGE_BY ) THEN
      PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE_BY) STRING_TO_INTEGER_RANGE_BY( ATMP(I), LO, HI, BY, HOOKS )
      VALUE_SIZE = VALUE_SIZE + (HI - LO + 1)/BY
    ENDIF

  ENDDO ParamIdFIlterSizeLoop
  PP_DEBUG_CRITICAL_COND_THROW( VALUE_SIZE.LT.1, ERRFLAG_VALUS_SIZE_LT_1 )

  ! Allocate the paramId array
  ALLOCATE( VALUE(VALUE_SIZE), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  ! Fill the paramId array
  J = 0_JPIB_K
  ParamIdFIlterFillLoop: DO I = 1, SIZE(ATMP)

    ! Check if the value is an integer
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER) STRING_IS_INTEGER( ATMP(I), IS_INTEGER, HOOKS )
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE) STRING_IS_INTEGER_RANGE( ATMP(I), IS_INTEGER_RANGE, HOOKS )
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE_BY) STRING_IS_INTEGER_RANGE_BY( ATMP(I), IS_INTEGER_RANGE_BY, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INTEGER .AND. .NOT.IS_INTEGER_RANGE .AND. .NOT.IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_1 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_2 )

    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE, ERRFLAG_INVALID_STRING_IN_ARRAY_3 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_4 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER_RANGE .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_5 )

    ! Update the total size of the paramId array
    IF ( IS_INTEGER ) THEN

      ! Fill the paramId array when the item is an integer (e.g., 1)
      J = J + 1
      PP_DEBUG_CRITICAL_COND_THROW( J.GT.SIZE(VALUE), ERRFLAG_VALUE_OUT_OF_BOUNDS )
      PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_TO_INTEGER) STRING_TO_INTEGER( ATMP(I), VALUE(J), HOOKS )

    ELSE IF ( IS_INTEGER_RANGE ) THEN

      ! Fill the paramId array when the item is an integer range (e.g., 1:10)
      PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE) STRING_TO_INTEGER_RANGE( ATMP(I), LO, HI, HOOKS )
      ParamIdFIlterFillRangeLoop: DO
        IF ( LO .LE. HI ) THEN
          J = J + 1
          PP_DEBUG_CRITICAL_COND_THROW( J.GT.SIZE(VALUE), ERRFLAG_VALUE_OUT_OF_BOUNDS )
          VALUE(J) = LO
          LO = LO + 1
        ELSE
          EXIT ParamIdFIlterFillRangeLoop
        ENDIF
      ENDDO ParamIdFIlterFillRangeLoop

    ELSE IF ( IS_INTEGER_RANGE_BY ) THEN

      ! Fill the paramId array when the item is an integer range by (e.g., 1:10:2)
      PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE_BY) STRING_TO_INTEGER_RANGE_BY( ATMP(I), LO, HI, BY, HOOKS )
      ParamIdFIlterFillRangeByLoop: DO
        IF ( LO .LE. HI ) THEN
          J = J + 1
          PP_DEBUG_CRITICAL_COND_THROW( J.GT.SIZE(VALUE), ERRFLAG_VALUE_OUT_OF_BOUNDS )
          VALUE(J) = LO
          LO = LO + BY
        ELSE
          EXIT ParamIdFIlterFillRangeByLoop
        ENDIF
      ENDDO ParamIdFIlterFillRangeByLoop

    ENDIF

  ENDDO ParamIdFIlterFillLoop

  ! Deallocate temporary memory
  IF ( ALLOCATED(ATMP) ) THEN
    DEALLOCATE( ATMP, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE(ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'YAML configuration not allocated' )
    CASE(ERRFLAG_STRING_ARRAY_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array not allocated after read' )
    CASE(ERRFLAG_STRING_ARRAY_SIZE_LT_1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array is empty' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string no matches (no integer, no integer range, no integer range by)' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string multiple matches ( integer, integer range, integer range by)' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_3)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string multiple matches (integer, integer range)' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_4)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string multiple matches (integer, integer range by)' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_5)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string multiple matches (integer range, integer range by)' )
    CASE(ERRFLAG_ALLOCATION_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating VALUES' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating VALUES: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE(ERRFLAG_VALUE_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Out of bounds while setting integer value to the output array' )
    CASE(ERRFLAG_DEALLOCATION_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating ATMP' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating ATMP: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_VALUS_SIZE_LT_1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value size is less than 1 after parsing all the elements' )
    CASE (ERRFLAG_ERROR_CALL_READ_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling YAML_READ_STRING_ARRAY' )
    CASE (ERRFLAG_ERROR_CALL_STRING_IS_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_IS_INTEGER' )
    CASE (ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_IS_INTEGER_RANGE' )
    CASE (ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE_BY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_IS_INTEGER_RANGE_BY' )
    CASE (ERRFLAG_ERROR_CALL_STRING_TO_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_TO_INTEGER' )
    CASE (ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_TO_INTEGER_RANGE' )
    CASE (ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE_BY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_TO_INTEGER_RANGE_BY' )
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
  ! Exit point on error
  RETURN

END FUNCTION YAML_READ_INTEGER_ARRAY_WITH_RANGES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Reads an integer keyset with ranges from a YAML configuration.
!>
!> This function reads an integer keyset from a provided YAML configuration object (`CFG`)
!> using the specified key (`KEY`). The `VALUE` keyset is populated with the parsed integers,
!> supporting ranges in the YAML configuration. The function returns an error code indicating
!> success or failure of the operation. If verbose mode is enabled, additional output is generated
!> for debugging purposes.
!>
!> @section interface
!> @param [in] CFG The YAML configuration object from which the integer keyset is read.
!> @param [in] KEY The key corresponding to the integer keyset in the YAML configuration.
!> @param [out] VALUE The integer keyset to be populated with the parsed values, supporting ranges.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating the outcome of the function.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE]      YAML_CONFIGURATION_T
!>   - @dependency [PROCEDURE] YAML_READ_STRING_ARRAY
!>   - @dependency [PROCEDURE] STRING_IS_INTEGER
!>   - @dependency [PROCEDURE] STRING_IS_INTEGER_RANGE
!>   - @dependency [PROCEDURE] STRING_IS_INTEGER_RANGE_BY
!>   - @dependency [PROCEDURE] STRING_TO_INTEGER
!>   - @dependency [PROCEDURE] STRING_TO_INTEGER_RANGE
!>   - @dependency [PROCEDURE] STRING_TO_INTEGER_RANGE_BY
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see YAML_READ_STRING_ARRAY
!> @see YAML_READ_INTEGER_ARRAY
!> @see YAML_READ_INTEGER_ARRAY_WITH_FILTER
!> @see YAML_READ_STRING
!> @see YAML_READ_FLOAT
!> @see YAML_READ_LOGICAL
!> @see YAML_READ_INTEGER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_READ_INTEGER_KEYSET_WITH_RANGES'
PP_THREAD_SAFE FUNCTION YAML_READ_INTEGER_KEYSET_WITH_RANGES( CFG, KEY, KEYSET, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsics modules
   USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: KEYSET_INT64_MOD,  ONLY: KEYSET_INT64_T

  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_IS_INTEGER
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_IS_INTEGER_RANGE
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_IS_INTEGER_RANGE_BY
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_TO_INTEGER
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_TO_INTEGER_RANGE
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_TO_INTEGER_RANGE_BY

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  CHARACTER(LEN=*),           INTENT(IN)    :: KEY
  TYPE(KEYSET_INT64_T),       INTENT(INOUT) :: KEYSET
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE, DIMENSION(:) :: ATMP
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  LOGICAL :: IS_INTEGER
  LOGICAL :: IS_INTEGER_RANGE
  LOGICAL :: IS_INTEGER_RANGE_BY
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  INTEGER(KIND=JPIB_K) :: BY
  INTEGER(KIND=JPIB_K) :: VALUE_SIZE
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG


  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_ARRAY_NOT_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_ARRAY_SIZE_LT_1=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_1=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_2=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_3=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_4=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_5=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR=13_JPIB_K

  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_READ_STRING_ARRAY=15_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_IS_INTEGER=16_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE=17_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE_BY=18_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_TO_INTEGER=19_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE=20_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE_BY=21_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_KEYSET_PUSH=22_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )

  ! Read the paramId as a string array
  PP_TRYCALL(ERRFLAG_ERROR_CALL_READ_STRING_ARRAY) YAML_READ_STRING_ARRAY( CFG, KEY, ATMP, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(ATMP), ERRFLAG_STRING_ARRAY_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(ATMP).LT.1, ERRFLAG_STRING_ARRAY_SIZE_LT_1 )

  ! Fill the paramId array
  J = 0_JPIB_K
  ParamIdFIlterFillLoop: DO I = 1, SIZE(ATMP)

    ! Check if the value is an integer
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER) STRING_IS_INTEGER( ATMP(I), IS_INTEGER, HOOKS )
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE) STRING_IS_INTEGER_RANGE( ATMP(I), IS_INTEGER_RANGE, HOOKS )
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE_BY) STRING_IS_INTEGER_RANGE_BY( ATMP(I), IS_INTEGER_RANGE_BY, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INTEGER .AND. .NOT.IS_INTEGER_RANGE .AND. .NOT.IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_1 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_2 )

    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE, ERRFLAG_INVALID_STRING_IN_ARRAY_3 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_4 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER_RANGE .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_5 )

    ! Update the total size of the paramId array
    IF ( IS_INTEGER ) THEN

      ! Fill the paramId array when the item is an integer (e.g., 1)
      PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_TO_INTEGER) STRING_TO_INTEGER( ATMP(I), LO, HOOKS )
      PP_TRYCALL(ERRFLAG_ERROR_CALL_KEYSET_PUSH) KEYSET%INSERT( INT( LO, KIND=INT64), HOOKS )

    ELSE IF ( IS_INTEGER_RANGE ) THEN

      ! Fill the paramId array when the item is an integer range (e.g., 1:10)
      PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE) STRING_TO_INTEGER_RANGE( ATMP(I), LO, HI, HOOKS )
      ParamIdFIlterFillRangeLoop: DO
        IF ( LO .LE. HI ) THEN
          PP_TRYCALL(ERRFLAG_ERROR_CALL_KEYSET_PUSH) KEYSET%INSERT( INT( LO, KIND=INT64), HOOKS )
          LO = LO + 1
        ELSE
          EXIT ParamIdFIlterFillRangeLoop
        ENDIF
      ENDDO ParamIdFIlterFillRangeLoop

    ELSE IF ( IS_INTEGER_RANGE_BY ) THEN

      ! Fill the paramId array when the item is an integer range by (e.g., 1:10:2)
      PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE_BY) STRING_TO_INTEGER_RANGE_BY( ATMP(I), LO, HI, BY, HOOKS )
      ParamIdFIlterFillRangeByLoop: DO
        IF ( LO .LE. HI ) THEN
          PP_TRYCALL(ERRFLAG_ERROR_CALL_KEYSET_PUSH) KEYSET%INSERT( INT( LO, KIND=INT64), HOOKS )
          LO = LO + BY
        ELSE
          EXIT ParamIdFIlterFillRangeByLoop
        ENDIF
      ENDDO ParamIdFIlterFillRangeByLoop

    ENDIF

  ENDDO ParamIdFIlterFillLoop

  ! Deallocate temporary memory
  IF ( ALLOCATED(ATMP) ) THEN
    DEALLOCATE( ATMP, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE(ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'YAML configuration not allocated' )
    CASE(ERRFLAG_STRING_ARRAY_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array not allocated after read' )
    CASE(ERRFLAG_STRING_ARRAY_SIZE_LT_1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array is empty' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string no matches (no integer, no integer range, no integer range by)' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string multiple matches ( integer, integer range, integer range by)' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_3)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string multiple matches (integer, integer range)' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_4)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string multiple matches (integer, integer range by)' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_5)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string multiple matches (integer range, integer range by)' )
    CASE(ERRFLAG_DEALLOCATION_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating ATMP' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating ATMP: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_ERROR_CALL_READ_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling YAML_READ_STRING_ARRAY' )
    CASE (ERRFLAG_ERROR_CALL_STRING_IS_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_IS_INTEGER' )
    CASE (ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_IS_INTEGER_RANGE' )
    CASE (ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE_BY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_IS_INTEGER_RANGE_BY' )
    CASE (ERRFLAG_ERROR_CALL_STRING_TO_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_TO_INTEGER' )
    CASE (ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_TO_INTEGER_RANGE' )
    CASE (ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE_BY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_TO_INTEGER_RANGE_BY' )
    CASE (ERRFLAG_ERROR_CALL_KEYSET_PUSH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling KEYSET_PUSH' )
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
  ! Exit point on error
  RETURN

END FUNCTION YAML_READ_INTEGER_KEYSET_WITH_RANGES
#undef PP_PROCEDURE_NAME



!>
!> @brief Reads an integer array from a YAML configuration.
!>
!> This function reads an integer array from a provided YAML configuration object (`CFG`)
!> using the specified key (`KEY`). The `VALUE` array is populated with the parsed integers.
!> If an error occurs during the reading process, the function returns an error code.
!>
!> @section interface
!> @param [in] CFG The YAML configuration object from which the integer array is read.
!> @param [in] KEY The key corresponding to the integer array in the YAML configuration.
!> @param [out] VALUE The integer array to be populated with the parsed values.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating the outcome of the function.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE] YAML_CONFIGURATION_T
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_READ_INTEGER_ARRAY'
PP_THREAD_SAFE FUNCTION YAML_READ_INTEGER_ARRAY( CFG, KEY, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),                       INTENT(IN)    :: CFG
  CHARACTER(LEN=*),                                 INTENT(IN)    :: KEY
  INTEGER(KIND=JPIB_K), ALLOCATABLE,  DIMENSION(:), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_INTEGER_ARRAY=2_JPIB_K


  ! Local variables
  LOGICAL :: EX

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )

  ! Read the string
!$omp critical(FCKIT_YAMLCONFIGURATION)
  EX = CFG%CFG_%GET( KEY, VALUE )
!$omp end critical(FCKIT_YAMLCONFIGURATION)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_UNABLE_TO_READ_INTEGER_ARRAY )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration not allocated' )
    CASE (ERRFLAG_UNABLE_TO_READ_INTEGER_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read integer array' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_READ_INTEGER_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_READ_FLOAT_ARRAY'
PP_THREAD_SAFE FUNCTION YAML_READ_FLOAT_ARRAY( CFG, KEY, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),                    INTENT(IN)    :: CFG
  CHARACTER(LEN=*),                              INTENT(IN)    :: KEY
  REAL(KIND=JPRD_K), ALLOCATABLE,  DIMENSION(:), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),                                 INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FLOAT_ARRAY=2_JPIB_K


  ! Local variables
  LOGICAL :: EX

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )

  ! Read the string
!$omp critical(FCKIT_YAMLCONFIGURATION)
  EX = CFG%CFG_%GET( KEY, VALUE )
!$omp end critical(FCKIT_YAMLCONFIGURATION)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_UNABLE_TO_READ_FLOAT_ARRAY )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration not allocated' )
    CASE (ERRFLAG_UNABLE_TO_READ_FLOAT_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read float array' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_READ_FLOAT_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Reads a string value from a YAML configuration object.
!>
!> This function retrieves a string value from the specified YAML configuration (`CFG`)
!> using the given key (`KEY`) and assigns the result to the output variable (`VALUE`).
!>
!> @section interface
!>
!> @param [in] CFG The YAML configuration object from which the string value is read.
!> @param [in] KEY The key used to extract the string value from the configuration.
!> @param [out] VALUE The string value extracted from the configuration, which is allocated
!>                     dynamically.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation:
!>   - `0`: Success
!>   - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE] YAML_CONFIGURATION_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see YAML_READ_FLOAT
!> @see YAML_READ_INTEGER
!> @see YAML_READ_LOGICAL
!> @see YAML_READ_STRING_ARRAY
!> @see YAML_READ_INTEGER_ARRAY
!> @see YAML_READ_INTEGER_ARRAY_WITH_RANGES
!> @see YAML_READ_INTEGER_ARRAY_WITH_FILTER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_READ_STRING'
PP_THREAD_SAFE FUNCTION YAML_READ_STRING( CFG, KEY, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),    INTENT(IN)    :: CFG
  CHARACTER(LEN=*),              INTENT(IN)    :: KEY
  CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_STRING=2_JPIB_K

  ! Local variables
  LOGICAL :: EX

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )

  ! Read the string
!$omp critical(FCKIT_YAMLCONFIGURATION)
  EX = CFG%CFG_%GET( KEY, VALUE )
!$omp end critical(FCKIT_YAMLCONFIGURATION)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_UNABLE_TO_READ_STRING )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration not allocated' )
    CASE (ERRFLAG_UNABLE_TO_READ_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read string from configuration' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_READ_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE






!>
!> @brief Reads a string value from a YAML configuration object. If the string contains
!>        environment variables (the syntax is with curly brachets 'sdakjfhdsjkl{ENV_NAME}ladsfsld;kfa'),
!>        they are expanded.
!>
!> This function retrieves a string value from the specified YAML configuration (`CFG`)
!> using the given key (`KEY`) and assigns the result to the output variable (`VALUE`).
!>
!> @section interface
!>
!> @param [in] CFG The YAML configuration object from which the string value is read.
!> @param [in] KEY The key used to extract the string value from the configuration.
!> @param [out] VALUE The string value extracted from the configuration, which is allocated
!>                     dynamically.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation:
!>   - `0`: Success
!>   - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE] YAML_CONFIGURATION_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see YAML_READ_FLOAT
!> @see YAML_READ_INTEGER
!> @see YAML_READ_LOGICAL
!> @see YAML_READ_STRING_ARRAY
!> @see YAML_READ_INTEGER_ARRAY
!> @see YAML_READ_INTEGER_ARRAY_WITH_RANGES
!> @see YAML_READ_INTEGER_ARRAY_WITH_FILTER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_READ_STRING_WITH_ENV_EXPANSION'
PP_THREAD_SAFE FUNCTION YAML_READ_STRING_WITH_ENV_EXPANSION( CFG, KEY, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: REPLACE_ENVVAR_IN_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T),    INTENT(IN)    :: CFG
  CHARACTER(LEN=*),              INTENT(IN)    :: KEY
  CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: EX
  CHARACTER(LEN=8196) :: CTMP
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_STRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_REPLACE_ENVVAR=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_VALUE=4_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )

  ! Read the string
!$omp critical(FCKIT_YAMLCONFIGURATION)
  EX = CFG%CFG_%GET( KEY, VALUE )
!$omp end critical(FCKIT_YAMLCONFIGURATION)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_UNABLE_TO_READ_STRING )

  ! Expand the environment variables
  CTMP = REPEAT(' ', 8196)
  PP_TRYCALL(ERRFLAG_UNABLE_TO_REPLACE_ENVVAR) REPLACE_ENVVAR_IN_STRING( VALUE, CTMP, HOOKS )

  ! Deallocate old value
  DEALLOCATE(VALUE, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE_VALUE )

  ! Copy the result
  VALUE = TRIM(ADJUSTL(CTMP))

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration not allocated' )
    CASE (ERRFLAG_UNABLE_TO_READ_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read string from configuration' )
    CASE (ERRFLAG_UNABLE_TO_REPLACE_ENVVAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to replace environment variables in string' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'string: "'//TRIM(ADJUSTL(VALUE))//'"' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating VALUE' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_READ_STRING_WITH_ENV_EXPANSION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads an integer value from a YAML configuration object.
!>
!> This function retrieves an integer value from the specified YAML configuration (`CFG`)
!> using the given key (`KEY`) and assigns the result to the output variable (`VALUE`).
!>
!> @section interface
!>
!> @param [in] CFG The YAML configuration object from which the integer value is read.
!> @param [in] KEY The key used to extract the integer value from the configuration.
!> @param [out] VALUE The integer value extracted from the configuration.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation:
!>   - `0`: Success
!>   - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE] YAML_CONFIGURATION_T
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see YAML_READ_FLOAT
!> @see YAML_READ_LOGICAL
!> @see YAML_READ_STRING
!> @see YAML_READ_STRING_ARRAY
!> @see YAML_READ_INTEGER_ARRAY
!> @see YAML_READ_INTEGER_ARRAY_WITH_RANGES
!> @see YAML_READ_INTEGER_ARRAY_WITH_FILTER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_READ_INTEGER'
PP_THREAD_SAFE FUNCTION YAML_READ_INTEGER( CFG, KEY, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  CHARACTER(LEN=*),           INTENT(IN)    :: KEY
  INTEGER(KIND=JPIB_K),       INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_INTEGER=2_JPIB_K

  ! Local variables
  LOGICAL :: EX

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )

  ! Read the string
!$omp critical(FCKIT_YAMLCONFIGURATION)
  EX = CFG%CFG_%GET( KEY, VALUE )
!$omp end critical(FCKIT_YAMLCONFIGURATION)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_UNABLE_TO_READ_INTEGER )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration not allocated' )
    CASE (ERRFLAG_UNABLE_TO_READ_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read integer from configuration' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_READ_INTEGER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads a floating-point value from a YAML configuration object.
!>
!> This function reads a floating-point value from the provided YAML configuration (`CFG`)
!> using the specified key (`KEY`) and assigns the result to the output variable (`VALUE`).
!> If an error occurs during the reading process, an appropriate error code is returned.
!>
!> @section interface
!>
!> @param [in] CFG The YAML configuration object from which the floating-point value is read.
!> @param [in] KEY The key used to retrieve the floating-point value from the configuration.
!> @param [out] VALUE The floating-point value extracted from the configuration.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation:
!>   - `0`: Success
!>   - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [PROCEDURE] READ_INTEGER_PATTERNS
!>
!> @subsection module dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPRD_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see YAML_READ_LOGICAL
!> @see YAML_READ_INTEGER
!> @see YAML_READ_STRING
!> @see YAML_READ_STRING_ARRAY
!> @see YAML_READ_INTEGER_ARRAY
!> @see YAML_READ_INTEGER_ARRAY_WITH_RANGES
!> @see YAML_READ_INTEGER_ARRAY_WITH_FILTER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_READ_FLOAT'
PP_THREAD_SAFE FUNCTION YAML_READ_FLOAT( CFG, KEY, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  CHARACTER(LEN=*),           INTENT(IN)    :: KEY
  REAL(KIND=JPRD_K),          INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FLOAT=2_JPIB_K

  ! Local variables
  LOGICAL :: EX

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )

  ! Read the string
!$omp critical(FCKIT_YAMLCONFIGURATION)
  EX = CFG%CFG_%GET( KEY, VALUE )
!$omp end critical(FCKIT_YAMLCONFIGURATION)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_UNABLE_TO_READ_FLOAT )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration not allocated' )
    CASE (ERRFLAG_UNABLE_TO_READ_FLOAT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read float from configuration' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_READ_FLOAT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads a logical (boolean) value from a YAML configuration object.
!>
!> This function reads a logical (`TRUE` or `FALSE`) value from the provided YAML
!> configuration (`CFG`) and assigns it to the output logical variable (`VALUE`).
!> If an error occurs, it returns an appropriate error code.
!>
!> @section interface
!>
!> @param [in] CFG The YAML configuration object from which the logical value is read.
!> @param [in] KEY The key used to extract the logical value from the configuration.
!> @param [out] VALUE The logical value extracted from the configuration.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation:
!>   - `0`: Success
!>   - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [TYPE] YAML_CONFIGURATION_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see YAML_READ_FLOAT
!> @see YAML_READ_INTEGER
!> @see YAML_READ_STRING
!> @see YAML_READ_STRING_ARRAY
!> @see YAML_READ_INTEGER_ARRAY
!> @see YAML_READ_INTEGER_ARRAY_WITH_RANGES
!> @see YAML_READ_INTEGER_ARRAY_WITH_FILTER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YAML_READ_LOGICAL'
PP_THREAD_SAFE FUNCTION YAML_READ_LOGICAL( CFG, KEY, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  CHARACTER(LEN=*),           INTENT(IN)    :: KEY
  LOGICAL,                    INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_LOGICAL=2_JPIB_K

  ! Local variables
  LOGICAL :: EX

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(CFG%CFG_), ERRFLAG_CFG_NOT_ALLOCATED )

  ! Read the string
!$omp critical(FCKIT_YAMLCONFIGURATION)
  EX = CFG%CFG_%GET( KEY, VALUE )
!$omp end critical(FCKIT_YAMLCONFIGURATION)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_UNABLE_TO_READ_LOGICAL )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration not allocated' )
    CASE (ERRFLAG_UNABLE_TO_READ_LOGICAL)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read bool from configuration' )
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

  ! Exit point on error
  RETURN

END FUNCTION YAML_READ_LOGICAL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE YAML_CORE_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
