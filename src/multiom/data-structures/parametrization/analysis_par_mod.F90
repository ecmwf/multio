! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'analysis_par_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ANALYSIS_PAR_MOD'
MODULE ANALYSIS_PAR_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

  !> ANALYSIS information I am not able to fit into mars message
  TYPE :: ANALYSIS_PAR_T
    INTEGER(KIND=JPIB_K) :: LENGTH_OF_TIME_WINDOW_= UNDEF_PARAM_E
  CONTAINS
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT => ANALYSIS_PAR_INIT
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: COPY_FROM => ANALYSIS_PAR_COPY_FROM
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: READ_FROM_YAML => READ_ANALYSIS_PAR_FROM_YAML
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: WRITE_TO_YAML => WRITE_ANALYSIS_PAR_TO_YAML
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE => ANALYSIS_PAR_FREE
  END TYPE

  !> Whitelist of public symbols (types)
  PUBLIC :: ANALYSIS_PAR_T

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ANALYSIS_PAR_INIT'
PP_THREAD_SAFE FUNCTION ANALYSIS_PAR_INIT( ANALYSIS_PAR, HOOKS ) RESULT(RET)

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
  CLASS(ANALYSIS_PAR_T), INTENT(INOUT) :: ANALYSIS_PAR
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

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

  ! Copy the data
  ANALYSIS_PAR%LENGTH_OF_TIME_WINDOW_ = UNDEF_PARAM_E

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

END FUNCTION ANALYSIS_PAR_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ANALYSIS_PAR_FREE'
PP_THREAD_SAFE FUNCTION ANALYSIS_PAR_FREE( ANALYSIS_PAR, HOOKS ) RESULT(RET)

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
  CLASS(ANALYSIS_PAR_T), INTENT(INOUT) :: ANALYSIS_PAR
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

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

  ! Copy the data
  ANALYSIS_PAR%LENGTH_OF_TIME_WINDOW_ = UNDEF_PARAM_E

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

END FUNCTION ANALYSIS_PAR_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ANALYSIS_PAR_COPY_FROM'
PP_THREAD_SAFE FUNCTION ANALYSIS_PAR_COPY_FROM( ANALYSIS_PAR_TO, ANALYSIS_PAR_FROM, HOOKS ) RESULT(RET)

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
  CLASS(ANALYSIS_PAR_T), INTENT(INOUT) :: ANALYSIS_PAR_TO
  TYPE(ANALYSIS_PAR_T), INTENT(IN)    :: ANALYSIS_PAR_FROM
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

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

  ! Copy the data
  ANALYSIS_PAR_TO%LENGTH_OF_TIME_WINDOW_ = ANALYSIS_PAR_FROM%LENGTH_OF_TIME_WINDOW_

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

END FUNCTION ANALYSIS_PAR_COPY_FROM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_ANALYSIS_PAR_FROM_YAML'
PP_THREAD_SAFE FUNCTION READ_ANALYSIS_PAR_FROM_YAML( ANALYSIS_PAR, CONFIG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ANALYSIS_PAR_T),       INTENT(INOUT) :: ANALYSIS_PAR
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CONFIG
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_KEY
  TYPE(YAML_CONFIGURATION_T)  :: ANALYSIS_CONFIGURATION

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TEST_CASE_DELETE_ERROR = 3_JPIB_K

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

  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CONFIG, 'analysis', HAS_KEY, HOOKS )

  !> Read representations
  IF ( HAS_KEY  ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CONFIG, 'analysis', ANALYSIS_CONFIGURATION, HOOKS )

    !> Read the "truncate-degrees"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( ANALYSIS_CONFIGURATION, 'length-of-time-window', HAS_KEY, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.HAS_KEY, ERRFLAG_UNABLE_TO_READ_CFG )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( ANALYSIS_CONFIGURATION, 'length-of-time-window', ANALYSIS_PAR%LENGTH_OF_TIME_WINDOW_, HOOKS )

    !> Destroy the configuration object
    PP_TRYCALL(ERRFLAG_TEST_CASE_DELETE_ERROR) YAML_DELETE_CONFIGURATION( ANALYSIS_CONFIGURATION, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
    CASE(ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfiguration' )
    CASE(ERRFLAG_TEST_CASE_DELETE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to delete configuration' )
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

END FUNCTION READ_ANALYSIS_PAR_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WRITE_ANALYSIS_PAR_TO_YAML'
PP_THREAD_SAFE FUNCTION WRITE_ANALYSIS_PAR_TO_YAML( ANALYSIS_PAR, UNIT, OFFSET, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E
  USE :: LOG_UTILS_MOD,     ONLY: TO_STRING
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ANALYSIS_PAR_T), INTENT(IN)    :: ANALYSIS_PAR
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=MAX_STR_LEN) :: CTMP
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  LOGICAL :: IS_OPENED
  LOGICAL, DIMENSION(1) :: CONDITIONS

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_OFFSET = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNIT_NOT_OPENED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_STRING = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_ERROR = 4_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( OFFSET.LT.0, ERRFLAG_INVALID_OFFSET )

  CONDITIONS(1) = ANALYSIS_PAR%LENGTH_OF_TIME_WINDOW_ .NE. UNDEF_PARAM_E

  IF ( CONDITIONS(1) ) THEN

    ! Check if it is possible to write on the provided unit
    INQUIRE( UNIT=UNIT, OPENED=IS_OPENED )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_OPENED, ERRFLAG_UNIT_NOT_OPENED )

    ! Write to the unit
    WRITE( UNIT, '(A,A)', IOSTAT=WRITE_STAT ) REPEAT(' ', OFFSET), 'analysis:'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )

    ! convert integer to string
    CTMP = REPEAT(' ', MAX_STR_LEN)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_STRING) TO_STRING( ANALYSIS_PAR%LENGTH_OF_TIME_WINDOW_, CTMP, HOOKS )

    ! Write to the unit
    WRITE( UNIT, '(A,A,A)', IOSTAT=WRITE_STAT ) REPEAT(' ', OFFSET+2), 'length-of-time-window: ', TRIM(ADJUSTL(CTMP))
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )

    ! Add an empty line
    WRITE( UNIT, '(A)', IOSTAT=WRITE_STAT ) REPEAT(' ', OFFSET)
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )

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
    CASE(ERRFLAG_INVALID_OFFSET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid offset' )
    CASE(ERRFLAG_UNIT_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unit not opened' )
    CASE(ERRFLAG_UNABLE_TO_CONVERT_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to string' )
    CASE(ERRFLAG_WRITE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'write error' )
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

END FUNCTION WRITE_ANALYSIS_PAR_TO_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE ANALYSIS_PAR_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
