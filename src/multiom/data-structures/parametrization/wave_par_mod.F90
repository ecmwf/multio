! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'wave_par_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'WAVE_PAR_MOD'
MODULE WAVE_PAR_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

  !> Wave information I am not able to fit into mars message
  TYPE :: WAVE_PAR_T
    LOGICAL :: TO_BE_DEALLOCATED=.FALSE.
    REAL(KIND=JPRD_K), POINTER, DIMENSION(:) :: DIRS_ => NULL()
    REAL(KIND=JPRD_K), POINTER, DIMENSION(:) :: FREQ_ => NULL()
  CONTAINS
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: COPY_FROM => WAVE_PAR_COPY_FROM
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: READ_FROM_YAML => READ_WAVE_PAR_FROM_YAML
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE => WAVE_PAR_FREE
  END TYPE

  !> Whitelist of public symbols (types)
  PUBLIC :: WAVE_PAR_T

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WAVE_PAR_FREE'
PP_THREAD_SAFE FUNCTION WAVE_PAR_FREE( WAVE_PAR, HOOKS ) RESULT(RET)

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
  CLASS(WAVE_PAR_T), INTENT(INOUT) :: WAVE_PAR
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 1_JPIB_K

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
  IF ( WAVE_PAR%TO_BE_DEALLOCATED ) THEN
    IF ( ASSOCIATED(WAVE_PAR%DIRS_) ) THEN
      DEALLOCATE( WAVE_PAR%DIRS_, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
    ENDIF
    IF ( ASSOCIATED(WAVE_PAR%FREQ_) ) THEN
      DEALLOCATE( WAVE_PAR%FREQ_, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
    ENDIF
    WAVE_PAR%TO_BE_DEALLOCATED = .FALSE.
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
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate memory' )
      IF ( ALLOCATED( ERRMSG)) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS )
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

END FUNCTION WAVE_PAR_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WAVE_PAR_COPY_FROM'
PP_THREAD_SAFE FUNCTION WAVE_PAR_COPY_FROM( WAVE_PAR_TO, WAVE_PAR_FROM, HOOKS ) RESULT(RET)

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
  CLASS(WAVE_PAR_T), INTENT(INOUT) :: WAVE_PAR_TO
  TYPE(WAVE_PAR_T), INTENT(IN)    :: WAVE_PAR_FROM
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

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
  WAVE_PAR_TO%DIRS_ => WAVE_PAR_FROM%DIRS_
  WAVE_PAR_TO%FREQ_ => WAVE_PAR_FROM%FREQ_
  WAVE_PAR_TO%TO_BE_DEALLOCATED = .FALSE.

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

END FUNCTION WAVE_PAR_COPY_FROM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_WAVE_PAR_FROM_YAML'
PP_THREAD_SAFE FUNCTION READ_WAVE_PAR_FROM_YAML( WAVE_PAR, CONFIG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_FLOAT_ARRAY

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(WAVE_PAR_T),           INTENT(INOUT) :: WAVE_PAR
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CONFIG
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  LOGICAL :: HAS_KEY
  TYPE(YAML_CONFIGURATION_T)  :: WAVE_CONFIGURATION
  REAL(KIND=JPRD_K), DIMENSION(:), ALLOCATABLE :: FTMP

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TEST_CASE_DELETE_ERROR = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FLOAT_ARRAY = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_ALLOCATE = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ASSOCIATED = 7_JPIB_K

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

  !> Error handler
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(WAVE_PAR%DIRS_), ERRFLAG_ALREADY_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(WAVE_PAR%FREQ_), ERRFLAG_ALREADY_ASSOCIATED )


  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CONFIG, 'wave', HAS_KEY, HOOKS )

  !> Read representations
  IF ( HAS_KEY  ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CONFIG, 'wave', WAVE_CONFIGURATION, HOOKS )

    !> Read the "wave-directions"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( WAVE_CONFIGURATION, 'direction', HAS_KEY, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.HAS_KEY, ERRFLAG_UNABLE_TO_READ_CFG )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_FLOAT_ARRAY( WAVE_CONFIGURATION, 'direction', FTMP, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(FTMP), ERRFLAG_UNABLE_TO_READ_CFG )

    !> Assign the values
    ALLOCATE( WAVE_PAR%DIRS_( SIZE(FTMP) ), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_ALLOCATE )

    !> Copy the values
    WAVE_PAR%DIRS_ = FTMP

    !> Destroy the configuration object
    DEALLOCATE( FTMP, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )

    !> Read the "wave-frequency"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( WAVE_CONFIGURATION, 'frequency', HAS_KEY, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.HAS_KEY, ERRFLAG_UNABLE_TO_READ_CFG )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_FLOAT_ARRAY( WAVE_CONFIGURATION, 'frequency', FTMP, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(FTMP), ERRFLAG_UNABLE_TO_READ_CFG )

    !> Assign the values
    ALLOCATE( WAVE_PAR%FREQ_( SIZE(FTMP) ), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_ALLOCATE )

    !> Copy the values
    WAVE_PAR%FREQ_ = FTMP

    !> Destroy the configuration object
    DEALLOCATE( FTMP, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )

    ! Needs to be deallocated
    WAVE_PAR%TO_BE_DEALLOCATED = .TRUE.

    !> Destroy the configuration object
    PP_TRYCALL(ERRFLAG_TEST_CASE_DELETE_ERROR) YAML_DELETE_CONFIGURATION( WAVE_CONFIGURATION, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_READ_FLOAT_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read float array' )
    CASE(ERRFLAG_UNABLE_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate memory' )
      IF ( ALLOCATED( ERRMSG)) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS )
      ENDIF
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate memory' )
      IF ( ALLOCATED( ERRMSG)) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS )
      ENDIF
    CASE (ERRFLAG_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'already associated' )
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

END FUNCTION READ_WAVE_PAR_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE WAVE_PAR_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
