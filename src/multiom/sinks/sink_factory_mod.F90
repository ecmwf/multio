! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'sink_factory_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'SINK_FACTORY_MOD'
MODULE SINK_FACTORY_MOD

  ! Symbols imported from other modules within the project.
  USE :: SINK_BASE_MOD, ONLY: SINK_CONTAINER_T

IMPLICIT NONE

! Defautl visibility of the module
PRIVATE

! White list of the public symbols (types)
PUBLIC :: SINK_CONTAINER_T

! White list of the public symbols (procedures)
PUBLIC :: MAKE_SINKS
PUBLIC :: DESTROY_SINKS

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_SINKS'
PP_THREAD_SAFE FUNCTION MAKE_SINKS( CFG, SINKS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: SINK_BASE_MOD,       ONLY: SINK_CONTAINER_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATIONS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_CONFIGURATIONS_SIZE
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_CONFIGURATION_BY_ID
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(YAML_CONFIGURATIONS_T),                   INTENT(IN)    :: CFG
  TYPE(SINK_CONTAINER_T), DIMENSION(:), POINTER, INTENT(INOUT) :: SINKS
  TYPE(HOOKS_T),                                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: N_SINKS
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  TYPE(YAML_CONFIGURATION_T) :: SINK_CONFIGURATION
  LOGICAL :: HAS_TYPE

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_NUMBER_OF_SINKS = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_SINKS = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SINKS_ALREADY_ASSOCIATED = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG_BY_ID = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SINK_TYPE = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_SINKS = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_SINK = 8_JPIB_K

  !> Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  !> Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  !> Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  !> Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  !> Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(SINKS), ERRFLAG_SINKS_ALREADY_ASSOCIATED )

  !> Get the sections size
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE) YAML_GET_CONFIGURATIONS_SIZE( CFG, N_SINKS, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( N_SINKS .LE. 0, ERRFLAG_WRONG_NUMBER_OF_SINKS )

  ALLOCATE(SINKS(N_SINKS), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_SINKS )

  !> Loop over sinks
  DO I = 1, N_SINKS


    !> Get section configuration by ID
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG_BY_ID) YAML_GET_CONFIGURATION_BY_ID( CFG, I, SINK_CONFIGURATION, HOOKS )

    !> Create the rule object
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SINK_TYPE) YAML_CONFIGURATION_HAS_KEY( SINK_CONFIGURATION, "type", HAS_TYPE, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_TYPE, ERRFLAG_UNABLE_TO_READ_SINK_TYPE )

    !> Allocate the sink
    PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_SINK) MAKE_SINK( SINK_CONFIGURATION, SINKS(I)%SINK_, HOOKS )

    !> Deallocate section configuration
    PP_TRYCALL(ERRFLAG_UNABLE_TO_DEALLOCATE_SINKS) YAML_DELETE_CONFIGURATION( SINK_CONFIGURATION, HOOKS )

  ENDDO

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
    CASE(ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get the size of the sub-configurations' )
    CASE(ERRFLAG_WRONG_NUMBER_OF_SINKS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong number of sinks' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE_SINKS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate sinks' )
      IF ( ALLOCATED(ERRMSG)) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STATUS)
      END IF
    CASE(ERRFLAG_SINKS_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'sinks already associated' )
    CASE(ERRFLAG_UNABLE_TO_READ_CFG_BY_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read the configuration by ID' )
    CASE(ERRFLAG_UNABLE_TO_READ_SINK_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read the sink type' )
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE_SINKS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate sinks' )
    CASE(ERRFLAG_UNABLE_TO_MAKE_SINK)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make sink' )
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

END FUNCTION MAKE_SINKS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_SINK'
PP_THREAD_SAFE FUNCTION MAKE_SINK( CFG, SINK, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: SINK_BASE_MOD,       ONLY: SINK_BASE_A
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING
  USE :: GENERAL_UTILS_MOD,   ONLY: TOLOWER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(YAML_CONFIGURATION_T),  INTENT(IN)    :: CFG
  CLASS(SINK_BASE_A), POINTER, INTENT(INOUT) :: SINK
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: TYPE
  CHARACTER(LEN=128) :: LOC_TYPE
  CHARACTER(LEN=128) :: LOC_TYPE_LC
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SINK_TYPE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TYPE_TOO_LONG = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_SINK_TYPE = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC = 6_JPIB_K

  !> Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  !> Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  !> Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  !> Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  !> Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  !> Check if the rule is associated
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(SINK), ERRFLAG_ALREADY_ASSOCIATED )

  !> Get sink "type"
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SINK_TYPE) YAML_READ_STRING( CFG, "type", TYPE, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ALLOCATED(TYPE), ERRFLAG_UNABLE_TO_GET_SINK_TYPE )
  PP_DEBUG_CRITICAL_COND_THROW( LEN(TYPE).GT.LEN(LOC_TYPE), ERRFLAG_TYPE_TOO_LONG )
  LOC_TYPE = REPEAT(' ', LEN(LOC_TYPE))
  LOC_TYPE = TYPE
  DEALLOCATE(TYPE, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE )

  !> Convert sink type to lowercase
  LOC_TYPE_LC = REPEAT(' ', LEN(LOC_TYPE_LC))
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( TRIM(LOC_TYPE), LOC_TYPE_LC, HOOKS )

  !> Create the sink object
  SELECT CASE(TRIM(LOC_TYPE_LC))
  CASE ( 'grib_message-to-file' )
  CASE ( 'grib-message-to-fdb' )
  CASE ( 'partial-grib-header-to-multio' )
  CASE ( 'full-grib-header-to-multio' )
  CASE ( 'fortran-metadata-to-multio' )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_SINK_TYPE )
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
    CASE(ERRFLAG_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'sink already associated' )
    CASE(ERRFLAG_UNABLE_TO_GET_SINK_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get the sink type' )
    CASE(ERRFLAG_TYPE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'sink type too long' )
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate sink type' )
      IF ( ALLOCATED(ERRMSG)) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STATUS)
      END IF
    CASE(ERRFLAG_UNKNOWN_SINK_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown sink type: "'//TRIM(LOC_TYPE_LC)//'"' )
    CASE(ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
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

END FUNCTION MAKE_SINK
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DESTROY_SINKS'
PP_THREAD_SAFE FUNCTION DESTROY_SINKS( SINKS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: SINK_BASE_MOD,       ONLY: SINK_CONTAINER_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(SINK_CONTAINER_T), DIMENSION(:), POINTER, INTENT(INOUT) :: SINKS
  TYPE(HOOKS_T),                                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  !> Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  !> Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  !> Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  !> Initialization of good path return value
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


END FUNCTION DESTROY_SINKS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE SINK_FACTORY_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
