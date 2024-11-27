! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'multiom_debug_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MULTIOM_DEBUG_MOD'
MODULE MULTIOM_DEBUG_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

IMPLICIT NONE

! Default visibility of the module
PRIVATE

TYPE :: DEBUG_POINT_T
  CHARACTER(LEN=256) :: NAME
  TYPE(FORTRAN_MESSAGE_T) :: MSG
  TYPE(PARAMETRIZATION_T) :: PAR
END TYPE

TYPE :: DEBUG_POINT_COLLECTION_T
  PRIVATE
  TYPE(DEBUG_POINT_T), ALLOCATABLE, DIMENSION(:) :: DEBUG_POINTS
CONTAINS
  PROCEDURE, PUBLIC, PASS :: INIT => DEBUG_POINT_COLLECTION_INIT
  PROCEDURE, PUBLIC, PASS :: SIZE => DEBUG_POINT_COLLECTION_SIZE
  PROCEDURE, PUBLIC, PASS :: GET  => DEBUG_POINT_COLLECTION_GET
  PROCEDURE, PUBLIC, PASS :: FREE => DEBUG_POINT_COLLECTION_FREE
END TYPE

!> Whitelist of public symbols
PUBLIC :: DEBUG_POINT_COLLECTION_T

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DEBUG_POINT_COLLECTION_INIT'
PP_THREAD_SAFE FUNCTION DEBUG_POINT_COLLECTION_INIT( THIS, MAPPING_FNAME, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATIONS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_CONFIGURATIONS_SIZE
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_CONFIGURATION_BY_ID
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_NEW_CONFIGURATION_FROM_FILE
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(DEBUG_POINT_COLLECTION_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: MAPPING_FNAME
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(YAML_CONFIGURATION_T) :: MAIN_TEST_CASES_CONFIG
  TYPE(YAML_CONFIGURATIONS_T) ::TEST_CASES_CONFIGURATIONS
  TYPE(YAML_CONFIGURATION_T) :: TEST_CASES_CONFIGURATION
  TYPE(YAML_CONFIGURATION_T) :: MESSAGE_CONFIG
  INTEGER(KIND=JPIB_K) :: SZ
  INTEGER(KIND=JPIB_K) :: I
  LOGICAL :: HAS_TESTCASES
  LOGICAL :: HAS_MSG
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=:), ALLOCATABLE :: CTMP

  !> Errflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOAD_MAPPING_FILE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_RULE_DELETE_ERROR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SUBCFG=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_NUMBER_OF_TIMERANGES=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TEST_CASE_DELETE_ERROR=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOC_ERROR=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_MESSAGE=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=11_JPIB_K


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

  !> Open the configuration file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_LOAD_MAPPING_FILE) YAML_NEW_CONFIGURATION_FROM_FILE( &
& TRIM(ADJUSTL(MAPPING_FNAME)), MAIN_TEST_CASES_CONFIG, HOOKS )

  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( MAIN_TEST_CASES_CONFIG, 'test-cases', HAS_TESTCASES, HOOKS )

  IF ( HAS_TESTCASES  ) THEN
    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATIONS( MAIN_TEST_CASES_CONFIG, 'test-cases', TEST_CASES_CONFIGURATIONS, HOOKS )

    !> Get the sections size
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE) YAML_GET_CONFIGURATIONS_SIZE( TEST_CASES_CONFIGURATIONS, SZ, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( SZ .LE. 0, ERRFLAG_WRONG_NUMBER_OF_TIMERANGES )

    ALLOCATE( THIS%DEBUG_POINTS(SZ), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_ALLOC_ERROR )

    DO I = 1, SZ

      !> Get the configuration by ID
      PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SUBCFG) YAML_GET_CONFIGURATION_BY_ID( TEST_CASES_CONFIGURATIONS, I, TEST_CASES_CONFIGURATION, HOOKS )

      !> Read the name
      IF ( ALLOCATED(CTMP) ) THEN
        DEALLOCATE(CTMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
        PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      ENDIF
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_STRING( TEST_CASES_CONFIGURATION, 'name', CTMP, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CTMP), ERRFLAG_UNABLE_TO_READ_CFG )
      THIS%DEBUG_POINTS(I)%NAME = CTMP
      IF ( ALLOCATED(CTMP) ) THEN
        DEALLOCATE(CTMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
        PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      ENDIF

      !> Read the configuration message
      PP_TRYCALL(ERRFLAG_READ_MESSAGE) THIS%DEBUG_POINTS(I)%MSG%READ_FROM_YAML( TEST_CASES_CONFIGURATION, HOOKS )

      !> Read the parametrization configuration
      PP_TRYCALL(ERRFLAG_READ_MESSAGE) THIS%DEBUG_POINTS(I)%PAR%READ_FROM_YAML( TEST_CASES_CONFIGURATION, HOOKS )

      !> Destroy the configuration object
      PP_TRYCALL(ERRFLAG_TEST_CASE_DELETE_ERROR) YAML_DELETE_CONFIGURATION( TEST_CASES_CONFIGURATION, HOOKS )

    ENDDO

    !> Destroy the configuration object
    PP_TRYCALL(ERRFLAG_TEST_CASE_DELETE_ERROR) YAML_DELETE_CONFIGURATIONS( TEST_CASES_CONFIGURATIONS, HOOKS )

  ENDIF

  !> Destroy the configuration object
  PP_TRYCALL(ERRFLAG_MAPPING_RULE_DELETE_ERROR) YAML_DELETE_CONFIGURATION( MAIN_TEST_CASES_CONFIG, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_LOAD_MAPPING_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to load mapping file' )
    CASE (ERRFLAG_MAPPING_RULE_DELETE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'mapping rule delete error' )
    CASE (ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read cfg' )
    CASE (ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get subcfg size' )
    CASE (ERRFLAG_UNABLE_TO_GET_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get subcfg' )
    CASE (ERRFLAG_WRONG_NUMBER_OF_TIMERANGES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong number of timeranges' )
    CASE (ERRFLAG_TEST_CASE_DELETE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'test case delete error' )
    CASE (ERRFLAG_ALLOC_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'alloc error' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      ENDIF
    CASE (ERRFLAG_READ_MESSAGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'read message' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate' )
    CASE (ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subcfg' )
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

END FUNCTION DEBUG_POINT_COLLECTION_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DEBUG_POINT_COLLECTION_SIZE'
PP_THREAD_SAFE FUNCTION DEBUG_POINT_COLLECTION_SIZE( THIS, SZ, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

IMPLICIT NONE

  !> Dummy arguments
  CLASS(DEBUG_POINT_COLLECTION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),            INTENT(OUT)   :: SZ
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=1_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(THIS%DEBUG_POINTS), ERRFLAG_NOT_ALLOCATED )

  !> Open the configuration file
  SZ = SIZE(THIS%DEBUG_POINTS)

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
    CASE (ERRFLAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'alloc error' )
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

END FUNCTION DEBUG_POINT_COLLECTION_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DEBUG_POINT_COLLECTION_GET'
PP_THREAD_SAFE FUNCTION DEBUG_POINT_COLLECTION_GET( THIS, ID, NAME, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

IMPLICIT NONE

  !> Dummy arguments
  CLASS(DEBUG_POINT_COLLECTION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),            INTENT(IN)    :: ID
  CHARACTER(LEN=256),              INTENT(OUT)   :: NAME
  TYPE(FORTRAN_MESSAGE_T),         INTENT(OUT)   :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(OUT)   :: PAR
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COPY_MESSAGE=3_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(THIS%DEBUG_POINTS), ERRFLAG_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ID.LT.1, ERRFLAG_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( ID.GT.SIZE(THIS%DEBUG_POINTS), ERRFLAG_OUT_OF_BOUNDS )

  !> Open the configuration file
  NAME = THIS%DEBUG_POINTS(ID)%NAME
  PP_TRYCALL(ERRFLAG_UNABLE_TO_COPY_MESSAGE) MSG%COPY_FROM( THIS%DEBUG_POINTS(ID)%MSG, HOOKS )
  ! PP_TRYCALL(ERRFLAG_UNABLE_TO_COPY_PARAM) PAR%COPY_FROM( THIS%DEBUG_POINTS(ID)%PAR, HOOKS )

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
    CASE (ERRFLAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'alloc error' )
    CASE (ERRFLAG_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'out of bounds' )
    CASE (ERRFLAG_UNABLE_TO_COPY_MESSAGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to copy message' )
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

END FUNCTION DEBUG_POINT_COLLECTION_GET
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DEBUG_POINT_COLLECTION_FREE'
PP_THREAD_SAFE FUNCTION DEBUG_POINT_COLLECTION_FREE( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
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
  CLASS(DEBUG_POINT_COLLECTION_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOC_ERROR=1_JPIB_K

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

  IF ( ALLOCATED(THIS%DEBUG_POINTS) ) THEN
    DEALLOCATE( THIS%DEBUG_POINTS, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_ALLOC_ERROR )
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
    CASE (ERRFLAG_ALLOC_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'alloc error' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        DEALLOCATE( ERRMSG, STAT=ALLOC_STAT )
      ENDIF
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

END FUNCTION DEBUG_POINT_COLLECTION_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE MULTIOM_DEBUG_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME