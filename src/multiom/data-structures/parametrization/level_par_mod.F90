! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'level_par_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'LEVEL_PAR_MOD'
MODULE LEVEL_PAR_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

  !> Level information
  TYPE :: LEVEL_PAR_T
    LOGICAL :: TO_BE_DEALLOCATED=.FALSE.
    REAL(KIND=JPRD_K), DIMENSION(:), POINTER :: PV => NULL()
  CONTAINS
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT => LEVEL_PAR_INIT
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: COPY_FROM => LEVEL_PAR_COPY_FROM
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: READ_FROM_YAML => READ_LEVEL_PAR_FROM_YAML
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE => LEVEL_PAR_FREE
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: WRITE_TO_YAML => WRITE_LEVEL_PAR_TO_YAML
  END TYPE

  !> Whitelist of public symbols (types)
  PUBLIC :: LEVEL_PAR_T

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'LEVEL_PAR_INIT'
PP_THREAD_SAFE FUNCTION LEVEL_PAR_INIT( LEVEL_PAR, HOOKS ) RESULT(RET)

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
  CLASS(LEVEL_PAR_T), INTENT(INOUT) :: LEVEL_PAR
  TYPE(HOOKS_T),      INTENT(INOUT) :: HOOKS

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

  ! Reset the data
  IF ( LEVEL_PAR%TO_BE_DEALLOCATED ) THEN
    IF ( ASSOCIATED(LEVEL_PAR%PV) ) THEN
      DEALLOCATE( LEVEL_PAR%PV, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      NULLIFY( LEVEL_PAR%PV )
    ENDIF
    LEVEL_PAR%TO_BE_DEALLOCATED = .FALSE.
  ELSE
    LEVEL_PAR%TO_BE_DEALLOCATED = .FALSE.
    NULLIFY( LEVEL_PAR%PV )
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

END FUNCTION LEVEL_PAR_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'LEVEL_PAR_FREE'
PP_THREAD_SAFE FUNCTION LEVEL_PAR_FREE( LEVEL_PAR, HOOKS ) RESULT(RET)

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
  CLASS(LEVEL_PAR_T), INTENT(INOUT) :: LEVEL_PAR
  TYPE(HOOKS_T),      INTENT(INOUT) :: HOOKS

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

  ! Reset the data
  IF ( LEVEL_PAR%TO_BE_DEALLOCATED ) THEN

    IF ( ASSOCIATED(LEVEL_PAR%PV) ) THEN
      LEVEL_PAR%PV = 0.0
      DEALLOCATE( LEVEL_PAR%PV, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      NULLIFY( LEVEL_PAR%PV )
    ENDIF
    LEVEL_PAR%TO_BE_DEALLOCATED = .FALSE.
  ELSE
    LEVEL_PAR%TO_BE_DEALLOCATED = .FALSE.
    NULLIFY( LEVEL_PAR%PV )
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

END FUNCTION LEVEL_PAR_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'LEVEL_PAR_COPY_FROM'
PP_THREAD_SAFE FUNCTION LEVEL_PAR_COPY_FROM( LEVEL_PAR_TO, LEVEL_PAR_FROM, HOOKS ) RESULT(RET)

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
  CLASS(LEVEL_PAR_T), INTENT(INOUT) :: LEVEL_PAR_TO
  TYPE(LEVEL_PAR_T), INTENT(IN)    :: LEVEL_PAR_FROM
  TYPE(HOOKS_T),     INTENT(INOUT) :: HOOKS

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
  LEVEL_PAR_TO%PV => LEVEL_PAR_FROM%PV
  LEVEL_PAR_TO%TO_BE_DEALLOCATED = .FALSE.

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

END FUNCTION LEVEL_PAR_COPY_FROM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_LEVEL_PAR_FROM_YAML'
PP_THREAD_SAFE FUNCTION READ_LEVEL_PAR_FROM_YAML( LEVEL_PAR, CONFIG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_FLOAT_ARRAY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER
  USE :: ENUMERATORS_MOD,     ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(LEVEL_PAR_T),         INTENT(INOUT) :: LEVEL_PAR
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CONFIG
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  LOGICAL :: HAS_KEY
  TYPE(YAML_CONFIGURATION_T)  :: LEVEL_CONFIGURATION
  REAL(KIND=JPRD_K), DIMENSION(:), ALLOCATABLE :: PV

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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(LEVEL_PAR%PV), ERRFLAG_ALREADY_ASSOCIATED )

  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CONFIG, 'levels', HAS_KEY, HOOKS )

  !> Read representations
  IF ( HAS_KEY  ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CONFIG, 'levels', LEVEL_CONFIGURATION, HOOKS )

    !> Read the "pv array"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( LEVEL_CONFIGURATION, 'pv', HAS_KEY, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.HAS_KEY, ERRFLAG_UNABLE_TO_READ_CFG )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_FLOAT_ARRAY( LEVEL_CONFIGURATION, 'pv', PV, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(PV), ERRFLAG_UNABLE_TO_READ_CFG )

    !> Assign the values
    ALLOCATE( LEVEL_PAR%PV( SIZE(PV) ), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_ALLOCATE )

    !> Copy the values
    LEVEL_PAR%PV = PV

    !> Destroy the configuration object
    DEALLOCATE( PV, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )

    !> Needs to be deallocated
    LEVEL_PAR%TO_BE_DEALLOCATED = .TRUE.

    !> Destroy the configuration object
    PP_TRYCALL(ERRFLAG_TEST_CASE_DELETE_ERROR) YAML_DELETE_CONFIGURATION( LEVEL_CONFIGURATION, HOOKS )

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

END FUNCTION READ_LEVEL_PAR_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WRITE_LEVEL_PAR_TO_YAML'
PP_THREAD_SAFE FUNCTION WRITE_LEVEL_PAR_TO_YAML( LEVEL_PAR, UNIT, OFFSET, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  USE :: LOG_UTILS_MOD, ONLY: TO_STRING
  USE :: LOG_UTILS_MOD, ONLY: MAX_STR_LEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(LEVEL_PAR_T),   INTENT(IN)    :: LEVEL_PAR
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE :: CATMP
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  INTEGER(KIND=JPIB_K) :: I
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  LOGICAL :: IS_OPENED

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_OFFSET = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNIT_NOT_OPENED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_ERROR = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_STRING = 5_JPIB_K

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

  IF ( ASSOCIATED(LEVEL_PAR%PV) ) THEN

    ! convert integer to string
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_STRING) TO_STRING( LEVEL_PAR%PV, CATMP, HOOKS )

    ! Check if it is possible to write on the provided unit
    INQUIRE( UNIT=UNIT, OPENED=IS_OPENED )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_OPENED, ERRFLAG_UNIT_NOT_OPENED )

    ! Write to the unit
    WRITE( UNIT, '(A,A)', IOSTAT=WRITE_STAT ) REPEAT(' ', OFFSET), 'levels:'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )

    WRITE( UNIT, '(A,A)', IOSTAT=WRITE_STAT ) REPEAT(' ', OFFSET+2), 'pv: ['
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )

    WRITE( UNIT, '(A)', ADVANCE='NO',IOSTAT=WRITE_STAT ) REPEAT(' ', OFFSET+4)
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )

    IF ( ALLOCATED(CATMP) ) THEN
      DO I = 1, SIZE(CATMP)-1
        IF ( MOD(I,10) .EQ. 0 ) THEN
          WRITE( UNIT, '(A)', IOSTAT=WRITE_STAT ) TRIM(ADJUSTL(CATMP(I)))//', '
          PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
          WRITE( UNIT, '(A)', ADVANCE='NO',IOSTAT=WRITE_STAT ) REPEAT(' ', OFFSET+4)
          PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
        ELSE
          WRITE( UNIT, '(A)', ADVANCE='NO', IOSTAT=WRITE_STAT ) TRIM(ADJUSTL(CATMP(I)))//', '
          PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
        ENDIF
      ENDDO
      WRITE( UNIT, '(A)', IOSTAT=WRITE_STAT ) TRIM(ADJUSTL(CATMP(SIZE(CATMP))))
      PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
      WRITE( UNIT, '(A)', IOSTAT=WRITE_STAT ) REPEAT(' ', OFFSET+2)//']'
      PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )

      DEALLOCATE(CATMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE )
    ENDIF

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
    CASE(ERRFLAG_WRITE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'write error' )
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate memory' )
      IF ( ALLOCATED( ERRMSG)) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      ENDIF
    CASE(ERRFLAG_UNABLE_TO_CONVERT_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to string' )
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

END FUNCTION WRITE_LEVEL_PAR_TO_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE LEVEL_PAR_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
