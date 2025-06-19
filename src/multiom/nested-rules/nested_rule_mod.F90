! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'nested_rule_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'NESTED_RULE_MOD'
MODULE NESTED_RULE_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: FILTER_BASE_MOD,   ONLY: FILTER_BASE_A

  ! Symbols imported from other libraries
  USE  :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T

IMPLICIT NONE


TYPE :: NESTED_RULE_CONTAINER_T

  !> Nested rule
  CLASS(NESTED_RULE_T), POINTER :: RULE_ => NULL()

END TYPE


TYPE :: NESTED_RULE_T

  !> Default visibility of the module
  PRIVATE

  !> Id of the rule
  INTEGER(KIND=JPIB_K) :: ID_=-9999_JPIB_K

  !> Filters of the rule
  CLASS(FILTER_BASE_A), POINTER :: FILTER_ => NULL()

  !> Name of the rule
  CHARACTER(LEN=:), ALLOCATABLE :: RULE_FNAME_

CONTAINS

  !> Initialize the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: INIT => RULE_INIT_FILE

  !> Match the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: MATCH => RULE_MATCH

  !> Get the encoder of the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: GET_FNAME => RULE_GET_FNAME

  !> Free all the memory allocated by the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: PRINT => RULE_PRINT

  !> Free all the memory allocated by the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: FREE => RULE_FREE

END TYPE

!> Whitelist of public symbols
PUBLIC :: NESTED_RULE_T
PUBLIC :: NESTED_RULE_CONTAINER_T

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_INIT_FILE'
PP_THREAD_SAFE FUNCTION RULE_INIT_FILE( THIS, ID, RULE_FNAME, FILTER_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: FILTER_FACTORY_MOD,  ONLY: MAKE_FILTER
  USE :: FILTER_OPTIONS_MOD,  ONLY: FILTER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_NEW_CONFIGURATION_FROM_FILE
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(NESTED_RULE_T),   INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),   INTENT(IN)    :: ID
  CHARACTER(LEN=*),       INTENT(IN)    :: RULE_FNAME
  TYPE(FILTER_OPTIONS_T), INTENT(IN)    :: FILTER_OPT
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: FEXIST
  TYPE(YAML_CONFIGURATION_T) :: LOC_CFG
  TYPE(YAML_CONFIGURATION_T) :: FILTER_CFG
  LOGICAL :: CONFIGURATION_HAS_FILTER
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: ENCODER_TYPE
  CHARACTER(LEN=:), ALLOCATABLE :: FILTER_TYPE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_FILE_DOES_NOT_EXIST=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FILE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULE_DEALLOCATION_ERROR=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULE_WITHOUT_FILTER=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FILTER_CFG=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_FILTER=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DELETE_FILTER_CFG=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_FILTER_TYPE=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULE_ALREADY_ALLOCATED=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULE_FNAME_ALLOCATION_ERROR=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED_AFTER_READ=12_JPIB_K

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

  !> Initialization
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%FILTER_), ERRFLAG_RULE_ALREADY_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%RULE_FNAME_), ERRFLAG_RULE_ALREADY_ALLOCATED )

  !> Set the ID
  THIS%ID_ = ID
  ALLOCATE( CHARACTER(LEN=LEN_TRIM(RULE_FNAME))::THIS%RULE_FNAME_, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_RULE_FNAME_ALLOCATION_ERROR )
  THIS%RULE_FNAME_(1:LEN_TRIM(RULE_FNAME)) = TRIM(RULE_FNAME)

  FEXIST = .FALSE.
  INQUIRE( FILE=TRIM(ADJUSTL(RULE_FNAME)), EXIST=FEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. FEXIST, ERRFLAG_CFG_FILE_DOES_NOT_EXIST )

  !> Read the configuration from the file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_FILE) YAML_NEW_CONFIGURATION_FROM_FILE( TRIM(ADJUSTL(RULE_FNAME)), LOC_CFG, HOOKS )

  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( LOC_CFG, 'filter', CONFIGURATION_HAS_FILTER, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. CONFIGURATION_HAS_FILTER, ERRFLAG_RULE_WITHOUT_FILTER )

  !>
  !> Load subconfiguration for the filter
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_FILTER_CFG) YAML_GET_SUBCONFIGURATION( LOC_CFG, 'filter', FILTER_CFG, HOOKS )

  !> Read the filter type
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_STRING( FILTER_CFG, 'type', FILTER_TYPE, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(FILTER_TYPE), ERRFLAG_NOT_ALLOCATED_AFTER_READ )

  !> Read the filter
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_FILTER) MAKE_FILTER( THIS%FILTER_, FILTER_TYPE, FILTER_CFG, FILTER_OPT, HOOKS )

  !> Delete the subconfiguration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DELETE_FILTER_CFG) YAML_DELETE_CONFIGURATION( FILTER_CFG, HOOKS )

  !> Deallocate the filter type
  DEALLOCATE( FILTER_TYPE, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE_FILTER_TYPE )

  !> Free local configuration
  PP_TRYCALL(ERRFLAG_RULE_DEALLOCATION_ERROR) YAML_DELETE_CONFIGURATION( LOC_CFG, HOOKS )

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
    CASE(ERRFLAG_CFG_FILE_DOES_NOT_EXIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration file does not exist' )
    CASE(ERRFLAG_UNABLE_TO_READ_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the configuration file' )
    CASE(ERRFLAG_RULE_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Rule deallocation error' )
    CASE(ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the configuration' )
    CASE(ERRFLAG_RULE_WITHOUT_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Rule without filter' )
    CASE(ERRFLAG_UNABLE_TO_READ_FILTER_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the filter configuration' )
    CASE(ERRFLAG_UNABLE_TO_MAKE_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to make the filter' )
    CASE(ERRFLAG_UNABLE_TO_DELETE_FILTER_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to delete the filter configuration' )
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE_FILTER_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate the filter type' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error: "' // TRIM(ADJUSTL(ERRMSG)) // '"' )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE(ERRFLAG_RULE_FNAME_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate the rule file name' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error: "' // TRIM(ADJUSTL(ERRMSG)) // '"' )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE(ERRFLAG_RULE_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Rule already allocated' )
    CASE(ERRFLAG_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The string was not allocated after reading' )
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


END FUNCTION RULE_INIT_FILE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_MATCH'
PP_THREAD_SAFE FUNCTION RULE_MATCH( THIS, MSG, MATCH, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(NESTED_RULE_T),    INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T), INTENT(IN)    :: MSG
  LOGICAL,                 INTENT(OUT)   :: MATCH
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILTER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_FILTERS=2_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%FILTER_), ERRFLAG_FILTER_NOT_ASSOCIATED )

  !> Match the filter
  PP_TRYCALL(ERRFLAG_MATCH_FILTERS) THIS%FILTER_%MATCH( MSG, MATCH, HOOKS )

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
    CASE(ERRFLAG_FILTER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Filter not associated' )
    CASE(ERRFLAG_MATCH_FILTERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to match the filter' )
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


END FUNCTION RULE_MATCH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_GET_FNAME'
PP_THREAD_SAFE FUNCTION RULE_GET_FNAME( THIS, RULES_LIST, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: NESTED_RULES_LIST_MOD, ONLY: RAW_RULES_LIST_T

  ! Symbols imported from other libraries
  USE  :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(NESTED_RULE_T),   INTENT(INOUT) :: THIS
  TYPE(RAW_RULES_LIST_T), INTENT(INOUT) :: RULES_LIST
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PUSH_FNAME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FNAME_NOT_ALLOCATED=2_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ALLOCATED(THIS%RULE_FNAME_), ERRFLAG_FNAME_NOT_ALLOCATED )

  ! Initialization
  PP_TRYCALL(ERRFLAG_PUSH_FNAME) RULES_LIST%ADD_RULE( TRIM(ADJUSTL(THIS%RULE_FNAME_)), HOOKS )

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
    CASE(ERRFLAG_FNAME_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Rule name not allocated' )
    CASE(ERRFLAG_PUSH_FNAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to push fname' )
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


END FUNCTION RULE_GET_FNAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_PRINT'
PP_THREAD_SAFE FUNCTION RULE_PRINT( THIS, UNIT, OFFSET, HOOKS, SEPARATOR ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(NESTED_RULE_T),         INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=*), OPTIONAL,   INTENT(IN)    :: SEPARATOR

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Locl variables
  CHARACTER(LEN=32) :: CRULE_ID
  INTEGER(KIND=JPIB_K) :: IOERR

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILTER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULENAME_NOT_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_FILTER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IO_ERROR=4_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%FILTER_), ERRFLAG_FILTER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ALLOCATED(THIS%RULE_FNAME_), ERRFLAG_RULENAME_NOT_ALLOCATED )

  !> Print the rule
  WRITE(CRULE_ID, '(I32)', IOSTAT=IOERR ) THIS%ID_
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_IO_ERROR )

  !> Open rule
  WRITE( UNIT, '(A,A,A,A)', IOSTAT=IOERR ) REPEAT(' ', OFFSET), 'Rule[', TRIM(ADJUSTL(CRULE_ID)), ']: ('
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_IO_ERROR )

  !> Print the name of the rule
  WRITE( UNIT, '(A,A,A,A)', IOSTAT=IOERR ) REPEAT(' ', OFFSET+2), 'Rule name: "', TRIM(THIS%RULE_FNAME_), '"; ...'
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_IO_ERROR )
  WRITE( UNIT, '(A)', IOSTAT=IOERR ) ' '
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_IO_ERROR )

  !> Print the filter
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_FILTER) THIS%FILTER_%PRINT( UNIT, OFFSET+2, HOOKS )

  !> Close rule
  IF ( PRESENT(SEPARATOR) ) THEN
    WRITE( UNIT, '(A,A,A)', IOSTAT=IOERR ) REPEAT(' ', OFFSET), ')', TRIM(ADJUSTL(SEPARATOR))
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_IO_ERROR )
  ELSE
    WRITE( UNIT, '(A,A)', IOSTAT=IOERR ) REPEAT(' ', OFFSET), ')'
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_IO_ERROR )
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
    CASE (ERRFLAG_FILTER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Filter not associated' )
    CASE (ERRFLAG_RULENAME_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Rule name not allocated' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the filter' )
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

END FUNCTION RULE_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_FREE'
PP_THREAD_SAFE FUNCTION RULE_FREE( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,  ONLY: JPIB_K
  USE :: HOOKS_MOD,          ONLY: HOOKS_T
  USE :: FILTER_FACTORY_MOD, ONLY: DESTROY_FILTER

  ! Symbols imported from other libraries
  USE  :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(NESTED_RULE_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_FILTER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_ENCODER=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILTER_NOT_ASSOCIATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_NOT_ASSOCIATED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_RULE_FNAME=5_JPIB_K


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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%FILTER_), ERRFLAG_FILTER_NOT_ASSOCIATED )

  !> Reset the name of the rule
  IF ( ALLOCATED(THIS%RULE_FNAME_) ) THEN
    DEALLOCATE( THIS%RULE_FNAME_, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE_RULE_FNAME )
  ENDIF

  !> Destroy the filters
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DESTROY_FILTER) DESTROY_FILTER( THIS%FILTER_, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_DESTROY_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to destroy the filter' )
    CASE(ERRFLAG_UNABLE_TO_DESTROY_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to destroy the encoder' )
    CASE(ERRFLAG_FILTER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Filter not associated' )
    CASE(ERRFLAG_ENCODER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder not associated' )
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE_RULE_FNAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate the sample' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( ERRMSG )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STATUS)
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

END FUNCTION RULE_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE NESTED_RULE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
