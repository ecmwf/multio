! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'mapping_rules_collection_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MAPPING_RULES_COLLECTION_MOD'
MODULE MAPPING_RULES_COLLECTION_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,  ONLY: JPIB_K
  USE :: MAPPING_RULE_MOD,   ONLY: MAPPING_RULE_CONTAINER_T
  USE :: FILTER_OPTIONS_MOD, ONLY: FILTER_OPTIONS_T

IMPLICIT NONE

PRIVATE

TYPE :: MAPPING_RULES_COLLECTION_T

  !> Default visibility of the module
  PRIVATE

  !> Encoder rule
  TYPE(MAPPING_RULE_CONTAINER_T), DIMENSION(:), POINTER :: RULES_ => NULL()

  !> Temporary arry of matching rules
  INTEGER(KIND=JPIB_K), DIMENSION(:), POINTER :: MATCHES_ => NULL()

CONTAINS

  !> Initialize the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: INIT => RULE_INIT

  !> Match the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: MATCH => RULE_MATCH

  !> Free all the memory allocated by the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: PRINT => RULE_PRINT

  !> Free all the memory allocated by the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: FREE => RULE_FREE

END TYPE

!> Whitelist of public symbols
PUBLIC :: MAPPING_RULES_COLLECTION_T

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_INIT'
PP_THREAD_SAFE FUNCTION RULE_INIT( THIS, CFG, FILTER_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,           ONLY: JPIB_K
  USE :: HOOKS_MOD,                   ONLY: HOOKS_T
  USE :: FILTER_OPTIONS_MOD,          ONLY: FILTER_OPTIONS_T
  USE :: MAPPING_RULES_FACTORY_MOD,   ONLY: MAKE_MAPPING_RULES
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATIONS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATIONS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(MAPPING_RULES_COLLECTION_T),   INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),          INTENT(IN)    :: CFG
  TYPE(FILTER_OPTIONS_T),      TARGET, INTENT(IN)    :: FILTER_OPT
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(YAML_CONFIGURATIONS_T) :: MAPPING_RULES_CONFIGURATION
  LOGICAL :: HAS_MAPPING_RULES
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECTIONS_UNDEFINED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAKE_MAPPING_RULE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DELETE_CONFIGURATIONS=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULES_ALREADY_INITIALIZED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCHES_ALREADY_INITIALIZED=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCHES_ALLOCATION_ERROR=8_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%RULES_), ERRFLAG_RULES_ALREADY_INITIALIZED )
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%MATCHES_), ERRFLAG_MATCHES_ALREADY_INITIALIZED )

  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'mapping-rules', HAS_MAPPING_RULES, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_MAPPING_RULES, ERRFLAG_SECTIONS_UNDEFINED )

  !> Read all the subconfigurations
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATIONS( CFG, 'mapping-rules', MAPPING_RULES_CONFIGURATION, HOOKS )

  !> Deallocate section configuration
  PP_TRYCALL(ERRFLAG_MAKE_MAPPING_RULE) MAKE_MAPPING_RULES( THIS%RULES_, MAPPING_RULES_CONFIGURATION, FILTER_OPT, HOOKS )

  !> Destroy the configuration object
  PP_TRYCALL(ERRFLAG_DELETE_CONFIGURATIONS) YAML_DELETE_CONFIGURATIONS( MAPPING_RULES_CONFIGURATION, HOOKS )

  !> Allocate and initialize the matches array
  ALLOCATE( THIS%MATCHES_(SIZE(THIS%RULES_)), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_MATCHES_ALLOCATION_ERROR )

  !> Initialize the matches array
  DO I = 1, SIZE(THIS%MATCHES_)
    THIS%MATCHES_(I) = -1_JPIB_K
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
    CASE(ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the configuration' )
    CASE(ERRFLAG_SECTIONS_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The configuration does not have the "rules" key' )
    CASE(ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the subconfigurations' )
    CASE(ERRFLAG_MAKE_MAPPING_RULE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to make the rules' )
    CASE(ERRFLAG_DELETE_CONFIGURATIONS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to delete the configurations' )
    CASE(ERRFLAG_RULES_ALREADY_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Rules already initialized' )
    CASE(ERRFLAG_MATCHES_ALREADY_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Matches already initialized' )
    CASE(ERRFLAG_MATCHES_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate the matches array' )
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


END FUNCTION RULE_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_MATCH'
PP_THREAD_SAFE FUNCTION RULE_MATCH( THIS, MSG, PAR, FILTER_OPT, MAPPING_INFO, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: ASSIGNMENT_BASE_MOD, ONLY: ASSIGNMENT_BASE_A
  USE :: FILTER_OPTIONS_MOD,  ONLY: FILTER_OPTIONS_T
  USE :: CASHED_MAPPER_MOD,   ONLY: CASHED_MAPPER_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(MAPPING_RULES_COLLECTION_T),            INTENT(IN)    :: THIS
  TYPE(PARAMETRIZATION_T),                      INTENT(IN)    :: PAR
  TYPE(FORTRAN_MESSAGE_T),                      INTENT(IN)    :: MSG
  TYPE(CASHED_MAPPER_T), POINTER, DIMENSION(:), INTENT(OUT)   :: MAPPING_INFO
  TYPE(FILTER_OPTIONS_T), TARGET,               INTENT(IN)    :: FILTER_OPT
  TYPE(HOOKS_T),                                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: MATCH
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: NUM_MATCHES
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CLASS(ASSIGNMENT_BASE_A), POINTER :: ASSIGNMENT
  CHARACTER(LEN=256) :: TAG
  CHARACTER(LEN=256) :: NAME

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILTER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_FILTERS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INIT_MAPING_INFO=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNEXPECTED_NUMBER_OF_MATCHES=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCHES_ALLOCATION_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_ASSIGNMENTS=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%RULES_), ERRFLAG_FILTER_NOT_ASSOCIATED )

  !> Match the filter
  NUM_MATCHES = 0_JPIB_K
  DO I = 1, SIZE(THIS%RULES_)
    PP_TRYCALL(ERRFLAG_MATCH_FILTERS) THIS%RULES_(I)%RULE_%MATCH( MSG, PAR, MATCH, HOOKS )
    IF ( MATCH ) THEN
      NUM_MATCHES = NUM_MATCHES + 1
      THIS%MATCHES_(NUM_MATCHES) = I
    ENDIF
  ENDDO

  !> Initialize the encoding info
  IF ( NUM_MATCHES .EQ. 0 ) THEN

    ! Default constructor is a copy constructor allocated on the fly
    ALLOCATE( MAPPING_INFO(1), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_MATCHES_ALLOCATION_ERROR )
    PP_TRYCALL(ERRFLAG_INIT_MAPING_INFO) MAPPING_INFO(1)%INIT( HOOKS )

  ELSEIF ( NUM_MATCHES .GT. 0 ) THEN

    !> Allocate the encoding info to be output
    ALLOCATE( MAPPING_INFO(NUM_MATCHES), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_MATCHES_ALLOCATION_ERROR )

    !> Initialize encoding info
    DO I = 1, NUM_MATCHES
      NULLIFY(ASSIGNMENT)
      TAG = REPEAT(' ',256)
      PP_TRYCALL(ERRFLAG_GET_ASSIGNMENTS) THIS%RULES_(THIS%MATCHES_(I))%RULE_%GET_ASSIGNMENT( ASSIGNMENT, TAG, NAME, HOOKS )
      PP_TRYCALL(ERRFLAG_INIT_MAPING_INFO) MAPPING_INFO(I)%INIT( ASSIGNMENT, TAG, NAME, HOOKS )
      THIS%MATCHES_(I) = -1_JPIB_K
    ENDDO

    ! PAranoid operation
    NUM_MATCHES = 0_JPIB_K

  ELSEIF ( NUM_MATCHES .LT. 0 ) THEN

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNEXPECTED_NUMBER_OF_MATCHES)

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
    CASE(ERRFLAG_FILTER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Filter not associated' )
    CASE(ERRFLAG_MATCH_FILTERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to match the filters' )
    CASE(ERRFLAG_INIT_MAPING_INFO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the encoding info' )
    CASE(ERRFLAG_UNEXPECTED_NUMBER_OF_MATCHES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unexpected number of matches' )
    CASE(ERRFLAG_MATCHES_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate the matches array' )
    CASE(ERRFLAG_GET_ASSIGNMENTS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the encoders' )
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


END FUNCTION RULE_MATCH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_PRINT'
PP_THREAD_SAFE FUNCTION RULE_PRINT( THIS, UNIT, OFFSET, HOOKS ) RESULT(RET)

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

  !> Dummy arguments
  CLASS(MAPPING_RULES_COLLECTION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),              INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),              INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULES_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT=2_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%RULES_), ERRFLAG_RULES_NOT_ASSOCIATED )

  !> Print the contained rules
  DO I = 1, SIZE(THIS%RULES_)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT) THIS%RULES_(I)%RULE_%PRINT( UNIT, OFFSET, HOOKS )
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
    CASE(ERRFLAG_RULES_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Rules not associated' )
    CASE(ERRFLAG_UNABLE_TO_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the rule' )
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

END FUNCTION RULE_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_FREE'
PP_THREAD_SAFE FUNCTION RULE_FREE( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: MAPPING_RULES_FACTORY_MOD, ONLY: DESTROY_MAPPING_RULES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(MAPPING_RULES_COLLECTION_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_RULES_DEALLOCATION_ERROR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_ASSIGNMENT=2_JPIB_K

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
  IF ( ASSOCIATED(THIS%RULES_) ) THEN
    PP_TRYCALL( ERRFLAG_MAPPING_RULES_DEALLOCATION_ERROR ) DESTROY_MAPPING_RULES( THIS%RULES_, HOOKS )
  ENDIF

  IF ( ASSOCIATED(THIS%MATCHES_) ) THEN
    DEALLOCATE( THIS%MATCHES_, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_MAPPING_RULES_DEALLOCATION_ERROR )
    NULLIFY(THIS%MATCHES_)
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
    CASE(ERRFLAG_MAPPING_RULES_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to destroy the encoding rules' )
    CASE(ERRFLAG_UNABLE_TO_DESTROY_ASSIGNMENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to destroy the encoder' )
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

END FUNCTION RULE_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE MAPPING_RULES_COLLECTION_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
