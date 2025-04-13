! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'encoding_rules_linear_loader_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ENCODING_RULES_LINEAR_LOADER_MOD'
MODULE ENCODING_RULES_LINEAR_LOADER_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: ENCODING_RULE_MOD,        ONLY: ENCODING_RULE_CONTAINER_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FILTER_OPTIONS_MOD,       ONLY: FILTER_OPTIONS_T

IMPLICIT NONE

PRIVATE

TYPE :: ENCODING_RULES_LINEAR_LOADER_T

  !> Default visibility of the module
  PRIVATE

  !> Encoding options
  TYPE(GRIB_ENCODER_OPTIONS_T), POINTER :: ENCODER_OPT_ => NULL()

  !> Filter options
  TYPE(FILTER_OPTIONS_T), POINTER :: FILTER_OPT_ => NULL()

  !> Encoder rule
  TYPE(ENCODING_RULE_CONTAINER_T), DIMENSION(:), POINTER :: RULES_ => NULL()

  !> Temporary arry of matching rules
  INTEGER(KIND=JPIB_K), DIMENSION(:), POINTER :: MATCHES_ => NULL()

CONTAINS

  !> Initialize the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: INIT => RULE_INIT

  !> Match the rule
  PROCEDURE, PASS, PRIVATE, NON_OVERRIDABLE :: MATCH_SINGLE => RULE_MATCH_SINGLE
  PROCEDURE, PASS, PRIVATE, NON_OVERRIDABLE :: MATCH_MULTIPLE => RULE_MATCH_MULTIPLE

  !> Caount the number of matching rules
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: COUNT_MATCHES => RULE_COUNT_MATCHES

  !> Free all the memory allocated by the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: PRINT => RULE_PRINT

  !> Get he size of the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: SIZE => RULE_SIZE

  !> Free all the memory allocated by the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: FREE => RULE_FREE

  !> Generic rule for matching
  GENERIC, PUBLIC :: MATCH => MATCH_SINGLE, MATCH_MULTIPLE

END TYPE

!> Whitelist of public symbols
PUBLIC :: ENCODING_RULES_LINEAR_LOADER_T

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_INIT'
PP_THREAD_SAFE FUNCTION RULE_INIT( THIS, CFG, FILTER_OPT, ENCODER_OPT, HOOKS, NESTING_LEVEL ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD,   ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FILTER_OPTIONS_MOD,         ONLY: FILTER_OPTIONS_T
  USE :: ENCODING_RULES_FACTORY_MOD, ONLY: MAKE_ENCODING_RULES

  USE :: ENCODING_RULES_FACTORY_MOD, ONLY: MAKE_ENCODING_RULES
  ! Symbols imported from other libraries
  USE  :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE  :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATIONS_T
  USE  :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE  :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATIONS
  USE  :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATIONS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENCODING_RULES_LINEAR_LOADER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),            INTENT(IN)    :: CFG
  TYPE(FILTER_OPTIONS_T),       TARGET,  INTENT(IN)    :: FILTER_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T), TARGET,  INTENT(IN)    :: ENCODER_OPT
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K), OPTIONAL,        INTENT(IN)    :: NESTING_LEVEL

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(YAML_CONFIGURATIONS_T) :: ENCODING_RULES_CONFIGURATION
  LOGICAL :: HAS_ENCODING_RULES
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECTIONS_UNDEFINED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAKE_ENCODING_RULE=4_JPIB_K
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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'encoding-rules', HAS_ENCODING_RULES, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_ENCODING_RULES, ERRFLAG_SECTIONS_UNDEFINED )

  !> Read all the subconfigurations
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATIONS( CFG, 'encoding-rules', ENCODING_RULES_CONFIGURATION, HOOKS )

  !> Deallocate section configuration
  IF ( PRESENT(NESTING_LEVEL) ) THEN
    PP_TRYCALL(ERRFLAG_MAKE_ENCODING_RULE) MAKE_ENCODING_RULES( THIS%RULES_, &
&     ENCODING_RULES_CONFIGURATION, FILTER_OPT, ENCODER_OPT, HOOKS, NESTING_LEVEL=NESTING_LEVEL )
  ELSE
    PP_TRYCALL(ERRFLAG_MAKE_ENCODING_RULE) MAKE_ENCODING_RULES( THIS%RULES_, &
      ENCODING_RULES_CONFIGURATION, FILTER_OPT, ENCODER_OPT, HOOKS )
  ENDIF

  !> Destroy the configuration object
  PP_TRYCALL(ERRFLAG_DELETE_CONFIGURATIONS) YAML_DELETE_CONFIGURATIONS( ENCODING_RULES_CONFIGURATION, HOOKS )

  !> Allocate and initialize the matches array
  ALLOCATE( THIS%MATCHES_(SIZE(THIS%RULES_)), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_MATCHES_ALLOCATION_ERROR )

  !> Initialize the matches array
  DO I = 1, SIZE(THIS%MATCHES_)
    THIS%MATCHES_(I) = -1_JPIB_K
  ENDDO

  !> Associate oprions
  THIS%FILTER_OPT_  => FILTER_OPT
  THIS%ENCODER_OPT_ => ENCODER_OPT

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
    CASE(ERRFLAG_MAKE_ENCODING_RULE)
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
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN


END FUNCTION RULE_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_COUNT_MATCHES'
PP_THREAD_SAFE FUNCTION RULE_COUNT_MATCHES( THIS, MSG, PAR, NUM_MATCHES, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENCODING_RULES_LINEAR_LOADER_T), INTENT(IN)    :: THIS
  TYPE(PARAMETRIZATION_T),               INTENT(IN)    :: PAR
  TYPE(FORTRAN_MESSAGE_T),               INTENT(IN)    :: MSG
  INTEGER(KIND=JPIB_K),                  INTENT(OUT)   :: NUM_MATCHES
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: MATCH
  INTEGER(KIND=JPIB_K) :: I

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULES_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_RULE=2_JPIB_K

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

  !> Match the filter
  NUM_MATCHES = 0_JPIB_K
  DO I = 1, SIZE(THIS%RULES_)
    PP_TRYCALL(ERRFLAG_MATCH_RULE) THIS%RULES_(I)%RULE_%MATCH( MSG, PAR, MATCH, HOOKS )
    IF ( MATCH ) THEN
      NUM_MATCHES = NUM_MATCHES + 1
      THIS%MATCHES_(NUM_MATCHES) = I
    ENDIF
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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Filter not associated' )
    CASE(ERRFLAG_MATCH_RULE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to match the filters' )
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


END FUNCTION RULE_COUNT_MATCHES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_MATCH_SINGLE'
PP_THREAD_SAFE FUNCTION RULE_MATCH_SINGLE( THIS, MSG, PAR, RULE, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_BASE_A
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: CACHED_ENCODER_MOD,       ONLY: CACHED_ENCODER_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: GRIB_ENCODER_FACTORY_MOD, ONLY: MAKE_ENCODER
  USE :: ENCODING_RULE_MOD,        ONLY: ENCODING_RULE_T
  USE :: SAMPLE_MOD,               ONLY: SAMPLE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENCODING_RULES_LINEAR_LOADER_T), INTENT(IN)    :: THIS
  TYPE(FORTRAN_MESSAGE_T),               INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),               INTENT(IN)    :: PAR
  TYPE(ENCODING_RULE_T), POINTER,        INTENT(INOUT) :: RULE
  TYPE(GRIB_ENCODER_OPTIONS_T),          INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: MATCH
  INTEGER(KIND=JPIB_K) :: I

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULES_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIPLE_MATCHES=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_RULE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULE_ALREADY_ASSOCIATED=4_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(RULE), ERRFLAG_RULE_ALREADY_ASSOCIATED )

  !> Match the filter
  DO I = 1, SIZE(THIS%RULES_)
    PP_TRYCALL(ERRFLAG_MATCH_RULE) THIS%RULES_(I)%RULE_%MATCH( MSG, PAR, MATCH, HOOKS )
    IF ( MATCH ) THEN
      PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(RULE), ERRFLAG_MULTIPLE_MATCHES )
      RULE => THIS%RULES_(I)%RULE_
    ENDIF
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
    CASE(ERRFLAG_MULTIPLE_MATCHES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Multiple matches' )
    CASE(ERRFLAG_MATCH_RULE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to match the rule' )
    CASE(ERRFLAG_RULE_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Rule already associated' )
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


END FUNCTION RULE_MATCH_SINGLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_MATCH_MULTIPLE'
PP_THREAD_SAFE FUNCTION RULE_MATCH_MULTIPLE( THIS, MSG, PAR, RULES, IRULE, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_BASE_A
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: CACHED_ENCODER_MOD,       ONLY: CACHED_ENCODER_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: ENCODING_RULE_MOD,        ONLY: ENCODING_RULE_CONTAINER_T
  USE :: GRIB_ENCODER_FACTORY_MOD, ONLY: MAKE_ENCODER
  USE :: SAMPLE_MOD,               ONLY: SAMPLE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENCODING_RULES_LINEAR_LOADER_T),                  INTENT(IN)    :: THIS
  TYPE(FORTRAN_MESSAGE_T),                                INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),                                INTENT(IN)    :: PAR
  TYPE(ENCODING_RULE_CONTAINER_T), DIMENSION(:), POINTER, INTENT(INOUT) :: RULES
  INTEGER(KINd=JPIB_K),                                   INTENT(INOUT) :: IRULE
  TYPE(GRIB_ENCODER_OPTIONS_T),                           INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                                          INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET


  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_IMPLEMENTED=1_JPIB_K

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
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_IMPLEMENTED )

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
    CASE(ERRFLAG_NOT_IMPLEMENTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Not implemented' )
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


END FUNCTION RULE_MATCH_MULTIPLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_PRINT'
PP_THREAD_SAFE FUNCTION RULE_PRINT( THIS, UNIT, OFFSET, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENCODING_RULES_LINEAR_LOADER_T), INTENT(IN) :: THIS
  INTEGER(KIND=JPIB_K),                  INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),                  INTENT(IN)    :: OFFSET
  TYPE(GRIB_ENCODER_OPTIONS_T),          INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: N

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
  N = SIZE(THIS%RULES_)
  IF ( N .GT. 1 ) THEN
    DO I = 1, N-1
      PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT) THIS%RULES_(I)%RULE_%PRINT( UNIT, OFFSET, OPT, HOOKS, '; ...' )
    ENDDO
  ENDIF
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT) THIS%RULES_(N)%RULE_%PRINT( UNIT, OFFSET, OPT, HOOKS )

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
PP_THREAD_SAFE FUNCTION RULE_FREE( THIS, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: ENCODING_RULES_FACTORY_MOD, ONLY: DESTROY_ENCODING_RULES
  USE :: GRIB_ENCODER_OPTIONS_MOD,   ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENCODING_RULES_LINEAR_LOADER_T), INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T),      INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODING_RULES_DEALLOCATION_ERROR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_ENCODER=2_JPIB_K

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
    PP_TRYCALL( ERRFLAG_ENCODING_RULES_DEALLOCATION_ERROR ) DESTROY_ENCODING_RULES( THIS%RULES_, OPT, HOOKS )
  ENDIF

  IF ( ASSOCIATED(THIS%MATCHES_) ) THEN
    DEALLOCATE( THIS%MATCHES_, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_ENCODING_RULES_DEALLOCATION_ERROR )
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
    CASE(ERRFLAG_ENCODING_RULES_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to destroy the encoding rules' )
    CASE(ERRFLAG_UNABLE_TO_DESTROY_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to destroy the encoder' )
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


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_SIZE'
PP_THREAD_SAFE FUNCTION RULE_SIZE( THIS, SZ, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENCODING_RULES_LINEAR_LOADER_T), INTENT(IN) :: THIS
  INTEGER(KIND=JPIB_K),                  INTENT(OUT)   :: SZ
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULES_NOT_ASSOCIATED=1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%RULES_), ERRFLAG_RULES_NOT_ASSOCIATED )

  ! Get teh size of the rules
  SZ = SIZE(THIS%RULES_)

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoding rules not associated' )
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

END FUNCTION RULE_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE ENCODING_RULES_LINEAR_LOADER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
