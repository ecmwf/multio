! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'encoding_rule_collection_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ENCODING_RULE_COLLECTION_MOD'
MODULE ENCODING_RULE_COLLECTION_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: ENCODING_RULE_MOD,        ONLY: ENCODING_RULE_CONTAINER_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FILTER_OPTIONS_MOD,       ONLY: FILTER_OPTIONS_T

IMPLICIT NONE

PRIVATE

TYPE :: ENCODING_RULE_COLLECTION_T

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
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: MATCH => RULE_MATCH

  !> Free all the memory allocated by the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: PRINT => RULE_PRINT

  !> Free all the memory allocated by the rule
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: FREE => RULE_FREE

END TYPE

!> Whitelist of public symbols
PUBLIC :: ENCODING_RULE_COLLECTION_T

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RULE_INIT'
PP_THREAD_SAFE FUNCTION RULE_INIT( THIS, CFG, FILTER_OPT, ENCODER_OPT, HOOKS ) RESULT(RET)

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
  CLASS(ENCODING_RULE_COLLECTION_T),    INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),           INTENT(IN)    :: CFG
  TYPE(FILTER_OPTIONS_T),       TARGET, INTENT(IN)    :: FILTER_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T), TARGET, INTENT(IN)    :: ENCODER_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

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
  PP_TRYCALL(ERRFLAG_MAKE_ENCODING_RULE) MAKE_ENCODING_RULES( THIS%RULES_, ENCODING_RULES_CONFIGURATION, FILTER_OPT, ENCODER_OPT, HOOKS )

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
#define PP_PROCEDURE_NAME 'RULE_MATCH'
PP_THREAD_SAFE FUNCTION RULE_MATCH( THIS, MSG, PAR, METADATA, ENCODERS, OPT, HOOKS ) RESULT(RET)

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
  USE :: SAMPLE_MOD,               ONLY: SAMPLE_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_FACTORY_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENCODING_RULE_COLLECTION_T),             INTENT(IN)    :: THIS
  TYPE(PARAMETRIZATION_T),                       INTENT(IN)    :: PAR
  TYPE(FORTRAN_MESSAGE_T),                       INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER,               INTENT(IN)    :: METADATA
  TYPE(CACHED_ENCODER_T), DIMENSION(:), POINTER, INTENT(OUT)   :: ENCODERS
  TYPE(GRIB_ENCODER_OPTIONS_T),                  INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: MATCH
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: NUM_MATCHES
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CLASS(GRIB_SECTION_BASE_A), POINTER :: ENCODER
  TYPE(SAMPLE_T), POINTER :: SAMPLE
  CHARACTER(LEN=256) :: TAG
  CHARACTER(LEN=256) :: NAME
  CHARACTER(LEN=:), ALLOCATABLE :: JSON
  TYPE(GRIB_SECTION_FACTORY_T) :: ENCODER_FACTORY

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILTER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_FILTERS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INIT_ENCODING_INFO=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNEXPECTED_NUMBER_OF_MATCHES=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCHES_ALLOCATION_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_ENCODERS=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_IMPLEMENTED_YET=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_TO_JSON=8_JPIB_K

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

  ! Initialize the sample to null
  SAMPLE => NULL()

  !> Initialize the encoding info
  IF ( NUM_MATCHES .EQ. 0 ) THEN

    ! Extract the Json of the message
    PP_TRYCALL(ERRFLAG_MARS_TO_JSON) MSG%TO_JSON( JSON, HOOKS )
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_IMPLEMENTED_YET )

    !> On the fly create the encoder using msg and par
    !> Allocate the encoding info to be output
    ALLOCATE( ENCODERS(1), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_MATCHES_ALLOCATION_ERROR )

    !> Lazy construction of the encoder
    PP_TRYCALL(ERRFLAG_INIT_ENCODING_INFO) MAKE_ENCODER( ENCODER, MSG, PAR, OPT, ENCODER_FACTORY, HOOKS )
    TAG = REPEAT(' ',256)
    TAG = 'Lazy encoder'
    NAME = REPEAT(' ',256)
    NAME = 'default-encoder'
    !> Initialize the encoding info
    PP_TRYCALL(ERRFLAG_INIT_ENCODING_INFO) ENCODERS(1)%INIT( MSG, PAR, TAG, NAME, METADATA, SAMPLE, ENCODER, .TRUE., OPT, HOOKS )

  ELSEIF ( NUM_MATCHES .GT. 0 ) THEN

    !> Allocate the encoding info to be output
    ALLOCATE( ENCODERS(NUM_MATCHES), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_MATCHES_ALLOCATION_ERROR )

    !> Initialize encoding info
    DO I = 1, NUM_MATCHES
      NULLIFY(ENCODER)
      NULLIFY(SAMPLE)
      TAG = REPEAT(' ',256)
      NAME = REPEAT(' ',256)
      PP_TRYCALL(ERRFLAG_GET_ENCODERS) THIS%RULES_(THIS%MATCHES_(I))%RULE_%GET_ENCODER( NAME, TAG, SAMPLE, ENCODER, HOOKS )
      PP_TRYCALL(ERRFLAG_INIT_ENCODING_INFO) ENCODERS(I)%INIT( MSG, PAR, TAG, NAME, METADATA, SAMPLE, ENCODER, .FALSE., OPT, HOOKS )
      THIS%MATCHES_(I) = -1_JPIB_K
    ENDDO

    ! PAranoid operation
    NUM_MATCHES = 0_JPIB_K

  ELSE

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
    CASE(ERRFLAG_INIT_ENCODING_INFO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the encoding info' )
    CASE(ERRFLAG_UNEXPECTED_NUMBER_OF_MATCHES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unexpected number of matches' )
    CASE(ERRFLAG_MATCHES_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate the matches array' )
    CASE(ERRFLAG_GET_ENCODERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the encoders' )
    CASE(ERRFLAG_NOT_IMPLEMENTED_YET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'No matching rule found' )
      IF ( ALLOCATED(JSON) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( TRIM(ADJUSTL(JSON)) )
        DEALLOCATE(JSON, STAT=DEALLOC_STATUS)
      ENDIF
    CASE (ERRFLAG_MARS_TO_JSON)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert MARS to JSON' )
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
  CLASS(ENCODING_RULE_COLLECTION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),              INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),              INTENT(IN)    :: OFFSET
  TYPE(GRIB_ENCODER_OPTIONS_T),      INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

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
  CLASS(ENCODING_RULE_COLLECTION_T), INTENT(INOUT) :: THIS
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



END MODULE ENCODING_RULE_COLLECTION_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
