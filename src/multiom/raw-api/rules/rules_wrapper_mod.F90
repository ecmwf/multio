! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'rules_wrapper_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'RULES_WRAPPER_MOD'
MODULE RULES_WRAPPER_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

  ! Symbols imported from other modules within the project.
  USE :: NESTED_RULE_COLLECTION_MOD, ONLY: NESTED_RULE_COLLECTION_T

IMPLICIT NONE

  !> Default module visibility
  PRIVATE

  !> RULES index used used to identify the rules wrapper
  INTEGER(KIND=JPIB_K), PARAMETER :: RULES_IDX_E = 1_JPIB_K
  CHARACTER(LEN=*), PARAMETER :: RULES_NAME_E = 'raw-rules'

  !> Class used as a wrapper for the rules
  TYPE :: RULES_CONTAINER_T
    TYPE(NESTED_RULE_COLLECTION_T), POINTER :: RULES => NULL()
  END TYPE

  !> Whitelist of public symbols
  PUBLIC :: RULES_IDX_E
  PUBLIC :: RULES_NAME_E
  PUBLIC :: CREATE_RULES
  PUBLIC :: EXTRACT_RULES
  PUBLIC :: INIT_RULES
  PUBLIC :: SEARCH_RULE
  PUBLIC :: PRINT_RULES
  PUBLIC :: FREE_RULES


CONTAINS



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INIT_RULES'
PP_THREAD_SAFE FUNCTION INIT_RULES( WRAPPED_RULES, FNAME, FILTER_OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,                ONLY: JPIB_K
  USE :: HOOKS_MOD,                        ONLY: HOOKS_T
  USE :: NESTED_RULE_COLLECTION_MOD,       ONLY: NESTED_RULE_COLLECTION_T
  USE :: FILTER_OPTIONS_MOD,               ONLY: FILTER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),            INTENT(INOUT) :: WRAPPED_RULES
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  TYPE(FILTER_OPTIONS_T), INTENT(IN)    :: FILTER_OPT
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(NESTED_RULE_COLLECTION_T), POINTER :: RULES

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULES_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_RULES=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INITIALIZE_RULES=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INJECT_CHECKSUM=5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( C_ASSOCIATED(WRAPPED_RULES), ERRFLAG_WRAPPER_ALREADY_ASSOCIATED )

  !> Initialize the encoder
  RULES => NULL()

  !> Create the encoder
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CREATE_RULES) CREATE_RULES( &
&       WRAPPED_RULES, RULES, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(RULES), ERRFLAG_RULES_NOT_ASSOCIATED )

  !> Initialize the encoder
   PP_TRYCALL(ERRFLAG_INITIALIZE_RULES) RULES%INIT(&
&        TRIM(ADJUSTL(FNAME)), FILTER_OPT, HOOKS )

  ! Inject checksum
  PP_TRYCALL(ERRFLAG_INJECT_CHECKSUM) INJECT_CHECKSUM_RULES( WRAPPED_RULES, HOOKS )


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
    CASE (ERRFLAG_WRAPPER_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper already associated' )
    CASE (ERRFLAG_RULES_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Rules not associated' )
    CASE (ERRFLAG_UNABLE_TO_CREATE_RULES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create rules' )
    CASE (ERRFLAG_INITIALIZE_RULES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize rules' )
    CASE (ERRFLAG_INJECT_CHECKSUM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inject checksum' )
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

END FUNCTION INIT_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SEARCH_RULE'
PP_THREAD_SAFE FUNCTION SEARCH_RULE( WRAPPED_RULES, &
&  F_MARS_DICT, NAME, TAG, SAMPLE, RULE, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIM_K
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD,        ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,        ONLY: PARAMETRIZATION_T
  USE :: NESTED_RULE_COLLECTION_MOD, ONLY: NESTED_RULE_COLLECTION_T

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
  TYPE(C_PTR),                INTENT(IN)    :: WRAPPED_RULES
  TYPE(FORTRAN_MESSAGE_T),    INTENT(IN)    :: F_MARS_DICT
  CHARACTER(LEN=*),           INTENT(OUT)   :: NAME
  CHARACTER(LEN=*),           INTENT(OUT)   :: TAG
  CHARACTER(LEN=*),           INTENT(OUT)   :: SAMPLE
  TYPE(YAML_CONFIGURATION_T), INTENT(OUT)   :: RULE
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(NESTED_RULE_COLLECTION_T), POINTER :: RULES
  INTEGER(KIND=JPIB_K) :: NUM_MATCHES
  CHARACTER(LEN=:), ALLOCATABLE :: JSON_MSG
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_RULES=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_COUNT_MATCHES=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEARCH_RULE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_MATCHES_COUNT=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_MATCHING_RULE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GENERATE_JSON=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIPLE_MATCHING_RULES=7_JPIB_K

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

#if 0
  ! Paranoid initialization of output arguments
  IF ( ALLOCATED(JSON_MSG) ) THEN
    DEALLOCATE( JSON_MSG, STAT=ALLOC_STAT )
  ENDIF

  ! Extract the rules
  PP_TRYCALL(ERRFLAG_EXTRACT_RULES) EXTRACT_RULES( WRAPPED_RULES, RULES, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(RULES), ERRFLAG_EXTRACT_RULES )

  ! Count number of matching rules
  NUM_MATCHES = 0_JPIB_K
  PP_TRYCALL(ERRFLAG_COUNT_MATCHES) RULES%COUNT_MATCHES( F_MARS_DICT, &
&  NUM_MATCHES, HOOKS )

  ! Search rule
  IF ( NUM_MATCHES .LT. 0_JPIB_K ) THEN
    ! Get the message as json for error handling
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GENERATE_JSON) F_MARS_DICT%TO_JSON( &
      JSON_MSG, HOOKS )
    ! Throw error
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_WRONG_MATCHES_COUNT )
  ELSEIF ( NUM_MATCHES .EQ. 0_JPIB_K ) THEN
    ! Get the message as json for error handling
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GENERATE_JSON) F_MARS_DICT%TO_JSON( &
      JSON_MSG, HOOKS )
    ! Throw error
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_MATCHING_RULE )
  ELSEIF ( NUM_MATCHES .EQ. 1_JPIB_K ) THEN
    ! Call the encoder
    PP_TRYCALL(ERRFLAG_SEARCH_RULE) RULES%SEARCH( F_MARS_DICT, &
&     NAME, TAG, SAMPLE, RULE, HOOKS )
  ELSE
    ! Get the message as json for error handling
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GENERATE_JSON) F_MARS_DICT%TO_JSON( &
      JSON_MSG, HOOKS )
    ! Get the list of multiple matching rules
    !! TODO
    ! Throw error
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_MULTIPLE_MATCHING_RULES )
  ENDIF
#endif

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
    CASE (ERRFLAG_EXTRACT_RULES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract rules' )
    CASE (ERRFLAG_COUNT_MATCHES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to count matches' )
    CASE (ERRFLAG_SEARCH_RULE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to search rule' )
    CASE (ERRFLAG_WRONG_MATCHES_COUNT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong matches count' )
      IF ( ALLOCATED(JSON_MSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ADJUSTL(JSON_MSG)) )
        DEALLOCATE( JSON_MSG, STAT=ALLOC_STAT )
      END IF
    CASE (ERRFLAG_NO_MATCHING_RULE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'No matching rule found for message' )
      IF ( ALLOCATED(JSON_MSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ADJUSTL(JSON_MSG)) )
        DEALLOCATE( JSON_MSG, STAT=ALLOC_STAT )
      END IF
    CASE (ERRFLAG_UNABLE_TO_GENERATE_JSON)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to generate json for message' )
      IF ( ALLOCATED(JSON_MSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ADJUSTL(JSON_MSG)) )
        DEALLOCATE( JSON_MSG, STAT=ALLOC_STAT )
      END IF
    CASE (ERRFLAG_MULTIPLE_MATCHING_RULES)
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

END FUNCTION SEARCH_RULE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_RULES'
PP_THREAD_SAFE FUNCTION PRINT_RULES( WRAPPED_RULES, &
&   OFFSET, UNIT, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),          INTENT(IN)    :: WRAPPED_RULES
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: OFFSET
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(NESTED_RULE_COLLECTION_T), POINTER :: RULES

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_RULES=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEARCH_RULE=2_JPIB_K

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

  ! Extract the encoder
  PP_TRYCALL(ERRFLAG_EXTRACT_RULES) EXTRACT_RULES( WRAPPED_RULES, RULES, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(RULES), ERRFLAG_EXTRACT_RULES )

  ! Call the encoder
  PP_TRYCALL(ERRFLAG_SEARCH_RULE) RULES%PRINT( OFFSET, UNIT, HOOKS )

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
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_EXTRACT_RULES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract rules' )
    CASE (ERRFLAG_SEARCH_RULE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to search rule' )
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

END FUNCTION PRINT_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FREE_RULES'
PP_THREAD_SAFE FUNCTION FREE_RULES( WRAPPED_RULES, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,         ONLY: JPIB_K
  USE :: HOOKS_MOD,                 ONLY: HOOKS_T
  USE ::NESTED_RULE_COLLECTION_MOD, ONLY: NESTED_RULE_COLLECTION_T
  USE :: API_F_C_WRAPPER_MOD,       ONLY: F_C_WRAPPER_T
  USE :: API_F_C_WRAPPER_MOD,       ONLY: F_C_EXTRACT_WRAPPER
  USE :: API_F_C_WRAPPER_MOD,       ONLY: F_C_FREE_WRAPPER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),   INTENT(INOUT) :: WRAPPED_RULES
  TYPE(HOOKS_T), INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(F_C_WRAPPER_T), POINTER, DIMENSION(:) :: WRAPPER
  INTEGER(KIND=C_INT8_T), POINTER, DIMENSION(:) :: BUFFER
  INTEGER(KIND=JPIB_K) :: DIM
  TYPE(RULES_CONTAINER_T) :: RULES_CONTAINER
  TYPE(NESTED_RULE_COLLECTION_T), POINTER :: RULES
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local prameters
  INTEGER(KIND=JPIB_K), PARAMETER :: RAW_RULES_CONTAINER_BYTE_SIZE = &
&   STORAGE_SIZE(RULES_CONTAINER) / 8_JPIB_K

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_BUFFER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_BUFFER_NOT_ASSOCIATED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_BUFFER_SIZE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULES_NOT_ASSOCIATED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_RULES=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_RULES=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_WRAPPER=9_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(WRAPPED_RULES), ERRFLAG_WRAPPER_NOT_ASSOCIATED )

  ! Extract the wrapper
  WRAPPER => NULL()
  CALL C_F_POINTER( WRAPPED_RULES, WRAPPER, [1] )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(WRAPPER), ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER )

  ! Extract the buffer
  BUFFER => NULL()
  DIM = 0_JPIB_K
  PP_TRYCALL(ERRFLAG_UNABLE_TO_EXTRACT_BUFFER) F_C_EXTRACT_WRAPPER( &
&   WRAPPER, BUFFER, DIM, HOOKS )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(BUFFER), ERRFLAG_BUFFER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( DIM .NE. RAW_RULES_CONTAINER_BYTE_SIZE, ERRFLAG_WRONG_BUFFER_SIZE )

  ! Get the Container
  RULES_CONTAINER = TRANSFER( BUFFER, RULES_CONTAINER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(RULES_CONTAINER%RULES), ERRFLAG_RULES_NOT_ASSOCIATED )

  ! Associate the encoder pointer
  RULES => RULES_CONTAINER%RULES

  ! Print the encoder to be freed
  !! MIVAL: This is a debug print statement that should be enable to debug the c/fortran interoperability
  !! PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_ENCODER) ENCODER%PRINT( 6_JPIB_K, 0_JPIB_K, HOOKS )
  ! Free the encoder
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_RULES) RULES%FREE( HOOKS )

  ! Deallocate the encoder
  DEALLOCATE( RULES_CONTAINER%RULES, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0_JPIB_K, ERRFLAG_UNABLE_TO_DEALLOCATE_RULES )
  RULES_CONTAINER%RULES => NULL()
  RULES => NULL()

  ! Error handling
  BUFFER = 0_C_INT8_T

  ! Free the wrapper
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_WRAPPER) F_C_FREE_WRAPPER( &
&   WRAPPER, HOOKS )
  WRAPPER => NULL()
  BUFFER => NULL()

  ! Reset the c pointer
  WRAPPED_RULES = C_NULL_PTR

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_WRAPPER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper not associated' )
    CASE (ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract wrapper' )
    CASE (ERRFLAG_UNABLE_TO_EXTRACT_BUFFER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract buffer' )
    CASE (ERRFLAG_BUFFER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Buffer not associated' )
    CASE (ERRFLAG_WRONG_BUFFER_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong buffer size' )
    CASE (ERRFLAG_RULES_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Rules not associated' )
    CASE (ERRFLAG_UNABLE_TO_FREE_RULES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free rules' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE_RULES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate rules' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=ALLOC_STAT )
      END IF
    CASE (ERRFLAG_UNABLE_TO_FREE_WRAPPER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free wrapper' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION FREE_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


! ======================================================================

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CREATE_RULES'
PP_THREAD_SAFE FUNCTION CREATE_RULES( WRAPPED_RULE, RULES, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: NESTED_RULE_COLLECTION_MOD, ONLY: NESTED_RULE_COLLECTION_T
  USE :: API_F_C_WRAPPER_MOD,        ONLY: F_C_WRAPPER_T
  USE :: API_F_C_WRAPPER_MOD,        ONLY: F_C_ALLOCATE_WRAPPER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),                             INTENT(INOUT) :: WRAPPED_RULE
  TYPE(NESTED_RULE_COLLECTION_T), POINTER, INTENT(INOUT) :: RULES
  TYPE(HOOKS_T),                           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(RULES_CONTAINER_T) :: RULES_CONTAINER
  TYPE(F_C_WRAPPER_T),    POINTER, DIMENSION(:) :: WRAPPER
  INTEGER(KIND=C_INT8_T), POINTER, DIMENSION(:) :: BUFFER
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local prameters
  INTEGER(KIND=JPIB_K), PARAMETER :: RULES_CONTAINER_BYTE_SIZE = &
&   STORAGE_SIZE(RULES_CONTAINER) / 8_JPIB_K

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULES_ALREADY_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_RULES=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_WRAPPER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_NOT_ASSOCIATED_AFTER_ALLOCATION=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_BUFFER_NOT_ASSOCIATED_AFTER_ALLOCATION=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( C_ASSOCIATED(WRAPPED_RULE), ERRFLAG_WRAPPER_ALREADY_ASSOCIATED)
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(RULES), ERRFLAG_RULES_ALREADY_ASSOCIATED)

  ! Allocate the rules
  ALLOCATE( RULES_CONTAINER%RULES, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0_JPIB_K, ERRFLAG_UNABLE_TO_ALLOCATE_RULES )

  ! Allocate the wrapper
  WRAPPER => NULL()
  BUFFER => NULL()
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE_WRAPPER) F_C_ALLOCATE_WRAPPER( &
&   WRAPPER, BUFFER, RULES_IDX_E, RULES_CONTAINER_BYTE_SIZE, HOOKS )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(WRAPPER), ERRFLAG_WRAPPER_NOT_ASSOCIATED_AFTER_ALLOCATION)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(BUFFER), ERRFLAG_BUFFER_NOT_ASSOCIATED_AFTER_ALLOCATION)

  ! Wrap the rules in order to be able to expose it to c
  BUFFER = TRANSFER(RULES_CONTAINER, BUFFER, RULES_CONTAINER_BYTE_SIZE )

  ! Set the return arguments
  RULES => RULES_CONTAINER%RULES
  WRAPPED_RULE = C_LOC(WRAPPER)

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
    CASE (ERRFLAG_WRAPPER_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper already associated' )
    CASE (ERRFLAG_RULES_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'RULES already associated' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_RULES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate rules' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=ALLOC_STAT )
      END IF
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_WRAPPER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate wrapper' )
    CASE (ERRFLAG_WRAPPER_NOT_ASSOCIATED_AFTER_ALLOCATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper not associated after allocation' )
    CASE (ERRFLAG_BUFFER_NOT_ASSOCIATED_AFTER_ALLOCATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Buffer not associated after allocation' )
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

END FUNCTION CREATE_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INJECT_CHECKSUM_RULES'
PP_THREAD_SAFE FUNCTION INJECT_CHECKSUM_RULES( WRAPPED_RULE, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: API_F_C_WRAPPER_MOD,        ONLY: F_C_WRAPPER_T
  USE :: API_F_C_WRAPPER_MOD,        ONLY: F_C_INJECT_CHECKSUM_WRAPPER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),   INTENT(INOUT) :: WRAPPED_RULE
  TYPE(HOOKS_T), INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(F_C_WRAPPER_T),    POINTER, DIMENSION(:) :: WRAPPER

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INJECT_CHECKOSUM=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(WRAPPED_RULE), ERRFLAG_WRAPPER_NOT_ASSOCIATED )

  ! Extract the wrapper
  WRAPPER => NULL()
  CALL C_F_POINTER( WRAPPED_RULE, WRAPPER, [1] )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(WRAPPER), ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER )

  ! Extract the buffer
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INJECT_CHECKOSUM) F_C_INJECT_CHECKSUM_WRAPPER( WRAPPER, HOOKS )

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

    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_WRAPPER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper not associated' )
    CASE (ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract wrapper' )
    CASE (ERRFLAG_UNABLE_TO_INJECT_CHECKOSUM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inject checksum' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  RETURN

END FUNCTION INJECT_CHECKSUM_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'EXTRACT_RULES'
PP_THREAD_SAFE FUNCTION EXTRACT_RULES( WRAPPED_RULE, RULES, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: NESTED_RULE_COLLECTION_MOD, ONLY: NESTED_RULE_COLLECTION_T
  USE :: API_F_C_WRAPPER_MOD,        ONLY: F_C_WRAPPER_T
  USE :: API_F_C_WRAPPER_MOD,        ONLY: F_C_EXTRACT_WRAPPER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),                             INTENT(IN)    :: WRAPPED_RULE
  TYPE(NESTED_RULE_COLLECTION_T), POINTER, INTENT(INOUT) :: RULES
  TYPE(HOOKS_T),                           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(F_C_WRAPPER_T),    POINTER, DIMENSION(:) :: WRAPPER
  INTEGER(KIND=C_INT8_T), POINTER, DIMENSION(:) :: BUFFER
  TYPE(RULES_CONTAINER_T) :: RULES_CONTAINER
  INTEGER(KIND=JPIB_K) :: DIM

  !> Local prameters
  INTEGER(KIND=JPIB_K), PARAMETER :: RULES_CONTAINER_BYTE_SIZE = &
&   STORAGE_SIZE(RULES_CONTAINER) / 8_JPIB_K

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULES_ALREADY_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_BUFFER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_BUFFER_NOT_ASSOCIATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_BUFFER_SIZE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULES_NOT_ASSOCIATED=7_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(WRAPPED_RULE), ERRFLAG_WRAPPER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(RULES), ERRFLAG_RULES_ALREADY_ASSOCIATED )

  ! Extract the wrapper
  WRAPPER => NULL()
  CALL C_F_POINTER( WRAPPED_RULE, WRAPPER, [1] )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(WRAPPER), ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER )

  ! Extract the buffer
  BUFFER => NULL()
  DIM = 0_JPIB_K
  PP_TRYCALL(ERRFLAG_UNABLE_TO_EXTRACT_BUFFER) F_C_EXTRACT_WRAPPER( &
&   WRAPPER, BUFFER, DIM, HOOKS )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(BUFFER), ERRFLAG_BUFFER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( DIM .NE. RULES_CONTAINER_BYTE_SIZE, ERRFLAG_WRONG_BUFFER_SIZE )

  ! Get the Container
  RULES_CONTAINER = TRANSFER( BUFFER, RULES_CONTAINER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(RULES_CONTAINER%RULES), ERRFLAG_RULES_NOT_ASSOCIATED )

  ! Associate the rules pointer
  RULES => RULES_CONTAINER%RULES

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

    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_WRAPPER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper not associated' )
    CASE (ERRFLAG_RULES_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'RULES already associated' )
    CASE (ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract wrapper' )
    CASE (ERRFLAG_UNABLE_TO_EXTRACT_BUFFER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract buffer' )
    CASE (ERRFLAG_BUFFER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Buffer not associated' )
    CASE (ERRFLAG_WRONG_BUFFER_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong buffer size' )
    CASE (ERRFLAG_RULES_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'RULES not associated' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  RETURN

END FUNCTION EXTRACT_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE RULES_WRAPPER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME