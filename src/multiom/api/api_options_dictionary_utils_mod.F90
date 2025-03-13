! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'api_options_dictionary_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'API_OPTIONS_DICTIONARY_UTILS_MOD'
MODULE API_OPTIONS_DICTIONARY_UTILS_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

TYPE :: API_OPTIONS_T
  CHARACTER(LEN=8192) :: MAPPING_RULES_FNAME = '{MULTIO_INSTALL_DIR}/mappings/mapping-rules.yaml'
  CHARACTER(LEN=8192) :: ENCODING_RULES_FNAME = '{MULTIO_INSTALL_DIR}/encodings/encoding-rules.yaml'
  CHARACTER(LEN=8192) :: SAMPLE_RULES_FNAME = '{MULTIO_INSTALL_DIR}/samples/sample.tmpl'
  CHARACTER(LEN=8192) :: ENCODER_TYPE = REPEAT(' ',1024)  ! For future use
END TYPE

!> Enumerators for the iterator
INTEGER(KIND=JPIB_K), PARAMETER :: OPTIONS_ITERATOR_ENCODING_RULES_FILE = 1_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: OPTIONS_ITERATOR_MAPPING_RULES_FILE = 2_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: OPTIONS_ITERATOR_SAMPLE_PATH = 3_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: OPTIONS_ITERATOR_ENCODER_TYPE = 4_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: OPTIONS_ITERATOR_UNDEF = 5_JPIB_K


! Whitelist of public symbols
PUBLIC :: API_OPTIONS_T
PUBLIC :: OPTIONS_DICTIONARY_MAX_ITERATOR
PUBLIC :: OPTIONS_DICTIONARY_INIT_ITERATOR
PUBLIC :: OPTIONS_DICTIONARY_NAME2ITERATOR
PUBLIC :: OPTIONS_DICTIONARY_GET_NEXT_ITERATOR
PUBLIC :: OPTIONS_DICTIONARY_HAS
PUBLIC :: OPTIONS_DICTIONARY_GET_KEY_AS_STRING
PUBLIC :: OPTIONS_DICTIONARY_GET_VALUE_AS_STRING
PUBLIC :: OPTIONS_DICTIONARY_SET_VALUE_FROM_STRING

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'OPTIONS_DICTIONARY_MAX_ITERATOR'
PP_THREAD_SAFE FUNCTION OPTIONS_DICTIONARY_MAX_ITERATOR( OPTIONS_DICTIONARY, MAX_ITERATOR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD,   ONLY: TOLOWER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(API_OPTIONS_T),  INTENT(IN)    :: OPTIONS_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: MAX_ITERATOR
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
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

  ! Set the maximum iterator
  MAX_ITERATOR = OPTIONS_ITERATOR_UNDEF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
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

END FUNCTION OPTIONS_DICTIONARY_MAX_ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'OPTIONS_DICTIONARY_INIT_ITERATOR'
PP_THREAD_SAFE FUNCTION OPTIONS_DICTIONARY_INIT_ITERATOR( OPTIONS_DICTIONARY, ITERATOR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD,   ONLY: TOLOWER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(API_OPTIONS_T),  INTENT(IN)    :: OPTIONS_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: ITERATOR
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
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

  ! Set the maximum iterator
  ITERATOR = 1_JPIB_K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
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

END FUNCTION OPTIONS_DICTIONARY_INIT_ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'OPTIONS_DICTIONARY_GET_NEXT_ITERATOR'
PP_THREAD_SAFE FUNCTION OPTIONS_DICTIONARY_GET_NEXT_ITERATOR( OPTIONS_DICTIONARY, ITERATOR, END_OF_ITERATORS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,     ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(API_OPTIONS_T),  INTENT(IN)    :: OPTIONS_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: ITERATOR
  LOGICAL,              INTENT(OUT)   :: END_OF_ITERATORS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: HAS_KEY

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HAS_ITERATOR=1_JPIB_K

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

  ! Set the maximum iterator
  HAS_KEY = .FALSE.
  END_OF_ITERATORS = .FALSE.
  DO WHILE(.NOT.HAS_KEY)

    ! Get the next iterator
    ITERATOR = ITERATOR + 1

    ! Check if the iterator is the last one
    IF ( ITERATOR .EQ. OPTIONS_ITERATOR_UNDEF ) THEN
      END_OF_ITERATORS = .TRUE.
      HAS_KEY = .TRUE.
    ELSE
      PP_TRYCALL(ERRFLAG_HAS_ITERATOR) OPTIONS_DICTIONARY_HAS( OPTIONS_DICTIONARY, ITERATOR, HAS_KEY, HOOKS )
    ENDIF

  ENDDO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
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
    CASE(ERRFLAG_HAS_ITERATOR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check if the iterator exists' )
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

END FUNCTION OPTIONS_DICTIONARY_GET_NEXT_ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'OPTIONS_DICTIONARY_NAME2ITERATOR'
PP_THREAD_SAFE FUNCTION OPTIONS_DICTIONARY_NAME2ITERATOR( OPTIONS_DICTIONARY, KEY, ITERATOR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD,   ONLY: TOLOWER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(API_OPTIONS_T),  INTENT(IN)    :: OPTIONS_DICTIONARY
  CHARACTER(LEN=*),     INTENT(IN)    :: KEY
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: ITERATOR
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=LEN(KEY)) :: KEY_LOW

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_OPTIONS_KEY=2_JPIB_K

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

  ! Convert to string to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( KEY, KEY_LOW, HOOKS )

  ! Get the iterator from the key name
  SELECT CASE ( KEY_LOW )

  CASE ( 'encoding-rules', 'encoding-file', 'encoding' )
    ITERATOR = OPTIONS_ITERATOR_ENCODING_RULES_FILE

  CASE (  'mapping-rules', 'mapping-file', 'mapping' )
    ITERATOR = OPTIONS_ITERATOR_MAPPING_RULES_FILE

  CASE ( 'samples-path' )
    ITERATOR = OPTIONS_ITERATOR_SAMPLE_PATH

  CASE ( 'encoder-type' )
    ITERATOR = OPTIONS_ITERATOR_ENCODER_TYPE

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_OPTIONS_KEY )

  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
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
    CASE(ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to lowercase' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'key: '//TRIM(ADJUSTL(KEY)) )
    CASE(ERRFLAG_NO_OPTIONS_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'No OPTIONS key found' )
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

END FUNCTION OPTIONS_DICTIONARY_NAME2ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'OPTIONS_DICTIONARY_HAS'
PP_THREAD_SAFE FUNCTION OPTIONS_DICTIONARY_HAS( OPTIONS_DICTIONARY, ITERATOR, HAS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,     ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(API_OPTIONS_T),  INTENT(IN)    :: OPTIONS_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: ITERATOR
  LOGICAL,              INTENT(OUT)   :: HAS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_OPTIONS_KEY=2_JPIB_K

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

  SELECT CASE ( ITERATOR )

  CASE ( OPTIONS_ITERATOR_ENCODING_RULES_FILE )
    HAS = .FALSE.

  CASE ( OPTIONS_ITERATOR_MAPPING_RULES_FILE )
    HAS = .FALSE.

  CASE ( OPTIONS_ITERATOR_SAMPLE_PATH )
    HAS = .FALSE.

  CASE ( OPTIONS_ITERATOR_ENCODER_TYPE )
    HAS = .FALSE.

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_OPTIONS_KEY )

  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN


! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error variables
    CHARACTER(LEN=32) :: CIT

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_NO_OPTIONS_KEY)
      CIT=REPEAT(' ',32)
      WRITE(CIT,'(I32)') ITERATOR
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid enumerator found' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Iterator value is: '//TRIM(ADJUSTL(CIT)) )
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

END FUNCTION OPTIONS_DICTIONARY_HAS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'OPTIONS_DICTIONARY_GET_KEY_AS_STRING'
PP_THREAD_SAFE FUNCTION OPTIONS_DICTIONARY_GET_KEY_AS_STRING( OPTIONS_DICTIONARY, ITERATOR, KEY, HOOKS ) RESULT(RET)

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

  ! Dummy arguments
  TYPE(API_OPTIONS_T),  INTENT(IN)    :: OPTIONS_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITERATOR
  CHARACTER(LEN=64),    INTENT(OUT)   :: KEY
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_OPTIONS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_ENUM_STRING=3_JPIB_K

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

  ! Initialize the value
  KEY = REPEAT( ' ', LEN(KEY) )

  SELECT CASE ( ITERATOR )

  CASE ( OPTIONS_ITERATOR_ENCODING_RULES_FILE )
    KEY = 'encoding-rules'

  CASE ( OPTIONS_ITERATOR_MAPPING_RULES_FILE )
    KEY = 'mapping-rules'

  CASE ( OPTIONS_ITERATOR_SAMPLE_PATH )
    KEY = 'samples-path'

  CASE ( OPTIONS_ITERATOR_ENCODER_TYPE )
    KEY = 'encoder-type'

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_OPTIONS_KEY )

  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
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
    CASE(ERRFLAG_NO_OPTIONS_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid enumerator found' )
    CASE(ERRFLAG_CONVERT_ENUM_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert enumerator to string' )
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

END FUNCTION OPTIONS_DICTIONARY_GET_KEY_AS_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'OPTIONS_DICTIONARY_GET_VALUE_AS_STRING'
PP_THREAD_SAFE FUNCTION OPTIONS_DICTIONARY_GET_VALUE_AS_STRING( OPTIONS_DICTIONARY, ITERATOR, VALUE, HAS, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,       ONLY: UNDEF_PARAM_E
  USE :: API_GENERAL_UTILS_MOD, ONLY: CONVERT_TO_C_STRING
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD,     ONLY: REPLACE_ENVVAR_IN_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(API_OPTIONS_T),  INTENT(IN)    :: OPTIONS_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITERATOR
  TYPE(C_PTR),          INTENT(OUT)   :: VALUE
  LOGICAL,              INTENT(OUT)   :: HAS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=64) :: TMP_VALUE
  CHARACTER(LEN=8196) :: CTMP

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_REPLACE_ENVVAR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_OPTIONS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_ENUM_STRING=3_JPIB_K

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

  ! Initialize the value
  CTMP = REPEAT( ' ', LEN(CTMP) )

  SELECT CASE ( ITERATOR )

  CASE ( OPTIONS_ITERATOR_ENCODING_RULES_FILE )
    CTMP = REPEAT(' ', 8196)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_REPLACE_ENVVAR) REPLACE_ENVVAR_IN_STRING( TRIM(OPTIONS_DICTIONARY%ENCODING_RULES_FNAME), CTMP, HOOKS )
    PP_TRYCALL(ERRFLAG_CONVERT_ENUM_STRING) CONVERT_TO_C_STRING(CTMP, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( OPTIONS_ITERATOR_MAPPING_RULES_FILE )
    CTMP = REPEAT(' ', 8196)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_REPLACE_ENVVAR) REPLACE_ENVVAR_IN_STRING( TRIM(OPTIONS_DICTIONARY%MAPPING_RULES_FNAME), CTMP, HOOKS )
    PP_TRYCALL(ERRFLAG_CONVERT_ENUM_STRING) CONVERT_TO_C_STRING(CTMP, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( OPTIONS_ITERATOR_SAMPLE_PATH )
    CTMP = REPEAT(' ', 8196)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_REPLACE_ENVVAR) REPLACE_ENVVAR_IN_STRING( TRIM(OPTIONS_DICTIONARY%SAMPLE_RULES_FNAME), CTMP, HOOKS )
    PP_TRYCALL(ERRFLAG_CONVERT_ENUM_STRING) CONVERT_TO_C_STRING(CTMP, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( OPTIONS_ITERATOR_ENCODER_TYPE )
    CTMP = REPEAT(' ', 8196)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_REPLACE_ENVVAR) REPLACE_ENVVAR_IN_STRING( TRIM(OPTIONS_DICTIONARY%ENCODER_TYPE), CTMP, HOOKS )
    PP_TRYCALL(ERRFLAG_CONVERT_ENUM_STRING) CONVERT_TO_C_STRING(CTMP, VALUE, HOOKS )
    HAS = .TRUE.

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_OPTIONS_KEY )

  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
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
    CASE(ERRFLAG_UNABLE_TO_REPLACE_ENVVAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to replace environment variables' )
    CASE(ERRFLAG_NO_OPTIONS_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid enumerator found' )
    CASE(ERRFLAG_CONVERT_ENUM_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert enumerator to string' )
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

END FUNCTION OPTIONS_DICTIONARY_GET_VALUE_AS_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'OPTIONS_DICTIONARY_SET_VALUE_FROM_STRING'
PP_THREAD_SAFE FUNCTION OPTIONS_DICTIONARY_SET_VALUE_FROM_STRING( OPTIONS_DICTIONARY, ITERATOR, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIM_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,     ONLY: UNDEF_PARAM_E
  USE :: ENUMERATORS_MOD,     ONLY: CSTREAM2ISTREAM
  USE :: ENUMERATORS_MOD,     ONLY: CTYPE2ITYPE
  USE :: ENUMERATORS_MOD,     ONLY: CCLASS2ICLASS
  USE :: ENUMERATORS_MOD,     ONLY: CLEVTYPE2ILEVTYPE
  USE :: ENUMERATORS_MOD,     ONLY: CORIGIN2IORIGIN
  USE :: ENUMERATORS_MOD,     ONLY: CINT2IINT
  USE :: ENUMERATORS_MOD,     ONLY: CFLOAT2IFLOAT
  USE :: ENUMERATORS_MOD,     ONLY: CREPRES2IREPRES
  USE :: ENUMERATORS_MOD,     ONLY: CPACKING2IPACKING
  USE :: ENUMERATORS_MOD,     ONLY: CMODEL2IMODEL
  USE :: ENUMERATORS_MOD,     ONLY: CPARAMTYPE2IPARAMTYPE
  USE :: ENUMERATORS_MOD,     ONLY: CTIME2ITIME
  USE :: GRIB_API,            ONLY: GRIB_SET_SAMPLES_PATH
  USE :: GRIB_API,            ONLY: GRIB_SUCCESS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(API_OPTIONS_T),  INTENT(INOUT) :: OPTIONS_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITERATOR
  CHARACTER(LEN=*),     INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ITEMP
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_OPTIONS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_OPTIONS_TO_ENUM=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_SAMPLES_PATH_FAILED=4_JPIB_K

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

  SELECT CASE ( ITERATOR )

  CASE ( OPTIONS_ITERATOR_ENCODING_RULES_FILE )
        OPTIONS_DICTIONARY%ENCODING_RULES_FNAME = REPEAT(' ',8192)
        OPTIONS_DICTIONARY%ENCODING_RULES_FNAME = TRIM(ADJUSTL(VALUE))

  CASE ( OPTIONS_ITERATOR_MAPPING_RULES_FILE )
        OPTIONS_DICTIONARY%MAPPING_RULES_FNAME = REPEAT(' ',8192)
        OPTIONS_DICTIONARY%MAPPING_RULES_FNAME = TRIM(ADJUSTL(VALUE))

  CASE ( OPTIONS_ITERATOR_SAMPLE_PATH )
        CALL GRIB_SET_SAMPLES_PATH( TRIM(ADJUSTL(VALUE)), STATUS=KRET )
        PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_SET_SAMPLES_PATH_FAILED )
        OPTIONS_DICTIONARY%SAMPLE_RULES_FNAME = REPEAT(' ',8192)
        OPTIONS_DICTIONARY%SAMPLE_RULES_FNAME = TRIM(ADJUSTL(VALUE))

  CASE ( OPTIONS_ITERATOR_ENCODER_TYPE )
        OPTIONS_DICTIONARY%ENCODER_TYPE = REPEAT(' ',8192)
        OPTIONS_DICTIONARY%ENCODER_TYPE = TRIM(ADJUSTL(VALUE))

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_OPTIONS_KEY )

  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
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
    CASE( ERRFLAG_NO_OPTIONS_KEY )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid enumerator found' )
    CASE( ERRFLAG_CONVERT_OPTIONS_TO_ENUM )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert OPTIONS to enumerator' )
    CASE( ERRFLAG_GRIB_SET_SAMPLES_PATH_FAILED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to to set ECCODES_SAMPLES_PATH' )
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

END FUNCTION OPTIONS_DICTIONARY_SET_VALUE_FROM_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE API_OPTIONS_DICTIONARY_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
