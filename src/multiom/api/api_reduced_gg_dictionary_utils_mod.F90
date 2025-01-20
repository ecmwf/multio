! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'api_reduced_gg_dictionary_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'API_REDUCED_GG_DICTIONARY_UTILS_MOD'
MODULE API_REDUCED_GG_DICTIONARY_UTILS_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!> Enumerators for the iterator
INTEGER(KIND=JPIB_K), PARAMETER :: REDUCED_GG_ITERATOR_TRUNCATE_DEGREES = 1_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REDUCED_GG_ITERATOR_NPTS_ALONG_MERIDIAN = 2_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REDUCED_GG_ITERATOR_NPARALLELS = 3_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REDUCED_GG_ITERATOR_LAT_FIRST_GP_DEG = 4_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REDUCED_GG_ITERATOR_LON_FIRST_GP_DEG = 5_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REDUCED_GG_ITERATOR_LAT_LAST_GP_DEG = 6_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REDUCED_GG_ITERATOR_LON_LAST_GP_DEG = 7_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REDUCED_GG_ITERATOR_PL = 8_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REDUCED_GG_ITERATOR_UNDEF = 9_JPIB_K


! Whitelist of public symbols
PUBLIC :: REDUCED_GG_DICTIONARY_MAX_ITERATOR
PUBLIC :: REDUCED_GG_DICTIONARY_INIT_ITERATOR
PUBLIC :: REDUCED_GG_DICTIONARY_NAME2ITERATOR
PUBLIC :: REDUCED_GG_DICTIONARY_GET_NEXT_ITERATOR
PUBLIC :: REDUCED_GG_DICTIONARY_HAS
PUBLIC :: REDUCED_GG_DICTIONARY_GET_KEY_AS_STRING
PUBLIC :: REDUCED_GG_DICTIONARY_GET_VALUE_AS_STRING
PUBLIC :: REDUCED_GG_DICTIONARY_SET_VALUE_FROM_STRING

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REDUCED_GG_DICTIONARY_MAX_ITERATOR'
PP_THREAD_SAFE FUNCTION REDUCED_GG_DICTIONARY_MAX_ITERATOR( REDUCED_GG_DICTIONARY, MAX_ITERATOR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD,   ONLY: TOLOWER
  USE :: REPRESENTATIONS_MOD, ONLY: REDUCED_GG_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(REDUCED_GG_T),   INTENT(IN)    :: REDUCED_GG_DICTIONARY
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
  MAX_ITERATOR = REDUCED_GG_ITERATOR_UNDEF

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

END FUNCTION REDUCED_GG_DICTIONARY_MAX_ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REDUCED_GG_DICTIONARY_INIT_ITERATOR'
PP_THREAD_SAFE FUNCTION REDUCED_GG_DICTIONARY_INIT_ITERATOR( REDUCED_GG_DICTIONARY, ITERATOR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: REDUCED_GG_T
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
  TYPE(REDUCED_GG_T),   INTENT(IN)    :: REDUCED_GG_DICTIONARY
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

END FUNCTION REDUCED_GG_DICTIONARY_INIT_ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REDUCED_GG_DICTIONARY_GET_NEXT_ITERATOR'
PP_THREAD_SAFE FUNCTION REDUCED_GG_DICTIONARY_GET_NEXT_ITERATOR( REDUCED_GG_DICTIONARY, ITERATOR, END_OF_ITERATORS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: REDUCED_GG_T
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
  TYPE(REDUCED_GG_T),   INTENT(IN)    :: REDUCED_GG_DICTIONARY
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
    IF ( ITERATOR .EQ. REDUCED_GG_ITERATOR_UNDEF ) THEN
      END_OF_ITERATORS = .TRUE.
      HAS_KEY = .TRUE.
    ELSE
      PP_TRYCALL(ERRFLAG_HAS_ITERATOR) REDUCED_GG_DICTIONARY_HAS( REDUCED_GG_DICTIONARY, ITERATOR, HAS_KEY, HOOKS )
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

END FUNCTION REDUCED_GG_DICTIONARY_GET_NEXT_ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REDUCED_GG_DICTIONARY_NAME2ITERATOR'
PP_THREAD_SAFE FUNCTION REDUCED_GG_DICTIONARY_NAME2ITERATOR( REDUCED_GG_DICTIONARY, KEY, ITERATOR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: REDUCED_GG_T
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
  TYPE(REDUCED_GG_T),   INTENT(IN)    :: REDUCED_GG_DICTIONARY
  CHARACTER(LEN=*),     INTENT(IN)    :: KEY
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: ITERATOR
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=LEN(KEY)) :: KEY_LOW

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_REDUCED_GG_KEY=2_JPIB_K

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

  CASE ( 'truncate-degrees' )
    ITERATOR = REDUCED_GG_ITERATOR_TRUNCATE_DEGREES

  CASE ( 'number-of-points-along-a-meridian' )
    ITERATOR = REDUCED_GG_ITERATOR_NPTS_ALONG_MERIDIAN

  CASE ( 'number-of-parallels-between-pole-and-equator' )
    ITERATOR = REDUCED_GG_ITERATOR_NPARALLELS

  CASE ( 'latitude-of-first-grid-point-in-degrees' )
    ITERATOR = REDUCED_GG_ITERATOR_LAT_FIRST_GP_DEG

  CASE ( 'longitude-of-first-grid-point-in-degrees' )
    ITERATOR = REDUCED_GG_ITERATOR_LON_FIRST_GP_DEG

  CASE ( 'latitude-of-last-grid-point-in-degrees' )
    ITERATOR = REDUCED_GG_ITERATOR_LAT_LAST_GP_DEG

  CASE ( 'longitude-of-last-grid-point-in-degrees' )
    ITERATOR = REDUCED_GG_ITERATOR_LON_LAST_GP_DEG

  CASE ( 'pl' )
    ITERATOR = REDUCED_GG_ITERATOR_PL

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_REDUCED_GG_KEY )

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
    CASE(ERRFLAG_NO_REDUCED_GG_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'No REDUCED_GG key found' )
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

END FUNCTION REDUCED_GG_DICTIONARY_NAME2ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REDUCED_GG_DICTIONARY_HAS'
PP_THREAD_SAFE FUNCTION REDUCED_GG_DICTIONARY_HAS( REDUCED_GG_DICTIONARY, ITERATOR, HAS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: REDUCED_GG_T
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
  TYPE(REDUCED_GG_T),   INTENT(IN)    :: REDUCED_GG_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: ITERATOR
  LOGICAL,              INTENT(OUT)   :: HAS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_REDUCED_GG_KEY=2_JPIB_K

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

  CASE ( REDUCED_GG_ITERATOR_TRUNCATE_DEGREES )
    HAS = .TRUE.

  CASE ( REDUCED_GG_ITERATOR_NPTS_ALONG_MERIDIAN )
    HAS = .TRUE.

  CASE ( REDUCED_GG_ITERATOR_NPARALLELS )
    HAS = .TRUE.

  CASE ( REDUCED_GG_ITERATOR_LAT_FIRST_GP_DEG )
    HAS = .TRUE.

  CASE ( REDUCED_GG_ITERATOR_LON_FIRST_GP_DEG )
    HAS = .TRUE.

  CASE ( REDUCED_GG_ITERATOR_LAT_LAST_GP_DEG )
    HAS = .TRUE.

  CASE ( REDUCED_GG_ITERATOR_LON_LAST_GP_DEG )
    HAS = .TRUE.

  CASE ( REDUCED_GG_ITERATOR_PL )
    IF ( ASSOCIATED(REDUCED_GG_DICTIONARY%PL) ) THEN
      HAS = .TRUE.
    ELSE
      HAS = .FALSE.
    ENDIF

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_REDUCED_GG_KEY )

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
    CASE(ERRFLAG_NO_REDUCED_GG_KEY)
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

END FUNCTION REDUCED_GG_DICTIONARY_HAS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REDUCED_GG_DICTIONARY_GET_KEY_AS_STRING'
PP_THREAD_SAFE FUNCTION REDUCED_GG_DICTIONARY_GET_KEY_AS_STRING( REDUCED_GG_DICTIONARY, ITERATOR, KEY, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: REDUCED_GG_T
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
  TYPE(REDUCED_GG_T),   INTENT(IN)    :: REDUCED_GG_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITERATOR
  CHARACTER(LEN=64),    INTENT(OUT)   :: KEY
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_REDUCED_GG_KEY=2_JPIB_K
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

  CASE ( REDUCED_GG_ITERATOR_TRUNCATE_DEGREES )
    KEY = 'truncate-degrees'

  CASE ( REDUCED_GG_ITERATOR_NPTS_ALONG_MERIDIAN )
    KEY = 'number-of-points-along-a-meridian'

  CASE ( REDUCED_GG_ITERATOR_NPARALLELS )
    KEY = 'number-of-parallels-between-pole-and-equator'

  CASE ( REDUCED_GG_ITERATOR_LAT_FIRST_GP_DEG )
    KEY = 'latitude-of-first-grid-point-in-degrees'

  CASE ( REDUCED_GG_ITERATOR_LON_FIRST_GP_DEG )
    KEY = 'longitude-of-first-grid-point-in-degrees'

  CASE ( REDUCED_GG_ITERATOR_LAT_LAST_GP_DEG )
    KEY = 'latitude-of-last-grid-point-in-degrees'

  CASE ( REDUCED_GG_ITERATOR_LON_LAST_GP_DEG )
    KEY = 'longitude-of-last-grid-point-in-degrees'

  CASE ( REDUCED_GG_ITERATOR_PL )
    KEY = 'pl'

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_REDUCED_GG_KEY )

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
    CASE(ERRFLAG_NO_REDUCED_GG_KEY)
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

END FUNCTION REDUCED_GG_DICTIONARY_GET_KEY_AS_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REDUCED_GG_DICTIONARY_GET_VALUE_AS_STRING'
PP_THREAD_SAFE FUNCTION REDUCED_GG_DICTIONARY_GET_VALUE_AS_STRING( REDUCED_GG_DICTIONARY, ITERATOR, VALUE, HAS, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD,   ONLY: REDUCED_GG_T
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: API_GENERAL_UTILS_MOD, ONLY: CONVERT_TO_C_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(REDUCED_GG_T),   INTENT(IN)    :: REDUCED_GG_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITERATOR
  TYPE(C_PTR),          INTENT(OUT)   :: VALUE
  LOGICAL,              INTENT(OUT)   :: HAS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_REDUCED_GG_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_STRING=3_JPIB_K

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

  CASE ( REDUCED_GG_ITERATOR_TRUNCATE_DEGREES )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REDUCED_GG_DICTIONARY%TRUNCATE_DEGREES, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REDUCED_GG_ITERATOR_NPTS_ALONG_MERIDIAN )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REDUCED_GG_DICTIONARY%NUMBER_OF_POINTS_ALONG_A_MERIDIAN, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REDUCED_GG_ITERATOR_NPARALLELS )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REDUCED_GG_DICTIONARY%NUMBER_OF_PARALLELS_BETWEEN_POLE_AND_EQUATOR, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REDUCED_GG_ITERATOR_LAT_FIRST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REDUCED_GG_DICTIONARY%LAT_FIRST_GP_DEG, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REDUCED_GG_ITERATOR_LON_FIRST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REDUCED_GG_DICTIONARY%LON_FIRST_GP_DEG, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REDUCED_GG_ITERATOR_LAT_LAST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REDUCED_GG_DICTIONARY%LAT_LAST_GP_DEG, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REDUCED_GG_ITERATOR_LON_LAST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REDUCED_GG_DICTIONARY%LON_LAST_GP_DEG, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REDUCED_GG_ITERATOR_PL )
    IF ( ASSOCIATED(REDUCED_GG_DICTIONARY%PL) ) THEN
      PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REDUCED_GG_DICTIONARY%PL, VALUE, HOOKS )
      HAS = .TRUE.
    ELSE
      HAS = .FALSE.
    ENDIF

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_REDUCED_GG_KEY )

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
    CASE(ERRFLAG_NO_REDUCED_GG_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid enumerator found' )
    CASE(ERRFLAG_CONVERT_TO_STRING)
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

END FUNCTION REDUCED_GG_DICTIONARY_GET_VALUE_AS_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REDUCED_GG_DICTIONARY_SET_VALUE_FROM_STRING'
PP_THREAD_SAFE FUNCTION REDUCED_GG_DICTIONARY_SET_VALUE_FROM_STRING( REDUCED_GG_DICTIONARY, ITERATOR, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD,     ONLY: REDUCED_GG_T
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPRD_K
  USE :: HOOKS_MOD,               ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,         ONLY: UNDEF_PARAM_E
  USE :: ENUMERATORS_MOD,         ONLY: CINT2IINT
  USE :: ENUMERATORS_MOD,         ONLY: CFLOAT2IFLOAT
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_TO_REAL_ARRAY

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(REDUCED_GG_T), INTENT(INOUT) :: REDUCED_GG_DICTIONARY
  INTEGER(KIND=JPIB_K),    INTENT(IN)    :: ITERATOR
  CHARACTER(LEN=*),        INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ITEMP
  REAL(KIND=JPRD_K) :: FTEMP
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  REAL(KIND=JPRD_K), DIMENSION(:), ALLOCATABLE :: F_ARR

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_REDUCED_GG_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_REDUCED_GG_TO_ENUM=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOC_FAILURE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOC_FAILURE=6_JPIB_K

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

  CASE ( REDUCED_GG_ITERATOR_TRUNCATE_DEGREES )
    PP_TRYCALL(ERRFLAG_CONVERT_REDUCED_GG_TO_ENUM) CINT2IINT(VALUE, ITEMP, HOOKS)
    REDUCED_GG_DICTIONARY%TRUNCATE_DEGREES=ITEMP

  CASE ( REDUCED_GG_ITERATOR_NPTS_ALONG_MERIDIAN )
    PP_TRYCALL(ERRFLAG_CONVERT_REDUCED_GG_TO_ENUM) CINT2IINT(VALUE, ITEMP, HOOKS)
    REDUCED_GG_DICTIONARY%NUMBER_OF_POINTS_ALONG_A_MERIDIAN=ITEMP

  CASE ( REDUCED_GG_ITERATOR_NPARALLELS )
    PP_TRYCALL(ERRFLAG_CONVERT_REDUCED_GG_TO_ENUM) CINT2IINT(VALUE, ITEMP, HOOKS)
    REDUCED_GG_DICTIONARY%NUMBER_OF_PARALLELS_BETWEEN_POLE_AND_EQUATOR=ITEMP

  CASE ( REDUCED_GG_ITERATOR_LAT_FIRST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_REDUCED_GG_TO_ENUM) CFLOAT2IFLOAT(VALUE, FTEMP, HOOKS)
    REDUCED_GG_DICTIONARY%LAT_FIRST_GP_DEG=FTEMP

  CASE ( REDUCED_GG_ITERATOR_LON_FIRST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_REDUCED_GG_TO_ENUM) CFLOAT2IFLOAT(VALUE, FTEMP, HOOKS)
    REDUCED_GG_DICTIONARY%LON_FIRST_GP_DEG=FTEMP

  CASE ( REDUCED_GG_ITERATOR_LAT_LAST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_REDUCED_GG_TO_ENUM) CFLOAT2IFLOAT(VALUE, FTEMP, HOOKS)
    REDUCED_GG_DICTIONARY%LAT_LAST_GP_DEG=FTEMP

  CASE ( REDUCED_GG_ITERATOR_LON_LAST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_REDUCED_GG_TO_ENUM) CFLOAT2IFLOAT(VALUE, FTEMP, HOOKS)
    REDUCED_GG_DICTIONARY%LON_LAST_GP_DEG=FTEMP

  CASE ( REDUCED_GG_ITERATOR_PL )

    PP_TRYCALL(ERRFLAG_CONVERT_REDUCED_GG_TO_ENUM) STRING_TO_REAL_ARRAY(VALUE, F_ARR, HOOKS)

    IF ( ASSOCIATED(REDUCED_GG_DICTIONARY%PL) ) THEN
        DEALLOCATE(REDUCED_GG_DICTIONARY%PL, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
        PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOC_FAILURE )
    ENDIF

    ALLOCATE( REDUCED_GG_DICTIONARY%PL(SIZE(F_ARR) ), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_ALLOC_FAILURE )
    DO ITEMP = 1, SIZE(F_ARR)
        REDUCED_GG_DICTIONARY%PL(ITEMP) = F_ARR(ITEMP)
    END DO
    REDUCED_GG_DICTIONARY%TO_BE_DEALLOCATED=.TRUE.

    DEALLOCATE( F_ARR, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOC_FAILURE )

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_REDUCED_GG_KEY )

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
    CASE( ERRFLAG_NO_REDUCED_GG_KEY )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid enumerator found' )
    CASE( ERRFLAG_CONVERT_REDUCED_GG_TO_ENUM )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert REDUCED_GG to enumerator' )
    CASE( ERRFLAG_ALLOC_FAILURE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Allocation failure' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STATUS)
      ENDIF
    CASE( ERRFLAG_DEALLOC_FAILURE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Deallocation failure' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STATUS)
      ENDIF
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

END FUNCTION REDUCED_GG_DICTIONARY_SET_VALUE_FROM_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE API_REDUCED_GG_DICTIONARY_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
