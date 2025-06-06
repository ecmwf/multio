! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'api_HEALPix_dictionary_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'API_HEALPIX_DICTIONARY_UTILS_MOD'
MODULE API_HEALPIX_DICTIONARY_UTILS_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!> Enumerators for the iterator
INTEGER(KIND=JPIB_K), PARAMETER :: HEALPIX_ITERATOR_NSIDE = 1_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: HEALPIX_ITERATOR_ORDERING_CONVENTION = 2_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: HEALPIX_ITERATOR_LONGITUDE_OF_FIRST_GRID_POINT_IN_DEGREES = 3_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: HEALPIX_ITERATOR_UNDEF = 4_JPIB_K


! Whitelist of public symbols
PUBLIC :: HEALPIX_DICTIONARY_MAX_ITERATOR
PUBLIC :: HEALPIX_DICTIONARY_INIT_ITERATOR
PUBLIC :: HEALPIX_DICTIONARY_NAME2ITERATOR
PUBLIC :: HEALPIX_DICTIONARY_GET_NEXT_ITERATOR
PUBLIC :: HEALPIX_DICTIONARY_HAS
PUBLIC :: HEALPIX_DICTIONARY_GET_KEY_AS_STRING
PUBLIC :: HEALPIX_DICTIONARY_GET_VALUE_AS_STRING

PUBLIC :: HEALPIX_DICTIONARY_SET_VALUE_FROM_STRING
PUBLIC :: HEALPIX_DICTIONARY_SET_VALUE_FROM_INT64
PUBLIC :: HEALPIX_DICTIONARY_SET_VALUE_FROM_REAL64
PUBLIC :: HEALPIX_DICTIONARY_SET_VALUE_FROM_INT64_ARRAY
PUBLIC :: HEALPIX_DICTIONARY_SET_VALUE_FROM_REAL64_ARRAY

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HEALPIX_DICTIONARY_MAX_ITERATOR'
PP_THREAD_SAFE FUNCTION HEALPIX_DICTIONARY_MAX_ITERATOR( HEALPIX_DICTIONARY, MAX_ITERATOR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD,   ONLY: TOLOWER
  USE :: REPRESENTATIONS_MOD, ONLY: HEALPIX_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(HEALPIX_T),      INTENT(IN)    :: HEALPIX_DICTIONARY
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
  MAX_ITERATOR = HEALPIX_ITERATOR_UNDEF

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

END FUNCTION HEALPIX_DICTIONARY_MAX_ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HEALPIX_DICTIONARY_INIT_ITERATOR'
PP_THREAD_SAFE FUNCTION HEALPIX_DICTIONARY_INIT_ITERATOR( HEALPIX_DICTIONARY, ITERATOR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: HEALPIX_T
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
  TYPE(HEALPIX_T),   INTENT(IN)    :: HEALPIX_DICTIONARY
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

END FUNCTION HEALPIX_DICTIONARY_INIT_ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HEALPIX_DICTIONARY_GET_NEXT_ITERATOR'
PP_THREAD_SAFE FUNCTION HEALPIX_DICTIONARY_GET_NEXT_ITERATOR( HEALPIX_DICTIONARY, ITERATOR, END_OF_ITERATORS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: HEALPIX_T
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
  TYPE(HEALPIX_T),   INTENT(IN)    :: HEALPIX_DICTIONARY
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
    IF ( ITERATOR .EQ. HEALPIX_ITERATOR_UNDEF ) THEN
      END_OF_ITERATORS = .TRUE.
      HAS_KEY = .TRUE.
    ELSE
      PP_TRYCALL(ERRFLAG_HAS_ITERATOR) HEALPIX_DICTIONARY_HAS( HEALPIX_DICTIONARY, ITERATOR, HAS_KEY, HOOKS )
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

END FUNCTION HEALPIX_DICTIONARY_GET_NEXT_ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HEALPIX_DICTIONARY_NAME2ITERATOR'
PP_THREAD_SAFE FUNCTION HEALPIX_DICTIONARY_NAME2ITERATOR( HEALPIX_DICTIONARY, KEY, ITERATOR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: HEALPIX_T
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
  TYPE(HEALPIX_T),   INTENT(IN)    :: HEALPIX_DICTIONARY
  CHARACTER(LEN=*),     INTENT(IN)    :: KEY
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: ITERATOR
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=LEN(KEY)) :: KEY_LOW

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_HEALPIX_KEY=2_JPIB_K

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

  CASE ( 'nside' )
    ITERATOR = HEALPIX_ITERATOR_NSIDE

  CASE ( 'ordering-convention', 'orderingConvention' )
    ITERATOR = HEALPIX_ITERATOR_ORDERING_CONVENTION

  CASE ( 'longitudeOfFirstGridPointInDegrees', 'longitude-of-first-grid-point-in-degrees' )
    ITERATOR = HEALPIX_ITERATOR_LONGITUDE_OF_FIRST_GRID_POINT_IN_DEGREES

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_HEALPIX_KEY )

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
    CASE(ERRFLAG_NO_HEALPIX_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'No HEALPIX key found' )
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

END FUNCTION HEALPIX_DICTIONARY_NAME2ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HEALPIX_DICTIONARY_HAS'
PP_THREAD_SAFE FUNCTION HEALPIX_DICTIONARY_HAS( HEALPIX_DICTIONARY, ITERATOR, HAS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: HEALPIX_T
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
  TYPE(HEALPIX_T),   INTENT(IN)    :: HEALPIX_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: ITERATOR
  LOGICAL,              INTENT(OUT)   :: HAS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_HEALPIX_KEY=2_JPIB_K

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

  CASE ( HEALPIX_ITERATOR_NSIDE )
    HAS = .TRUE.

  CASE ( HEALPIX_ITERATOR_ORDERING_CONVENTION )
    HAS = .TRUE.

  CASE ( HEALPIX_ITERATOR_LONGITUDE_OF_FIRST_GRID_POINT_IN_DEGREES )
    HAS = .TRUE.


  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_HEALPIX_KEY )

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
    CASE(ERRFLAG_NO_HEALPIX_KEY)
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

END FUNCTION HEALPIX_DICTIONARY_HAS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HEALPIX_DICTIONARY_GET_KEY_AS_STRING'
PP_THREAD_SAFE FUNCTION HEALPIX_DICTIONARY_GET_KEY_AS_STRING( HEALPIX_DICTIONARY, ITERATOR, KEY, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: HEALPIX_T
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
  TYPE(HEALPIX_T),   INTENT(IN)    :: HEALPIX_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITERATOR
  CHARACTER(LEN=64),    INTENT(OUT)   :: KEY
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_HEALPIX_KEY=2_JPIB_K
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

  CASE ( HEALPIX_ITERATOR_NSIDE )
    KEY = 'nside'

  CASE ( HEALPIX_ITERATOR_ORDERING_CONVENTION )
    KEY = 'ordering-convention'

  CASE ( HEALPIX_ITERATOR_LONGITUDE_OF_FIRST_GRID_POINT_IN_DEGREES )
    KEY = 'longitude-of-first-grid-point-in-degrees'

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_HEALPIX_KEY )

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
    CASE(ERRFLAG_NO_HEALPIX_KEY)
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

END FUNCTION HEALPIX_DICTIONARY_GET_KEY_AS_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HEALPIX_DICTIONARY_GET_VALUE_AS_STRING'
PP_THREAD_SAFE FUNCTION HEALPIX_DICTIONARY_GET_VALUE_AS_STRING( HEALPIX_DICTIONARY, ITERATOR, VALUE, HAS, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD,   ONLY: HEALPIX_T
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: API_GENERAL_UTILS_MOD, ONLY: CONVERT_TO_C_STRING
  USE :: ENUMERATORS_MOD,       ONLY: IORDERING_CONVENTION2CORDERING_CONVENTION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(HEALPIX_T),      INTENT(IN)    :: HEALPIX_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITERATOR
  TYPE(C_PTR),          INTENT(OUT)   :: VALUE
  LOGICAL,              INTENT(OUT)   :: HAS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=16) :: VALUE_STR

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_HEALPIX_KEY=2_JPIB_K
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

  CASE ( HEALPIX_ITERATOR_NSIDE )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( HEALPIX_DICTIONARY%NSIDE, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( HEALPIX_ITERATOR_ORDERING_CONVENTION )
    VALUE_STR = REPEAT( ' ', LEN(VALUE_STR) )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) IORDERING_CONVENTION2CORDERING_CONVENTION( HEALPIX_DICTIONARY%ORDERING_CONVENTION, VALUE_STR, HOOKS )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( TRIM(ADJUSTL(VALUE_STR)), VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( HEALPIX_ITERATOR_LONGITUDE_OF_FIRST_GRID_POINT_IN_DEGREES )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( HEALPIX_DICTIONARY%LONGITUDE_OF_FIRST_GRID_POINT_IN_DEGREES, VALUE, HOOKS )
    HAS = .TRUE.

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_HEALPIX_KEY )

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
    CASE(ERRFLAG_NO_HEALPIX_KEY)
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

END FUNCTION HEALPIX_DICTIONARY_GET_VALUE_AS_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HEALPIX_DICTIONARY_SET_VALUE_FROM_STRING'
PP_THREAD_SAFE FUNCTION HEALPIX_DICTIONARY_SET_VALUE_FROM_STRING( HEALPIX_DICTIONARY, ITERATOR, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD,     ONLY: HEALPIX_T
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPRD_K
  USE :: HOOKS_MOD,               ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,         ONLY: UNDEF_PARAM_E
  USE :: ENUMERATORS_MOD,         ONLY: CINT2IINT
  USE :: ENUMERATORS_MOD,         ONLY: CFLOAT2IFLOAT
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_TO_REAL_ARRAY
  USE :: ENUMERATORS_MOD,         ONLY: CORDERING_CONVENTION2IORDERING_CONVENTION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(HEALPIX_T),      INTENT(INOUT) :: HEALPIX_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITERATOR
  CHARACTER(LEN=*),     INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

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
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_HEALPIX_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_HEALPIX_TO_ENUM=3_JPIB_K

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

  CASE ( HEALPIX_ITERATOR_NSIDE )
    PP_TRYCALL(ERRFLAG_CONVERT_HEALPIX_TO_ENUM) CINT2IINT(VALUE, ITEMP, HOOKS)
    HEALPIX_DICTIONARY%NSIDE=ITEMP

  CASE ( HEALPIX_ITERATOR_ORDERING_CONVENTION )
    PP_TRYCALL(ERRFLAG_CONVERT_HEALPIX_TO_ENUM) CORDERING_CONVENTION2IORDERING_CONVENTION(VALUE, ITEMP, HOOKS)
    HEALPIX_DICTIONARY%ORDERING_CONVENTION=ITEMP

  CASE ( HEALPIX_ITERATOR_LONGITUDE_OF_FIRST_GRID_POINT_IN_DEGREES )
    PP_TRYCALL(ERRFLAG_CONVERT_HEALPIX_TO_ENUM) CINT2IINT(VALUE, ITEMP, HOOKS)
    HEALPIX_DICTIONARY%LONGITUDE_OF_FIRST_GRID_POINT_IN_DEGREES=ITEMP

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_HEALPIX_KEY )

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
    CASE( ERRFLAG_NO_HEALPIX_KEY )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid enumerator found' )
    CASE( ERRFLAG_CONVERT_HEALPIX_TO_ENUM )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert HEALPIX to enumerator' )
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

END FUNCTION HEALPIX_DICTIONARY_SET_VALUE_FROM_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HEALPIX_DICTIONARY_SET_VALUE_FROM_INT64'
PP_THREAD_SAFE FUNCTION HEALPIX_DICTIONARY_SET_VALUE_FROM_INT64( HEALPIX_DICTIONARY, ITERATOR, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD,     ONLY: HEALPIX_T
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPRD_K
  USE :: HOOKS_MOD,               ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(HEALPIX_T),   INTENT(INOUT) :: HEALPIX_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITERATOR
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_HEALPIX_KEY=2_JPIB_K

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

  CASE ( HEALPIX_ITERATOR_NSIDE )
    HEALPIX_DICTIONARY%NSIDE = VALUE

  CASE ( HEALPIX_ITERATOR_LONGITUDE_OF_FIRST_GRID_POINT_IN_DEGREES )
    HEALPIX_DICTIONARY%LONGITUDE_OF_FIRST_GRID_POINT_IN_DEGREES = VALUE

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_HEALPIX_KEY )

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
    CASE( ERRFLAG_NO_HEALPIX_KEY )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid enumerator found' )
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

END FUNCTION HEALPIX_DICTIONARY_SET_VALUE_FROM_INT64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HEALPIX_DICTIONARY_SET_VALUE_FROM_REAL64'
PP_THREAD_SAFE FUNCTION HEALPIX_DICTIONARY_SET_VALUE_FROM_REAL64( HEALPIX_DICTIONARY, ITERATOR, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD,     ONLY: HEALPIX_T
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPRD_K
  USE :: HOOKS_MOD,               ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(HEALPIX_T),   INTENT(INOUT) :: HEALPIX_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITERATOR
  REAL(KIND=JPRD_K),    INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_HEALPIX_KEY=2_JPIB_K

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

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_HEALPIX_KEY )

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
    CASE( ERRFLAG_NO_HEALPIX_KEY )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid enumerator found' )
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

END FUNCTION HEALPIX_DICTIONARY_SET_VALUE_FROM_REAL64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HEALPIX_DICTIONARY_SET_VALUE_FROM_INT64_ARRAY'
PP_THREAD_SAFE FUNCTION HEALPIX_DICTIONARY_SET_VALUE_FROM_INT64_ARRAY( HEALPIX_DICTIONARY, ITERATOR, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD,     ONLY: HEALPIX_T
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPRD_K
  USE :: HOOKS_MOD,               ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(HEALPIX_T),                 INTENT(INOUT) :: HEALPIX_DICTIONARY
  INTEGER(KIND=JPIB_K),               INTENT(IN)    :: ITERATOR
  INTEGER(KIND=JPIB_K), DIMENSION(:), INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_HEALPIX_KEY=2_JPIB_K

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

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_HEALPIX_KEY )

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
    CASE( ERRFLAG_NO_HEALPIX_KEY )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid enumerator found' )
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

END FUNCTION HEALPIX_DICTIONARY_SET_VALUE_FROM_INT64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HEALPIX_DICTIONARY_SET_VALUE_FROM_REAL64_ARRAY'
PP_THREAD_SAFE FUNCTION HEALPIX_DICTIONARY_SET_VALUE_FROM_REAL64_ARRAY( HEALPIX_DICTIONARY, ITERATOR, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD,     ONLY: HEALPIX_T
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPRD_K
  USE :: HOOKS_MOD,               ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(HEALPIX_T),              INTENT(INOUT) :: HEALPIX_DICTIONARY
  INTEGER(KIND=JPIB_K),            INTENT(IN)    :: ITERATOR
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_HEALPIX_KEY=2_JPIB_K

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

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_HEALPIX_KEY )

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
    CASE( ERRFLAG_NO_HEALPIX_KEY )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid enumerator found' )
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

END FUNCTION HEALPIX_DICTIONARY_SET_VALUE_FROM_REAL64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE API_HEALPIX_DICTIONARY_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
