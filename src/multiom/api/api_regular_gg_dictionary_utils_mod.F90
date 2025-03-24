! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'api_regular_gg_dictionary_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'API_REGULAR_GG_DICTIONARY_UTILS_MOD'
MODULE API_REGULAR_GG_DICTIONARY_UTILS_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!> Enumerators for the iterator
INTEGER(KIND=JPIB_K), PARAMETER :: REGULAR_GG_ITERATOR_TRUNCATE_DEGREES = 1_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REGULAR_GG_ITERATOR_NPTS_ALONG_MERIDIAN = 2_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REGULAR_GG_ITERATOR_NPTS_ALONG_PARALLEL = 3_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REGULAR_GG_ITERATOR_NPARALLELS = 4_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REGULAR_GG_ITERATOR_LAT_FIRST_GP_DEG = 5_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REGULAR_GG_ITERATOR_LON_FIRST_GP_DEG = 6_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REGULAR_GG_ITERATOR_LAT_LAST_GP_DEG = 7_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REGULAR_GG_ITERATOR_LON_LAST_GP_DEG = 8_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REGULAR_GG_ITERATOR_IDIR_INC = 9_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: REGULAR_GG_ITERATOR_UNDEF = 10_JPIB_K


! Whitelist of public symbols
PUBLIC :: REGULAR_GG_DICTIONARY_MAX_ITERATOR
PUBLIC :: REGULAR_GG_DICTIONARY_INIT_ITERATOR
PUBLIC :: REGULAR_GG_DICTIONARY_NAME2ITERATOR
PUBLIC :: REGULAR_GG_DICTIONARY_GET_NEXT_ITERATOR
PUBLIC :: REGULAR_GG_DICTIONARY_HAS
PUBLIC :: REGULAR_GG_DICTIONARY_GET_KEY_AS_STRING
PUBLIC :: REGULAR_GG_DICTIONARY_GET_VALUE_AS_STRING

PUBLIC :: REGULAR_GG_DICTIONARY_SET_VALUE_FROM_STRING
PUBLIC :: REGULAR_GG_DICTIONARY_SET_VALUE_FROM_INT64
PUBLIC :: REGULAR_GG_DICTIONARY_SET_VALUE_FROM_REAL64
PUBLIC :: REGULAR_GG_DICTIONARY_SET_VALUE_FROM_INT64_ARRAY
PUBLIC :: REGULAR_GG_DICTIONARY_SET_VALUE_FROM_REAL64_ARRAY

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REGULAR_GG_DICTIONARY_MAX_ITERATOR'
PP_THREAD_SAFE FUNCTION REGULAR_GG_DICTIONARY_MAX_ITERATOR( REGULAR_GG_DICTIONARY, MAX_ITERATOR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD,   ONLY: TOLOWER
  USE :: REPRESENTATIONS_MOD, ONLY: REGULAR_GG_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(REGULAR_GG_T),   INTENT(IN)    :: REGULAR_GG_DICTIONARY
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
  MAX_ITERATOR = REGULAR_GG_ITERATOR_UNDEF

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

END FUNCTION REGULAR_GG_DICTIONARY_MAX_ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REGULAR_GG_DICTIONARY_INIT_ITERATOR'
PP_THREAD_SAFE FUNCTION REGULAR_GG_DICTIONARY_INIT_ITERATOR( REGULAR_GG_DICTIONARY, ITERATOR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: REGULAR_GG_T
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
  TYPE(REGULAR_GG_T),   INTENT(IN)    :: REGULAR_GG_DICTIONARY
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

END FUNCTION REGULAR_GG_DICTIONARY_INIT_ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REGULAR_GG_DICTIONARY_GET_NEXT_ITERATOR'
PP_THREAD_SAFE FUNCTION REGULAR_GG_DICTIONARY_GET_NEXT_ITERATOR( REGULAR_GG_DICTIONARY, ITERATOR, END_OF_ITERATORS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: REGULAR_GG_T
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
  TYPE(REGULAR_GG_T),   INTENT(IN)    :: REGULAR_GG_DICTIONARY
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
    IF ( ITERATOR .EQ. REGULAR_GG_ITERATOR_UNDEF ) THEN
      END_OF_ITERATORS = .TRUE.
      HAS_KEY = .TRUE.
    ELSE
      PP_TRYCALL(ERRFLAG_HAS_ITERATOR) REGULAR_GG_DICTIONARY_HAS( REGULAR_GG_DICTIONARY, ITERATOR, HAS_KEY, HOOKS )
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

END FUNCTION REGULAR_GG_DICTIONARY_GET_NEXT_ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REGULAR_GG_DICTIONARY_NAME2ITERATOR'
PP_THREAD_SAFE FUNCTION REGULAR_GG_DICTIONARY_NAME2ITERATOR( REGULAR_GG_DICTIONARY, KEY, ITERATOR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: REGULAR_GG_T
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
  TYPE(REGULAR_GG_T),   INTENT(IN)    :: REGULAR_GG_DICTIONARY
  CHARACTER(LEN=*),     INTENT(IN)    :: KEY
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: ITERATOR
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=LEN(KEY)) :: KEY_LOW

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_REGULAR_GG_KEY=2_JPIB_K

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

  CASE ( 'truncatedegrees', 'truncate-degrees' )
    ITERATOR = REGULAR_GG_ITERATOR_TRUNCATE_DEGREES

  CASE ( 'numberofpointsalongameridian', 'number-of-points-along-a-meridian' )
    ITERATOR = REGULAR_GG_ITERATOR_NPTS_ALONG_MERIDIAN

  CASE ( 'numberofpointsalongaparallel', 'number-of-points-along-a-parallel' )
    ITERATOR = REGULAR_GG_ITERATOR_NPTS_ALONG_PARALLEL

  CASE ( 'numberofparallelsbetweenapoleandtheequator', 'number-of-parallels-between-a-pole-and-the-equator', 'number-of-parallels-between-pole-and-equator' )
    ITERATOR = REGULAR_GG_ITERATOR_NPARALLELS

  CASE ( 'latitudeoffirstgridpointindegrees', 'latitude-of-first-grid-point-in-degrees' )
    ITERATOR = REGULAR_GG_ITERATOR_LAT_FIRST_GP_DEG

  CASE ( 'longitudeoffirstgridpointindegrees', 'longitude-of-first-grid-point-in-degrees' )
    ITERATOR = REGULAR_GG_ITERATOR_LON_FIRST_GP_DEG

  CASE ( 'latitudeoflastgridpointindegrees', 'latitude-of-last-grid-point-in-degrees' )
    ITERATOR = REGULAR_GG_ITERATOR_LAT_LAST_GP_DEG

  CASE ( 'longitudeoflastgridpointindegrees', 'longitude-of-last-grid-point-in-degrees' )
    ITERATOR = REGULAR_GG_ITERATOR_LON_LAST_GP_DEG

  CASE ( 'idirectionincrementindegrees', 'i-direction-increment-in-degrees' )
    ITERATOR = REGULAR_GG_ITERATOR_IDIR_INC

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_REGULAR_GG_KEY )

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
    CASE(ERRFLAG_NO_REGULAR_GG_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'No REGULAR_GG key found' )
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

END FUNCTION REGULAR_GG_DICTIONARY_NAME2ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REGULAR_GG_DICTIONARY_HAS'
PP_THREAD_SAFE FUNCTION REGULAR_GG_DICTIONARY_HAS( REGULAR_GG_DICTIONARY, ITERATOR, HAS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: REGULAR_GG_T
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
  TYPE(REGULAR_GG_T),   INTENT(IN)    :: REGULAR_GG_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: ITERATOR
  LOGICAL,              INTENT(OUT)   :: HAS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_REGULAR_GG_KEY=2_JPIB_K

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

  CASE ( REGULAR_GG_ITERATOR_TRUNCATE_DEGREES )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_NPTS_ALONG_MERIDIAN )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_NPTS_ALONG_PARALLEL )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_NPARALLELS )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_LAT_FIRST_GP_DEG )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_LON_FIRST_GP_DEG )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_LAT_LAST_GP_DEG )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_LON_LAST_GP_DEG )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_IDIR_INC )
    HAS = .TRUE.

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_REGULAR_GG_KEY )

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
    CASE(ERRFLAG_NO_REGULAR_GG_KEY)
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

END FUNCTION REGULAR_GG_DICTIONARY_HAS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REGULAR_GG_DICTIONARY_GET_KEY_AS_STRING'
PP_THREAD_SAFE FUNCTION REGULAR_GG_DICTIONARY_GET_KEY_AS_STRING( REGULAR_GG_DICTIONARY, ITERATOR, KEY, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD, ONLY: REGULAR_GG_T
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
  TYPE(REGULAR_GG_T),   INTENT(IN)    :: REGULAR_GG_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITERATOR
  CHARACTER(LEN=64),    INTENT(OUT)   :: KEY
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_REGULAR_GG_KEY=2_JPIB_K
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

  CASE ( REGULAR_GG_ITERATOR_TRUNCATE_DEGREES )
    KEY = 'truncate-degrees'

  CASE ( REGULAR_GG_ITERATOR_NPTS_ALONG_MERIDIAN )
    KEY = 'number-of-points-along-a-meridian'

  CASE ( REGULAR_GG_ITERATOR_NPTS_ALONG_PARALLEL )
    KEY = 'number-of-points-along-a-parallel'

  CASE ( REGULAR_GG_ITERATOR_NPARALLELS )
    KEY = 'number-of-parallels-between-pole-and-equator'

  CASE ( REGULAR_GG_ITERATOR_LAT_FIRST_GP_DEG )
    KEY = 'latitude-of-first-grid-point-in-degrees'

  CASE ( REGULAR_GG_ITERATOR_LON_FIRST_GP_DEG )
    KEY = 'longitude-of-first-grid-point-in-degrees'

  CASE ( REGULAR_GG_ITERATOR_LAT_LAST_GP_DEG )
    KEY = 'latitude-of-last-grid-point-in-degrees'

  CASE ( REGULAR_GG_ITERATOR_LON_LAST_GP_DEG )
    KEY = 'longitude-of-last-grid-point-in-degrees'

  CASE ( REGULAR_GG_ITERATOR_IDIR_INC )
    KEY = 'iDirectionIncrementInDegrees'

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_REGULAR_GG_KEY )

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
    CASE(ERRFLAG_NO_REGULAR_GG_KEY)
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

END FUNCTION REGULAR_GG_DICTIONARY_GET_KEY_AS_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REGULAR_GG_DICTIONARY_GET_VALUE_AS_STRING'
PP_THREAD_SAFE FUNCTION REGULAR_GG_DICTIONARY_GET_VALUE_AS_STRING( REGULAR_GG_DICTIONARY, ITERATOR, VALUE, HAS, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD,   ONLY: REGULAR_GG_T
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
  TYPE(REGULAR_GG_T),   INTENT(IN)    :: REGULAR_GG_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITERATOR
  TYPE(C_PTR),          INTENT(OUT)   :: VALUE
  LOGICAL,              INTENT(OUT)   :: HAS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_REGULAR_GG_KEY=2_JPIB_K
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

  CASE ( REGULAR_GG_ITERATOR_TRUNCATE_DEGREES )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REGULAR_GG_DICTIONARY%TRUNCATE_DEGREES, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_NPTS_ALONG_MERIDIAN )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REGULAR_GG_DICTIONARY%NUMBER_OF_POINTS_ALONG_A_MERIDIAN, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_NPTS_ALONG_PARALLEL )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REGULAR_GG_DICTIONARY%NUMBER_OF_POINTS_ALONG_A_PARALLEL, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_NPARALLELS )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REGULAR_GG_DICTIONARY%NUMBER_OF_PARALLELS_BETWEEN_POLE_AND_EQUATOR, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_LAT_FIRST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REGULAR_GG_DICTIONARY%LAT_FIRST_GP_DEG, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_LON_FIRST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REGULAR_GG_DICTIONARY%LON_FIRST_GP_DEG, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_LAT_LAST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REGULAR_GG_DICTIONARY%LAT_LAST_GP_DEG, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_LON_LAST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REGULAR_GG_DICTIONARY%LON_LAST_GP_DEG, VALUE, HOOKS )
    HAS = .TRUE.

  CASE ( REGULAR_GG_ITERATOR_IDIR_INC )
    PP_TRYCALL(ERRFLAG_CONVERT_TO_STRING) CONVERT_TO_C_STRING( REGULAR_GG_DICTIONARY%IDIR_INC, VALUE, HOOKS )
    HAS = .TRUE.

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_REGULAR_GG_KEY )

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
    CASE(ERRFLAG_NO_REGULAR_GG_KEY)
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

END FUNCTION REGULAR_GG_DICTIONARY_GET_VALUE_AS_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REGULAR_GG_DICTIONARY_SET_VALUE_FROM_STRING'
PP_THREAD_SAFE FUNCTION REGULAR_GG_DICTIONARY_SET_VALUE_FROM_STRING( REGULAR_GG_DICTIONARY, ITERATOR, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD,     ONLY: REGULAR_GG_T
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
  TYPE(REGULAR_GG_T), INTENT(INOUT) :: REGULAR_GG_DICTIONARY
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
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_REGULAR_GG_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_REGULAR_GG_TO_ENUM=3_JPIB_K
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

  CASE ( REGULAR_GG_ITERATOR_TRUNCATE_DEGREES )
    PP_TRYCALL(ERRFLAG_CONVERT_REGULAR_GG_TO_ENUM) CINT2IINT(VALUE, ITEMP, HOOKS)
    REGULAR_GG_DICTIONARY%TRUNCATE_DEGREES=ITEMP

  CASE ( REGULAR_GG_ITERATOR_NPTS_ALONG_MERIDIAN )
    PP_TRYCALL(ERRFLAG_CONVERT_REGULAR_GG_TO_ENUM) CINT2IINT(VALUE, ITEMP, HOOKS)
    REGULAR_GG_DICTIONARY%NUMBER_OF_POINTS_ALONG_A_MERIDIAN=ITEMP

  CASE ( REGULAR_GG_ITERATOR_NPTS_ALONG_PARALLEL )
    PP_TRYCALL(ERRFLAG_CONVERT_REGULAR_GG_TO_ENUM) CINT2IINT(VALUE, ITEMP, HOOKS)
    REGULAR_GG_DICTIONARY%NUMBER_OF_POINTS_ALONG_A_PARALLEL=ITEMP

  CASE ( REGULAR_GG_ITERATOR_NPARALLELS )
    PP_TRYCALL(ERRFLAG_CONVERT_REGULAR_GG_TO_ENUM) CINT2IINT(VALUE, ITEMP, HOOKS)
    REGULAR_GG_DICTIONARY%NUMBER_OF_PARALLELS_BETWEEN_POLE_AND_EQUATOR=ITEMP

  CASE ( REGULAR_GG_ITERATOR_LAT_FIRST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_REGULAR_GG_TO_ENUM) CFLOAT2IFLOAT(VALUE, FTEMP, HOOKS)
    REGULAR_GG_DICTIONARY%LAT_FIRST_GP_DEG=FTEMP

  CASE ( REGULAR_GG_ITERATOR_LON_FIRST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_REGULAR_GG_TO_ENUM) CFLOAT2IFLOAT(VALUE, FTEMP, HOOKS)
    REGULAR_GG_DICTIONARY%LON_FIRST_GP_DEG=FTEMP

  CASE ( REGULAR_GG_ITERATOR_LAT_LAST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_REGULAR_GG_TO_ENUM) CFLOAT2IFLOAT(VALUE, FTEMP, HOOKS)
    REGULAR_GG_DICTIONARY%LAT_LAST_GP_DEG=FTEMP

  CASE ( REGULAR_GG_ITERATOR_LON_LAST_GP_DEG )
    PP_TRYCALL(ERRFLAG_CONVERT_REGULAR_GG_TO_ENUM) CFLOAT2IFLOAT(VALUE, FTEMP, HOOKS)
    REGULAR_GG_DICTIONARY%LON_LAST_GP_DEG=FTEMP

  CASE ( REGULAR_GG_ITERATOR_IDIR_INC )
    PP_TRYCALL(ERRFLAG_CONVERT_REGULAR_GG_TO_ENUM) CFLOAT2IFLOAT(VALUE, FTEMP, HOOKS)
    REGULAR_GG_DICTIONARY%IDIR_INC=FTEMP

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_REGULAR_GG_KEY )

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
    CASE( ERRFLAG_NO_REGULAR_GG_KEY )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid enumerator found' )
    CASE( ERRFLAG_CONVERT_REGULAR_GG_TO_ENUM )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert REGULAR_GG to enumerator' )
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

END FUNCTION REGULAR_GG_DICTIONARY_SET_VALUE_FROM_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REGULAR_GG_DICTIONARY_SET_VALUE_FROM_INT64'
PP_THREAD_SAFE FUNCTION REGULAR_GG_DICTIONARY_SET_VALUE_FROM_INT64( REGULAR_GG_DICTIONARY, ITERATOR, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD,     ONLY: REGULAR_GG_T
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
  TYPE(REGULAR_GG_T),   INTENT(INOUT) :: REGULAR_GG_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITERATOR
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_REGULAR_GG_KEY=2_JPIB_K

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

  CASE ( REGULAR_GG_ITERATOR_TRUNCATE_DEGREES )
    REGULAR_GG_DICTIONARY%TRUNCATE_DEGREES = VALUE

  CASE ( REGULAR_GG_ITERATOR_NPTS_ALONG_MERIDIAN )
    REGULAR_GG_DICTIONARY%NUMBER_OF_POINTS_ALONG_A_MERIDIAN = VALUE

  CASE ( REGULAR_GG_ITERATOR_NPTS_ALONG_PARALLEL )
    REGULAR_GG_DICTIONARY%NUMBER_OF_POINTS_ALONG_A_PARALLEL = VALUE

  CASE ( REGULAR_GG_ITERATOR_NPARALLELS )
    REGULAR_GG_DICTIONARY%NUMBER_OF_PARALLELS_BETWEEN_POLE_AND_EQUATOR = VALUE

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_REGULAR_GG_KEY )

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
    CASE( ERRFLAG_NO_REGULAR_GG_KEY )
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

END FUNCTION REGULAR_GG_DICTIONARY_SET_VALUE_FROM_INT64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REGULAR_GG_DICTIONARY_SET_VALUE_FROM_REAL64'
PP_THREAD_SAFE FUNCTION REGULAR_GG_DICTIONARY_SET_VALUE_FROM_REAL64( REGULAR_GG_DICTIONARY, ITERATOR, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD,     ONLY: REGULAR_GG_T
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
  TYPE(REGULAR_GG_T),   INTENT(INOUT) :: REGULAR_GG_DICTIONARY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ITERATOR
  REAL(KIND=JPRD_K),    INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_REGULAR_GG_KEY=2_JPIB_K

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

  CASE ( REGULAR_GG_ITERATOR_LAT_FIRST_GP_DEG )
    REGULAR_GG_DICTIONARY%LAT_FIRST_GP_DEG = VALUE

  CASE ( REGULAR_GG_ITERATOR_LON_FIRST_GP_DEG )
    REGULAR_GG_DICTIONARY%LON_FIRST_GP_DEG = VALUE

  CASE ( REGULAR_GG_ITERATOR_LAT_LAST_GP_DEG )
    REGULAR_GG_DICTIONARY%LAT_LAST_GP_DEG = VALUE

  CASE ( REGULAR_GG_ITERATOR_LON_LAST_GP_DEG )
    REGULAR_GG_DICTIONARY%LON_LAST_GP_DEG = VALUE

  CASE ( REGULAR_GG_ITERATOR_IDIR_INC )
    REGULAR_GG_DICTIONARY%IDIR_INC = VALUE

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_REGULAR_GG_KEY )

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
    CASE( ERRFLAG_NO_REGULAR_GG_KEY )
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

END FUNCTION REGULAR_GG_DICTIONARY_SET_VALUE_FROM_REAL64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REGULAR_GG_DICTIONARY_SET_VALUE_FROM_INT64_ARRAY'
PP_THREAD_SAFE FUNCTION REGULAR_GG_DICTIONARY_SET_VALUE_FROM_INT64_ARRAY( REGULAR_GG_DICTIONARY, ITERATOR, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD,     ONLY: REGULAR_GG_T
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
  TYPE(REGULAR_GG_T),                 INTENT(INOUT) :: REGULAR_GG_DICTIONARY
  INTEGER(KIND=JPIB_K),               INTENT(IN)    :: ITERATOR
  INTEGER(KIND=JPIB_K), DIMENSION(:), INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_REGULAR_GG_KEY=2_JPIB_K

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

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_REGULAR_GG_KEY )

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
    CASE( ERRFLAG_NO_REGULAR_GG_KEY )
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

END FUNCTION REGULAR_GG_DICTIONARY_SET_VALUE_FROM_INT64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REGULAR_GG_DICTIONARY_SET_VALUE_FROM_REAL64_ARRAY'
PP_THREAD_SAFE FUNCTION REGULAR_GG_DICTIONARY_SET_VALUE_FROM_REAL64_ARRAY( REGULAR_GG_DICTIONARY, ITERATOR, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: REPRESENTATIONS_MOD,     ONLY: REGULAR_GG_T
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
  TYPE(REGULAR_GG_T),              INTENT(INOUT) :: REGULAR_GG_DICTIONARY
  INTEGER(KIND=JPIB_K),            INTENT(IN)    :: ITERATOR
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_REGULAR_GG_KEY=2_JPIB_K

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

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NO_REGULAR_GG_KEY )

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
    CASE( ERRFLAG_NO_REGULAR_GG_KEY )
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

END FUNCTION REGULAR_GG_DICTIONARY_SET_VALUE_FROM_REAL64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE API_REGULAR_GG_DICTIONARY_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
