! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'repres_record_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'REPRES_RECORD_MOD'
MODULE REPRES_RECORD_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E
  USE :: RECORD_BASE_MOD,   ONLY: RECORD_BASE_A

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!> Maximum lenght of the calss value as string
INTEGER(KIND=JPIB_K), PARAMETER :: CVALUE_LEN=2_JPIB_K

!> Record used to wrap a repres value
TYPE, EXTENDS(RECORD_BASE_A) :: REPRES_RECORD_T

  !> Default visibility of the type
  PRIVATE

  !> Value
  INTEGER(KIND=JPIB_K) :: VALUE_= UNDEF_PARAM_E

CONTAINS

  !> @brief Initializes the record
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: INIT

  !> @brief Checks if the record has been initialized
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: HAS

  !> @brief Checks if the record has been initialized
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: IS_SCALAR
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: IS_RANGE
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: IS_ARRAY

  !> @brief Reset to value to an unitialized value
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: RESET

  !> @brief Set the value of the record
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: SET_STRING
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: SET_BOOL
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: SET_INT64
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: SET_REAL64
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: SET_INT64_RANGE
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: SET_INT64_ARRAY
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: SET_REAL64_ARRAY

  !> @brief Get the value of the record
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: GET_STRING
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: GET_BOOL
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: GET_INT64
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: GET_REAL64
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: GET_INT64_RANGE
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: GET_INT64_ARRAY
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: GET_REAL64_ARRAY

  !> @brief Convert the record to a string to be printed
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: TO_STRING
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: IS_LOWER_THAN
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: IS_EQUAL_TO
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: COPY_FROM

  !> @brief Free the record (reset all internal fields)
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FREE

  ! Generic interface for setting values
  GENERIC, PUBLIC :: SET => SET_STRING
  GENERIC, PUBLIC :: SET => SET_BOOL
  GENERIC, PUBLIC :: SET => SET_INT64
  GENERIC, PUBLIC :: SET => SET_INT64_ARRAY
  GENERIC, PUBLIC :: SET => SET_INT_64_RANGE
  GENERIC, PUBLIC :: SET => SET_REAL64
  GENERIC, PUBLIC :: SET => SET_REAL64_ARRAY


  ! Generic interface for setting values
  GENERIC, PUBLIC :: GET => GET_STRING
  GENERIC, PUBLIC :: GET => GET_BOOL
  GENERIC, PUBLIC :: GET => GET_INT64
  GENERIC, PUBLIC :: GET => GET_INT64_ARRAY
  GENERIC, PUBLIC :: GET => GET_INT_64_RANGE
  GENERIC, PUBLIC :: GET => GET_REAL64
  GENERIC, PUBLIC :: GET => GET_REAL64_ARRAY

END TYPE

! Whitelist of public symbols
PUBLIC :: REPRES_RECORD_T

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_INIT'
PP_THREAD_SAFE FUNCTION REPRES_INIT( THIS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

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

  ! Initialize the values of the record
  THIS%INITIALIZED = .FALSE.
  THIS%VALUE_ = UNDEF_PARAM_E

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION REPRES_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_HAS'
PP_THREAD_SAFE FUNCTION REPRES_HAS( THIS, HAS, HOOKS ) RESULT(RET)

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
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  LOGICAL,               INTENT(OUT)   :: HAS
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

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

  ! Initialize the values of the record
  HAS = THIS%INITIALIZED_

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION REPRES_HAS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_IS_SCALAR'
PP_THREAD_SAFE FUNCTION REPRES_IS_SCALAR( THIS, IS_SCALAR, HOOKS ) RESULT(RET)

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
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  LOGICAL,               INTENT(OUT)   :: IS_SCALAR
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

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

  ! Initialize the values of the record
  IS_SCALAR = .TRUE.

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION REPRES_IS_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_IS_RANGE'
PP_THREAD_SAFE FUNCTION REPRES_IS_RANGE( THIS, IS_RANGE, HOOKS ) RESULT(RET)

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
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  LOGICAL,               INTENT(OUT)   :: IS_RANGE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

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

  ! Initialize the values of the record
  IS_RANGE = .FALSE.

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION REPRES_IS_RANGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_IS_ARRAY'
PP_THREAD_SAFE FUNCTION REPRES_IS_ARRAY( THIS, IS_ARRAY, HOOKS ) RESULT(RET)

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
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  LOGICAL,               INTENT(OUT)   :: IS_ARRAY
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

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

  ! Initialize the values of the record
  IS_ARRAY = .FALSE.

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION REPRES_IS_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_SET_STRING'
PP_THREAD_SAFE FUNCTION REPRES_SET_STRING( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: CREPRES2IREPRES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_WRONG_LENGTH=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_ENUM=2_JPIB_K

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

  ! Initialize the values of the record
  PP_DEBUG_CRITICAL_COND_THROW( LEN(VALUE).LT.CVALUE_LEN, ERRFLAG_VALUE_WRONG_LENGTH )
  P_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_ENUM) CREPRES2ICLASS(VALUE, THIS%VALUE_, HOOKS)

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
    CASE (ERRFLAG_VALUE_WRONG_LENGTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid length for "repres"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input value: ' // TRIM(VALUE) )
    CASE (ERRFLAG_UNABLE_TO_CONVERT_TO_ENUM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert "repres" to enum' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input value: ' // TRIM(VALUE) )
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

END FUNCTION REPRES_SET_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_SET_BOOL'
PP_THREAD_SAFE FUNCTION REPRES_SET_BOOL( THIS, VALUE, HOOKS ) RESULT(RET)

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
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  LOGICAL,               INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_APPLICABLE=1_JPIB_K

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

  ! Initialize the values of the record
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_APPLICABLE )

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
    CHARACTER(LEN=32) :: CTMP
    INTEGER(KIND=JPIB_K) :: WRITE_STATUS

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_NOT_APPLICABLE)
      CTMP = REPEAT(' ', 32)
      WRITE(CTMP, '(L)', IOSTAT=WRITE_STATUS) VALUE
      PP_DEBUG_PUSH_MSG_TO_FRAME( '"repres" cannot be converted to bool' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input value: ' // TRIM(CTMP) )
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

END FUNCTION REPRES_SET_BOOL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_SET_INT64'
PP_THREAD_SAFE FUNCTION REPRES_SET_INT64( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E
  USE :: ENUMERATORS_MOD,   ONLY: IREPRES2CREPRES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=CVALUE_LEN) :: CVALUE

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_STRING=1_JPIB_K

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

  ! Try to convert to string just to check that the value is valid
  CVALUE = REPEAT(' ', CVALUE_LEN)
  P_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_STRING) IREPRES2CCLASS(VALUE, CVALUE, HOOKS)

  ! Initialize the values of the record
  THIS%VALUE_ = VALUE

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
    CHARACTER(LEN=32) :: CTMP
    INTEGER(KIND=JPIB_K) :: WRITE_STATUS

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_NOT_APPLICABLE)
      CTMP = REPEAT(' ', 32)
      WRITE(CTMP, '(I32)', IOSTAT=WRITE_STATUS) VALUE
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid enum for "repres"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input value: ' // TRIM(CTMP) )
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

END FUNCTION REPRES_SET_INT64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_SET_INT64_RANGE'
PP_THREAD_SAFE FUNCTION REPRES_SET_INT64_RANGE( THIS, VALUE1, VALUE2, HOOKS ) RESULT(RET)

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
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  LOGICAL,               INTENT(IN)    :: VALUE1
  LOGICAL,               INTENT(IN)    :: VALUE2
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_APPLICABLE=1_JPIB_K

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

  ! Initialize the values of the record
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_APPLICABLE )

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
    CASE (ERRFLAG_NOT_APPLICABLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( '"repres" cannot be converted to int64-range' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input value: ' // TRIM(VALUE) )
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

END FUNCTION REPRES_SET_INT64_RANGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_SET_INT64_ARRAY'
PP_THREAD_SAFE FUNCTION REPRES_SET_INT64_ARRAY( THIS, VALUE, HOOKS ) RESULT(RET)

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
  CLASS(REPRES_RECORD_T),              INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K), DIMENSION(:), INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_APPLICABLE=1_JPIB_K

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

  ! Initialize the values of the record
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_APPLICABLE )

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
    CASE (ERRFLAG_NOT_APPLICABLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( '"repres" cannot be converted to int64-array' )
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

END FUNCTION REPRES_SET_INT64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_SET_REAL64'
PP_THREAD_SAFE FUNCTION REPRES_SET_REAL64( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  REAL(KIND=JPRD_K),     INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_APPLICABLE=1_JPIB_K

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

  ! Initialize the values of the record
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_APPLICABLE )

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
    CASE (ERRFLAG_NOT_APPLICABLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( '"repres" cannot be converted to real64' )
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

END FUNCTION REPRES_SET_REAL64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_SET_REAL64_ARRAY'
PP_THREAD_SAFE FUNCTION REPRES_SET_REAL64_ARRAY( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(REPRES_RECORD_T),           INTENT(INOUT) :: THIS
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_APPLICABLE=1_JPIB_K

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

  ! Initialize the values of the record
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_APPLICABLE )

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
    CASE (ERRFLAG_NOT_APPLICABLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( '"repres" cannot be converted to real64-array' )
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

END FUNCTION REPRES_SET_REAL64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_GET_STRING'
PP_THREAD_SAFE FUNCTION REPRES_GET_STRING( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: IREPRES2CREPRES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_UNDEFINED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_WRONG_LENGTH=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_STRING=4_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_GET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_VALUE_NOT_INITIALIZED )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%VALUE_.EQ.UNDEF_PARAM_E, ERRFLAG_VALUE_UNDEFINED )
  PP_DEBUG_CRITICAL_COND_THROW( LEN(VALUE).LT.CVALUE_LEN, ERRFLAG_VALUE_WRONG_LENGTH )

  ! Initialize the values of the record
  VALUE = REPEAT(' ', LEN(VALUE))
  P_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_STRING) IREPRES2CCLASS(THIS%VALUE_, VALUE, HOOKS)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_GET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_VALUE_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value for "repres" is not initialized' )
    CASE (ERRFLAG_VALUE_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value for "repres" is not defined' )
    CASE (ERRFLAG_VALUE_WRONG_LENGTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value for "repres" has wrong length' )
    CASE (ERRFLAG_UNABLE_TO_CONVERT_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert "repres" to string' )
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

END FUNCTION REPRES_GET_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_GET_BOOL'
PP_THREAD_SAFE FUNCTION REPRES_GET_BOOL( THIS, VALUE, HOOKS ) RESULT(RET)

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
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  LOGICAL,               INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_APPLICABLE=1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_GET_ERR_SUCCESS( RET )

  ! Initialize the values of the record
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_APPLICABLE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_GET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_NOT_APPLICABLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( '"repres" cannot be converted to bool' )
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

END FUNCTION REPRES_GET_BOOL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_GET_INT64'
PP_THREAD_SAFE FUNCTION REPRES_GET_INT64( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),  INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_UNDEFINED=2_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_GET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.THIS%INITIALIZED_,       ERRFLAG_VALUE_NOT_INITIALIZED )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%VALUE_.EQ.UNDEF_PARAM_E, ERRFLAG_VALUE_UNDEFINED )

  ! Initialize the values of the record
  VALUE = HIS%VALUE_

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_GET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_VALUE_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value for "repres" is not initialized' )
    CASE (ERRFLAG_VALUE_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value for "repres" is not defined' )
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

END FUNCTION REPRES_GET_INT64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_GET_INT64_RANGE'
PP_THREAD_SAFE FUNCTION REPRES_GET_INT64_RANGE( THIS, VALUE1, VALUE2, HOOKS ) RESULT(RET)

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
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  LOGICAL,               INTENT(OUT)   :: VALUE1
  LOGICAL,               INTENT(OUT)   :: VALUE2
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_APPLICABLE=1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_GET_ERR_SUCCESS( RET )

  ! Initialize the values of the record
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_APPLICABLE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_GET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_NOT_APPLICABLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( '"repres" cannot be converted to int64-range' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input value: ' // TRIM(VALUE) )
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

END FUNCTION REPRES_GET_INT64_RANGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_GET_INT64_ARRAY'
PP_THREAD_SAFE FUNCTION REPRES_GET_INT64_ARRAY( THIS, VALUE, HOOKS ) RESULT(RET)

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
  CLASS(REPRES_RECORD_T),              INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K), DIMENSION(:), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_APPLICABLE=1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_GET_ERR_SUCCESS( RET )

  ! Initialize the values of the record
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_APPLICABLE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_GET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_NOT_APPLICABLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( '"repres" cannot be converted to int64-array' )
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

END FUNCTION REPRES_GET_INT64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_GET_REAL64'
PP_THREAD_SAFE FUNCTION REPRES_GET_REAL64( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  REAL(KIND=JPRD_K),     INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_APPLICABLE=1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_GET_ERR_SUCCESS( RET )

  ! Initialize the values of the record
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_APPLICABLE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_GET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_NOT_APPLICABLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( '"repres" cannot be converted to real64' )
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

END FUNCTION REPRES_GET_REAL64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_GET_REAL64_ARRAY'
PP_THREAD_SAFE FUNCTION REPRES_GET_REAL64_ARRAY( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(REPRES_RECORD_T),           INTENT(INOUT) :: THIS
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_APPLICABLE=1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_GET_ERR_SUCCESS( RET )

  ! Initialize the values of the record
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_APPLICABLE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_GET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_NOT_APPLICABLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( '"repres" cannot be converted to real64-array' )
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

END FUNCTION REPRES_GET_REAL64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_TO_STRING'
PP_THREAD_SAFE FUNCTION REPRES_TO_STRING( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: IREPRES2CREPRES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Errorf flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_UNDEFINED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_WRONG_LENGTH=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_STRING=4_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_GET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_VALUE_NOT_INITIALIZED )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%VALUE_.EQ.UNDEF_PARAM_E, ERRFLAG_VALUE_UNDEFINED )
  PP_DEBUG_CRITICAL_COND_THROW( LEN(VALUE).LT.CVALUE_LEN, ERRFLAG_VALUE_WRONG_LENGTH )

  ! Initialize the values of the record
  VALUE = REPEAT(' ', LEN(VALUE))
  P_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_STRING) IREPRES2CCLASS(THIS%VALUE_, VALUE, HOOKS)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_GET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_VALUE_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value for "repres" is not initialized' )
    CASE (ERRFLAG_VALUE_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value for "repres" is not defined' )
    CASE (ERRFLAG_VALUE_WRONG_LENGTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value for "repres" has wrong length' )
    CASE (ERRFLAG_UNABLE_TO_CONVERT_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert "repres" to string' )
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

END FUNCTION REPRES_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_IS_EQUAL_TO'
PP_THREAD_SAFE FUNCTION REPRES_IS_EQUAL_TO( THIS, OTHER, IS_EQUAL_TO, HOOKS ) RESULT(RET)

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
  CLASS(REPRES_RECORD_T),         INTENT(INOUT) :: THIS
  CLASS(RECORD_BASE_A), POINTER, INTENT(IN)    :: OTHER
  LOGICAL,                       INTENT(OUT)   :: IS_EQUAL_TO
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: OTHER_VALUE

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OTHER_UNABLE_TO_CHECK=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OTHER_UNABLE_TO_GET=2_JPIB_K

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

  ! Initialize the values of the record
  SELECT TYPE(O => OTHER)
  CLASS IS (REPRES_RECORD_T)
    PP_TRYCALL(ERRFLAG_OTHER_UNABLE_TO_CHECK) O%HAS( HAS, HOOKS )
    IF ( THIS%INITIALIZED_ .AND HAS ) THEN
      PP_TRYCALL(ERRFLAG_OTHER_UNABLE_TO_GET) O%GET_INT64( OTHER_VALUE, HOOKS )
      IS_EQUAL_TO = THIS%VALUE_ .EQ. OTHER_VALUE
    ELSEIF ( THIS%INITIALIZED_ .AND .NOT.HAS ) THEN
      IS_EQUAL_TO = .FALSE.
    ELSEIF ( .NOT.THIS%INITIALIZED_ .AND. HAS ) THEN
      IS_EQUAL_TO = .FALSE.
    ELSE
      IS_EQUAL_TO = .TRUE.
    END IF
    IS_EQUAL_TO = THIS%VALUE_ == OTHER%VALUE_
  CLASS DEFAULT
    IS_EQUAL_TO = .FALSE.
  END SELECT

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
    CHARACTER(LEN=16) :: TMPSTR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_OTHER_UNABLE_TO_CHECK)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check if "other" has the value' )
    CASE (ERRFLAG_OTHER_UNABLE_TO_GET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the value of "other"' )
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

END FUNCTION REPRES_IS_EQUAL_TO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_IS_LOWER_THAN'
PP_THREAD_SAFE FUNCTION REPRES_IS_LOWER_THAN( THIS, OTHER, IS_LOWER_THAN, HOOKS ) RESULT(RET)

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
  CLASS(REPRES_RECORD_T),         INTENT(INOUT) :: THIS
  CLASS(RECORD_BASE_A), POINTER, INTENT(IN)    :: OTHER
  LOGICAL,                       INTENT(OUT)   :: IS_LOWER_THAN
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: OTHER_VALUE

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OTHER_UNABLE_TO_CHECK=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OTHER_UNABLE_TO_GET=2_JPIB_K

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

  ! Initialize the values of the record
  SELECT TYPE(0 => OTHER)
  CLASS IS (REPRES_RECORD_T)
    PP_TRYCALL(ERRFLAG_OTHER_UNABLE_TO_CHECK) O%HAS( HAS, HOOKS )
    IF ( THIS%INITIALIZED_ .AND HAS ) THEN
      PP_TRYCALL(ERRFLAG_OTHER_UNABLE_TO_GET) O%GET_INT64( OTHER_VALUE, HOOKS )
      IS_EQUAL_TO = THIS%VALUE_ .LT. OTHER_VALUE
    ELSEIF ( THIS%INITIALIZED_ .AND .NOT.HAS ) THEN
      IS_EQUAL_TO = .FALSE.
    ELSEIF ( .NOT.THIS%INITIALIZED_ .AND. HAS ) THEN
      IS_EQUAL_TO = .FALSE.
    ELSE
      IS_EQUAL_TO = .TRUE.
    END IF
  CLASS DEFAULT
    IS_EQUAL_TO = .FALSE.
  END SELECT

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
    CHARACTER(LEN=16) :: TMPSTR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_OTHER_UNABLE_TO_CHECK)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check if "other" has the value' )
    CASE (ERRFLAG_OTHER_UNABLE_TO_GET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the value of "other"' )
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

END FUNCTION REPRES_IS_LOWER_THAN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_COPY_FROM'
PP_THREAD_SAFE FUNCTION REPRES_COPY_FROM( THIS, OTHER, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(REPRES_RECORD_T),         INTENT(INOUT) :: THIS
  CLASS(RECORD_BASE_A), POINTER, INTENT(IN)    :: OTHER
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: OTHER_VALUE

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OTHER_UNABLE_TO_CHECK=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OTHER_UNABLE_TO_GET=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_OTHER_REPRES=3_JPIB_K

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

  ! Initialize the values of the record
  SELECT TYPE(OTHER)
  CLASS IS (O => REPRES_RECORD_T)
    PP_TRYCALL(ERRFLAG_OTHER_UNABLE_TO_CHECK) O%HAS( HAS, HOOKS )
    IF ( HAS ) THEN
      PP_TRYCALL(ERRFLAG_OTHER_UNABLE_TO_GET) O%GET_INT64( OTHER_VALUE, HOOKS )
      THIS%INITIALIZED_ = .TRUE.
      THIS%VALUE_ = OTHER_VALUE
    ELSE
      THIS%INITIALIZED_ = .FALSE.
      THIS%VALUE_ = UNDEF_PARAM_E
    END IF
  CLASS DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_WRONG_OTHER_REPRES )
  END SELECT

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
    CHARACTER(LEN=16) :: TMPSTR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_OTHER_UNABLE_TO_CHECK)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check if "other" has the value' )
    CASE (ERRFLAG_OTHER_UNABLE_TO_GET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the value of "other"' )
    CASE (ERRFLAG_WRONG_OTHER_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong repres for "other"' )
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

END FUNCTION REPRES_COPY_FROM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPRES_FREE'
PP_THREAD_SAFE FUNCTION REPRES_FREE( THIS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(REPRES_RECORD_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

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

  ! Initialize the values of the record
  THIS%INITIALIZED = .FALSE.
  THIS%VALUE_ = UNDEF_PARAM_E

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION REPRES_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE REPRES_RECORD_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME