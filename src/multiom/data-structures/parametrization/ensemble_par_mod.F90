! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'ensemble_par_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ENSEMBLE_PAR_MOD'
MODULE ENSEMBLE_PAR_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

  !> Ensemble information I am not able to fit into mars message
  TYPE :: ENSEMBLE_PAR_T
    ! PRIVATE
    INTEGER(KIND=JPIB_K) :: SYSTEM_NUMBER_= UNDEF_PARAM_E
    INTEGER(KIND=JPIB_K) :: METHOD_NUMBER_= UNDEF_PARAM_E
    INTEGER(KIND=JPIB_K) :: TYPE_OF_ENSEMBLE_FORECAST_= UNDEF_PARAM_E
    INTEGER(KIND=JPIB_K) :: NUMBER_OF_FORECASTS_IN_ENSEMBLE_= UNDEF_PARAM_E
  CONTAINS
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: COPY_FROM => ENSEMBLE_PAR_COPY_FROM
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: READ_FROM_YAML => READ_ENSEMBLE_PAR_FROM_YAML

    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_METHOD_NUMBER => ENSEMBLE_PAR_SET_METHOD_NUMBER
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_METHOD_NUMBER => ENSEMBLE_PAR_GET_METHOD_NUMBER

    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_SYSTEM_NUMBER => ENSEMBLE_PAR_SET_SYSTEM_NUMBER
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_SYSTEM_NUMBER => ENSEMBLE_PAR_GET_SYSTEM_NUMBER

    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_TYPE_OF_ENSEMBLE_FORECAST => ENSEMBLE_PAR_SET_TYPE_OF_ENSEMBLE_FORECAST
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_TYPE_OF_ENSEMBLE_FORECAST => ENSEMBLE_PAR_GET_TYPE_OF_ENSEMBLE_FORECAST
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_NUMBER_OF_FORECASTS_IN_ENSEMBLE => ENSEMBLE_PAR_SET_NUMBER_OF_FORECASTS_IN_ENSEMBLE
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_NUMBER_OF_FORECASTS_IN_ENSEMBLE => ENSEMBLE_PAR_GET_NUMBER_OF_FORECASTS_IN_ENSEMBLE
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: WRITE_TO_YAML => WRITE_ENSEMBLE_PAR_TO_YAML
    PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE => ENSEMBLE_PAR_FREE
  END TYPE

  !> Whitelist of public symbols (types)
  PUBLIC :: ENSEMBLE_PAR_T

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENSEMBLE_PAR_FREE'
PP_THREAD_SAFE FUNCTION ENSEMBLE_PAR_FREE( ENSEMBLE_PAR, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENSEMBLE_PAR_T), INTENT(INOUT) :: ENSEMBLE_PAR
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

  ! Copy the data
  ENSEMBLE_PAR%SYSTEM_NUMBER_ = UNDEF_PARAM_E
  ENSEMBLE_PAR%METHOD_NUMBER_ = UNDEF_PARAM_E
  ENSEMBLE_PAR%TYPE_OF_ENSEMBLE_FORECAST_ = UNDEF_PARAM_E
  ENSEMBLE_PAR%NUMBER_OF_FORECASTS_IN_ENSEMBLE_ = UNDEF_PARAM_E

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

END FUNCTION ENSEMBLE_PAR_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENSEMBLE_PAR_SET_SYSTEM_NUMBER'
PP_THREAD_SAFE FUNCTION ENSEMBLE_PAR_SET_SYSTEM_NUMBER( ENSEMBLE_PAR, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENSEMBLE_PAR_T), INTENT(INOUT) :: ENSEMBLE_PAR
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUE = 1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( VALUE.LT.0, ERRFLAG_INVALID_VALUE )

  ! Copy the data
  ENSEMBLE_PAR%SYSTEM_NUMBER_ = VALUE

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
    CASE(ERRFLAG_INVALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid value' )
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

END FUNCTION ENSEMBLE_PAR_SET_SYSTEM_NUMBER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENSEMBLE_PAR_GET_SYSTEM_NUMBER'
PP_THREAD_SAFE FUNCTION ENSEMBLE_PAR_GET_SYSTEM_NUMBER( ENSEMBLE_PAR, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENSEMBLE_PAR_T), INTENT(IN)    :: ENSEMBLE_PAR
  INTEGER(KIND=JPIB_K),  INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUE = 1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ENSEMBLE_PAR%SYSTEM_NUMBER_.LT.0, ERRFLAG_INVALID_VALUE )

  ! Copy the data
  VALUE = ENSEMBLE_PAR%SYSTEM_NUMBER_

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
    CASE(ERRFLAG_INVALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid value' )
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

END FUNCTION ENSEMBLE_PAR_GET_SYSTEM_NUMBER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENSEMBLE_PAR_SET_METHOD_NUMBER'
PP_THREAD_SAFE FUNCTION ENSEMBLE_PAR_SET_METHOD_NUMBER( ENSEMBLE_PAR, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENSEMBLE_PAR_T), INTENT(INOUT) :: ENSEMBLE_PAR
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUE = 1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( VALUE.LT.0, ERRFLAG_INVALID_VALUE )

  ! Copy the data
  ENSEMBLE_PAR%METHOD_NUMBER_ = VALUE

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
    CASE(ERRFLAG_INVALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid value' )
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

END FUNCTION ENSEMBLE_PAR_SET_METHOD_NUMBER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENSEMBLE_PAR_GET_METHOD_NUMBER'
PP_THREAD_SAFE FUNCTION ENSEMBLE_PAR_GET_METHOD_NUMBER( ENSEMBLE_PAR, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENSEMBLE_PAR_T), INTENT(IN)    :: ENSEMBLE_PAR
  INTEGER(KIND=JPIB_K),  INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUE = 1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ENSEMBLE_PAR%METHOD_NUMBER_.LT.0, ERRFLAG_INVALID_VALUE )

  ! Copy the data
  VALUE = ENSEMBLE_PAR%METHOD_NUMBER_

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
    CASE(ERRFLAG_INVALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid value' )
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

END FUNCTION ENSEMBLE_PAR_GET_METHOD_NUMBER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE








#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENSEMBLE_PAR_SET_TYPE_OF_ENSEMBLE_FORECAST'
PP_THREAD_SAFE FUNCTION ENSEMBLE_PAR_SET_TYPE_OF_ENSEMBLE_FORECAST( ENSEMBLE_PAR, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENSEMBLE_PAR_T), INTENT(INOUT) :: ENSEMBLE_PAR
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUE = 1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( VALUE.LT.0, ERRFLAG_INVALID_VALUE )

  ! Copy the data
  ENSEMBLE_PAR%TYPE_OF_ENSEMBLE_FORECAST_ = VALUE

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
    CASE(ERRFLAG_INVALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid value' )
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

END FUNCTION ENSEMBLE_PAR_SET_TYPE_OF_ENSEMBLE_FORECAST
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENSEMBLE_PAR_GET_TYPE_OF_ENSEMBLE_FORECAST'
PP_THREAD_SAFE FUNCTION ENSEMBLE_PAR_GET_TYPE_OF_ENSEMBLE_FORECAST( ENSEMBLE_PAR, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENSEMBLE_PAR_T), INTENT(IN)    :: ENSEMBLE_PAR
  INTEGER(KIND=JPIB_K),  INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUE = 1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ENSEMBLE_PAR%TYPE_OF_ENSEMBLE_FORECAST_.LT.0, ERRFLAG_INVALID_VALUE )

  ! Copy the data
  VALUE = ENSEMBLE_PAR%TYPE_OF_ENSEMBLE_FORECAST_

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
    CASE(ERRFLAG_INVALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid value' )
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

END FUNCTION ENSEMBLE_PAR_GET_TYPE_OF_ENSEMBLE_FORECAST
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENSEMBLE_PAR_SET_NUMBER_OF_FORECASTS_IN_ENSEMBLE'
PP_THREAD_SAFE FUNCTION ENSEMBLE_PAR_SET_NUMBER_OF_FORECASTS_IN_ENSEMBLE( ENSEMBLE_PAR, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENSEMBLE_PAR_T), INTENT(INOUT) :: ENSEMBLE_PAR
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUE = 1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( VALUE.LE.0, ERRFLAG_INVALID_VALUE )

  ! Copy the data
  ENSEMBLE_PAR%NUMBER_OF_FORECASTS_IN_ENSEMBLE_ = VALUE

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
    CASE(ERRFLAG_INVALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid value' )
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

END FUNCTION ENSEMBLE_PAR_SET_NUMBER_OF_FORECASTS_IN_ENSEMBLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENSEMBLE_PAR_GET_NUMBER_OF_FORECASTS_IN_ENSEMBLE'
PP_THREAD_SAFE FUNCTION ENSEMBLE_PAR_GET_NUMBER_OF_FORECASTS_IN_ENSEMBLE( ENSEMBLE_PAR, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENSEMBLE_PAR_T), INTENT(IN)    :: ENSEMBLE_PAR
  INTEGER(KIND=JPIB_K),  INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUE = 1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ENSEMBLE_PAR%NUMBER_OF_FORECASTS_IN_ENSEMBLE_.LE.0, ERRFLAG_INVALID_VALUE )

  ! Copy the data
  VALUE = ENSEMBLE_PAR%NUMBER_OF_FORECASTS_IN_ENSEMBLE_

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
    CASE(ERRFLAG_INVALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid value' )
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

END FUNCTION ENSEMBLE_PAR_GET_NUMBER_OF_FORECASTS_IN_ENSEMBLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENSEMBLE_PAR_COPY_FROM'
PP_THREAD_SAFE FUNCTION ENSEMBLE_PAR_COPY_FROM( ENSEMBLE_PAR_TO, ENSEMBLE_PAR_FROM, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENSEMBLE_PAR_T), INTENT(INOUT) :: ENSEMBLE_PAR_TO
  TYPE(ENSEMBLE_PAR_T), INTENT(IN)    :: ENSEMBLE_PAR_FROM
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

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

  ! Copy the data
  ENSEMBLE_PAR_TO%SYSTEM_NUMBER_ = ENSEMBLE_PAR_FROM%SYSTEM_NUMBER_
  ENSEMBLE_PAR_TO%METHOD_NUMBER_ = ENSEMBLE_PAR_FROM%METHOD_NUMBER_
  ENSEMBLE_PAR_TO%TYPE_OF_ENSEMBLE_FORECAST_ = ENSEMBLE_PAR_FROM%TYPE_OF_ENSEMBLE_FORECAST_
  ENSEMBLE_PAR_TO%NUMBER_OF_FORECASTS_IN_ENSEMBLE_ = ENSEMBLE_PAR_FROM%NUMBER_OF_FORECASTS_IN_ENSEMBLE_

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

END FUNCTION ENSEMBLE_PAR_COPY_FROM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_ENSEMBLE_PAR_FROM_YAML'
PP_THREAD_SAFE FUNCTION READ_ENSEMBLE_PAR_FROM_YAML( ENSEMBLE_PAR, CONFIG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_INTEGER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENSEMBLE_PAR_T),       INTENT(INOUT) :: ENSEMBLE_PAR
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CONFIG
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_KEY
  TYPE(YAML_CONFIGURATION_T)  :: ENSEMBLE_CONFIGURATION

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TEST_CASE_DELETE_ERROR = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_VALUE_FOR_TFIE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_VALUE_FOR_NFIE = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_VALUE_FOR_SN = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_VALUE_FOR_VN = 7_JPIB_K

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

  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CONFIG, 'ensemble', HAS_KEY, HOOKS )

  !> Read representations
  IF ( HAS_KEY  ) THEN

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CONFIG, 'ensemble', ENSEMBLE_CONFIGURATION, HOOKS )


    !> Read the "truncate-degrees"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( ENSEMBLE_CONFIGURATION, 'system-number', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( ENSEMBLE_CONFIGURATION, 'system-number', ENSEMBLE_PAR%SYSTEM_NUMBER_, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( ENSEMBLE_PAR%SYSTEM_NUMBER_.LE.0, ERRFLAG_WRONG_VALUE_FOR_SN)
    ELSE
      ENSEMBLE_PAR%SYSTEM_NUMBER_ = 1_JPIB_K
    ENDIF


    !> Read the "truncate-degrees"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( ENSEMBLE_CONFIGURATION, 'method-number', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( ENSEMBLE_CONFIGURATION, 'method-number', ENSEMBLE_PAR%METHOD_NUMBER_, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( ENSEMBLE_PAR%METHOD_NUMBER_.LE.0, ERRFLAG_WRONG_VALUE_FOR_VN )
    ELSE
      ENSEMBLE_PAR%METHOD_NUMBER_ = 1_JPIB_K
    ENDIF


    !> Read the "truncate-degrees"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( ENSEMBLE_CONFIGURATION, 'type-of-ensemble-forecast', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( ENSEMBLE_CONFIGURATION, 'type-of-ensemble-forecast', ENSEMBLE_PAR%TYPE_OF_ENSEMBLE_FORECAST_, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( ENSEMBLE_PAR%TYPE_OF_ENSEMBLE_FORECAST_.LE.0, ERRFLAG_WRONG_VALUE_FOR_TFIE )
    ELSE
      ENSEMBLE_PAR%TYPE_OF_ENSEMBLE_FORECAST_ = 1_JPIB_K
    ENDIF

    !> Read the "number-of-points-along-a-meridian"
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( ENSEMBLE_CONFIGURATION, 'number-of-forecasts-in-ensemble', HAS_KEY, HOOKS )
    IF ( HAS_KEY ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( ENSEMBLE_CONFIGURATION, 'number-of-forecasts-in-ensemble', ENSEMBLE_PAR%NUMBER_OF_FORECASTS_IN_ENSEMBLE_, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( ENSEMBLE_PAR%NUMBER_OF_FORECASTS_IN_ENSEMBLE_.LE.0, ERRFLAG_WRONG_VALUE_FOR_NFIE )
    ELSE
      ENSEMBLE_PAR%NUMBER_OF_FORECASTS_IN_ENSEMBLE_ = UNDEF_PARAM_E
    ENDIF

    !> Destroy the configuration object
    PP_TRYCALL(ERRFLAG_TEST_CASE_DELETE_ERROR) YAML_DELETE_CONFIGURATION( ENSEMBLE_CONFIGURATION, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
    CASE(ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read sub-configuration' )
    CASE(ERRFLAG_TEST_CASE_DELETE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to delete configuration' )
    CASE(ERRFLAG_WRONG_VALUE_FOR_TFIE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong value for type-of-ensemble-forecast' )
    CASE(ERRFLAG_WRONG_VALUE_FOR_NFIE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong value for number-of-forecasts-in-ensemble' )
    CASE(ERRFLAG_WRONG_VALUE_FOR_SN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong value for system-number' )
    CASE(ERRFLAG_WRONG_VALUE_FOR_VN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong value for version-number' )
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

END FUNCTION READ_ENSEMBLE_PAR_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WRITE_ENSEMBLE_PAR_TO_YAML'
PP_THREAD_SAFE FUNCTION WRITE_ENSEMBLE_PAR_TO_YAML( ENSEMBLE_PAR, UNIT, OFFSET, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  USE :: LOG_UTILS_MOD, ONLY: TO_STRING
  USE :: LOG_UTILS_MOD, ONLY: MAX_STR_LEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENSEMBLE_PAR_T), INTENT(INOUT) :: ENSEMBLE_PAR
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=MAX_STR_LEN) :: CTMP
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  LOGICAL :: IS_OPENED

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_OFFSET = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNIT_NOT_OPENED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_STRING = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_ERROR = 4_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( OFFSET.LT.0, ERRFLAG_INVALID_OFFSET )

  ! Check if it is possible to write on the provided unit
  INQUIRE( UNIT=UNIT, OPENED=IS_OPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_OPENED, ERRFLAG_UNIT_NOT_OPENED )

  ! Write to the unit
  WRITE( UNIT, '(A,A)', IOSTAT=WRITE_STAT ) REPEAT(' ', OFFSET), 'ensemble:'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )



  ! convert integer to string
  CTMP = REPEAT(' ', MAX_STR_LEN)
  IF ( ENSEMBLE_PAR%TYPE_OF_ENSEMBLE_FORECAST_ .EQ. UNDEF_PARAM_E ) THEN
    CTMP='"undefined"'
  ELSE
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_STRING) TO_STRING( ENSEMBLE_PAR%TYPE_OF_ENSEMBLE_FORECAST_, CTMP, HOOKS )
  ENDIF

  ! Write to the unit
  WRITE( UNIT, '(A,A,A,A)', IOSTAT=WRITE_STAT ) REPEAT(' ', OFFSET+2), 'type-of-ensemble-forecast: ', TRIM(ADJUSTL(CTMP)), ' # type of ensemble forecast (default=1)'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )



  ! convert integer to string
  CTMP = REPEAT(' ', MAX_STR_LEN)
  IF ( ENSEMBLE_PAR%NUMBER_OF_FORECASTS_IN_ENSEMBLE_ .EQ. UNDEF_PARAM_E ) THEN
    CTMP='"undefined"'
  ELSE
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_STRING) TO_STRING( ENSEMBLE_PAR%NUMBER_OF_FORECASTS_IN_ENSEMBLE_, CTMP, HOOKS )
  ENDIF

  ! Write to the unit
  WRITE( UNIT, '(A,A,A,A)', IOSTAT=WRITE_STAT ) REPEAT(' ', OFFSET+2), 'number-of-forecasts-in-ensemble: ', TRIM(ADJUSTL(CTMP)), ' # total number of forecasts in ensemble (if not present then it is assumed that the simulation is deterministic)'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )


  ! convert integer to string
  CTMP = REPEAT(' ', MAX_STR_LEN)
  IF ( ENSEMBLE_PAR%SYSTEM_NUMBER_ .EQ. UNDEF_PARAM_E ) THEN
    CTMP='"undefined"'
  ELSE
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_STRING) TO_STRING( ENSEMBLE_PAR%SYSTEM_NUMBER_, CTMP, HOOKS )
  ENDIF

  ! Write to the unit
  WRITE( UNIT, '(A,A,A,A)', IOSTAT=WRITE_STAT ) REPEAT(' ', OFFSET+2), 'system-number: ', TRIM(ADJUSTL(CTMP)), ' # system number to be set in section2'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )


  ! convert integer to string
  CTMP = REPEAT(' ', MAX_STR_LEN)
  IF ( ENSEMBLE_PAR%METHOD_NUMBER_ .EQ. UNDEF_PARAM_E ) THEN
    CTMP='"undefined"'
  ELSE
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_STRING) TO_STRING( ENSEMBLE_PAR%METHOD_NUMBER_, CTMP, HOOKS )
  ENDIF

  ! Write to the unit
  WRITE( UNIT, '(A,A,A,A)', IOSTAT=WRITE_STAT ) REPEAT(' ', OFFSET+2), 'method-number: ', TRIM(ADJUSTL(CTMP)), ' # version number to be set in section2'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )







  ! Add an empty line
  WRITE( UNIT, '(A)', IOSTAT=WRITE_STAT ) REPEAT(' ', OFFSET)
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )

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
    CASE(ERRFLAG_INVALID_OFFSET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid offset' )
    CASE(ERRFLAG_UNIT_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unit not opened' )
    CASE(ERRFLAG_UNABLE_TO_CONVERT_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to string' )
    CASE(ERRFLAG_WRITE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'write error' )
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

END FUNCTION WRITE_ENSEMBLE_PAR_TO_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE ENSEMBLE_PAR_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
