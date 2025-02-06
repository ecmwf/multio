! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'parametrization_enumerators_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'PARAMETRIZATION_ENUMERATORS_MOD'
MODULE PARAMETRIZATION_ENUMERATORS_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

  !> Integer enumerators (general)
  INTEGER(KIND=JPIB_K), PARAMETER :: PARINTFLD_TABLES_VERSION_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PARINTFLD_GENERATING_PROCESS_IDENTIFIER_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PARINTFLD_SINK_TYPE_E=3_JPIB_K

  !> Integer enumerators (time)
  INTEGER(KIND=JPIB_K), PARAMETER :: PARINTFLD_TIME_INITIAL_STEP_E=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PARINTFLD_TIME_LENGTH_OF_TIME_STEP_IN_SECONDS_E=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PARINTFLD_TIME_LENGTH_OF_TIME_RANGE_IN_SECONDS_E=6_JPIB_K

  !> Integer enumerators (bitmap)
  INTEGER(KIND=JPIB_K), PARAMETER :: PARINTFLD_BITMAP_NUMBER_OF_MISSING_VALUES_E=7_JPIB_K

  !> Integer enumerators (ensemble)
  INTEGER(KIND=JPIB_K), PARAMETER :: PARINTFLD_ENSEMBLE_TYPE_OF_ENSEMBLE_FORECAST_E=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PARINTFLD_ENSEMBLE_NUMBER_OF_FORECASTS_IN_ENSEMBLE_E=9_JPIB_K

  !> Integer enumerators (analysis)
  INTEGER(KIND=JPIB_K), PARAMETER :: PARINTFLD_ANALYSIS_LENGTH_OF_TIME_WINDOW_E=10_JPIB_K

  !> Integer enumerators (satellite)
  INTEGER(KIND=JPIB_K), PARAMETER :: PARINTFLD_SATELLITE_SATELLITE_SERIES_E=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PARINTFLD_SATELLITE_SCALED_FACTOR_OF_CENTRAL_VAWENUMBER_E=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: PARINTFLD_SATELLITE_SCALED_VALUE_OF_CENTRAL_VAWENUMBER_E=13_JPIB_K

  !> Integer enumerators (data-representation)
  INTEGER(KIND=JPIB_K), PARAMETER :: PARINTFLD_DATA_REPRESENTATION_BITS_PER_VALUE_E=14_JPIB_K

  !> Total number of integer enumerators
  INTEGER(KIND=JPIB_K), PARAMETER :: N_PARINTFLDS=14_JPIB_K


  ! String enumerators
  INTEGER(KIND=JPIB_K), PARAMETER :: N_PARSTRFLDS=0_JPIB_K


  ! Float enumerators
  INTEGER(KIND=JPIB_K), PARAMETER :: PARFLOATFLD_SCALEFACTOR_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_PARFLOATFLDS=1_JPIB_K


  !> White list of public symbols (enumerators)
  PUBLIC :: PARINTFLD_TABLES_VERSION_E
  PUBLIC :: PARINTFLD_GENERATING_PROCESS_IDENTIFIER_E
  PUBLIC :: PARINTFLD_SINK_TYPE_E
  PUBLIC :: PARINTFLD_TIME_INITIAL_STEP_E
  PUBLIC :: PARINTFLD_TIME_LENGTH_OF_TIME_STEP_IN_SECONDS_E
  PUBLIC :: PARINTFLD_TIME_LENGTH_OF_TIME_RANGE_IN_SECONDS_E
  PUBLIC :: PARINTFLD_BITMAP_NUMBER_OF_MISSING_VALUES_E
  PUBLIC :: PARINTFLD_ENSEMBLE_TYPE_OF_ENSEMBLE_FORECAST_E
  PUBLIC :: PARINTFLD_ENSEMBLE_NUMBER_OF_FORECASTS_IN_ENSEMBLE_E
  PUBLIC :: PARINTFLD_ANALYSIS_LENGTH_OF_TIME_WINDOW_E
  PUBLIC :: PARINTFLD_SATELLITE_SATELLITE_SERIES_E
  PUBLIC :: PARINTFLD_SATELLITE_SCALED_FACTOR_OF_CENTRAL_VAWENUMBER_E
  PUBLIC :: PARINTFLD_SATELLITE_SCALED_VALUE_OF_CENTRAL_VAWENUMBER_E
  PUBLIC :: PARINTFLD_DATA_REPRESENTATION_BITS_PER_VALUE_E
  PUBLIC :: N_PARINTFLDS

  PUBLIC :: N_PARSTRFLDS

  PUBLIC :: PARFLOATFLD_SCALEFACTOR_E
  PUBLIC :: N_PARFLOATFLDS

  !> Whitelist of public symbols (procedures)
  PUBLIC :: IPARINTFLDS2CPARINTFLDS
  PUBLIC :: CPARINTFLDS2IPARINTFLDS

  PUBLIC :: IPARFLOATFLDS2CPARFLOATFLDS
  PUBLIC :: CPARFLOATFLDS2IPARFLOATFLDS

  PUBLIC :: IPARSTRINGFLDS2CPARSTRINGFLDS
  PUBLIC :: CPARSTRINGFLDS2IPARSTRINGFLDS

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IPARINTFLDS2CPARINTFLDS'
PP_THREAD_SAFE FUNCTION IPARINTFLDS2CPARINTFLDS( IPARINTFLDS, CPARINTFLDS, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IPARINTFLDS
  CHARACTER(LEN=64),    INTENT(OUT)   :: CPARINTFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PARINTFLD_UNARY=1_JPIB_K

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

  !> Initialization of the output variable
  CPARINTFLDS = REPEAT(' ', 64)

  !> Select the prefix
  SELECT CASE ( IPARINTFLDS )
  CASE (PARINTFLD_TABLES_VERSION_E)
    CPARINTFLDS = 'tables-version'
  CASE (PARINTFLD_GENERATING_PROCESS_IDENTIFIER_E)
    CPARINTFLDS = 'generating-process-identifier'
  CASE (PARINTFLD_SINK_TYPE_E)
    CPARINTFLDS = 'direct-to-fdb'
  CASE (PARINTFLD_TIME_INITIAL_STEP_E)
    CPARINTFLDS = 'initial-step'
  CASE (PARINTFLD_TIME_LENGTH_OF_TIME_STEP_IN_SECONDS_E)
    CPARINTFLDS = 'length-of-time-step-in-seconds'
  CASE (PARINTFLD_TIME_LENGTH_OF_TIME_RANGE_IN_SECONDS_E)
    CPARINTFLDS = 'length-of-time-range-in-seconds'
  CASE (PARINTFLD_BITMAP_NUMBER_OF_MISSING_VALUES_E)
    CPARINTFLDS = 'number-of-missing-values'
  CASE (PARINTFLD_ENSEMBLE_TYPE_OF_ENSEMBLE_FORECAST_E)
    CPARINTFLDS = 'type-of-ensemble-forecast'
  CASE (PARINTFLD_ENSEMBLE_NUMBER_OF_FORECASTS_IN_ENSEMBLE_E)
    CPARINTFLDS = 'number-of-forecasts-in-ensemble'
  CASE (PARINTFLD_ANALYSIS_LENGTH_OF_TIME_WINDOW_E)
    CPARINTFLDS = 'length-of-time-window'
  CASE (PARINTFLD_SATELLITE_SATELLITE_SERIES_E)
    CPARINTFLDS = 'satellite-series'
  CASE (PARINTFLD_SATELLITE_SCALED_FACTOR_OF_CENTRAL_VAWENUMBER_E)
    CPARINTFLDS = 'scaled-factor-of-central-wavenumber'
  CASE (PARINTFLD_SATELLITE_SCALED_VALUE_OF_CENTRAL_VAWENUMBER_E)
    CPARINTFLDS = 'scaled-value-of-central-wavenumber'
  CASE (PARINTFLD_DATA_REPRESENTATION_BITS_PER_VALUE_E)
    CPARINTFLDS = 'bits-per-value'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PARINTFLD_UNARY )
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
    CASE (ERRFLAG_UNKNOWN_PARINTFLD_UNARY)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IPARINTFLDS
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown iintop_unary: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IPARINTFLDS2CPARINTFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CPARINTFLDS2IPARINTFLDS'
PP_THREAD_SAFE FUNCTION CPARINTFLDS2IPARINTFLDS( CPARINTFLDS, IPARINTFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: CPARINTFLDS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IPARINTFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CPARINTFLDS)) :: LOC_CPARINTFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PARINTFLD_UNARY=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=2_JPIB_K

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

  !> Initialization of the output variable
  IPARINTFLDS = UNDEF_PARAM_E

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CPARINTFLDS, LOC_CPARINTFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CPARINTFLDS)) )
  CASE ( 'tables-version' )
    IPARINTFLDS = PARINTFLD_TABLES_VERSION_E
  CASE ( 'initial-step' )
    IPARINTFLDS = PARINTFLD_TIME_INITIAL_STEP_E
  CASE ( 'generating-precess-identifier' )
    IPARINTFLDS = PARINTFLD_GENERATING_PROCESS_IDENTIFIER_E
  CASE ( 'direct-to-fdb' )
    IPARINTFLDS = PARINTFLD_SINK_TYPE_E
  CASE ( 'length-of-time-step-in-seconds' )
    IPARINTFLDS = PARINTFLD_TIME_LENGTH_OF_TIME_STEP_IN_SECONDS_E
  CASE ( 'length-of-time-range-in-seconds' )
    IPARINTFLDS = PARINTFLD_TIME_LENGTH_OF_TIME_RANGE_IN_SECONDS_E
  CASE ( 'number-of-missing-values' )
    IPARINTFLDS = PARINTFLD_BITMAP_NUMBER_OF_MISSING_VALUES_E
  CASE ( 'type-of-ensemble-forecast' )
    IPARINTFLDS = PARINTFLD_ENSEMBLE_TYPE_OF_ENSEMBLE_FORECAST_E
  CASE ( 'number-of-forecasts-in-ensemble' )
    IPARINTFLDS = PARINTFLD_ENSEMBLE_NUMBER_OF_FORECASTS_IN_ENSEMBLE_E
  CASE ( 'length-of-time-window' )
    IPARINTFLDS = PARINTFLD_ANALYSIS_LENGTH_OF_TIME_WINDOW_E
  CASE ( 'satellite-series' )
    IPARINTFLDS = PARINTFLD_SATELLITE_SATELLITE_SERIES_E
  CASE ( 'scaled-factor-of-central-wavenumber' )
    IPARINTFLDS = PARINTFLD_SATELLITE_SCALED_FACTOR_OF_CENTRAL_VAWENUMBER_E
  CASE ( 'scaled-value-of-central-wavenumber' )
    IPARINTFLDS = PARINTFLD_SATELLITE_SCALED_VALUE_OF_CENTRAL_VAWENUMBER_E
  CASE ( 'bits-per-value' )
    IPARINTFLDS = PARINTFLD_DATA_REPRESENTATION_BITS_PER_VALUE_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PARINTFLD_UNARY )
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

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
    CASE (ERRFLAG_UNKNOWN_PARINTFLD_UNARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cintop_unary: '//TRIM(ADJUSTL(CPARINTFLDS)) )
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

END FUNCTION CPARINTFLDS2IPARINTFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CPARFLOATFLDS2IPARFLOATFLDS'
PP_THREAD_SAFE FUNCTION CPARFLOATFLDS2IPARFLOATFLDS( CPARFLOATFLDS, IPARFLOATFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: CPARFLOATFLDS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IPARFLOATFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CPARFLOATFLDS)) :: LOC_CPARFLOATFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PARFLOATFLD_UNARY=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=2_JPIB_K

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

  !> Initialization of the output variable
  IPARFLOATFLDS = UNDEF_PARAM_E

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CPARFLOATFLDS, LOC_CPARFLOATFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CPARFLOATFLDS)) )
  CASE ( 'scale-factor' )
    IPARFLOATFLDS = PARFLOATFLD_SCALEFACTOR_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PARFLOATFLD_UNARY )
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

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
    CASE (ERRFLAG_UNKNOWN_PARFLOATFLD_UNARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cfloatop_unary: '//TRIM(ADJUSTL(CPARFLOATFLDS)) )
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

END FUNCTION CPARFLOATFLDS2IPARFLOATFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IPARFLOATFLDS2CPARFLOATFLDS'
PP_THREAD_SAFE FUNCTION IPARFLOATFLDS2CPARFLOATFLDS( IPARFLOATFLDS, CPARFLOATFLDS, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IPARFLOATFLDS
  CHARACTER(LEN=64),    INTENT(OUT)   :: CPARFLOATFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PARFLOATFLD_UNARY=1_JPIB_K

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

  !> Initialization of the output variable
  CPARFLOATFLDS = REPEAT(' ', 64)

  !> Select the prefix
  SELECT CASE ( IPARFLOATFLDS )
  CASE (PARFLOATFLD_SCALEFACTOR_E)
    CPARFLOATFLDS = 'scale-factor'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PARFLOATFLD_UNARY )
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
    CASE (ERRFLAG_UNKNOWN_PARFLOATFLD_UNARY)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IPARFLOATFLDS
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown iintop_unary: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IPARFLOATFLDS2CPARFLOATFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CPARSTRINGFLDS2IPARSTRINGFLDS'
PP_THREAD_SAFE FUNCTION CPARSTRINGFLDS2IPARSTRINGFLDS( CPARSTRINGFLDS, IPARSTRINGFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: CPARSTRINGFLDS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IPARSTRINGFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CPARSTRINGFLDS)) :: LOC_CPARSTRINGFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PARSTRINGFLD_UNARY=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=2_JPIB_K

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

  !> Initialization of the output variable
  IPARSTRINGFLDS = UNDEF_PARAM_E

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CPARSTRINGFLDS, LOC_CPARSTRINGFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CPARSTRINGFLDS)) )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PARSTRINGFLD_UNARY )
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

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
    CASE (ERRFLAG_UNKNOWN_PARSTRINGFLD_UNARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cstringop_unary: '//TRIM(ADJUSTL(CPARSTRINGFLDS)) )
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

END FUNCTION CPARSTRINGFLDS2IPARSTRINGFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IPARSTRINGFLDS2CPARSTRINGFLDS'
PP_THREAD_SAFE FUNCTION IPARSTRINGFLDS2CPARSTRINGFLDS( IPARSTRINGFLDS, CPARSTRINGFLDS, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IPARSTRINGFLDS
  CHARACTER(LEN=16),    INTENT(OUT)   :: CPARSTRINGFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_PARSTRINGFLD_UNARY=1_JPIB_K

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

  !> Initialization of the output variable
  CPARSTRINGFLDS = REPEAT(' ', 16)

  !> Select the prefix
  SELECT CASE ( IPARSTRINGFLDS )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_PARSTRINGFLD_UNARY )
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
    CASE (ERRFLAG_UNKNOWN_PARSTRINGFLD_UNARY)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IPARSTRINGFLDS
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown iintop_unary: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IPARSTRINGFLDS2CPARSTRINGFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE PARAMETRIZATION_ENUMERATORS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
