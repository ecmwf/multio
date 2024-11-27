! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib2_section4_time_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SECTION4_STATISTICS_UTILS_MOD'
MODULE GRIB2_SECTION4_STATISTICS_UTILS_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A
  USE :: ENUMERATORS_MOD,       ONLY: TYPE_OF_STATISTICAL_PROCESS_MISSING_E
IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!> Whitelist of public symbols
PUBLIC :: COMPUTE_TIME_SINCE_START
PUBLIC :: SET_END_OF_TIME_INTERVAL
PUBLIC :: CLENGTH_OF_TIMERANGE2ILENGTH_OF_TIMERANGE


CONTAINS



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMPUTE_TIME_SINCE_START'
PP_THREAD_SAFE FUNCTION COMPUTE_TIME_SINCE_START( &
&  MSG, PAR, TIME_HIST, CURR_TIME, OPT, TIME_SINCE_START_IN_SECONDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: TIME_UTILS_MOD,           ONLY: TIME_HISTORY_T
  USE :: TIME_UTILS_MOD,           ONLY: CURR_TIME_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: DATETIME_UTILS_MOD,       ONLY: HOURS2SECONDS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  INTEGER(KIND=JPIB_K),            INTENT(OUT)   :: TIME_SINCE_START_IN_SECONDS
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Compute the time since the start of the simulation
  TIME_SINCE_START_IN_SECONDS = MSG%STEP*PAR%LENGTH_OF_TIME_STEP_IN_SECONDS

  ! Add the contribution of the initial step
  TIME_SINCE_START_IN_SECONDS = TIME_SINCE_START_IN_SECONDS + PAR%INITIAL_STEP * HOURS2SECONDS

  ! Trace end of procedure (on error)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION COMPUTE_TIME_SINCE_START
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SET_END_OF_TIME_INTERVAL'
PP_THREAD_SAFE FUNCTION SET_END_OF_TIME_INTERVAL( &
&  MSG, PAR, TIME_HIST, CURR_TIME, TIME_SINCE_START_IN_SECONDS, OPT, METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: TIME_UTILS_MOD,           ONLY: TIME_HISTORY_T
  USE :: TIME_UTILS_MOD,           ONLY: CURR_TIME_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: DATETIME_UTILS_MOD,       ONLY: YYYYMMDD_HHMMSS_TO_DATETIME
  USE :: DATETIME_UTILS_MOD,       ONLY: ADD_SECONDS_TO_DATETIME

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  INTEGER(KIND=JPIB_K),            INTENT(IN)    :: TIME_SINCE_START_IN_SECONDS
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: IN_DATETIME
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: OUT_DATETIME

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_DATETIME=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ADD_SECONDS_TO_DATETIME=3_JPIB_K


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA, ERRFLAG_METADATA )

  ! Unpack date time
  PP_TRYCALL(ERRFLAG_UNABLE_TO_EXTRACT_DATETIME) YYYYMMDD_HHMMSS_TO_DATETIME( MSG%DATE, MSG%TIME, IN_DATETIME, HOOKS )

  ! Add seconds related to the current time to date/time
  IF ( TIME_SINCE_START_IN_SECONDS .GT. 0 ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_ADD_SECONDS_TO_DATETIME) ADD_SECONDS_TO_DATETIME( IN_DATETIME, TIME_SINCE_START_IN_SECONDS, OUT_DATETIME, HOOKS )
  ELSE
    OUT_DATETIME = IN_DATETIME
  ENDIF

  !> Set the metadata to the grib message
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'yearOfEndOfOverallTimeInterval',   OUT_DATETIME(1))
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'monthOfEndOfOverallTimeInterval',  OUT_DATETIME(2))
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'dayOfEndOfOverallTimeInterval',    OUT_DATETIME(3))
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'hourOfEndOfOverallTimeInterval',   OUT_DATETIME(4))
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'minuteOfEndOfOverallTimeInterval', OUT_DATETIME(5))
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'secondOfEndOfOverallTimeInterval', OUT_DATETIME(6))

  ! Trace end of procedure (on error)
  PP_METADATA_EXIT_PROCEDURE( METADATA, ERRFLAG_METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to set metadata' )
    CASE (ERRFLAG_UNABLE_TO_EXTRACT_DATETIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert date time' )
    CASE (ERRFLAG_UNABLE_TO_ADD_SECONDS_TO_DATETIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to add seconds to date time' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION SET_END_OF_TIME_INTERVAL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Converts a string representation of a time range into an integer representing length in seconds.
!>
!> This function takes a string input that specifies a length of time in a defined format (e.g., "5h" for 5 hours),
!> converts it into an integer representing the total length in seconds, and outputs this value through an output parameter.
!>
!> The function also processes input for a variety of time units:
!> - Seconds (s or S)
!> - Minutes (m or M)
!> - Hours (h or H)
!> - Days (d or D)
!>
!> @param [in]    CLENGTH_OF_TIMERANGE The string representing the length of time in the specified format.
!> @param [out]   ILENGTH_OF_TIMERANGE The integer output that will contain the length of time in seconds.
!> @param [inout] HOOKS                A structure for hooks that may be used for debugging, logging, or other functionalities.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CLENGTH_OF_TIMERANGE2ILENGTH_OF_TIMERANGE'
PP_THREAD_SAFE FUNCTION CLENGTH_OF_TIMERANGE2ILENGTH_OF_TIMERANGE( CLENGTH_OF_TIMERANGE, ILENGTH_OF_TIMERANGE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: CLENGTH_OF_TIMERANGE
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: ILENGTH_OF_TIMERANGE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: SCALE_FACTOR
  INTEGER(KIND=JPIB_K) :: TMP
  INTEGER(KIND=JPIB_K) :: READ_STATUS
  CHARACTER(LEN=LEN(CLENGTH_OF_TIMERANGE)) :: LOC_CLENGTH_OF_TIMERANGE
  CHARACTER(LEN=LEN(CLENGTH_OF_TIMERANGE)) :: CLENGTH_OF_TIMERANGE_DIGITS
  CHARACTER(LEN=1) :: CLENGTH_OF_TIMERANGE_SCALE_FACTOR

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_LENGTH_OF_TIMERANGE=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_SCALE_FACTOR_FOR_TIMERANGE=2_JPIB_K

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

  !> Convert prefix to lowercase
  LOC_CLENGTH_OF_TIMERANGE = REPEAT(' ', LEN( CLENGTH_OF_TIMERANGE ) )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( TRIM(ADJUSTL(CLENGTH_OF_TIMERANGE)), LOC_CLENGTH_OF_TIMERANGE, HOOKS )
  N =  LEN_TRIM(LOC_CLENGTH_OF_TIMERANGE)

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( N.LT.2, ERRFLAG_INVALID_LENGTH_OF_TIMERANGE )

  !> Split the string into digits and scale factor
  CLENGTH_OF_TIMERANGE_DIGITS = REPEAT(' ', LEN( CLENGTH_OF_TIMERANGE_DIGITS ) )
  CLENGTH_OF_TIMERANGE_DIGITS = LOC_CLENGTH_OF_TIMERANGE(1:N-1)
  CLENGTH_OF_TIMERANGE_SCALE_FACTOR = LOC_CLENGTH_OF_TIMERANGE(N:N)

  !> Assign the scale factor
  SELECT CASE( CLENGTH_OF_TIMERANGE_SCALE_FACTOR )
  CASE ( 's')
    SCALE_FACTOR = 1_JPIB_K
  CASE ( 'm')
    SCALE_FACTOR = 60_JPIB_K
  CASE ( 'h')
    SCALE_FACTOR = 3600_JPIB_K
  CASE ( 'd')
    SCALE_FACTOR = 86400_JPIB_K
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_SCALE_FACTOR_FOR_TIMERANGE )
  END SELECT

  !> Convert the digits to integer
  READ( CLENGTH_OF_TIMERANGE_DIGITS, *, IOSTAT=READ_STATUS) TMP
  PP_DEBUG_CRITICAL_COND_THROW( READ_STATUS .NE. 0, ERRFLAG_INVALID_LENGTH_OF_TIMERANGE )

  !> Calculate the length of the timerange
  ILENGTH_OF_TIMERANGE = TMP * SCALE_FACTOR

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ILENGTH_OF_TIMERANGE .LT. 1, ERRFLAG_INVALID_LENGTH_OF_TIMERANGE )
  PP_DEBUG_CRITICAL_COND_THROW( MOD(ILENGTH_OF_TIMERANGE,3600_JPIB_K).NE.0, ERRFLAG_INVALID_LENGTH_OF_TIMERANGE )

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
    CASE ( ERRFLAG_INVALID_LENGTH_OF_TIMERANGE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid length of timerange' )
    CASE ( ERRFLAG_UNABLE_TO_CONVERT_LC )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
    CASE ( ERRFLAG_INVALID_SCALE_FACTOR_FOR_TIMERANGE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid scale factor for timerange' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION CLENGTH_OF_TIMERANGE2ILENGTH_OF_TIMERANGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB2_SECTION4_STATISTICS_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
