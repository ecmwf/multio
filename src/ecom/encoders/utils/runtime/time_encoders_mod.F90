! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'time_encoders_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'TIME_ENCODERS_MOD'
MODULE TIME_ENCODERS_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

IMPLICIT NONE

! Default visibility
PRIVATE

TYPE :: ENCODERS_OPTIONS_T
  INTEGER(KIND=JPIB_K) :: TEST_
END TYPE


INTERFACE
SUBROUTINE ENCODE_TIME_ATM_IF( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )
  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A
  IMPORT :: ENCODERS_OPTIONS_T
IMPLICIT NONE
  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS
END SUBROUTINE ENCODE_TIME_ATM_IF
SUBROUTINE ENCODE_TIME_WAM_IF( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )
  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_WAM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A
  IMPORT :: ENCODERS_OPTIONS_T
IMPLICIT NONE
  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_WAM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS
END SUBROUTINE ENCODE_TIME_WAM_IF
END INTERFACE


! Whitelist of public symbols
PUBLIC :: ENCODERS_OPTIONS_T
PUBLIC :: ENCODE_TIME_WAM_IF
PUBLIC :: ENCODE_TIME_ATM_IF
PUBLIC :: ERROR_WAM
PUBLIC :: ERROR_ATM
PUBLIC :: NOT_IMPLEMENTED_WAM
PUBLIC :: NOT_IMPLEMENTED_ATM
PUBLIC :: INSTANT_TIME_ENCODER_GRIB1_ATM
PUBLIC :: INSTANT_TIME_ENCODER_GRIB2_ATM
PUBLIC :: INSTANT_TIME_ENCODER_GRIB1_WAM
PUBLIC :: INSTANT_TIME_ENCODER_GRIB2_WAM
PUBLIC :: AVERAGE_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM
PUBLIC :: AVERAGE_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM
PUBLIC :: AVERAGE_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM
PUBLIC :: AVERAGE_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM
PUBLIC :: ACCUMULATION_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM
PUBLIC :: ACCUMULATION_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM
PUBLIC :: ACCUMULATION_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM
PUBLIC :: ACCUMULATION_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM
PUBLIC :: ACCUMULATION_FROM_STEP0_TIME_ENCODER_GRIB1_ATM
PUBLIC :: ACCUMULATION_FROM_STEP0_TIME_ENCODER_GRIB2_ATM
PUBLIC :: MIN_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM
PUBLIC :: MIN_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM
PUBLIC :: MIN_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM
PUBLIC :: MIN_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM
PUBLIC :: MIN_FROM_STEP0_TIME_ENCODER_GRIB1_ATM
PUBLIC :: MIN_FROM_STEP0_TIME_ENCODER_GRIB2_ATM
PUBLIC :: MAX_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM
PUBLIC :: MAX_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM
PUBLIC :: MAX_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM
PUBLIC :: MAX_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM
PUBLIC :: MAX_FROM_STEP0_TIME_ENCODER_GRIB1_ATM
PUBLIC :: MAX_FROM_STEP0_TIME_ENCODER_GRIB2_ATM

CONTAINS

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ERROR_WAM'
SUBROUTINE ERROR_WAM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_WAM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_WAM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Error handling variables
  CHARACTER(LEN=:), ALLOCATABLE :: STR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Generate the error message
  PP_DEBUG_CREATE_ERROR_MSG( STR, 'WAM encoder error' )

  ! Trace end of procedure (on error)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

  ! Write the error message and stop the program
  PP_DEBUG_ABORT( STR )

  ! Exit point
  RETURN

END SUBROUTINE ERROR_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ERROR_ATM'
SUBROUTINE ERROR_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Error handling variables
  CHARACTER(LEN=:), ALLOCATABLE :: STR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Generate the error message
  PP_DEBUG_CREATE_ERROR_MSG( STR, 'ATM encoder error' )

  ! Trace end of procedure (on error)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

  ! Write the error message and stop the program
  PP_DEBUG_ABORT( STR )

  ! Exit point
  RETURN

END SUBROUTINE ERROR_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'NOT_IMPLEMENTED_WAM'
SUBROUTINE NOT_IMPLEMENTED_WAM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_WAM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_WAM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Error handling variables
  CHARACTER(LEN=:), ALLOCATABLE :: STR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Generate the error message
  PP_DEBUG_CREATE_ERROR_MSG( STR, 'WAM encoder not implemented' )

  ! Trace end of procedure (on error)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

  ! Write the error message and stop the program
  PP_DEBUG_ABORT( STR )

  ! Exit point
  RETURN

END SUBROUTINE NOT_IMPLEMENTED_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'NOT_IMPLEMENTED_ATM'
SUBROUTINE NOT_IMPLEMENTED_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Error handling variables
  CHARACTER(LEN=:), ALLOCATABLE :: STR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Generate the error message
  PP_DEBUG_CREATE_ERROR_MSG( STR, 'ATM encoder not implemented' )

  ! Trace end of procedure (on error)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

  ! Write the error message and stop the program
  PP_DEBUG_ABORT( STR )

  ! Exit point
  RETURN

END SUBROUTINE NOT_IMPLEMENTED_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE






#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'INSTANT_TIME_ENCODER_GRIB1_ATM'
SUBROUTINE INSTANT_TIME_ENCODER_GRIB1_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )

  PP_METADATA_SET( METADATA,  'stepType', 'instant' )

  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC/MODEL_PARAMS%SIM_%TSTEP*3600 )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF


  ! WRITE(*,*) 'PLGG time encoder', CURR_TIME%ISEC, MODEL_PARAMS%SIM_%TSTEP

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE INSTANT_TIME_ENCODER_GRIB1_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'INSTANT_TIME_ENCODER_GRIB2_ATM'
SUBROUTINE INSTANT_TIME_ENCODER_GRIB2_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Here probably some logic is needed to compute this depending if it is an analysis or a forecast
  PP_METADATA_SET( METADATA,  'timeRangeIndicator', 0 )

  PP_METADATA_SET( METADATA,  'stepType', 'instant' )

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )

  ! WRITE(*,*) 'Time encoder logging: ', CURR_TIME%ISEC, MODEL_PARAMS%SIM_%TSTEP
  ! PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )
  PP_METADATA_SET( METADATA,  'forecastTime',  CURR_TIME%ISEC/MODEL_PARAMS%SIM_%TSTEP )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
    PP_METADATA_SET( METADATA,  'indicatorOfUnitOfTimeRange', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE INSTANT_TIME_ENCODER_GRIB2_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'INSTANT_TIME_ENCODER_GRIB1_WAM'
SUBROUTINE INSTANT_TIME_ENCODER_GRIB1_WAM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_WAM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_WAM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )


  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE INSTANT_TIME_ENCODER_GRIB1_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'INSTANT_TIME_ENCODER_GRIB2_WAM'
SUBROUTINE INSTANT_TIME_ENCODER_GRIB2_WAM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_WAM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_WAM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )


  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE INSTANT_TIME_ENCODER_GRIB2_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'AVERAGE_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM'
SUBROUTINE AVERAGE_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LAST_PP_SEC

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  !! PP_DEBUG_DEVELOP_COND_THROW( MSG%IPREVPP_ .LT. 0, 1 )

  ! Compute the last post processing step in seconds
  LAST_PP_SEC = TIME_HIST%HIST_(TIME_HIST%SIZE_) * MODEL_PARAMS%SIM_%TSTEP

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepType',  'average' )
  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'startStep', MSG%IPREVPP_*MODEL_PARAMS%SIM_%TSTEP + CURR_TIME%ISEC0 )
  PP_METADATA_SET( METADATA,  'endStep',   CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE AVERAGE_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'AVERAGE_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM'
SUBROUTINE AVERAGE_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LAST_PP_SEC

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  !! PP_DEBUG_DEVELOP_COND_THROW( MSG%IPREVPP_ .LT. 0, 1 )

  ! Compute the last post processing step in seconds
  LAST_PP_SEC = TIME_HIST%HIST_(TIME_HIST%SIZE_) * MODEL_PARAMS%SIM_%TSTEP

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepType',  'average' )
  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'startStep', MSG%IPREVPP_*MODEL_PARAMS%SIM_%TSTEP + CURR_TIME%ISEC0 )
  PP_METADATA_SET( METADATA,  'endStep',   CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE AVERAGE_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'AVERAGE_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM'
SUBROUTINE AVERAGE_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'average' )
  PP_METADATA_SET( METADATA,  'startStep',  MAX(0,CURR_TIME%ISEC-GRIB_INFO%OVERALL_LENGTH_OF_TIME_RANGE_*3600) )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE AVERAGE_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'AVERAGE_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM'
SUBROUTINE AVERAGE_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'accum' )
  PP_METADATA_SET( METADATA,  'startStep',  MAX(0,CURR_TIME%ISEC-GRIB_INFO%OVERALL_LENGTH_OF_TIME_RANGE_*3600) )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE AVERAGE_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ACCUMULATION_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM'
SUBROUTINE ACCUMULATION_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LAST_PP_SEC

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  !! PP_DEBUG_DEVELOP_COND_THROW( MSG%IPREVPP_ .LT. 0, 1 )

  ! Compute the last post processing step in seconds
  LAST_PP_SEC = TIME_HIST%HIST_(TIME_HIST%SIZE_) * MODEL_PARAMS%SIM_%TSTEP

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepType',  'accum' )
  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'startStep', MSG%IPREVPP_*MODEL_PARAMS%SIM_%TSTEP + CURR_TIME%ISEC0 )
  PP_METADATA_SET( METADATA,  'endStep',   CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF


  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE ACCUMULATION_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ACCUMULATION_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM'
SUBROUTINE ACCUMULATION_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LAST_PP_SEC

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  !! PP_DEBUG_DEVELOP_COND_THROW( MSG%IPREVPP_ .LT. 0, 1 )

  ! Compute the last post processing step in seconds
  LAST_PP_SEC = TIME_HIST%HIST_(TIME_HIST%SIZE_) * MODEL_PARAMS%SIM_%TSTEP

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepType',  'accum' )
  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'startStep', MSG%IPREVPP_*MODEL_PARAMS%SIM_%TSTEP + CURR_TIME%ISEC0 )
  PP_METADATA_SET( METADATA,  'endStep',   CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF



  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE ACCUMULATION_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ACCUMULATION_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM'
SUBROUTINE ACCUMULATION_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'accum' )
  PP_METADATA_SET( METADATA,  'startStep',  MAX(0,CURR_TIME%ISEC-GRIB_INFO%OVERALL_LENGTH_OF_TIME_RANGE_*3600) )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE ACCUMULATION_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ACCUMULATION_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM'
SUBROUTINE ACCUMULATION_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'accum' )
  PP_METADATA_SET( METADATA,  'startStep',  MAX(0,CURR_TIME%ISEC-GRIB_INFO%OVERALL_LENGTH_OF_TIME_RANGE_*3600) )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE ACCUMULATION_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ACCUMULATION_FROM_STEP0_TIME_ENCODER_GRIB1_ATM'
SUBROUTINE ACCUMULATION_FROM_STEP0_TIME_ENCODER_GRIB1_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'accum' )
  PP_METADATA_SET( METADATA,  'startStep',  0 )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC/MODEL_PARAMS%SIM_%TSTEP*3600 )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE ACCUMULATION_FROM_STEP0_TIME_ENCODER_GRIB1_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ACCUMULATION_FROM_STEP0_TIME_ENCODER_GRIB2_ATM'
SUBROUTINE ACCUMULATION_FROM_STEP0_TIME_ENCODER_GRIB2_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)
  PP_METADATA_SET( METADATA, 'productDefinitionTemplateNumber', GRIB_INFO%PRODUCT_DEFINITION_TEMPLATE_NUMBER_ )
  ! WRITE(*,*) 'ACCUMULATION FROM STEP 0'
  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'accum' )
  PP_METADATA_SET( METADATA,  'startStep',  0 )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE ACCUMULATION_FROM_STEP0_TIME_ENCODER_GRIB2_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MIN_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM'
SUBROUTINE MIN_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LAST_PP_SEC

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Compute the last post processing step in seconds
  LAST_PP_SEC = TIME_HIST%HIST_(TIME_HIST%SIZE_) * MODEL_PARAMS%SIM_%TSTEP

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'min' )
  PP_METADATA_SET( METADATA,  'startStep', LAST_PP_SEC )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MIN_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MIN_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM'
SUBROUTINE MIN_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LAST_PP_SEC

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Compute the last post processing step in seconds
  LAST_PP_SEC = TIME_HIST%HIST_(TIME_HIST%SIZE_) * MODEL_PARAMS%SIM_%TSTEP

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'min' )
  PP_METADATA_SET( METADATA,  'startStep', LAST_PP_SEC )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MIN_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MIN_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM'
SUBROUTINE MIN_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )


  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'min' )
  PP_METADATA_SET( METADATA,  'startStep', MAX(0,CURR_TIME%ISEC-GRIB_INFO%OVERALL_LENGTH_OF_TIME_RANGE_*3600) )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MIN_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MIN_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM'
SUBROUTINE MIN_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'min' )
  PP_METADATA_SET( METADATA,  'startStep', MAX(0,CURR_TIME%ISEC-GRIB_INFO%OVERALL_LENGTH_OF_TIME_RANGE_*3600) )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MIN_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MIN_FROM_STEP0_TIME_ENCODER_GRIB1_ATM'
SUBROUTINE MIN_FROM_STEP0_TIME_ENCODER_GRIB1_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'min' )
  PP_METADATA_SET( METADATA,  'startStep', 0 )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF


  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MIN_FROM_STEP0_TIME_ENCODER_GRIB1_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MIN_FROM_STEP0_TIME_ENCODER_GRIB2_ATM'
SUBROUTINE MIN_FROM_STEP0_TIME_ENCODER_GRIB2_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'min' )
  PP_METADATA_SET( METADATA,  'startStep', 0 )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MIN_FROM_STEP0_TIME_ENCODER_GRIB2_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAX_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM'
SUBROUTINE MAX_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LAST_PP_SEC

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Compute the last post processing step in seconds
  IF ( TIME_HIST%SIZE_ .GT. 1 ) THEN
    LAST_PP_SEC = TIME_HIST%HIST_(TIME_HIST%SIZE_-1) * MODEL_PARAMS%SIM_%TSTEP
  ELSE
    LAST_PP_SEC = 0
  ENDIF

  ! WRITE(*,*) ' + START STEP :: ', LAST_PP_SEC
  ! WRITE(*,*) ' + END STEP   :: ', CURR_TIME%ISEC

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'max' )
  PP_METADATA_SET( METADATA,  'startStep', LAST_PP_SEC/MODEL_PARAMS%SIM_%TSTEP*3600 )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC/MODEL_PARAMS%SIM_%TSTEP*3600 )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MAX_FROM_LASTPP_TIME_ENCODER_GRIB1_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAX_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM'
SUBROUTINE MAX_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LAST_PP_SEC

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Compute the last post processing step in seconds
  LAST_PP_SEC = TIME_HIST%HIST_(TIME_HIST%SIZE_) * MODEL_PARAMS%SIM_%TSTEP

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'max' )
  PP_METADATA_SET( METADATA,  'startStep', LAST_PP_SEC )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MAX_FROM_LASTPP_TIME_ENCODER_GRIB2_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAX_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM'
SUBROUTINE MAX_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'max' )
  PP_METADATA_SET( METADATA,  'startStep', MAX(0,CURR_TIME%ISEC-GRIB_INFO%OVERALL_LENGTH_OF_TIME_RANGE_*3600) )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF


  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MAX_FIXED_RANGE_TIME_ENCODER_GRIB1_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAX_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM'
SUBROUTINE MAX_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'max' )
  PP_METADATA_SET( METADATA,  'startStep', MAX(0,CURR_TIME%ISEC-GRIB_INFO%OVERALL_LENGTH_OF_TIME_RANGE_*3600) )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF


  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MAX_FIXED_RANGE_TIME_ENCODER_GRIB2_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAX_FROM_STEP0_TIME_ENCODER_GRIB1_ATM'
SUBROUTINE MAX_FROM_STEP0_TIME_ENCODER_GRIB1_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'max' )
  PP_METADATA_SET( METADATA,  'startStep', 0 )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF


  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MAX_FROM_STEP0_TIME_ENCODER_GRIB1_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAX_FROM_STEP0_TIME_ENCODER_GRIB2_ATM'
SUBROUTINE MAX_FROM_STEP0_TIME_ENCODER_GRIB2_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(ENCODERS_OPTIONS_T),        INTENT(IN)    :: OPTIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA, 'timeRangeIndicator',0)

  PP_METADATA_SET( METADATA,  'stepUnits', 's' )
  PP_METADATA_SET( METADATA,  'stepType',  'max' )
  PP_METADATA_SET( METADATA,  'startStep', 0 )
  PP_METADATA_SET( METADATA,  'endStep',  CURR_TIME%ISEC )

  IF ( CURR_TIME%ISEC .GT. 0 .AND. MOD(CURR_TIME%ISEC,3600) .EQ. 0 ) THEN
    PP_METADATA_SET( METADATA,  'stepUnits', 'h' )
  ENDIF



  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MAX_FROM_STEP0_TIME_ENCODER_GRIB2_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE TIME_ENCODERS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME