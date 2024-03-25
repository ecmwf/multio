#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib_encoder_grib1_gg_ws_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_ENCODER_GRIB1_GG_WS_MOD'
MODULE GRIB_ENCODER_GRIB1_GG_WS_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,           ONLY: JPIM_K
  USE :: GRIB_ENCODER_BASE_MOD, ONLY: GRIB_ENCODER_A

IMPLICIT NONE

! Default visibility of the module
PRIVATE

! Encoder for GRIB1 pressure level fields
TYPE, EXTENDS(GRIB_ENCODER_A) :: ENCODER_GRIB1_GG_WS

  ! Default visibility
  PRIVATE

  !> @brief Metadata handlers used to store encoded metadata.
  INTEGER(KIND=JPIM_K) :: SAMPLE_HANDLE_ = 0_JPIM_K

CONTAINS
  ! Virtual procedures
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INITIALISE => ENCODER_GRIB1_GG_WS_INITIALISE
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ENCODE_ATM => ENCODER_GRIB1_GG_WS_ENCODE_ATM
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ENCODE_WAM => ENCODER_GRIB1_GG_WS_ENCODE_WAM
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FINALISE   => ENCODER_GRIB1_GG_WS_FINALISE
END TYPE

! Name of the grib sample to be loaded
CHARACTER(LEN=*), PARAMETER :: SAMPLE_NAME_GRIB1_GG_WS='gg_sfc_grib1'

! Whitelist of public symbols
PUBLIC :: MAKE_GRIB1_GG_WS_ENCODER
PUBLIC :: DESTROY_GRIB1_GG_WS_ENCODER

CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAKE_GRIB1_GG_WS_ENCODER'
SUBROUTINE MAKE_GRIB1_GG_WS_ENCODER( CFG, MODEL_PARAMS, METADATA_KIND, ENCODER )

  ! Symbols imported from other modules within the project.
  USE :: GRIB_ENCODER_BASE_MOD, ONLY: GRIB_ENCODER_A
  USE :: OM_CORE_MOD,           ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,           ONLY: JPIB_K

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION),      INTENT(IN) :: CFG
  TYPE(MODEL_PAR_T),              INTENT(IN)    :: MODEL_PARAMS
  CHARACTER(LEN=*),               INTENT(IN)    :: METADATA_KIND
  CLASS(GRIB_ENCODER_A), POINTER, INTENT(INOUT) :: ENCODER

  ! Local variables
  INTEGER(KIND=JPIB_K) :: GRIB_HANDLE
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Allocate the encoder
  NULLIFY(ENCODER)
  ALLOCATE(ENCODER_GRIB1_GG_WS::ENCODER, STAT=STAT, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 1 )

  ! Allocation of the internal structure of the encoder
  CALL ENCODER%INITIALISE( MODEL_PARAMS, METADATA_KIND )

  ! If errmsg is allocated and any I arrive here something is wrong
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG)
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate the encoder' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MAKE_GRIB1_GG_WS_ENCODER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'DESTROY_GRIB1_GG_WS_ENCODER'
SUBROUTINE DESTROY_GRIB1_GG_WS_ENCODER( ENCODER )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_ENCODER_A), POINTER, INTENT(INOUT) :: ENCODER

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Free the internal memory of the encoder
  IF ( ASSOCIATED( ENCODER ) ) THEN
    CALL ENCODER%FINALISE()
    DEALLOCATE( ENCODER )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE DESTROY_GRIB1_GG_WS_ENCODER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODER_GRIB1_GG_WS_INITIALISE'
SUBROUTINE ENCODER_GRIB1_GG_WS_INITIALISE( THIS, MODEL_PARAMS, METADATA_KIND )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: GRIB_API,    ONLY: GRIB_NEW_FROM_SAMPLES
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODER_GRIB1_GG_WS), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),           INTENT(IN)    :: METADATA_KIND
  TYPE(MODEL_PAR_T),          INTENT(IN)    :: MODEL_PARAMS

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialise the grib handle
  CALL GRIB_NEW_FROM_SAMPLES( THIS%SAMPLE_HANDLE_, TRIM(ADJUSTL(SAMPLE_NAME_GRIB1_GG_WS)), STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 1 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to read the sample.', KRET, GRIB_ERROR )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE ENCODER_GRIB1_GG_WS_INITIALISE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODER_GRIB1_GG_WS_ENCODE_ATM'
FUNCTION ENCODER_GRIB1_GG_WS_ENCODE_ATM( THIS, MODEL_PARAMS, GRIB_INFO, &
& TIME_HIST, CURR_TIME, MSG, METADATA ) RESULT(EX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: GRIB1_E
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODER_GRIB1_GG_WS),      INTENT(INOUT) :: THIS
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Function result
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )


  EX = .FALSE.


  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION ENCODER_GRIB1_GG_WS_ENCODE_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODER_GRIB1_GG_WS_ENCODE_WAM'
FUNCTION ENCODER_GRIB1_GG_WS_ENCODE_WAM( THIS, MODEL_PARAMS, GRIB_INFO, &
& TIME_HIST, CURR_TIME, MSG, METADATA ) RESULT(EX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: GRIB1_E
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_WAM_MSG_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: TRACK_TIME_MOD,     ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,     ONLY: CURR_TIME_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODER_GRIB1_GG_WS),      INTENT(INOUT) :: THIS
  TYPE(MODEL_PAR_T),               INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(OM_WAM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Function result
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  EX = .FALSE.


  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION ENCODER_GRIB1_GG_WS_ENCODE_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODER_GRIB1_GG_WS_FINALISE'
SUBROUTINE ENCODER_GRIB1_GG_WS_FINALISE( THIS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: GRIB_API,    ONLY: GRIB_RELEASE
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODER_GRIB1_GG_WS), INTENT(INOUT) :: THIS

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Destroy the handle
  CALL GRIB_RELEASE( THIS%SAMPLE_HANDLE_, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 1 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to destroy the sample.', KRET, GRIB_ERROR )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE ENCODER_GRIB1_GG_WS_FINALISE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB_ENCODER_GRIB1_GG_WS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME