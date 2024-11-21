#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'general_runtime_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GENERAL_RUNTIME_UTILS_MOD'
MODULE GENERAL_RUNTIME_UTILS_MOD

  ! Symbols imported from intrinsic modules
  USE :: OM_CORE_MOD, ONLY: JPRD_K

IMPLICIT NONE

! Default visibility
PRIVATE

REAL(KIND=JPRD_K), PARAMETER :: DEG = 57.295778667_JPRD_K

! Scald frequencies for WAM
REAL(KIND=JPRD_K), ALLOCATABLE, DIMENSION(:) :: SCTH
REAL(KIND=JPRD_K), ALLOCATABLE, DIMENSION(:) :: SCFR

! Whitelist of public symbols
PUBLIC :: GENERAL_RUNTIME_INIT
PUBLIC :: GENERAL_RUNTIME_FREE
PUBLIC :: SET_PRODUCT_DEFINITION_TEMPLATE_NUMBER_ATM
PUBLIC :: SET_PRODUCT_DEFINITION_TEMPLATE_NUMBER_WAM
PUBLIC :: SET_SPECTRUM_WAM

CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GENERAL_RUNTIME_INIT'
SUBROUTINE GENERAL_RUNTIME_INIT( CFG, MODEL_PARAMS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,  ONLY: MODEL_PAR_T

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN) :: CFG
  TYPE(MODEL_PAR_T), TARGET, INTENT(IN) :: MODEL_PARAMS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE GENERAL_RUNTIME_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GENERAL_RUNTIME_FREE'
SUBROUTINE GENERAL_RUNTIME_FREE( )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( ALLOCATED(SCTH) ) THEN
    DEALLOCATE(SCTH)
  ENDIF

  IF ( ALLOCATED(SCFR) ) THEN
    DEALLOCATE(SCFR)
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE GENERAL_RUNTIME_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_SPECTRUM_WAM'
SUBROUTINE SET_SPECTRUM_WAM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: OM_CORE_MOD,       ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,       ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,       ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD,       ONLY: TIME_HISTORY_T
  USE :: OM_CORE_MOD,       ONLY: OM_WAM_MSG_T
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

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

  ! Local variables
  INTEGER(KIND=JPIB_K) :: KK
  INTEGER(KIND=JPIB_K) :: MM
  INTEGER(KIND=JPIB_K) :: IDIRSCALING
  INTEGER(KIND=JPIB_K) :: IFRESCALING

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  IDIRSCALING = 2
  IFRESCALING = 6
  IF ( .NOT.ALLOCATED(SCTH) ) THEN
    ALLOCATE(SCTH(MODEL_PARAMS%WAM_%NANG))
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(MODEL_PARAMS%WAM_%TH), 1 )
    DO KK=1,MODEL_PARAMS%WAM_%NANG
       SCTH(KK)=NINT(MODEL_PARAMS%WAM_%TH(KK)*10**IDIRSCALING*DEG)
    ENDDO
  ENDIF
  IF ( .NOT.ALLOCATED(SCFR) ) THEN
    ALLOCATE(SCFR(MODEL_PARAMS%WAM_%NFRE_RED))
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(MODEL_PARAMS%WAM_%TH), 2 )
    DO MM=1,MODEL_PARAMS%WAM_%NFRE_RED
      SCFR(MM)=NINT(MODEL_PARAMS%WAM_%FR(MM)*10**IFRESCALING)
    ENDDO
  ENDIF

  PP_METADATA_SET( METADATA,  'numberOfWaveDirections', MODEL_PARAMS%WAM_%NANG )
  PP_METADATA_SET( METADATA,  'scaleFactorOfWaveDirections', IDIRSCALING )
  PP_METADATA_SET( METADATA,  'scaledValuesOfWaveDirections', SCTH )
  PP_METADATA_SET( METADATA,  'numberOfWaveFrequencies', MODEL_PARAMS%WAM_%NFRE_RED )
  PP_METADATA_SET( METADATA,  'scaleFactorOfWaveFrequencies', IFRESCALING )
  PP_METADATA_SET( METADATA,  'scaledValuesOfWaveFrequencies', SCFR )
  PP_METADATA_SET( METADATA,  'waveDirectionNumber', MSG%IANGLE )
  PP_METADATA_SET( METADATA,  'waveFrequencyNumber', MSG%IFREQ )

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=128) :: CGRIB_ID

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Theta not allocated' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Frequency not allocated' )
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

END SUBROUTINE SET_SPECTRUM_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Set the latest table version.
!>
!> This function sets the Latest table version
!>
!> @param [inout] METADATA Metadata object where the GPI will be set.
!> @param [in]    YDIOS    Data structure used to retrieve the information necessary to compute the GPI.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_PRODUCT_DEFINITION_TEMPLATE_NUMBER_WAM'
SUBROUTINE SET_PRODUCT_DEFINITION_TEMPLATE_NUMBER_WAM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: OM_CORE_MOD,       ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,       ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,       ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD,       ONLY: TIME_HISTORY_T
  USE :: OM_CORE_MOD,       ONLY: OM_WAM_MSG_T
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

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

  INTEGER(KIND=JPIB_K), DIMENSION(7), PARAMETER :: LIST=[140114,140115,140116,140117,140118,140119,140120]

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  IF ( ANY(MSG%PARAM_ID_ .EQ. LIST) ) THEN
    IF ( MODEL_PARAMS%WAM_%NTOTENS .GT. 0 ) THEN
      PP_METADATA_SET( METADATA,  'productDefinitionTemplateNumber', MODEL_PARAMS%WAM_%NTRG2TMPP )
    ELSE
      PP_METADATA_SET( METADATA,  'productDefinitionTemplateNumber', MODEL_PARAMS%WAM_%NTRG2TMPD )
    ENDIF
    IF ( MODEL_PARAMS%WAM_%ITMIN .NE. 0 .AND. &
&         MODEL_PARAMS%WAM_%ITMAX .NE. 0  ) THEN
      PP_METADATA_SET( METADATA,  'typeOfWavePeriodInterval', 7)
      PP_METADATA_SET( METADATA,  'scaleFactorOfLowerWavePeriodLimit', 0)
      PP_METADATA_SET( METADATA,  'scaledValueOfLowerWavePeriodLimit', MODEL_PARAMS%WAM_%ITMIN)
      PP_METADATA_SET( METADATA,  'scaleFactorOfUpperWavePeriodLimit', 0)
      PP_METADATA_SET( METADATA,  'scaledValueOfUpperWavePeriodLimit', MODEL_PARAMS%WAM_%ITMAX)
    ELSEIF ( MODEL_PARAMS%WAM_%ITMIN .NE. 0 .AND. &
&             MODEL_PARAMS%WAM_%ITMAX .EQ. 0  ) THEN
      PP_METADATA_SET( METADATA,  'typeOfWavePeriodInterval', 3)
      PP_METADATA_SET( METADATA,  'scaleFactorOfLowerWavePeriodLimit', 0)
      PP_METADATA_SET( METADATA,  'scaledValueOfLowerWavePeriodLimit', MODEL_PARAMS%WAM_%ITMIN)
    ELSEIF ( MODEL_PARAMS%WAM_%ITMIN .EQ. 0 .AND. &
&             MODEL_PARAMS%WAM_%ITMAX .NE. 0  ) THEN
      PP_METADATA_SET( METADATA,  'typeOfWavePeriodInterval', 4)
      PP_METADATA_SET( METADATA,  'scaleFactorOfUpperWavePeriodLimit', 0)
      PP_METADATA_SET( METADATA,  'scaledValueOfUpperWavePeriodLimit', MODEL_PARAMS%WAM_%ITMAX)
    ENDIF
  ELSEIF ( MSG%PARAM_ID_ .EQ. 140251 ) THEN
    IF ( MODEL_PARAMS%WAM_%NTOTENS .GT. 0 ) THEN
      PP_METADATA_SET( METADATA,  'productDefinitionTemplateNumber', MODEL_PARAMS%WAM_%NSPEC2TMPP )
    ELSE
      PP_METADATA_SET( METADATA,  'productDefinitionTemplateNumber', MODEL_PARAMS%WAM_%NSPEC2TMPD )
    ENDIF
  ELSE
    IF ( MODEL_PARAMS%WAM_%NTOTENS .GT. 0 ) THEN
      PP_METADATA_SET( METADATA,  'productDefinitionTemplateNumber', 1 )
    ELSE
      PP_METADATA_SET( METADATA,  'productDefinitionTemplateNumber', 0 )
    ENDIF
  ENDIF




  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE SET_PRODUCT_DEFINITION_TEMPLATE_NUMBER_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Set the latest table version.
!>
!> This function sets the Latest table version
!>
!> @param [inout] METADATA Metadata object where the GPI will be set.
!> @param [in]    YDIOS    Data structure used to retrieve the information necessary to compute the GPI.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_PRODUCT_DEFINITION_TEMPLATE_NUMBER_ATM'
SUBROUTINE SET_PRODUCT_DEFINITION_TEMPLATE_NUMBER_ATM( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,       ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,       ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD,       ONLY: TIME_HISTORY_T
  USE :: OM_CORE_MOD,       ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

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


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Set product definition template number
  ! IF ( CURR_TIME%IS_STEP_0 ) THEN
  !   PP_METADATA_SET( METADATA,  'productDefinitionTemplateNumber', GRIB_INFO%PRODUCT_DEFINITION_TEMPLATE_NUMBER0_ )
  ! ELSE
    PP_METADATA_SET( METADATA,  'productDefinitionTemplateNumber', GRIB_INFO%PRODUCT_DEFINITION_TEMPLATE_NUMBER_ )
  ! ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE SET_PRODUCT_DEFINITION_TEMPLATE_NUMBER_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GENERAL_RUNTIME_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME