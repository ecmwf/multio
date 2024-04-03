#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'general_preset_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GENERAL_PRESET_UTILS_MOD'
MODULE GENERAL_PRESET_UTILS_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: GENERAL_PRESET_INIT
PUBLIC :: GENERAL_PRESET_FREE
PUBLIC :: LATEST_TABLE_VERSION
PUBLIC :: GENERATING_PROCESS_IDENTIFIER_PRESET
PUBLIC :: SET_PRODUCT_DEFINITION_TEMPLATE_NUMBER_WAM
PUBLIC :: GENERATING_PROCESS_IDENTIFIER_PRESET_WAM

CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GENERAL_PRESET_INIT'
SUBROUTINE GENERAL_PRESET_INIT( CFG, MODEL_PARAMS )

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

END SUBROUTINE GENERAL_PRESET_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GENERAL_PRESET_FREE'
SUBROUTINE GENERAL_PRESET_FREE( )

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

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE GENERAL_PRESET_FREE
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
SUBROUTINE SET_PRODUCT_DEFINITION_TEMPLATE_NUMBER_WAM( MODEL_PARAMS, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: OM_CORE_MOD,       ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,       ONLY: MODEL_PAR_T
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Set product definition template number
  IF ( MODEL_PARAMS%WAM_%NTOTENS .GT. 0 ) THEN
    PP_METADATA_SET( METADATA,  'productDefinitionTemplateNumber', MODEL_PARAMS%WAM_%NTRG2TMPP )
  ELSE
    PP_METADATA_SET( METADATA,  'productDefinitionTemplateNumber', MODEL_PARAMS%WAM_%NTRG2TMPD )
  ENDIF

  IF ( MODEL_PARAMS%WAM_%ITMIN .NE. 0 .AND. &
&      MODEL_PARAMS%WAM_%ITMAX .NE. 0  ) THEN
    PP_METADATA_SET( METADATA,  'typeOfWavePeriodInterval', 7)
    PP_METADATA_SET( METADATA,  'scaleFactorOfLowerWavePeriodLimit', 0)
    PP_METADATA_SET( METADATA,  'scaledValueOfLowerWavePeriodLimit', MODEL_PARAMS%WAM_%ITMIN)
    PP_METADATA_SET( METADATA,  'scaleFactorOfUpperWavePeriodLimit', 0)
    PP_METADATA_SET( METADATA,  'scaledValueOfUpperWavePeriodLimit', MODEL_PARAMS%WAM_%ITMAX)
  ELSEIF ( MODEL_PARAMS%WAM_%ITMIN .NE. 0 .AND. &
&          MODEL_PARAMS%WAM_%ITMAX .EQ. 0  ) THEN
    PP_METADATA_SET( METADATA,  'typeOfWavePeriodInterval', 3)
    PP_METADATA_SET( METADATA,  'scaleFactorOfLowerWavePeriodLimit', 0)
    PP_METADATA_SET( METADATA,  'scaledValueOfLowerWavePeriodLimit', MODEL_PARAMS%WAM_%ITMIN)
  ELSEIF ( MODEL_PARAMS%WAM_%ITMIN .EQ. 0 .AND. &
&          MODEL_PARAMS%WAM_%ITMAX .NE. 0  ) THEN
    PP_METADATA_SET( METADATA,  'typeOfWavePeriodInterval', 4)
    PP_METADATA_SET( METADATA,  'scaleFactorOfUpperWavePeriodLimit', 0)
    PP_METADATA_SET( METADATA,  'scaledValueOfUpperWavePeriodLimit', MODEL_PARAMS%WAM_%ITMAX)
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
#define PP_PROCEDURE_NAME 'LATEST_TABLE_VERSION'
SUBROUTINE LATEST_TABLE_VERSION( MODEL_PARAMS, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: OM_CORE_MOD,       ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,       ONLY: MODEL_PAR_T
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA,  'tablesVersion', MODEL_PARAMS%SIM_%IGRIB2_TABLES_VERSION_LATEST )

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE LATEST_TABLE_VERSION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Sets the Generating Process Identifier (GPI) in the metadata object.
!>
!> This function sets the Generating Process Identifier (GPI) into the provided metadata object
!> using information retrieved from the specified data structure (YDIOS).
!>
!> @param [inout] METADATA Metadata object where the GPI will be set.
!> @param [in]    YDIOS    Data structure used to retrieve the information necessary to compute the GPI.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GENERATING_PROCESS_IDENTIFIER_PRESET'
SUBROUTINE GENERATING_PROCESS_IDENTIFIER_PRESET( MODEL_PARAMS, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA,  'generatingProcessIdentifier', MODEL_PARAMS%SIM_%NCYCLE )

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE GENERATING_PROCESS_IDENTIFIER_PRESET
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Sets the Generating Process Identifier (GPI) in the metadata object.
!>
!> This function sets the Generating Process Identifier (GPI) into the provided metadata object
!> using information retrieved from the specified data structure (YDIOS).
!>
!> @param [inout] METADATA Metadata object where the GPI will be set.
!> @param [in]    YDIOS    Data structure used to retrieve the information necessary to compute the GPI.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GENERATING_PROCESS_IDENTIFIER_PRESET_WAM'
SUBROUTINE GENERATING_PROCESS_IDENTIFIER_PRESET_WAM( MODEL_PARAMS, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

!     MODEL IDENTIFICATION.
  IF (  MODEL_PARAMS%WAM_%CLDOMAIN == 'g' ) THEN
    PP_METADATA_SET( METADATA,  'generatingProcessIdentifier', MODEL_PARAMS%WAM_%IMDLGRBID_G )
  ELSE
    PP_METADATA_SET( METADATA,  'generatingProcessIdentifier', MODEL_PARAMS%WAM_%IMDLGRBID_M )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE GENERATING_PROCESS_IDENTIFIER_PRESET_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GENERAL_PRESET_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME