#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'compression_runtime_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'COMPRESSION_RUNTIME_UTILS_MOD'
MODULE COMPRESSION_RUNTIME_UTILS_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: COMPRESSION_RUNTIME_INIT
PUBLIC :: COMPRESSION_RUNTIME_FREE
PUBLIC :: SET_COMPRESSION_ATM
PUBLIC :: SET_COMPRESSION_WAM

CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'COMPRESSION_RUNTIME_INIT'
SUBROUTINE COMPRESSION_RUNTIME_INIT( CFG, MODEL_PARAMS )

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

END SUBROUTINE COMPRESSION_RUNTIME_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'LEVELS_RUNTIME_FREE'
SUBROUTINE COMPRESSION_RUNTIME_FREE( )

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

END SUBROUTINE COMPRESSION_RUNTIME_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_COMPRESSION_ATM'
SUBROUTINE SET_COMPRESSION_ATM( MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD,        ONLY: PACKING_TYPE_GRIB_SIMPLE_E
  USE :: OM_CORE_MOD,        ONLY: PACKING_TYPE_GRIB_CCSDE_E
  USE :: OM_CORE_MOD,        ONLY: PACKING_TYPE_GRIB_COMPLEX_E
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  SELECT CASE (GRIB_INFO%PACKING_TYPE)
  CASE (PACKING_TYPE_GRIB_SIMPLE_E)
    PP_METADATA_SET( METADATA,  'packingType', 'grid_simple' )
  CASE (PACKING_TYPE_GRIB_CCSDE_E)
    PP_METADATA_SET( METADATA,  'packingType', 'grid_ccsds' )
  CASE (PACKING_TYPE_GRIB_COMPLEX_E)
    PP_METADATA_SET( METADATA, 'packingType', 'spectral_complex'  )
  CASE DEFAULT
    PP_LOG_DEVELOP_STR( 'ENCODER: warning invalid "packingType"' )
  END SELECT

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE SET_COMPRESSION_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_COMPRESSION_WAM'
SUBROUTINE SET_COMPRESSION_WAM( MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_WAM_MSG_T
  USE :: OM_CORE_MOD,        ONLY: PACKING_TYPE_GRIB_SIMPLE_E
  USE :: OM_CORE_MOD,        ONLY: PACKING_TYPE_GRIB_CCSDE_E
  USE :: OM_CORE_MOD,        ONLY: PACKING_TYPE_GRIB_COMPLEX_E
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_WAM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Type of level Need to be moved out from here
  SELECT CASE (GRIB_INFO%PACKING_TYPE)
  CASE (PACKING_TYPE_GRIB_SIMPLE_E)
    PP_METADATA_SET( METADATA,  'packingType', 'grid_simple' )
  CASE (PACKING_TYPE_GRIB_CCSDE_E)
    PP_METADATA_SET( METADATA,  'packingType', 'grid_ccsds' )
  CASE (PACKING_TYPE_GRIB_COMPLEX_E)
    PP_METADATA_SET( METADATA, 'packingType', 'spectral_complex'  )
  CASE DEFAULT
    PP_LOG_DEVELOP_STR( 'ENCODER: warning invalid "packingType"' )
  END SELECT

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE SET_COMPRESSION_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE COMPRESSION_RUNTIME_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME