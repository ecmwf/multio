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

CHARACTER(LEN=32) :: PACKING_TYPE


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

  ! Local variables
  TYPE(FCKIT_CONFIGURATION) :: COMPRESSION_RUNTIME_CFG
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Default compression type
  PACKING_TYPE = 'grid_simple'

  ! Parse the configuration
  IF ( CFG%GET( 'compression-runtime-utils', COMPRESSION_RUNTIME_CFG ) ) THEN

    IF ( ALLOCATED(CLTMP) ) DEALLOCATE(CLTMP)
    IF ( COMPRESSION_RUNTIME_CFG%GET( 'packing-type', CLTMP  ) ) THEN
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CLTMP), 1 )
      PP_DEBUG_CRITICAL_COND_THROW( LEN(CLTMP).GT.LEN(PACKING_TYPE), 2 )
      PACKING_TYPE = TRIM(CLTMP)
      IF ( ALLOCATED(CLTMP) ) DEALLOCATE( CLTMP )
      CALL COMPRESSION_RUNTIME_CFG%FINAL()
    ELSE
      CALL COMPRESSION_RUNTIME_CFG%FINAL()
      PACKING_TYPE = 'grid_simple'
    ENDIF

  ENDIF
  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read packing type' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'PackingType too long' )
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
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

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
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )


  PP_METADATA_SET( METADATA,  'packingType', TRIM(PACKING_TYPE) )

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
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

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
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Type of level Need to be moved out from here
  PP_METADATA_SET( METADATA,  'packingType', TRIM(PACKING_TYPE) )

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