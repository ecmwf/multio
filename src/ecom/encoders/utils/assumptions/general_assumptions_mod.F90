#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'general_assumptions_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GENERAL_ASSUMPTIONS_MOD'
MODULE GENERAL_ASSUMPTIONS_MOD
IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: IS_ENSAMBLE_SIMULATION
PUBLIC :: GENERAL_ASSUMPTIONS_INIT
PUBLIC :: GENERAL_ASSUMPTIONS_FREE

CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GENERAL_ASSUMPTIONS_INIT'
SUBROUTINE GENERAL_ASSUMPTIONS_INIT( CFG, MODEL_PARAMS )

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

END SUBROUTINE GENERAL_ASSUMPTIONS_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GENERAL_ASSUMPTIONS_FREE'
SUBROUTINE GENERAL_ASSUMPTIONS_FREE( )

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

END SUBROUTINE GENERAL_ASSUMPTIONS_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'IS_ENSAMBLE_SIMULATION'
FUNCTION IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) RESULT(LDRET)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T), INTENT(IN) :: YDMODEL_PARAMETERS

  ! Function result
  LOGICAL :: LDRET

  ! Local variables
  LOGICAL, DIMENSION(5) :: CONDITIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Sepcial cases handling
  CONDITIONS(1) = (YDMODEL_PARAMETERS%SIM_%CTYPE .EQ. 'cf')    ! Control forecast (gribCode=10)
  CONDITIONS(2) = (YDMODEL_PARAMETERS%SIM_%CTYPE .EQ. 'pf')    ! Perturbed forecast (gribCode=11)
  CONDITIONS(3) = (YDMODEL_PARAMETERS%SIM_%CTYPE .EQ. 'cv')    ! Calibration/Validation forecast (gribCode=65)
  CONDITIONS(4) = (YDMODEL_PARAMETERS%SIM_%NSTREAM .EQ. 1030)  ! 'enda' -> Ensemble data assimilation
  CONDITIONS(5) = (YDMODEL_PARAMETERS%SIM_%NSTREAM .EQ. 1249)  ! 'elda' -> Ensemble Long window Data Assimilation

  LDRET = ANY(CONDITIONS)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION IS_ENSAMBLE_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE GENERAL_ASSUMPTIONS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME