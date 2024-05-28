#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'level_assumptions_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'LEVEL_ASSUMPTIONS_MOD'
MODULE LEVEL_ASSUMPTIONS_MOD
IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: COMPUTE_TOPBOT
PUBLIC :: LEVEL_ASSUMPTIONS_INIT
PUBLIC :: LEVEL_ASSUMPTIONS_FREE


CONTAINS

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'LEVEL_ASSUMPTIONS_INIT'
SUBROUTINE LEVEL_ASSUMPTIONS_INIT( CFG, MODEL_PARAMS )

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

END SUBROUTINE LEVEL_ASSUMPTIONS_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'LEVEL_ASSUMPTIONS_FREE'
SUBROUTINE LEVEL_ASSUMPTIONS_FREE( )

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

END SUBROUTINE LEVEL_ASSUMPTIONS_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'COMPUTE_TOPBOT'
SUBROUTINE COMPUTE_TOPBOT( MODEL_PARAMS, PARAMID, ITOP, IBOT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_CORE_MOD,          ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,          ONLY: UNDEF_PARAM_E
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_FINDLOC

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),     INTENT(IN)  :: MODEL_PARAMS
  INTEGER(KIND=JPIB_K),  INTENT(IN)  :: PARAMID
  INTEGER(KIND=JPIB_K),  INTENT(OUT) :: ITOP
  INTEGER(KIND=JPIB_K),  INTENT(OUT) :: IBOT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: IL

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Output initialization
  IL = UNDEF_PARAM_E

  ! Compute top/bottom levels (Fortran 2008 FindLoc)
  ! IL = FINDLOC( MODEL_PARAMS%GEO_%NSFLEVS(:,1), PARAMID, DIM=1)
  IL = OM_FINDLOC( MODEL_PARAMS%GEO_%NSFLEVS(:,1), PARAMID )

  ! If found then associate ibot/itop
  IF( IL .NE. 0 ) THEN
    IBOT = MODEL_PARAMS%GEO_%NSFLEVS(IL,2)
    ITOP = MODEL_PARAMS%GEO_%NSFLEVS(IL,3)
  ELSE
    IBOT = UNDEF_PARAM_E
    ITOP = UNDEF_PARAM_E
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE COMPUTE_TOPBOT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE LEVEL_ASSUMPTIONS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME