! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'constants_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'CONSTANTS_MOD'
MODULE CONSTANTS_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!> Constants
REAL(KIND=JPRD_K), PARAMETER :: RAD2DEG=57.29577951308232087679815481410517033240547246656432154916_JPRD_K


!> Whitelist of public symbols
PUBLIC :: RAD2DEG

END MODULE CONSTANTS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
