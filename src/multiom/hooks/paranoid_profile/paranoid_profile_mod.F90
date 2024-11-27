! Include preprocessor utils
! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'paranoid_profile_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'PARANOID_PROFILE_MOD'
MODULE PARANOID_PROFILE_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

! The profiler type
TYPE :: PARANOID_PROFILER_T
  PRIVATE
END TYPE


! Whitelist of public symbols (types)
PUBLIC :: PARANOID_PROFILER_T

END MODULE PARANOID_PROFILE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
