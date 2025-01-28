! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'hooks_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'HOOKS_MOD'
MODULE HOOKS_MOD

  USE :: DEBUG_MOD, ONLY: DEBUG_T
  USE :: TRACE_MOD, ONLY: TRACER_T
  USE :: LOG_MOD,   ONLY: LOGGER_T
  USE :: PARANOID_PROFILE_MOD, ONLY: PARANOID_PROFILER_T

IMPLICIT NONE

!> Default visibility of the module members
PRIVATE


TYPE :: HOOKS_T
  LOGICAL :: VERBOSE_
  ! CLASS(OPTIONS_HOOK_A), POINTER :: OPTIONS_HOOK_ => NULL()
  ! CLASS(LOGGING_HOOK_A), POINTER :: LOGGING_HOOK_ => NULL()
  TYPE(TRACER_T) :: TRACE_HOOK_
  TYPE(LOGGER_T) :: LOG_HOOK_
  TYPE(DEBUG_T)  :: DEBUG_HOOK_
  TYPE(PARANOID_PROFILER_T) :: PARANOID_PROFILE_HOOK_
END TYPE

!> Public symbols
PUBLIC :: HOOKS_T

END MODULE HOOKS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME