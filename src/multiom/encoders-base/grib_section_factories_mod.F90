! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib_section_factories_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_SECTION_FACTORIES_MOD'
MODULE GRIB_SECTION_FACTORIES_MOD

IMPLICIT NONE

!> Default visibility of the module members
PRIVATE



INTERFACE
PP_THREAD_SAFE FUNCTION GRIB_SECTION_FACTORY( GRIB_SECTION, CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A),   INTENT(INOUT) :: GRIB_SECTION
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION GRIB_SECTION_FACTORY
END INTERFACE


TYPE :: GRIB_SECTION_FACTORY_T
  !> The function pointer type for the GRIB section factory
  PROCEDURE(GRIB_SECTION_FACTORY), POINTER, NOPASS :: GRIB2_SECTION0_FACTORY_MOD => NULL()
END TYPE GRIB_SECTION_FACTORY_T


!> Whitelist of public symbols
PUBLIC :: GRIB_SECTION_FACTORY_T
PUBLIC :: GRIB_SECTION_FACTORY

END MODULE GRIB_SECTION_FACTORIES_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
