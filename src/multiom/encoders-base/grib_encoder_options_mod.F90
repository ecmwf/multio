! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib_encoder_options_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_ENCODER_OPTIONS_MOD'
MODULE GRIB_ENCODER_OPTIONS_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,   ONLY: OPT_CACHE_FULL_E

IMPLICIT NONE

PRIVATE

TYPE :: GRIB_ENCODER_OPTIONS_T

  ! Variables used to control the cache dump functionality
  LOGICAL :: ALLOW_MULTIPLE_ENCODING_RULES = .FALSE.
  LOGICAL :: DUMP_SAMPLES_TO_ONE_FILE = .TRUE.
  LOGICAL :: PRINT_DICTIONARIES = .FALSE.
  LOGICAL :: WRITE_DEBUG_FIELDS = .FALSE.
  CHARACTER(LEN=256) :: DUMP_PATH = '.'

  ! Variables used to control the size of the cache
  LOGICAL :: CACHE_LOCAL_USE_INFO          = .TRUE. ! Should always be true
  LOGICAL :: CACHE_PRODUCT_DEFINITION_INFO = .TRUE. ! Should always be true
  LOGICAL :: CACHE_GRID_DEFINITION_INFO    = .TRUE. ! Should always be true

  LOGICAL :: CACHE_SATELLITES_INFO         = .FALSE.
  LOGICAL :: CACHE_TYPE_OF_LEVELS          = .FALSE.
  LOGICAL :: CACHE_DIRECTION_FREQUENCY     = .FALSE.

  ! This should never be true, it is only for consistency in the comparison routines
  LOGICAL :: CACHE_TIME_RELATED_INFO   = .FALSE. ! Should always be false

  ! Variables used to control the cache usage
  LOGICAL :: ENABLE_CACHE = .FALSE.

  LOGICAL :: USE_TYPE_OF_LEVEL = .FALSE.
  INTEGER(KIND=JPIB_K) :: CACHE_STRATEGY = OPT_CACHE_FULL_E

  ! Dimension of the time history buffer
  INTEGER(KIND=JPIB_K) :: TIME_HISTORY_CAPACITY = 100_JPIB_K

END TYPE



PUBLIC :: GRIB_ENCODER_OPTIONS_T

END MODULE GRIB_ENCODER_OPTIONS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
