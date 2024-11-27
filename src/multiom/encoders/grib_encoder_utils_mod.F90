! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib_encoder_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MODULE YAML_CORE_UTIGRIB_ENCODER_UTILS_MODLS_MOD'
MODULE GRIB_ENCODER_UTILS_MOD
IMPLICIT NONE

PRIVATE

PUBLIC :: VALIDATE_LENGTH_OF_TIMERANGE
PUBLIC :: VALIDATE_TYPE_OF_STATISTICAL_PROCESS

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'VALIDATE_LENGTH_OF_TIMERANGE'
PP_THREAD_SAFE FUNCTION VALIDATE_LENGTH_OF_TIMERANGE( LENGTH_OF_TIMERANGE, VALID, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: LENGTH_OF_TIMERANGE
  LOGICAL,              INTENT(OUT)   :: VALID
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for loging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  SELECT CASE (LENGTH_OF_TIMERANGE)
    CASE (1*3600)
      VALID = .TRUE.
    CASE (3*3600)
      VALID = .TRUE.
    CASE (6*3600)
      VALID = .TRUE.
    CASE DEFAULT
      VALID = .FALSE.
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION VALIDATE_LENGTH_OF_TIMERANGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'VALIDATE_TYPE_OF_STATISTICAL_PROCESS'
PP_THREAD_SAFE FUNCTION VALIDATE_TYPE_OF_STATISTICAL_PROCESS( TYPE_OF_STATISTICAL_PROCESS, OP_ID, VALID, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: TYPE_OF_STATISTICAL_PROCESS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: OP_ID
  LOGICAL,              INTENT(OUT)   :: VALID
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for loging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  SELECT CASE ( TYPE_OF_STATISTICAL_PROCESS )
  CASE ('Average')
      OP_ID = 0
      VALID = .TRUE.
  CASE ('Accumulation')
      OP_ID = 1
      VALID = .TRUE.
  CASE ('Maximum')
      OP_ID = 2
      VALID = .TRUE.
  CASE ('Minimum')
      OP_ID = 3
      VALID = .TRUE.
  CASE ('Difference forward')
      OP_ID = 4
      VALID = .TRUE.
  CASE ('Root mean square')
      OP_ID = 5
      VALID = .TRUE.
  CASE ('Standard deviation')
      OP_ID = 6
      VALID = .TRUE.
  CASE ('Covariance')
      OP_ID = 7
      VALID = .TRUE.
  CASE ('Difference backward')
      OP_ID = 8
      VALID = .TRUE.
  CASE ('Ratio')
      OP_ID = 9
      VALID = .TRUE.
  CASE ('Standardized anomaly')
      OP_ID = 10
      VALID = .TRUE.
  CASE ('Summation')
      OP_ID = 11
      VALID = .TRUE.
  CASE ('Severity')
      OP_ID = 100
      VALID = .TRUE.
  CASE ('Mode')
      OP_ID = 101
      VALID = .TRUE.
  CASE ('Missing')
      OP_ID = 255
      VALID = .TRUE.
  CASE DEFAULT
      OP_ID = -1
      VALID = .FALSE.
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION VALIDATE_TYPE_OF_STATISTICAL_PROCESS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GRIB_ENCODER_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME