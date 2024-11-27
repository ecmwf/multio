! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'encoding_rule_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ENCODING_INFO_MOD'
MODULE ENCODING_INFO_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Default visibility of the module
  PRIVATE

  !> Encoding info data structure
  TYPE :: ENCODING_INFO_T

    ! Default visibility of the type
    PRIVATE

    !> Wrapped encoder
    CLASS(GRIB_SECTION_BASE_A), POINTER :: ENCODER_ => NULL()

    !> Tag
    CHARACTER(LEN=256) :: TAG_ = REPEAT(' ',256)

    !> To decide what to do when free the encoder
    LOGICAL :: IS_LAZY_ = .FALSE.

  CONTAINS

    PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: INIT    => ENCODING_INFO_INIT
    ! PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: PREPARE => ENCODING_INFO_PREPARE
    ! PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: ENCODE  => ENCODING_INFO_ENCODE
    ! PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: FREE    => ENCODING_INFO_FREE

  END TYPE

  !> Whitelist of public symbols (types)
  PUBLIC :: ENCODING_INFO_T

CONTAINS



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_INFO_INIT'
PP_THREAD_SAFE FUNCTION ENCODING_INFO_INIT( THIS, ENCODER, TAG, IS_LAZY, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENCODING_INFO_T),              INTENT(INOUT) :: THIS
  CLASS(GRIB_SECTION_BASE_A), POINTER, INTENT(IN)    :: ENCODER
  CHARACTER(LEN=256),                  INTENT(IN)    :: TAG
  LOGICAL,                             INTENT(IN)    :: IS_LAZY
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Associate the fields
  THIS%ENCODER_ => ENCODER
  THIS%TAG_ = TAG

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION ENCODING_INFO_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE ENCODING_INFO_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
