! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'cashed_mapper_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'CASHED_MAPPER_MOD'
MODULE CASHED_MAPPER_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: ASSIGNMENT_BASE_MOD, ONLY: ASSIGNMENT_BASE_A

IMPLICIT NONE

  !> Default visibility of the module
  PRIVATE

  !> Encoding info data structure
  TYPE :: CASHED_MAPPER_T

    ! Default visibility of the type
    PRIVATE

    !> Wrapped encoder
    CLASS(ASSIGNMENT_BASE_A), POINTER :: ASSIGNMENT_ => NULL()

    !> Tag
    CHARACTER(LEN=256) :: TAG_ = REPEAT(' ',256)

    !> NAme
    CHARACTER(LEN=256) :: NAME_ = REPEAT(' ',256)

    !> Initialization status
    LOGICAL :: INITIALIZED_ = .FALSE.

  CONTAINS

    PROCEDURE, PASS, PRIVATE, NON_OVERRIDABLE :: INIT_DEFAULT => CASHED_MAPPER_INIT_DEFAULT
    PROCEDURE, PASS, PRIVATE, NON_OVERRIDABLE :: INIT_COPY => CASHED_MAPPER_INIT_COPY
    GENERIC, PUBLIC :: INIT => INIT_DEFAULT, INIT_COPY

    PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: EVAL => CASHED_MAPPER_EVAL
    PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: FREE => CASHED_MAPPER_FREE

  END TYPE

  !> Whitelist of public symbols (types)
  PUBLIC :: CASHED_MAPPER_T

CONTAINS



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CASHED_MAPPER_INIT_DEFAULT'
PP_THREAD_SAFE FUNCTION CASHED_MAPPER_INIT_DEFAULT( THIS, ASSIGNMENT, TAG, NAME, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: ASSIGNMENT_BASE_MOD, ONLY: ASSIGNMENT_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CASHED_MAPPER_T),            INTENT(INOUT) :: THIS
  CLASS(ASSIGNMENT_BASE_A), POINTER, INTENT(IN)    :: ASSIGNMENT
  CHARACTER(LEN=256),                INTENT(IN)    :: NAME
  CHARACTER(LEN=256),                INTENT(IN)    :: TAG
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

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
  THIS%ASSIGNMENT_ => ASSIGNMENT
  THIS%NAME_ = NAME
  THIS%TAG_ = TAG
  THIS%INITIALIZED_ = .TRUE.

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION CASHED_MAPPER_INIT_DEFAULT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CASHED_MAPPER_INIT_COPY'
PP_THREAD_SAFE FUNCTION CASHED_MAPPER_INIT_COPY( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: ASSIGNMENT_BASE_MOD, ONLY: ASSIGNMENT_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CASHED_MAPPER_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

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
  THIS%ASSIGNMENT_ => NULL()
  THIS%NAME_ = 'default copy assignment'
  THIS%TAG_ = 'copy assignment'
  THIS%INITIALIZED_ = .TRUE.

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION CASHED_MAPPER_INIT_COPY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CASHED_MAPPER_FREE'
PP_THREAD_SAFE FUNCTION CASHED_MAPPER_FREE( THIS, MAPPING_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MAPPING_OPTIONS_MOD, ONLY: MAPPING_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CASHED_MAPPER_T),  INTENT(INOUT) :: THIS
  TYPE(MAPPING_OPTIONS_T), INTENT(IN)    :: MAPPING_OPT
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

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
  THIS%ASSIGNMENT_ => NULL()
  THIS%TAG_  = REPEAT(' ',256)
  THIS%INITIALIZED_ = .FALSE.

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION CASHED_MAPPER_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CASHED_MAPPER_EVAL'
PP_THREAD_SAFE FUNCTION CASHED_MAPPER_EVAL( THIS, IN_MSG, IN_PAR, &
&                   OUT_MSG, OUT_PAR, TAG, NAME, HOOKS  ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CASHED_MAPPER_T),   INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),  INTENT(IN)    :: IN_MSG
  TYPE(PARAMETRIZATION_T),  INTENT(IN)    :: IN_PAR
  TYPE(FORTRAN_MESSAGE_T),  INTENT(OUT)   :: OUT_MSG
  TYPE(PARAMETRIZATION_T),  INTENT(OUT)   :: OUT_PAR
  CHARACTER(LEN=256),       INTENT(OUT)   :: TAG
  CHARACTER(LEN=256),       INTENT(OUT)   :: NAME
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EVALUATE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COPY_MESSAGE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COPY_PARAMETRIZATION=4_JPIB_K

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

  ! Error handling: Check if the encoder is associated
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_MAPPING_NOT_INITIALIZED )

  ! Associate the fields
  IF ( ASSOCIATED(THIS%ASSIGNMENT_) ) THEN
    TAG  = THIS%TAG_
    NAME = THIS%NAME_
    PP_TRYCALL(ERRFLAG_UNABLE_TO_EVALUATE) THIS%ASSIGNMENT_%EVAL( IN_MSG, IN_PAR, OUT_MSG, OUT_PAR, HOOKS )
  ELSE
    ! The default mapper just copy the input to the output
    PP_TRYCALL(ERRFLAG_UNABLE_TO_COPY_MESSAGE) OUT_MSG%COPY_FROM( IN_MSG, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_COPY_PARAMETRIZATION) OUT_PAR%COPY_FROM( IN_PAR, HOOKS )
    TAG  = THIS%TAG_
    NAME = THIS%NAME_
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_MAPPING_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Assignment not associated' )
    CASE(ERRFLAG_UNABLE_TO_EVALUATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to evaluate assignment' )
    CASE(ERRFLAG_UNABLE_TO_COPY_MESSAGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to copy message' )
    CASE(ERRFLAG_UNABLE_TO_COPY_PARAMETRIZATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to copy parametrization' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION CASHED_MAPPER_EVAL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE CASHED_MAPPER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
