! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'representations_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'REPRESENTATIONS_MOD'
MODULE REPRESENTATIONS_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

IMPLICIT NONE

!> @brief Default visibility of the module
PRIVATE


TYPE, ABSTRACT :: REPRES_A
  CHARACTER(LEN=32) :: DATA_REPRESENTATION_TYPE=REPEAT(' ',32)
  CHARACTER(LEN=32) :: NAME=REPEAT(' ',32)
CONTAINS
  PROCEDURE(REPRES_FREE_IF), PASS, PUBLIC, DEFERRED :: FREE
END TYPE

ABSTRACT INTERFACE
PP_THREAD_SAFE FUNCTION REPRES_FREE_IF( REPRES, HOOKS ) RESULT(RET)
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  IMPORT :: REPRES_A
IMPLICIT NONE
  CLASS(REPRES_A), INTENT(INOUT) :: REPRES
  TYPE(HOOKS_T),   INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION REPRES_FREE_IF
END INTERFACE


TYPE, EXTENDS(REPRES_A) :: REDUCED_GG_T
  INTEGER(KIND=JPIB_K) :: TRUNCATE_DEGREES=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: NUMBER_OF_POINTS_ALONG_A_MERIDIAN=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: NUMBER_OF_PARALLELS_BETWEEN_POLE_AND_EQUATOR=0_JPIB_K
  REAL(KIND=JPRD_K) :: LAT_FIRST_GP_DEG=0.0_JPRD_K
  REAL(KIND=JPRD_K) :: LON_FIRST_GP_DEG=0.0_JPRD_K
  REAL(KIND=JPRD_K) :: LAT_LAST_GP_DEG=0.0_JPRD_K
  REAL(KIND=JPRD_K) :: LON_LAST_GP_DEG=0.0_JPRD_K
  LOGICAL :: TO_BE_DEALLOCATED=.FALSE.
  INTEGER(KIND=JPIB_K), DIMENSION(:), POINTER :: PL => NULL()
CONTAINS
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => REDUCED_GG_FREE
END TYPE

TYPE, EXTENDS(REPRES_A) :: REGULAR_GG_T
  INTEGER(KIND=JPIB_K) :: TRUNCATE_DEGREES=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: NUMBER_OF_POINTS_ALONG_A_MERIDIAN=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: NUMBER_OF_POINTS_ALONG_A_PARALLEL=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: NUMBER_OF_PARALLELS_BETWEEN_POLE_AND_EQUATOR=0_JPIB_K
  REAL(KIND=JPRD_K) :: LAT_FIRST_GP_DEG=0.0_JPRD_K
  REAL(KIND=JPRD_K) :: LON_FIRST_GP_DEG=0.0_JPRD_K
  REAL(KIND=JPRD_K) :: LAT_LAST_GP_DEG=0.0_JPRD_K
  REAL(KIND=JPRD_K) :: LON_LAST_GP_DEG=0.0_JPRD_K
  REAL(KIND=JPRD_K) :: IDIR_INC=0.0_JPRD_K
CONTAINS
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => REGULAR_GG_FREE
END TYPE


TYPE, EXTENDS(REPRES_A) :: SH_T
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_J=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_K=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_M=0_JPIB_K
CONTAINS
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => SH_FREE
END TYPE

TYPE, EXTENDS(REPRES_A) :: STRETCHED_SH_T
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_J=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_K=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_M=0_JPIB_K
  REAL(KIND=JPRD_K) :: STRETCH_FACTOR=0.0_JPRD_K
CONTAINS
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => STRETCHED_SH_FREE
END TYPE

TYPE, EXTENDS(REPRES_A) :: STRETCHED_ROTATED_SH_T
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_J=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_K=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_M=0_JPIB_K
  REAL(KIND=JPRD_K) :: STRETCH_FACTOR=0.0_JPRD_K
  REAL(KIND=JPRD_K) :: LAT_STRET_DEG=0.0_JPRD_K
  REAL(KIND=JPRD_K) :: LON_STRET_DEG=0.0_JPRD_K
CONTAINS
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => STRETCHED_ROTATED_SH_FREE
END TYPE


!> Whitelist of public symbols
PUBLIC :: REPRES_A

! Fields defined in IFS
PUBLIC :: REDUCED_GG_T
PUBLIC :: REGULAR_GG_T
PUBLIC :: STRETCHED_ROTATED_SH_T
PUBLIC :: STRETCHED_SH_T
PUBLIC :: SH_T

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REDUCED_GG_FREE'
PP_THREAD_SAFE FUNCTION REDUCED_GG_FREE( REPRES, HOOKS ) RESULT(RET)

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
  CLASS(REDUCED_GG_T), INTENT(INOUT) :: REPRES
  TYPE(HOOKS_T),       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error Flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=0_JPIB_K

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

  IF ( REPRES%TO_BE_DEALLOCATED ) THEN
    DEALLOCATE( REPRES%PL, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  ENDIF
  NULLIFY(  REPRES%PL )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate the pointer' )
      IF (ALLOCATED(ERRMSG)) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION REDUCED_GG_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REGULAR_GG_FREE'
PP_THREAD_SAFE FUNCTION REGULAR_GG_FREE( REPRES, HOOKS ) RESULT(RET)

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
  CLASS(REGULAR_GG_T), INTENT(INOUT) :: REPRES
  TYPE(HOOKS_T),       INTENT(INOUT) :: HOOKS

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

  ! Nothing to be done for now

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION REGULAR_GG_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SH_FREE'
PP_THREAD_SAFE FUNCTION SH_FREE( REPRES, HOOKS ) RESULT(RET)

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
  CLASS(SH_T),   INTENT(INOUT) :: REPRES
  TYPE(HOOKS_T), INTENT(INOUT) :: HOOKS

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

  ! Nothing to be done for now

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION SH_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRETCHED_SH_FREE'
PP_THREAD_SAFE FUNCTION STRETCHED_SH_FREE( REPRES, HOOKS ) RESULT(RET)

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
  CLASS(STRETCHED_SH_T), INTENT(INOUT) :: REPRES
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

  ! Nothing to be done for now

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION STRETCHED_SH_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRETCHED_ROTATED_SH_FREE'
PP_THREAD_SAFE FUNCTION STRETCHED_ROTATED_SH_FREE( REPRES, HOOKS ) RESULT(RET)

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
  CLASS(STRETCHED_ROTATED_SH_T), INTENT(INOUT) :: REPRES
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

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

  ! Nothing to be done for now

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION STRETCHED_ROTATED_SH_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE REPRESENTATIONS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
