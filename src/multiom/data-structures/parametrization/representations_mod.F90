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
  PROCEDURE(REPRES_TO_YAML_IF), PASS, PUBLIC, DEFERRED :: WRITE_TO_YAML
  PROCEDURE(REPRES_FREE_IF),    PASS, PUBLIC, DEFERRED :: FREE
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

PP_THREAD_SAFE FUNCTION REPRES_TO_YAML_IF( THIS, UNIT, OFFSET, HOOKS ) RESULT(RET)
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  IMPORT :: REPRES_A
IMPLICIT NONE
  CLASS(REPRES_A),      INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION REPRES_TO_YAML_IF
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
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: WRITE_TO_YAML => REDUCED_GG_TO_YAML
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
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: WRITE_TO_YAML => REGULAR_GG_TO_YAML
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => REGULAR_GG_FREE
END TYPE


TYPE, EXTENDS(REPRES_A) :: SH_T
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_J=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_K=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_M=0_JPIB_K
CONTAINS
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: WRITE_TO_YAML => SH_TO_YAML
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => SH_FREE
END TYPE

TYPE, EXTENDS(REPRES_A) :: STRETCHED_SH_T
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_J=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_K=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: PENTAGONAL_RESOLUTIONS_PAR_M=0_JPIB_K
  REAL(KIND=JPRD_K) :: STRETCH_FACTOR=0.0_JPRD_K
CONTAINS
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: WRITE_TO_YAML => STRETCHED_SH_TO_YAML
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
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: WRITE_TO_YAML => STRETCHED_ROTATED_SH_TO_YAML
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
#define PP_PROCEDURE_NAME 'REDUCED_GG_TO_YAML'
PP_THREAD_SAFE FUNCTION REDUCED_GG_TO_YAML( THIS, UNIT, OFFSET, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN
  USE :: LOG_UTILS_MOD,     ONLY: TO_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(REDUCED_GG_T),  INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE :: STR
  INTEGER(KIND=JPIB_K) :: I
  CHARACTER(LEN=32) :: CTMP
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  LOGICAL :: UNIT_OPENED

  ! Error Flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNIT_NOT_OPENED=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IOSTATUS_NOT_ZERO=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_STRING=2_JPIB_K

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

  ! Erro handling
  INQUIRE(UNIT=UNIT, OPENED=UNIT_OPENED)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.UNIT_OPENED, ERRFLAG_UNIT_NOT_OPENED )

  ! Write the representation type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET)//'representation:'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! grid type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'type: "reduced_gg"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! data-representation-type type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'data-representation-type: "'//TRIM(ADJUSTL(THIS%DATA_REPRESENTATION_TYPE))//'"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! grid name
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'name: "'//TRIM(ADJUSTL(THIS%NAME))//'"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! truncated degrees
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%TRUNCATE_DEGREES
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'truncate-degrees: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! number of points along a meridian
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%NUMBER_OF_POINTS_ALONG_A_MERIDIAN
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'number-of-points-along-a-meridian: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! number of parallels between pole and equator
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%NUMBER_OF_PARALLELS_BETWEEN_POLE_AND_EQUATOR
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'number-of-parallels-between-pole-and-equator: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! latitude of first grid point in degrees
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(F11.4)',IOSTAT=WRITE_STAT) THIS%LAT_FIRST_GP_DEG
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'latitude-of-first-grid-point-in-degrees: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! logitude of first grid point in degrees
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(F11.4)',IOSTAT=WRITE_STAT) THIS%LON_FIRST_GP_DEG
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'longitude-of-first-grid-point-in-degrees: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! latitude of last grid point in degrees
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(F11.4)',IOSTAT=WRITE_STAT) THIS%LAT_LAST_GP_DEG
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'latitude-of-last-grid-point-in-degrees: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! logitude of last grid point in degrees
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(F11.4)',IOSTAT=WRITE_STAT) THIS%LON_LAST_GP_DEG
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'longitude-of-last-grid-point-in-degrees: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  IF ( ASSOCIATED(THIS%PL) ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_STRING) TO_STRING( THIS%PL, STR, HOOKS )
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'pl-array: ['
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT, ADVANCE='NO') REPEAT(' ',OFFSET+4)
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )
    DO I = 1, SIZE(STR)
      IF ( I .EQ. SIZE(STR) ) THEN
        WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT, ADVANCE='NO') TRIM(ADJUSTL(STR(I)))
        PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )
      ELSE
        WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT, ADVANCE='NO') TRIM(ADJUSTL(STR(I)))//', '
        PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )
      ENDIF
      IF ( MOD(I-1,10) .EQ. 0 .AND. I.GT.1 .AND. I.LT. SIZE(STR)) THEN
        WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) ' '
        PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )
        WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT, ADVANCE='NO') REPEAT(' ',OFFSET+4)
        PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )
      END IF
    END DO
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) ' '
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//']'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )
  ENDIF

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
    CASE (ERRFLAG_UNIT_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unit not opened' )
    CASE (ERRFLAG_IOSTATUS_NOT_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'IO status not zero' )
    CASE (ERRFLAG_UNABLE_TO_CONVERT_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert the array to string' )
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

END FUNCTION REDUCED_GG_TO_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REGULAR_GG_TO_YAML'
PP_THREAD_SAFE FUNCTION REGULAR_GG_TO_YAML( THIS, UNIT, OFFSET, HOOKS ) RESULT(RET)

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
  CLASS(REGULAR_GG_T),  INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=32) :: CTMP
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  LOGICAL :: UNIT_OPENED

  ! Error Flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNIT_NOT_OPENED=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IOSTATUS_NOT_ZERO=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_STRING=2_JPIB_K

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

  ! Erro handling
  INQUIRE(UNIT=UNIT, OPENED=UNIT_OPENED)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.UNIT_OPENED, ERRFLAG_UNIT_NOT_OPENED )

  ! Write the representation type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET)//'representation:'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! grid type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'type: "regular_gg"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! data-representation-type type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'data-representation-type: "'//TRIM(ADJUSTL(THIS%DATA_REPRESENTATION_TYPE))//'"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! grid name
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'name: "'//TRIM(ADJUSTL(THIS%NAME))//'"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! truncated degrees
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%TRUNCATE_DEGREES
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'truncate-degrees: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! number of points along a meridian
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%NUMBER_OF_POINTS_ALONG_A_MERIDIAN
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'number-of-points-along-a-meridian: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! number of points along a parallel
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%NUMBER_OF_POINTS_ALONG_A_PARALLEL
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'number-of-points-along-a-parallel: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! number of parallels between pole and equator
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%NUMBER_OF_PARALLELS_BETWEEN_POLE_AND_EQUATOR
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'number-of-parallels-between-pole-and-equator: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! latitude of first grid point in degrees
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(F11.4)',IOSTAT=WRITE_STAT) THIS%LAT_FIRST_GP_DEG
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'latitude-of-first-grid-point-in-degrees: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! logitude of first grid point in degrees
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(F11.4)',IOSTAT=WRITE_STAT) THIS%LON_FIRST_GP_DEG
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'longitude-of-first-grid-point-in-degrees: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! latitude of last grid point in degrees
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(F11.4)',IOSTAT=WRITE_STAT) THIS%LAT_LAST_GP_DEG
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'latitude-of-last-grid-point-in-degrees: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! logitude of last grid point in degrees
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(F11.4)',IOSTAT=WRITE_STAT) THIS%LON_LAST_GP_DEG
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'longitude-of-last-grid-point-in-degrees: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! idir increment
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(F11.4)',IOSTAT=WRITE_STAT) THIS%IDIR_INC
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'i-direction-increment: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )


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
    CASE (ERRFLAG_UNIT_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unit not opened' )
    CASE (ERRFLAG_IOSTATUS_NOT_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'IO status not zero' )
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

END FUNCTION REGULAR_GG_TO_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SH_TO_YAML'
PP_THREAD_SAFE FUNCTION SH_TO_YAML( THIS, UNIT, OFFSET, HOOKS ) RESULT(RET)

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
  CLASS(SH_T),          INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=32) :: CTMP
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  LOGICAL :: UNIT_OPENED

  ! Error Flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNIT_NOT_OPENED=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IOSTATUS_NOT_ZERO=1_JPIB_K

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

  ! Erro handling
  INQUIRE(UNIT=UNIT, OPENED=UNIT_OPENED)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.UNIT_OPENED, ERRFLAG_UNIT_NOT_OPENED )

  ! Write the representation type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET)//'representation:'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! grid type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'type: "sh"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! data-representation-type type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'data-representation-type: "'//TRIM(ADJUSTL(THIS%DATA_REPRESENTATION_TYPE))//'"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! grid name
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'name: "'//TRIM(ADJUSTL(THIS%NAME))//'"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! pentagonal resolution parametr j
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%PENTAGONAL_RESOLUTIONS_PAR_J
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'pentagonal-resolution-parameter-j: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! pentagonal resolution parametr k
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%PENTAGONAL_RESOLUTIONS_PAR_K
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'pentagonal-resolution-parameter-k: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! pentagonal resolution parametr m
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%PENTAGONAL_RESOLUTIONS_PAR_M
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'pentagonal-resolution-parameter-m: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )


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
    CASE (ERRFLAG_UNIT_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unit not opened' )
    CASE (ERRFLAG_IOSTATUS_NOT_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'IO status not zero' )
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

END FUNCTION SH_TO_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRETCHED_SH_TO_YAML'
PP_THREAD_SAFE FUNCTION STRETCHED_SH_TO_YAML( THIS, UNIT, OFFSET, HOOKS ) RESULT(RET)

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
  CLASS(STRETCHED_SH_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=32) :: CTMP
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  LOGICAL :: UNIT_OPENED

  ! Error Flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNIT_NOT_OPENED=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IOSTATUS_NOT_ZERO=1_JPIB_K

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

  ! Erro handling
  INQUIRE(UNIT=UNIT, OPENED=UNIT_OPENED)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.UNIT_OPENED, ERRFLAG_UNIT_NOT_OPENED )

  ! Write the representation type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET)//'representation:'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! grid type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'type: "stretched_sh"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! data-representation-type type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'data-representation-type: "'//TRIM(ADJUSTL(THIS%DATA_REPRESENTATION_TYPE))//'"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! grid name
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'name: "'//TRIM(ADJUSTL(THIS%NAME))//'"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! pentagonal resolution parametr j
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%PENTAGONAL_RESOLUTIONS_PAR_J
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'pentagonal-resolution-parameter-j: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! pentagonal resolution parametr k
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%PENTAGONAL_RESOLUTIONS_PAR_K
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'pentagonal-resolution-parameter-k: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! pentagonal resolution parametr m
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%PENTAGONAL_RESOLUTIONS_PAR_M
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'pentagonal-resolution-parameter-m: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! stretch factor
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(F11.4)',IOSTAT=WRITE_STAT) THIS%STRETCH_FACTOR
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'stretch-factor: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

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
    CASE (ERRFLAG_UNIT_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unit not opened' )
    CASE (ERRFLAG_IOSTATUS_NOT_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'IO status not zero' )
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

END FUNCTION STRETCHED_SH_TO_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRETCHED_ROTATED_SH_TO_YAML'
PP_THREAD_SAFE FUNCTION STRETCHED_ROTATED_SH_TO_YAML( THIS, UNIT, OFFSET, HOOKS ) RESULT(RET)

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
  CLASS(STRETCHED_ROTATED_SH_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),          INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),          INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=32) :: CTMP
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  LOGICAL :: UNIT_OPENED

  ! Error Flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNIT_NOT_OPENED=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IOSTATUS_NOT_ZERO=1_JPIB_K

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

  ! Erro handling
  INQUIRE(UNIT=UNIT, OPENED=UNIT_OPENED)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.UNIT_OPENED, ERRFLAG_UNIT_NOT_OPENED )

  ! Write the representation type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET)//'representation:'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! grid type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'type: "stretched_rotated_sh"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! data-representation-type type
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'data-representation-type: "'//TRIM(ADJUSTL(THIS%DATA_REPRESENTATION_TYPE))//'"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! grid name
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'name: "'//TRIM(ADJUSTL(THIS%NAME))//'"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! pentagonal resolution parametr j
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%PENTAGONAL_RESOLUTIONS_PAR_J
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'pentagonal-resolution-parameter-j: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! pentagonal resolution parametr k
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%PENTAGONAL_RESOLUTIONS_PAR_K
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'pentagonal-resolution-parameter-k: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! pentagonal resolution parametr m
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(I8)',IOSTAT=WRITE_STAT) THIS%PENTAGONAL_RESOLUTIONS_PAR_M
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'pentagonal-resolution-parameter-m: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! stretch factor
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(F11.4)',IOSTAT=WRITE_STAT) THIS%STRETCH_FACTOR
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'stretch-factor: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! latitude of streatching pole in degrees
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(F11.4)',IOSTAT=WRITE_STAT) THIS%LAT_STRET_DEG
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'latitude-of-stretching-pole-in-degrees: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

  ! longitude of streatching pole in degrees
  CTMP = REPEAT(' ',32)
  WRITE(CTMP,'(F11.4)',IOSTAT=WRITE_STAT) THIS%LON_STRET_DEG
  WRITE(UNIT,'(A)',IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2)//'logitude-of-stretching-pole-in-degrees: '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_IOSTATUS_NOT_ZERO )

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
    CASE (ERRFLAG_UNIT_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unit not opened' )
    CASE (ERRFLAG_IOSTATUS_NOT_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'IO status not zero' )
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

END FUNCTION STRETCHED_ROTATED_SH_TO_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


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
