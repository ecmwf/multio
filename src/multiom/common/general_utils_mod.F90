! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'general_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GENERAL_UTILS_MOD'
MODULE GENERAL_UTILS_MOD

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!> Verbosity level
LOGICAL :: GVERBOSE=.FALSE.

!> Need to compute field statistics
LOGICAL :: GCOMPUTE_FIELD_STATISTICS=.FALSE.

!> Dr Hook interface for tracing forwarding
INTERFACE
  SUBROUTINE DR_HOOK_DEFAULT8_IF(CDNAME,KSWITCH,PKEY)
    USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
    USE :: DATAKINDS_DEF_MOD, ONLY: JPTR_K
  IMPLICIT NONE
    CHARACTER(LEN=*),     INTENT(IN)    :: CDNAME
    INTEGER(KIND=JPIM_K), INTENT(IN)    :: KSWITCH
    REAL(KIND=JPTR_K),    INTENT(INOUT) :: PKEY
  END SUBROUTINE DR_HOOK_DEFAULT8_IF
END INTERFACE

!> Tracing variables
PROCEDURE(DR_HOOK_DEFAULT8_IF), POINTER :: GDR_HOOK => NULL()
LOGICAL :: GLHOOK = .FALSE.

!> Default error unit
INTEGER(KIND=JPIB_K) :: JPERR_UNIT=ERROR_UNIT
INTEGER(KIND=JPIB_K) :: JPOUT_UNIT=OUTPUT_UNIT

!> Whitelist of public symbols (Interfaces)
PUBLIC :: DR_HOOK_DEFAULT8_IF

!> Whitelist of public symbols (Procedures)
PUBLIC :: ENVVAR_IS_DEFINED
PUBLIC :: READ_ENVVAR
PUBLIC :: REPLACE_ENVVAR_IN_STRING
PUBLIC :: READ_TYPE_FROM_ENV
PUBLIC :: READ_YAML_FROM_ENV
PUBLIC :: CUSTOM_FINDLOC
PUBLIC :: TOLOWER
PUBLIC :: TOUPPER
PUBLIC :: IS_DIRECTORY
PUBLIC :: MAKE_DIRECTORY
PUBLIC :: FILE_SET_PERMISSION
PUBLIC :: NEED_FIT_SPECTRUM
PUBLIC :: GET_VERBOSE
PUBLIC :: SET_VERBOSE
PUBLIC :: GET_COMPUTE_FIELD_STATISTICS
PUBLIC :: SET_COMPUTE_FIELD_STATISTICS
PUBLIC :: SET_DR_HOOK
PUBLIC :: GET_DR_HOOK
PUBLIC :: SET_CUSTOM_UNITS
PUBLIC :: GET_CUSTOM_UNITS

CONTAINS



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GET_COMPUTE_FIELD_STATISTICS'
PP_THREAD_SAFE FUNCTION GET_COMPUTE_FIELD_STATISTICS( LCOMPUTE_FIELD_STATISTICS, HOOKS ) RESULT(RET)

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
  LOGICAL,       INTENT(OUT)   :: LCOMPUTE_FIELD_STATISTICS
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

  !> Set the hook
!$omp critical(NEED_FIELD_STATISTICS)
  LCOMPUTE_FIELD_STATISTICS = GCOMPUTE_FIELD_STATISTICS
!$omp end critical(NEED_FIELD_STATISTICS)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION GET_COMPUTE_FIELD_STATISTICS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SET_COMPUTE_FIELD_STATISTICS'
PP_THREAD_SAFE FUNCTION SET_COMPUTE_FIELD_STATISTICS( LCOMPUTE_FIELD_STATISTICS, HOOKS ) RESULT(RET)

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
  LOGICAL,       INTENT(IN)    :: LCOMPUTE_FIELD_STATISTICS
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

  !> Set the hook
!$omp critical(NEED_FIELD_STATISTICS)
  GCOMPUTE_FIELD_STATISTICS = LCOMPUTE_FIELD_STATISTICS
!$omp end critical(NEED_FIELD_STATISTICS)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION SET_COMPUTE_FIELD_STATISTICS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GET_VERBOSE'
PP_THREAD_SAFE FUNCTION GET_VERBOSE( VERBOSE, HOOKS ) RESULT(RET)

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
  LOGICAL,       INTENT(OUT)   :: VERBOSE
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

  !> Set the hook
!$omp critical(ACCESS_VERBOSITY)
  VERBOSE = GVERBOSE
!$omp end critical(ACCESS_VERBOSITY)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION GET_VERBOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SET_VERBOSE'
PP_THREAD_SAFE FUNCTION SET_VERBOSE( VERBOSE, HOOKS ) RESULT(RET)

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
  LOGICAL,       INTENT(IN)    :: VERBOSE
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

  !> Set the hook
!$omp critical(ACCESS_VERBOSITY)
  GVERBOSE = VERBOSE
!$omp end critical(ACCESS_VERBOSITY)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION SET_VERBOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SET_DR_HOOK'
PP_THREAD_SAFE FUNCTION SET_DR_HOOK( LHOOK, DR_HOOK_PROCEDURE, HOOKS ) RESULT(RET)

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
  LOGICAL,                                 INTENT(IN) :: LHOOK
  PROCEDURE(DR_HOOK_DEFAULT8_IF), POINTER, INTENT(IN) :: DR_HOOK_PROCEDURE
  TYPE(HOOKS_T),                           INTENT(INOUT) :: HOOKS

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

  !> Set the hook
!$omp critical(MANAGE_DR_HOOK)
  GLHOOK  = LHOOK
  GDR_HOOK => DR_HOOK_PROCEDURE
!$omp end critical(MANAGE_DR_HOOK)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION SET_DR_HOOK
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GET_DR_HOOK'
PP_THREAD_SAFE FUNCTION GET_DR_HOOK( LHOOK, DR_HOOK_PROCEDURE, HOOKS ) RESULT(RET)

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
  LOGICAL,                                 INTENT(OUT)   :: LHOOK
  PROCEDURE(DR_HOOK_DEFAULT8_IF), POINTER, INTENT(OUT)   :: DR_HOOK_PROCEDURE
  TYPE(HOOKS_T),                           INTENT(INOUT) :: HOOKS

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

  !> Set the hook
!$omp critical(MANAGE_DR_HOOK)
  LHOOK  = GLHOOK
  DR_HOOK_PROCEDURE => GDR_HOOK
!$omp end critical(MANAGE_DR_HOOK)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION GET_DR_HOOK
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SET_CUSTOM_UNITS'
PP_THREAD_SAFE FUNCTION SET_CUSTOM_UNITS( CUSTOM_OUTPUT_UNIT, CUSTOM_ERROR_UNIT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIM_K), INTENT(IN)    :: CUSTOM_OUTPUT_UNIT
  INTEGER(KIND=JPIM_K), INTENT(IN)    :: CUSTOM_ERROR_UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

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

!$omp critical(MANAGE_CUSTOM_UNITS)
  JPERR_UNIT = CUSTOM_ERROR_UNIT
  JPOUT_UNIT = CUSTOM_OUTPUT_UNIT
!$omp end critical(MANAGE_CUSTOM_UNITS)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION SET_CUSTOM_UNITS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GET_CUSTOM_UNITS'
PP_THREAD_SAFE FUNCTION GET_CUSTOM_UNITS( CUSTOM_OUTPUT_UNIT, CUSTOM_ERROR_UNIT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIM_K), INTENT(OUT)   :: CUSTOM_OUTPUT_UNIT
  INTEGER(KIND=JPIM_K), INTENT(OUT)   :: CUSTOM_ERROR_UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

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

!$omp critical(MANAGE_CUSTOM_UNITS)
  CUSTOM_ERROR_UNIT = JPERR_UNIT
  CUSTOM_OUTPUT_UNIT = JPOUT_UNIT
!$omp end critical(MANAGE_CUSTOM_UNITS)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION GET_CUSTOM_UNITS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NEED_FIT_SPECTRUM'
PP_THREAD_SAFE FUNCTION NEED_FIT_SPECTRUM( NSMAX, LNFT, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: NSMAX
  LOGICAL,              INTENT(OUT)   :: LNFT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

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

  ! Check if the number of spectral coefficients is greater than 10
  LNFT = NSMAX .GT. 10

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION NEED_FIT_SPECTRUM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FMODE2CMODE'
FUNCTION FMODE2CMODE( F_MODE, C_MODE, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR

  !> Symbols imported from other modules within the project.
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
  CHARACTER(LEN=9),                            INTENT(IN)    :: F_MODE
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(10), INTENT(OUT)   :: C_MODE
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRLFAG_INVALID_MODE = 1_JPIB_K

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

  ! HAndle permission (Case insenistive whitelist)
  SELECT CASE( F_MODE(1:1) )
  CASE ( 'R', 'r' )
    C_MODE(1) = 'r'
  CASE ( '-' )
    C_MODE(1) = '-'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRLFAG_INVALID_MODE )
  END SELECT
  SELECT CASE( F_MODE(2:2) )
  CASE ( 'W', 'w' )
    C_MODE(2) = 'w'
  CASE ( '-' )
    C_MODE(2) = '-'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRLFAG_INVALID_MODE )
  END SELECT
  SELECT CASE( F_MODE(3:3) )
  CASE ( 'X', 'x' )
    C_MODE(3) = 'x'
  CASE ( '-' )
    C_MODE(3) = '-'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRLFAG_INVALID_MODE )
  END SELECT

  SELECT CASE( F_MODE(4:4) )
  CASE ( 'R', 'r' )
    C_MODE(4) = 'r'
  CASE ( '-' )
    C_MODE(4) = '-'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRLFAG_INVALID_MODE )
  END SELECT
  SELECT CASE( F_MODE(5:5) )
  CASE ( 'W', 'w' )
    C_MODE(5) = 'w'
  CASE ( '-' )
    C_MODE(5) = '-'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRLFAG_INVALID_MODE )
  END SELECT
  SELECT CASE( F_MODE(6:6) )
  CASE ( 'X', 'x' )
    C_MODE(6) = 'x'
  CASE ( '-' )
    C_MODE(6) = '-'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRLFAG_INVALID_MODE )
  END SELECT

  SELECT CASE( F_MODE(7:7) )
  CASE ( 'R', 'r' )
    C_MODE(7) = 'r'
  CASE ( '-' )
    C_MODE(7) = '-'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRLFAG_INVALID_MODE )
  END SELECT
  SELECT CASE( F_MODE(8:8) )
  CASE ( 'W', 'w' )
    C_MODE(8) = 'w'
  CASE ( '-' )
    C_MODE(8) = '-'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRLFAG_INVALID_MODE )
  END SELECT
  SELECT CASE( F_MODE(9:9) )
  CASE ( 'X', 'x' )
    C_MODE(9) = 'x'
  CASE ( '-' )
    C_MODE(9) = '-'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRLFAG_INVALID_MODE )
  END SELECT
  C_MODE(10) = C_NULL_CHAR


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

    CASE (ERRLFAG_INVALID_MODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invlid rquested mode' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'mode name: "'//TRIM(ADJUSTL(F_MODE))//'"' )

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

  ! Exit point (on error)
  RETURN

END FUNCTION FMODE2CMODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IS_DIRECTORY'
FUNCTION IS_DIRECTORY( DIRECTORY_NAME, IS_DIR, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR

  !> Symbols imported from other modules within the project.
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
  CHARACTER(LEN=*), INTENT(IN)    :: DIRECTORY_NAME
  LOGICAL,          INTENT(OUT)   :: IS_DIR
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_INT)  :: C_STATUS
  CHARACTER(KIND=C_CHAR,LEN=LEN_TRIM(DIRECTORY_NAME)+1), TARGET :: C_DIRECTORY_NAME

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CHECK_DIRECTORY = 1_JPIB_K

  ! Explicit interfaces
  INTERFACE
    FUNCTION C_ISDIR( C_PATH ) RESULT(C_STATUS) BIND(C, NAME="is_directory")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR),  VALUE, INTENT(IN) :: C_PATH
      INTEGER(C_INT) :: C_STATUS
    END FUNCTION C_ISDIR
  END INTERFACE

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

  ! Fill the c character
  C_DIRECTORY_NAME = REPEAT(C_NULL_CHAR,LEN_TRIM(DIRECTORY_NAME)+1)
  DO I = 1, LEN_TRIM(DIRECTORY_NAME)
      C_DIRECTORY_NAME(I:I) = DIRECTORY_NAME(I:I)
  ENDDO

  ! Call the C function
  C_STATUS = C_ISDIR( C_LOC(C_DIRECTORY_NAME) )

  ! Assign the output flag
  IF ( C_STATUS .EQ. 1 ) THEN
    IS_DIR = .TRUE.
  ELSE
    IS_DIR = .FALSE.
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

    CASE (ERRFLAG_UNABLE_TO_CHECK_DIRECTORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check directory' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'drectory name: "'//TRIM(ADJUSTL(DIRECTORY_NAME))//'"' )

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

  ! Exit point (on error)
  RETURN

END FUNCTION IS_DIRECTORY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE







#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_DIRECTORY'
FUNCTION MAKE_DIRECTORY( F_DIRECTORY_NAME, F_MODE, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR

  !> Symbols imported from other modules within the project.
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
  CHARACTER(LEN=*), INTENT(IN)    :: F_DIRECTORY_NAME
  CHARACTER(LEN=9), INTENT(IN)    :: F_MODE
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_INT)  :: C_STATUS
  CHARACTER(KIND=C_CHAR,LEN=LEN_TRIM(F_DIRECTORY_NAME)+1), TARGET :: C_DIRECTORY_NAME
  CHARACTER(KIND=C_CHAR,LEN=1), DIMENSION(10), TARGET :: C_MODE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRLFAG_INVALID_MODE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_DIRECTORY = 2_JPIB_K

  ! Explicit interfaces
  INTERFACE
    FUNCTION C_MKDIR( C_PATH, C_MODE ) RESULT(C_STATUS) BIND(C, NAME="make_directory_sym")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR),  VALUE, INTENT(IN) :: C_PATH
      TYPE(C_PTR), VALUE, INTENT(IN) :: C_MODE
      INTEGER(C_INT) :: C_STATUS
    END FUNCTION C_MKDIR
  END INTERFACE

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

  ! Fill the c character
  C_DIRECTORY_NAME = REPEAT(C_NULL_CHAR,LEN_TRIM(F_DIRECTORY_NAME)+1)
  DO I = 1, LEN_TRIM(F_DIRECTORY_NAME)
      C_DIRECTORY_NAME(I:I) = F_DIRECTORY_NAME(I:I)
  ENDDO

  ! Handle permission (Case insenistive whitelist)
  PP_TRYCALL(ERRLFAG_INVALID_MODE) FMODE2CMODE( F_MODE, C_MODE, HOOKS )

  ! Call the C function
  C_STATUS = C_MKDIR( C_LOC(C_DIRECTORY_NAME), C_LOC(C_MODE) )
  PP_DEBUG_CRITICAL_COND_THROW( C_STATUS.NE.0, ERRFLAG_UNABLE_TO_CREATE_DIRECTORY )

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

    CASE (ERRLFAG_INVALID_MODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invlid mode in directory creation' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'drectory name: "'//TRIM(ADJUSTL(F_DIRECTORY_NAME))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'drectory mode: "'//TRIM(ADJUSTL(F_MODE))//'"' )

    CASE (ERRFLAG_UNABLE_TO_CREATE_DIRECTORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create directory' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'drectory name: "'//TRIM(ADJUSTL(F_DIRECTORY_NAME))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'drectory mode: "'//TRIM(ADJUSTL(F_MODE))//'"' )
      IF ( C_STATUS .EQ. 1_C_INT ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to parse symbolic mode' )
      ELSEIF ( C_STATUS .EQ. 2_C_INT ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error calling "mkdir"' )
      ENDIF
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

  ! Exit point (on error)
  RETURN

END FUNCTION MAKE_DIRECTORY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FILE_SET_PERMISSION'
FUNCTION FILE_SET_PERMISSION( F_FILE_NAME, F_MODE, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR

  !> Symbols imported from other modules within the project.
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
  CHARACTER(LEN=*), INTENT(IN)    :: F_FILE_NAME
  CHARACTER(LEN=9), INTENT(IN)    :: F_MODE
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_INT)  :: C_STATUS
  CHARACTER(KIND=C_CHAR,LEN=LEN_TRIM(F_FILE_NAME)+1), TARGET :: C_FILE_NAME
  CHARACTER(KIND=C_CHAR,LEN=1), DIMENSION(10), TARGET :: C_MODE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRLFAG_INVALID_MODE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_FILE_PERMISSION = 2_JPIB_K

  ! Explicit interfaces
  INTERFACE
    FUNCTION C_SET_PERM( C_PATH, C_MODE ) RESULT(C_STATUS) BIND(C, NAME="set_perm")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR),  VALUE, INTENT(IN) :: C_PATH
      TYPE(C_PTR), VALUE, INTENT(IN) :: C_MODE
      INTEGER(C_INT) :: C_STATUS
    END FUNCTION C_SET_PERM
  END INTERFACE

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

  ! Fill the c character
  C_FILE_NAME = REPEAT(C_NULL_CHAR,LEN_TRIM(F_FILE_NAME)+1)
  DO I = 1, LEN_TRIM(F_FILE_NAME)
      C_FILE_NAME(I:I) = F_FILE_NAME(I:I)
  ENDDO

  ! Handle permission (Case insenistive whitelist)
  PP_TRYCALL(ERRLFAG_INVALID_MODE) FMODE2CMODE( F_MODE, C_MODE, HOOKS )

  ! Call the C function
  C_STATUS = C_SET_PERM( C_LOC(C_FILE_NAME), C_LOC(C_MODE) )
  PP_DEBUG_CRITICAL_COND_THROW( C_STATUS.NE.0, ERRFLAG_UNABLE_TO_SET_FILE_PERMISSION )

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

    CASE (ERRLFAG_INVALID_MODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invlid mode in directory creation' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'drectory name: "'//TRIM(ADJUSTL(F_FILE_NAME))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'drectory mode: "'//TRIM(ADJUSTL(F_MODE))//'"' )

    CASE (ERRFLAG_UNABLE_TO_SET_FILE_PERMISSION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create directory' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'drectory name: "'//TRIM(ADJUSTL(F_FILE_NAME))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'drectory mode: "'//TRIM(ADJUSTL(F_MODE))//'"' )
      IF ( C_STATUS .EQ. 1_C_INT ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to parse symbolic mode' )
      ELSEIF ( C_STATUS .EQ. 2_C_INT ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error calling "chmod"' )
      ENDIF
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

  ! Exit point (on error)
  RETURN

END FUNCTION FILE_SET_PERMISSION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REPLACE_ENVVAR_IN_STRING'
PP_THREAD_SAFE FUNCTION REPLACE_ENVVAR_IN_STRING( INPSTR, OUTSTR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
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
  CHARACTER(LEN=*), INTENT(IN)    :: INPSTR
  CHARACTER(LEN=*), INTENT(OUT)   :: OUTSTR
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=1024)  :: TMPSTR
  CHARACTER(LEN=1024)  :: ENVVAR
  INTEGER(KIND=JPIB_K) :: ELEN
  INTEGER(KIND=JPIM_K) :: ISRC
  INTEGER(KIND=JPIM_K) :: IDST
  INTEGER(KIND=JPIM_K) :: ITMP
  INTEGER(KIND=JPIM_K) :: N
  INTEGER(KIND=JPIM_K) :: M
  INTEGER(KIND=JPIM_K) :: Q
  INTEGER(KIND=JPIM_K) :: STAT
  LOGICAL :: IS_DEFINED

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS_INPSTRING=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS_TMPSTRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS_OUTSTRING=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISDEFINED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENVVAR_NOT_DEFINED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS_ENVVAR=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_READ_ENVVAR=7_JPIB_K

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

  ! Initialization of local variables
  N=LEN_TRIM(INPSTR)
  M=LEN(OUTSTR)
  Q=LEN(TMPSTR)
  OUTSTR = REPEAT(' ',M)

  ! Loop
  ISRC = 0
  IDST = 0
  Forward: DO

    ! Increment source index
    ISRC = ISRC + 1

    ! Exit condition
    IF ( ISRC .GT. N ) THEN
      EXIT Forward
    ENDIF

    ! Check for substitution
    IF ( INPSTR(ISRC:ISRC) .EQ. '{' ) THEN
      TMPSTR=REPEAT(' ',1024)
      ITMP = 0
      ReadEnv: DO
        ISRC = ISRC + 1
        PP_DEBUG_DEVELOP_COND_THROW( ISRC .GT. N, ERRFLAG_OUT_OF_BOUNDS_INPSTRING )
        IF ( INPSTR(ISRC:ISRC) .EQ. '}' ) THEN
          EXIT ReadEnv
        ELSE
          ITMP = ITMP + 1
          PP_DEBUG_DEVELOP_COND_THROW( ITMP .GT. Q,  ERRFLAG_OUT_OF_BOUNDS_TMPSTRING )
          TMPSTR(ITMP:ITMP) = INPSTR(ISRC:ISRC)
        ENDIF
      ENDDO ReadEnv

      ! Check if the environment variable exists
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISDEFINED) ENVVAR_IS_DEFINED( TMPSTR, IS_DEFINED, HOOKS, NDLEN=ELEN )
      PP_DEBUG_DEVELOP_COND_THROW( .NOT.IS_DEFINED, ERRFLAG_ENVVAR_NOT_DEFINED )
      PP_DEBUG_DEVELOP_COND_THROW( ELEN.GT.LEN(ENVVAR), ERRFLAG_OUT_OF_BOUNDS_ENVVAR )

      ! Read Environment variable
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_READ_ENVVAR) READ_ENVVAR( TMPSTR, ENVVAR, ELEN, HOOKS )

      ! Forward environment variable
      DO ITMP = 1, LEN_TRIM(ENVVAR)
        IDST = IDST + 1
        PP_DEBUG_DEVELOP_COND_THROW( IDST .GT. M,  ERRFLAG_OUT_OF_BOUNDS_OUTSTRING )
        OUTSTR(IDST:IDST) = ENVVAR(ITMP:ITMP)
      ENDDO
    ELSE
      IDST = IDST + 1
      PP_DEBUG_DEVELOP_COND_THROW( IDST .GT. M,  ERRFLAG_OUT_OF_BOUNDS_OUTSTRING )
      OUTSTR(IDST:IDST) = INPSTR(ISRC:ISRC)
    ENDIF

  ENDDO Forward

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_OUT_OF_BOUNDS_INPSTRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Hit end of string while searching closed bracket' )
    CASE (ERRFLAG_OUT_OF_BOUNDS_TMPSTRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Temporary string too short' )
    CASE (ERRFLAG_OUT_OF_BOUNDS_OUTSTRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Destination string too short' )
    CASE (ERRFLAG_ENVVAR_NOT_DEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Environment variable is not defined' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Var. name: "'//TRIM(ADJUSTL(TMPSTR))//'"' )
    CASE (ERRFLAG_OUT_OF_BOUNDS_ENVVAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Environment variable too short' )
    CASE (ERRFLAG_UNABLE_TO_CALL_ISDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error calling: "ENVVAR_IS_DEFINED"' )
    CASE (ERRFLAG_UNABLE_TO_CALL_READ_ENVVAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error calling: "READ_ENVVAR"' )
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

END FUNCTION REPLACE_ENVVAR_IN_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENVVAR_IS_DEFINED'
PP_THREAD_SAFE FUNCTION ENVVAR_IS_DEFINED( CDENVVARNAME, LDIS_DEFINED, HOOKS, NDLEN) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
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
  CHARACTER(LEN=*),               INTENT(IN)    :: CDENVVARNAME
  LOGICAL,                        INTENT(OUT)   :: LDIS_DEFINED
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: NDLEN

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: NLLEN
  INTEGER(KIND=JPIM_K) :: STAT
  INTEGER(KIND=JPIM_K) :: NLLEN4

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

  ! Check if the environment variable is readable
  CALL GET_ENVIRONMENT_VARIABLE( CDENVVARNAME, LENGTH=NLLEN4, STATUS=STAT )

  ! Read Output Manager Type
  IF ( STAT .EQ. 0 ) THEN

    NLLEN = NLLEN4
    LDIS_DEFINED = .TRUE.

  ELSE

    NLLEN = NLLEN4
    LDIS_DEFINED = .FALSE.

  ENDIF

  ! Optional arguments
  IF ( PRESENT(NDLEN) ) THEN
    NDLEN = NLLEN
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION ENVVAR_IS_DEFINED
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_ENVVAR'
PP_THREAD_SAFE FUNCTION READ_ENVVAR( CDENVVARNAME, CDENVVARVAL, NDLEN, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
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
  CHARACTER(LEN=*),     INTENT(IN)    :: CDENVVARNAME
  CHARACTER(LEN=*),     INTENT(OUT)   :: CDENVVARVAL
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: NDLEN
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: STAT
  INTEGER(KIND=JPIM_K) :: NDLEN4

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_ENVVAR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_TOO_SHORT=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FAILED_TO_READ_ENVVAR=3_JPIB_K

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

  ! Check if the environment variable is readable
  CALL GET_ENVIRONMENT_VARIABLE( CDENVVARNAME, LENGTH=NDLEN4, STATUS=STAT )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_ENVVAR )
  PP_DEBUG_CRITICAL_COND_THROW( LEN(CDENVVARVAL) .LT. NDLEN4, ERRFLAG_OUTPUT_TOO_SHORT )

  ! Initialize the variable
  CDENVVARVAL = REPEAT(' ',LEN(CDENVVARVAL))

  ! Read the environment variable
  CALL GET_ENVIRONMENT_VARIABLE( CDENVVARNAME, VALUE=CDENVVARVAL, STATUS=STAT )
  NDLEN = NDLEN4

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_FAILED_TO_READ_ENVVAR )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_UNABLE_TO_READ_ENVVAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Impossible to read environment variable: '//TRIM(CDENVVARNAME) )
    CASE (ERRFLAG_OUTPUT_TOO_SHORT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output variable too short for the environment variable' )
    CASE (ERRFLAG_FAILED_TO_READ_ENVVAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to read environment variable: '//TRIM(CDENVVARNAME) )
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

END FUNCTION READ_ENVVAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Retrieves the output manager type from the 'OUTPUT_MANAGER_YAML' environment variable.
!>
!> This function reads the 'OUTPUT_MANAGER_YAML' environment variable to determine the configure
!> the main YAML configuration file. If the variable is not defined, the default value
!> '../output-manager-config.yaml' is assumed.
!>
!> @attention the folder is "../" because by default each instance of the output manager run in
!>            the folder calles io_serv.<procId>.d
!>
!> @param [out] OMYAML Name of the main YAML configuraiton file
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_YAML_FROM_ENV'
PP_THREAD_SAFE FUNCTION READ_YAML_FROM_ENV( OMYAML, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*), INTENT(OUT)   :: OMYAML
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: NENVLN
  INTEGER(KIND=JPIB_K) :: STAT
  LOGICAL :: IS_DEFINED
  LOGICAL :: EX

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENVVAR_TOO_LONG = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CHECK_ENVVAR = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_ENVVAR = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_YAML_FILE_DOES_NOT_EXISTS = 4_JPIB_K

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

  ! Initialise the YAML name
  OMYAML = REPEAT(' ',LEN(OMYAML))

  ! Read Output Manager Type
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CHECK_ENVVAR) ENVVAR_IS_DEFINED( 'OUTPUT_MANAGER_YAML', IS_DEFINED, HOOKS, NDLEN=NENVLN )
  PP_DEBUG_DEVELOP_COND_THROW( (NENVLN.GT.LEN(OMYAML)), ERRFLAG_ENVVAR_TOO_LONG )

  ! Read Output Manager Type
  IF ( IS_DEFINED ) THEN

    ! Read the environment variable
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_ENVVAR) READ_ENVVAR( 'OUTPUT_MANAGER_YAML', OMYAML, NENVLN, HOOKS )

    ! Check if the file exsts
    INQUIRE( FILE=TRIM(OMYAML), EXIST=EX )
    PP_DEBUG_DEVELOP_COND_THROW( .NOT.EX, ERRFLAG_YAML_FILE_DOES_NOT_EXISTS )

  ELSE

    ! Default value for output manager type when environment variable
    ! is not defined.
    ! "../" Because the output manager "main" directory is: "io_serv.0000?.d"
    OMYAML = '../output-manager-config.yaml'

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_ENVVAR_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'OUTPUT_MANAGER_YAML env. var. too long' )
    CASE (ERRFLAG_UNABLE_TO_CHECK_ENVVAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error checking: "OUTPUT_MANAGER_YAML" env. var.' )
    CASE (ERRFLAG_UNABLE_TO_READ_ENVVAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading: "OUTPUT_MANAGER_YAML" env. var.' )
    CASE (ERRFLAG_YAML_FILE_DOES_NOT_EXISTS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'YAML file does not exists' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION READ_YAML_FROM_ENV
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Retrieves the output manager type from the 'OUTPUT_MANAGER_TYPE' environment variable.
!>
!> This function reads the 'OUTPUT_MANAGER_TYPE' environment variable to determine the desired
!> type of output manager to be constructed. If the variable is not defined, the default value
!> 'NOOP' is assumed.
!>
!> @return The name of the output manager to be implemented.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_TYPE_FROM_ENV'
PP_THREAD_SAFE FUNCTION READ_TYPE_FROM_ENV( OMTYPE, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*), INTENT(OUT)   :: OMTYPE
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=LEN(OMTYPE)) :: LOC_OMTYPE
  INTEGER(KIND=JPIB_K) :: NENVLN
  INTEGER(KIND=JPIB_K) :: STAT
  LOGICAL :: IS_DEFINED

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENVVAR_TOO_LONG = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CHECK_ENVVAR = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_ENVVAR = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_UPPERCASE = 4_JPIB_K

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

  ! Initialise the YAML name
  OMTYPE = REPEAT(' ',LEN(OMTYPE))
  LOC_OMTYPE = REPEAT(' ',LEN(OMTYPE))

  ! Read Output Manager Type
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CHECK_ENVVAR) ENVVAR_IS_DEFINED( 'OUTPUT_MANAGER_TYPE', IS_DEFINED, HOOKS, NDLEN=NENVLN )
  PP_DEBUG_DEVELOP_COND_THROW( (NENVLN.GT.LEN(OMTYPE)), ERRFLAG_ENVVAR_TOO_LONG )

  ! Read Output Manager Type
  IF ( IS_DEFINED ) THEN

    ! Read the environment variable
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_ENVVAR) READ_ENVVAR( 'OUTPUT_MANAGER_TYPE', LOC_OMTYPE, NENVLN, HOOKS )

    ! Convert to uppercase the environment variable
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_UPPERCASE)  TOUPPER( LOC_OMTYPE, OMTYPE, HOOKS )

  ELSE

    ! Default value for output manager type when environment variable is not defined
    OMTYPE = 'NOOP'

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_ENVVAR_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'OUTPUT_MANAGER_TYPE env. var. too long' )
    CASE (ERRFLAG_UNABLE_TO_CHECK_ENVVAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error checking: "OUTPUT_MANAGER_TYPE" env. var.' )
    CASE (ERRFLAG_UNABLE_TO_READ_ENVVAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading: "OUTPUT_MANAGER_TYPE" env. var.' )
    CASE (ERRFLAG_UNABLE_TO_CONVERT_UPPERCASE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error converting to uppercase' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION READ_TYPE_FROM_ENV
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CUSTOM_FINDLOC'
PP_THREAD_SAFE FUNCTION CUSTOM_FINDLOC( DAT, REF, LOC, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), DIMENSION(:), INTENT(IN)    :: DAT
  INTEGER(KIND=JPIB_K),               INTENT(IN)    :: REF
  INTEGER(KIND=JPIB_K),               INTENT(OUT)   :: LOC
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

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

  LOC = 0_JPIB_K
  SearchLoop: DO I = 1, SIZE(DAT)
    IF ( DAT(I) .EQ. REF ) THEN
      LOC = I
      EXIT SearchLoop
    ENDIF
  ENDDO SearchLoop

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION CUSTOM_FINDLOC
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOLOWER'
PP_THREAD_SAFE FUNCTION TOLOWER( INPSTRING, OUTSTRING, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*), INTENT(IN)    :: INPSTRING
  CHARACTER(LEN=*), INTENT(OUT)   :: OUTSTRING
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: ASCIIVALUE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS_OUTSTRING = 1_JPIB_K

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

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(INPSTRING) .GT. LEN(OUTSTRING), ERRFLAG_OUT_OF_BOUNDS_OUTSTRING )

  ! Convert each character to lower
  DO I = 1, LEN_TRIM(INPSTRING)
    ASCIIVALUE = ICHAR(INPSTRING(I:I))
    IF (ASCIIVALUE .GE. ICHAR('A') .AND. ASCIIVALUE .LE. ICHAR('Z')) THEN
      OUTSTRING(I:I) = CHAR(ASCIIVALUE - ICHAR('A') + ICHAR('a'))
    ELSE
      OUTSTRING(I:I) = INPSTRING(I:I)
    END IF
  END DO

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
    CASE (ERRFLAG_OUT_OF_BOUNDS_OUTSTRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output string too short' )
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

END FUNCTION TOLOWER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOUPPER'
PP_THREAD_SAFE FUNCTION TOUPPER( INPSTRING, OUTSTRING, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*), INTENT(IN)    :: INPSTRING
  CHARACTER(LEN=*), INTENT(OUT)   :: OUTSTRING
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: ASCIIVALUE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS_OUTSTRING = 1_JPIB_K

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

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(INPSTRING) .GT. LEN(OUTSTRING), ERRFLAG_OUT_OF_BOUNDS_OUTSTRING )

  ! Convert each character to lower
  DO I = 1, LEN_TRIM(INPSTRING)
    ASCIIVALUE = ICHAR(INPSTRING(I:I))
    IF (ASCIIVALUE .GE. ICHAR('a') .AND. ASCIIVALUE .LE. ICHAR('z')) THEN
      OUTSTRING(I:I) = CHAR(ASCIIVALUE - ICHAR('a') + ICHAR('A'))
    ELSE
      OUTSTRING(I:I) = INPSTRING(I:I)
    END IF
  END DO

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
    CASE (ERRFLAG_OUT_OF_BOUNDS_OUTSTRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output string too short' )
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

END FUNCTION TOUPPER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GENERAL_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
