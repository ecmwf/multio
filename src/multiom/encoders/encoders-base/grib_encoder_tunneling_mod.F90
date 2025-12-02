! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib_encoder_tunneling_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_ENCODER_TUNNELING_MOD'
MODULE GRIB_ENCODER_TUNNELING_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,         ONLY: JPIB_K
  USE :: GRIB_ENCODER_REGISTER_MOD, ONLY: GRIB_SECTION_REGISTER_T
  USE :: HOOKS_MOD,                 ONLY: HOOKS_T

IMPLICIT NONE

  !> This module is used to pass non c interoperable data to the classes

  !> Default visibility of the module
  PRIVATE

  ! Datatype used to share information
  TYPE :: TUNNELING_INFO_T

    !> Pointer to the encoder options
    ! TYPE(GRIB_SECTION_REGISTER_T), POINTER :: PVT_SECTION_REGISTER => NULL()

    !> Pointer to the hooks
    TYPE(HOOKS_T), POINTER :: PVT_HOOKS => NULL()

  END TYPE


  ! Type to store the encoder options
  TYPE(GRIB_SECTION_REGISTER_T), TARGET :: PVT_SECTION_REGISTER
  TYPE(TUNNELING_INFO_T), ALLOCATABLE, DIMENSION(:) :: PVT_TUNNELING_INFO

  !> Whitelist of public symbols
  PUBLIC :: INIT_TUNNELING_INFO
  PUBLIC :: SET_TUNNELING_INFO
  PUBLIC :: GET_TUNNELING_INFO
  PUBLIC :: GET_SECTIONS_REGISTER
  PUBLIC :: FREE_TUNNELING_INFO

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INIT_TUNNELING_INFO'
PP_THREAD_SAFE FUNCTION INIT_TUNNELING_INFO( HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,         ONLY: JPIB_K
  USE :: GRIB_ENCODER_REGISTER_MOD, ONLY: GRIB_SECTION_REGISTER_T
  USE :: HOOKS_MOD,                 ONLY: HOOKS_T
  USE :: SYSINFO_MOD,               ONLY: GET_NUM_THREADS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(HOOKS_T), INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: NTH
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_NUM_THREADS = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_TUNNELING_INFO = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_INITIALIZED = 3_JPIB_K

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

!$omp single
  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(PVT_TUNNELING_INFO), ERRFLAG_ALREADY_INITIALIZED )

  ! Get the number of threads
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NUM_THREADS) GET_NUM_THREADS( NTH, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( NTH .LE. 0_JPIB_K, ERRFLAG_UNABLE_TO_GET_NUM_THREADS )

  ! Allocate the shared information
  ALLOCATE( PVT_TUNNELING_INFO(NTH), STAT=ALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_TUNNELING_INFO )
!$omp end single

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
    CASE (ERRFLAG_UNABLE_TO_GET_NUM_THREADS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get the number of threads' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_TUNNELING_INFO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate the tunneling info' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=ALLOC_STAT )
      END IF
    CASE (ERRFLAG_ALREADY_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'already initialized' )
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

END FUNCTION INIT_TUNNELING_INFO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SET_TUNNELING_INFO'
PP_THREAD_SAFE FUNCTION SET_TUNNELING_INFO( IDX, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,         ONLY: JPIB_K
  USE :: GRIB_ENCODER_REGISTER_MOD, ONLY: GRIB_SECTION_REGISTER_T
  USE :: HOOKS_MOD,                 ONLY: HOOKS_T
  USE :: SYSINFO_MOD,               ONLY: GET_TID

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),  INTENT(OUT)   :: IDX
  TYPE(HOOKS_T), TARGET, INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
    INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_TID = 1_JPIB_K

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

  ! Get the thread id
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_TID) GET_TID( IDX, HOOKS )

!$omp critical(TUNNELING_INFO)
  ! Associate the Hooks
  PVT_TUNNELING_INFO(IDX)%PVT_HOOKS => HOOKS
!$omp end critical(TUNNELING_INFO)

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
    CASE (ERRFLAG_UNABLE_TO_GET_TID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get the thread id' )
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

END FUNCTION SET_TUNNELING_INFO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GET_TUNNELING_INFO'
PP_THREAD_SAFE SUBROUTINE GET_TUNNELING_INFO( IDX, HOOKS )

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,         ONLY: JPIB_K
  USE :: GRIB_ENCODER_REGISTER_MOD, ONLY: GRIB_SECTION_REGISTER_T
  USE :: HOOKS_MOD,                 ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),   INTENT(IN)  :: IDX
  TYPE(HOOKS_T), POINTER, INTENT(OUT) :: HOOKS

  ! Local variables
  LOGICAL, DIMENSION(4) :: CONDITIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  CONDITIONS(1) = .NOT.ALLOCATED(PVT_TUNNELING_INFO)
  CONDITIONS(2) = IDX .LT. 1_JPIB_K
  CONDITIONS(3) = IDX .GT. SIZE(PVT_TUNNELING_INFO)
  CONDITIONS(4) = .NOT.ASSOCIATED(PVT_TUNNELING_INFO(IDX)%PVT_HOOKS)

!$omp critical(TUNNELING_INFO)
  IF ( ALL(CONDITIONS) ) THEN
    ! Get the hooks to be returned
    HOOKS => PVT_TUNNELING_INFO(IDX)%PVT_HOOKS

    ! Nullify the pointer in the tunneling info
    PVT_TUNNELING_INFO(IDX)%PVT_HOOKS => NULL()
  ELSE
    HOOKS => NULL()
  ENDIF
!$omp end critical(TUNNELING_INFO)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END SUBROUTINE GET_TUNNELING_INFO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GET_SECTIONS_REGISTER'
PP_THREAD_SAFE FUNCTION GET_SECTIONS_REGISTER( REG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,         ONLY: JPIB_K
  USE :: GRIB_ENCODER_REGISTER_MOD, ONLY: GRIB_SECTION_REGISTER_T
  USE :: HOOKS_MOD,                 ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GRIB_SECTION_REGISTER_T), POINTER, INTENT(OUT)   :: REG
  TYPE(HOOKS_T),                          INTENT(INOUT) :: HOOKS

  ! Function result
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

!$omp critical(TUNNELING_INFO)
  REG => PVT_SECTION_REGISTER
!$omp end critical(TUNNELING_INFO)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION GET_SECTIONS_REGISTER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FREE_TUNNELING_INFO'
PP_THREAD_SAFE FUNCTION FREE_TUNNELING_INFO( HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,         ONLY: JPIB_K
  USE :: GRIB_ENCODER_REGISTER_MOD, ONLY: GRIB_SECTION_REGISTER_T
  USE :: HOOKS_MOD,                 ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(HOOKS_T), TARGET, INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_TUNNELING_INFO = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED = 3_JPIB_K

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

!$omp single
  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(PVT_TUNNELING_INFO), ERRFLAG_NOT_INITIALIZED )

  ! Allocate the shared information
  DEALLOCATE( PVT_TUNNELING_INFO, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_TUNNELING_INFO )
!$omp end single

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
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE_TUNNELING_INFO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate the tunneling info' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'already initialized' )
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

END FUNCTION FREE_TUNNELING_INFO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GRIB_ENCODER_TUNNELING_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
