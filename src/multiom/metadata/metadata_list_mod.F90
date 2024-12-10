!>
!> @file filter_level_mod.F90
!>
!> @brief Module containing definitions and procedures for level filters.
!>
!> This module defines the `FILTER_LEVELIST_T` type, along with its associated
!> procedures and helper functions that facilitate the creation, management, and
!> utilization of level filters within the system. Level filters allow for
!> complex filtering operations by combining multiple nested filters.
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'metadata_list_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'METADATA_LIST_MOD'
MODULE METADATA_LIST_MOD

  ! Symbols imported from other modules within the project.
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

TYPE :: METADATA_NODE_T
  CHARACTER(LEN=256) :: MAPPING_TAG_=REPEAT(' ', 256)
  CHARACTER(LEN=256) :: ENCODER_TAG_=REPEAT(' ', 256)
  CHARACTER(LEN=256) :: MAPPING_NAME_=REPEAT(' ', 256)
  CHARACTER(LEN=256) :: ENCODER_NAME_=REPEAT(' ', 256)
  TYPE(FORTRAN_MESSAGE_T) :: MSG
  TYPE(PARAMETRIZATION_T) :: PAR
  CLASS(METADATA_BASE_A), POINTER :: METADATA => NULL()

  TYPE(METADATA_NODE_T), POINTER :: NEXT => NULL()
  TYPE(METADATA_NODE_T), POINTER :: PREV => NULL()
END TYPE

TYPE :: METADATA_LIST_T
  PRIVATE
  TYPE(METADATA_NODE_T), POINTER :: HEAD => NULL()
  TYPE(METADATA_NODE_T), POINTER :: TAIL => NULL()
  INTEGER :: SIZE_ = 0
CONTAINS
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: INIT => METADATA_LIST_INIT
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: PUSH => METADATA_LIST_PUSH
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: POP  => METADATA_LIST_POP
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: IS_EMPTY  => METADATA_LIST_IS_EMPTY
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SIZE => METADATA_LIST_SIZE
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: FREE => METADATA_LIST_FREE

END TYPE

!> Whitelist of public symbols in the module
PUBLIC :: METADATA_LIST_T

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'METADATA_LIST_INIT'
PP_THREAD_SAFE FUNCTION METADATA_LIST_INIT( THIS, HOOKS ) RESULT(RET)

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

  !> Dummy arguments
  CLASS(METADATA_LIST_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_INITIALIZED = 1_JPIB_K

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

  ! Error handler
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .NE. 0, ERRFLAG_ALREADY_INITIALIZED )
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%HEAD), ERRFLAG_ALREADY_INITIALIZED )
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%TAIL), ERRFLAG_ALREADY_INITIALIZED )

  ! Initialization of the list
  THIS%SIZE_ = 0
  THIS%HEAD => NULL()
  THIS%TAIL => NULL()

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
    CASE(ERRFLAG_ALREADY_INITIALIZED)
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


END FUNCTION METADATA_LIST_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'METADATA_LIST_IS_EMPTY'
PP_THREAD_SAFE FUNCTION METADATA_LIST_IS_EMPTY( THIS, IS_EMPTY, HOOKS ) RESULT(RET)

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

  !> Dummy arguments
  CLASS(METADATA_LIST_T), INTENT(INOUT) :: THIS
  LOGICAL,                INTENT(OUT) :: IS_EMPTY
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

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

  ! Check if the list is empty
  IS_EMPTY = (THIS%SIZE_ .EQ. 0)

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


END FUNCTION METADATA_LIST_IS_EMPTY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'METADATA_LIST_SIZE'
PP_THREAD_SAFE FUNCTION METADATA_LIST_SIZE( THIS, SIZE, HOOKS ) RESULT(RET)

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

  !> Dummy arguments
  CLASS(METADATA_LIST_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),   INTENT(OUT)   :: SIZE
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

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

  ! Check if the list is empty
  SIZE = THIS%SIZE_

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


END FUNCTION METADATA_LIST_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'METADATA_LIST_PUSH'
PP_THREAD_SAFE FUNCTION METADATA_LIST_PUSH( THIS, MSG, PAR, &
& MAPPING_TAG, MAPPING_NAME, ENCODER_TAG, ENCODER_NAME, METADATA, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(METADATA_LIST_T),          INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  CHARACTER(LEN=256),              INTENT(IN)    :: MAPPING_TAG
  CHARACTER(LEN=256),              INTENT(IN)    :: MAPPING_NAME
  CHARACTER(LEN=256),              INTENT(IN)    :: ENCODER_TAG
  CHARACTER(LEN=256),              INTENT(IN)    :: ENCODER_NAME
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variable
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOC_FAILED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COPY_MSG = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COPY_PAR = 3_JPIB_K

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

  ! Allocate a new node
  ALLOCATE( THIS%TAIL%NEXT, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_ALLOC_FAILED )

  ! Link the new node to the list
  THIS%TAIL => THIS%TAIL%NEXT
  IF ( .NOT.ASSOCIATED(THIS%HEAD) ) THEN
    THIS%HEAD => THIS%TAIL
  ENDIF

  ! Fill the node
  PP_TRYCALL(ERRFLAG_UNABLE_TO_COPY_MSG) THIS%TAIL%MSG%COPY_FROM( MSG, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_COPY_PAR) THIS%TAIL%PAR%COPY_FROM( PAR, HOOKS )
  THIS%TAIL%MAPPING_TAG_ = MAPPING_TAG
  THIS%TAIL%MAPPING_NAME_ = MAPPING_NAME
  THIS%TAIL%ENCODER_TAG_ = ENCODER_TAG
  THIS%TAIL%ENCODER_NAME_ = ENCODER_NAME
  THIS%TAIL%METADATA => METADATA
  METADATA => NULL()

  ! Update the size of the list
  THIS%SIZE_ = THIS%SIZE_ + 1

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
    CASE(ERRFLAG_ALLOC_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'allocation failed' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=ALLOC_STATUS )
      ENDIF
    CASE(ERRFLAG_UNABLE_TO_COPY_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to copy message' )
    CASE(ERRFLAG_UNABLE_TO_COPY_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to copy parametrization' )
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


END FUNCTION METADATA_LIST_PUSH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'METADATA_LIST_POP'
PP_THREAD_SAFE FUNCTION METADATA_LIST_POP( THIS, MSG, PAR, &
& MAPPING_TAG, MAPPING_NAME, ENCODER_TAG, ENCODER_NAME, METADATA, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(METADATA_LIST_T),          INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(OUT)   :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(OUT)   :: PAR
  CHARACTER(LEN=256),              INTENT(OUT)   :: MAPPING_TAG
  CHARACTER(LEN=256),              INTENT(OUT)   :: MAPPING_NAME
  CHARACTER(LEN=256),              INTENT(OUT)   :: ENCODER_TAG
  CHARACTER(LEN=256),              INTENT(OUT)   :: ENCODER_NAME
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variable
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  TYPE(METADATA_NODE_T), POINTER :: CURRENT

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LIST_IS_EMPTY = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TAIL_NOT_ASSOCIATED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HEAD_NOT_ASSOCIATED = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_MSG = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_PAR = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COPY_MSG = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COPY_PAR = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOC_FAILED = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INCONSISTENT_SIZE = 9_JPIB_K

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

  ! Error handler
  PP_DEBUG_CRITICAL_COND_THROW(  THIS%SIZE_ .EQ. 0, ERRFLAG_LIST_IS_EMPTY )
  PP_DEBUG_CRITICAL_COND_THROW(  .NOT.ASSOCIATED(THIS%TAIL), ERRFLAG_TAIL_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW(  .NOT.ASSOCIATED(THIS%HEAD), ERRFLAG_HEAD_NOT_ASSOCIATED )

  ! Init the output variables
  PP_TRYCALL(ERRFLAG_FREE_MSG) MSG%FREE( HOOKS )
  PP_TRYCALL(ERRFLAG_FREE_PAR) PAR%FREE( HOOKS )
  METADATA => NULL()

  ! Fill output variables
  CURRENT => THIS%TAIL

  ! Fill output variables
  PP_TRYCALL(ERRFLAG_UNABLE_TO_COPY_MSG) MSG%COPY_FROM( CURRENT%MSG, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_COPY_PAR) PAR%COPY_FROM( CURRENT%PAR, HOOKS )
  METADATA => CURRENT%METADATA
  MAPPING_TAG = CURRENT%MAPPING_TAG_
  MAPPING_NAME = CURRENT%MAPPING_NAME_
  ENCODER_TAG = CURRENT%ENCODER_TAG_
  ENCODER_NAME = CURRENT%ENCODER_NAME_

  ! Relink list
  THIS%TAIL => THIS%TAIL%PREV
  IF ( .NOT.ASSOCIATED(THIS%TAIL) ) THEN
      THIS%HEAD => NULL()
  ELSE
      THIS%TAIL%NEXT => NULL()
  ENDIF

  ! Free node
  PP_TRYCALL(ERRFLAG_FREE_MSG) CURRENT%MSG%FREE( HOOKS )
  PP_TRYCALL(ERRFLAG_FREE_PAR) CURRENT%PAR%FREE( HOOKS )
  CURRENT%METADATA => NULL()

  DEALLOCATE( CURRENT, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_DEALLOC_FAILED )

  ! Update the size of the list
  THIS%SIZE_ = THIS%SIZE_ - 1

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW(  THIS%SIZE_ .LT. 0, ERRFLAG_INCONSISTENT_SIZE )

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
    CASE(ERRFLAG_LIST_IS_EMPTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'list is empty' )
    CASE(ERRFLAG_TAIL_NOT_ASSOCIATED)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'tail not associated' )
    CASE(ERRFLAG_HEAD_NOT_ASSOCIATED)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'head not associated' )
    CASE(ERRFLAG_FREE_MSG)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'free message failed' )
    CASE(ERRFLAG_FREE_PAR)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'free parametrization failed' )
    CASE(ERRFLAG_UNABLE_TO_COPY_MSG)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to copy message' )
    CASE(ERRFLAG_UNABLE_TO_COPY_PAR)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to copy parametrization' )
    CASE(ERRFLAG_DEALLOC_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'deallocation failed' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS )
      ENDIF
    CASE(ERRFLAG_INCONSISTENT_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'inconsistent size' )
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


END FUNCTION METADATA_LIST_POP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'METADATA_LIST_FREE'
PP_THREAD_SAFE FUNCTION METADATA_LIST_FREE( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPIB_K
  USE :: HOOKS_MOD,            ONLY: HOOKS_T
  USE :: METADATA_FACTORY_MOD, ONLY: DESTROY_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(METADATA_LIST_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variable
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  TYPE(METADATA_NODE_T), POINTER :: CURRENT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SIZE_MISMATCH = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOC_FAILED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DESTROY_METADATA = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DESTROY_MSG = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DESTROY_PAR = 5_JPIB_K

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

  ! Free all the nodes in the list
  IF ( ASSOCIATED(THIS%HEAD) ) THEN
    CURRENT => THIS%HEAD
    DO WHILE( ASSOCIATED(CURRENT) )
      IF ( ASSOCIATED(CURRENT%METADATA) ) THEN
        PP_TRYCALL(ERRFLAG_DESTROY_METADATA) DESTROY_METADATA( CURRENT%METADATA, HOOKS )
      ENDIF
      PP_TRYCALL(ERRFLAG_DESTROY_MSG) CURRENT%MSG%FREE( HOOKS )
      PP_TRYCALL(ERRFLAG_DESTROY_PAR) CURRENT%PAR%FREE( HOOKS )
      CURRENT => CURRENT%NEXT
      THIS%HEAD => CURRENT
      IF ( .NOT.ASSOCIATED(CURRENT) ) THEN
        THIS%TAIL => NULL()
      ELSE
        CURRENT%PREV => THIS%HEAD
      ENDIF
      THIS%SIZE_ = THIS%SIZE_ - 1
      DEALLOCATE( CURRENT, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_DEALLOC_FAILED )
    ENDDO
  ENDIF

  ! Error handler
  PP_DEBUG_CRITICAL_COND_THROW(  THIS%SIZE_ .NE. 0, ERRFLAG_SIZE_MISMATCH )

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
    CASE(ERRFLAG_SIZE_MISMATCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'size mismatch' )
    CASE(ERRFLAG_DEALLOC_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'deallocation failed' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS )
      ENDIF
    CASE(ERRFLAG_DESTROY_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'destroy metadata failed' )
    CASE(ERRFLAG_DESTROY_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'destroy message failed' )
    CASE(ERRFLAG_DESTROY_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'destroy parametrization failed' )
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


END FUNCTION METADATA_LIST_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE METADATA_LIST_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME