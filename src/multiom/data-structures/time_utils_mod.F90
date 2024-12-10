!> @file time_utils_mod.F90
!>
!> @brief Module containing circular buffer data structure and operations.
!>
!> @author Mirco Valentini
!> @date February 7, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'time_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'TIME_UTILS_MOD'
MODULE TIME_UTILS_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

  ! Default visibility
  PRIVATE

  !> @brief data structure containing the desctiption of the current point in time
  TYPE :: CURR_TIME_T

    CHARACTER(LEN=256) :: PAR_
    INTEGER(KIND=JPIB_K) :: ISEC_

  CONTAINS

    !> @brief Default initialization of the data structure
    PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS :: INIT_DEFAULT => CURR_TIME_INIT_DEFAULT

    !> @brief Generic interface for the initialization
    GENERIC, PUBLIC :: INIT => INIT_DEFAULT

    !> @brief Default initialization of the data structure
    PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: COPY_TO => CURR_TIME_COPY_TO

    !> @brief Compute the bytesize of the data structure
    PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: BYTESIZE => CURR_TIME_MEMORY_BYTESIZE

    !> @brief Cleanup of the current time data structure
    PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: FREE => CURR_TIME_FREE

  END TYPE


  !> @brief data structure that contains all the relevant fields needed to define a circular buffer
  TYPE :: TIME_HISTORY_T

    !> Default visibility of the type
    PRIVATE

    !> The buffer that contains the elements of the circular buffer
    TYPE(CURR_TIME_T), ALLOCATABLE, DIMENSION(:) :: BUFFER_

    !> The head of the circular buffer
    INTEGER(KIND=JPIB_K) :: HEAD_ = 1_JPIB_K

    !> The tail of the circular buffer
    INTEGER(KIND=JPIB_K) :: TAIL_ = 1_JPIB_K

    !> The size of the circular buffer
    INTEGER(KIND=JPIB_K) :: SIZE_ = 0_JPIB_K

    !> The capacity of the circular buffer
    INTEGER(KIND=JPIB_K) :: CAPACITY_=-99999_JPIB_K

  CONTAINS

    !> @brief Initializes a circular buffer with the specified capacity.
    PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: INIT => CB_INIT

    !> @brief Frees the memory allocated for the circular buffer.
    PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: ENQUEUE => CB_ENQUEUE

    !> @brief Frees the memory allocated for the circular buffer.
    PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: DEQUEUE => CB_DEQUEUE

    !> @brief Retrieves the value at the specified index in the circular buffer.
    PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: GET => CB_GET

    !> @brief Retrieves all the values in the circular buffer.
    PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: GET_ALL => CB_GET_ALL

    !> @brief Frees the memory allocated for the circular buffer.
    PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SIZE => CB_SIZE

    !> @brief Frees the memory allocated for the circular buffer.
    PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: BYTESIZE => CB_BYTESIZE

    !> @brief Frees the memory allocated for the circular buffer.
    PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: IS_EMPTY => CB_IS_EMPTY

    !> @brief Frees the memory allocated for the circular buffer.
    PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: FREE => CB_FREE

  END TYPE

  ! Whitelist of public symbols (datatypes)
  PUBLIC :: CURR_TIME_T
  PUBLIC :: TIME_HISTORY_T
  PUBLIC :: COMPUTE_CURRENT_TIME

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMPUTE_CURRENT_TIME'
PP_THREAD_SAFE FUNCTION COMPUTE_CURRENT_TIME( &
&  MSG, PAR, TIME_HISTORY, CURR_TIME, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(TIME_HISTORY_T),            INTENT(INOUT) :: TIME_HISTORY
  TYPE(CURR_TIME_T),               INTENT(INOUT) :: CURR_TIME
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

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

  ! TODO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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


END FUNCTION COMPUTE_CURRENT_TIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CURR_TIME_INIT_DEFAULT'
PP_THREAD_SAFE FUNCTION CURR_TIME_INIT_DEFAULT( THIS, &
&  HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CURR_TIME_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),      INTENT(INOUT) :: HOOKS

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

  ! TODO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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


END FUNCTION CURR_TIME_INIT_DEFAULT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CURR_TIME_COPY_TO'
PP_THREAD_SAFE FUNCTION CURR_TIME_COPY_TO( THIS, &
&  OTHER, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CURR_TIME_T), INTENT(IN)    :: THIS
  CLASS(CURR_TIME_T), INTENT(INOUT) :: OTHER
  TYPE(HOOKS_T),      INTENT(INOUT) :: HOOKS

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

  ! TODO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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


END FUNCTION CURR_TIME_COPY_TO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CURR_TIME_MEMORY_BYTESIZE'
PP_THREAD_SAFE FUNCTION CURR_TIME_MEMORY_BYTESIZE( THIS, &
&  MEMORY_BYTESIZE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CURR_TIME_T),   INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: MEMORY_BYTESIZE
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

  ! TODO: Needs to be updated with the actual memory bytesize
  MEMORY_BYTESIZE = 264_JPIB_K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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


END FUNCTION CURR_TIME_MEMORY_BYTESIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CURR_TIME_FREE'
PP_THREAD_SAFE FUNCTION CURR_TIME_FREE( THIS, &
&  HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CURR_TIME_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),      INTENT(INOUT) :: HOOKS

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

  ! TODO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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


END FUNCTION CURR_TIME_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Initializes a circular buffer with the specified capacity.
!>
!> This subroutine initializes a circular buffer with the specified capacity.
!> It allocates memory for the circular buffer and sets its capacity.
!>
!> @param [out] THIS The circular buffer structure to be initialized.
!> @param [in]  CAPACITY The capacity of the circular buffer.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CB_INIT'
PP_THREAD_SAFE FUNCTION CB_INIT(THIS, CAPACITY, HOOKS ) RESULT(RET)

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
  CLASS(TIME_HISTORY_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: CAPACITY
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_CAPACITY=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INIT_BUFFER=4_JPIB_K

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

  ! Free memory just to avoid memory leaks
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE) THIS%FREE( HOOKS )

  ! Capacity needs to be greater than 0
  PP_DEBUG_CRITICAL_COND_THROW( CAPACITY .LE. 0, ERRFLAG_WRONG_CAPACITY )

  ! Initialize the circular buffer management
  THIS%HEAD_ = 1_JPIB_K
  THIS%TAIL_ = 1_JPIB_K
  THIS%SIZE_ = 0_JPIB_K
  THIS%CAPACITY_ = CAPACITY

  ! Allocate memory for the circular buffer
  ALLOCATE( THIS%BUFFER_(CAPACITY), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW(  ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )

  ! Initialize the circular buffer data
  DO I = 1,  THIS%CAPACITY_
    PP_TRYCALL(ERRFLAG_INIT_BUFFER) THIS%BUFFER_(I)%INIT( HOOKS )
  END DO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate memory' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=ALLOC_STATUS )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free memory' )
    CASE (ERRFLAG_WRONG_CAPACITY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong capacity' )
    CASE (ERRFLAG_INIT_BUFFER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to initialize buffer' )
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

END FUNCTION CB_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Initializes a circular buffer with the specified capacity.
!>
!> This subroutine initializes a circular buffer with the specified capacity.
!> It allocates memory for the circular buffer and sets its capacity.
!>
!> @param [out] THIS The circular buffer structure to be initialized.
!> @param [in]  CAPACITY The capacity of the circular buffer.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CB_FREE'
PP_THREAD_SAFE FUNCTION CB_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(TIME_HISTORY_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_BUFFER=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_BUFFER_NOT_ALLOCATED=3_JPIB_K

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

  ! Free the circular buffer data
  IF (  THIS%CAPACITY_ .GT. 0 ) THEN
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(THIS%BUFFER_), ERRFLAG_BUFFER_NOT_ALLOCATED )
    DO I = 1, THIS%CAPACITY_
      PP_TRYCALL(ERRFLAG_FREE_BUFFER) THIS%BUFFER_(I)%FREE( HOOKS )
    END DO
  ENDIF

  ! Free the circular buffer
  THIS%HEAD_ = 1_JPIB_K
  THIS%TAIL_ = 1_JPIB_K
  THIS%SIZE_ = 0_JPIB_K
  THIS%CAPACITY_=-99_JPIB_K
  IF ( ALLOCATED(THIS%BUFFER_) ) THEN
    DEALLOCATE(THIS%BUFFER_, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_FREE )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_UNABLE_TO_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate memory' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS )
      ENDIF
    CASE (ERRFLAG_FREE_BUFFER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free buffer' )
    CASE (ERRFLAG_BUFFER_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'buffer not allocated' )
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

END FUNCTION CB_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Enqueues a value into the circular buffer.
!>
!> This subroutine enqueues a value into the circular buffer by adding it to the front of the buffer.
!> If the buffer is full, the oldest value in the buffer will be overwritten.
!>
!> @param [inout] THIS  The circular buffer structure.
!> @param [in]    VALUE The value to be enqueued into the circular buffer.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CB_ENQUEUE'
PP_THREAD_SAFE FUNCTION CB_ENQUEUE(THIS, VALUE, HOOKS ) RESULT(RET)

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
  CLASS(TIME_HISTORY_T), INTENT(INOUT) :: THIS
  TYPE(CURR_TIME_T),     INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(CURR_TIME_T) :: TMP
  LOGICAL :: EX

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_DEQUEUE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SIZE_OUT_OF_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INDEX_OUT_OF_BOUNDS=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COPY=4_JPIB_K

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

  ! If the buffer is full then remove the last element
  IF (THIS%SIZE_ .EQ. THIS%CAPACITY_) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_DEQUEUE) CB_DEQUEUE( THIS, TMP, EX, HOOKS )
  ENDIF

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .LT. 0, ERRFLAG_SIZE_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .GT. THIS%CAPACITY_, ERRFLAG_SIZE_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%HEAD_ .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%HEAD_ .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%TAIL_ .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%TAIL_ .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )

  ! Copy the value to the buffe
  PP_TRYCALL(ERRFLAG_UNABLE_TO_COPY) VALUE%COPY_TO( THIS%BUFFER_(THIS%HEAD_), HOOKS )

  ! Update head and tail
  THIS%HEAD_ = MOD(THIS%HEAD_, THIS%CAPACITY_) + 1
  THIS%SIZE_ = THIS%SIZE_ + 1

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_UNABLE_TO_CALL_DEQUEUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call dequeue' )
    CASE (ERRFLAG_SIZE_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'size out of bounds' )
    CASE (ERRFLAG_INDEX_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'index out of bounds' )
    CASE (ERRFLAG_UNABLE_TO_COPY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to copy value' )
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

END FUNCTION CB_ENQUEUE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Dequeues a value from the circular buffer.
!>
!> This function dequeues a value from the circular buffer by removing the oldest value
!> from the end of the buffer and returning it.
!>
!> @param [inout] THIS The circular buffer structure.
!> @param [out] VALUE The value dequeued from the circular buffer.
!>
!> @return EX Logical indicating the success of the dequeue operation.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CB_DEQUEUE'
PP_THREAD_SAFE FUNCTION CB_DEQUEUE(THIS, VALUE, EX, HOOKS ) RESULT(RET)

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
  CLASS(TIME_HISTORY_T), INTENT(INOUT) :: THIS
  TYPE(CURR_TIME_T),     INTENT(OUT)   :: VALUE
  LOGICAL,               INTENT(OUT)   :: EX
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SIZE_OUT_OF_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INDEX_OUT_OF_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNBALE_TO_FREE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COPY=4_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .LT. 0, ERRFLAG_SIZE_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .GT. THIS%CAPACITY_, ERRFLAG_SIZE_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%HEAD_ .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%HEAD_ .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%TAIL_ .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%TAIL_ .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )

  IF ( THIS%SIZE_ .EQ. 0 ) THEN
    EX = .FALSE.
    PP_TRYCALL(ERRFLAG_UNBALE_TO_FREE) VALUE%FREE( HOOKS )
  ELSE
    EX = .TRUE.
    ! Copy time history from the buffer (call specific method)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_COPY) THIS%BUFFER_(THIS%TAIL_)%COPY_TO( VALUE, HOOKS )
    THIS%TAIL_ = MOD(THIS%TAIL_, THIS%CAPACITY_) + 1
    THIS%SIZE_ = THIS%SIZE_ - 1
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_SIZE_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'size out of bounds' )
    CASE (ERRFLAG_INDEX_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'index out of bounds' )
    CASE (ERRFLAG_UNBALE_TO_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free memory' )
    CASE (ERRFLAG_UNABLE_TO_COPY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to copy value' )
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

END FUNCTION CB_DEQUEUE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Retrieves the value at the specified index in the circular buffer.
!>
!> This function retrieves the value at the specified index in the circular buffer.
!> If the requested index is out of bounds or if the buffer is empty, the function assigns
!> -9999 to the VALUE variable and returns FALSE.
!>
!> @param [inout] THIS The circular buffer structure.
!> @param [in]    I    The index of the requested element in the buffer, starting from head.
!> @param [out]   VALUE The value retrieved from the circular buffer.
!>
!> @return EX Logical indicating the success of the retrieval operation.
!>
!> @note This function does not remove the retrieved element from the buffer.
!>
!> @note If the requested value is not present, the VALUE variable is assigned to -9999.
!>
!> @note I is the index of the requested element in the buffer starting from head.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CB_GET'
PP_THREAD_SAFE FUNCTION CB_GET(THIS, I, VALUE, EX, HOOKS ) RESULT(RET)

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
  CLASS(TIME_HISTORY_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: I
  TYPE(CURR_TIME_T),     INTENT(OUT)   :: VALUE
  LOGICAL,               INTENT(OUT)   :: EX
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SIZE_OUT_OF_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INDEX_OUT_OF_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNBALE_TO_FREE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COPY=4_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .LT. 0, ERRFLAG_SIZE_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .GT. THIS%CAPACITY_, ERRFLAG_SIZE_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%HEAD_ .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%HEAD_ .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%TAIL_ .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%TAIL_ .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( I .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( I .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )

  ! Retrieve the value at the specified index
  IF ( THIS%SIZE_ .EQ. 0 .OR. THIS%CAPACITY_ .LT. I) THEN
    EX = .FALSE.
    PP_TRYCALL(ERRFLAG_UNBALE_TO_FREE) VALUE%FREE( HOOKS )
  ELSE
    EX = .TRUE.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_COPY) THIS%BUFFER_(MOD(THIS%HEAD_+THIS%CAPACITY_-I-1,THIS%CAPACITY_)+1)%COPY_TO( VALUE, HOOKS )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_SIZE_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'size out of bounds' )
    CASE (ERRFLAG_INDEX_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'index out of bounds' )
    CASE (ERRFLAG_UNBALE_TO_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free memory' )
    CASE (ERRFLAG_UNABLE_TO_COPY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to copy value' )
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

END FUNCTION CB_GET
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Retrieves all the values in the circular buffer.
!>
!>
!> @param [inout] THIS   The circular buffer structure.
!> @param [out]   VALUES The value retrieved from the circular buffer.
!>
!> @return EX Logical indicating the success of the retrieval operation.
!>
!> @note This function does not remove the retrieved element from the buffer.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CB_GET_VALUES'
PP_THREAD_SAFE FUNCTION CB_GET_ALL(THIS, SZ, VALUES, EX, HOOKS ) RESULT(RET)

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
  CLASS(TIME_HISTORY_T),           INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),            INTENT(OUT)   :: SZ
  TYPE(CURR_TIME_T), DIMENSION(:), INTENT(OUT)   :: VALUES
  LOGICAL,                         INTENT(OUT)   :: EX
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SIZE_OUT_OF_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INDEX_OUT_OF_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNBALE_TO_FREE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COPY=4_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .LT. 0, ERRFLAG_SIZE_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .GT. THIS%CAPACITY_, ERRFLAG_SIZE_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%HEAD_ .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%HEAD_ .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%TAIL_ .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%TAIL_ .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )

  ! Initialize the output values
  DO I = 1, SIZE(VALUES)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_COPY) VALUES(I)%FREE( HOOKS )
  ENDDO

  ! Copy values from the buffer
  SZ = MIN( SIZE(VALUES), THIS%SIZE_ )
  DO I = 1, SZ
    EX = .TRUE.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_COPY) THIS%BUFFER_(MOD(THIS%HEAD_+THIS%CAPACITY_-I-1,THIS%CAPACITY_)+1)%COPY_TO(VALUES(SZ-I+1), HOOKS)
  ENDDO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_SIZE_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'size out of bounds' )
    CASE (ERRFLAG_INDEX_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'index out of bounds' )
    CASE (ERRFLAG_UNBALE_TO_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free memory' )
    CASE (ERRFLAG_UNABLE_TO_COPY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to copy value' )
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

END FUNCTION CB_GET_ALL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Returns the current size of the circular buffer.
!>
!> This function returns the current size of the circular buffer, which represents
!> the number of elements currently stored in the buffer.
!>
!> @param [in]  THIS The circular buffer structure.
!>
!> @return SIZE The current size of the circular buffer.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CB_SIZE'
PP_THREAD_SAFE FUNCTION CB_SIZE(THIS, SIZE, HOOKS ) RESULT(RET)

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
  CLASS(TIME_HISTORY_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),  INTENT(OUT)   :: SIZE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SIZE_OUT_OF_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INDEX_OUT_OF_BOUNDS=2_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .LT. 0, ERRFLAG_SIZE_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .GT. THIS%CAPACITY_, ERRFLAG_SIZE_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%HEAD_ .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%HEAD_ .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%TAIL_ .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%TAIL_ .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )

  ! Get the current size
  SIZE = THIS%SIZE_

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_SIZE_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'size out of bounds' )
    CASE (ERRFLAG_INDEX_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'index out of bounds' )
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

END FUNCTION CB_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CB_BYTESIZE'
PP_THREAD_SAFE FUNCTION CB_BYTESIZE(THIS, MEMORY_BYTESIZE, HOOKS ) RESULT(RET)

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
  CLASS(TIME_HISTORY_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),  INTENT(OUT)   :: MEMORY_BYTESIZE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: TMP_MEMORY_BYTESIZE

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SIZE_OUT_OF_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INDEX_OUT_OF_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COMPUTE_BUFFER_BYTESIZE=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .LT. 0, ERRFLAG_SIZE_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .GT. THIS%CAPACITY_, ERRFLAG_SIZE_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%HEAD_ .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%HEAD_ .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%TAIL_ .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%TAIL_ .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )

  ! Get the current size
  MEMORY_BYTESIZE = 32_JPIB_K

  ! Accumulate the size of the buffer
  DO I = 1, SIZE(THIS%BUFFER_)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_COMPUTE_BUFFER_BYTESIZE) THIS%BUFFER_(I)%BYTESIZE( TMP_MEMORY_BYTESIZE, HOOKS )
    MEMORY_BYTESIZE = MEMORY_BYTESIZE + TMP_MEMORY_BYTESIZE
  ENDDO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_SIZE_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'size out of bounds' )
    CASE (ERRFLAG_INDEX_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'index out of bounds' )
    CASE (ERRFLAG_UNABLE_TO_COMPUTE_BUFFER_BYTESIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to compute buffer bytesize' )
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

END FUNCTION CB_BYTESIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief True if the queue is empty
!>
!> @param [in]  THIS The circular buffer structure.
!>
!> @return IS_EMPTY A flag that is true if the queue is empty
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CB_IS_EMPTY'
PP_THREAD_SAFE FUNCTION CB_IS_EMPTY(THIS, IS_EMPTY, HOOKS ) RESULT(RET)

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
  CLASS(TIME_HISTORY_T), INTENT(IN)    :: THIS
  LOGICAL,               INTENT(OUT)   :: IS_EMPTY
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SIZE_OUT_OF_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INDEX_OUT_OF_BOUNDS=2_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .LT. 0, ERRFLAG_SIZE_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_ .GT. THIS%CAPACITY_, ERRFLAG_SIZE_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%HEAD_ .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%HEAD_ .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%TAIL_ .LT. 1, ERRFLAG_INDEX_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%TAIL_ .GT. THIS%CAPACITY_, ERRFLAG_INDEX_OUT_OF_BOUNDS )

  ! Check if the queue is empty
  IS_EMPTY = (THIS%SIZE_.EQ.0)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_SIZE_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'size out of bounds' )
    CASE (ERRFLAG_INDEX_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'index out of bounds' )
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

END FUNCTION CB_IS_EMPTY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE TIME_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME