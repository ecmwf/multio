!>
!> @file ENCODE_STACK_mod.F90
!>
!> @brief A module for managing debugging error stacks.
!>
!> This module defines a data structure `ENCODE_STACK_T` that provides facilities for
!> managing error frames and messages in a linked list format. It includes methods
!> to initialize the debug structure, free the allocated memory, and add error frames
!> and messages to the stack. Additionally, it provides functionality to print the
!> entire error stack for debugging purposes.
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>
! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'metadata_trace_frame_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'METADATA_TRACE_FRAME_MOD'
MODULE METADATA_TRACE_FRAME_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPIB_K
  USE :: METADATA_TRACE_NODE_MOD, ONLY: ENCODE_STACK_MSG_NODE

IMPLICIT NONE

  !> Default visibility of the module
  PRIVATE


  !>
  !> @brief Represents a node in a linked list of error frames.
  !>
  !> The `ENCODE_STACK_FRAME_NODE` type is used to store detailed information
  !> about an error frame, including the file, section, procedure, line
  !> number, and additional error metadata. Each frame can also contain
  !> a linked list of error messages. The structure supports maintaining
  !> a stack of error frames via linked pointers.
  !>
  TYPE :: ENCODE_STACK_FRAME_NODE

    !> Visibility of the type
    PRIVATE

    !> File name where the error occurred
    CHARACTER(LEN=:), ALLOCATABLE :: FILENAME
    CHARACTER(LEN=:), ALLOCATABLE :: SECTION_TYPE
    CHARACTER(LEN=:), ALLOCATABLE :: SECTION_NAME
    CHARACTER(LEN=:), ALLOCATABLE :: PROCEDURE_TYPE
    CHARACTER(LEN=:), ALLOCATABLE :: PROCEDURE_NAME
    CHARACTER(LEN=:), ALLOCATABLE :: ACTION
    INTEGER(KIND=JPIB_K) :: LINE
    INTEGER(KIND=JPIB_K) :: THREAD_ID
    INTEGER(KIND=JPIB_K) :: NUM_THREADS

    !> Messages list
    TYPE(ENCODE_STACK_MSG_NODE), POINTER :: MSG_LIST_HEAD => NULL()
    TYPE(ENCODE_STACK_MSG_NODE), POINTER :: MSG_LIST_TAIL => NULL()


    !> Parent Frame
    TYPE(ENCODE_STACK_FRAME_NODE), POINTER :: PARENT => NULL()

    !> Children Freames
    TYPE(ENCODE_STACK_FRAME_NODE), POINTER :: CHILDREN_HEAD => NULL()
    TYPE(ENCODE_STACK_FRAME_NODE), POINTER :: CHILDREN_TAIL => NULL()

    !> Siblings Freames
    TYPE(ENCODE_STACK_FRAME_NODE), POINTER :: SIBLING_NEXT => NULL()
    TYPE(ENCODE_STACK_FRAME_NODE), POINTER :: SIBLING_PREV => NULL()

  CONTAINS

    !> General management
    PROCEDURE, PUBLIC, PASS :: INIT => ENCODE_STACK_INIT_DEFAULT
    PROCEDURE, PUBLIC, PASS :: COPY_TO => ENCODE_STACK_COPY_TO

    !> Children
    PROCEDURE, PUBLIC, PASS :: PUSH_FRAME => ENCODE_STACK_PUSH_FRAME
    PROCEDURE, PUBLIC, PASS :: POP_FRAME  => ENCODE_STACK_POP_FRAME

    !> Sibling
    PROCEDURE, PUBLIC, PASS :: ADD_FRAME  => ENCODE_STACK_ADD_FRAME

    !> Get messages
    PROCEDURE, PUBLIC, PASS :: PARENT             => ENCODE_STACK_PARENT
    PROCEDURE, PUBLIC, PASS :: DUPLICATE_HEADER   => ENCODE_STACK_DUPLICATE_HEADER
    PROCEDURE, PUBLIC, PASS :: DUPLICATE_FRAMES   => ENCODE_STACK_DUPLICATE_CHILDREN
    PROCEDURE, PUBLIC, PASS :: DUPLICATE_MESSAGES => ENCODE_STACK_DUPLICATE_MESSAGES

    !> Different types of messages
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_EMPTY => ENCODE_STACK_PUSH_MSG_EMPTY
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_STRING => ENCODE_STACK_PUSH_MSG_STRING
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_STRING_ARRAY => ENCODE_STACK_PUSH_MSG_STRING_ARRAY
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_BOOL => ENCODE_STACK_PUSH_MSG_BOOL
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_BOOL_ARRAY => ENCODE_STACK_PUSH_MSG_BOOL_ARRAY
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_INT8 => ENCODE_STACK_PUSH_MSG_INT8
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_INT8_ARRAY => ENCODE_STACK_PUSH_MSG_INT8_ARRAY
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_INT16 => ENCODE_STACK_PUSH_MSG_INT16
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_INT16_ARRAY => ENCODE_STACK_PUSH_MSG_INT16_ARRAY
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_INT32 => ENCODE_STACK_PUSH_MSG_INT32
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_INT32_ARRAY => ENCODE_STACK_PUSH_MSG_INT32_ARRAY
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_INT64 => ENCODE_STACK_PUSH_MSG_INT64
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_INT64_ARRAY => ENCODE_STACK_PUSH_MSG_INT64_ARRAY
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_REAL32 => ENCODE_STACK_PUSH_MSG_REAL32
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_REAL32_ARRAY => ENCODE_STACK_PUSH_MSG_REAL32_ARRAY
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_REAL64 => ENCODE_STACK_PUSH_MSG_REAL64
    PROCEDURE, PRIVATE, PASS :: ADD_MSG_REAL64_ARRAY => ENCODE_STACK_PUSH_MSG_REAL64_ARRAY
    PROCEDURE, PRIVATE, PASS :: PRINT => ENCODE_STACK_PRINT
    PROCEDURE, PRIVATE, PASS :: FREE => ENCODE_STACK_FREE

  END TYPE


  !> Whitelist of public symbols
  PUBLIC :: ENCODE_STACK_T

CONTAINS

!>
!> @brief Manages the stack of error frames and messages for debugging.
!>
!> The `ENCODE_STACK_T` type is responsible for maintaining a stack of error frames, each
!> of which may contain multiple error messages. It provides methods for managing
!> this stack, including pushing new error frames and messages, as well as printing
!> the entire error stack. The stack can be initialized and freed to manage memory
!> efficiently. This type is used in debugging workflows where detailed error
!> tracking is required.
!>
!> @param [inout] THIS The `ENCODE_STACK_T` object whose error stack will be initialized.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_INIT_DEFAULT'
PP_THREAD_SAFE FUNCTION  ENCODE_STACK_INIT_DEFAULT( THIS, HOOKS ) RESULT(RET)

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
  CLASS(ENCODE_STACK_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_THE_STACK=1_JPIB_K

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

  !> Reset frame stack
  !> It is already emty it will not do anything
  PP_TRYCALL(ERRFLAG_FREE_THE_STACK) THIS%FREE( HOOKS )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  !> Exit point
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
    CASE (ERRFLAG_FREE_THE_STACK)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while freeing the stack' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION ENCODE_STACK_INIT_DEFAULT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





!>
!> @brief Manages the stack of error frames and messages for debugging.
!>
!> The `ENCODE_STACK_T` type is responsible for maintaining a stack of error frames, each
!> of which may contain multiple error messages. It provides methods for managing
!> this stack, including pushing new error frames and messages, as well as printing
!> the entire error stack. The stack can be initialized and freed to manage memory
!> efficiently. This type is used in debugging workflows where detailed error
!> tracking is required.
!>
!> @param [inout] THIS The `ENCODE_STACK_T` object whose error stack will be initialized.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_COPY_TO'
PP_THREAD_SAFE FUNCTION  ENCODE_STACK_COPY_TO( THIS, OTHER, HOOKS ) RESULT(RET)

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
  CLASS(ENCODE_STACK_T), INTENT(IN)    :: THIS
  TYPE(ENCODE_STACK_T),  INTENT(INOUT) :: OTHER
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(ENCODE_STACK_FRAME_NODE), POINTER :: CURRENT_FRAME
  TYPE(ENCODE_STACK_FRAME_NODE), POINTER :: NEXT_FRAME
  TYPE(ENCODE_STACK_MSG_NODE), POINTER :: CURRENT_MSG
  TYPE(ENCODE_STACK_MSG_NODE), POINTER :: NEXT_MSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILENAME_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECTION_TYPE_NOT_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECTION_NAME_NOT_ALLOCATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROCEDURE_TYPE_NOT_ALLOCATED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROCEDURE_NAME_NOT_ALLOCATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LINE_NOT_ALLOCATED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THREAD_ID_NOT_ALLOCATED=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NUM_THREADS_NOT_ALLOCATED=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ACTION_NOT_ALLOCATED=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MESSAGE_NOT_ALLOCATED=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_FREE=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_INIT=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PUSH_NEW_FRAME=14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PUSH_NEW_MSG=15_JPIB_K

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

  !> Reset frame stack
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_FREE) OTHER%FREE( HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_INIT) OTHER%INIT( HOOKS )

  !> Set the current frame to the top of the stack
  CURRENT_FRAME => THIS%FRAME_STACK

  !> Traverse all frames
  DO WHILE (ASSOCIATED(CURRENT_FRAME))

    !> Free all strings in the current frame
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CURRENT_FRAME%FILENAME), ERRFLAG_FILENAME_NOT_ALLOCATED )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CURRENT_FRAME%SECTION_TYPE), ERRFLAG_SECTION_TYPE_NOT_ALLOCATED  )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CURRENT_FRAME%SECTION_NAME), ERRFLAG_SECTION_NAME_NOT_ALLOCATED  )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CURRENT_FRAME%PROCEDURE_TYPE), ERRFLAG_PROCEDURE_TYPE_NOT_ALLOCATED  )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CURRENT_FRAME%PROCEDURE_NAME), ERRFLAG_PROCEDURE_NAME_NOT_ALLOCATED  )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CURRENT_FRAME%ACTION), ERRFLAG_ACTION_NOT_ALLOCATED  )

    !> Copy the current frame to the stack
    PP_TRYCALL(ERRFLAG_UNABLE_TO_PUSH_NEW_FRAME) OTHER%PUSH_FRAME( &
&     TRIM(ADJUSTL(CURRENT_FRAME%FILENAME)), &
&     TRIM(ADJUSTL(CURRENT_FRAME%SECTION_TYPE)), &
&     TRIM(ADJUSTL(CURRENT_FRAME%SECTION_NAME)), &
&     TRIM(ADJUSTL(CURRENT_FRAME%PROCEDURE_TYPE)), &
&     TRIM(ADJUSTL(CURRENT_FRAME%PROCEDURE_NAME)), &
&     TRIM(ADJUSTL(CURRENT_FRAME%ACTION)), &
&     CURRENT_FRAME%LINE, &
&     HOOKS )

    !> Move to the next frame
    NEXT_FRAME => CURRENT_FRAME%NEXT

    !> Free all messages in the current frame
    CURRENT_MSG => CURRENT_FRAME%MSG_LIST_HEAD
    DO WHILE (ASSOCIATED(CURRENT_MSG))
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CURRENT_MSG%MESSAGE), ERRFLAG_MESSAGE_NOT_ALLOCATED )
      PP_TRYCALL(ERRFLAG_UNABLE_TO_PUSH_NEW_MSG) OTHER%PUSH_MSG( TRIM(ADJUSTL(CURRENT_MSG%MESSAGE)), HOOKS )
      NEXT_MSG => CURRENT_MSG%NEXT
      CURRENT_MSG => NEXT_MSG
    END DO

  END DO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  !> Exit point
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
    CASE (ERRFLAG_FILENAME_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while copying the filename' )
    CASE (ERRFLAG_SECTION_TYPE_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while copying the section type' )
    CASE (ERRFLAG_SECTION_NAME_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while copying the section name' )
    CASE (ERRFLAG_PROCEDURE_TYPE_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while copying the procedure type' )
    CASE (ERRFLAG_PROCEDURE_NAME_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while copying the procedure name' )
    CASE (ERRFLAG_LINE_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while copying the line number' )
    CASE (ERRFLAG_THREAD_ID_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while copying the thread ID' )
    CASE (ERRFLAG_NUM_THREADS_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while copying the number of threads' )
    CASE (ERRFLAG_ACTION_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while copying the action' )
    CASE (ERRFLAG_MESSAGE_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while copying the message' )
    CASE (ERRFLAG_UNABLE_TO_CALL_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while freeing the stack' )
    CASE (ERRFLAG_UNABLE_TO_CALL_INIT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while initializing the stack' )
    CASE (ERRFLAG_UNABLE_TO_PUSH_NEW_FRAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while pushing a new frame' )
    CASE (ERRFLAG_UNABLE_TO_PUSH_NEW_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while pushing a new message' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION ENCODE_STACK_COPY_TO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Frees the error stack and all associated memory.
!>
!> This function deallocates the entire error stack in the `ENCODE_STACK_T` structure.
!> It frees all error frames and their associated messages, ensuring no memory
!> leaks occur after the debugging process. After calling this function, the
!> `FRAME_STACK` pointer is set to `NULL()`.
!>
!> @param [inout] THIS The `ENCODE_STACK_T` object whose error stack will be freed.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_FREE'
PP_THREAD_SAFE FUNCTION  ENCODE_STACK_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(ENCODE_STACK_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(ENCODE_STACK_FRAME_NODE), POINTER :: CURRENT_FRAME
  TYPE(ENCODE_STACK_FRAME_NODE), POINTER :: NEXT_FRAME
  TYPE(ENCODE_STACK_MSG_NODE), POINTER :: CURRENT_MSG
  TYPE(ENCODE_STACK_MSG_NODE), POINTER :: NEXT_MSG
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR=1_JPIB_K

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

  !> Set the current frame to the top of the stack
  CURRENT_FRAME => THIS%FRAME_STACK

  !> Traverse all frames
  DO WHILE (ASSOCIATED(CURRENT_FRAME))

    !> Free all strings in the current frame
    IF (ALLOCATED(CURRENT_FRAME%FILENAME)) THEN
      DEALLOCATE(CURRENT_FRAME%FILENAME, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
    ENDIF

    IF (ALLOCATED(CURRENT_FRAME%SECTION_TYPE)) THEN
      DEALLOCATE(CURRENT_FRAME%SECTION_TYPE, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
    ENDIF

    IF (ALLOCATED(CURRENT_FRAME%SECTION_NAME)) THEN
      DEALLOCATE(CURRENT_FRAME%SECTION_NAME, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
    ENDIF

    IF (ALLOCATED(CURRENT_FRAME%PROCEDURE_TYPE)) THEN
      DEALLOCATE(CURRENT_FRAME%PROCEDURE_TYPE, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
    ENDIF

    IF (ALLOCATED(CURRENT_FRAME%PROCEDURE_NAME)) THEN
      DEALLOCATE(CURRENT_FRAME%PROCEDURE_NAME, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
    ENDIF

    IF (ALLOCATED(CURRENT_FRAME%ACTION)) THEN
      DEALLOCATE(CURRENT_FRAME%ACTION, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
    ENDIF

    !> Free all messages in the current frame
    CURRENT_MSG => CURRENT_FRAME%MSG_LIST_HEAD
    DO WHILE (ASSOCIATED(CURRENT_MSG))

      !> Free the message string
      IF (ALLOCATED(CURRENT_MSG%MESSAGE)) THEN
        DEALLOCATE(CURRENT_MSG%MESSAGE, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
        PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
      ENDIF

      !> Deallocate the current message
      NEXT_MSG => CURRENT_MSG%NEXT
      NULLIFY(CURRENT_MSG%NEXT)
      DEALLOCATE(CURRENT_MSG, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )

      !> Move to the next message
      CURRENT_MSG => NEXT_MSG

    END DO

    !> Free the current frame
    NEXT_FRAME => CURRENT_FRAME%NEXT
    NULLIFY(CURRENT_FRAME%NEXT)
    NULLIFY(CURRENT_FRAME%MSG_LIST_HEAD)
    NULLIFY(CURRENT_FRAME%MSG_LIST_TAIL)
    DEALLOCATE(CURRENT_FRAME, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )

    !> Move to the next frame
    CURRENT_FRAME => NEXT_FRAME

  ENDDO

  !> Reset frame stack
  THIS%FRAME_STACK => NULL()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  !> Exit point
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
    CASE (ERRFLAG_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while deallocating memory' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION ENCODE_STACK_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Adds an error frame to the error stack in the `ENCODE_STACK_T` structure.
!>
!> This function creates a new error frame with the provided details (such as
!> file name, section type, procedure name, etc.) and pushes it onto the error stack
!> in the `ENCODE_STACK_T` object. This helps in tracking where an error occurred in
!> the code, including the specific procedure and line number.
!>
!> @param [inout] THIS The `ENCODE_STACK_T` object where the error frame will be added.
!> @param [in] FILENAME The name of the file where the error occurred.
!> @param [in] SECTION_TYPE The type of the section where the error occurred (e.g., function, function).
!> @param [in] SECTION_NAME The name of the section where the error occurred.
!> @param [in] PROCEDURE_TYPE The type of the procedure (e.g., function, function).
!> @param [in] PROCEDURE_NAME The name of the procedure where the error occurred.
!> @param [in] LINE The line number where the error occurred.
!> @param [in] ERRIDX The error index or code associated with the error.
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_PUSH_FRAME'
PP_THREAD_SAFE FUNCTION  ENCODE_STACK_PUSH_FRAME( THIS, FILENAME, SECTION_TYPE, &
&          SECTION_NAME, PROCEDURE_TYPE, PROCEDURE_NAME, ACTION, &
&          ILINE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from external libraries
  USE :: OMP_LIB, ONLY: OMP_GET_THREAD_NUM
  USE :: OMP_LIB, ONLY: OMP_GET_NUM_THREADS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(ENCODE_STACK_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(IN)    :: FILENAME
  CHARACTER(LEN=*),      INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),      INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),      INTENT(IN)    :: PROCEDURE_TYPE
  CHARACTER(LEN=*),      INTENT(IN)    :: PROCEDURE_NAME
  CHARACTER(LEN=*),      INTENT(IN)    :: ACTION
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: ILINE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(ENCODE_STACK_FRAME_NODE), POINTER :: NEW_FRAME
  INTEGER(JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=1_JPIB_K

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

  ! Create a new error frame node
  ALLOCATE(NEW_FRAME, STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_ALLOCATION_ERROR )

  !> Fill the frame header
  NEW_FRAME%FILENAME = TRIM(ADJUSTL(FILENAME))
  NEW_FRAME%SECTION_TYPE = TRIM(ADJUSTL(SECTION_TYPE))
  NEW_FRAME%SECTION_NAME = TRIM(ADJUSTL(SECTION_NAME))
  NEW_FRAME%PROCEDURE_TYPE = TRIM(ADJUSTL(PROCEDURE_TYPE))
  NEW_FRAME%PROCEDURE_NAME = TRIM(ADJUSTL(PROCEDURE_NAME))
  NEW_FRAME%ACTION = TRIM(ADJUSTL(ACTION))

  ! Write the line number
  NEW_FRAME%LINE = ILINE

  ! Write the current thread ID
  NEW_FRAME%THREAD_ID = OMP_GET_THREAD_NUM()

  ! Write total number of threads
  NEW_FRAME%NUM_THREADS = OMP_GET_NUM_THREADS()

  !> Insert the new frame at the top of the frame stack
  NEW_FRAME%NEXT => THIS%FRAME_STACK
  THIS%FRAME_STACK => NEW_FRAME

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  !> Exit point
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
    CASE (ERRFLAG_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while allocating memory for the new frame' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION ENCODE_STACK_PUSH_FRAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Adds an error message to the current error frame in the `ENCODE_STACK_T` structure.
!>
!> This function pushes a custom error message to the most recent error frame
!> in the `ENCODE_STACK_T` object's error stack. Each error frame can store multiple
!> error messages to provide additional context about the error.
!>
!> @param [inout] THIS The `ENCODE_STACK_T` object where the error message will be added.
!> @param [in] MSG The error message to be added to the current frame.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_PUSH_MSG_EMPTY'
PP_THREAD_SAFE FUNCTION  ENCODE_STACK_PUSH_MSG_EMPTY( THIS, MSG, HOOKS ) RESULT(RET)

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
  CLASS(ENCODE_STACK_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(IN)    :: MSG
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(ENCODE_STACK_MSG_NODE),   POINTER :: NEW_MSG
  TYPE(ENCODE_STACK_FRAME_NODE), POINTER :: CURRENT_FRAME
  INTEGER(JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PUSH_FAKE_FRAME=2_JPIB_K

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

  ! Check if there is a current frame
  CURRENT_FRAME => THIS%FRAME_STACK
  IF (.NOT. ASSOCIATED(CURRENT_FRAME)) THEN
    PP_TRYCALL(ERRFLAG_PUSH_FAKE_FRAME) THIS%PUSH_FRAME( &
&              'UNKNOWN', 'UNKNOWN', 'UNKNOWN', 'UNKNOWN', &
&              'UNKNOWN', 'UNKNOWN', -1_JPIB_K, HOOKS )
  END IF

  !> Create a new message node
  ALLOCATE(NEW_MSG, STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_ALLOCATION_ERROR )

  !> I am already in the error handler, theres not so much I can do in case of error
  IF ( ALLOC_STATUS .EQ. 0 ) THEN
    NEW_MSG%MESSAGE = MSG

    !> Insert the new message at the end of the message list
    IF ( .NOT.ASSOCIATED(CURRENT_FRAME%MSG_LIST_HEAD) ) THEN
      CURRENT_FRAME%MSG_LIST_HEAD => NEW_MSG
      CURRENT_FRAME%MSG_LIST_TAIL => NEW_MSG
    ELSE
      CURRENT_FRAME%MSG_LIST_TAIL%NEXT => NEW_MSG
      CURRENT_FRAME%MSG_LIST_TAIL => NEW_MSG
    END IF
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  !> Exit point
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
    CASE (ERRFLAG_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while allocating memory for the new message' )
    CASE (ERRFLAG_PUSH_FAKE_FRAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while pushing a fake frame' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION ENCODE_STACK_PUSH_MSG_EMPTY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_PUSH_MSG_BOOL'
PP_THREAD_SAFE FUNCTION  ENCODE_STACK_PUSH_MSG_BOOL( THIS, MSG, HOOKS ) RESULT(RET)

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
  CLASS(ENCODE_STACK_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(IN)    :: MSG
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(ENCODE_STACK_MSG_NODE),   POINTER :: NEW_MSG
  TYPE(ENCODE_STACK_FRAME_NODE), POINTER :: CURRENT_FRAME
  INTEGER(JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PUSH_FAKE_FRAME=2_JPIB_K

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

  ! Check if there is a current frame
  CURRENT_FRAME => THIS%FRAME_STACK
  IF (.NOT. ASSOCIATED(CURRENT_FRAME)) THEN
    PP_TRYCALL(ERRFLAG_PUSH_FAKE_FRAME) THIS%PUSH_FRAME( &
&              'UNKNOWN', 'UNKNOWN', 'UNKNOWN', 'UNKNOWN', &
&              'UNKNOWN', 'UNKNOWN', -1_JPIB_K, HOOKS )
  END IF

  !> Create a new message node
  ALLOCATE(NEW_MSG, STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_ALLOCATION_ERROR )

  !> I am already in the error handler, theres not so much I can do in case of error
  IF ( ALLOC_STATUS .EQ. 0 ) THEN
    NEW_MSG%MESSAGE = MSG

    !> Insert the new message at the end of the message list
    IF ( .NOT.ASSOCIATED(CURRENT_FRAME%MSG_LIST_HEAD) ) THEN
      CURRENT_FRAME%MSG_LIST_HEAD => NEW_MSG
      CURRENT_FRAME%MSG_LIST_TAIL => NEW_MSG
    ELSE
      CURRENT_FRAME%MSG_LIST_TAIL%NEXT => NEW_MSG
      CURRENT_FRAME%MSG_LIST_TAIL => NEW_MSG
    END IF
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  !> Exit point
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
    CASE (ERRFLAG_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while allocating memory for the new message' )
    CASE (ERRFLAG_PUSH_FAKE_FRAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error while pushing a fake frame' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION ENCODE_STACK_PUSH_MSG_BOOL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Prints all error frames and their associated messages in the `ENCODE_STACK_T` structure.
!>
!> This function traverses the error stack in the `ENCODE_STACK_T` object and prints
!> each error frame along with its corresponding error messages to the specified
!> output unit. This is useful for debugging purposes, as it provides a complete
!> overview of all recorded errors.
!>
!> @param [in] THIS The `ENCODE_STACK_T` object containing the error stack to be printed.
!> @param [in] UNIT The output unit where the error stack will be printed.
!>                  This can be a file unit or the standard output.
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_PRINT'
PP_THREAD_SAFE FUNCTION  ENCODE_STACK_PRINT( THIS, UNIT, HOOKS ) RESULT(RET)

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
  CLASS(ENCODE_STACK_T), INTENT(IN) :: THIS
  INTEGER(KIND=JPIB_K),  INTENT(IN) :: UNIT
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(ENCODE_STACK_FRAME_NODE), POINTER :: CURRENT_FRAME
  TYPE(ENCODE_STACK_MSG_NODE), POINTER :: CURRENT_MSG
  INTEGER(JPIB_K) :: OFFSET
  INTEGER(JPIB_K) :: LLEN
  INTEGER(JPIB_K) :: CNT
  INTEGER(JPIB_K) :: LOC_UNIT
  INTEGER(JPIB_K) :: OPEN_STATUS
  CHARACTER(LEN=32) :: TMP
  LOGICAL :: IS_OPENED

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

  !> Set the current frame to the top of the stack
  CURRENT_FRAME => THIS%FRAME_STACK
  IF (.NOT. ASSOCIATED(CURRENT_FRAME)) THEN
    WRITE(*,*) 'NO ERROR FRAMES IN THE STACK.'
    RETURN
  END IF

  ! Check the unit status
  INQUIRE( UNIT=UNIT, OPENED=IS_OPENED )
  IF ( IS_OPENED ) THEN
   LOC_UNIT = UNIT
  ELSE
    OPEN( NEWUNIT=LOC_UNIT, FILE='ENCODE_STACK.LOG', STATUS='UNKNOWN', &
&      ACTION='WRITE', FORM='FORMATTED', IOSTAT=OPEN_STATUS )
    IF ( OPEN_STATUS .NE. 0) THEN
      WRITE(*,*) 'UABLE TO FIND A VALID UNIT FOR THE ERROR STACK.'
      RETURN
    ENDIF
  END IF

  !> Print the error stack header
  WRITE(LOC_UNIT,*) 'ERROR STACK (MOST RECENT FIRST)'
  WRITE(LOC_UNIT,*) ' '

  !> Traverse all frames
  OFFSET=1
  CNT = 0
  LLEN=100
  DO WHILE (ASSOCIATED(CURRENT_FRAME))

    !> Print the current frame
    TMP=REPEAT(' ',32)
    WRITE(TMP,*, IOSTAT=OPEN_STATUS) CNT
    LLEN=LLEN-2

    WRITE(LOC_UNIT,'(A,A)', IOSTAT=OPEN_STATUS)         &
&     REPEAT(' ',OFFSET), REPEAT('-',LLEN)

    WRITE(LOC_UNIT,'(A,A,A)', IOSTAT=OPEN_STATUS)       &
&     REPEAT(' ',OFFSET), ' ** FRAME :: ', TRIM(ADJUSTL(TMP))

    WRITE(LOC_UNIT,'(A,A,A,A)', IOSTAT=OPEN_STATUS)     &
&     REPEAT(' ',OFFSET), REPEAT('-',LLEN)

    IF ( ALLOCATED(CURRENT_FRAME%FILENAME) ) THEN
    WRITE(LOC_UNIT,'(A,A,A,A,A,A)', IOSTAT=OPEN_STATUS) &
&     REPEAT(' ',OFFSET), ' - FILE         :: "', &
&     TRIM(ADJUSTL(CURRENT_FRAME%FILENAME)), '"'
    ENDIF

    IF ( ALLOCATED(CURRENT_FRAME%SECTION_TYPE) .AND. &
&        ALLOCATED(CURRENT_FRAME%SECTION_NAME)) THEN
      WRITE(LOC_UNIT,'(A,A,A,A,A,A)', IOSTAT=OPEN_STATUS) &
&       REPEAT(' ',OFFSET), ' - SECTION      :: "', &
&       TRIM(ADJUSTL(CURRENT_FRAME%SECTION_TYPE)),  &
&       '::', TRIM(ADJUSTL(CURRENT_FRAME%SECTION_NAME)), '"'
    ENDIF

    IF ( ALLOCATED(CURRENT_FRAME%PROCEDURE_TYPE) .AND. &
         ALLOCATED(CURRENT_FRAME%PROCEDURE_NAME) ) THEN
      WRITE(LOC_UNIT,'(A,A,A,A,A,A)', IOSTAT=OPEN_STATUS) &
&       REPEAT(' ',OFFSET), ' - PROCEDURE    :: "', &
&       TRIM(ADJUSTL(CURRENT_FRAME%PROCEDURE_TYPE)), &
&       '::', TRIM(ADJUSTL(CURRENT_FRAME%PROCEDURE_NAME)), '"'
    ENDIF

    WRITE(LOC_UNIT,'(A,A,I8)', IOSTAT=OPEN_STATUS) &
&        REPEAT(' ',OFFSET), ' - LINE         :: ', &
&        CURRENT_FRAME%LINE


    WRITE(LOC_UNIT,'(A,A,I8)', IOSTAT=OPEN_STATUS) &
&        REPEAT(' ',OFFSET), ' - THREAD ID.   :: ', &
&        CURRENT_FRAME%THREAD_ID

    WRITE(LOC_UNIT,'(A,A,I8)', IOSTAT=OPEN_STATUS) &
&        REPEAT(' ',OFFSET), ' - NUM. THREADS :: ', &
&        CURRENT_FRAME%NUM_THREADS


    WRITE(LOC_UNIT,'(A,A)', IOSTAT=OPEN_STATUS)   &
&      REPEAT(' ',OFFSET), ' ** MESSAGES:'

    !> Print all messages in the current frame
    CURRENT_MSG => CURRENT_FRAME%MSG_LIST_HEAD
    DO WHILE (ASSOCIATED(CURRENT_MSG))
      IF ( ALLOCATED(CURRENT_MSG%MESSAGE) ) THEN
        WRITE(LOC_UNIT,'(A,A,A)', IOSTAT=OPEN_STATUS) &
&          REPEAT(' ',OFFSET), "    - ", CURRENT_MSG%MESSAGE
      ENDIF
      CURRENT_MSG => CURRENT_MSG%NEXT

    END DO
    WRITE(LOC_UNIT,*, IOSTAT=OPEN_STATUS) ' '

    !> Move to the next frame
    CURRENT_FRAME => CURRENT_FRAME%NEXT

    !> Update offset
    IF ( CURRENT_FRAME%ACTION.EQ.'ENTER' ) THEN
      OFFSET = OFFSET + 2
    ELSEIF ( CURRENT_FRAME%ACTION.EQ.'EXIT' .AND. OFFSET.GE.2 ) THEN
      OFFSEt =OFFSET - 2
    ENDIF

    ! Update counter
    CNT = CNT + 1
  END DO

  !> If necessary close the file
  IF ( .NOT.IS_OPENED ) THEN
    CLOSE(UNIT=LOC_UNIT)
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  !> Exit point
  RETURN

END FUNCTION ENCODE_STACK_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE METADATA_TRACE_FRAME_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
