#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'multio_metadata_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'METADATA_TRACE_NODE_MOD'
MODULE METADATA_TRACE_NODE_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN

IMPLICIT NONE

  !> Default visibility of the module
  PRIVATE


  !>
  !> @brief Defines a node for storing error messages in a linked list.
  !>
  !> The `ENCODE_STACK_MSG_NODE` type represents a single node in a linked list of error messages.
  !> Each node contains an allocatable `MESSAGE` string and a pointer `NEXT` to the next node in the list.
  !> This allows dynamic storage of error messages in a linked structure.
  !>
  TYPE :: ENCODE_STACK_MSG_NODE

    !> Operation to be printed [SET, SET_MISSING, COPY_FROM_SAMPLE, LOAD_FROM_SAMPLE_NAME]
    CHARACTER(LEN=32) :: OPERATION=REPEAT(' ',32)

    !> Mode to be used in the operation (at the moment [ALLOCATE, PRESET, RUNTIME])
    CHARACTER(LEN=32) :: MODE=REPEAT(' ',32)

    !> Key to be used in the operation
    CHARACTER(LEN=32) :: KEY=REPEAT(' ',32)

    !> Values to be used in the operation (if any)
    CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE :: VAL

    !> Pointer to the next message in the list
    TYPE(ENCODE_STACK_MSG_NODE), POINTER :: NEXT => NULL()

  CONTAINS

    PROCEDURE, PUBLIC, PASS :: COPY_TO => ENCODE_STACK_MSG_NODE_COPY_TO

    PROCEDURE, PRIVATE, PASS :: INIT_NO_VALUE     => ENCODE_STACK_MSG_NODE_INIT_NO_VALUE
    PROCEDURE, PRIVATE, PASS :: INIT_STRING       => ENCODE_STACK_MSG_NODE_INIT_STRING
    PROCEDURE, PRIVATE, PASS :: INIT_STRING_ARRAY => ENCODE_STACK_MSG_NODE_INIT_STRING_ARRAY
    PROCEDURE, PRIVATE, PASS :: INIT_BOOL         => ENCODE_STACK_MSG_NODE_INIT_BOOL
    PROCEDURE, PRIVATE, PASS :: INIT_BOOL_ARRAY   => ENCODE_STACK_MSG_NODE_INIT_BOOL_ARRAY
    PROCEDURE, PRIVATE, PASS :: INIT_INT8         => ENCODE_STACK_MSG_NODE_INIT_INT8
    PROCEDURE, PRIVATE, PASS :: INIT_INT8_ARRAY   => ENCODE_STACK_MSG_NODE_INIT_INT8_ARRAY
    PROCEDURE, PRIVATE, PASS :: INIT_INT16        => ENCODE_STACK_MSG_NODE_INIT_INT16
    PROCEDURE, PRIVATE, PASS :: INIT_INT16_ARRAY  => ENCODE_STACK_MSG_NODE_INIT_INT16_ARRAY
    PROCEDURE, PRIVATE, PASS :: INIT_INT32        => ENCODE_STACK_MSG_NODE_INIT_INT32
    PROCEDURE, PRIVATE, PASS :: INIT_INT32_ARRAY  => ENCODE_STACK_MSG_NODE_INIT_INT32_ARRAY
    PROCEDURE, PRIVATE, PASS :: INIT_INT64        => ENCODE_STACK_MSG_NODE_INIT_INT64
    PROCEDURE, PRIVATE, PASS :: INIT_INT64_ARRAY  => ENCODE_STACK_MSG_NODE_INIT_INT64_ARRAY
    PROCEDURE, PRIVATE, PASS :: INIT_REAL32       => ENCODE_STACK_MSG_NODE_INIT_REAL32
    PROCEDURE, PRIVATE, PASS :: INIT_REAL32_ARRAY => ENCODE_STACK_MSG_NODE_INIT_REAL32_ARRAY
    PROCEDURE, PRIVATE, PASS :: INIT_REAL64       => ENCODE_STACK_MSG_NODE_INIT_REAL64
    PROCEDURE, PRIVATE, PASS :: INIT_REAL64_ARRAY => ENCODE_STACK_MSG_NODE_INIT_REAL64_ARRAY

    GENERIC, PUBLIC :: INIT => INIT_NO_VALUE, INIT_STRING, INIT_STRING_ARRAY, INIT_BOOL, INIT_BOOL_ARRAY, &
&                           INIT_INT8, INIT_INT8_ARRAY, INIT_INT16, INIT_INT16_ARRAY, INIT_INT32, INIT_INT32_ARRAY, &
&                           INIT_INT64, INIT_INT64_ARRAY, INIT_REAL32, INIT_REAL32_ARRAY, INIT_REAL64, INIT_REAL64_ARRAY

    PROCEDURE, PUBLIC, PASS :: FREE    => ENCODE_STACK_MSG_NODE_FREE

  END TYPE

!> Whitelist of public symbols
PUBLIC :: ENCODE_STACK_MSG_NODE

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_FREE'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_FREE( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODE_STACK_MSG_NODE), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOC_ERROR=1_JPIB_K

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

  ! Reset the fields
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Free the value array
  IF ( ALLOCATED(THIS%VAL) ) THEN
    DEALLOCATE(THIS%VAL, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOC_ERROR )
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_DEALLOC_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error deallocating the value array' )
      IF (DEALLOC_STATUS.NE.0) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG,STAT=DEALLOC_STATUS)
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

END FUNCTION ENCODE_STACK_MSG_NODE_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_COPY_TO'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_COPY_TO( THIS, OTHER, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODE_STACK_MSG_NODE), INTENT(INOUT) :: THIS
  CLASS(ENCODE_STACK_MSG_NODE), INTENT(INOUT) :: OTHER
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_OTHER_OBJECT=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_COPY_VALUES=2_JPIB_K

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

  ! Reset the fields
  PP_TRYCALL(ERRFLAG_FREE_OTHER_OBJECT) OTHER%FREE( HOOKS )

  ! Free the value array
  IF ( ALLOCATED(THIS%VAL) ) THEN
    PP_TRYCALL(ERRFLAG_COPY_VALUES) OTHER%INIT( THIS%MODE, THIS%OPERATION, THIS%KEY, THIS%VAL, HOOKS )
  ELSE
    PP_TRYCALL(ERRFLAG_COPY_VALUES) OTHER%INIT( THIS%MODE, THIS%OPERATION, THIS%KEY, HOOKS )
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_FREE_OTHER_OBJECT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error freeing the other object' )
    CASE(ERRFLAG_COPY_VALUES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error copying the values' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_COPY_TO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_NO_VALUE'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_NO_VALUE( THIS, MODE, OPERATION, KEY, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODE_STACK_MSG_NODE), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: MODE
  CHARACTER(LEN=*),             INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),             INTENT(IN)    :: KEY
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_NO_VALUE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_STRING'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_STRING( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODE_STACK_MSG_NODE), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: MODE
  CHARACTER(LEN=*),             INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),             INTENT(IN)    :: KEY
  CHARACTER(LEN=*),             INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VAL_TOO_LONG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(VAL)).GT.MAX_STR_LEN, ERRFLAG_VAL_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Allocate the value array
  ALLOCATE( THIS%VAL(1), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)
  THIS%VAL(1)    = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))
  THIS%VAL(1)    = TRIM(ADJUSTL(VAL))

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_VAL_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_ALLOCATE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error allocating the value array' )
      IF (ALLOC_STATUS.NE.0) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_STRING_ARRAY( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODE_STACK_MSG_NODE),   INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),               INTENT(IN)    :: MODE
  CHARACTER(LEN=*),               INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),               INTENT(IN)    :: KEY
  CHARACTER(LEN=*), DIMENSION(:), INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VAL_TOO_LONG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Get the size of the value array
  N = SIZE(VAL)

  ! Allocate the value array
  ALLOCATE( THIS%VAL(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  DO I = 1, N
    PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(VAL(I))).GT.MAX_STR_LEN, ERRFLAG_VAL_TOO_LONG )
    THIS%VAL(I) = REPEAT(' ',MAX_STR_LEN)
    THIS%VAL(I) = TRIM(ADJUSTL(VAL(I)))
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_VAL_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_ALLOCATE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error allocating the value array' )
      IF (ALLOC_STATUS.NE.0) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_BOOL'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_BOOL( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

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
  CLASS(ENCODE_STACK_MSG_NODE), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: MODE
  CHARACTER(LEN=*),             INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),             INTENT(IN)    :: KEY
  LOGICAL,                      INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERSION_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  PP_TRYCALL(ERRFLAG_CONVERSION_ERROR) TO_STRING( VAL, THIS%VAL, HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_CONVERSION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_BOOL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_BOOL_ARRAY'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_BOOL_ARRAY( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

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
  CLASS(ENCODE_STACK_MSG_NODE), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: MODE
  CHARACTER(LEN=*),             INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),             INTENT(IN)    :: KEY
  LOGICAL, DIMENSION(:),        INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERSION_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  PP_TRYCALL(ERRFLAG_CONVERSION_ERROR) TO_STRING( VAL, THIS%VAL, HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_CONVERSION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_BOOL_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_INT8'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT8( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

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
  CLASS(ENCODE_STACK_MSG_NODE), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: MODE
  CHARACTER(LEN=*),             INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),             INTENT(IN)    :: KEY
  INTEGER(KIND=INT8),           INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERSION_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  PP_TRYCALL(ERRFLAG_CONVERSION_ERROR) TO_STRING( VAL, THIS%VAL, HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_CONVERSION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT8
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_INT8_ARRAY'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT8_ARRAY( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

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
  CLASS(ENCODE_STACK_MSG_NODE),     INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                 INTENT(IN)    :: MODE
  CHARACTER(LEN=*),                 INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),                 INTENT(IN)    :: KEY
  INTEGER(KIND=INT8), DIMENSION(:), INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERSION_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  PP_TRYCALL(ERRFLAG_CONVERSION_ERROR) TO_STRING( VAL, THIS%VAL, HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_CONVERSION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT8_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_INT16'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT16( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

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
  CLASS(ENCODE_STACK_MSG_NODE), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: MODE
  CHARACTER(LEN=*),             INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),             INTENT(IN)    :: KEY
  INTEGER(KIND=INT16),          INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERSION_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  PP_TRYCALL(ERRFLAG_CONVERSION_ERROR) TO_STRING( VAL, THIS%VAL, HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_CONVERSION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT16
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_INT16_ARRAY'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT16_ARRAY( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

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
  CLASS(ENCODE_STACK_MSG_NODE),      INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: MODE
  CHARACTER(LEN=*),                  INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT16), DIMENSION(:), INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERSION_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  PP_TRYCALL(ERRFLAG_CONVERSION_ERROR) TO_STRING( VAL, THIS%VAL, HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_CONVERSION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT16_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_INT32'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT32( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

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
  CLASS(ENCODE_STACK_MSG_NODE), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: MODE
  CHARACTER(LEN=*),             INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),             INTENT(IN)    :: KEY
  INTEGER(KIND=INT32),          INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERSION_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  PP_TRYCALL(ERRFLAG_CONVERSION_ERROR) TO_STRING( VAL, THIS%VAL, HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_CONVERSION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT32
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_INT32_ARRAY'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT32_ARRAY( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

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
  CLASS(ENCODE_STACK_MSG_NODE),      INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: MODE
  CHARACTER(LEN=*),                  INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT32), DIMENSION(:), INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERSION_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  PP_TRYCALL(ERRFLAG_CONVERSION_ERROR) TO_STRING( VAL, THIS%VAL, HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_CONVERSION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT32_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_INT64'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT64( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  CLASS(ENCODE_STACK_MSG_NODE), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: MODE
  CHARACTER(LEN=*),             INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),             INTENT(IN)    :: KEY
  INTEGER(KIND=INT64),          INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERSION_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  PP_TRYCALL(ERRFLAG_CONVERSION_ERROR) TO_STRING( VAL, THIS%VAL, HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_CONVERSION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_INT64_ARRAY'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT64_ARRAY( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  CLASS(ENCODE_STACK_MSG_NODE),      INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: MODE
  CHARACTER(LEN=*),                  INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT64), DIMENSION(:), INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERSION_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  PP_TRYCALL(ERRFLAG_CONVERSION_ERROR) TO_STRING( VAL, THIS%VAL, HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_CONVERSION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_INT64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_REAL32'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_REAL32( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

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
  CLASS(ENCODE_STACK_MSG_NODE), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: MODE
  CHARACTER(LEN=*),             INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),             INTENT(IN)    :: KEY
  REAL(KIND=REAL32),            INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERSION_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  PP_TRYCALL(ERRFLAG_CONVERSION_ERROR) TO_STRING( VAL, THIS%VAL, HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_CONVERSION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_REAL32
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_REAL32_ARRAY'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_REAL32_ARRAY( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

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
  CLASS(ENCODE_STACK_MSG_NODE),    INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: MODE
  CHARACTER(LEN=*),                INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL32), DIMENSION(:), INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERSION_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  PP_TRYCALL(ERRFLAG_CONVERSION_ERROR) TO_STRING( VAL, THIS%VAL, HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_CONVERSION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_REAL32_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_REAL64'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_REAL64( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

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
  CLASS(ENCODE_STACK_MSG_NODE), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: MODE
  CHARACTER(LEN=*),             INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),             INTENT(IN)    :: KEY
  REAL(KIND=REAL64),            INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERSION_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  PP_TRYCALL(ERRFLAG_CONVERSION_ERROR) TO_STRING( VAL, THIS%VAL, HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_CONVERSION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_REAL64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_STACK_MSG_NODE_INIT_REAL64_ARRAY'
PP_THREAD_SAFE FUNCTION ENCODE_STACK_MSG_NODE_INIT_REAL64_ARRAY( THIS, MODE, OPERATION, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

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
  CLASS(ENCODE_STACK_MSG_NODE),    INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: MODE
  CHARACTER(LEN=*),                INTENT(IN)    :: OPERATION
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL64), DIMENSION(:), INTENT(IN)    :: VAL
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERATION_TOO_LONG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODE_TOO_LONG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_TOO_LONG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERSION_ERROR=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(OPERATION)).GT.MAX_STR_LEN, ERRFLAG_OPERATION_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(MODE)).GT.MAX_STR_LEN, ERRFLAG_MODE_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(ADJUSTL(KEY)).GT.MAX_STR_LEN, ERRFLAG_KEY_TOO_LONG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%VAL), ERRFLAG_ALREADY_ALLOCATED_ERROR )

  ! Initialize the node
  THIS%OPERATION = REPEAT(' ',MAX_STR_LEN)
  THIS%MODE      = REPEAT(' ',MAX_STR_LEN)
  THIS%KEY       = REPEAT(' ',MAX_STR_LEN)

  ! Copy the input values
  THIS%OPERATION = TRIM(ADJUSTL(OPERATION))
  THIS%MODE      = TRIM(ADJUSTL(MODE))
  THIS%KEY       = TRIM(ADJUSTL(KEY))

  ! Copy the input array
  PP_TRYCALL(ERRFLAG_CONVERSION_ERROR) TO_STRING( VAL, THIS%VAL, HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_OPERATION_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operation string is too long' )
    CASE(ERRFLAG_MODE_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mode string is too long' )
    CASE(ERRFLAG_KEY_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key string is too long' )
    CASE(ERRFLAG_ALREADY_ALLOCATED_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value array is already allocated' )
    CASE(ERRFLAG_CONVERSION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION ENCODE_STACK_MSG_NODE_INIT_REAL64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE METADATA_TRACE_NODE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME




