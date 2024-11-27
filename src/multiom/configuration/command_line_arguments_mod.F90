! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'command_line_arguments_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'COMMAND_LINE_ARGUMENTS_MOD'
MODULE COMMAND_LINE_ARGUMENTS_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K

PRIVATE

!> @brief Enumerators for different command line argument types
INTEGER(KINd=JPIB_K), PARAMETER :: COMMAND_LINE_ARG_FLAG_E=1_JPIB_K
INTEGER(KINd=JPIB_K), PARAMETER :: COMMAND_LINE_ARG_KEY_VALUE_E=2_JPIB_K

!> @brief Parameters
INTEGER(KINd=JPIB_K), PARAMETER :: KEY_LENGTH=16_JPIB_K
INTEGER(KINd=JPIB_K), PARAMETER :: VAL_LENGTH=16_JPIB_K
INTEGER(KINd=JPIB_K), PARAMETER :: COMMAND_LINE_LENGTH=131072_JPIB_K
INTEGER(KINd=JPIB_K), PARAMETER :: TOKEN_LENGTH=8192_JPIB_K

!> @brief Type to store the command line arguments
TYPE :: COMMAND_LINE_T
  INTEGER(KIND=JPIB_K) :: TYPE=0
  CHARACTER(LEN=KEY_LENGTH) :: KEY=REPEAT(" ", KEY_LENGTH)
  CHARACTER(LEN=VAL_LENGTH), DIMENSION(:), ALLOCATABLE :: VALUE
END TYPE

! Whitelist of public symbols
PUBLIC :: COMMAND_LINE_T
PUBLIC :: INIT_COMMAND_LINE_ARGUMENTS

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INIT_COMMAND_LINE_ARGUMENTS'
PP_THREAD_SAFE FUNCTION INIT_COMMAND_LINE_ARGUMENTS( COMMAND_LINE_ARGS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(COMMAND_LINE_T), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: COMMAND_LINE_ARGS
  TYPE(HOOKS_T),                                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=COMMAND_LINE_LENGTH) :: FULL_COMMAND
  CHARACTER(LEN=TOKEN_LENGTH) :: ARG
  INTEGER(KIND=JPIB_K) :: NUM_ARGS
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: GET_ARG_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_NARGS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_COMMAND_LINE_ARGS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_NEXT_ARG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PARSE_ARGUMENT=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_COMMAND_ARGUMENT=5_JPIB_K

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

  ! Initialization of the full command line
  FULL_COMMAND=REPEAT(" ", COMMAND_LINE_LENGTH)

  ! Get the number of command-line arguments
  NUM_ARGS = COMMAND_ARGUMENT_COUNT()

  ! Combine all arguments into a single string
  DO I = 1, NUM_ARGS
    ARG = REPEAT(" ", TOKEN_LENGTH)
    CALL GET_COMMAND_ARGUMENT(I, ARG, STATUS=GET_ARG_STATUS)
    PP_DEBUG_CRITICAL_COND_THROW( GET_ARG_STATUS .NE. 0, ERRFLAG_UNABLE_TO_GET_COMMAND_ARGUMENT )
    FULL_COMMAND = TRIM(FULL_COMMAND) // " " // TRIM(ARG)
  END DO

  ! Get the "real" number of arguments
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NARGS) COUNT_ARGUMENTS(FULL_COMMAND, NUM_ARGS, HOOKS)

  ! Allocate the command line arguments
  ALLOCATE(COMMAND_LINE_ARGS(NUM_ARGS), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_COMMAND_LINE_ARGS )

  ! Parse the full command
  CNT = 0_JPIB_K
  DO WHILE( LEN_TRIM(FULL_COMMAND) .GT. 0)

    ! Update counter
    CNT = CNT + 1

    ! Extract the next argument
    ARG = REPEAT(" ", TOKEN_LENGTH)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_EXTRACT_NEXT_ARG) READ_NEXT_ARG( FULL_COMMAND, ARG, HOOKS )

    ! Determine the type of argument
    PP_TRYCALL(ERRFLAG_UNABLE_TO_PARSE_ARGUMENT) PARSE_ARGUMENT( ARG, COMMAND_LINE_ARGS(CNT), HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_GET_NARGS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the number of arguments' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE_COMMAND_LINE_ARGS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate the command line arguments' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: '//TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS )
      END IF
    CASE(ERRFLAG_UNABLE_TO_EXTRACT_NEXT_ARG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract the next argument' )
    CASE(ERRFLAG_UNABLE_TO_PARSE_ARGUMENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to parse the argument' )
    CASE(ERRFLAG_UNABLE_TO_GET_COMMAND_ARGUMENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the command argument' )
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

END FUNCTION INIT_COMMAND_LINE_ARGUMENTS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARSE_ARGUMENT'
PP_THREAD_SAFE FUNCTION PARSE_ARGUMENT( STR, ARG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: STR
  TYPE(COMMAND_LINE_T), INTENT(OUT)   :: ARG
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: POS
  LOGICAL :: IS_FLAG
  LOGICAL :: IS_KEY_VALUE

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_COMMAND_LINE_ARGUMENT_TYPE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_VALUES=2_JPIB_K

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

  ! Initialization of the argument type
  IS_FLAG = INDEX(STR, "=") .LE. 0_JPIB_K
  IS_KEY_VALUE = INDEX(STR, "=") .GT. 0_JPIB_K

  ! Fill the command line argument
  IF ( IS_FLAG ) THEN

    ARG%TYPE = COMMAND_LINE_ARG_FLAG_E
    ARG%KEY = STR

  ELSE IF ( IS_KEY_VALUE ) THEN

    ARG%TYPE = COMMAND_LINE_ARG_KEY_VALUE_E
    POS = INDEX(STR, "=")
    ARG%KEY = STR(1:POS-1)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VALUES) PARSE_VALUES( TRIM(ADJUSTL(STR(POS+1:))), ARG%VALUE, HOOKS )

  ELSE

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_COMMAND_LINE_ARGUMENT_TYPE )

  END IF

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
    CASE(ERRFLAG_UNKNOWN_COMMAND_LINE_ARGUMENT_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown command line argument type' )
    CASE(ERRFLAG_UNABLE_TO_READ_VALUES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read values' )
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

END FUNCTION PARSE_ARGUMENT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARSE_VALUES'
PP_THREAD_SAFE FUNCTION PARSE_VALUES( STR, VAL, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),                                     INTENT(IN)    :: STR
  CHARACTER(LEN=VAL_LENGTH), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: VAL
  TYPE(HOOKS_T),                                        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=LEN(STR)) :: VALUES
  CHARACTER(LEN=16) :: TMP
  LOGICAL :: IS_ARRAY
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: N_VALUES
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IS_ARRAY=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_NUM_VALUES=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_NEXT_VALUE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_ARRAY=4_JPIB_K

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

  ! Check if it is an array of scalar
  PP_TRYCALL(ERRFLAG_IS_ARRAY) CHECK_IS_ARRAY( STR, IS_ARRAY, HOOKS )

  IF ( .NOT. IS_ARRAY ) THEN

    ! Allocate the array
    ALLOCATE(VAL(1), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_ARRAY )
    VAL(1) = REPEAT(" ", VAL_LENGTH)
    VAL(1) = TRIM(ADJUSTL(STR))

  ELSE

    ! Get number of values
    PP_TRYCALL(ERRFLAG_GET_NUM_VALUES) COUNT_VALUES( STR, N_VALUES, HOOKS )

    ! Allocate the array
    ALLOCATE(VAL(N_VALUES), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_ARRAY )

    ! Parse the values
    CNT = 0
    VALUES = STR
    DO WHILE(  LEN_TRIM(VALUES) .GT. 0 )

      ! Initialization of the temporary variable
      TMP = REPEAT(" ", VAL_LENGTH)

      ! Extract the next value
      PP_TRYCALL(ERRFLAG_EXTRACT_NEXT_VALUE) READ_NEXT_VALUE( VALUES, TMP, HOOKS )

      ! Update counter
      CNT = CNT + 1

      ! Store the value
      VAL(CNT) = REPEAT(" ", VAL_LENGTH)
      VAL(CNT) = TRIM(ADJUSTL(TMP))

    ENDDO

  END IF

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
    CASE(ERRFLAG_IS_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call is_array' )
    CASE(ERRFLAG_GET_NUM_VALUES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the number of values' )
    CASE(ERRFLAG_EXTRACT_NEXT_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract the next value' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate the array' )
      IF ( ALLOCATED(ERRMSG) ) THEN
          PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: '//TRIM(ERRMSG) )
          DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS )
      END IF
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

END FUNCTION PARSE_VALUES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COUNT_ARGUMENTS'
PP_THREAD_SAFE FUNCTION COUNT_ARGUMENTS( STR, N, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: STR
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: N
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  LOGICAL :: INSIDE_ARRAY

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

  ! Initialization of the number of arguments
  N = 0_JPIB_K
  INSIDE_ARRAY = .FALSE.

  ! Count the number of arguments
  N = 0_JPIB_K
  INSIDE_ARRAY = .FALSE.
  DO I = 1, LEN_TRIM(STR)
    IF ( STR(I:I) .EQ. '[' ) THEN
      INSIDE_ARRAY = .TRUE.
    ELSEIF ( STR(I:I) .EQ. ']' ) THEN
      INSIDE_ARRAY = .FALSE.
    ELSEIF ( STR(I:I) .EQ. "-" .AND. .NOT. INSIDE_ARRAY ) THEN
      N = N + 1
    END IF
  END DO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION COUNT_ARGUMENTS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COUNT_VALUES'
PP_THREAD_SAFE FUNCTION COUNT_VALUES( STR, N, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: STR
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: N
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

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

  ! Initialization of the number of values
  N = 0_JPIB_K

  ! Count the number of values
  DO I = 1, LEN_TRIM(STR)
    IF ( STR(I:I) .EQ. "," ) THEN
      N = N + 1
    END IF
  END DO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION COUNT_VALUES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CHECK_IS_ARRAY'
PP_THREAD_SAFE FUNCTION CHECK_IS_ARRAY( STR, IS_ARRAY, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*), INTENT(IN)    :: STR
  LOGICAL,          INTENT(OUT)   :: IS_ARRAY
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL, DIMENSION(4) :: IS_ARRAY_CONDITIONS
  INTEGER(KIND=JPIB_K) :: I
  CHARACTER(LEN=LEN_TRIM(STR)) :: TMP
  INTEGER(KIND=JPIB_K) :: LO_POS
  INTEGER(KIND=JPIB_K) :: HI_POS
  INTEGER(KIND=JPIB_K) :: SEP_POS

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

  ! Initialization of the number of values
  TMP = REPEAT( ' ', LEN(TMP) )
  TMP = TRIM(ADJUSTL(STR))

  ! Get the special characters
  LO_POS = INDEX(TMP, "[")
  HI_POS = INDEX(TMP, "]")
  SEP_POS = INDEX(TMP, ",")

  ! Conditions to check if it is an array
  IS_ARRAY_CONDITIONS(1) = LO_POS .EQ. 1
  IS_ARRAY_CONDITIONS(2) = HI_POS .EQ. LEN_TRIM(TMP)
  IS_ARRAY_CONDITIONS(3) = SEP_POS .GE. 1
  IS_ARRAY_CONDITIONS(4) = SEP_POS .LE. LEN_TRIM(TMP)

  ! Initialization
  IS_ARRAY = ALL( IS_ARRAY_CONDITIONS )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION CHECK_IS_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_NEXT_ARG'
PP_THREAD_SAFE FUNCTION READ_NEXT_ARG( CMD_ARGS, NEXT_ARG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*), INTENT(INOUT) :: CMD_ARGS
  CHARACTER(LEN=*), INTENT(OUT)   :: NEXT_ARG
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: POS
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  LOGICAL :: INSIDE_ARRAY

  ! Local states
  INTEGER(KIND=JPIB_K), PARAMETER :: SEARCH_ARG_START=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: SEARCH_KEY_START=1_JPIB_K

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

  ! Initialization of the next argument
  NEXT_ARG = REPEAT(" ", LEN(NEXT_ARG))
  DO WHILE ( LEN_TRIM(NEXT_ARG) .EQ. 0 )

    ! Find the position of the next space
    POS = INDEX(TRIM(CMD_ARGS), "--")

    IF (POS .EQ. 0) THEN

      ! No more spaces: return the entire command as the next argument
      NEXT_ARG = TRIM(CMD_ARGS)
      CMD_ARGS = ""

    ELSE

      ! Extract the next argument and update the command
      NEXT_ARG = TRIM(CMD_ARGS(1:POS-1))
      CMD_ARGS = TRIM(CMD_ARGS(POS+2:))

    ENDIF

  ENDDO
#if 0
  LO = -1_JPIB_K
  HI = -1_JPIB_K
  CNT = 1_JPIB_K
  CURR_STATE = SEARCH_ARG_START
  DO

    SELECT CASE( CMD_ARGS(CNT:CNT) )

    CASE ( SEARCH_ARG_START )
      IF ( CMD_ARGS(CNT:CNT) .EQ. '-' ) THEN
        CURR_STATE = SEARCH_KEY_START
      ELSE
        CNT = CNT + 1
      END IF

    CASE ( SEARCH_KEY_START )
      IF ( CMD_ARGS(CNT:CNT) .NE. '-' ) THEN
        LO = CNT
        CURR_STATE = SEARCH_KEY_END
      ELSE
        CNT = CNT + 1
      END IF

    CASE ( SEARCH_KEY_END )
      IF ( CMD_ARGS(CNT:CNT) .EQ. '=' .OR. CMD_ARGS(CNT:CNT) .EQ. ' ' ) THEN
        HI = CNT
        CURR_STATE = SEARCH_VALUE
      ELSE
        CNT = CNT + 1
      END IF

    CASE ( SEARCH_VALUE )
      IF ( CMD_ARGS(CNT:CNT) .EQ. '=' ) THEN ! Flag has value
        CNT = CNT + 1
        CURR_STATE = SEARCH_VALUE_START
      ELSEIF ( CMD_ARGS(CNT:CNT) .EQ. '-' ) THEN ! Flag has no value
        CURR_STATE = SEARCH_EXIT
      ELSEIF ( CMD_ARGS(CNT:CNT) .EQ. ' ' ) THEN ! Look for next relevant entry
        CNT = CNT + 1
      ELSE ! Some error
        ! ERRROR
      END IF

    CASE ( SEARCH_VALUE_START )
      IF ( CMD_ARGS(CNT:CNT) .NE. '-' .AND. CMD_ARGS(CNT:CNT) .NE. ' ' ) THEN
        CURR_STATE = IDENTIFY_VALUE_START
      ELSEIF ( CMD_ARGS(CNT:CNT) .EQ. '-' ) THEN
        CURR_STATE = SEARCH_EXIT
      ELSE
        CNT = CNT + 1
      END IF

    CASE ( IDENTIFY_VALUE_START )
      IF ( CMD_ARGS(CNT:CNT) .EQ. '[' ) THEN
        INSIDE_ARRAY = .TRUE.
        CNT = CNT + 1
        CURR_STATE = SEARCH_ARRAY_END
      ELSE
        CNT = CNT + 1
        CURR_STATE = SEARCH_VALUE_END
      END IF

    CASE (  SEARCH_ARRAY_END )
      IF ( CMD_ARGS(CNT:CNT) .EQ. ']' ) THEN
        INSIDE_ARRAY = .FALSE.
        CNT = CNT + 1
        CURR_STATE = SEARCH_EXIT
      ELSE
        CNT = CNT + 1
      END IF
    CASE DEFAULT
      ! ERROR

    END SELECT

  ENDDO
#endif

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION READ_NEXT_ARG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_NEXT_VALUE'
PP_THREAD_SAFE FUNCTION READ_NEXT_VALUE( VALUES, NEXT_VAL, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*), INTENT(INOUT) :: VALUES
  CHARACTER(LEN=*), INTENT(OUT)   :: NEXT_VAL
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: POS

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

  ! Initialization of the next argument
  NEXT_VAL = REPEAT(" ", LEN(NEXT_VAL))
  DO WHILE ( LEN_TRIM(NEXT_VAL) .EQ. 0 )

    ! Find the position of the next space
    POS = INDEX(TRIM(VALUES), ",")

    IF (POS .EQ. 0) THEN

      ! No more spaces: return the entire command as the next argument
      NEXT_VAL = TRIM(VALUES)
      VALUES = ""

    ELSE

      ! Extract the next argument and update the command
      NEXT_VAL = TRIM(VALUES(1:POS-1))
      VALUES = TRIM(VALUES(POS+2:))

    ENDIF

  ENDDO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION READ_NEXT_VALUE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE COMMAND_LINE_ARGUMENTS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME