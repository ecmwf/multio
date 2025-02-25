! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'command_arguments_tokenizer_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'COMMAND_ARGUMENTS_TOKENIZER_MOD'
MODULE COMMAND_ARGUMENTS_TOKENIZER_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

! Default visibility
PRIVATE

! Maximum length of the string
INTEGER(KIND=JPIB_K), PARAMETER :: MAX_STRLEN=32_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARGSTRING_LEN=256_JPIB_K

TYPE :: OPTIONS_T
  INTEGER(KIND=JPIB_K) :: MODIFIER=0
  CHARACTER(LEN=MAX_STRLEN) :: NAME=REPEAT(' ',MAX_STRLEN)
  LOGICAL :: MATCH = .TRUE.
  CHARACTER(LEN=MAX_STRLEN), DIMENSION(:), POINTER :: VALUES => NULL()
ENDTYPE

TYPE :: OPTIONS_NODE_T
  TYPE(OPTIONS_T) :: DATA_
  TYPE(OPTIONS_NODE_T), POINTER :: NEXT => NULL()
  TYPE(OPTIONS_NODE_T), POINTER :: PREV => NULL()
ENDTYPE


TYPE :: ARG_T
  CHARACTER(LEN=MAX_STRLEN) :: VAL=REPEAT(' ',MAX_STRLEN)
  TYPE(ARG_T), POINTER :: NEXT => NULL()
  TYPE(ARG_T), POINTER :: PREV => NULL()
ENDTYPE


TYPE :: OPTIONS_LIST_T
  INTEGER(KIND=JPIB_K) :: SIZE = 0_JPIB_K
  TYPE(OPTIONS_NODE_T), POINTER :: HEAD => NULL()
  TYPE(OPTIONS_NODE_T), POINTER :: TAIL => NULL()
ENDTYPE

TYPE :: ARG_LIST_T
  INTEGER(KIND=JPIB_K) :: SIZE = 0_JPIB_K
  TYPE(ARG_T), POINTER :: HEAD => NULL()
  TYPE(ARG_T), POINTER :: TAIL => NULL()
ENDTYPE


! Main class containing command line tokens
TYPE :: COMMAND_ARGUMENTS_TOKENIZER_T

  ! Default visibility
  PRIVATE

  ! Command arguments array
  TYPE(OPTIONS_T), DIMENSION(:), ALLOCATABLE :: ARGUMENTS_

CONTAINS

  PROCEDURE, PUBLIC, PASS :: INIT         => COMMAND_ARGUMENTS_TOKENIZER_INIT
  PROCEDURE, PUBLIC, PASS :: INITIALIZED  => COMMAND_ARGUMENTS_TOKENIZER_INITIALIZED
  PROCEDURE, PUBLIC, PASS :: SIZE         => COMMAND_ARGUMENTS_TOKENIZER_SIZE
  PROCEDURE, PUBLIC, PASS :: PRINT        => COMMAND_ARGUMENTS_TOKENIZER_PRINT
  PROCEDURE, PUBLIC, PASS :: GET_MODIFIER => COMMAND_ARGUMENTS_TOKENIZER_GET_MODIFIER
  PROCEDURE, PUBLIC, PASS :: GET_NAME     => COMMAND_ARGUMENTS_TOKENIZER_GET_NAME
  PROCEDURE, PUBLIC, PASS :: GET_MATCH    => COMMAND_ARGUMENTS_TOKENIZER_GET_MATCH
  PROCEDURE, PUBLIC, PASS :: GET_VALS     => COMMAND_ARGUMENTS_TOKENIZER_GET_VALUES
  PROCEDURE, PUBLIC, PASS :: FREE         => COMMAND_ARGUMENTS_TOKENIZER_FREE

END TYPE

! Whitelist of public symbols
PUBLIC :: COMMAND_ARGUMENTS_TOKENIZER_T
PUBLIC :: MAX_STRLEN

CONTAINS




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_ARGUMENTS_TOKENIZER_INIT'
PP_THREAD_SAFE FUNCTION COMMAND_ARGUMENTS_TOKENIZER_INIT( THIS, HOOKS ) RESULT(RET)

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
  CLASS(COMMAND_ARGUMENTS_TOKENIZER_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: MATCH
  LOGICAL :: EOC
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: ARGCOUNT
  INTEGER(KIND=JPIB_K), DIMENSION(2) :: COMMAND_ARGID
  INTEGER(KIND=JPIB_K) :: MODIFIER_COUNT
  TYPE(OPTIONS_LIST_T) :: OPTIONS
  TYPE(OPTIONS_NODE_T), POINTER :: CURR
  TYPE(OPTIONS_NODE_T), POINTER :: PREV
  CHARACTER(LEN=MAX_STRLEN) :: NAME
  CHARACTER(LEN=MAX_STRLEN), DIMENSION(:), POINTER :: VALUES
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_NEXT_ARG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=5_JPIB_K

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

  ! Initialization
  EOC = .FALSE.
  COMMAND_ARGID = [ 1_JPIB_K, 1_JPIB_K ]

  ! Check the status
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%ARGUMENTS_), ERRFLAG_ALREADY_ALLOCATED )

  ! Get number of arguments
  ARGCOUNT = COMMAND_ARGUMENT_COUNT()

  ! Check if the number of arguments is bigger than 0
  IF ( ARGCOUNT .GT. 0 ) THEN

    !
    ! Tokenize command line arguments
    DO WHILE ( .NOT. EOC )

      ! Initialization
      MODIFIER_COUNT = -1_JPIB_K
      NAME = REPEAT( ' ', MAX_STRLEN )
      MATCH = .TRUE.
      VALUES => NULL()

      ! Read the next command line argument
      PP_TRYCALL(ERRFLAG_READ_NEXT_ARG) READ_NEXT_ARG( COMMAND_ARGID, MODIFIER_COUNT, NAME, MATCH, VALUES, EOC, HOOKS )

      ! Push argument to a list
      IF ( .NOT.ASSOCIATED(OPTIONS%HEAD) ) THEN
        ALLOCATE( OPTIONS%HEAD, STAT=STAT, ERRMSG=ERRMSG )
        PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE )
        OPTIONS%SIZE = OPTIONS%SIZE + 1
        OPTIONS%TAIL => OPTIONS%HEAD
      ELSE
        ALLOCATE( OPTIONS%TAIL%NEXT, STAT=STAT, ERRMSG=ERRMSG )
        PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE )
        OPTIONS%SIZE = OPTIONS%SIZE + 1
        OPTIONS%TAIL%NEXT%PREV => OPTIONS%TAIL
        OPTIONS%TAIL => OPTIONS%TAIL%NEXT
      ENDIF
      OPTIONS%TAIL%DATA_%MODIFIER = MODIFIER_COUNT
      OPTIONS%TAIL%DATA_%NAME = TRIM(ADJUSTL(NAME))
      OPTIONS%TAIL%DATA_%MATCH = MATCH
      OPTIONS%TAIL%DATA_%VALUES => VALUES

    ENDDO

    !
    ! Allocate the arrat with all the command line options
    ALLOCATE(THIS%ARGUMENTS_(OPTIONS%SIZE), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE )

    !
    ! Copy command line arguments to an array
    PREV => NULL()
    CURR => OPTIONS%HEAD
    I = 0
    DO WHILE( ASSOCIATED(CURR) )
      I = I + 1_JPIB_K
      PP_DEBUG_CRITICAL_COND_THROW( I.GT.SIZE(THIS%ARGUMENTS_), ERRFLAG_OUT_OF_BOUNDS )
      THIS%ARGUMENTS_(I)%MODIFIER = CURR%DATA_%MODIFIER
      THIS%ARGUMENTS_(I)%NAME = CURR%DATA_%NAME
      THIS%ARGUMENTS_(I)%MATCH = CURR%DATA_%MATCH
      THIS%ARGUMENTS_(I)%VALUES => CURR%DATA_%VALUES
      PREV => CURR
      CURR => CURR%NEXT
      PREV%DATA_%MODIFIER = -1_JPIB_K
      PREV%DATA_%MATCH = .TRUE.
      PREV%DATA_%NAME = REPEAT(' ',MAX_STRLEN)
      PREV%DATA_%VALUES => NULL()
      DEALLOCATE(PREV, STAT=STAT, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE )
    ENDDO

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
    CASE (ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command line arguments already allocated' )
    CASE (ERRFLAG_READ_NEXT_ARG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read next arguments' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument out of bounds' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=STAT )
      ENDIF
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

END FUNCTION COMMAND_ARGUMENTS_TOKENIZER_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_ARGUMENTS_TOKENIZER_INITIALIZED'
PP_THREAD_SAFE FUNCTION COMMAND_ARGUMENTS_TOKENIZER_INITIALIZED( THIS, INITIALIZED, HOOKS ) RESULT(RET)

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
  CLASS(COMMAND_ARGUMENTS_TOKENIZER_T), INTENT(INOUT) :: THIS
  LOGICAL,                              INTENT(OUT)   :: INITIALIZED
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INCONSISTENT_STATE=1_JPIB_K

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

  ! Check if the class is initialized
  IF ( COMMAND_ARGUMENT_COUNT() .GT. 0 .AND. .NOT.ALLOCATED(THIS%ARGUMENTS_) ) THEN
    INITIALIZED = .FALSE.
  ELSEIF ( COMMAND_ARGUMENT_COUNT() .GT. 0 .AND. ALLOCATED(THIS%ARGUMENTS_) ) THEN
    INITIALIZED = .TRUE.
  ELSEIF ( COMMAND_ARGUMENT_COUNT() .EQ. 0 .AND. .NOT.ALLOCATED(THIS%ARGUMENTS_) ) THEN
    INITIALIZED = .TRUE.
  ELSE
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INCONSISTENT_STATE )
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
    CASE (ERRFLAG_INCONSISTENT_STATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Inconsistent state' )
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

END FUNCTION COMMAND_ARGUMENTS_TOKENIZER_INITIALIZED
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_ARGUMENTS_TOKENIZER_SIZE'
PP_THREAD_SAFE FUNCTION COMMAND_ARGUMENTS_TOKENIZER_SIZE( THIS, SZ, HOOKS ) RESULT(RET)

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
  CLASS(COMMAND_ARGUMENTS_TOKENIZER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                 INTENT(OUT)   :: SZ
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_INITIALIZED

  ! Local variables
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=2_JPIB_K

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

  ! Check initialization
  PP_TRYCALL(ERRFLAG_CALL_INITIALIZED) THIS%INITIALIZED( IS_INITIALIZED, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INITIALIZED, ERRFLAG_NOT_INITIALIZED )

  ! Check if the class is initialized
  IF ( ALLOCATED(THIS%ARGUMENTS_) ) THEN
    SZ = SIZE(THIS%ARGUMENTS_)
  ELSE
    SZ = 0_JPIB_K
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
    CASE (ERRFLAG_CALL_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check initialization status' )
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Class not initialized' )
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

END FUNCTION COMMAND_ARGUMENTS_TOKENIZER_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_ARGUMENTS_TOKENIZER_PRINT'
PP_THREAD_SAFE FUNCTION COMMAND_ARGUMENTS_TOKENIZER_PRINT( THIS, UNIT, HOOKS ) RESULT(RET)

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
  CLASS(COMMAND_ARGUMENTS_TOKENIZER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                 INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_INITIALIZED
  INTEGER(KIND=JPIB_K) :: SZ
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IOERROR=3_JPIB_K

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

  ! Check initialization
  PP_TRYCALL(ERRFLAG_CALL_INITIALIZED) THIS%INITIALIZED( IS_INITIALIZED, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INITIALIZED, ERRFLAG_NOT_INITIALIZED )

  ! Check if the class is initialized
  IF ( ALLOCATED(THIS%ARGUMENTS_) ) THEN
    SZ = SIZE(THIS%ARGUMENTS_)
    DO I = 1, SZ
      WRITE(UNIT,'(A,I2,A,I2)',IOSTAT=STAT) 'Argument ', I, ': Modifier: ', THIS%ARGUMENTS_(I)%MODIFIER
      PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_IOERROR )
      WRITE(UNIT,'(A,I2,A,A)',IOSTAT=STAT) 'Argument ', I, ': Name:     ', TRIM(ADJUSTL(THIS%ARGUMENTS_(I)%NAME))
      PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_IOERROR )
      WRITE(UNIT,'(A,I2,A,L)',IOSTAT=STAT) 'Argument ', I, ': Match:    ', THIS%ARGUMENTS_(I)%MATCH
      PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_IOERROR )
      IF ( ASSOCIATED(THIS%ARGUMENTS_(I)%VALUES) ) THEN
        WRITE(UNIT,'(A,I2,A)', ADVANCE='NO',IOSTAT=STAT) 'Argument ', I, ': Values:   ['
        PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_IOERROR )
        DO J = 1, SIZE(THIS%ARGUMENTS_(I)%VALUES)
          IF ( J .NE. SIZE(THIS%ARGUMENTS_(I)%VALUES) ) THEN
            IF ( MOD( J-1, 10 ) .EQ. 0 .AND. J.GT.1) THEN
              WRITE(UNIT,'(A)',IOSTAT=STAT) TRIM(ADJUSTL(THIS%ARGUMENTS_(I)%VALUES(J)))//', '
              PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_IOERROR )
            ELSE
              WRITE(UNIT,'(A)', ADVANCE='NO',IOSTAT=STAT) TRIM(ADJUSTL(THIS%ARGUMENTS_(I)%VALUES(J)))//', '
              PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_IOERROR )
            ENDIF
          ELSE
            WRITE(UNIT,'(A)',IOSTAT=STAT) TRIM(ADJUSTL(THIS%ARGUMENTS_(I)%VALUES(J)))//']'
            PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_IOERROR )
          ENDIF
        ENDDO
      ENDIF
      WRITE(UNIT,'(A)',IOSTAT=STAT) '-----------------------------------------------------------'
    END DO
  ELSE
    SZ = 0_JPIB_K
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
    CASE (ERRFLAG_CALL_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check initialization status' )
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Class not initialized' )
    CASE (ERRFLAG_IOERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'I/O error' )
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

END FUNCTION COMMAND_ARGUMENTS_TOKENIZER_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_ARGUMENTS_TOKENIZER_GET_MODIFIER'
PP_THREAD_SAFE FUNCTION COMMAND_ARGUMENTS_TOKENIZER_GET_MODIFIER( THIS, ID, MODIFIER_COUNT, HOOKS ) RESULT(RET)

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
  CLASS(COMMAND_ARGUMENTS_TOKENIZER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                 INTENT(IN)    :: ID
  INTEGER(KIND=JPIB_K),                 INTENT(OUT)   :: MODIFIER_COUNT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_INITIALIZED

  ! Local variables
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS=3_JPIB_K

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

  ! Initialization
  MODIFIER_COUNT = -1_JPIB_K

  ! Check initialization
  PP_TRYCALL(ERRFLAG_CALL_INITIALIZED) THIS%INITIALIZED( IS_INITIALIZED, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INITIALIZED, ERRFLAG_NOT_INITIALIZED )

  ! Check if the class is initialized
  IF ( ALLOCATED(THIS%ARGUMENTS_) ) THEN
    PP_DEBUG_CRITICAL_COND_THROW( ID.LT.1, ERRFLAG_OUT_OF_BOUNDS )
    PP_DEBUG_CRITICAL_COND_THROW( ID.GT.SIZE(THIS%ARGUMENTS_), ERRFLAG_OUT_OF_BOUNDS )
    MODIFIER_COUNT = THIS%ARGUMENTS_(ID)%MODIFIER
  ELSE
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_OUT_OF_BOUNDS )
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
    CASE (ERRFLAG_CALL_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check initialization status' )
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Class not initialized' )
    CASE (ERRFLAG_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Out of bounds' )
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

END FUNCTION COMMAND_ARGUMENTS_TOKENIZER_GET_MODIFIER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_ARGUMENTS_TOKENIZER_GET_NAME'
PP_THREAD_SAFE FUNCTION COMMAND_ARGUMENTS_TOKENIZER_GET_NAME( THIS, ID, NAME, HOOKS ) RESULT(RET)

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
  CLASS(COMMAND_ARGUMENTS_TOKENIZER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                 INTENT(IN)    :: ID
  CHARACTER(LEN=MAX_STRLEN),            INTENT(OUT)   :: NAME
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_INITIALIZED

  ! Local variables
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS=3_JPIB_K

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

  ! Initialization
  NAME = REPEAT( ' ', MAX_STRLEN )

  ! Check initialization
  PP_TRYCALL(ERRFLAG_CALL_INITIALIZED) THIS%INITIALIZED( IS_INITIALIZED, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INITIALIZED, ERRFLAG_NOT_INITIALIZED )

  ! Check if the class is initialized
  IF ( ALLOCATED(THIS%ARGUMENTS_) ) THEN
    PP_DEBUG_CRITICAL_COND_THROW( ID.LT.1, ERRFLAG_OUT_OF_BOUNDS )
    PP_DEBUG_CRITICAL_COND_THROW( ID.GT.SIZE(THIS%ARGUMENTS_), ERRFLAG_OUT_OF_BOUNDS )
    NAME = TRIM(ADJUSTL(THIS%ARGUMENTS_(ID)%NAME))
  ELSE
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_OUT_OF_BOUNDS )
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
    CASE (ERRFLAG_CALL_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check initialization status' )
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Class not initialized' )
    CASE (ERRFLAG_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Out of bounds' )
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

END FUNCTION COMMAND_ARGUMENTS_TOKENIZER_GET_NAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_ARGUMENTS_TOKENIZER_GET_MATCH'
PP_THREAD_SAFE FUNCTION COMMAND_ARGUMENTS_TOKENIZER_GET_MATCH( THIS, ID, MATCH, HOOKS ) RESULT(RET)

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
  CLASS(COMMAND_ARGUMENTS_TOKENIZER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                 INTENT(IN)    :: ID
  LOGICAL,                              INTENT(OUT)   :: MATCH
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_INITIALIZED

  ! Local variables
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS=3_JPIB_K

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

  ! Initialization
  MATCH = .TRUE.

  ! Check initialization
  PP_TRYCALL(ERRFLAG_CALL_INITIALIZED) THIS%INITIALIZED( IS_INITIALIZED, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INITIALIZED, ERRFLAG_NOT_INITIALIZED )

  ! Check if the class is initialized
  IF ( ALLOCATED(THIS%ARGUMENTS_) ) THEN
    PP_DEBUG_CRITICAL_COND_THROW( ID.LT.1, ERRFLAG_OUT_OF_BOUNDS )
    PP_DEBUG_CRITICAL_COND_THROW( ID.GT.SIZE(THIS%ARGUMENTS_), ERRFLAG_OUT_OF_BOUNDS )
    MATCH = THIS%ARGUMENTS_(ID)%MATCH
  ELSE
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_OUT_OF_BOUNDS )
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
    CASE (ERRFLAG_CALL_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check initialization status' )
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Class not initialized' )
    CASE (ERRFLAG_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Out of bounds' )
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

END FUNCTION COMMAND_ARGUMENTS_TOKENIZER_GET_MATCH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_ARGUMENTS_TOKENIZER_GET_VALUES'
PP_THREAD_SAFE FUNCTION COMMAND_ARGUMENTS_TOKENIZER_GET_VALUES( THIS, ID, VALUES, HOOKS ) RESULT(RET)

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
  CLASS(COMMAND_ARGUMENTS_TOKENIZER_T),             INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                             INTENT(IN)    :: ID
  CHARACTER(LEN=MAX_STRLEN), DIMENSION(:), POINTER, INTENT(OUT)   :: VALUES
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_INITIALIZED

  ! Local variables
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS=3_JPIB_K

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

  ! Initialization
  VALUES => NULL()

  ! Check initialization
  PP_TRYCALL(ERRFLAG_CALL_INITIALIZED) THIS%INITIALIZED( IS_INITIALIZED, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INITIALIZED, ERRFLAG_NOT_INITIALIZED )

  ! Check if the class is initialized
  IF ( ALLOCATED(THIS%ARGUMENTS_) ) THEN
    PP_DEBUG_CRITICAL_COND_THROW( ID.LT.1, ERRFLAG_OUT_OF_BOUNDS )
    PP_DEBUG_CRITICAL_COND_THROW( ID.GT.SIZE(THIS%ARGUMENTS_), ERRFLAG_OUT_OF_BOUNDS )
    VALUES = THIS%ARGUMENTS_(ID)%VALUES
  ELSE
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_OUT_OF_BOUNDS )
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
    CASE (ERRFLAG_CALL_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check initialization status' )
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Class not initialized' )
    CASE (ERRFLAG_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Out of bounds' )
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

END FUNCTION COMMAND_ARGUMENTS_TOKENIZER_GET_VALUES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_ARGUMENTS_TOKENIZER_FREE'
PP_THREAD_SAFE FUNCTION COMMAND_ARGUMENTS_TOKENIZER_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(COMMAND_ARGUMENTS_TOKENIZER_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=1_JPIB_K

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

  ! Reset args and deallocate values
  IF ( ALLOCATED(THIS%ARGUMENTS_) ) THEN
    DO I = 1, SIZE(THIS%ARGUMENTS_)
      THIS%ARGUMENTS_(I)%MODIFIER = -1_JPIB_K
      THIS%ARGUMENTS_(I)%MATCH = .TRUE.
      THIS%ARGUMENTS_(I)%NAME = REPEAT(' ',MAX_STRLEN)
      IF ( ASSOCIATED(THIS%ARGUMENTS_(I)%VALUES) ) THEN
        DEALLOCATE(THIS%ARGUMENTS_(I)%VALUES, STAT=STAT, ERRMSG=ERRMSG)
        PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE )
        THIS%ARGUMENTS_(I)%VALUES => NULL()
      ENDIF
    ENDDO
    DEALLOCATE(THIS%ARGUMENTS_, STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE )
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
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=STAT )
      ENDIF
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

END FUNCTION COMMAND_ARGUMENTS_TOKENIZER_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'POP_CHAR'
PP_THREAD_SAFE FUNCTION POP_CHAR( COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), DIMENSION(2), INTENT(INOUT) :: COMMAND_ARGID
  CHARACTER,                          INTENT(OUT)   :: CHAR_VALUE
  LOGICAL,                            INTENT(OUT)   :: EOC
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: ARGLEN
  INTEGER(KIND=JPIB_K) :: ARGCOUNT
  CHARACTER(LEN=ARGSTRING_LEN) :: ARGSTRING
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID1_OUT_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID2_OUT_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_COMMAND_ARG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGUMENT_TOO_LONG=4_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).LT.0, ERRFLAG_ARGID1_OUT_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(2).LT.0, ERRFLAG_ARGID2_OUT_BOUNDS )

  ! Get number of arguments
  ARGCOUNT = COMMAND_ARGUMENT_COUNT()
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).GT.ARGCOUNT, ERRFLAG_ARGID1_OUT_BOUNDS )

  ! Check if all arguments are read
  IF (COMMAND_ARGID(1) .LE. ARGCOUNT) THEN

    ! Initialize arg string
    ARGSTRING = REPEAT(' ',ARGSTRING_LEN)

    ! Retrieve the current argument
    CALL GET_COMMAND_ARGUMENT(COMMAND_ARGID(1), ARGSTRING, LENGTH=ARGLEN, STATUS=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_COMMAND_ARG )
    PP_DEBUG_CRITICAL_COND_THROW( ARGLEN.GT.ARGSTRING_LEN, ERRFLAG_ARGUMENT_TOO_LONG )

    ! Extract the next character
    IF ( COMMAND_ARGID(2) .LE. ARGLEN ) THEN
      CHAR_VALUE = ARGSTRING(COMMAND_ARGID(2):COMMAND_ARGID(2))
      COMMAND_ARGID(2) = COMMAND_ARGID(2) + 1_JPIB_K
    ELSE
      ! Move to the next argument
      COMMAND_ARGID = COMMAND_ARGID + 1_JPIB_K
      COMMAND_ARGID(2) = 1_JPIB_K

      ! Return an empty string
      CHAR_VALUE = ' '
    ENDIF

  ENDIF

  ! Check if all arguments are processed
  IF (COMMAND_ARGID(1) .GT. ARGCOUNT) THEN
    EOC = .TRUE.
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

    CHARACTER(LEN=ARGSTRING_LEN) :: TMP1

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ARGID1_OUT_BOUNDS)
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument Id out of bounds' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_ARGID2_OUT_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument string idx out of bounds' )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument string idx: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_UNABLE_TO_READ_COMMAND_ARG)
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read command line argument' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG,STAT=STAT)
      ENDIF
    CASE (ERRFLAG_ARGUMENT_TOO_LONG)
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command line argument too long (input truncated)' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Truncated: "'//TRIM(ADJUSTL(ARGSTRING))//'"' )
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

END FUNCTION POP_CHAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PUSH_CHAR'
PP_THREAD_SAFE FUNCTION PUSH_CHAR( COMMAND_ARGID, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), DIMENSION(2), INTENT(INOUT) :: COMMAND_ARGID
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ARGCOUNT
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: ARGLEN
  CHARACTER(LEN=ARGSTRING_LEN) :: ARGSTRING
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID1_OUT_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID2_OUT_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_COMMAND_ARG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGUMENT_TOO_LONG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_ARG_ID=5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).LT.0, ERRFLAG_ARGID1_OUT_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(2).LT.0, ERRFLAG_ARGID2_OUT_BOUNDS )

  ! Get number of arguments
  ARGCOUNT = COMMAND_ARGUMENT_COUNT()
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).GT.ARGCOUNT, ERRFLAG_ARGID1_OUT_BOUNDS )

  ! Check if the character pointer is greater than 1
  IF ( COMMAND_ARGID(2) .GT. 1_JPIB_K) THEN

    COMMAND_ARGID(2) = COMMAND_ARGID(2) - 1_JPIB_K

  ELSEIF ( COMMAND_ARGID(2) .EQ. 1_JPIB_K .AND. COMMAND_ARGID(1) .GT. 1_JPIB_K ) THEN

    COMMAND_ARGID(1) = COMMAND_ARGID(1) - 1_JPIB_K

    ! Retrieve the current argument
    CALL GET_COMMAND_ARGUMENT(COMMAND_ARGID(1), ARGSTRING, LENGTH=ARGLEN, STATUS=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_COMMAND_ARG )
    PP_DEBUG_CRITICAL_COND_THROW( ARGLEN.GT.ARGSTRING_LEN, ERRFLAG_ARGUMENT_TOO_LONG )

    COMMAND_ARGID(2) = LEN_TRIM(ARGSTRING) + 1_JPIB_K

  ELSE

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_ARG_ID )

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

    CHARACTER(LEN=ARGSTRING_LEN) :: TMP1

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ARGID1_OUT_BOUNDS)
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument Id out of bounds' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_ARGID2_OUT_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument string idx out of bounds' )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument string idx: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_UNABLE_TO_READ_COMMAND_ARG)
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read command ine argument' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG,STAT=STAT)
      ENDIF
    CASE (ERRFLAG_ARGUMENT_TOO_LONG)
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command line argument too long (input truncated)' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Truncated: "'//TRIM(ADJUSTL(ARGSTRING))//'"' )
    CASE (ERRFLAG_INVALID_ARG_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid indices' )
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

END FUNCTION PUSH_CHAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_NEXT_ARG'
PP_THREAD_SAFE FUNCTION READ_NEXT_ARG( COMMAND_ARGID, MODIFIER_COUNT, NAME, &
&  MATCH, VALUES, EOC, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), DIMENSION(2),               INTENT(INOUT) :: COMMAND_ARGID
  INTEGER(KIND=JPIB_K),                             INTENT(OUT)   :: MODIFIER_COUNT
  CHARACTER(LEN=MAX_STRLEN),                        INTENT(OUT)   :: NAME
  LOGICAL,                                          INTENT(OUT)   :: MATCH
  CHARACTER(LEN=MAX_STRLEN), DIMENSION(:), POINTER, INTENT(OUT)   :: VALUES
  LOGICAL,                                          INTENT(OUT)   :: EOC
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ARGCOUNT

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID1_OUT_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID2_OUT_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_COMMAND_ARG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_FLAG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_VALUES=5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).LT.0, ERRFLAG_ARGID1_OUT_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(2).LT.0, ERRFLAG_ARGID2_OUT_BOUNDS )

  ! Get number of arguments
  ARGCOUNT = COMMAND_ARGUMENT_COUNT()
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).GT.ARGCOUNT, ERRFLAG_ARGID1_OUT_BOUNDS )

  ! Read the flag
  PP_TRYCALL(ERRFLAG_READ_FLAG) READ_FLAG( COMMAND_ARGID, MODIFIER_COUNT, NAME, EOC, HOOKS )
  ! If needed try to read the value
  IF ( .NOT. EOC ) THEN
    PP_TRYCALL(ERRFLAG_READ_VALUES) READ_VALUES( COMMAND_ARGID, MATCH, VALUES, EOC, HOOKS )
  ELSE
    VALUES => NULL()
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

    CHARACTER(LEN=ARGSTRING_LEN) :: TMP1
    INTEGER(KIND=JPIB_K) :: STAT

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ARGID1_OUT_BOUNDS)
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument Id out of bounds' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_ARGID2_OUT_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument string idx out of bounds' )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument string idx: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_READ_FLAG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read flag' )
    CASE (ERRFLAG_READ_VALUES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read value' )
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

END FUNCTION READ_NEXT_ARG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IS_VALID_FLAG'
PP_THREAD_SAFE FUNCTION IS_VALID_FLAG( C, IS_VALID, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=1), INTENT(IN)    :: C
  LOGICAL,          INTENT(OUT)   :: IS_VALID
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  ! Function Result
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

  ! Check if it is a valid flag
  IS_VALID = ((C .GE. '0' .AND. C .LE. '9') .OR. &
&             (C .GE. 'a' .AND. C .LE. 'z') .OR. &
&             (C .GE. 'A' .AND. C .LE. 'Z') .OR. &
&              C .EQ. '-')

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION IS_VALID_FLAG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IS_VALID_VALUE'
PP_THREAD_SAFE FUNCTION IS_VALID_VALUE( C, IS_VALID, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=1), INTENT(IN) :: C
  LOGICAL,          INTENT(OUT)   :: IS_VALID
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  ! Function Result
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

  ! Check if it is a valid values
  IS_VALID = ((C .GE. '0' .AND. C .LE. '9') .OR. &
&             (C .GE. 'a' .AND. C .LE. 'z') .OR. &
&             (C .GE. 'A' .AND. C .LE. 'Z') .OR. &
&              C .EQ. '-' .OR.  C .EQ. '+'  .OR. &
&              C .EQ. '.' .OR.  C .EQ. ':' )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION IS_VALID_VALUE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_MODIFIER'
PP_THREAD_SAFE FUNCTION READ_MODIFIER( COMMAND_ARGID, MODIFIER_SIZE, EOC, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), DIMENSION(2), INTENT(INOUT) :: COMMAND_ARGID
  INTEGER(KIND=JPIB_K),               INTENT(OUT)   :: MODIFIER_SIZE
  LOGICAL,                            INTENT(OUT)   :: EOC
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ARGCOUNT
  CHARACTER(LEN=1) :: CHAR_VALUE

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID1_OUT_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID2_OUT_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_POP_CHAR=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PUSH_CHAR=4_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).LT.0, ERRFLAG_ARGID1_OUT_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(2).LT.0, ERRFLAG_ARGID2_OUT_BOUNDS )

  ! Get number of arguments
  ARGCOUNT = COMMAND_ARGUMENT_COUNT()
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).GT.ARGCOUNT, ERRFLAG_ARGID1_OUT_BOUNDS )

  ! Initialization
  EOC = .FALSE.
  MODIFIER_SIZE = 0_JPIB_K

  ! Count the number of modifiers
  PP_TRYCALL(ERRFLAG_POP_CHAR) POP_CHAR( COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS )
  DO WHILE( CHAR_VALUE .EQ. '-' .AND. .NOT.EOC )
    MODIFIER_SIZE = MODIFIER_SIZE + 1_JPIB_K
    PP_TRYCALL(ERRFLAG_POP_CHAR) POP_CHAR(COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS )
  ENDDO
  PP_TRYCALL(ERRFLAG_PUSH_CHAR) PUSH_CHAR(COMMAND_ARGID, HOOKS  )

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

    CHARACTER(LEN=ARGSTRING_LEN) :: TMP1
    INTEGER(KINd=JPIB_K) :: STAT

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ARGID1_OUT_BOUNDS)
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument Id out of bounds' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_ARGID2_OUT_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument string idx out of bounds' )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument string idx: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_POP_CHAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to pop character' )
    CASE (ERRFLAG_PUSH_CHAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to push character' )
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

END FUNCTION READ_MODIFIER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SKIP_WHITE'
PP_THREAD_SAFE FUNCTION SKIP_WHITE( COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), DIMENSION(2), INTENT(INOUT) :: COMMAND_ARGID
  CHARACTER(LEN=1),                   INTENT(OUT)   :: CHAR_VALUE
  LOGICAL,                            INTENT(OUT)   :: EOC
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ARGCOUNT

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID1_OUT_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID2_OUT_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_POP_CHAR=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PUSH_CHAR=4_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).LT.0, ERRFLAG_ARGID1_OUT_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(2).LT.0, ERRFLAG_ARGID2_OUT_BOUNDS )

  ! Get number of arguments
  ARGCOUNT = COMMAND_ARGUMENT_COUNT()
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).GT.ARGCOUNT, ERRFLAG_ARGID1_OUT_BOUNDS )

  ! Skip white spaces
  EOC = .FALSE.
  PP_TRYCALL(ERRFLAG_POP_CHAR) POP_CHAR(COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS )
  DO WHILE( CHAR_VALUE .EQ. ' ' .AND. .NOT.EOC )
    PP_TRYCALL(ERRFLAG_POP_CHAR) POP_CHAR(COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS )
  ENDDO

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

    CHARACTER(LEN=ARGSTRING_LEN) :: TMP1
    INTEGER(KIND=JPIB_K) :: STAT

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ARGID1_OUT_BOUNDS)
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument Id out of bounds' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_ARGID2_OUT_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument string idx out of bounds' )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument string idx: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_POP_CHAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to pop character' )
    CASE (ERRFLAG_PUSH_CHAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to push character' )
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

END FUNCTION SKIP_WHITE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_FLAG'
PP_THREAD_SAFE FUNCTION READ_FLAG( COMMAND_ARGID, MODIFIER, NAME, EOC, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), DIMENSION(2), INTENT(INOUT) :: COMMAND_ARGID
  INTEGER(KIND=JPIB_K),               INTENT(OUT)   :: MODIFIER
  CHARACTER(LEN=MAX_STRLEN),          INTENT(OUT)   :: NAME
  LOGICAL,                            INTENT(OUT)   :: EOC
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: LOOP
  LOGICAL :: IS_VALID
  CHARACTER(LEN=1) :: CHAR_VALUE
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: ARGCOUNT

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID1_OUT_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID2_OUT_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SKIP_WHITE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PUSH_CHAR=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_MODIFIER=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_POP_CHAR=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_IS_VALID_FLAG=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODIFIER_WITHOUT_FLAG=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_MODIFIER=9_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).LT.0, ERRFLAG_ARGID1_OUT_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(2).LT.0, ERRFLAG_ARGID2_OUT_BOUNDS )

  ! Get number of arguments
  ARGCOUNT = COMMAND_ARGUMENT_COUNT()
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).GT.ARGCOUNT, ERRFLAG_ARGID1_OUT_BOUNDS )

  ! Local variables Initialization
  NAME = REPEAT( ' ', MAX_STRLEN )
  LOOP = .TRUE.
  EOC = .FALSE.

  ! Skip white characters before the modifier
  PP_TRYCALL(ERRFLAG_SKIP_WHITE) SKIP_WHITE( COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( CHAR_VALUE.NE.'-', ERRFLAG_WRONG_MODIFIER )

  ! If not end of command line then read the modifer
  IF ( .NOT.EOC ) THEN

    ! Read the modifier
    PP_TRYCALL(ERRFLAG_PUSH_CHAR) PUSH_CHAR(COMMAND_ARGID, HOOKS )
    PP_TRYCALL(ERRFLAG_READ_MODIFIER) READ_MODIFIER( COMMAND_ARGID, MODIFIER, EOC, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( EOC, ERRFLAG_MODIFIER_WITHOUT_FLAG )

    ! Read the flag
    I = 0_JPIB_K
    DO WHILE(LOOP)
      PP_TRYCALL(ERRFLAG_POP_CHAR) POP_CHAR(COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS )
      IF ( EOC ) THEN
        LOOP = .FALSE.
      ELSE
        PP_TRYCALL(ERRFLAG_CALL_IS_VALID_FLAG) IS_VALID_FLAG( CHAR_VALUE, IS_VALID, HOOKS )
        IF ( IS_VALID ) THEN
          I = I + 1_JPIB_K
          NAME(I:I) = CHAR_VALUE
        ELSEIF ( CHAR_VALUE .EQ. ' ' .OR. CHAR_VALUE .EQ. '=' ) THEN
          LOOP = .FALSE.
        ENDIF
      ENDIF
    ENDDO

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

    CHARACTER(LEN=ARGSTRING_LEN) :: TMP1
    INTEGER(KIND=JPIB_K) :: STAT

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ARGID1_OUT_BOUNDS)
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument Id out of bounds' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_ARGID2_OUT_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument string idx out of bounds' )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument string idx: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_SKIP_WHITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to skip whitespaces' )
    CASE (ERRFLAG_PUSH_CHAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to push char' )
    CASE (ERRFLAG_READ_MODIFIER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read modifier' )
    CASE (ERRFLAG_POP_CHAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to pop char' )
    CASE (ERRFLAG_CALL_IS_VALID_FLAG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check if a character is valid for a flag' )
    CASE (ERRFLAG_MODIFIER_WITHOUT_FLAG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error readng modifier' )
    CASE (ERRFLAG_WRONG_MODIFIER)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong modifier: "'//CHAR_VALUE//'"' )
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

END FUNCTION READ_FLAG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#DEFINE PP_PROCEDURE_TYPE 'FUNCTION'
#DEFINE PP_PROCEDURE_NAME 'READ_VALUES'
PP_THREAD_SAFE FUNCTION READ_VALUES( COMMAND_ARGID, MATCH, VALUES, EOC, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), DIMENSION(2),               INTENT(INOUT) :: COMMAND_ARGID
  LOGICAL,                                          INTENT(OUT)   :: MATCH
  CHARACTER(LEN=MAX_STRLEN), DIMENSION(:), POINTER, INTENT(OUT)   :: VALUES
  LOGICAL,                                          INTENT(OUT)   :: EOC
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: ARGCOUNT
  CHARACTER(LEN=1) :: CHAR_VALUE
  CHARACTER(LEN=1) :: SEP
  CHARACTER(LEN=MAX_STRLEN) :: VALUE
  TYPE(ARG_LIST_T) :: LIST
  TYPE(ARG_T), POINTER :: CURR
  TYPE(ARG_T), POINTER :: PREV
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID1_OUT_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID2_OUT_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SKIP_WHITE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_ARRAY_VALUE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_ARRAY_TERMINATOR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_SCALAR_VALUE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_SCALAR_TERMINATOR=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PUSH_CHAR=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXPECTED_VALUE=11_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).LT.0, ERRFLAG_ARGID1_OUT_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(2).LT.0, ERRFLAG_ARGID2_OUT_BOUNDS )

  ! Get number of arguments
  ARGCOUNT = COMMAND_ARGUMENT_COUNT()
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).GT.ARGCOUNT, ERRFLAG_ARGID1_OUT_BOUNDS )

  VALUES => NULL()
  MATCH = .TRUE.
  EOC = .FALSE.
  PP_TRYCALL(ERRFLAG_SKIP_WHITE) SKIP_WHITE( COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS )

  ! Check if argument is negated
  IF ( .NOT.EOC ) THEN

    ! Check if the argument is a negation
    IF ( CHAR_VALUE .EQ. '%' ) THEN
      MATCH = .FALSE.

      ! Skip white after negation
      PP_TRYCALL(ERRFLAG_SKIP_WHITE) SKIP_WHITE( COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS )

    ENDIF

  ENDIF

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( (.NOT.MATCH).AND.EOC, ERRFLAG_EXPECTED_VALUE )

  ! Read the values (array or scalar)
  IF ( .NOT.EOC ) THEN

    IF ( CHAR_VALUE .EQ. '[' ) THEN
      ! Case of array value


      ! Read an array of comma separated tokens and push to the end of a linked list
      PP_TRYCALL(ERRFLAG_READ_ARRAY_VALUE) READ_NEXT_ARRAY_VALUE( COMMAND_ARGID, LIST, SEP, EOC, HOOKS )
      DO WHILE ( SEP .EQ. ',' )
        PP_TRYCALL(ERRFLAG_READ_ARRAY_VALUE) READ_NEXT_ARRAY_VALUE( COMMAND_ARGID, LIST, SEP, EOC, HOOKS )
      ENDDO

      ! If end of arry is found then convert the list into an array and deallocate the list
      IF ( SEP .EQ. ']' ) THEN

        ! Allocate array
        ALLOCATE(VALUES(LIST%SIZE), STAT=STAT, ERRMSG=ERRMSG)
        PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE )

        ! Visiting all elements in the list
        I = 0_JPIB_K
        CURR => LIST%HEAD
        DO WHILE( ASSOCIATED(CURR))

          ! Copy data from list node to the array
          I = I + 1_JPIB_K
          VALUES(I) = REPEAT( ' ', MAX_STRLEN )
          VALUES(I) = TRIM(ADJUSTL(CURR%VAL))

          ! Update list pointers
          PREV => CURR
          CURR => CURR%NEXT

          ! Deallocate list node
          DEALLOCATE(PREV, STAT=STAT, ERRMSG=ERRMSG)
          PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE )

          ! Update the list for consistency
          LIST%HEAD => CURR
          LIST%SIZE = LIST%SIZE - 1
        ENDDO

        ! Skip after last flag
        IF ( .NOT.EOC ) THEN
          PP_TRYCALL(ERRFLAG_SKIP_WHITE) SKIP_WHITE( COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS )
          PP_TRYCALL(ERRFLAG_PUSH_CHAR) PUSH_CHAR(COMMAND_ARGID, HOOKS )
        ENDIF

      ELSE

        ! Error in case of a wrong separator
        PP_DEBUG_CRITICAL_THROW( ERRFLAG_WRONG_ARRAY_TERMINATOR )

      ENDIF

    ELSE
      ! Case of a scalar value

      ! Read the value
      PP_TRYCALL(ERRFLAG_PUSH_CHAR) PUSH_CHAR(COMMAND_ARGID, HOOKS )
      PP_TRYCALL(ERRFLAG_READ_SCALAR_VALUE) READ_SCALAR_VALUE( COMMAND_ARGID, VALUE, SEP, EOC, HOOKS )

      ! Copy the value in the output array
      IF ( SEP .EQ. ' ' ) THEN

        ! Allocate array
        ALLOCATE( VALUES(1), STAT=STAT, ERRMSG=ERRMSG)
        PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE )

        ! Copy value
        VALUES(1) = REPEAT( ' ', MAX_STRLEN )
        VALUES(1) = TRIM(ADJUSTL(VALUE))

        ! Skip after last flag
        IF ( .NOT.EOC ) THEN
          PP_TRYCALL(ERRFLAG_SKIP_WHITE) SKIP_WHITE( COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS )
          PP_TRYCALL(ERRFLAG_PUSH_CHAR) PUSH_CHAR(COMMAND_ARGID, HOOKS )
        ENDIF
      ELSE

        ! Error in case of a wrong separator
        PP_DEBUG_CRITICAL_THROW( ERRFLAG_WRONG_SCALAR_TERMINATOR )

      ENDIF

    ENDIF

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

    CHARACTER(LEN=ARGSTRING_LEN) :: TMP1

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ARGID1_OUT_BOUNDS)
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument Id out of bounds' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_ARGID2_OUT_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument string idx out of bounds' )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument string idx: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_SKIP_WHITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error skip white-spaces' )
    CASE (ERRFLAG_READ_ARRAY_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error unable to read array value' )
    CASE (ERRFLAG_WRONG_ARRAY_TERMINATOR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error unable to read array terminator' )
    CASE (ERRFLAG_READ_SCALAR_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error unable to read saclar value' )
    CASE (ERRFLAG_WRONG_SCALAR_TERMINATOR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error unable to read scalar terminator' )
    CASE (ERRFLAG_PUSH_CHAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to push char' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate array' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate node' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_EXPECTED_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Expected value got end of command line arguments' )
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

END FUNCTION READ_VALUES
#UNDEF PP_PROCEDURE_NAME
#UNDEF PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_NEXT_ARRAY_VALUE'
PP_THREAD_SAFE FUNCTION READ_NEXT_ARRAY_VALUE( COMMAND_ARGID, LIST, SEP, EOC, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), DIMENSION(2), INTENT(INOUT) :: COMMAND_ARGID
  TYPE(ARG_LIST_T),                   INTENT(INOUT) :: LIST
  CHARACTER(LEN=1),                   INTENT(OUT)   :: SEP
  LOGICAL,                            INTENT(OUT)   :: EOC
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: LOOP
  CHARACTER(LEN=MAX_STRLEN) :: VALUE
  CHARACTER(LEN=1) :: CHAR_VALUE
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: ARGCOUNT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  LOGICAL :: IS_VALID

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID1_OUT_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID2_OUT_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SKIP_WHITE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PUSH_CHAR=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_NEXT_CHARACTER=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_END_OF_COMMAND_LINE_FOUND=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IS_VALID_VALUE=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CHARACTERS=10_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).LT.0, ERRFLAG_ARGID1_OUT_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(2).LT.0, ERRFLAG_ARGID2_OUT_BOUNDS )

  ! Get number of arguments
  ARGCOUNT = COMMAND_ARGUMENT_COUNT()
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).GT.ARGCOUNT, ERRFLAG_ARGID1_OUT_BOUNDS )

  ! Initialization
  VALUE = REPEAT( ' ', MAX_STRLEN )
  LOOP = .TRUE.
  EOC = .FALSE.

  ! Skip whitespaces
  PP_TRYCALL(ERRFLAG_SKIP_WHITE) SKIP_WHITE( COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS )

  ! If there is something to read
  IF ( .NOT.EOC ) THEN

    ! Push back the character before the loop
    PP_TRYCALL(ERRFLAG_PUSH_CHAR) PUSH_CHAR(COMMAND_ARGID, HOOKS )

    ! Loop over the characters
    I = 0_JPIB_K
    DO WHILE(LOOP)

      ! Read the next character
      PP_TRYCALL(ERRFLAG_READ_NEXT_CHARACTER) POP_CHAR(COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS )

      ! If end of command line reached exit the read loop
      PP_DEBUG_CRITICAL_COND_THROW( EOC, ERRFLAG_END_OF_COMMAND_LINE_FOUND )

      ! White space should be skipped
      IF ( CHAR_VALUE .EQ. ' ' ) THEN
        CYCLE
      ENDIF

      ! Check if the character is valid for a value
      PP_TRYCALL(ERRFLAG_IS_VALID_VALUE) IS_VALID_VALUE( CHAR_VALUE, IS_VALID, HOOKS )

      ! If is valid
      IF ( IS_VALID ) THEN

        ! If  is valid then add the character to the value
        I = I + 1_JPIB_K
        PP_DEBUG_CRITICAL_COND_THROW( I.GT.MAX_STRLEN, ERRFLAG_OUT_OF_BOUNDS )
        VALUE(I:I) = CHAR_VALUE

      ELSEIF ( CHAR_VALUE .EQ. ',' .OR. CHAR_VALUE .EQ. ']' ) THEN

        ! If a separator if found, then need to push the value in a list
        LOOP = .FALSE.
        SEP = CHAR_VALUE
        IF ( LIST%SIZE .EQ. 0 ) THEN
          ALLOCATE( LIST%HEAD, STAT=STAT, ERRMSG=ERRMSG )
          PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE )
          LIST%HEAD%VAL = REPEAT( ' ', MAX_STRLEN )
          LIST%HEAD%VAL = TRIM(ADJUSTL(VALUE))
          LIST%TAIL => LIST%HEAD
          LIST%SIZE = 1_JPIB_K
        ELSE
          ALLOCATE( LIST%TAIL%NEXT, STAT=STAT, ERRMSG=ERRMSG )
          PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE )
          LIST%TAIL%NEXT%VAL = REPEAT( ' ', MAX_STRLEN )
          LIST%TAIL%NEXT%VAL = TRIM(ADJUSTL(VALUE))
          LIST%TAIL%NEXT%PREV => LIST%TAIL
          LIST%TAIL => LIST%TAIL%NEXT
          LIST%SIZE = LIST%SIZE + 1_JPIB_K
        ENDIF

      ELSE

        ! If it is not a valid value, not a separator and not a white-space, then it is an error
        PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_CHARACTERS )

      ENDIF

    ENDDO

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

    CHARACTER(LEN=ARGSTRING_LEN) :: TMP1

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ARGID1_OUT_BOUNDS)
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument Id out of bounds' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_ARGID2_OUT_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument string idx out of bounds' )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument string idx: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_SKIP_WHITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to skip whitespaces' )
    CASE (ERRFLAG_PUSH_CHAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to push character' )
    CASE (ERRFLAG_READ_NEXT_CHARACTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read next character' )
    CASE (ERRFLAG_END_OF_COMMAND_LINE_FOUND)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unexpected end of command line' )
    CASE (ERRFLAG_IS_VALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check if the character is valid' )
    CASE (ERRFLAG_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command line token too long' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_INVALID_CHARACTERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid character found in command line: "'//CHAR_VALUE//'"' )
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

END FUNCTION READ_NEXT_ARRAY_VALUE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#DEFINE PP_PROCEDURE_TYPE 'FUNCTION'
#DEFINE PP_PROCEDURE_NAME 'READ_SCALAR_VALUE'
PP_THREAD_SAFE FUNCTION READ_SCALAR_VALUE( COMMAND_ARGID, VALUE, SEP, EOC, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), DIMENSION(2), INTENT(INOUT) :: COMMAND_ARGID
  CHARACTER(LEN=MAX_STRLEN),          INTENT(OUT)   :: VALUE
  CHARACTER(LEN=1),                   INTENT(OUT)   :: SEP
  LOGICAL,                            INTENT(OUT)   :: EOC
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  ! Function Result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: LOOP
  CHARACTER(LEN=1) :: CHAR_VALUE
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: ARGCOUNT
  LOGICAL :: IS_VALID

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID1_OUT_BOUNDS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARGID2_OUT_BOUNDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SKIP_WHITE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PUSH_CHAR=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_NEXT_CHARACTER=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_END_OF_COMMAND_LINE_FOUND=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IS_VALID_VALUE=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CHARACTERS=8_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).LT.0, ERRFLAG_ARGID1_OUT_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(2).LT.0, ERRFLAG_ARGID2_OUT_BOUNDS )

  ! Get number of arguments
  ARGCOUNT = COMMAND_ARGUMENT_COUNT()
  PP_DEBUG_CRITICAL_COND_THROW( COMMAND_ARGID(1).GT.ARGCOUNT, ERRFLAG_ARGID1_OUT_BOUNDS )


  ! Initialization
  VALUE = REPEAT( ' ', MAX_STRLEN )
  LOOP = .TRUE.
  EOC = .FALSE.

  ! Skip whitespaces
  PP_TRYCALL(ERRFLAG_SKIP_WHITE) SKIP_WHITE( COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS )

  ! If there is something to read
  IF ( .NOT.EOC ) THEN

    ! Push back the character before the loop
    PP_TRYCALL(ERRFLAG_PUSH_CHAR) PUSH_CHAR(COMMAND_ARGID, HOOKS )

    ! Loop over the characters
    I = 0_JPIB_K
    DO WHILE(LOOP)

      ! Read the next character
      PP_TRYCALL(ERRFLAG_READ_NEXT_CHARACTER) POP_CHAR(COMMAND_ARGID, CHAR_VALUE, EOC, HOOKS )

      ! If end of command line reached exit the read loop
      PP_DEBUG_CRITICAL_COND_THROW( EOC .AND. (I.EQ.0), ERRFLAG_END_OF_COMMAND_LINE_FOUND )

      ! If end of command line reached exit the read loop
      IF ( EOC ) THEN

        LOOP = .FALSE.
        SEP=' '

      ELSE

        ! Check if the character is valid for a value
        PP_TRYCALL(ERRFLAG_IS_VALID_VALUE) IS_VALID_VALUE( CHAR_VALUE, IS_VALID, HOOKS )

        ! If is valid
        IF ( IS_VALID ) THEN
          I = I + 1
          VALUE(I:I) = CHAR_VALUE
        ELSEIF ( CHAR_VALUE .EQ. ' ' ) THEN
          LOOP = .FALSE.
          SEP = CHAR_VALUE
        ELSE

          ! If it is not a valid value, not a separator and not a white-space, then it is an error
          PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_CHARACTERS )

        ENDIF

      ENDIF

    ENDDO

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

    CHARACTER(LEN=ARGSTRING_LEN) :: TMP1
    INTEGER(KIND=JPIB_K) :: STAT

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ARGID1_OUT_BOUNDS)
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument Id out of bounds' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_ARGID2_OUT_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Argument string idx out of bounds' )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument Index: '//TRIM(ADJUSTL(TMP1)) )
      TMP1 = REPEAT( ' ', ARGSTRING_LEN )
      WRITE(TMP1,'(I32)',IOSTAT=STAT) COMMAND_ARGID(2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Command argument string idx: '//TRIM(ADJUSTL(TMP1)) )
    CASE (ERRFLAG_SKIP_WHITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to skip whitespaces' )
    CASE (ERRFLAG_PUSH_CHAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to push character' )
    CASE (ERRFLAG_READ_NEXT_CHARACTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read next character' )
    CASE (ERRFLAG_END_OF_COMMAND_LINE_FOUND)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unexpected end of command line' )
    CASE (ERRFLAG_IS_VALID_VALUE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check if the character is valid' )
    CASE (ERRFLAG_INVALID_CHARACTERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid character found in command line: "'//CHAR_VALUE//'"' )
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


END FUNCTION READ_SCALAR_VALUE
#UNDEF PP_PROCEDURE_NAME
#UNDEF PP_PROCEDURE_TYPE

END MODULE COMMAND_ARGUMENTS_TOKENIZER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME