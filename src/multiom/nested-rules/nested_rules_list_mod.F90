! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'nested_rules_list_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'NESTED_RULES_LIST_MOD'
MODULE NESTED_RULES_LIST_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: FILTER_BASE_MOD,       ONLY: FILTER_BASE_A

IMPLICIT NONE

! Default visibility
PRIVATE

! Node for a rule in the list
TYPE :: RAW_RULE_NODE_T
  INTEGER(KIND=JPIB_K) :: ID_ = 0_JPIB_K
  CHARACTER(LEN=:), ALLOCATABLE :: RULE_FNAME_
  TYPE(RAW_RULE_NODE_T), POINTER :: NEXT_ => NULL()
  TYPE(RAW_RULE_NODE_T), POINTER :: PREV_ => NULL()
END TYPE

! List of matching rules
TYPE :: RAW_RULES_LIST_T

  ! Default visibility
  PRIVATE

  ! Head of the list
  TYPE(RAW_RULE_NODE_T), POINTER :: HEAD_ => NULL()

  ! Tail of the list
  TYPE(RAW_RULE_NODE_T), POINTER :: TAIL_ => NULL()

  ! Size of the list
  INTEGER(KIND=JPIB_K) :: SIZE_ = 0_JPIB_K

CONTAINS

  !> Add a new rule to the list
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: INTI => RAW_RULES_LIST_INIT

  !> Print the list of rules
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: SIZE => RAW_RULES_LIST_SIZE

  !> Add a new rule to the list
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: ADD_RULE => RAW_RULES_LIST_ADD_RULE

  !> Get the rule name by ID
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: GET_RULE_BY_ID => RAW_RULES_LIST_GET_RULE_BY_ID

  !> Free all the memory allocated by the list
  PROCEDURE, PASS, PUBLIC, NON_OVERRIDABLE :: FREE => RAW_RULES_LIST_FREE
END TYPE

! Whitelist of public symbols
PUBLIC :: RAW_RULES_LIST_T

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RAW_RULES_LIST_INIT'
PP_THREAD_SAFE FUNCTION RAW_RULES_LIST_INIT( THIS, HOOKS ) RESULT(RET)

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
  CLASS(RAW_RULES_LIST_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HEAD_ALREADY_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TAIL_ALREADY_ASSOCIATED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SIZE_NOT_ZERO = 3_JPIB_K

  !> Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  !> Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  !> Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  !> Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  !> Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%HEAD_), ERRFLAG_HEAD_ALREADY_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%TAIL_), ERRFLAG_TAIL_ALREADY_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_.NE.0_JPIB_K, ERRFLAG_SIZE_NOT_ZERO )

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
    CASE(ERRFLAG_HEAD_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'head already associated' )
    CASE(ERRFLAG_TAIL_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'tail already associated' )
    CASE(ERRFLAG_SIZE_NOT_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'size not zero' )
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

END FUNCTION RAW_RULES_LIST_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RAW_RULES_LIST_SIZE'
PP_THREAD_SAFE FUNCTION RAW_RULES_LIST_SIZE( THIS, SIZE, HOOKS ) RESULT(RET)

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
  CLASS(RAW_RULES_LIST_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),    INTENT(OUT)   :: SIZE
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  !> Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  !> Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  !> Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  !> Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

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

END FUNCTION RAW_RULES_LIST_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RAW_RULES_LIST_ADD_RULE'
PP_THREAD_SAFE FUNCTION RAW_RULES_LIST_ADD_RULE( THIS, FNAME, HOOKS ) RESULT(RET)

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
  CLASS(RAW_RULES_LIST_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),        INTENT(IN)    :: FNAME
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! > Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_FAILED = 1_JPIB_K

  !> Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  !> Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  !> Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  !> Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  !> Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  IF ( .NOT.ASSOCIATED(THIS%HEAD_) ) THEN
    ! Create a new node for the rule
    ALLOCATE(THIS%HEAD_, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT.NE.0_JPIB_K, ERRFLAG_ALLOCATION_FAILED )

    ! Initialize the head node
    THIS%HEAD_%ID_ = 1_JPIB_K
    ALLOCATE(CHARACTER(LEN=LEN_TRIM(FNAME))::THIS%HEAD_%RULE_FNAME_, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( RET.NE.0_JPIB_K, ERRFLAG_ALLOCATION_FAILED )
    THIS%HEAD_%RULE_FNAME_ = TRIM(ADJUSTL(FNAME))
    THIS%HEAD_%NEXT_ => NULL()
    THIS%HEAD_%PREV_ => NULL()
    THIS%SIZE_ = 1_JPIB_K

    ! Set the tail to the head
    THIS%TAIL_ => THIS%HEAD_

  ELSE

    ! Create a new node for the rule
    ALLOCATE(THIS%TAIL_%NEXT_, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( RET.NE.0_JPIB_K, ERRFLAG_ALLOCATION_FAILED )

    ! Initialize the new node
    THIS%TAIL_%NEXT_%ID_ = THIS%TAIL_%ID_ + 1_JPIB_K
    ALLOCATE(CHARACTER(LEN=LEN_TRIM(FNAME))::THIS%TAIL_%NEXT_%RULE_FNAME_, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( RET.NE.0_JPIB_K, ERRFLAG_ALLOCATION_FAILED )
    THIS%TAIL_%NEXT_%RULE_FNAME_ = TRIM(ADJUSTL(FNAME))
    THIS%TAIL_%NEXT_%NEXT_ => NULL()
    THIS%TAIL_%NEXT_%PREV_ => THIS%TAIL_
    THIS%SIZE_ = THIS%SIZE_ + 1_JPIB_K

    ! Update the tail pointer
    THIS%TAIL_ => THIS%TAIL_%NEXT_

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
    CASE(ERRFLAG_ALLOCATION_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate memory for the rule node' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'allocation error message: "' // TRIM(ADJUSTL(ERRMSG)) // '"' )
        DEALLOCATE( ERRMSG, STAT=ALLOC_STAT )
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

END FUNCTION RAW_RULES_LIST_ADD_RULE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RAW_RULES_LIST_GET_RULE_BY_ID'
PP_THREAD_SAFE FUNCTION RAW_RULES_LIST_GET_RULE_BY_ID( THIS, ID, FNAME, HOOKS ) RESULT(RET)

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
  CLASS(RAW_RULES_LIST_T), INTENT(IN)    :: THIS
  INTEGER(KIND=JPIB_K),    INTENT(IN)    :: ID
  CHARACTER(LEN=*),        INTENT(OUT)   :: FNAME
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(RAW_RULE_NODE_T), POINTER :: CURRENT => NULL()

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ID_OUT_OF_BOUNDS = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULE_FNAME_NOT_ALLOCATED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULE_FNAME_EMPTY = 3_JPIB_K

  !> Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  !> Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  !> Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  !> Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  !> Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ID.LT.1_JPIB_K,   ERRFLAG_ID_OUT_OF_BOUNDS )
  PP_DEBUG_CRITICAL_COND_THROW( ID.GT.THIS%SIZE_, ERRFLAG_ID_OUT_OF_BOUNDS )

  CURRENT => THIS%HEAD_
  DO WHILE(ASSOCIATED(CURRENT))

    ! Check if the current node has the requested ID
    IF (CURRENT%ID_ == ID) THEN
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CURRENT%RULE_FNAME_), ERRFLAG_RULE_FNAME_NOT_ALLOCATED )
      PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(CURRENT%RULE_FNAME_).EQ.0, ERRFLAG_RULE_FNAME_EMPTY )
      ! Copy the rule filename to the output variable
      FNAME = REPEAT(' ', LEN(FNAME))
      FNAME = TRIM(ADJUSTL(CURRENT%RULE_FNAME_))
      EXIT
    ENDIF

    ! Move to the next node
    CURRENT => CURRENT%NEXT_

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

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_ID_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'ID out of bounds' )
    CASE(ERRFLAG_RULE_FNAME_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'rule filename not allocated' )
    CASE(ERRFLAG_RULE_FNAME_EMPTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'rule filename is empty' )
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

END FUNCTION RAW_RULES_LIST_GET_RULE_BY_ID
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RAW_RULES_LIST_TO_JSON'
PP_THREAD_SAFE FUNCTION RAW_RULES_LIST_TO_JSON( THIS, JSON, HOOKS ) RESULT(RET)

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
  CLASS(RAW_RULES_LIST_T),       INTENT(IN)    :: THIS
  CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: JSON
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(RAW_RULE_NODE_T), POINTER :: CURRENT => NULL()
  INTEGER(KIND=JPIB_K) :: STR_LEN
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  LOGICAL :: FIRST
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_FAILED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULE_FNAME_NOT_ALLOCATED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_JSON_ALREADY_ALLOCATED = 3_JPIB_K

  !> Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  !> Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  !> Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  !> Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  !> Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(JSON), ERRFLAG_JSON_ALREADY_ALLOCATED )

  STR_LEN = 20_JPIB_K
  CURRENT => THIS%HEAD_
  DO WHILE(ASSOCIATED(CURRENT))

    STR_LEN = STR_LEN + LEN_TRIM(CURRENT%RULE_FNAME_) + 5_JPIB_K

  ENDDO

  ! Allocate the JSON string
  ALLOCATE( CHARACTER(LEN=STR_LEN)::JSON, STAT=ALLOC_STAT, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT.NE.0_JPIB_K, ERRFLAG_ALLOCATION_FAILED )

  ! Populate the JSON string
  LO=1_JPIB_K
  HI=13_JPIB_K
  FIRST = .TRUE.
  JSON(LO:HI) = '{ "rules": [ '
  CURRENT => THIS%HEAD_
  DO WHILE(ASSOCIATED(CURRENT))

    ! Error handling
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CURRENT%RULE_FNAME_), ERRFLAG_RULE_FNAME_NOT_ALLOCATED )

    ! Handling separators
    IF (.NOT.FIRST) THEN
      LO = HI + 1_JPIB_K
      HI = LO + 1_JPIB_K
      JSON(LO:HI) = ', '
    ELSE
      FIRST = .FALSE.
    ENDIF

    ! Add the rule filename to the JSON string
    LO = HI + 1_JPIB_K
    HI = LO + LEN_TRIM(CURRENT%RULE_FNAME_) + 1_JPIB_K

    JSON(LO:HI) = '"' // TRIM(ADJUSTL(CURRENT%RULE_FNAME_)) // '"'

  ENDDO

  ! Close the JSON string
  LO = HI + 1_JPIB_K
  HI = LO + 3_JPIB_K
  JSON(LO:HI) = ' ] }'

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
    CASE(ERRFLAG_ALLOCATION_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate memory for the JSON string' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'allocation error message: "' // TRIM(ADJUSTL(ERRMSG)) // '"' )
        DEALLOCATE( ERRMSG, STAT=ALLOC_STAT )
      ENDIF
    CASE(ERRFLAG_RULE_FNAME_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'rule filename not allocated' )
    CASE(ERRFLAG_JSON_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'JSON string already allocated' )
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

END FUNCTION RAW_RULES_LIST_TO_JSON
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'RAW_RULES_LIST_FREE'
PP_THREAD_SAFE FUNCTION RAW_RULES_LIST_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(RAW_RULES_LIST_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(RAW_RULE_NODE_T), POINTER :: CURRENT => NULL()
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_RULE_FNAME = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_RULE_NODE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SIZE_NOT_ZERO = 3_JPIB_K

  !> Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  !> Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  !> Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  !> Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  !> Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  CURRENT => THIS%HEAD_
  DO WHILE(ASSOCIATED(CURRENT))

    ! Just a paranoid move
    THIS%HEAD_%PREV_ => NULL()

    ! Deallocate the filename of the rule
    IF (ALLOCATED(CURRENT%RULE_FNAME_)) THEN
      ! Deallocate the rule name
      DEALLOCATE(CURRENT%RULE_FNAME_, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_DEALLOCATE_RULE_FNAME )
    ENDIF

    ! Move to the next node
    THIS%HEAD_ => THIS%HEAD_%NEXT_
    THIS%SIZE_ = THIS%SIZE_ - 1_JPIB_K

    ! Deallocate the current node
    DEALLOCATE(CURRENT, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_DEALLOCATE_RULE_NODE )

    ! Move to the next node
    CURRENT => THIS%HEAD_

  ENDDO

  ! Paranoid check
  PP_DEBUG_CRITICAL_COND_THROW( THIS%SIZE_.NE.0_JPIB_K, ERRFLAG_SIZE_NOT_ZERO )

  ! Reset the object
  THIS%HEAD_ => NULL()
  THIS%TAIL_ => NULL()
  THIS%SIZE_ = 0_JPIB_K

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
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE_RULE_FNAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate rule filename' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'deallocation error message: "' // TRIM(ADJUSTL(ERRMSG)) // '"' )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      ENDIF
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE_RULE_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate rule node' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'deallocation error message: "' // TRIM(ADJUSTL(ERRMSG)) // '"' )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      ENDIF
    CASE(ERRFLAG_SIZE_NOT_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'size not zero after deallocation' )
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

END FUNCTION RAW_RULES_LIST_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE NESTED_RULES_LIST_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME