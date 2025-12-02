! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib_encoder_register_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_ENCODER_REGISTER_MOD'
MODULE GRIB_ENCODER_REGISTER_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A
  USE :: GRIB_SECTION_BASE_MOD, ONLY: SECTION_ALLOCATOR_IF
  USE :: GRIB_SECTION_BASE_MOD, ONLY: SECTION_DEALLOCATOR_IF

IMPLICIT NONE

!> @brief Default visibility of the module
PRIVATE

!> @brief Flag used to enable the tree balancing
LOGICAL, PARAMETER :: RED_BLACK_BALANCING=.TRUE.
INTEGER(KIND=JPIB_K), PARAMETER :: KEY_LEN = 64_JPIB_K


!>
!>
!> @brief Datatype used to to store a node of the tree
TYPE :: GRIB_SECTION_REGISTER_NODE_T

  !> Key
  CHARACTER(LEN=KEY_LEN) :: KEY_ = REPEAT(' ',64)

  !> Allocator for grib section
  PROCEDURE(SECTION_ALLOCATOR_IF),   POINTER, NOPASS :: MAKE_FCN_ => NULL()
  PROCEDURE(SECTION_DEALLOCATOR_IF), POINTER, NOPASS :: DESTROY_FCN_ => NULL()
  INTEGER(KIND=JPIB_K) :: NALLOC_ = 0_JPIB_K

  !> Color
  LOGICAL :: RED = .FALSE.

  !> Index used to dump the graph
  INTEGER(KIND=JPIB_K) :: IDX

  !> Pointer to the parent node
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: PARENT => NULL()

  !> Pointer to the lef subtree
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: LEFT   => NULL()

  !> Pointer to the right subtree
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: RIGHT  => NULL()

END TYPE


!>
!>
!> @brief Datatype used to to store the entire map
TYPE :: GRIB_SECTION_REGISTER_T

  !> Pointer to the class
  PRIVATE

  !> Pointer to the root node
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: ROOT => NULL()

  !> Size of the map
  INTEGER(KIND=JPIB_K) :: SIZE = -1_JPIB_K

CONTAINS

  !> Public methods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT     => GRIB_SECTION_REGISTER_INIT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: MATCH    => GRIB_SECTION_REGISTER_MATCH
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: REGISTER => GRIB_SECTION_REGISTER_REGISTER
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: MAKE     => GRIB_SECTION_REGISTER_MAKE
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: DESTROY  => GRIB_SECTION_REGISTER_DESTROY
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: REMOVE   => GRIB_SECTION_REGISTER_REMOVE
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT    => GRIB_SECTION_REGISTER_PRINT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: LIST     => GRIB_SECTION_REGISTER_LIST
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE     => GRIB_SECTION_REGISTER_FREE

END TYPE

!> @brief Node used as terminal symbol
TYPE(GRIB_SECTION_REGISTER_NODE_T), TARGET :: NIL

!> Whitelist of public symbols
PUBLIC :: GRIB_SECTION_REGISTER_T

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'KEY_EQUAL_TO'
PP_THREAD_SAFE FUNCTION KEY_EQUAL_TO( KEY1, KEY2, IS_EQUAL, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=KEY_LEN), INTENT(IN)    :: KEY1
  CHARACTER(LEN=KEY_LEN), INTENT(IN)    :: KEY2
  LOGICAL,                INTENT(OUT)   :: IS_EQUAL
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

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

  ! Check if the keys are equal
  IS_EQUAL = KEY1 .EQ. KEY2

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

END FUNCTION KEY_EQUAL_TO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'KEY_LOWER_THAN'
PP_THREAD_SAFE FUNCTION KEY_LOWER_THAN( KEY1, KEY2, IS_LOWER_THAN, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=KEY_LEN), INTENT(IN)    :: KEY1
  CHARACTER(LEN=KEY_LEN), INTENT(IN)    :: KEY2
  LOGICAL,                INTENT(OUT)   :: IS_LOWER_THAN
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

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

  ! Check if the keys are equal
  IS_LOWER_THAN = KEY1 .LT. KEY2

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

END FUNCTION KEY_LOWER_THAN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_INSERT_NODE'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_REGISTER_INSERT_NODE( ROOT, KEY, MAKE_FCN, DESTROY_FCN, INSERTED, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: GRIB_SECTION_BASE_MOD, ONLY: SECTION_ALLOCATOR_IF
  USE :: GRIB_SECTION_BASE_MOD, ONLY: SECTION_DEALLOCATOR_IF

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  CHARACTER(LEN=KEY_LEN),                      INTENT(IN)    :: KEY
  PROCEDURE(SECTION_ALLOCATOR_IF), POINTER,    INTENT(IN)    :: MAKE_FCN
  PROCEDURE(SECTION_DEALLOCATOR_IF), POINTER,  INTENT(IN)    :: DESTROY_FCN
  LOGICAL,                                     INTENT(OUT)   :: INSERTED
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: INSERTION_POINT
  LOGICAL :: FOUND
  LOGICAL :: KEY_LT

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_NODE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INITIALIZE_NODE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FIXUP_INSERT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SEARCH_NODE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_LT_FAILURE=5_JPIB_K

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

  ! Initialization of the insertion flag
  INSERTED =.TRUE.

  ! Map is empty
  IF ( ASSOCIATED( ROOT, NIL  ) ) THEN

    INSERTED =.TRUE.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE_NODE) ALLOCATE_NODE( ROOT, HOOKS )
    INSERTION_POINT => NIL
    PP_TRYCALL(ERRFLAG_UNABLE_TO_INITIALIZE_NODE) GRIB_SECTION_REGISTER_NODE_INIT( ROOT, &
&                                   INSERTION_POINT, KEY,  MAKE_FCN, DESTROY_FCN, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FIXUP_INSERT) INSERT_FIXUP( ROOT, ROOT, HOOKS )
    INSERTION_POINT => ROOT

  ! Map not empty
  ELSE

    INSERTION_POINT => NIL
    PP_TRYCALL(ERRFLAG_UNABLE_TO_SEARCH_NODE) SEARCH( ROOT, INSERTION_POINT, KEY, FOUND, HOOKS )
    IF ( .NOT.FOUND ) THEN
      INSERTED =.TRUE.
      PP_TRYCALL(ERRFLAG_KEY_LT_FAILURE) KEY_LOWER_THAN( KEY, INSERTION_POINT%KEY_, KEY_LT, HOOKS )
      IF ( KEY_LT ) THEN
        PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE_NODE) ALLOCATE_NODE( INSERTION_POINT%LEFT, HOOKS )
        PP_TRYCALL(ERRFLAG_UNABLE_TO_INITIALIZE_NODE) GRIB_SECTION_REGISTER_NODE_INIT( &
&             INSERTION_POINT%LEFT, INSERTION_POINT, KEY, MAKE_FCN, DESTROY_FCN, HOOKS )
        PP_TRYCALL(ERRFLAG_UNABLE_TO_FIXUP_INSERT) INSERT_FIXUP( ROOT, INSERTION_POINT%LEFT, HOOKS )
      ELSE
        PP_TRYCALL(ERRFLAG_UNABLE_TO_INITIALIZE_NODE) ALLOCATE_NODE( INSERTION_POINT%RIGHT, HOOKS )
        PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE_NODE) GRIB_SECTION_REGISTER_NODE_INIT( &
&             INSERTION_POINT%RIGHT, INSERTION_POINT, KEY, MAKE_FCN, DESTROY_FCN, HOOKS )
        PP_TRYCALL(ERRFLAG_UNABLE_TO_FIXUP_INSERT) INSERT_FIXUP( ROOT, INSERTION_POINT%RIGHT, HOOKS )
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

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate node' )
    CASE (ERRFLAG_UNABLE_TO_INITIALIZE_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to initialize node' )
    CASE (ERRFLAG_UNABLE_TO_FIXUP_INSERT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to fixup insert' )
    CASE (ERRFLAG_UNABLE_TO_SEARCH_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to search node' )
    CASE (ERRFLAG_KEY_LT_FAILURE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'key lower than failure' )
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

END FUNCTION GRIB_SECTION_REGISTER_INSERT_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_REMOVE_NODE'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_REGISTER_REMOVE_NODE( ROOT, KEY, FOUND, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  CHARACTER(LEN=KEY_LEN),                      INTENT(IN)    :: KEY
  LOGICAL,                                     INTENT(OUT)   :: FOUND
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: INSERTION_POINT
  LOGICAL :: IS_LEAF

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_REMOVE_NODE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERRFLAG_UNABLE_TO_SEARCH_NODE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=3_JPIB_K

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

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT, IS_LEAF, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN
    PP_TRYCALL(ERRFLAG_ERRFLAG_UNABLE_TO_SEARCH_NODE) SEARCH( ROOT, INSERTION_POINT, KEY, FOUND, HOOKS )
    IF ( FOUND ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_REMOVE_NODE) REMOVE_NODE( ROOT, INSERTION_POINT, HOOKS )
    ENDIF
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
    CASE (ERRFLAG_UNABLE_TO_REMOVE_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to remove node' )
    CASE (ERRFLAG_ERRFLAG_UNABLE_TO_SEARCH_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to search node' )
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
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

END FUNCTION GRIB_SECTION_REGISTER_REMOVE_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_FREE_NODE'
RECURSIVE FUNCTION GRIB_SECTION_REGISTER_FREE_NODE( CURRENT, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: CURRENT
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_NODE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_NODE=2_JPIB_K

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

  ! Implementation
  IF ( .NOT.ASSOCIATED( CURRENT, NIL ) ) THEN

    ! Deallocate left subtree.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_NODE) GRIB_SECTION_REGISTER_FREE_NODE( CURRENT%LEFT, HOOKS )

    ! Deallocate right subtree.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_NODE) GRIB_SECTION_REGISTER_FREE_NODE( CURRENT%RIGHT, HOOKS )

    ! Free memory
    PP_TRYCALL(ERRFLAG_UNABLE_TO_DEALLOCATE_NODE) DEALLOCATE_NODE( CURRENT, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_FREE_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free node' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate node' )
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

END FUNCTION GRIB_SECTION_REGISTER_FREE_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SEARCH'
PP_THREAD_SAFE FUNCTION SEARCH( ROOT, CURRENT, KEY, FOUND, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: CURRENT
  CHARACTER(LEN=KEY_LEN),                      INTENT(IN)    :: KEY
  LOGICAL,                                     INTENT(OUT)   :: FOUND
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: IS_LEAF
  LOGICAL :: KEY_LT
  LOGICAL :: KEY_GT
  LOGICAL :: KEY_EQ

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_LT_FAILURE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_EQ_FAILURE=3_JPIB_K

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

  ! Initialization.
  FOUND = .TRUE.
  CURRENT => ROOT

  ! Map is empty.
  IF ( ASSOCIATED(ROOT, NIL) ) THEN

    FOUND = .FALSE.

  ELSE

    ! Perform the search loop
    SearchLoop: DO

      !> Handle exit conditions
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( CURRENT, IS_LEAF, HOOKS )
      PP_TRYCALL(ERRFLAG_KEY_EQ_FAILURE) KEY_EQUAL_TO( KEY, CURRENT%KEY_, KEY_EQ, HOOKS )

      IF ( IS_LEAF .OR. KEY_EQ ) THEN
        EXIT SearchLoop
      ENDIF

      ! Left subtree
      PP_TRYCALL(ERRFLAG_KEY_LT_FAILURE) KEY_LOWER_THAN( KEY, CURRENT%KEY_, KEY_LT, HOOKS )
      IF ( KEY_LT ) THEN

        !> Check if the current node is a leaf
        PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( CURRENT%LEFT, IS_LEAF, HOOKS )

        !> Depending if it si a leaf or not, move to the left or exit
        IF ( .NOT.IS_LEAF ) THEN

          CURRENT => CURRENT%LEFT
          CYCLE SearchLoop

        ELSE

          FOUND = .FALSE.
          EXIT SearchLoop

        ENDIF

      ! Node Found
      PP_TRYCALL(ERRFLAG_KEY_EQ_FAILURE) KEY_EQUAL_TO( KEY, CURRENT%KEY_, KEY_EQ, HOOKS )
      ELSEIF ( KEY_EQ ) THEN

        FOUND = .TRUE.
        EXIT SearchLoop

      ! Right subtree
      ELSE

        !> Check if the current node is a leaf
        PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( CURRENT%RIGHT, IS_LEAF, HOOKS )

        !> Depending if it si a leaf or not, move to the right or exit
        IF ( .NOT.IS_LEAF ) THEN

          CURRENT => CURRENT%RIGHT

          CYCLE SearchLoop

        ELSE

          FOUND = .FALSE.
          EXIT SearchLoop

        ENDIF

      ENDIF

    ENDDO SearchLoop

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_KEY_LT_FAILURE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'key lower than failure' )
    CASE (ERRFLAG_KEY_EQ_FAILURE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'key equal to failure' )
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

END FUNCTION SEARCH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_NODE_INIT'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_REGISTER_NODE_INIT( THIS, PARENT, KEY, MAKE_FCN, DESTROY_FCN, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: GRIB_SECTION_BASE_MOD, ONLY: SECTION_ALLOCATOR_IF
  USE :: GRIB_SECTION_BASE_MOD, ONLY: SECTION_DEALLOCATOR_IF

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GRIB_SECTION_REGISTER_NODE_T),          INTENT(INOUT) :: THIS
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(IN)    :: PARENT
  CHARACTER(LEN=KEY_LEN),                      INTENT(IN)    :: KEY
  PROCEDURE(SECTION_ALLOCATOR_IF),   POINTER,  INTENT(IN)    :: MAKE_FCN
  PROCEDURE(SECTION_DEALLOCATOR_IF), POINTER,  INTENT(IN)    :: DESTROY_FCN
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

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

  IF ( .NOT. ASSOCIATED(PARENT) ) THEN
    NULLIFY(THIS%PARENT)
  ELSE
    THIS%PARENT => PARENT
  ENDIF
  THIS%RIGHT  => NIL
  THIS%LEFT   => NIL
  THIS%RED    = .TRUE.
  THIS%KEY_   = KEY
  THIS%IDX    = -99
  THIS%MAKE_FCN_ => MAKE_FCN
  THIS%DESTROY_FCN_ => DESTROY_FCN
  THIS%NALLOC_ = 0_JPIB_K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION GRIB_SECTION_REGISTER_NODE_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ALLOCATE_NODE'
PP_THREAD_SAFE FUNCTION ALLOCATE_NODE( CURRENT, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: CURRENT
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_NODE=1_JPIB_K

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

  ! Allocate, initialize and fill the fields.
  NULLIFY( CURRENT )
  ALLOCATE( CURRENT, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE_NODE )

  ! Copy key fields in current node.
  CURRENT%KEY_ = REPEAT(' ',KEY_LEN)

  ! Initialize the allocator
  CURRENT%MAKE_FCN_ => NULL()
  CURRENT%DESTROY_FCN_ => NULL()

  ! Connect pointers.
  CURRENT%LEFT => NIL
  CURRENT%RIGHT => NIL
  CURRENT%PARENT => NIL

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
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_NODE)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating keyset node: ' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating keyset node: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
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

END FUNCTION ALLOCATE_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DEALLOCATE_NODE'
PP_THREAD_SAFE FUNCTION DEALLOCATE_NODE( X, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: X
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_NODE=1_JPIB_K

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

  ! Check node is associated
  IF ( ASSOCIATED( X ) ) THEN

    ! Nullify the allocator
    X%KEY_ = REPEAT(' ',KEY_LEN)
    X%MAKE_FCN_ => NULL()
    X%DESTROY_FCN_ => NULL()

    ! Free node memory.
    DEALLOCATE( X, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE_NODE )
    NULLIFY( X )

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
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE_NODE)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating keyset node: ' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating keyset node: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
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

END FUNCTION DEALLOCATE_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INSERT_FIXUP'
PP_THREAD_SAFE FUNCTION INSERT_FIXUP( ROOT, CUR, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: CUR
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: Y
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: X

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ROTATE_LEFT=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ROTATE_RIGHT=2_JPIB_K

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
  X => NULL()
  Y => NULL()

  !Balancing tree.
  CUR%RED = .TRUE.

  X => CUR

  ! Climbing the tree to fix colors
  DO WHILE ( X%PARENT%RED .AND. .NOT.ASSOCIATED( X, ROOT ) )

    IF ( ASSOCIATED( X%PARENT%PARENT%LEFT, X%PARENT ) ) THEN

      Y => X%PARENT%PARENT%RIGHT   ! UNCLE

      IF ( Y%RED ) THEN

        Y%RED = .FALSE.

        X%PARENT%RED = .FALSE.

        X%PARENT%PARENT%RED = .TRUE.

        X => X%PARENT%PARENT

      ELSE

        IF ( ASSOCIATED( X, X%PARENT%RIGHT ) ) THEN

          X => X%PARENT

          PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_LEFT) ROTATE_LEFT( ROOT, X, HOOKS )

        ENDIF

        X%PARENT%RED = .FALSE.

        X%PARENT%PARENT%RED = .TRUE.

        PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_RIGHT) ROTATE_RIGHT( ROOT, X%PARENT%PARENT, HOOKS )

      ENDIF

    ELSE

      !...Must be right grandchild to get here.
      Y => X%PARENT%PARENT%LEFT    ! AUNT

      IF ( Y%RED ) THEN

        Y%RED = .FALSE.

        X%PARENT%RED = .FALSE.

        X%PARENT%PARENT%RED = .TRUE.

        X => X%PARENT%PARENT

      ELSE

        IF ( ASSOCIATED( X, X%PARENT%LEFT ) ) THEN

          X => X%PARENT

          PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_RIGHT) ROTATE_RIGHT( ROOT, X, HOOKS )

        ENDIF

        X%PARENT%RED = .FALSE.

        X%PARENT%PARENT%RED = .TRUE.

        PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_LEFT) ROTATE_LEFT( ROOT, X%PARENT%PARENT, HOOKS )

      ENDIF

    ENDIF

  ENDDO

  ! Change color of the root
  ROOT%RED = .FALSE.

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
    CASE (ERRFLAG_UNABLE_TO_ROTATE_LEFT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to rotate left' )
    CASE (ERRFLAG_UNABLE_TO_ROTATE_RIGHT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to rotate right' )
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

END FUNCTION INSERT_FIXUP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ROTATE_LEFT'
PP_THREAD_SAFE FUNCTION ROTATE_LEFT( ROOT, X_, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(GRIB_SECTION_REGISTER_NODE_T), TARGET,  INTENT(INOUT) :: X_
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  LOGICAL :: IS_ROOT
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: X
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: Y

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K

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

  ! Local variables initialization
  X => X_
  Y => NULL()

  ! Rotate.
  Y => X%RIGHT
  X%RIGHT => Y%LEFT
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%LEFT, IS_LEAF, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    Y%LEFT%PARENT => X

  ENDIF

  Y%PARENT => X%PARENT

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( X%PARENT, IS_ROOT, HOOKS )
  IF ( IS_ROOT ) THEN

    ROOT => Y

  ELSE

    IF ( ASSOCIATED( X, X%PARENT%LEFT ) ) THEN

      X%PARENT%LEFT => Y

    ELSE

      X%PARENT%RIGHT => Y

    ENDIF

  ENDIF

  Y%LEFT => X

  X%PARENT => Y

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
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

END FUNCTION ROTATE_LEFT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ROTATE_RIGHT'
PP_THREAD_SAFE FUNCTION ROTATE_RIGHT( ROOT, X_, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T),POINTER, INTENT(INOUT) :: ROOT
  TYPE(GRIB_SECTION_REGISTER_NODE_T), TARGET, INTENT(INOUT) :: X_
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  LOGICAL :: IS_ROOT
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: X
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: Y

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K

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

  ! Local variables initialization
  X => X_
  Y => NULL()

  ! Rotate.
  Y => X%LEFT
  X%LEFT => Y%RIGHT
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%RIGHT, IS_LEAF, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    Y%RIGHT%PARENT => X

  ENDIF

  Y%PARENT => X%PARENT


  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( X%PARENT, IS_ROOT, HOOKS )
  IF ( IS_ROOT ) THEN

    ROOT => Y

  ELSE

    IF ( ASSOCIATED( X, X%PARENT%RIGHT ) ) THEN

      X%PARENT%RIGHT => Y

    ELSE

      X%PARENT%LEFT => Y

    ENDIF

  ENDIF

  Y%RIGHT => X

  X%PARENT => Y

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
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

END FUNCTION ROTATE_RIGHT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NODE_ISLEAF'
PP_THREAD_SAFE FUNCTION NODE_ISLEAF( X, ISLEAF, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER,  INTENT(IN)    :: X
  LOGICAL,                                      INTENT(OUT)   :: ISLEAF
  TYPE(HOOKS_T),                                INTENT(INOUT) :: HOOKS

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

  ISLEAF = ASSOCIATED(X, NIL)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION NODE_ISLEAF
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SUCCESSOR'
PP_THREAD_SAFE FUNCTION SUCCESSOR( X, Y, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(IN)    :: X
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(OUT)   :: Y
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: X_

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NODE_IS_LEAF=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_MINIMUM=3_JPIB_K

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

  ! Initialisation.
  X_ => X
  Y => NULL()

  ! Check.
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( X, IS_LEAF, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( IS_LEAF, ERRFLAG_NODE_IS_LEAF )

  ! Search cycle.
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( X%RIGHT, IS_LEAF, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    ! If the node has a right child then the successor is the
    ! minimum of the  right subtree of the node.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_MINIMUM) MINIMUM( X%RIGHT, Y, HOOKS )

  ELSE

    ! If the node has not right child, then the successor is the
    ! nearest parent node whose left child is a parent of the
    ! node.
    Y => X%PARENT

    SearchSuccessor: DO

      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y, IS_LEAF, HOOKS )
      IF ( .NOT.IS_LEAF .AND. ASSOCIATED( X_, Y%RIGHT ) ) THEN

        X_ => Y

        Y => Y%PARENT

      ELSE

        EXIT SearchSuccessor

      ENDIF

    ENDDO SearchSuccessor

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_NODE_IS_LEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'node is leaf' )
    CASE (ERRFLAG_UNABLE_TO_CALL_MINIMUM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call minimum' )
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

END FUNCTION SUCCESSOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PREDECESSOR'
PP_THREAD_SAFE FUNCTION PREDECESSOR( X, Y, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(IN)    :: X
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(OUT)   :: Y
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: X_

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NODE_IS_LEAF=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_MAXIMUM=3_JPIB_K

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

  ! Initialisation.
  X_ => X
  Y => NULL()

  ! Check.
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( X, IS_LEAF, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( IS_LEAF, ERRFLAG_NODE_IS_LEAF )

  ! Search cycle.
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( X%LEFT, IS_LEAF, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    ! If the node has a left child then the successor is the
    ! minimum of the  left subtree of the node.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_MAXIMUM) MAXIMUM( X%LEFT, Y, HOOKS )

  ELSE

    ! If the node has not left child, then the predecessor is the
    ! nearest  parent node whose right child is a parent of the
    ! node.
    Y => X%PARENT

    SearchPredecessor:   DO

      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y, IS_LEAF, HOOKS )
      IF ( .NOT.IS_LEAF .AND. ASSOCIATED( X_, Y%LEFT ) ) THEN

        X_ => Y

        Y => Y%PARENT

      ELSE

        EXIT SearchPredecessor

      ENDIF

    ENDDO SearchPredecessor

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_NODE_IS_LEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'node is leaf' )
    CASE (ERRFLAG_UNABLE_TO_CALL_MAXIMUM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call maximum' )
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

END FUNCTION PREDECESSOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MINIMUM'
PP_THREAD_SAFE FUNCTION MINIMUM( X, Y, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(IN)    :: X
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(OUT)   :: Y
  TYPE(HOOKS_T),                                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: IS_LEAF

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K

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

  !> Initialization.
  Y => X

  ! Search cycle.
  SearchMinimum: DO WHILE(.TRUE.)

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%LEFT, IS_LEAF, HOOKS )
    IF ( .NOT.IS_LEAF ) THEN

      Y => Y%LEFT

    ELSE

      EXIT SearchMinimum

    ENDIF

  ENDDO SearchMinimum

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
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

END FUNCTION MINIMUM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAXIMUM'
PP_THREAD_SAFE FUNCTION MAXIMUM( X, Y, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(IN)    :: X
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(OUT)   :: Y
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: IS_LEAF

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K

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

  ! Initialization.
  Y => X

  ! Search cycle.
  SearchMaximum: DO WHILE(.TRUE.)

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%RIGHT, IS_LEAF, HOOKS )
    IF ( .NOT.IS_LEAF ) THEN

      Y => Y%RIGHT

    ELSE

      EXIT SearchMaximum

    ENDIF

  ENDDO SearchMaximum

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
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

END FUNCTION MAXIMUM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SWAP_DATA'
PP_THREAD_SAFE FUNCTION SWAP_DATA( NODE_1, NODE_2, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: GRIB_SECTION_BASE_MOD, ONLY: SECTION_ALLOCATOR_IF
  USE :: GRIB_SECTION_BASE_MOD, ONLY: SECTION_DEALLOCATOR_IF

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: NODE_1
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: NODE_2
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  PROCEDURE(SECTION_ALLOCATOR_IF), POINTER :: TMP_MAKE_FCN
  PROCEDURE(SECTION_ALLOCATOR_IF), POINTER :: TMP_DESTROY_FCN
  CHARACTER(LEN=KEY_LEN) :: TMP_KEY

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

  ! Saving into temporary variables NODE_2 data.
  TMP_KEY = NODE_2%KEY_
  TMP_MAKE_FCN => NODE_2%MAKE_FCN_
  TMP_DESTROY_FCN => NODE_2%DESTROY_FCN_

  ! Copy all data contained in node_1 into node_2.
  NODE_2%KEY_ = NODE_1%KEY_
  NODE_2%MAKE_FCN_ => NODE_1%MAKE_FCN_
  NODE_2%DESTROY_FCN_ => NODE_1%DESTROY_FCN_

  ! Copy temporay saved data in NODE_1
  NODE_1%KEY_ = TMP_KEY
  NODE_1%MAKE_FCN_ => TMP_MAKE_FCN
  NODE_1%DESTROY_FCN_ => TMP_DESTROY_FCN

  ! PAranoid stuff
  TMP_MAKE_FCN => NULL()
  TMP_DESTROY_FCN => NULL()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION SWAP_DATA
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REMOVE_NODE'
PP_THREAD_SAFE FUNCTION REMOVE_NODE( ROOT, Z, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: Z
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  LOGICAL :: IS_LEAF_LEFT
  LOGICAL :: IS_LEAF_RIGHT
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: Y
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: X

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_SUCESSOR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_REMOVE_FIXUP=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_DEALLOCATE_NODE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_SWAP_DATA=5_JPIB_K

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

  ! Initialization.
  X => NULL()
  Y => NULL()

  ! Rimozione del nodo.

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%LEFT,  IS_LEAF_LEFT,  HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%RIGHT, IS_LEAF_RIGHT, HOOKS )
  IF ( IS_LEAF_LEFT .OR. IS_LEAF_RIGHT ) THEN

    Y => Z

  ELSE

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_SUCESSOR) SUCCESSOR( Z, Y, HOOKS )

  ENDIF

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%LEFT,  IS_LEAF,  HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    X => Y%LEFT

  ELSE

    X => Y%RIGHT

  ENDIF

  ! Set the X parent, in the case of RED_BLACK_BALANCING the
  ! assignment is unconditioned else the assignment will be done
  ! only if X .NE. NIL
  IF ( RED_BLACK_BALANCING ) THEN

    X%PARENT => Y%PARENT

  ELSE

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( X,  IS_LEAF,  HOOKS )
    IF ( .NOT.IS_LEAF ) THEN

      X%PARENT => Y%PARENT

    ENDIF

  ENDIF

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%PARENT,  IS_LEAF,  HOOKS )
  IF ( IS_LEAF ) THEN

    ROOT => X

  ELSE

    IF ( ASSOCIATED( Y, Y%PARENT%LEFT) ) THEN

      Y%PARENT%LEFT => X

    ELSE

      Y%PARENT%RIGHT => X

    ENDIF

  ENDIF

  ! If the node to be deleted <z> and the node removed from the
  ! list <y> aren't  the some node, then copy the data contained in
  ! <y> in <z> and trash the old  <z> data.
  IF ( .NOT.ASSOCIATED( Z, Y ) ) THEN

    ! Copy data.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_SWAP_DATA) SWAP_DATA( Y, Z, HOOKS )

  ENDIF

  ! Adjust colors of the map.
  IF ( RED_BLACK_BALANCING ) THEN

    IF ( .NOT.Y%RED ) THEN

      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_REMOVE_FIXUP) REMOVE_NODE_FIXUP( ROOT, X, HOOKS )

    ENDIF

  ENDIF

  ! Free memory.
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_DEALLOCATE_NODE) DEALLOCATE_NODE( Y, HOOKS )
  Y => NULL()

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_UNABLE_TO_CALL_SUCESSOR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call successor' )
    CASE (ERRFLAG_UNABLE_TO_CALL_REMOVE_FIXUP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call remove fixup' )
    CASE (ERRFLAG_UNABLE_TO_CALL_DEALLOCATE_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call deallocate node' )
    CASE (ERRFLAG_UNABLE_TO_CALL_SWAP_DATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call swap data' )
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

END FUNCTION REMOVE_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REMOVE_NODE_FIXUP'
PP_THREAD_SAFE FUNCTION REMOVE_NODE_FIXUP( ROOT, X, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(INOUT) :: X
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: W

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NODE_IS_LEAF=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ROTATE_LEFT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ROTATE_RIGHT=4_JPIB_K

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

  ! Initialization.
  W=>NULL()

  ! Color fix-up cycle.
  ColorFixupLoop: DO WHILE( .NOT.ASSOCIATED( X, ROOT ) .AND. .NOT.X%RED )

    IF ( ASSOCIATED( X, X%PARENT%LEFT ) ) THEN

      W => X%PARENT%RIGHT

      IF ( W%RED ) THEN

        W%RED = .FALSE.

        X%PARENT%RED = .TRUE.

        PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_LEFT) ROTATE_LEFT(ROOT, X%PARENT, HOOKS )

        W => X%PARENT%RIGHT

      ENDIF

      ! Check if current node is a leaf
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( W, IS_LEAF, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( IS_LEAF, ERRFLAG_NODE_IS_LEAF )

      IF ( ( .NOT.W%LEFT%RED ) .AND. ( .NOT.W%RIGHT%RED ) ) THEN
        W%RED = .TRUE.

        X => X%PARENT

      ELSE

        IF ( .NOT.W%RIGHT%RED ) THEN ! CASO 3

          W%LEFT%RED = .FALSE.

          W%RED = .TRUE.

          PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_RIGHT) ROTATE_RIGHT(ROOT, W, HOOKS )

          W => X%PARENT%RIGHT

        ENDIF

        W%RED = X%PARENT%RED

        X%PARENT%RED = .FALSE.

        W%RIGHT%RED = .FALSE.

        PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_LEFT)  ROTATE_LEFT(ROOT, X%PARENT, HOOKS )

        X => ROOT

      ENDIF

    ELSE ! Right child

      W => X%PARENT%LEFT

      IF ( W%RED ) THEN

        W%RED = .FALSE.

        X%PARENT%RED = .TRUE.

        PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_RIGHT)  ROTATE_RIGHT(ROOT, X%PARENT, HOOKS )

        W => X%PARENT%LEFT

      ENDIF

      ! Check if current node is a leaf
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( W, IS_LEAF, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( IS_LEAF, ERRFLAG_NODE_IS_LEAF )

      IF ( .NOT.W%RIGHT%RED .AND. .NOT.W%LEFT%RED ) THEN

        W%RED = .TRUE.

        X => X%PARENT

      ELSE

        IF ( .NOT.W%LEFT%RED ) THEN

          W%RIGHT%RED = .FALSE.

          W%RED = .TRUE.

          PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_LEFT)  ROTATE_LEFT( ROOT, W, HOOKS )

          W => X%PARENT%LEFT

        ENDIF

        W%RED = X%PARENT%RED

        X%PARENT%RED = .FALSE.

        W%LEFT%RED = .FALSE.

        PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_RIGHT)  ROTATE_RIGHT( ROOT, X%PARENT, HOOKS )

        X => ROOT

      ENDIF

    ENDIF

  ENDDO ColorFixupLoop

  ! Change color to the node
  X%RED = .FALSE.

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_NODE_IS_LEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'node is leaf' )
    CASE (ERRFLAG_UNABLE_TO_ROTATE_LEFT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to rotate left' )
    CASE (ERRFLAG_UNABLE_TO_ROTATE_RIGHT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to rotate right' )
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

END FUNCTION REMOVE_NODE_FIXUP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_LIST_NODE'
RECURSIVE FUNCTION GRIB_SECTION_REGISTER_LIST_NODE( ROOT, CURRENT, CNT, UNIT, PREFIX, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(IN)    :: ROOT
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER, INTENT(IN)    :: CURRENT
  INTEGER(KIND=JPIB_K),                        INTENT(INOUT) :: CNT
  INTEGER(KIND=JPIB_K),                        INTENT(IN)    :: UNIT
  CHARACTER(LEN=*),                            INTENT(IN)    :: PREFIX
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=128) :: CKEY
  CHARACTER(LEN=128) :: CCNT
  LOGICAL :: IS_LEAF
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_LEFT_SUBTREE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_RIGHT_SUBTREE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_ERROR=4_JPIB_K

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

  ! First node in the list.
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( CURRENT, IS_LEAF, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_LEFT_SUBTREE) GRIB_SECTION_REGISTER_LIST_NODE( ROOT, CURRENT%LEFT, CNT, UNIT, PREFIX, HOOKS )

    CNT = CNT + 1
    WRITE(CKEY,*,IOSTAT=WRITE_STATUS) CURRENT%KEY_
    WRITE(CCNT,*,IOSTAT=WRITE_STATUS) CNT
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_ERROR )

    WRITE(UNIT,'(A,A,A,A,A)',IOSTAT=WRITE_STATUS) TRIM(ADJUSTL(PREFIX)), '(', TRIM(ADJUSTL(CCNT)), ') = ', TRIM(ADJUSTL(CKEY))
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_ERROR )


    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_RIGHT_SUBTREE) GRIB_SECTION_REGISTER_LIST_NODE( ROOT, CURRENT%RIGHT, CNT, UNIT, PREFIX, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_UNABLE_TO_CALL_LEFT_SUBTREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call left subtree' )
    CASE (ERRFLAG_UNABLE_TO_CALL_RIGHT_SUBTREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call right subtree' )
    CASE (ERRFLAG_WRITE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'write error' )
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

END FUNCTION GRIB_SECTION_REGISTER_LIST_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_RENUMBER_NODE'
RECURSIVE FUNCTION GRIB_SECTION_REGISTER_RENUMBER_NODE( ROOT, IDX, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), INTENT(INOUT) :: ROOT
  INTEGER(KIND=JPIB_K),               INTENT(INOUT) :: IDX
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: IS_LEAF

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_RENUMBER_NODE=2_JPIB_K

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

  ! Renumber left subtree
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT%LEFT, IS_LEAF, HOOKS )
  IF ( .NOT. IS_LEAF ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_RENUMBER_NODE) GRIB_SECTION_REGISTER_RENUMBER_NODE( ROOT%LEFT, IDX, HOOKS )
  ENDIF

  ! Renumber right subtree
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT%RIGHT, IS_LEAF, HOOKS )
  IF ( .NOT. IS_LEAF ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_RENUMBER_NODE) GRIB_SECTION_REGISTER_RENUMBER_NODE( ROOT%RIGHT, IDX, HOOKS )
  ENDIF

  ! Renumber the current node
  IDX = IDX + 1
  ROOT%IDX = IDX

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_UNABLE_TO_CALL_RENUMBER_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call renumber node' )
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

END FUNCTION GRIB_SECTION_REGISTER_RENUMBER_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_NODE_WRITE_CONNECTIVITY'
RECURSIVE FUNCTION GRIB_SECTION_REGISTER_NODE_WRITE_CONNECTIVITY( ROOT, UNIT, HOOKS ) RESULT(RET)

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
  TYPE(GRIB_SECTION_REGISTER_NODE_T), INTENT(INOUT) :: ROOT
  INTEGER(KIND=JPIB_K),               INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_WRITE_CONNECTIVITY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_ERROR=3_JPIB_K

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

  ! Write connectivity of the left subtree
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT%LEFT, IS_LEAF, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_WRITE_CONNECTIVITY) GRIB_SECTION_REGISTER_NODE_WRITE_CONNECTIVITY( ROOT%LEFT, UNIT, HOOKS )
  ENDIF

  ! Write connectivity of the right subtree
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT%RIGHT, IS_LEAF, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_WRITE_CONNECTIVITY) GRIB_SECTION_REGISTER_NODE_WRITE_CONNECTIVITY( ROOT%RIGHT, UNIT, HOOKS )
  ENDIF

  ! Write connectivity of the current node
  IF ( ASSOCIATED(ROOT%PARENT) ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT%PARENT, IS_LEAF, HOOKS )
    IF ( .NOT. IS_LEAF ) THEN
      WRITE(UNIT,'(I6.6,A,I6.6)',IOSTAT=WRITE_STATUS) ROOT%PARENT%IDX, '->', ROOT%IDX
      PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_ERROR )
    ENDIF
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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_UNABLE_TO_CALL_WRITE_CONNECTIVITY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call write connectivity' )
    CASE (ERRFLAG_WRITE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'write error' )
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

END FUNCTION GRIB_SECTION_REGISTER_NODE_WRITE_CONNECTIVITY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_WRITE_NODE'
RECURSIVE FUNCTION GRIB_SECTION_REGISTER_WRITE_NODE( ROOT, UNIT , HOOKS ) RESULT(RET)

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
  CLASS(GRIB_SECTION_REGISTER_NODE_T), INTENT(INOUT) :: ROOT
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS
  CHARACTER(LEN=128) :: CKEY

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_WRITE_NODE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_ERROR=3_JPIB_K

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

  ! Write nodes in the left subtree
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT%LEFT, IS_LEAF, HOOKS )
  IF ( .NOT. ASSOCIATED( ROOT%LEFT, NIL ) ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_WRITE_NODE) GRIB_SECTION_REGISTER_WRITE_NODE( ROOT%LEFT, UNIT, HOOKS )
  ENDIF

  ! Write nodes in the right subtree
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT%RIGHT, IS_LEAF, HOOKS )
  IF ( .NOT. ASSOCIATED( ROOT%RIGHT, NIL ) ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_WRITE_NODE) GRIB_SECTION_REGISTER_WRITE_NODE( ROOT%RIGHT, UNIT, HOOKS )
  ENDIF

  ! Write the current key
  CKEY = REPEAT( ' ', 128 )
  WRITE(CKEY,*,IOSTAT=WRITE_STATUS) ROOT%KEY_
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_ERROR )

  ! Write the current node
  IF ( ROOT%RED ) THEN
    WRITE(UNIT,'(I6.6,A,I8.8,A)', IOSTAT=WRITE_STATUS) ROOT%IDX, '   [ label="', TRIM(ADJUSTL(CKEY)) ,'", fillcolor=red]'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_ERROR )
  ELSE
    WRITE(UNIT,'(I6.6,A,I8.8,A)', IOSTAT=WRITE_STATUS) ROOT%IDX, '   [ label="', TRIM(ADJUSTL(CKEY)) ,'", fillcolor=black]'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_ERROR )
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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_WRITE_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map write node' )
    CASE (ERRFLAG_WRITE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'write error' )
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

END FUNCTION GRIB_SECTION_REGISTER_WRITE_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_INIT'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_REGISTER_INIT( GRIB_SECTION_REGISTER, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_SECTION_REGISTER_T), INTENT(INOUT) :: GRIB_SECTION_REGISTER
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

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

  ! Map initialization.
  GRIB_SECTION_REGISTER%ROOT => NIL
  GRIB_SECTION_REGISTER%SIZE = 0

  ! Nil initialisation
  !> @todo this needs to be moved somewhere else
  NIL%LEFT  => NIL
  NIL%RIGHT => NIL
  NIL%RED   =  .FALSE.
  NIL%KEY_  =  REPEAT( ' ', KEY_LEN )
  NIL%MAKE_FCN_ => NULL()
  NIL%DESTROY_FCN_ => NULL()
  NIL%NALLOC_ = 0_JPIB_K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION GRIB_SECTION_REGISTER_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_FREE'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_REGISTER_FREE( GRIB_SECTION_REGISTER, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_SECTION_REGISTER_T), INTENT(INOUT) :: GRIB_SECTION_REGISTER
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_FREE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_INIT=2_JPIB_K

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

  ! Remove the map if it is not empty.
  IF ( GRIB_SECTION_REGISTER%SIZE .GT. 0 ) THEN

    ! Recursive deletion of the tree.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_FREE) GRIB_SECTION_REGISTER_FREE_NODE( GRIB_SECTION_REGISTER%ROOT, HOOKS )

  ENDIF

  ! Reset the initial condition.
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_INIT) GRIB_SECTION_REGISTER_INIT( GRIB_SECTION_REGISTER, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map free' )
    CASE (ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_INIT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map init' )
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

END FUNCTION GRIB_SECTION_REGISTER_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_REGISTER'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_REGISTER_REGISTER( THIS, KEY, MAKE_FCN, DESTROY_FCN, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: GRIB_SECTION_BASE_MOD, ONLY: SECTION_ALLOCATOR_IF
  USE :: GRIB_SECTION_BASE_MOD, ONLY: SECTION_DEALLOCATOR_IF

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_SECTION_REGISTER_T),             INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                           INTENT(IN)    :: KEY
  PROCEDURE(SECTION_ALLOCATOR_IF), POINTER,   INTENT(IN)    :: MAKE_FCN
  PROCEDURE(SECTION_DEALLOCATOR_IF), POINTER, INTENT(IN)    :: DESTROY_FCN
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=KEY_LEN) :: LOC_KEY
  LOGICAL :: INSERTED

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_INSERT_NODE=1_JPIB_K

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

  ! Copy key to local variable
  LOC_KEY = REPEAT( ' ', KEY_LEN )
  LOC_KEY = KEY

  ! Call the routine to insert a node in the map
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_INSERT_NODE) GRIB_SECTION_REGISTER_INSERT_NODE( &
&                 THIS%ROOT, KEY, MAKE_FCN, DESTROY_FCN, INSERTED, HOOKS )

  ! Update the number of elements in the map
  IF ( INSERTED ) THEN
    THIS%SIZE = THIS%SIZE + 1
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
    CASE (ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_INSERT_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map insert node' )
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

END FUNCTION GRIB_SECTION_REGISTER_REGISTER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_MATCH'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_REGISTER_MATCH( THIS, KEY, MATCH, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_SECTION_REGISTER_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),               INTENT(IN)    :: KEY
  LOGICAL,                        INTENT(OUT)   :: MATCH
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: ROOT_IS_LEAF
  CHARACTER(LEN=KEY_LEN) :: LOC_KEY
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: SEARCHED_NODE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_SEARCH=2_JPIB_K

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

  ! Copy key to local variable
  LOC_KEY = REPEAT( ' ', KEY_LEN )
  LOC_KEY = KEY

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( THIS%ROOT, ROOT_IS_LEAF, HOOKS )
  IF ( ROOT_IS_LEAF ) THEN

    MATCH = .FALSE.

  ELSE

    ! Search the node in the map
    SEARCHED_NODE => NIL
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_SEARCH) SEARCH( THIS%ROOT, SEARCHED_NODE, KEY, MATCH, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_UNABLE_TO_CALL_SEARCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call search' )
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

END FUNCTION GRIB_SECTION_REGISTER_MATCH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_MAKE'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_REGISTER_MAKE( THIS, KEY, OUT_CLASS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_SECTION_REGISTER_T),      INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                    INTENT(IN)    :: KEY
  CLASS(GRIB_SECTION_BASE_A), POINTER, INTENT(INOUT) :: OUT_CLASS
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: MATCH
  LOGICAL :: ROOT_IS_LEAF
  CHARACTER(LEN=KEY_LEN) :: LOC_KEY
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: SEARCHED_NODE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_SEARCH=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_REGISTER_EMPTY=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATOR_NOT_FOUND=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATOR_NOT_ASSOCIATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE=6_JPIB_K

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

  ! Copy key to local variable
  LOC_KEY = REPEAT( ' ', KEY_LEN )
  LOC_KEY = KEY

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( THIS%ROOT, ROOT_IS_LEAF, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( ROOT_IS_LEAF, ERRFLAG_REGISTER_EMPTY )

  ! Search the node in the map
  SEARCHED_NODE => NIL
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_SEARCH) SEARCH( THIS%ROOT, SEARCHED_NODE, KEY, MATCH, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.MATCH, ERRFLAG_ALLOCATOR_NOT_FOUND )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(SEARCHED_NODE), ERRFLAG_ALLOCATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(SEARCHED_NODE%MAKE_FCN_), ERRFLAG_ALLOCATOR_NOT_ASSOCIATED )

  ! Allocate the class if it is not found
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE) SEARCHED_NODE%MAKE_FCN_( OUT_CLASS, HOOKS )
  SEARCHED_NODE%NALLOC_ = SEARCHED_NODE%NALLOC_ + 1

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_UNABLE_TO_CALL_SEARCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call search' )
    CASE (ERRFLAG_REGISTER_EMPTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'register empty' )
    CASE (ERRFLAG_ALLOCATOR_NOT_FOUND)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'constructor not found' )
    CASE (ERRFLAG_ALLOCATOR_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'constructor not associated' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate' )
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

END FUNCTION GRIB_SECTION_REGISTER_MAKE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_DESTROY'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_REGISTER_DESTROY( THIS, KEY, IN_CLASS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_SECTION_REGISTER_T),      INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                    INTENT(IN)    :: KEY
  CLASS(GRIB_SECTION_BASE_A), POINTER, INTENT(INOUT) :: IN_CLASS
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: MATCH
  LOGICAL :: ROOT_IS_LEAF
  CHARACTER(LEN=KEY_LEN) :: LOC_KEY
  TYPE(GRIB_SECTION_REGISTER_NODE_T), POINTER :: SEARCHED_NODE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_SEARCH=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_REGISTER_EMPTY=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATOR_NOT_FOUND=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATOR_NOT_ASSOCIATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_DEALLOCATION=7_JPIB_K

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

  ! Copy key to local variable
  LOC_KEY = REPEAT( ' ', KEY_LEN )
  LOC_KEY = KEY

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( THIS%ROOT, ROOT_IS_LEAF, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( ROOT_IS_LEAF, ERRFLAG_REGISTER_EMPTY )

  ! Search the node in the map
  SEARCHED_NODE => NIL
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_SEARCH) SEARCH( THIS%ROOT, SEARCHED_NODE, KEY, MATCH, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.MATCH, ERRFLAG_DEALLOCATOR_NOT_FOUND )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(SEARCHED_NODE), ERRFLAG_DEALLOCATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(SEARCHED_NODE%DESTROY_FCN_), ERRFLAG_DEALLOCATOR_NOT_ASSOCIATED )

  ! Allocate the class if it is not found
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DEALLOCATE) SEARCHED_NODE%DESTROY_FCN_( IN_CLASS, HOOKS )
  SEARCHED_NODE%NALLOC_ = SEARCHED_NODE%NALLOC_ - 1

  ! Check the number of deallocations
  PP_DEBUG_CRITICAL_COND_THROW( SEARCHED_NODE%NALLOC_ .LT. 0, ERRFLAG_INVALID_DEALLOCATION )

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_UNABLE_TO_CALL_SEARCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call search' )
    CASE (ERRFLAG_REGISTER_EMPTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'register empty' )
    CASE (ERRFLAG_DEALLOCATOR_NOT_FOUND)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'constructor not found' )
    CASE (ERRFLAG_DEALLOCATOR_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'constructor not associated' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate' )
    CASE (ERRFLAG_INVALID_DEALLOCATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid deallocation (more dallocations that allocations)' )
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

END FUNCTION GRIB_SECTION_REGISTER_DESTROY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_REMOVE'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_REGISTER_REMOVE( THIS, KEY, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_SECTION_REGISTER_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),               INTENT(IN)    :: KEY
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=KEY_LEN) :: LOC_KEY
  LOGICAL :: ROOT_IS_LEAF
  LOGICAL :: FOUND

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_REMOVE_NODE=2_JPIB_K

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

  ! Copy key to local variable
  LOC_KEY = REPEAT( ' ', KEY_LEN )
  LOC_KEY = KEY

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( THIS%ROOT, ROOT_IS_LEAF, HOOKS )
  IF ( .NOT.ROOT_IS_LEAF ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_REMOVE_NODE) GRIB_SECTION_REGISTER_REMOVE_NODE( THIS%ROOT, KEY, FOUND, HOOKS )
    IF ( FOUND ) THEN
      THIS%SIZE = THIS%SIZE - 1
    ENDIF
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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_REMOVE_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map remove node' )
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

END FUNCTION GRIB_SECTION_REGISTER_REMOVE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_LIST'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_REGISTER_LIST( THIS, UNIT, PREFIX, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_SECTION_REGISTER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),           INTENT(IN)    :: UNIT
  CHARACTER(LEN=*),               INTENT(IN)    :: PREFIX
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: CNT

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_LIST_NODE=1_JPIB_K

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

  ! Call the recursive writing
  CNT = 0_JPIB_K
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_LIST_NODE) GRIB_SECTION_REGISTER_LIST_NODE( THIS%ROOT, THIS%ROOT, CNT, UNIT, PREFIX, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_LIST_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map list node' )
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

END FUNCTION GRIB_SECTION_REGISTER_LIST
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_REGISTER_PRINT'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_REGISTER_PRINT( THIS, NAME, IDX, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_SECTION_REGISTER_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),               INTENT(IN)    :: NAME
  INTEGER(KIND=JPIB_K),           INTENT(IN)    :: IDX
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=128)   :: FNAME
  INTEGER(KIND=JPIB_K) :: UNIT
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS
  LOGICAL              :: ROOT_IS_LEAF

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_FILE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_RENUMBER_NODE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_WRITE_NODE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_WRITE_CONNECTIVITY=5_JPIB_K

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

  FNAME=REPEAT(' ',128)
  WRITE(FNAME,'(A,I8.8,A)',IOSTAT=WRITE_STATUS) TRIM(ADJUSTL(NAME))//'_', IDX, '.dot'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_FILE )

  UNIT = 0
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_RENUMBER_NODE) GRIB_SECTION_REGISTER_RENUMBER_NODE( THIS%ROOT, UNIT, HOOKS )
  UNIT=131


  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( THIS%ROOT, ROOT_IS_LEAF, HOOKS )
  IF ( .NOT. ROOT_IS_LEAF ) THEN
    OPEN(unit=unit, file=TRIM(FNAME), action='write',IOSTAT=WRITE_STATUS )
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_FILE )
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STATUS) 'digraph RedBlackTree {'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_FILE )
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STATUS) 'node [shape=circle, style=filled, fontcolor=white, fontsize=12, width=0.5]'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_FILE )
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STATUS) 'edge [arrowhead=vee]'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_FILE )
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STATUS) ''
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_FILE )
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STATUS) '// Nodes'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_FILE )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_WRITE_NODE) GRIB_SECTION_REGISTER_WRITE_NODE( THIS%ROOT, UNIT, HOOKS )
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STATUS) ''
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_FILE )
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STATUS) '// Connectivity'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_FILE )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_WRITE_CONNECTIVITY) GRIB_SECTION_REGISTER_NODE_WRITE_CONNECTIVITY( THIS%ROOT, UNIT, HOOKS )
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STATUS) ''
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_FILE )
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STATUS) '}'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_FILE )
    CLOSE( UNIT,IOSTAT=WRITE_STATUS )
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_FILE )
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
    CASE (ERRFLAG_UNABLE_TO_WRITE_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to write file' )
    CASE (ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_RENUMBER_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map renumber node' )
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_WRITE_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map write node' )
    CASE (ERRFLAG_UNABLE_TO_CALL_CLASS_REGISTER_WRITE_CONNECTIVITY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map write connectivity' )
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

END FUNCTION GRIB_SECTION_REGISTER_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB_ENCODER_REGISTER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
