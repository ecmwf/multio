!> @file map_mod.F90
!>
!> @brief Module containing the implementation of a Red Black tree.
!>
!> Implementation of a Red Black tree.
!> Every routine in this file is deeply explained in the book:
!> "Introduction to Algorithms"
!> { Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, Clifford Stein}
!>
!> @todo improve error handling
!>
!> @author Mirco Valentini
!> @date   January 31, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'encoding_cache_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ENCODING_CACHE_MOD'
MODULE ENCODING_CACHE_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,             ONLY: JPIB_K
  USE :: CACHED_ENCODER_COLLECTION_MOD, ONLY: CACHED_ENCODER_COLLECTION_T
  USE :: FORTRAN_MESSAGE_MOD,           ONLY: FORTRAN_MESSAGE_T

IMPLICIT NONE

!> @brief Default visibility of the module
PRIVATE

!> @brief Flag used to enable the tree balancing
LOGICAL, PARAMETER :: RED_BLACK_BALANCING=.TRUE.



INTERFACE
  PP_THREAD_SAFE FUNCTION FUNCTION_ENCODING_CACHE_IF( KEY, CACHE_OPT, HOOKS ) RESULT(RET)
    ! Symbols imported from other modules within the project.
    USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
    USE :: HOOKS_MOD,                ONLY: HOOKS_T
    USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
    USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T
  IMPLICIT NONE
    TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: KEY
    TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: CACHE_OPT
    TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS
    INTEGER(KIND=JPIB_K) :: RET
  END FUNCTION FUNCTION_ENCODING_CACHE_IF
END INTERFACE

TYPE, ABSTRACT :: FUNCTOR_ENCODING_CACHE_A
CONTAINS
PROCEDURE(FUNCTOR_ENCODING_CACHE_IF), DEFERRED, PUBLIC, PASS :: APPLY
END TYPE

ABSTRACT INTERFACE
  PP_THREAD_SAFE FUNCTION FUNCTOR_ENCODING_CACHE_IF( THIS, KEY, CACHE_OPT, HOOKS ) RESULT(RET)
    ! Symbols imported from other modules within the project.
    USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
    USE :: HOOKS_MOD,                ONLY: HOOKS_T
    USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
    USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T
    IMPORT :: FUNCTOR_ENCODING_CACHE_A
  IMPLICIT NONE
    CLASS(FUNCTOR_ENCODING_CACHE_A), INTENT(INOUT) :: THIS
    TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: KEY
    TYPE(CACHE_OPTIONS_T),           INTENT(IN)    :: CACHE_OPT
    TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS
    INTEGER(KIND=JPIB_K) :: RET
  END FUNCTION FUNCTOR_ENCODING_CACHE_IF
END INTERFACE


!> @brief Datatype used to to store a node of the tree
TYPE :: ENCODING_CACHE_NODE_T

  !> Key
  TYPE(FORTRAN_MESSAGE_T) :: KEY

  !> Value
  TYPE(CACHED_ENCODER_COLLECTION_T), POINTER :: ENCODERS_ => NULL()

  !> Color
  LOGICAL :: RED = .FALSE.

  !> Index used to dump the graph
  INTEGER(KIND=JPIB_K) :: IDX

  !> Pointer to the parent node
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: PARENT => NULL()

  !> Pointer to the lef subtree
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: LEFT   => NULL()

  !> Pointer to the right subtree
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: RIGHT  => NULL()

END TYPE


!> @brief Datatype used to to store the entire map
TYPE :: ENCODING_CACHE_T

  !> Pointer to the class
  PRIVATE

  !> Pointer to the root node
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: ROOT => NULL()

  !> Size of the map
  INTEGER(KIND=JPIB_K) :: SIZE_ = -1_JPIB_K

CONTAINS

  !> Public methods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT             => ENCODING_CACHE_INIT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: ACCESS_OR_CREATE => ENCODING_CACHE_ACCESS_OR_CREATE
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: REMOVE           => ENCODING_CACHE_REMOVE
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE             => ENCODING_CACHE_FREE
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: MATCH            => ENCODING_CACHE_MATCH
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SIZE             => ENCODING_CACHE_SIZE
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: MAXIMUM          => ENCODING_CACHE_MAXIMUM
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: MINIMUM          => ENCODING_CACHE_MINIMUM
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_SORTED_KEYS  => ENCODING_CACHE_GET_SORTED_KEYS
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: LIST             => ENCODING_CACHE_LIST
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PRINT            => ENCODING_CACHE_PRINT


  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: DUMP             => ENCODING_CACHE_DUMP
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: BYTESIZE         => ENCODING_CACHE_BYTESIZE
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: APPLY_FUNCTION   => ENCODING_CACHE_APPLY_FUNCTION
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: APPLY_FUNCTOR    => ENCODING_CACHE_APPLY_FUNCTOR

END TYPE


!> @brief Node used as terminal symbol
TYPE(ENCODING_CACHE_NODE_T), TARGET :: NIL

!> Whitelist of public symbols
PUBLIC :: ENCODING_CACHE_T
PUBLIC :: FUNCTION_ENCODING_CACHE_IF
PUBLIC :: FUNCTOR_ENCODING_CACHE_A

CONTAINS




!>
!> @brief Inserts a key-value pair into a map (Red Black Tree).
!>
!> This subroutine comares two keys and returns a flag indicating if the
!> first key is equal to the second key.
!>
!> @param [in]    KEY1     The first key to be compared
!> @param [in]    KEY2     The second key to be compares
!> @param [out]   IS_EQUAL Flag indicating if the keys are equal
!> @param [inout] HOOKS    The hooks structure
!>
!> @return Integer error code (`RET`) indicating the success or failure of the initialization.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'KEY_EQUAL_TO'
PP_THREAD_SAFE FUNCTION KEY_EQUAL_TO( KEY1, KEY2, CACHE_OPT, IS_EQUAL, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,     ONLY: CACHE_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FORTRAN_MESSAGE_T), INTENT(IN)    :: KEY1
  TYPE(FORTRAN_MESSAGE_T), INTENT(IN)    :: KEY2
  TYPE(CACHE_OPTIONS_T),   INTENT(IN)    :: CACHE_OPT
  LOGICAL,                 INTENT(OUT)   :: IS_EQUAL
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_COMPARE_ERROR=1_JPIB_K

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

  ! Check if the keys are equal (the check depends on the options)
  PP_TRYCALL(ERRFLAG_COMPARE_ERROR) KEY1%IS_EQUAL_TO( KEY2, CACHE_OPT, IS_EQUAL, HOOKS )

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
    CASE (ERRFLAG_COMPARE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'compare error' )
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


!>
!> @brief Inserts a key-value pair into a map (Red Black Tree).
!>
!> This subroutine comares two keys and returns a flag indicating if the
!> first key is lower than the second key.
!>
!> @param [in]    KEY1          The first key to be compared
!> @param [in]    KEY2          The second key to be compares
!> @param [out]   IS_LOWER_THAN Flag indicating if the first key is
!>                              lower than the second key
!> @param [inout] HOOKS         The hooks structure
!>
!> @return Integer error code (`RET`) indicating the success or failure of the initialization.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'KEY_LOWER_THAN'
PP_THREAD_SAFE FUNCTION KEY_LOWER_THAN( KEY1, KEY2, CACHE_OPT, IS_LOWER_THAN, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,     ONLY: CACHE_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FORTRAN_MESSAGE_T), INTENT(IN)    :: KEY1
  TYPE(FORTRAN_MESSAGE_T), INTENT(IN)    :: KEY2
  TYPE(CACHE_OPTIONS_T),   INTENT(IN)    :: CACHE_OPT
  LOGICAL,                 INTENT(OUT)   :: IS_LOWER_THAN
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_COMPARE_ERROR=1_JPIB_K

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

  ! Check if the keys are lower than (the check depends on the options)
  PP_TRYCALL(ERRFLAG_COMPARE_ERROR) KEY1%IS_LOWER_THAN( KEY2, CACHE_OPT, IS_LOWER_THAN, HOOKS )

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
    CASE (ERRFLAG_COMPARE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'compare error' )
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


!>
!> @brief Inserts a key-value pair into a map (Red Black Tree).
!>
!> This subroutine inserts a key-value pair into a map data structure.
!> It navigates the map structure and inserts the provided key and value appropriately.
!>
!> @param [inout] ROOT  Pointer to the root node of the map.
!>                      The map structure will be modified to include
!>                      the new key-value pair.
!> @param [in]    KEY   The key to be inserted into the map.
!>
!> @note This subroutine assumes that the map data structure is properly initialized.
!>        It is the responsibility of the caller to ensure that the map is correctly set up
!>
!> @warning This subroutine does not perform any action if the specified key is already found in the map.
!>          It is the responsibility of the caller to handle such scenarios appropriately.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ACCESS_OR_CREATE_NODE'
PP_THREAD_SAFE FUNCTION ACCESS_OR_CREATE_NODE( ROOT, KEY, PAR, METADATA, ENCODING_RULES, &
&                                        ENCODERS, INSERTED, CACHE_OPT, ENCODER_OPT, FILTER_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,             ONLY: JPIB_K
  USE :: HOOKS_MOD,                     ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD,      ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FILTER_OPTIONS_MOD,            ONLY: FILTER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,           ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,           ONLY: PARAMETRIZATION_T
  USE :: ENCODING_RULE_COLLECTION_MOD,  ONLY: ENCODING_RULE_COLLECTION_T
  USE :: CACHED_ENCODER_COLLECTION_MOD, ONLY: CACHED_ENCODER_COLLECTION_T
  USE :: CACHE_UTILS_MOD,               ONLY: CACHE_OPTIONS_T
  USE :: METADATA_BASE_MOD,             ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER,       INTENT(INOUT) :: ROOT
  TYPE(FORTRAN_MESSAGE_T),                    INTENT(IN)    :: KEY
  TYPE(PARAMETRIZATION_T),                    INTENT(IN)    :: PAR
  CLASS(METADATA_BASE_A), POINTER,            INTENT(IN)    :: METADATA
  TYPE(ENCODING_RULE_COLLECTION_T),           INTENT(IN)    :: ENCODING_RULES
  TYPE(CACHED_ENCODER_COLLECTION_T), POINTER, INTENT(OUT)   :: ENCODERS
  LOGICAL,                                    INTENT(OUT)   :: INSERTED
  TYPE(CACHE_OPTIONS_T),                      INTENT(IN)    :: CACHE_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T),               INTENT(IN)    :: ENCODER_OPT
  TYPE(FILTER_OPTIONS_T),                     INTENT(IN)    :: FILTER_OPT
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: INSERTION_POINT
  LOGICAL :: FOUND
  LOGICAL :: KEY_LT
  LOGICAL :: ROOT_IS_LEAF

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_NODE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INITIALIZE_NODE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FIXUP_INSERT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SEARCH_NODE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_LT_FAILURE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ROOT_NOT_ASSOCIATED=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA_NOT_ASSOCIATED=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODERS_NOT_ASSOCIATED=9_JPIB_K

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

  !> Error handling (In case of empty map Root still needs to be associated to NIL)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED( ROOT ), ERRFLAG_ROOT_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED( METADATA ), ERRFLAG_METADATA_NOT_ASSOCIATED )

  ! Initialization of the insertion flag
  INSERTED =.FALSE.

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT, ROOT_IS_LEAF, CACHE_OPT, HOOKS )
  IF ( ROOT_IS_LEAF ) THEN

  ! Map is empty
    INSERTED =.TRUE.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE_NODE) ALLOCATE_NODE( ROOT, CACHE_OPT, HOOKS )
    INSERTION_POINT => NIL
    PP_TRYCALL(ERRFLAG_UNABLE_TO_INITIALIZE_NODE) NODE_INIT( ROOT, INSERTION_POINT, KEY, &
&       PAR, METADATA, ENCODING_RULES, CACHE_OPT, ENCODER_OPT, FILTER_OPT, HOOKS )
    ENCODERS => ROOT%ENCODERS_
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FIXUP_INSERT) INSERT_FIXUP( ROOT, ROOT, CACHE_OPT, HOOKS )
    INSERTION_POINT => ROOT
      PP_LOG_INFO( 'Node created (map empty)' )
  ELSE

    ! Map not empty
    INSERTION_POINT => NIL
    PP_TRYCALL(ERRFLAG_UNABLE_TO_SEARCH_NODE) SEARCH_NODE( ROOT, INSERTION_POINT, KEY, CACHE_OPT, FOUND, HOOKS )

    IF ( .NOT.FOUND ) THEN
      ! If the node is not in the map then insert it
      INSERTED =.TRUE.
      PP_TRYCALL(ERRFLAG_KEY_LT_FAILURE) KEY_LOWER_THAN( KEY, INSERTION_POINT%KEY, CACHE_OPT, KEY_LT, HOOKS )
      IF ( KEY_LT ) THEN
        PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE_NODE) ALLOCATE_NODE( INSERTION_POINT%LEFT, CACHE_OPT, HOOKS )
        PP_TRYCALL(ERRFLAG_UNABLE_TO_INITIALIZE_NODE) NODE_INIT( INSERTION_POINT%LEFT, INSERTION_POINT, &
&             KEY, PAR, METADATA, ENCODING_RULES, CACHE_OPT, ENCODER_OPT, FILTER_OPT, HOOKS )
        ENCODERS => INSERTION_POINT%LEFT%ENCODERS_
        PP_TRYCALL(ERRFLAG_UNABLE_TO_FIXUP_INSERT) INSERT_FIXUP( ROOT, INSERTION_POINT%LEFT, CACHE_OPT, HOOKS )
      ELSE
        PP_TRYCALL(ERRFLAG_UNABLE_TO_INITIALIZE_NODE) ALLOCATE_NODE( INSERTION_POINT%RIGHT, CACHE_OPT, HOOKS )
        PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE_NODE) NODE_INIT( INSERTION_POINT%RIGHT, INSERTION_POINT, &
&             KEY, PAR, METADATA, ENCODING_RULES, CACHE_OPT, ENCODER_OPT, FILTER_OPT, HOOKS )
        ENCODERS => INSERTION_POINT%RIGHT%ENCODERS_
        PP_TRYCALL(ERRFLAG_UNABLE_TO_FIXUP_INSERT) INSERT_FIXUP( ROOT, INSERTION_POINT%RIGHT, CACHE_OPT, HOOKS )
      ENDIF
      PP_LOG_INFO( 'Node created' )
    ELSE
      PP_LOG_INFO( 'Node already in the map' )
      ! If the node is in the map then return the value
      INSERTED =.FALSE.
      ENCODERS => INSERTION_POINT%ENCODERS_
    ENDIF

  ENDIF


  ! At exit ENCODERS must be associated
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED( ENCODERS ), ERRFLAG_ENCODERS_NOT_ASSOCIATED )

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
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_ROOT_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'root not associated' )
    CASE (ERRFLAG_METADATA_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'metadata not associated' )
    CASE (ERRFLAG_ENCODERS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'encoders not associated' )
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

END FUNCTION ACCESS_OR_CREATE_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Removes a key-value pair from a map.
!>
!> This subroutine removes a key-value pair from a map data structure.
!> It searches for the specified key in the map structure and removes the corresponding entry.
!>
!> @param [inout] ROOT Pointer to the root node of the map.
!>                     The map structure will be modified to exclude the specified key-value pair.
!> @param [in]    KEY  The key whose associated value is to be removed from the map.
!>
!> @note This subroutine assumes that the map data structure is properly initialized.
!>        It is the responsibility of the caller to ensure that the map is correctly set up
!>
!> @warning This subroutine does not perform any action if the specified key is not found in the map.
!>           It is the responsibility of the caller to handle such scenarios appropriately.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DELETE_NODE'
PP_THREAD_SAFE FUNCTION DELETE_NODE( ROOT, KEY, CACHE_OPT, ENCODER_OPT, FOUND, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(FORTRAN_MESSAGE_T),              INTENT(IN)    :: KEY
  LOGICAL,                              INTENT(OUT)   :: FOUND
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T),         INTENT(IN)    :: ENCODER_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: INSERTION_POINT
  LOGICAL :: ROOT_IS_LEAF

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

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT, ROOT_IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT.ROOT_IS_LEAF ) THEN
    PP_TRYCALL(ERRFLAG_ERRFLAG_UNABLE_TO_SEARCH_NODE) SEARCH_NODE( ROOT, INSERTION_POINT, KEY, CACHE_OPT, FOUND, HOOKS )
    IF ( FOUND ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_REMOVE_NODE) REMOVE_NODE( ROOT, INSERTION_POINT, CACHE_OPT, ENCODER_OPT, HOOKS )
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

END FUNCTION DELETE_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Recursively frees memory associated with a map node.
!>
!>  This subroutine recursively frees memory associated with a map node and its descendants.
!>  It traverses the map structure starting from the specified node and deallocates memory for each node encountered.
!>
!>  @param [inout] CURRENT Pointer to the current node in the map structure.
!>                         The subroutine frees memory associated with this node and its descendants.
!>  @param [out]   ERR     Output parameter indicating the error status of the operation.
!>                         A non-zero value indicates an error occurred during the memory deallocation process.
!>
!>  @note This subroutine assumes that the map nodes and associated memory are properly initialized.
!>        It is the responsibility of the caller to ensure that the map structure is correctly set up.
!>
#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'FREE_NODE'
RECURSIVE FUNCTION FREE_NODE( CURRENT, CACHE_OPT, ENCODER_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: CURRENT
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T),         INTENT(IN)    :: ENCODER_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: NODE_IS_LEAF

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_NODE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_NODE=2_JPIB_K
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

  ! Implementation
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( CURRENT, NODE_IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT.NODE_IS_LEAF ) THEN

    ! Deallocate left subtree.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_NODE) FREE_NODE( CURRENT%LEFT, CACHE_OPT, ENCODER_OPT, HOOKS )

    ! Deallocate right subtree.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_NODE) FREE_NODE( CURRENT%RIGHT, CACHE_OPT, ENCODER_OPT, HOOKS )

    ! Free memory
    PP_TRYCALL(ERRFLAG_UNABLE_TO_DEALLOCATE_NODE) DEALLOCATE_NODE( CURRENT, CACHE_OPT, ENCODER_OPT, HOOKS )

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

END FUNCTION FREE_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Searches for a key in a map structure.
!>
!> This subroutine searches for a specified key in a map structure starting from a given node.
!> It traverses the map structure recursively, comparing keys until the desired key is found or the search terminates.
!>
!> @param [inout] ROOT    Pointer to the root node of the map structure.
!>                        The search operation starts from this node.
!> @param [inout] CURRENT Pointer to the current node being evaluated during the search operation.
!>                        This pointer is updated during the recursive traversal of the map structure.
!> @param [in]    KEY     The key to search for in the map structure.
!> @param [out]   ERR     Output parameter indicating the status of the search operation.
!>                        A non-zero value indicates an error occurred during the search process, such as key not found.
!>
!> @note This subroutine assumes that the map structure is properly initialized.
!>       It is the responsibility of the caller to ensure that the map is correctly set up.
!>
!> @warning This subroutine may modify the value of the `CURRENT` pointer during the search process.
!>          The `ERR` parameter will be set to a non-zero value if the key is not found during the search.
!>          It is the responsibility of the caller to handle such scenarios appropriately.
!>          The caller should check the value of `ERR` after calling this subroutine to determine the outcome of the search.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SEARCH_NODE'
PP_THREAD_SAFE FUNCTION SEARCH_NODE( ROOT, CURRENT, KEY, CACHE_OPT, FOUND, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  !> Templated use
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: CURRENT
  TYPE(FORTRAN_MESSAGE_T),              INTENT(IN)    :: KEY
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  LOGICAL,                              INTENT(OUT)   :: FOUND
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: ROOT_IS_LEAF
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

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT, ROOT_IS_LEAF, CACHE_OPT, HOOKS )
  IF ( ROOT_IS_LEAF ) THEN

    FOUND = .FALSE.

  ELSE

    ! Perform the search loop
    SearchLoop: DO

      !> Handle exit conditions
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( CURRENT, IS_LEAF, CACHE_OPT, HOOKS )
      PP_TRYCALL(ERRFLAG_KEY_EQ_FAILURE) KEY_EQUAL_TO( KEY, CURRENT%KEY, CACHE_OPT, KEY_EQ, HOOKS )

      IF ( IS_LEAF .OR. KEY_EQ ) THEN
        EXIT SearchLoop
      ENDIF

      ! Left subtree
      PP_TRYCALL(ERRFLAG_KEY_LT_FAILURE) KEY_LOWER_THAN( KEY, CURRENT%KEY, CACHE_OPT, KEY_LT, HOOKS )
      IF ( KEY_LT ) THEN

        !> Check if the current node is a leaf
        PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( CURRENT%LEFT, IS_LEAF, CACHE_OPT, HOOKS )

        !> Depending if it si a leaf or not, move to the left or exit
        IF ( .NOT.IS_LEAF ) THEN

          CURRENT => CURRENT%LEFT
          CYCLE SearchLoop

        ELSE

          FOUND = .FALSE.
          EXIT SearchLoop

        ENDIF

      ! Node Found
      PP_TRYCALL(ERRFLAG_KEY_EQ_FAILURE) KEY_EQUAL_TO( KEY, CURRENT%KEY, CACHE_OPT, KEY_EQ, HOOKS )
      ELSEIF ( KEY_EQ ) THEN

        FOUND = .TRUE.
        EXIT SearchLoop

      ! Right subtree
      ELSE

        !> Check if the current node is a leaf
        PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( CURRENT%RIGHT, IS_LEAF, CACHE_OPT, HOOKS )

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

END FUNCTION SEARCH_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Initializes a map node with the provided information.
!>
!> This subroutine initializes a map node with the specified parent, key, and value.
!> It sets the internal attributes of the node accordingly.
!>
!> @param [inout] THIS   The map node to be initialized.
!> @param [in]    PARENT Pointer to the parent node of the current node.
!>                       This parameter can be NULL if the node has no parent.
!> @param [in]    KEY    The key associated with the node.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NODE_INIT'
PP_THREAD_SAFE FUNCTION NODE_INIT( THIS, PARENT, KEY, PAR, &
  METADATA, ENCODING_RULES, CACHE_OPT, ENCODER_OPT, FILTER_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,             ONLY: JPIB_K
  USE :: HOOKS_MOD,                     ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,               ONLY: CACHE_OPTIONS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD,      ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FILTER_OPTIONS_MOD,            ONLY: FILTER_OPTIONS_T
  USE :: PARAMETRIZATION_MOD,           ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,           ONLY: FORTRAN_MESSAGE_T
  USE :: ENCODING_RULE_COLLECTION_MOD,  ONLY: ENCODING_RULE_COLLECTION_T
  USE :: CACHED_ENCODER_COLLECTION_MOD, ONLY: CACHED_ENCODER_COLLECTION_T
  USE :: CACHE_UTILS_MOD,               ONLY: CACHE_OPTIONS_T
  USE :: METADATA_BASE_MOD,             ONLY: METADATA_BASE_A
  USE :: CACHED_ENCODER_MOD,            ONLY: CACHED_ENCODER_T
  USE :: ENCODING_UTILS_MOD,            ONLY: MAKE_ENCODER_COLLECTION


  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T),          INTENT(INOUT) :: THIS
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(IN)    :: PARENT
  TYPE(FORTRAN_MESSAGE_T),              INTENT(IN)    :: KEY
  TYPE(PARAMETRIZATION_T),              INTENT(IN)    :: PAR
  CLASS(METADATA_BASE_A), POINTER,      INTENT(IN)    :: METADATA
  TYPE(ENCODING_RULE_COLLECTION_T),     INTENT(IN)    :: ENCODING_RULES
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T),         INTENT(IN)    :: ENCODER_OPT
  TYPE(FILTER_OPTIONS_T),               INTENT(IN)    :: FILTER_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  TYPE(CACHED_ENCODER_T), DIMENSION(:), POINTER :: ENCODERS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_COPY_KEY_FAILURE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_ENCODERS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODERS_ALREADY_ASSOCIATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_ENCODERS=4_JPIB_K

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

  !> Node management initialization
  IF ( .NOT. ASSOCIATED(PARENT) ) THEN
    NULLIFY(THIS%PARENT)
  ELSE
    THIS%PARENT => PARENT
  ENDIF
  THIS%RIGHT  => NIL
  THIS%LEFT   => NIL
  THIS%RED    = .TRUE.
  THIS%KEY    = KEY
  THIS%IDX    = -99

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%ENCODERS_), ERRFLAG_ENCODERS_ALREADY_ASSOCIATED )

  !> Key initialization
  PP_TRYCALL(ERRFLAG_COPY_KEY_FAILURE) THIS%KEY%COPY_FROM( KEY, HOOKS )

  !> Allocate the encode
  ALLOCATE( THIS%ENCODERS_, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE_ENCODERS )

  !> Create the encoders
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CREATE_ENCODERS) MAKE_ENCODER_COLLECTION( &
&  KEY, PAR, METADATA, ENCODING_RULES, ENCODER_OPT, FILTER_OPT, THIS%ENCODERS_, HOOKS )

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
    CASE (ERRFLAG_COPY_KEY_FAILURE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to copy the key' )
    CASE (ERRFLAG_UNABLE_TO_CREATE_ENCODERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to create encoders' )
    CASE (ERRFLAG_ENCODERS_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'encoders already associated' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_ENCODERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate encoders' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=ALLOC_STATUS)
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

END FUNCTION NODE_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Allocate a new node in the map.
!>
!> This subroutine is used to allocate a new node in the map
!>
!> @param [inout] CURRENT The map node to be allocated.
!>
!> @note this function is a placehlder for a future stack custom
!>       allocator.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ALLOCATE_NODE'
PP_THREAD_SAFE FUNCTION ALLOCATE_NODE( CURRENT, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: CURRENT
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

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
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating encoders_map node: ' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating encoders_map node: '//TRIM(ADJUSTL(ERRMSG)) )
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


!>
!> @brief Deallocate a new node in the map.
!>
!> This subroutine is used to deallocate a new node in the map
!>
!> @param [inout] CURRENT The map node to be deallocated.
!> @param [out]   ERR     Error in deallocation
!>
!> @note this function is a placehlder for a future stack custom
!>       allocator.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DEALLOCATE_NODE'
PP_THREAD_SAFE FUNCTION DEALLOCATE_NODE( X, CACHE_OPT, ENCODER_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: X
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T),         INTENT(IN)    :: ENCODER_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_NODE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_VAL=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_ENCODERS=4_JPIB_K

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

    ! Free memory of the key and payload
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_KEY) X%KEY%FREE( HOOKS )

    IF ( ASSOCIATED(X%ENCODERS_) ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_VAL) X%ENCODERS_%FREE( ENCODER_OPT, HOOKS )
      DEALLOCATE( X%ENCODERS_, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE_ENCODERS )
    ENDIF

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating encoders_map_node' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE_ENCODERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating encoders' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_FREE_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error freeing key' )
    CASE (ERRFLAG_UNABLE_TO_FREE_VAL)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error freeing value' )
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


!>
!> @brief Restores Red-Black properties after insertion.
!>
!> This subroutine performs fix-up operations to restore the Red-Black properties
!> of the map structure after a node insertion.
!>
!> @param [inout] ROOT Pointer to the root node of the map structure.
!>                     The map structure might be modified during the fix-up process.
!> @param [inout] CUR  Pointer to the current node that requires fix-up operations.
!>
!> @note This subroutine assumes that the map structure is a Red-Black tree
!>       and that the node insertion has potentially violated its properties.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INSERT_FIXUP'
PP_THREAD_SAFE FUNCTION INSERT_FIXUP( ROOT, CUR, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  !> Templated use
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: CUR
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: Y
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: X

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

          PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_LEFT) ROTATE_LEFT( ROOT, X, CACHE_OPT, HOOKS )

        ENDIF

        X%PARENT%RED = .FALSE.

        X%PARENT%PARENT%RED = .TRUE.

        PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_RIGHT) ROTATE_RIGHT( ROOT, X%PARENT%PARENT, CACHE_OPT, HOOKS )

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

          PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_RIGHT) ROTATE_RIGHT( ROOT, X, CACHE_OPT, HOOKS )

        ENDIF

        X%PARENT%RED = .FALSE.

        X%PARENT%PARENT%RED = .TRUE.

        PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_LEFT) ROTATE_LEFT( ROOT, X%PARENT%PARENT, CACHE_OPT, HOOKS )

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


!>
!> @brief Performs a left rotation on a binary search tree.
!>
!> This subroutine performs a left rotation operation on a binary search tree.
!> It adjusts the structure of the tree to maintain the properties of the binary search tree.
!>
!> @param [inout] ROOT Pointer to the root node of the binary search tree.
!>                     The tree structure might be modified during the rotation process.
!> @param [inout] X_   Pointer to the node around which the left rotation is performed.
!>
!> @note This subroutine assumes that the binary search tree structure is properly initialized
!>       and that the left rotation operation will maintain the binary search tree properties.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ROTATE_LEFT'
PP_THREAD_SAFE FUNCTION ROTATE_LEFT( ROOT, X_, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(ENCODING_CACHE_NODE_T), TARGET,  INTENT(INOUT) :: X_
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  LOGICAL :: IS_ROOT
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: X
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: Y

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%LEFT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    Y%LEFT%PARENT => X

  ENDIF

  Y%PARENT => X%PARENT

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( X%PARENT, IS_ROOT, CACHE_OPT, HOOKS )
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


!>
!> @brief Performs a right rotation on a binary search tree.
!>
!> This subroutine performs a right rotation operation on a binary search tree.
!> It adjusts the structure of the tree to maintain the properties of the binary search tree.
!>
!> @param [inout] ROOT Pointer to the root node of the binary search tree.
!>                     The tree structure might be modified during the rotation process.
!> @param [inout] X_   Pointer to the node around which the right rotation is performed.
!>
!> @note This subroutine assumes that the binary search tree structure is properly initialized
!>       and that the right rotation operation will maintain the binary search tree properties.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ROTATE_RIGHT'
PP_THREAD_SAFE FUNCTION ROTATE_RIGHT( ROOT, X_, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T),POINTER, INTENT(INOUT) :: ROOT
  TYPE(ENCODING_CACHE_NODE_T), TARGET, INTENT(INOUT) :: X_
  TYPE(CACHE_OPTIONS_T),               INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  LOGICAL :: IS_ROOT_LEAF
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: X
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: Y

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%RIGHT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    Y%RIGHT%PARENT => X

  ENDIF

  Y%PARENT => X%PARENT


  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( X%PARENT, IS_ROOT_LEAF, CACHE_OPT, HOOKS )
  IF ( IS_ROOT_LEAF ) THEN

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


!>
!> @brief Checks if a node is a leaf node.
!>
!> This function checks if the specified node is a leaf node in the map structure.
!> A leaf node is a node with no children.
!>
!> @param X Pointer to the node to be checked.
!>
!> @return Logical result indicating whether the node is a leaf (TRUE) or not (FALSE).
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NODE_ISLEAF'
PP_THREAD_SAFE FUNCTION NODE_ISLEAF( X, ISLEAF, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER,  INTENT(IN)    :: X
  LOGICAL,                               INTENT(OUT)   :: ISLEAF
  TYPE(CACHE_OPTIONS_T),                 INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

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


!>
!> @brief Finds the successor node in a binary search tree.
!>
!> This function finds the successor node of the specified node in a binary search tree.
!> The successor of a node is the node with the smallest key greater than the key of the specified node.
!>
!> @param [in]  X   Pointer to the node for which the successor is to be found.
!> @param [out] ERR Output parameter indicating the error status of the operation.
!>                  A non-zero value indicates an error occurred during the successor search process.
!>
!> @return Pointer to the successor node, if found.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SUCCESSOR'
PP_THREAD_SAFE FUNCTION SUCCESSOR( X, Y, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(IN)    :: X
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(OUT)   :: Y
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: X_

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( X, IS_LEAF, CACHE_OPT, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( IS_LEAF, ERRFLAG_NODE_IS_LEAF )

  ! Search cycle.
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( X%RIGHT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    ! If the node has a right child then the successor is the
    ! minimum of the  right subtree of the node.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_MINIMUM) MINIMUM( X%RIGHT, Y, CACHE_OPT, HOOKS )

  ELSE

    ! If the node has not right child, then the successor is the
    ! nearest parent node whose left child is a parent of the
    ! node.
    Y => X%PARENT

    SearchSuccessor: DO

      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y, IS_LEAF, CACHE_OPT, HOOKS )
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


!>
!> @brief Finds the predecessor node in a binary search tree.
!>
!> This function finds the predecessor node of the specified node in a binary search tree.
!> The predecessor of a node is the node with the smallest key greater than the key of the specified node.
!>
!> @param [in]  X   Pointer to the node for which the predecessor is to be found.
!> @param [out] ERR Output parameter indicating the error status of the operation.
!>                  A non-zero value indicates an error occurred during the predecessor search process.
!>
!> @return Pointer to the predecessor node, if found.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PREDECESSOR'
PP_THREAD_SAFE FUNCTION PREDECESSOR( X, Y, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(IN)    :: X
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(OUT)   :: Y
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: X_

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( X, IS_LEAF, CACHE_OPT, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( IS_LEAF, ERRFLAG_NODE_IS_LEAF )

  ! Search cycle.
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( X%LEFT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    ! If the node has a left child then the successor is the
    ! minimum of the  left subtree of the node.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_MAXIMUM) MAXIMUM( X%LEFT, Y, CACHE_OPT, HOOKS )

  ELSE

    ! If the node has not left child, then the predecessor is the
    ! nearest  parent node whose right child is a parent of the
    ! node.
    Y => X%PARENT

    SearchPredecessor:   DO

      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y, IS_LEAF, CACHE_OPT, HOOKS )
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


!> @brief Finds the minimum key node in a binary search tree.
!>
!> This function finds the node with the minimum key value in a binary search tree
!> rooted at the specified node `X`.
!>
!> @param [in] X Pointer to the root node of the binary search tree.
!>               The minimum key node will be searched within the subtree rooted at this node.
!>
!> @return Pointer to the node with the minimum key value, if found.
!>
!> @note This function assumes that the binary search tree structure is properly initialized
!>       and that the minimum key node can be found within the subtree rooted at the specified node `X`.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MINIMUM'
PP_THREAD_SAFE FUNCTION MINIMUM( X, Y, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  !> Templated use
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(IN)    :: X
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(OUT)   :: Y
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

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

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%LEFT, IS_LEAF, CACHE_OPT, HOOKS )
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


!> @brief Finds the maximum key node in a binary search tree.
!>
!> This function finds the node with the maximum key value in a binary search tree
!> rooted at the specified node `X`.
!>
!> @param [in] X Pointer to the root node of the binary search tree.
!>               The maximum key node will be searched within the subtree rooted at this node.
!>
!> @return Pointer to the node with the maximum key value, if found.
!>
!> @note This function assumes that the binary search tree structure is properly initialized
!>       and that the maximum key node can be found within the subtree rooted at the specified node `X`.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAXIMUM'
PP_THREAD_SAFE FUNCTION MAXIMUM( X, Y, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  !> Templated use
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(IN)    :: X
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(OUT)   :: Y
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

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

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%RIGHT, IS_LEAF, CACHE_OPT, HOOKS )
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


!>
!> @brief Swaps the data of two map nodes.
!>
!> This subroutine swaps the data (key and value) of two map nodes.
!> It exchanges the content of the nodes without modifying the tree structure.
!>
!> @param NODE_1 Pointer to the first map node whose data is to be swapped.
!> @param NODE_2 Pointer to the second map node whose data is to be swapped.
!>
!> @note This subroutine assumes that both `NODE_1` and `NODE_2` are valid pointers to map nodes.
!>       It is the responsibility of the caller to ensure proper initialization of the nodes.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SWAP_DATA'
PP_THREAD_SAFE FUNCTION SWAP_DATA( NODE_1, NODE_2, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,             ONLY: JPIB_K
  USE :: HOOKS_MOD,                     ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD,           ONLY: FORTRAN_MESSAGE_T
  USE :: CACHED_ENCODER_COLLECTION_MOD, ONLY: CACHED_ENCODER_COLLECTION_T
  USE :: CACHE_UTILS_MOD,               ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: NODE_1
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: NODE_2
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(CACHED_ENCODER_COLLECTION_T), POINTER :: TMP

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SWAP_KEYS=1_JPIB_K

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

  ! Swapping keys between two nodes
  PP_TRYCALL(ERRFLAG_SWAP_KEYS) NODE_1%KEY%SWAP_DATA( NODE_2%KEY, CACHE_OPT, HOOKS )

  ! Swapping values between two nodes
  TMP => NODE_1%ENCODERS_
  NODE_1%ENCODERS_ => NODE_2%ENCODERS_
  NODE_2%ENCODERS_ => TMP
  TMP => NULL()

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
    CASE (ERRFLAG_SWAP_KEYS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to swap keys' )
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

END FUNCTION SWAP_DATA
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Removes a node from a binary search tree.
!>
!> This subroutine removes a specified node from a binary search tree.
!> It adjusts the tree structure accordingly to maintain the properties of a binary search tree.
!>
!> @param [inout] ROOT Pointer to the root node of the binary search tree.
!>                     The tree structure might be modified during the removal process.
!> @param [inout] Z    Pointer to the node to be removed from the binary search tree.
!> @param [out]   ERR  Output parameter indicating the error status of the operation.
!>                     A non-zero value indicates an error occurred during the removal process.
!>
!> @note This subroutine assumes that the binary search tree structure is properly initialized
!>       and that the node to be removed is present in the tree.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REMOVE_NODE'
PP_THREAD_SAFE FUNCTION REMOVE_NODE( ROOT, Z, CACHE_OPT, ENCODER_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: Z
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T),         INTENT(IN)    :: ENCODER_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  LOGICAL :: IS_LEAF_LEFT
  LOGICAL :: IS_LEAF_RIGHT
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: Y
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: X

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

  ! Remove the node
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%LEFT,  IS_LEAF_LEFT,  CACHE_OPT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%RIGHT, IS_LEAF_RIGHT, CACHE_OPT, HOOKS )
  IF ( IS_LEAF_LEFT .OR. IS_LEAF_RIGHT ) THEN

    Y => Z

  ELSE

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_SUCESSOR) SUCCESSOR( Z, Y, CACHE_OPT, HOOKS )

  ENDIF

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%LEFT,  IS_LEAF, CACHE_OPT, HOOKS )
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

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( X,  IS_LEAF, CACHE_OPT, HOOKS )
    IF ( .NOT.IS_LEAF ) THEN

      X%PARENT => Y%PARENT

    ENDIF

  ENDIF

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( Y%PARENT,  IS_LEAF, CACHE_OPT, HOOKS )
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
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_SWAP_DATA) SWAP_DATA( Y, Z, CACHE_OPT, HOOKS )

  ENDIF

  ! Adjust colors of the map.
  IF ( RED_BLACK_BALANCING ) THEN

    IF ( .NOT.Y%RED ) THEN

      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_REMOVE_FIXUP) REMOVE_NODE_FIXUP( ROOT, X, CACHE_OPT, HOOKS )

    ENDIF

  ENDIF

  ! Free memory.
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_DEALLOCATE_NODE) DEALLOCATE_NODE( Y, CACHE_OPT, ENCODER_OPT, HOOKS )
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


!>
!> @brief Restores Red-Black properties after node removal.
!>
!> This subroutine performs fix-up operations to restore the Red-Black properties
!> of the map structure after a node removal.
!>
!> @param [inout] ROOT Pointer to the root node of the map structure.
!>                     The map structure might be modified during the fix-up process.
!> @param [inout] X    Pointer to the current node that requires fix-up operations.
!> @param [out]   ERR  Output parameter indicating the error status of the operation.
!>                     A non-zero value indicates an error occurred during the fix-up process.
!>
!> @note This subroutine assumes that the map structure is a Red-Black tree
!>       and that the node removal has potentially violated its properties.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REMOVE_NODE_FIXUP'
PP_THREAD_SAFE FUNCTION REMOVE_NODE_FIXUP( ROOT, X, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(INOUT) :: X
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: W

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

        PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_LEFT) ROTATE_LEFT(ROOT, X%PARENT, CACHE_OPT, HOOKS )

        W => X%PARENT%RIGHT

      ENDIF

      ! Check if current node is a leaf
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( W, IS_LEAF, CACHE_OPT, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( IS_LEAF, ERRFLAG_NODE_IS_LEAF )

      IF ( ( .NOT.W%LEFT%RED ) .AND. ( .NOT.W%RIGHT%RED ) ) THEN
        W%RED = .TRUE.

        X => X%PARENT

      ELSE

        IF ( .NOT.W%RIGHT%RED ) THEN ! CASO 3

          W%LEFT%RED = .FALSE.

          W%RED = .TRUE.

          PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_RIGHT) ROTATE_RIGHT(ROOT, W, CACHE_OPT, HOOKS )

          W => X%PARENT%RIGHT

        ENDIF

        W%RED = X%PARENT%RED

        X%PARENT%RED = .FALSE.

        W%RIGHT%RED = .FALSE.

        PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_LEFT)  ROTATE_LEFT(ROOT, X%PARENT, CACHE_OPT, HOOKS )

        X => ROOT

      ENDIF

    ELSE ! Right child

      W => X%PARENT%LEFT

      IF ( W%RED ) THEN

        W%RED = .FALSE.

        X%PARENT%RED = .TRUE.

        PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_RIGHT)  ROTATE_RIGHT(ROOT, X%PARENT, CACHE_OPT, HOOKS )

        W => X%PARENT%LEFT

      ENDIF

      ! Check if current node is a leaf
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( W, IS_LEAF, CACHE_OPT, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( IS_LEAF, ERRFLAG_NODE_IS_LEAF )

      IF ( .NOT.W%RIGHT%RED .AND. .NOT.W%LEFT%RED ) THEN

        W%RED = .TRUE.

        X => X%PARENT

      ELSE

        IF ( .NOT.W%LEFT%RED ) THEN

          W%RIGHT%RED = .FALSE.

          W%RED = .TRUE.

          PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_LEFT)  ROTATE_LEFT( ROOT, W, CACHE_OPT, HOOKS )

          W => X%PARENT%LEFT

        ENDIF

        W%RED = X%PARENT%RED

        X%PARENT%RED = .FALSE.

        W%LEFT%RED = .FALSE.

        PP_TRYCALL(ERRFLAG_UNABLE_TO_ROTATE_RIGHT)  ROTATE_RIGHT( ROOT, X%PARENT, CACHE_OPT, HOOKS )

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







!> @brief Prints all keys in the subtree pointed to by the current node.
!>
!> This subroutine recursively prints all keys in the subtree pointed to by the current node.
!>
!> @param [in] ROOT    Pointer to the root node of the subtree.
!> @param [in] CURRENT Pointer to the current node in the subtree.
!> @param [in] UNIT    Unit number of the output file where keys will be printed.
!>
!> @note This subroutine assumes that the subtree pointed to by the current node is properly initialized.
!>
#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'LIST_NODE'
RECURSIVE FUNCTION LIST_NODE( ROOT, CURRENT, CNT, UNIT, PREFIX, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(IN)    :: ROOT
  TYPE(ENCODING_CACHE_NODE_T), POINTER, INTENT(IN)    :: CURRENT
  INTEGER(KIND=JPIB_K),                 INTENT(INOUT) :: CNT
  INTEGER(KIND=JPIB_K),                 INTENT(IN)    :: UNIT
  CHARACTER(LEN=*),                     INTENT(IN)    :: PREFIX
  TYPE(CACHE_OPTIONS_T),                INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( CURRENT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_LEFT_SUBTREE) LIST_NODE( ROOT, CURRENT%LEFT, CNT, UNIT, PREFIX, CACHE_OPT, HOOKS )

    CNT = CNT + 1
    WRITE(CKEY,*,IOSTAT=WRITE_STATUS) CURRENT%KEY
    WRITE(CCNT,*,IOSTAT=WRITE_STATUS) CNT
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_ERROR )

    WRITE(UNIT,'(A,A,A,A,A)',IOSTAT=WRITE_STATUS) TRIM(ADJUSTL(PREFIX)), '(', TRIM(ADJUSTL(CCNT)), ') = ', TRIM(ADJUSTL(CKEY))
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_ERROR )


    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_RIGHT_SUBTREE) LIST_NODE( ROOT, CURRENT%RIGHT, CNT, UNIT, PREFIX, CACHE_OPT, HOOKS )

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

END FUNCTION LIST_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Prints all keys in the subtree pointed to by the current node.
!>
!> This subroutine recursively prints all keys in the subtree pointed to by the current node.
!>
!> @param [in] ROOT     Pointer to the root node of the subtree.
!> @param [in] CURRENT  Pointer to the current node in the subtree.
!> @param [in] FUNCTION Function to be called for each node
!>
!> @note This subroutine assumes that the subtree pointed to by the current node is properly initialized.
!>
#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'FUNCTION_NODE'
RECURSIVE FUNCTION FUNCTION_NODE( ROOT, CURRENT, PFUNCTION, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER,           INTENT(IN)    :: ROOT
  TYPE(ENCODING_CACHE_NODE_T), POINTER,           INTENT(IN)    :: CURRENT
  PROCEDURE(FUNCTION_ENCODING_CACHE_IF), POINTER, INTENT(IN)    :: PFUNCTION
  TYPE(CACHE_OPTIONS_T),                          INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                                  INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=128) :: CKEY
  LOGICAL :: IS_LEAF
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_LEFT_SUBTREE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_RIGHT_SUBTREE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_MAP_FUNCTION_ERROR=4_JPIB_K

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( CURRENT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_LEFT_SUBTREE) FUNCTION_NODE( ROOT, CURRENT%LEFT, PFUNCTION, CACHE_OPT, HOOKS )
    PP_TRYCALL(ERRFLAG_CALL_MAP_FUNCTION_ERROR) PFUNCTION( CURRENT%KEY, CACHE_OPT, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_RIGHT_SUBTREE) FUNCTION_NODE( ROOT, CURRENT%RIGHT, PFUNCTION, CACHE_OPT, HOOKS )

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call the map function' )
    CASE (ERRFLAG_CALL_MAP_FUNCTION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling function' )
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

END FUNCTION FUNCTION_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Prints all keys in the subtree pointed to by the current node.
!>
!> This subroutine recursively prints all keys in the subtree pointed to by the current node.
!>
!> @param [in] ROOT     Pointer to the root node of the subtree.
!> @param [in] CURRENT  Pointer to the current node in the subtree.
!> @param [in] FUNCTION Function to be called for each node
!>
!> @note This subroutine assumes that the subtree pointed to by the current node is properly initialized.
!>
#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'FUNCTOR_NODE'
RECURSIVE FUNCTION FUNCTOR_NODE( ROOT, CURRENT, FUNCTOR, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T),     POINTER, INTENT(IN)    :: ROOT
  TYPE(ENCODING_CACHE_NODE_T),     POINTER, INTENT(IN)    :: CURRENT
  CLASS(FUNCTOR_ENCODING_CACHE_A), POINTER, INTENT(IN)    :: FUNCTOR
  TYPE(CACHE_OPTIONS_T),                    INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=128) :: CKEY
  LOGICAL :: IS_LEAF
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_LEFT_SUBTREE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_RIGHT_SUBTREE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_MAP_FUNCTOR_ERROR=4_JPIB_K

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( CURRENT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_LEFT_SUBTREE) FUNCTOR_NODE( ROOT, CURRENT%LEFT, FUNCTOR, CACHE_OPT, HOOKS )
    PP_TRYCALL(ERRFLAG_CALL_MAP_FUNCTOR_ERROR) FUNCTOR%APPLY( CURRENT%KEY, CACHE_OPT, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_RIGHT_SUBTREE) FUNCTOR_NODE( ROOT, CURRENT%RIGHT, FUNCTOR, CACHE_OPT, HOOKS )

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call the map function' )
    CASE (ERRFLAG_CALL_MAP_FUNCTOR_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'erro calling the functo' )
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

END FUNCTION FUNCTOR_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE






!> @brief Prints all keys in the subtree pointed to by the current node.
!>
!> This subroutine recursively prints all keys in the subtree pointed to by the current node.
!>
!> @param [in] ROOT     Pointer to the root node of the subtree.
!> @param [in] CURRENT  Pointer to the current node in the subtree.
!> @param [in] FUNCTION Function to be called for each node
!>
!> @note This subroutine assumes that the subtree pointed to by the current node is properly initialized.
!>
#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_NODE'
RECURSIVE FUNCTION DUMP_NODE( ROOT, CURRENT, DUMP_PATH, CNT, CACHE_OPT, ENCODER_OPTIONS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T),     POINTER, INTENT(IN)    :: ROOT
  TYPE(ENCODING_CACHE_NODE_T),     POINTER, INTENT(IN)    :: CURRENT
  CHARACTER(LEN=*),                         INTENT(IN)    :: DUMP_PATH
  INTEGER(KIND=JPIB_K),                     INTENT(INOUT) :: CNT
  TYPE(CACHE_OPTIONS_T),                    INTENT(IN)    :: CACHE_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T),             INTENT(IN)    :: ENCODER_OPTIONS
  TYPE(HOOKS_T),                            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=128) :: CKEY
  LOGICAL :: IS_LEAF
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_LEFT_SUBTREE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_RIGHT_SUBTREE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_MAP_FUNCTOR_ERROR=4_JPIB_K

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( CURRENT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_LEFT_SUBTREE) DUMP_NODE( ROOT, CURRENT%LEFT, DUMP_PATH, CNT, CACHE_OPT, ENCODER_OPTIONS, HOOKS )
    CNT = CNT + 1
    PP_TRYCALL(ERRFLAG_CALL_MAP_FUNCTOR_ERROR) CURRENT%ENCODERS_%DUMP( DUMP_PATH, CNT, ENCODER_OPTIONS, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_RIGHT_SUBTREE) DUMP_NODE( ROOT, CURRENT%RIGHT, DUMP_PATH, CNT, CACHE_OPT, ENCODER_OPTIONS, HOOKS )

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call the map function' )
    CASE (ERRFLAG_CALL_MAP_FUNCTOR_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'erro calling the functo' )
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

END FUNCTION DUMP_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!> @brief Prints all keys in the subtree pointed to by the current node.
!>
!> This subroutine recursively prints all keys in the subtree pointed to by the current node.
!>
!> @param [in] ROOT     Pointer to the root node of the subtree.
!> @param [in] CURRENT  Pointer to the current node in the subtree.
!> @param [in] FUNCTION Function to be called for each node
!>
!> @note This subroutine assumes that the subtree pointed to by the current node is properly initialized.
!>
#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'MEMORY_SIZE_NODE'
RECURSIVE FUNCTION MEMORY_SIZE_NODE( ROOT, CURRENT, MEMORY_BYTESIZE, CACHE_OPT, ENCODER_OPTIONS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T),  POINTER, INTENT(IN)    :: ROOT
  TYPE(ENCODING_CACHE_NODE_T),  POINTER, INTENT(IN)    :: CURRENT
  INTEGER(KIND=JPIB_K),                  INTENT(INOUT) :: MEMORY_BYTESIZE
  TYPE(CACHE_OPTIONS_T),                 INTENT(IN)    :: CACHE_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T),          INTENT(IN)    :: ENCODER_OPTIONS
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=128) :: CKEY
  LOGICAL :: IS_LEAF
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS
  INTEGER(KIND=JPIB_K) :: TMP_MEMORY_BYTESIZE

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_LEFT_SUBTREE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_RIGHT_SUBTREE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_MAP_FUNCTOR_ERROR=4_JPIB_K

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( CURRENT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_LEFT_SUBTREE) MEMORY_SIZE_NODE( ROOT, CURRENT%LEFT, MEMORY_BYTESIZE, CACHE_OPT, ENCODER_OPTIONS, HOOKS )
    PP_TRYCALL(ERRFLAG_CALL_MAP_FUNCTOR_ERROR) CURRENT%ENCODERS_%BYTESIZE( TMP_MEMORY_BYTESIZE, ENCODER_OPTIONS, HOOKS )
    MEMORY_BYTESIZE = MEMORY_BYTESIZE + TMP_MEMORY_BYTESIZE
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_RIGHT_SUBTREE) MEMORY_SIZE_NODE( ROOT, CURRENT%RIGHT, MEMORY_BYTESIZE, CACHE_OPT, ENCODER_OPTIONS, HOOKS )

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call the map function' )
    CASE (ERRFLAG_CALL_MAP_FUNCTOR_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'erro calling the functo' )
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

END FUNCTION MEMORY_SIZE_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Renumber nodes in a Red Black tree.
!>
!> This subroutine recursively renumbers nodes in a Red Black tree rooted at the specified node.
!>
!> @param [inout] ROOT The root node of the Red Black tree.
!> @param [inout] IDX The index used for renumbering nodes.
!>
!> @note This subroutine assumes that the Red Black tree structure is properly initialized.
!>
#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'RENUMBER_NODE'
RECURSIVE FUNCTION RENUMBER_NODE( ROOT, IDX, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T),  INTENT(INOUT) :: ROOT
  INTEGER(KIND=JPIB_K),         INTENT(INOUT) :: IDX
  TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT%LEFT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT. IS_LEAF ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_RENUMBER_NODE) RENUMBER_NODE( ROOT%LEFT, IDX, CACHE_OPT, HOOKS )
  ENDIF

  ! Renumber right subtree
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT%RIGHT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT. IS_LEAF ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_RENUMBER_NODE) RENUMBER_NODE( ROOT%RIGHT, IDX, CACHE_OPT, HOOKS )
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

END FUNCTION RENUMBER_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Renumber writes node connectivity in a Red Black tree.
!>
!> This subroutine recursively writes node connectivity in a Red Black tree rooted at the specified node.
!>
!> @param [inout] ROOT The root node of the Red Black tree.
!> @param [inout] UNIT Unit number of the output file where keys will be printed.
!>
!> @note This subroutine assumes that the Red Black tree structure is properly initialized.
!> @note Connectivity is written in the dot format to be parsed with graphviz
!>
#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'NODE_WRITE_CONNECTIVITY'
RECURSIVE FUNCTION NODE_WRITE_CONNECTIVITY( ROOT, UNIT, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T),  INTENT(INOUT) :: ROOT
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: UNIT
  TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT%LEFT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_WRITE_CONNECTIVITY) NODE_WRITE_CONNECTIVITY( ROOT%LEFT, UNIT, CACHE_OPT, HOOKS )
  ENDIF

  ! Write connectivity of the right subtree
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT%RIGHT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT.IS_LEAF ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_WRITE_CONNECTIVITY) NODE_WRITE_CONNECTIVITY( ROOT%RIGHT, UNIT, CACHE_OPT, HOOKS )
  ENDIF

  ! Write connectivity of the current node
  IF ( ASSOCIATED(ROOT%PARENT) ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT%PARENT, IS_LEAF, CACHE_OPT, HOOKS )
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

END FUNCTION NODE_WRITE_CONNECTIVITY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Renumber writes nodes in a Red Black tree.
!>
!> This subroutine recursively writes nodes in a Red Black tree rooted at the specified node.
!>
!> @param [inout] ROOT The root node of the Red Black tree.
!> @param [inout] UNIT Unit number of the output file where keys will be printed.
!>
!> @note This subroutine assumes that the Red Black tree structure is properly initialized.
!> @note Nodes are written in the dot format to be parsed with graphviz
!>
#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'WRITE_NODE'
RECURSIVE FUNCTION WRITE_NODE( ROOT, UNIT, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_NODE_T), INTENT(INOUT) :: ROOT
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: UNIT
  TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_LEAF
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS
  CHARACTER(LEN=128) :: CKEY

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_WRITE_NODE=2_JPIB_K
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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT%LEFT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT. ASSOCIATED( ROOT%LEFT, NIL ) ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_WRITE_NODE) WRITE_NODE( ROOT%LEFT, UNIT, CACHE_OPT, HOOKS )
  ENDIF

  ! Write nodes in the right subtree
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( ROOT%RIGHT, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT. ASSOCIATED( ROOT%RIGHT, NIL ) ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_WRITE_NODE) WRITE_NODE( ROOT%RIGHT, UNIT, CACHE_OPT, HOOKS )
  ENDIF

  ! Write the current key
  CKEY = REPEAT( ' ', 128 )
  WRITE(CKEY,*,IOSTAT=WRITE_STATUS) ROOT%KEY
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
    CASE (ERRFLAG_UNABLE_TO_CALL_WRITE_NODE)
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

END FUNCTION WRITE_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'RECURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'GET_SORTED_KEYS_INT_NODE'
RECURSIVE FUNCTION GET_SORTED_KEYS_NODE( NODE, SORTED_KEYS, CNT, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(ENCODING_CACHE_NODE_T), POINTER,  INTENT(INOUT) :: NODE
  TYPE(FORTRAN_MESSAGE_T), DIMENSION(:), INTENT(INOUT) :: SORTED_KEYS
  INTEGER(KIND=JPIB_K),                  INTENT(INOUT) :: CNT
  TYPE(CACHE_OPTIONS_T),                 INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: Y
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: PREV
  LOGICAL :: IS_LEAF

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_GET_SORTED_KEYS_NODE=2_JPIB_K
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

  ! Remove the map if it is not empty.
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( NODE, IS_LEAF, CACHE_OPT, HOOKS )
  IF ( .NOT. IS_LEAF ) THEN

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_GET_SORTED_KEYS_NODE) GET_SORTED_KEYS_NODE( NODE%LEFT,  SORTED_KEYS, CNT, CACHE_OPT, HOOKS )

    CNT = CNT + 1
    PP_DEBUG_CRITICAL_COND_THROW( CNT.GT.SIZE(SORTED_KEYS), ERRFLAG_OUT_OF_BOUNDS )
    SORTED_KEYS(CNT) = NODE%KEY

    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_GET_SORTED_KEYS_NODE) GET_SORTED_KEYS_NODE( NODE%RIGHT, SORTED_KEYS, CNT, CACHE_OPT, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_CALL_GET_SORTED_KEYS_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map get sorted keys node' )
    CASE (ERRFLAG_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'out of bounds' )
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

END FUNCTION GET_SORTED_KEYS_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




!>
!> @brief Initializes a map structure.
!>
!> This subroutine initializes the given map structure.
!> It sets up the necessary components to prepare the map for use.
!>
!> @param [inout] ENCODERS_MAP The map structure to be initialized.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_INIT'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_INIT( ENCODERS_MAP, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T),      INTENT(INOUT) :: ENCODERS_MAP
  TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

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
  ENCODERS_MAP%ROOT => NIL
  ENCODERS_MAP%SIZE_ = 0

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION ENCODING_CACHE_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




!>
!> @brief Initializes a map structure.
!>
!> This subroutine initializes the given map structure.
!> It sets up the necessary components to prepare the map for use.
!>
!> @param [inout] ENCODERS_MAP The map structure to be initialized.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_SIZE'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_SIZE( ENCODERS_MAP, SZ, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T),      INTENT(INOUT) :: ENCODERS_MAP
  INTEGER(KIND=JPIB_K),         INTENT(OUT)   :: SZ
  TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

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
  SZ = ENCODERS_MAP%SIZE_

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION ENCODING_CACHE_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Finalisation of a map structure.
!>
!> This subroutine finalises the given map structure.
!>
!> @param [inout] ENCODERS_MAP The map structure to be finalised.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_FREE'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_FREE( ENCODERS_MAP, CACHE_OPT, ENCODER_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T),      INTENT(INOUT) :: ENCODERS_MAP
  TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: CACHE_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: ENCODER_OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_FREE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_INIT=2_JPIB_K

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
  IF ( ENCODERS_MAP%SIZE_ .GT. 0 ) THEN

    ! Recursive deletion of the tree.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_FREE) FREE_NODE( ENCODERS_MAP%ROOT, CACHE_OPT, ENCODER_OPT, HOOKS )

  ENDIF

  ! Reset the initial condition.
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_INIT) ENCODERS_MAP%INIT( CACHE_OPT, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_CALL_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map free' )
    CASE (ERRFLAG_UNABLE_TO_CALL_INIT)
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

END FUNCTION ENCODING_CACHE_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Get the minimum value in the map.
!>
!> This subroutine finalises the given map structure.
!>
!> @param [inout] ENCODERS_MAP The map structure to be finalised.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_MINIMUM'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_MINIMUM( ENCODERS_MAP, KEY, &
&  CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,   ONLY: CACHE_OPTIONS_T

  !> Templated use
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T), INTENT(INOUT) :: ENCODERS_MAP
  TYPE(FORTRAN_MESSAGE_T), INTENT(OUT)   :: KEY
  TYPE(CACHE_OPTIONS_T),   INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: Y

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_MINIMUM=1_JPIB_K

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
  IF ( ENCODERS_MAP%SIZE_ .GT. 0 ) THEN

    ! Recursive deletion of the tree.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_MINIMUM) MINIMUM( ENCODERS_MAP%ROOT, Y, &
&                 CACHE_OPT, HOOKS )

    KEY = Y%KEY

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

END FUNCTION ENCODING_CACHE_MINIMUM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Get the maximum value of the map.
!>
!> This subroutine finalises the given map structure.
!>
!> @param [inout] ENCODERS_MAP The map structure to be finalised.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_MAXIMUM'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_MAXIMUM( ENCODERS_MAP, KEY, &
&          CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  !> Templated use
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T),      INTENT(INOUT) :: ENCODERS_MAP
  TYPE(FORTRAN_MESSAGE_T),      INTENT(OUT)   :: KEY
  TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: Y

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_MINIMUM=1_JPIB_K

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
  IF ( ENCODERS_MAP%SIZE_ .GT. 0 ) THEN

    ! Recursive deletion of the tree.
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_MINIMUM) MAXIMUM( ENCODERS_MAP%ROOT, Y, CACHE_OPT, HOOKS )

    KEY = Y%KEY

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
    CASE (ERRFLAG_UNABLE_TO_CALL_MINIMUM)
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

END FUNCTION ENCODING_CACHE_MAXIMUM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Get the maximum value of the map.
!>
!> This subroutine finalises the given map structure.
!>
!> @param [inout] ENCODERS_MAP The map structure to be finalised.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_GET_SORTED_KEYS'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_GET_SORTED_KEYS( ENCODERS_MAP, SORTED_KEYS, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS
IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T),               INTENT(INOUT) :: ENCODERS_MAP
  TYPE(FORTRAN_MESSAGE_T), DIMENSION(:), INTENT(INOUT) :: SORTED_KEYS
  TYPE(CACHE_OPTIONS_T),                 INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: CNT

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_SIZE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_GET_SORTED_KEYS_NODE=3_JPIB_K

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

  ! Error handing
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(ENCODERS_MAP%ROOT), ERRFLAG_NOT_INITIALIZED )
  PP_DEBUG_CRITICAL_COND_THROW( ENCODERS_MAP%SIZE_ .NE. SIZE(SORTED_KEYS), ERRFLAG_WRONG_SIZE )

  ! Remove the map if it is not empty.
  IF ( ENCODERS_MAP%SIZE_ .GT. 0 ) THEN

    ! Recursive deletion of the tree.
    CNT = 0
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_GET_SORTED_KEYS_NODE) GET_SORTED_KEYS_NODE( ENCODERS_MAP%ROOT, SORTED_KEYS, CNT, CACHE_OPT, HOOKS )
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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'map not initialized' )
    CASE (ERRFLAG_WRONG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong size' )
    CASE (ERRFLAG_UNABLE_TO_CALL_GET_SORTED_KEYS_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map get sorted keys node' )
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

END FUNCTION ENCODING_CACHE_GET_SORTED_KEYS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Inserts a key-value pair into the map.
!>
!> This subroutine inserts a key-value pair into the map structure.
!>
!> @param [inout] THIS  The map structure where the key-value pair will be inserted.
!> @param [in]    KEY   The key to be inserted into the map.
!>
!> @note This subroutine assumes that the map structure is properly initialized.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_ACCESS_OR_CREATE'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_ACCESS_OR_CREATE( THIS, KEY, PAR, METADATA, ENCODING_RULES, &
&                   ENCODERS, CACHE_OPT, ENCODER_OPT, FILTER_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,             ONLY: JPIB_K
  USE :: HOOKS_MOD,                     ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD,      ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,           ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,           ONLY: PARAMETRIZATION_T
  USE :: ENCODING_RULE_COLLECTION_MOD,  ONLY: ENCODING_RULE_COLLECTION_T
  USE :: CACHED_ENCODER_COLLECTION_MOD, ONLY: CACHED_ENCODER_COLLECTION_T
  USE :: METADATA_BASE_MOD,             ONLY: METADATA_BASE_A
  USE :: FORTRAN_MESSAGE_MOD,           ONLY: FORTRAN_MESSAGE_T
  USE :: CACHE_UTILS_MOD,               ONLY: CACHE_OPTIONS_T
  USE :: FILTER_OPTIONS_MOD,            ONLY: FILTER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T),                    INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),                    INTENT(IN)    :: KEY
  TYPE(PARAMETRIZATION_T),                    INTENT(IN)    :: PAR
  CLASS(METADATA_BASE_A), POINTER,            INTENT(IN)    :: METADATA
  TYPE(ENCODING_RULE_COLLECTION_T),           INTENT(IN)    :: ENCODING_RULES
  TYPE(CACHED_ENCODER_COLLECTION_T), POINTER, INTENT(OUT)   :: ENCODERS
  TYPE(CACHE_OPTIONS_T),                      INTENT(IN)    :: CACHE_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T),               INTENT(IN)    :: ENCODER_OPT
  TYPE(FILTER_OPTIONS_T),                     INTENT(IN)    :: FILTER_OPT
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: INSERTED

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_INSERT_NODE=1_JPIB_K

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

  ! Call the routine to insert a node in the map
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_INSERT_NODE) ACCESS_OR_CREATE_NODE( THIS%ROOT, KEY, PAR, &
&  METADATA, ENCODING_RULES, ENCODERS, INSERTED, CACHE_OPT, ENCODER_OPT, FILTER_OPT, HOOKS )

  ! Update the number of elements in the map
  IF ( INSERTED ) THEN
    THIS%SIZE_ = THIS%SIZE_ + 1
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
    CASE (ERRFLAG_UNABLE_TO_CALL_INSERT_NODE)
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

END FUNCTION ENCODING_CACHE_ACCESS_OR_CREATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Search a key into the map.
!>
!> This subroutine searches a key into the map structure.
!>
!> @param [inout] THIS  The map structure where the key-value pair will be inserted.
!> @param [in]    KEY   The key to be inserted into the map.
!>
!> @result True if the key has been found
!>
!> @note This subroutine assumes that the map structure is properly initialized.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_MATCH'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_MATCH( THIS, KEY, CACHE_OPT, LMATCH, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T),      INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: KEY
  TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: CACHE_OPT
  LOGICAL,                      INTENT(OUT)   :: LMATCH
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: ROOT_IS_LEAF
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: SEARCHED_NODE

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

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( THIS%ROOT, ROOT_IS_LEAF, CACHE_OPT, HOOKS )
  IF ( ROOT_IS_LEAF ) THEN

    LMATCH = .FALSE.

  ELSE

    ! Search the node in the map
    SEARCHED_NODE => NIL
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_SEARCH) SEARCH_NODE( THIS%ROOT, SEARCHED_NODE, KEY, CACHE_OPT, LMATCH, HOOKS )
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

END FUNCTION ENCODING_CACHE_MATCH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Removes a key-value pair into the map.
!>
!> This subroutine removes a key-value pair into the map structure.
!>
!> @param [inout] THIS  The map structure from where the key-value pair will be removed.
!> @param [in]    KEY   The key to be removed into the map.
!>
!> @note This subroutine assumes that the map structure is properly initialized.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_REMOVE'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_REMOVE( THIS, KEY, CACHE_OPT, ENCODER_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T),      INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: KEY
  TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: CACHE_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: ENCODER_OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(ENCODING_CACHE_NODE_T), POINTER :: SEARCHED_NODE
  LOGICAL :: ROOT_IS_LEAF
  LOGICAL :: LMATCH

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_REMOVE_NODE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAP_IS_EMPTY=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_SEARCH=4_JPIB_K

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

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( THIS%ROOT, ROOT_IS_LEAF, CACHE_OPT, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( ROOT_IS_LEAF, ERRFLAG_MAP_IS_EMPTY )

  ! Search the node in the map
  SEARCHED_NODE => NIL
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_SEARCH) SEARCH_NODE( THIS%ROOT, SEARCHED_NODE, KEY, CACHE_OPT, LMATCH, HOOKS )

  !> If node is found then removve it
  IF ( LMATCH ) THEN
    !> Remove the node from the map
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_REMOVE_NODE) REMOVE_NODE( THIS%ROOT, SEARCHED_NODE, CACHE_OPT, ENCODER_OPT, HOOKS )
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
    CASE (ERRFLAG_MAP_IS_EMPTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'map is empty' )
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_UNABLE_TO_CALL_REMOVE_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map remove node' )
    CASE (ERRFLAG_UNABLE_TO_CALL_SEARCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map search' )
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

END FUNCTION ENCODING_CACHE_REMOVE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Lists all keys in the map.
!>
!> This subroutine lists all keys stored in the map structure.
!>
!> @param [inout] THIS The map structure to list the key-value pairs from.
!> @param [in]    UNIT Unit number of the output file where keys will be printed.
!>
!> @note This subroutine assumes that the map structure is properly initialized.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_LIST'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_LIST( THIS, UNIT, PREFIX, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T),      INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: UNIT
  CHARACTER(LEN=*),             INTENT(IN)    :: PREFIX
  TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: CNT

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_LIST_NODE=1_JPIB_K

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_LIST_NODE) LIST_NODE( THIS%ROOT, THIS%ROOT, CNT, UNIT, PREFIX, CACHE_OPT, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_CALL_LIST_NODE)
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

END FUNCTION ENCODING_CACHE_LIST
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Lists all keys in the map.
!>
!> This subroutine apply a function to all nodes in the encoders_map.
!>
!> @param [inout] THIS     The map structure to list the key-value pairs from.
!> @param [in]    FUNCTION Pointer tot he function to be applied to all nodes
!>
!> @note This subroutine assumes that the map structure is properly initialized.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_APPLY_FUNCTION'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_APPLY_FUNCTION( THIS, FUNCTION, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T),                        INTENT(INOUT) :: THIS
  PROCEDURE(FUNCTION_ENCODING_CACHE_IF), POINTER, INTENT(IN)    :: FUNCTION
  TYPE(CACHE_OPTIONS_T),                          INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                                  INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_FUNCTION_NODE=1_JPIB_K

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

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_FUNCTION_NODE) FUNCTION_NODE( THIS%ROOT, &
&                          THIS%ROOT, FUNCTION, CACHE_OPT, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_CALL_FUNCTION_NODE)
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

END FUNCTION ENCODING_CACHE_APPLY_FUNCTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Lists all keys in the map.
!>
!> This subroutine apply a function to all nodes in the encoders_map.
!>
!> @param [inout] THIS     The map structure to list the key-value pairs from.
!> @param [in]    FUNCTION Pointer tot he function to be applied to all nodes
!>
!> @note This subroutine assumes that the map structure is properly initialized.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_APPLY_FUNCTOR'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_APPLY_FUNCTOR( THIS, FUNCTOR, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T),                  INTENT(INOUT) :: THIS
  CLASS(FUNCTOR_ENCODING_CACHE_A), POINTER, INTENT(IN)    :: FUNCTOR
  TYPE(CACHE_OPTIONS_T),                    INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_FUNCTOR_NODE=1_JPIB_K

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

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_FUNCTOR_NODE) FUNCTOR_NODE( THIS%ROOT, &
&                        THIS%ROOT, FUNCTOR, CACHE_OPT, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_CALL_FUNCTOR_NODE)
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

END FUNCTION ENCODING_CACHE_APPLY_FUNCTOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_DUMP'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_DUMP( THIS, DUMP_PATH, CACHE_OPT, ENCODER_OPTIONS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T),      INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: DUMP_PATH
  TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: CACHE_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: ENCODER_OPTIONS
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: CNT

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_FUNCTOR_NODE=1_JPIB_K

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

  ! Counter initialization
  CNT = 0_JPIB_K

  ! Recursive call the dump procedure
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_FUNCTOR_NODE) DUMP_NODE( THIS%ROOT, &
&     THIS%ROOT, DUMP_PATH, CNT, CACHE_OPT, ENCODER_OPTIONS, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_CALL_FUNCTOR_NODE)
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

END FUNCTION ENCODING_CACHE_DUMP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_BYTESIZE'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_BYTESIZE( THIS, MEMORY_BYTESIZE, CACHE_OPT, ENCODER_OPTIONS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T),      INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(INOUT) :: MEMORY_BYTESIZE
  TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: CACHE_OPT
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: ENCODER_OPTIONS
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_FUNCTOR_NODE=1_JPIB_K

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

  ! Set the memory bytesize to zero
  MEMORY_BYTESIZE = 0_JPIB_K

  ! Recursive call the dump procedure
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_FUNCTOR_NODE) MEMORY_SIZE_NODE( THIS%ROOT, &
&         THIS%ROOT, MEMORY_BYTESIZE, CACHE_OPT, ENCODER_OPTIONS, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_CALL_FUNCTOR_NODE)
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

END FUNCTION ENCODING_CACHE_BYTESIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE






!>
!> @briefPrint the map.
!>
!> This subroutine prints the map in dot format.
!>
!> @param [inout] THIS The map structure to list the key-value pairs from.
!> @param [in]    NAME Basename of the file where to print the graph.
!> @param [in]    IDX  Index used to generate the name.
!>
!> @note The final nema will be: `<basename>_<IDX:8.8>.dot`
!> @note This subroutine assumes that the map structure is properly initialized.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODING_CACHE_PRINT'
PP_THREAD_SAFE FUNCTION ENCODING_CACHE_PRINT( THIS, NAME, IDX, CACHE_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODING_CACHE_T),      INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: NAME
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: IDX
  TYPE(CACHE_OPTIONS_T),        INTENT(IN)    :: CACHE_OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=128)   :: FNAME
  INTEGER(KIND=JPIB_K) :: UNIT
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS
  LOGICAL              :: ROOT_IS_LEAF

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_FILE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_RENUMBER_NODE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ISLEAF=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_WRITE_NODE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_WRITE_CONNECTIVITY=5_JPIB_K

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_RENUMBER_NODE) RENUMBER_NODE( THIS%ROOT, UNIT, CACHE_OPT, HOOKS )
  UNIT=131


  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ISLEAF) NODE_ISLEAF( THIS%ROOT, ROOT_IS_LEAF, CACHE_OPT, HOOKS )
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
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_WRITE_NODE) WRITE_NODE( THIS%ROOT, UNIT, CACHE_OPT, HOOKS )
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STATUS) ''
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_FILE )
    WRITE(UNIT,'(A)',IOSTAT=WRITE_STATUS) '// Connectivity'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE_FILE )


    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_WRITE_CONNECTIVITY) NODE_WRITE_CONNECTIVITY( THIS%ROOT, UNIT, CACHE_OPT, HOOKS )
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
    CASE (ERRFLAG_UNABLE_TO_CALL_RENUMBER_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map renumber node' )
    CASE (ERRFLAG_UNABLE_TO_CALL_ISLEAF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call isleaf' )
    CASE (ERRFLAG_UNABLE_TO_CALL_WRITE_NODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call map write node' )
    CASE (ERRFLAG_UNABLE_TO_CALL_WRITE_CONNECTIVITY)
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

END FUNCTION ENCODING_CACHE_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE ENCODING_CACHE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
