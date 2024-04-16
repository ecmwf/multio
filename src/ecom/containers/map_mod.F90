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
#define PP_FILE_NAME 'map_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MAP_MOD'
MODULE MAP_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

IMPLICIT NONE

!> @brief Default visibility of the module
PRIVATE

!> @brief Flag used to enable the tree balancing
LOGICAL, PARAMETER :: RED_BLACK_BALANCING=.TRUE.

!> @brief Datatype used to store the keys
!>
!> @note for the moment I decided to use as only possible key an integer,
!>       since it is the only one needed. It is easy to implment other
!>       keys using polymorphism or preprocessor (fypp).
!>
!> @todo implement support for other keys
!>
TYPE :: KEY_T
  INTEGER :: K
END TYPE


!> Operators used to compare the keys
INTERFACE OPERATOR( == )
  MODULE PROCEDURE KEY_EQ
END INTERFACE

INTERFACE OPERATOR( /= )
  MODULE PROCEDURE KEY_NE
END INTERFACE

INTERFACE OPERATOR( > )
  MODULE PROCEDURE KEY_GT
END INTERFACE

INTERFACE OPERATOR( >= )
  MODULE PROCEDURE KEY_GE
END INTERFACE

INTERFACE OPERATOR( < )
  MODULE PROCEDURE KEY_LT
END INTERFACE

INTERFACE OPERATOR( <= )
  MODULE PROCEDURE KEY_LE
END INTERFACE


!> @brief Datatype used to to store a node of the tree
TYPE :: MAP_NODE_T

  !> Key
  TYPE(KEY_T) :: KEY

  !> Payload
  CLASS(*), POINTER :: VALUE => NULL()

  !> Color
  LOGICAL :: RED = .FALSE.

  !> Index used to dumping the graph
  INTEGER(KIND=JPIB_K) :: IDX

  !> Pointer to the parent node
  TYPE(MAP_NODE_T), POINTER :: PARENT => NULL()

  !> Pointer to the lef subtree
  TYPE(MAP_NODE_T), POINTER :: LEFT   => NULL()

  !> Pointer to the right subtree
  TYPE(MAP_NODE_T), POINTER :: RIGHT  => NULL()
END TYPE


!> @brief Datatype used to to store the entire map
TYPE :: MAP_T
  TYPE(MAP_NODE_T), POINTER :: ROOT => NULL()
  INTEGER :: SIZE = 99
END TYPE


!> @brief Node used as terminal symbol
TYPE(MAP_NODE_T), TARGET :: NIL

!> Whitelist of public symbols
PUBLIC :: MAP_T
PUBLIC :: KEY_T
PUBLIC :: MAP_INIT
PUBLIC :: MAP_FREE
PUBLIC :: MAP_GET
PUBLIC :: MAP_INSERT
PUBLIC :: MAP_REMOVE
PUBLIC :: MAP_PRINT
PUBLIC :: MAP_LIST
PUBLIC :: MAP_MINIMUM
PUBLIC :: MAP_MAXIMUM
PUBLIC :: MAP_GET_SORTED_KEYS_INT

CONTAINS


!>
!> @brief Compares two keys for equality.
!>
!> This function compares two keys for equality.
!>
!> @param [in] KEY_A Input key A of type KEY_T.
!> @param [in] KEY_B Input key B of type KEY_T.
!>
!> @return RES Result of the comparison, a logical value.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'KEY_EQ'
FUNCTION KEY_EQ( KEY_A, KEY_B ) RESULT( RES )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(KEY_T), INTENT(IN) :: KEY_A
  TYPE(KEY_T), INTENT(IN) :: KEY_B

  ! Local variables
  LOGICAL :: RES

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  RES = KEY_A%K .EQ. KEY_B%K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION KEY_EQ
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Checks if two keys are not equal.
!>
!> This function checks if two keys are not equal.
!>
!> @param [in] KEY_A Input key A of type KEY_T.
!> @param [in] KEY_B Input key B of type KEY_T.
!>
!> @return RES Result of the comparison, a logical value.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'KEY_NE'
FUNCTION KEY_NE( KEY_A, KEY_B ) RESULT( RES )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(KEY_T), INTENT(IN) :: KEY_A
  TYPE(KEY_T), INTENT(IN) :: KEY_B

  ! Local variables
  LOGICAL :: RES

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  RES = KEY_A%K .NE. KEY_B%K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION KEY_NE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Checks if key A is less than key B.
!>
!> This function checks if key A is less than key B.
!>
!> @param [in] KEY_A Input key A of type KEY_T.
!> @param [in] KEY_B Input key B of type KEY_T.
!>
!> @return RES Result of the comparison, a logical value.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'KEY_LT'
FUNCTION KEY_LT( KEY_A, KEY_B ) RESULT( RES )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(KEY_T), INTENT(IN) :: KEY_A
  TYPE(KEY_T), INTENT(IN) :: KEY_B

  ! Local variables
  LOGICAL :: RES

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  RES = KEY_A%K .LT. KEY_B%K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION KEY_LT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Checks if key A is greater than key B.
!>
!> This function checks if key A is greater than key B.
!>
!> @param [in] KEY_A Input key A of type KEY_T.
!> @param [in] KEY_B Input key B of type KEY_T.
!>
!> @return RES Result of the comparison, a logical value.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'KEY_GT'
FUNCTION KEY_GT( KEY_A, KEY_B ) RESULT( RES )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(KEY_T), INTENT(IN) :: KEY_A
  TYPE(KEY_T), INTENT(IN) :: KEY_B

  ! Local variables
  LOGICAL :: RES

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  RES = KEY_A%K .GT. KEY_B%K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION KEY_GT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Checks if key A is less than or equal to key B.
!>
!> This function checks if key A is less than or equal to key B.
!>
!> @param [in] KEY_A Input key A of type KEY_T.
!> @param [in] KEY_B Input key B of type KEY_T.
!>
!> @return RES Result of the comparison, a logical value.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'KEY_LE'
FUNCTION KEY_LE( KEY_A, KEY_B ) RESULT( RES )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(KEY_T), INTENT(IN) :: KEY_A
  TYPE(KEY_T), INTENT(IN) :: KEY_B

  ! Local variables
  LOGICAL :: RES

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  RES = KEY_A%K .LE. KEY_B%K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION KEY_LE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Checks if key A is greater than or equal to key B.
!>
!> This function checks if key A is greater than or equal to key B.
!>
!> @param [in] KEY_A Input key A of type KEY_T.
!> @param [in] KEY_B Input key B of type KEY_T.
!>
!> @return RES Result of the comparison, a logical value.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'KEY_GE'
FUNCTION KEY_GE( KEY_A, KEY_B ) RESULT( RES )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(KEY_T), INTENT(IN) :: KEY_A
  TYPE(KEY_T), INTENT(IN) :: KEY_B

  ! Local variables
  LOGICAL :: RES

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  RES = KEY_A%K .GE. KEY_B%K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION KEY_GE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Free memory used by the key.
!>
!> This function free the memory used by the key. In the current
!> implementation this is jus a dummy routine since no explicit memory
!> is allocated within the key
!>
!> @param [inout] KEY key to be dallocated
!> @param [out]   ERR Error during the dallocation
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'KEY_FREE'
SUBROUTINE KEY_FREE( KEY, ERR )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(KEY_T),        INTENT(INOUT) :: KEY
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: ERR

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Nothing to be deallocated
  ERR = 0
  KEY%K = 0

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE KEY_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Copy a key to another key.
!>
!> This function is used to copy the value of a key to another key.
!>
!> @param [in]  KEY1 Source key
!> @param [out] KEY2 Target key
!> @param [out] ERR  Error during the dallocation
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME ''
SUBROUTINE KEY_COPY( KEY1, KEY2, ERR )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(KEY_T),        INTENT(IN)  :: KEY1
  TYPE(KEY_T),        INTENT(OUT) :: KEY2
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: ERR

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Just copy the integer
  KEY2%K = KEY1%K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE KEY_COPY
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
!> @param [in]    VALUE Pointer to the value associated with the key to be inserted.
!>
!> @note This subroutine assumes that the map data structure is properly initialized.
!>        It is the responsibility of the caller to ensure that the map is correctly set up
!>
!> @warning This subroutine does not perform any action if the specified key is already found in the map.
!>          It is the responsibility of the caller to handle such scenarios appropriately.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAP_INSERT_NODE'
FUNCTION MAP_INSERT_NODE( ROOT, KEY, VALUE, FORCE ) RESULT(EX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(KEY_T),               INTENT(IN)    :: KEY
  CLASS(*), POINTER,         INTENT(IN)    :: VALUE
  LOGICAL, OPTIONAL,         INTENT(IN)    :: FORCE

  ! Function result
  LOGICAL :: EX

  ! Local variables
  LOGICAL :: LOC_FORCE
  TYPE(MAP_NODE_T), POINTER :: INSERTION_POINT
  INTEGER(KIND=JPIB_K) :: FOUND
  INTEGER(KIND=JPIB_K) :: ERR

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( PRESENT(FORCE) ) THEN
    LOC_FORCE = FORCE
  ELSE
    LOC_FORCE = .FALSE.
  ENDIF

  ! Map is empty
  IF ( ASSOCIATED( ROOT, NIL  ) ) THEN

    EX = .TRUE.
    CALL ALLOCATE_NODE( ROOT )
    INSERTION_POINT => NIL
    CALL MAP_NODE_INIT( ROOT, INSERTION_POINT, KEY, VALUE )
    CALL INSERT_FIXUP( ROOT, ROOT )
    INSERTION_POINT => ROOT

  ! Map not empty
  ELSE

    CALL SEARCH( ROOT, INSERTION_POINT, KEY, FOUND )
    IF ( FOUND .NE. 0 ) THEN
      EX = .TRUE.
      IF ( KEY .LT. INSERTION_POINT%KEY ) THEN
        CALL ALLOCATE_NODE( INSERTION_POINT%LEFT )
        CALL MAP_NODE_INIT( INSERTION_POINT%LEFT, INSERTION_POINT, KEY, VALUE )
        CALL INSERT_FIXUP( ROOT, INSERTION_POINT%LEFT )
      ELSE
        CALL ALLOCATE_NODE( INSERTION_POINT%RIGHT )
        CALL MAP_NODE_INIT( INSERTION_POINT%RIGHT, INSERTION_POINT, KEY, VALUE )
        CALL INSERT_FIXUP( ROOT, INSERTION_POINT%RIGHT )
      ENDIF
    ELSE
      EX = .FALSE.
      ! @todo: decide what to do in the case in which the key is
      !        already present in the map
      ! IF ( LOC_FORCE ) THEN
      ! ELSE
      ! ENDIF
    ENDIF

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION MAP_INSERT_NODE
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_REMOVE_NODE'
SUBROUTINE MAP_REMOVE_NODE( ROOT, KEY )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(KEY_T),                INTENT(IN)    :: KEY

  ! Local variables
  TYPE(MAP_NODE_T), POINTER :: INSERTION_POINT
  INTEGER(JPIB_K) :: FOUND
  INTEGER(JPIB_K) :: ERR

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( .NOT.ASSOCIATED( ROOT, NIL  ) ) THEN
    CALL SEARCH( ROOT, INSERTION_POINT, KEY, FOUND )
    IF ( FOUND .EQ. 0 ) THEN
      CALL REMOVE_NODE( ROOT, INSERTION_POINT, ERR )
    ELSE
      ! @todo: decide what to do in the case in which the key is already present
    ENDIF
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_REMOVE_NODE
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
#define PP_PROCEDURE_TYPE 'RECURSIVE SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_FREE_NODE'
RECURSIVE SUBROUTINE MAP_FREE_NODE( CURRENT, ERR )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: CURRENT
  INTEGER(KIND=JPIB_K),        INTENT(OUT)   :: ERR

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation
  ERR = 0

  ! Implementation
  IF ( .NOT.ASSOCIATED( CURRENT, NIL ) ) THEN

    ! Deallocate left subtree.
    CALL MAP_FREE_NODE( CURRENT%LEFT, ERR )

    ! Deallocate right subtree.
    CALL MAP_FREE_NODE( CURRENT%RIGHT, ERR )

    ! Free memory
    CALL DEALLOCATE_NODE( CURRENT, ERR )

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_FREE_NODE
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SEARCH'
SUBROUTINE SEARCH( ROOT, CURRENT, KEY, ERR )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: CURRENT
  TYPE(KEY_T),               INTENT(IN)    :: KEY
  INTEGER(KIND=JPIB_K),      INTENT(OUT)   :: ERR

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization.
  ERR = 0
  CURRENT => ROOT

  ! Map is empty.
  IF ( ASSOCIATED(ROOT, NIL) ) THEN

    ERR = 10

  ELSE

    ! Perform the search loop
    SearchLoop: DO  WHILE( .NOT.ASSOCIATED( CURRENT, NIL ) .AND. ( KEY .NE. CURRENT%KEY ) )

      ! Left subtree
      IF ( KEY .LT. CURRENT%KEY ) THEN

        IF (.NOT.ISLEAF( CURRENT%LEFT )) THEN

          CURRENT => CURRENT%LEFT
          CYCLE SearchLoop

        ELSE

          ERR = -1
          EXIT SearchLoop

        ENDIF

      ! Node Found
      ELSEIF ( KEY .EQ. CURRENT%KEY ) THEN

        ERR = 0

        EXIT SearchLoop

      ! Right subtree
      ELSE

        IF ( .NOT.ISLEAF( CURRENT%RIGHT ) ) THEN

          CURRENT => CURRENT%RIGHT

          CYCLE SearchLoop

        ELSE

          ERR = 1

          EXIT SearchLoop

        ENDIF

      ENDIF

    ENDDO SearchLoop

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SEARCH
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
!> @param [in]    VALUE  Pointer to the value associated with the node.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_NODE_INIT'
SUBROUTINE MAP_NODE_INIT( THIS, PARENT, KEY, VALUE )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T),          INTENT(INOUT) :: THIS
  TYPE(MAP_NODE_T), POINTER, INTENT(IN)    :: PARENT
  TYPE(KEY_T),               INTENT(IN)    :: KEY
  CLASS(*), POINTER,         INTENT(IN)    :: VALUE

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( .NOT. ASSOCIATED(PARENT) ) THEN
    NULLIFY(THIS%PARENT)
  ELSE
    THIS%PARENT => PARENT
  ENDIF
  THIS%RIGHT  => NIL
  THIS%LEFT   => NIL
  THIS%RED    = .TRUE.
  THIS%KEY%K  = KEY%K
  THIS%VALUE  => VALUE
  THIS%IDX    = -99

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_NODE_INIT
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ALLOCATE_NODE'
SUBROUTINE ALLOCATE_NODE( CURRENT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: CURRENT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ERR

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Allocate, initialize and fill the fields.
  NULLIFY( CURRENT )
  ALLOCATE( CURRENT )

  ! Copy key fields in current node.
  CALL KEY_FREE( CURRENT%KEY, ERR )

  ! Connect pointers.
  CURRENT%LEFT => NIL
  CURRENT%RIGHT => NIL
  CURRENT%PARENT => NIL

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE ALLOCATE_NODE
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'DEALLOCATE_NODE'
SUBROUTINE DEALLOCATE_NODE( X, ERR )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: X
  INTEGER(KIND=JPIB_K),      INTENT(OUT)   :: ERR

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  ! Initialization.
  ERR = 0

  ! Check node is associated
  IF ( ASSOCIATED( X ) ) THEN

    ! Free data.
    CALL KEY_FREE( X%KEY, ERR )
    NULLIFY( X%VALUE )

    ! Free node memory.
    DEALLOCATE( X, STAT=ERR )
    NULLIFY( X )

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE DEALLOCATE_NODE
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'INSERT_FIXUP'
SUBROUTINE INSERT_FIXUP( ROOT, CUR )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: CUR

  ! Local variables
  TYPE(MAP_NODE_T), POINTER :: Y=>NULL()
  TYPE(MAP_NODE_T), POINTER :: X=>NULL()

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  !Balancing tree.
  CUR%RED = .TRUE.

  X => CUR

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

          CALL ROTATE_LEFT( ROOT, X )

        ENDIF

        X%PARENT%RED = .FALSE.

        X%PARENT%PARENT%RED = .TRUE.

        CALL ROTATE_RIGHT( ROOT, X%PARENT%PARENT )

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

          CALL ROTATE_RIGHT( ROOT, X )

        ENDIF

        X%PARENT%RED = .FALSE.

        X%PARENT%PARENT%RED = .TRUE.

        CALL ROTATE_LEFT( ROOT, X%PARENT%PARENT )

      ENDIF

    ENDIF

  ENDDO

  ! Change color of the root
  ROOT%RED = .FALSE.

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE INSERT_FIXUP
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ROTATE_LEFT'
SUBROUTINE ROTATE_LEFT(ROOT, X_)

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(MAP_NODE_T), TARGET,  INTENT(INOUT) :: X_

  ! Local variables
  TYPE(MAP_NODE_T), POINTER :: X => NULL()
  TYPE(MAP_NODE_T), POINTER :: Y => NULL()

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization.
  X => X_

  ! Rotate.
  Y => X%RIGHT
  X%RIGHT => Y%LEFT
  IF ( .NOT.ISLEAF( Y%LEFT ) ) THEN

    Y%LEFT%PARENT => X

  ENDIF

  Y%PARENT => X%PARENT

  IF ( ASSOCIATED( X%PARENT, NIL ) ) THEN

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

END SUBROUTINE ROTATE_LEFT
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ROTATE_RIGHT'
SUBROUTINE ROTATE_RIGHT(ROOT, X_)

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T),POINTER, INTENT(INOUT) :: ROOT
  TYPE(MAP_NODE_T), TARGET, INTENT(INOUT) :: X_

  ! Local variables
  TYPE(MAP_NODE_T), POINTER :: X => NULL()
  TYPE(MAP_NODE_T), POINTER :: Y => NULL()

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization.
  X => X_

  ! Rotate.
  Y => X%LEFT
  X%LEFT => Y%RIGHT
  IF ( .NOT.ISLEAF( Y%RIGHT ) ) THEN

    Y%RIGHT%PARENT => X

  ENDIF

  Y%PARENT => X%PARENT

  IF ( ASSOCIATED( X%PARENT, NIL ) ) THEN

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

END SUBROUTINE ROTATE_RIGHT
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
#define PP_PROCEDURE_NAME 'ISLEAF'
FUNCTION ISLEAF(X) RESULT(B)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,   ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE (MAP_NODE_T), POINTER, INTENT(IN) :: X

  ! Function result
  LOGICAL :: B

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  B = ASSOCIATED(X, NIL)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION ISLEAF
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
FUNCTION SUCCESSOR( X, ERR ) RESULT( Y )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(IN)  :: X
  INTEGER(KIND=JPIB_K),      INTENT(OUT) :: ERR

  ! Function result
  TYPE(MAP_NODE_T), POINTER :: Y

  ! Local variables
  TYPE(MAP_NODE_T), POINTER :: X_

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation.
  ERR = 0
  X_ => X
  Y => NULL()

  ! Check.
  IF ( ASSOCIATED( X, NIL ) ) THEN

    ERR = 1

  ELSE

    ! Search cycle.
    IF ( .NOT.ASSOCIATED( X%RIGHT, NIL ) ) THEN

      ! If the node has a right child then the successor is the
      ! minimum of the  right subtree of the node.
      Y => MINIMUM( X%RIGHT )

    ELSE

      ! If the node has not right child, then the successor is the
      ! nearest parent node whose left child is a parent of the
      ! node.
      Y = X%PARENT

      SEARCH: DO

        IF ( .NOT.ASSOCIATED( Y, NIL ) .AND. ASSOCIATED( X_, Y%RIGHT ) ) THEN

          X_ => Y

          Y => Y%PARENT

        ELSE

          EXIT SEARCH

        ENDIF

      ENDDO SEARCH

    ENDIF

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
FUNCTION PREDECESSOR( X, ERR ) RESULT(Y)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(IN)  :: X
  INTEGER(KIND=JPIB_K),      INTENT(OUT) :: ERR

  ! Function result
  TYPE(MAP_NODE_T), POINTER :: Y

  ! Local variables
  TYPE(MAP_NODE_T), POINTER :: X_

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation.
  ERR = 0
  X_ => X
  Y => NULL()

  ! Check.
  IF ( ASSOCIATED( X, NIL ) ) THEN

    ERR = 1

  ELSE

    ! Search cycle.
    IF ( .NOT.ASSOCIATED( X%LEFT, NIL ) ) THEN

      ! If the node has a left child then the successor is the
      ! minimum of the  left subtree of the node.
      Y => MAXIMUM( X%LEFT )

    ELSE

      ! If the node has not left child, then the predecessor is the
      ! nearest  parent node whose right child is a parent of the
      ! node.
      Y = X%PARENT

      SEARCH: DO

        IF ( .NOT.ASSOCIATED( Y, NIL ) .AND. ASSOCIATED( X_, Y%LEFT ) ) THEN

          X_ => Y

          Y => Y%PARENT

        ELSE

          EXIT SEARCH

        ENDIF

      ENDDO SEARCH

    ENDIF

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
FUNCTION MINIMUM( X ) RESULT( Y )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(IN) :: X

  ! Function result
  TYPE(MAP_NODE_T), POINTER :: Y

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization.
  Y => X

  ! Search cycle.
  SEARCH: DO

    IF ( .NOT.ASSOCIATED( Y%LEFT, NIL ) ) THEN

      Y => Y%LEFT

    ELSE

      EXIT SEARCH

    ENDIF

  ENDDO SEARCH

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
FUNCTION MAXIMUM( X ) RESULT( Y )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(IN) :: X

  ! Function result
  TYPE(MAP_NODE_T), POINTER :: Y

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization.
  Y => X

  ! Search cycle.
  SEARCH: DO

    IF ( .NOT.ASSOCIATED( Y%RIGHT, NIL ) ) THEN

      Y => Y%RIGHT

    ELSE

      EXIT SEARCH

    ENDIF

  ENDDO SEARCH

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SWAP_DATA'
SUBROUTINE SWAP_DATA( NODE_1, NODE_2 )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: NODE_1
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: NODE_2

  ! Local variables
  CLASS(*) , POINTER :: TMP_VALUE
  TYPE(KEY_T)        :: TMP_KEY
  INTEGER(KIND=JPIB_K) :: ERR

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization.
  ERR = 0

  ! Saving into temporary variables NODE_2 data.
  TMP_VALUE   => NODE_2%VALUE
  CALL KEY_COPY( NODE_2%KEY, TMP_KEY, ERR )

  ! Reset NODE_2
  CALL KEY_FREE( NODE_2%KEY, ERR )
  NODE_2%VALUE => NULL()

  ! Copy all data contained in node_1 into node_2.
  CALL KEY_COPY( NODE_1%KEY, NODE_2%KEY, ERR )
  CALL KEY_FREE( NODE_1%KEY, ERR )
  NODE_2%VALUE => NODE_1%VALUE

  ! Copy temporay saved data in NODE_1
  CALL KEY_COPY( TMP_KEY, NODE_1%KEY, ERR )
  CALL KEY_FREE( TMP_KEY, ERR )
  NODE_1%VALUE => TMP_VALUE

  ! Free memory and reset pointers.
  TMP_VALUE=>NULL()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SWAP_DATA
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'REMOVE_NODE'
SUBROUTINE REMOVE_NODE( ROOT, Z, ERR )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: Z
  INTEGER(KIND=JPIB_K),        INTENT(OUT)   :: ERR

  ! Local variables
  TYPE(MAP_NODE_T), POINTER :: Y => NULL()
  TYPE(MAP_NODE_T), POINTER :: X => NULL()

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization.
  ERR = 0

  ! Rimozione del nodo.
  IF ( ASSOCIATED( Z%LEFT, NIL ) .OR. ASSOCIATED( Z%RIGHT, NIL ) ) THEN

    Y => Z

  ELSE

    Y => SUCCESSOR( Z, ERR )

  ENDIF

  IF ( .NOT.ASSOCIATED( Y%LEFT, NIL ) ) THEN

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

    IF ( .NOT.ASSOCIATED( X, NIL ) ) THEN

      X%PARENT => Y%PARENT

    ENDIF

  ENDIF


  IF ( ASSOCIATED( Y%PARENT, NIL ) ) THEN

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
    CALL SWAP_DATA( Y, Z )

  ENDIF

  ! Adjust colors of the map.
  IF ( RED_BLACK_BALANCING ) THEN

    IF ( .NOT.Y%RED ) THEN

      CALL REMOVE_NODE_FIXUP( ROOT, X, ERR )

    ENDIF

  ENDIF

  ! Free memory.
  CALL DEALLOCATE_NODE( Y, ERR )
  Y => NULL()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE REMOVE_NODE
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'REMOVE_NODE_FIXUP'
SUBROUTINE REMOVE_NODE_FIXUP( ROOT, X, ERR )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: ROOT
  TYPE(MAP_NODE_T), POINTER, INTENT(INOUT) :: X
  INTEGER(KIND=JPIB_K),        INTENT(OUT)   :: ERR

  ! Local variables
  TYPE(MAP_NODE_T), POINTER :: W=>NULL()

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization.
  ERR = 0

  ! Reconfigurazione dei colori.
  DO WHILE( .NOT.ASSOCIATED( X, ROOT ) .AND. .NOT.X%RED )

    LEFT_CHILD : IF ( ASSOCIATED( X, X%PARENT%LEFT ) ) THEN

      W => X%PARENT%RIGHT

      CASE_1 : IF ( W%RED ) THEN

        W%RED = .FALSE.

        X%PARENT%RED = .TRUE.

        CALL ROTATE_LEFT(ROOT, X%PARENT )

        W => X%PARENT%RIGHT

      ENDIF  CASE_1


      ! IF ( ASSOCIATED( W, NIL) ) THEN
      !   ERROR
      !   RETURN
      ! ENDIF

      CASE_2_3_4 : IF ( ( .NOT.W%LEFT%RED ) .AND. ( .NOT.W%RIGHT%RED ) ) THEN
        W%RED = .TRUE.

        X => X%PARENT

      ELSE

        CASE_3 : IF ( .NOT.W%RIGHT%RED ) THEN ! CASO 3

          W%LEFT%RED = .FALSE.

          W%RED = .TRUE.

          CALL ROTATE_RIGHT(ROOT, W )

          W => X%PARENT%RIGHT

        ENDIF CASE_3

        W%RED = X%PARENT%RED

        X%PARENT%RED = .FALSE.

        W%RIGHT%RED = .FALSE.

        CALL ROTATE_LEFT(ROOT, X%PARENT )

        X => ROOT

      ENDIF CASE_2_3_4

      ELSE ! Right child

      W => X%PARENT%LEFT

      CASE_1B : IF ( W%RED ) THEN

        W%RED = .FALSE.

        X%PARENT%RED = .TRUE.

        CALL ROTATE_RIGHT(ROOT, X%PARENT )

        W => X%PARENT%LEFT

      ENDIF  CASE_1B

      ! IF ( ASSOCIATED( W, NIL) ) THEN
      !   ERROR
      !   RETURN
      ! ENDIF

      CASE_2_3_4B : IF ( .NOT.W%RIGHT%RED .AND. .NOT.W%LEFT%RED ) THEN

        W%RED = .TRUE.

        X => X%PARENT

      ELSE

        CASE_3B : IF ( .NOT.W%LEFT%RED ) THEN

          W%RIGHT%RED = .FALSE.

          W%RED = .TRUE.

          CALL ROTATE_LEFT(ROOT, W )

          W => X%PARENT%LEFT

        ENDIF CASE_3B

        W%RED = X%PARENT%RED

        X%PARENT%RED = .FALSE.

        W%LEFT%RED = .FALSE.

        CALL ROTATE_RIGHT(ROOT, X%PARENT )

        X => ROOT

      ENDIF CASE_2_3_4B

    ENDIF LEFT_CHILD

  ENDDO

  ! Change color to the node
  X%RED = .FALSE.

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE REMOVE_NODE_FIXUP
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
#define PP_PROCEDURE_TYPE 'RECURSIVE SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_LIST_NODE'
RECURSIVE SUBROUTINE MAP_LIST_NODE( ROOT, CURRENT, UNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER, INTENT(IN) :: ROOT
  TYPE(MAP_NODE_T), POINTER, INTENT(IN) :: CURRENT
  INTEGER(KIND=JPIB_K),        INTENT(IN) :: UNIT

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! First node in the list.
  IF ( .NOT. ASSOCIATED(CURRENT, NIL) ) THEN

    CALL MAP_LIST_NODE( ROOT, CURRENT%LEFT, UNIT )
    CALL MAP_LIST_NODE( ROOT, CURRENT%RIGHT, UNIT )

    WRITE(UNIT,*) ' P -> ', CURRENT%KEY%K

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_LIST_NODE
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
#define PP_PROCEDURE_TYPE 'RECURSIVE SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_RENUMBER_NODE'
RECURSIVE SUBROUTINE MAP_RENUMBER_NODE( ROOT, IDX )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T),     INTENT(INOUT) :: ROOT
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: IDX

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Renumber left subtree
  IF ( .NOT. ASSOCIATED( ROOT%LEFT, NIL ) ) THEN
    CALL MAP_RENUMBER_NODE( ROOT%LEFT, IDX )
  ENDIF

  ! Renumber right subtree
  IF ( .NOT. ASSOCIATED( ROOT%RIGHT, NIL ) ) THEN
    CALL MAP_RENUMBER_NODE( ROOT%RIGHT, IDX )
  ENDIF

  ! Renumber the current node
  IDX = IDX + 1
  ROOT%IDX = IDX

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_RENUMBER_NODE
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
#define PP_PROCEDURE_TYPE 'RECURSIVE SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_NODE_WRITE_CONNECTIVITY'
RECURSIVE SUBROUTINE MAP_NODE_WRITE_CONNECTIVITY( ROOT, UNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T),   INTENT(INOUT) :: ROOT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Write connectivity of the left subtree
  IF ( .NOT. ASSOCIATED( ROOT%LEFT, NIL ) ) THEN
    CALL MAP_NODE_WRITE_CONNECTIVITY( ROOT%LEFT, UNIT )
  ENDIF

  ! Write connectivity of the right subtree
  IF ( .NOT. ASSOCIATED( ROOT%RIGHT, NIL ) ) THEN
    CALL MAP_NODE_WRITE_CONNECTIVITY( ROOT%RIGHT, UNIT )
  ENDIF

  ! Write connectivity of the current node
  IF ( ASSOCIATED(ROOT%PARENT) .AND. .NOT. ASSOCIATED( ROOT%PARENT, NIL) ) THEN
    WRITE(UNIT,'(I6.6,A,I6.6)') ROOT%PARENT%IDX, '->', ROOT%IDX
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_NODE_WRITE_CONNECTIVITY
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
#define PP_PROCEDURE_TYPE 'RECURSIVE SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_WRITE_NODE'
RECURSIVE SUBROUTINE MAP_WRITE_NODE( ROOT, UNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MAP_NODE_T),    INTENT(INOUT) :: ROOT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Write nodes in the left subtree
  IF ( .NOT. ASSOCIATED( ROOT%LEFT, NIL ) ) THEN
    CALL MAP_WRITE_NODE( ROOT%LEFT, UNIT )
  ENDIF

  ! Write nodes in the right subtree
  IF ( .NOT. ASSOCIATED( ROOT%RIGHT, NIL ) ) THEN
    CALL MAP_WRITE_NODE( ROOT%RIGHT, UNIT )
  ENDIF

  ! Write the current node
  IF ( ROOT%RED ) THEN
    WRITE(UNIT,'(I6.6,A,I8.8,A)') ROOT%IDX, '   [ label="', ROOT%KEY%K ,'", fillcolor=red]'
  ELSE
    WRITE(UNIT,'(I6.6,A,I8.8,A)') ROOT%IDX, '   [ label="', ROOT%KEY%K ,'", fillcolor=black]'
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_WRITE_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Initializes a map structure.
!>
!> This subroutine initializes the given map structure.
!> It sets up the necessary components to prepare the map for use.
!>
!> @param [inout] MAP The map structure to be initialized.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_INIT'
SUBROUTINE MAP_INIT( MAP )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_T), INTENT(INOUT) :: MAP

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Map initialization.
  MAP%ROOT => NIL
  MAP%SIZE = 0

  ! Nil initialisation
  !> @todo this needs to be moved somewhere else
  NIL%LEFT  => NIL
  NIL%RIGHT => NIL
  NIL%RED   =  .FALSE.
  NIL%KEY%K =  -99

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Finalisation of a map structure.
!>
!> This subroutine finalises the given map structure.
!>
!> @param [inout] MAP The map structure to be finalised.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_FREE'
SUBROUTINE MAP_FREE( MAP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_T), INTENT(INOUT) :: MAP

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ERR
  INTEGER(KIND=JPIB_K) :: NUMEL

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization.
  ERR = 0

  ! Remove the map if it is not empty.
  IF ( MAP%SIZE .GT. 0 ) THEN

    ! Recursive deletion of the tree.
    CALL MAP_FREE_NODE( MAP%ROOT, ERR )

  ENDIF

  ! Reset the initial condition.
  CALL MAP_INIT( MAP )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Get the minimum value in the map.
!>
!> This subroutine finalises the given map structure.
!>
!> @param [inout] MAP The map structure to be finalised.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_MINIMUM'
SUBROUTINE MAP_MINIMUM( MAP, KEY, VALUE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_T),       INTENT(INOUT) :: MAP
  TYPE(KEY_T),       INTENT(OUT)   :: KEY
  CLASS(*), POINTER, INTENT(OUT)   :: VALUE

  ! Local variables
  TYPE(MAP_NODE_T), POINTER :: Y
  INTEGER(KIND=JPIB_K) :: ERR
  INTEGER(KIND=JPIB_K) :: NUMEL

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization.
  ERR = 0

  ! Remove the map if it is not empty.
  IF ( MAP%SIZE .GT. 0 ) THEN

    ! Recursive deletion of the tree.
    Y => MINIMUM( MAP%ROOT )

    KEY = Y%KEY
    VALUE => Y%VALUE

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_MINIMUM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Get the maximum value of the map.
!>
!> This subroutine finalises the given map structure.
!>
!> @param [inout] MAP The map structure to be finalised.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_MAXIMUM'
SUBROUTINE MAP_MAXIMUM( MAP, KEY, VALUE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_T),       INTENT(INOUT) :: MAP
  TYPE(KEY_T),       INTENT(OUT)   :: KEY
  CLASS(*), POINTER, INTENT(OUT)   :: VALUE

  ! Local variables
  TYPE(MAP_NODE_T), POINTER :: Y
  INTEGER(KIND=JPIB_K) :: ERR
  INTEGER(KIND=JPIB_K) :: NUMEL

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization.
  ERR = 0

  ! Remove the map if it is not empty.
  IF ( MAP%SIZE .GT. 0 ) THEN

    ! Recursive deletion of the tree.
    Y => MAXIMUM( MAP%ROOT )

    KEY = Y%KEY
    VALUE => Y%VALUE

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_MAXIMUM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'RECURSIVE SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_GET_SORTED_KEYS_INT_NODE'
RECURSIVE SUBROUTINE MAP_GET_SORTED_KEYS_INT_NODE( NODE, SORTED_KEYS_INT, CNT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_NODE_T), POINTER,          INTENT(INOUT) :: NODE
  INTEGER(KIND=JPIM_K), DIMENSION(:), INTENT(INOUT) :: SORTED_KEYS_INT
  INTEGER(KIND=JPIB_K),               INTENT(INOUT) :: CNT

  ! Local variables
  TYPE(MAP_NODE_T), POINTER :: Y
  TYPE(MAP_NODE_T), POINTER :: PREV
  INTEGER(KIND=JPIB_K) :: ERR
  INTEGER(KIND=JPIB_K) :: NUMEL

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization.
  ERR = 0

  ! Remove the map if it is not empty.
  IF ( .NOT. ASSOCIATED(NODE, NIL) ) THEN

    CALL MAP_GET_SORTED_KEYS_INT_NODE( NODE%LEFT,  SORTED_KEYS_INT, CNT )

    CNT = CNT + 1
    SORTED_KEYS_INT(CNT) = NODE%KEY%K

    CALL MAP_GET_SORTED_KEYS_INT_NODE( NODE%RIGHT, SORTED_KEYS_INT, CNT )

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_GET_SORTED_KEYS_INT_NODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Get the maximum value of the map.
!>
!> This subroutine finalises the given map structure.
!>
!> @param [inout] MAP The map structure to be finalised.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAP_GET_SORTED_KEYS_INT'
FUNCTION MAP_GET_SORTED_KEYS_INT( MAP, SORTED_KEYS_INT ) RESULT(EX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_T),                        INTENT(INOUT) :: MAP
  INTEGER(KIND=JPIM_K), DIMENSION(:), INTENT(OUT)   :: SORTED_KEYS_INT

  ! Function result
  LOGICAL :: EX

  ! Local variables
  TYPE(MAP_NODE_T), POINTER :: Y
  TYPE(MAP_NODE_T), POINTER :: PREV
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: ERR
  INTEGER(KIND=JPIB_K) :: NUMEL

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization.
  ERR = 0
  EX = .TRUE.

  ! Error handing
  IF ( MAP%SIZE .NE. SIZE(SORTED_KEYS_INT) ) THEN
    EX = .FALSE.
    RETURN
  ENDIF

  ! Remove the map if it is not empty.
  IF ( MAP%SIZE .GT. 0 ) THEN

    ! Recursive deletion of the tree.
    CNT = 0
    CALL MAP_GET_SORTED_KEYS_INT_NODE( MAP%ROOT, SORTED_KEYS_INT, CNT )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION MAP_GET_SORTED_KEYS_INT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Inserts a key-value pair into the map.
!>
!> This subroutine inserts a key-value pair into the map structure.
!>
!> @param [inout] THIS  The map structure where the key-value pair will be inserted.
!> @param [in]    KEY   The key to be inserted into the map.
!> @param [in]    VALUE Pointer to the value associated with the key.
!>
!> @note This subroutine assumes that the map structure is properly initialized.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_INSERT'
SUBROUTINE MAP_INSERT( THIS, KEY, VALUE, FORCE )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_T),       INTENT(INOUT) :: THIS
  TYPE(KEY_T),       INTENT(IN)    :: KEY
  CLASS(*), POINTER, INTENT(IN)    :: VALUE
  LOGICAL, OPTIONAL, INTENT(IN)    :: FORCE

  ! Local variables
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( PRESENT(FORCE) ) THEN
    EX = MAP_INSERT_NODE( THIS%ROOT, KEY, VALUE, FORCE )
  ELSE
    EX = MAP_INSERT_NODE( THIS%ROOT, KEY, VALUE )
  ENDIF

  ! Update the number of elements in the map
  IF ( EX ) THEN
    THIS%SIZE = THIS%SIZE + 1
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_INSERT
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
#define PP_PROCEDURE_NAME 'MAP_GET'
FUNCTION MAP_GET( THIS, KEY, VALUE ) RESULT(EX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_T),       INTENT(INOUT) :: THIS
  TYPE(KEY_T),       INTENT(IN)    :: KEY
  CLASS(*), POINTER, INTENT(INOUT) :: VALUE

  ! Function result
  LOGICAL :: EX

  ! Local variables
  INTEGER(KIND=JPIB_K) :: FOUND
  TYPE(MAP_NODE_T), POINTER :: SEARCHED_NODE

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( ASSOCIATED(THIS%ROOT,NIL) ) THEN
    EX = .FALSE.
    VALUE => NULL()
  ELSE

    ! Search the node in the map
    CALL SEARCH( THIS%ROOT, SEARCHED_NODE, KEY, FOUND )

    IF ( FOUND .EQ. 0 ) THEN
      EX = .TRUE.
      VALUE => SEARCHED_NODE%VALUE
    ELSE
      EX = .FALSE.
      VALUE => NULL()
    ENDIF

  ENDIF


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION MAP_GET
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_REMOVE'
SUBROUTINE MAP_REMOVE( THIS, KEY )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_T), INTENT(INOUT) :: THIS
  TYPE(KEY_T), INTENT(IN)    :: KEY

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  CALL MAP_REMOVE_NODE( THIS%ROOT, KEY )
  THIS%SIZE = THIS%SIZE - 1

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_REMOVE
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_LIST'
SUBROUTINE MAP_LIST( THIS, UNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_T),        INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  CALL MAP_LIST_NODE( THIS%ROOT, THIS%ROOT, UNIT )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_LIST
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAP_PRINT'
SUBROUTINE MAP_PRINT( THIS, NAME, IDX )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAP_T),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),     INTENT(IN)    :: NAME
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IDX

  ! Local variables
  CHARACTER(LEN=128)   :: FNAME
  INTEGER(KIND=JPIB_K) :: UNIT

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  FNAME=REPEAT(' ',128)
  WRITE(FNAME,'(A,I8.8,A)') TRIM(ADJUSTL(NAME))//'_', IDX, '.dot'
  UNIT = 0
  CALL MAP_RENUMBER_NODE( THIS%ROOT, UNIT )
  UNIT=131
  IF ( .NOT. ASSOCIATED( THIS%ROOT, NIL ) ) THEN
    open(unit=unit, file=TRIM(FNAME), action='write' )
    WRITE(UNIT,'(A)') 'digraph RedBlackTree {'
    WRITE(UNIT,'(A)') 'node [shape=circle, style=filled, fontcolor=white, fontsize=12, width=0.5]'
    WRITE(UNIT,'(A)') 'edge [arrowhead=vee]'
    WRITE(UNIT,'(A)') ''
    WRITE(UNIT,'(A)') '// Nodes'
    CALL MAP_WRITE_NODE( THIS%ROOT, UNIT )
    WRITE(UNIT,'(A)') ''
    WRITE(UNIT,'(A)') '// Connectivity'
    CALL MAP_NODE_WRITE_CONNECTIVITY( THIS%ROOT, UNIT )
    WRITE(UNIT,'(A)') ''
    WRITE(UNIT,'(A)') '}'
    CLOSE( UNIT )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MAP_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE MAP_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
