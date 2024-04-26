!> @file circular_buffer_mod.F90
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
#define PP_FILE_NAME 'circular_buffer_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'CIRCULAR_BUFFER_MOD'
MODULE CIRCULAR_BUFFER_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

IMPLICIT NONE

  ! Default visibility
  PRIVATE

  !> @brief data structure that contains all the relevant fields needed to define a circular buffer
  TYPE :: CIRCULARBUFFER_T
    INTEGER(KIND=JPIB_K), ALLOCATABLE, DIMENSION(:) :: BUFFER
    INTEGER(KIND=JPIB_K) :: HEAD = 1
    INTEGER(KIND=JPIB_K) :: TAIL = 1
    INTEGER(KIND=JPIB_K) :: SIZE = 0
    INTEGER(KIND=JPIB_K) :: CAPACITY=-99
  END TYPE

  !> @brief node in a list that contains pointer to circular buffers.
  TYPE :: CB_LIST_NODE_T
    CLASS(CIRCULARBUFFER_T), POINTER :: CB_ => NULL()
    TYPE(CB_LIST_NODE_T), POINTER :: NEXT_ => NULL()
    TYPE(CB_LIST_NODE_T), POINTER :: PREV_ => NULL()
  END TYPE

  !> @brief List of circular buffers
  TYPE :: CB_LIST_T
    TYPE(CB_LIST_NODE_T), POINTER :: HEAD_ => NULL()
    TYPE(CB_LIST_NODE_T), POINTER :: TAIL_ => NULL()
    INTEGER(KIND=JPIB_K) :: SIZE = 0
  END TYPE


  ! Whitelist of public symbols (datatypes)
  PUBLIC :: CIRCULARBUFFER_T
  PUBLIC :: CB_LIST_T

  ! Whitelist of public symbols (precedures)
  PUBLIC :: CB_INIT
  PUBLIC :: CB_ENQUEUE
  PUBLIC :: CB_SIZE
  PUBLIC :: CB_GET
  PUBLIC :: CB_GET_ALL
  PUBLIC :: CB_NEW
  PUBLIC :: CB_FREE_ALL

CONTAINS

!>
!> @brief Allocates a new circular buffer.
!>
!> This function allocates memory for a new circular buffer with the specified capacity.
!> The circular buffer structure is initialized, and memory is allocated to hold elements
!> based on the specified capacity.
!>
!> @param [out] THIS     The circular buffer structure to be initialized.
!> @param [in]  CAPACITY The capacity of the circular buffer.
!>
!> @return PCB Pointer to the newly allocated circular buffer.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CB_NEW'
FUNCTION CB_NEW(THIS, CAPACITY) RESULT(PCB)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(CB_LIST_T),      INTENT(OUT) :: THIS
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: CAPACITY

  ! Local variables
  CLASS(CIRCULARBUFFER_T), POINTER :: PCB

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation
  NULLIFY( PCB )

  ! Allocation
  IF ( .NOT. ASSOCIATED(THIS%HEAD_) ) THEN
    ALLOCATE( THIS%HEAD_ )
    THIS%TAIL_ => THIS%HEAD_
    ALLOCATE( THIS%HEAD_%CB_ )
    CALL CB_INIT( THIS%HEAD_%CB_, CAPACITY )
    PCB => THIS%TAIL_%CB_
    THIS%SIZE = 1
    NULLIFY(THIS%HEAD_%PREV_)
    NULLIFY(THIS%HEAD_%NEXT_)
  ELSE
    ALLOCATE( THIS%TAIL_%NEXT_ )
    THIS%TAIL_%NEXT_%PREV_ => THIS%TAIL_
    THIS%TAIL_ => THIS%TAIL_%NEXT_
    NULLIFY(THIS%TAIL_%NEXT_)
    PCB => THIS%TAIL_%CB_
    THIS%SIZE = THIS%SIZE + 1
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION CB_NEW
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees memory allocated for all circular buffers.
!>
!> This subroutine frees the memory allocated for all circular buffers in the list.
!> It deallocates memory associated with each circular buffer and resets the list.
!>
!> @param [out] THIS The circular buffer list structure to be freed.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'CB_FREE_ALL'
SUBROUTINE CB_FREE_ALL(THIS)

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(CB_LIST_T), INTENT(OUT) :: THIS

  ! Local variables
  TYPE(CB_LIST_NODE_T), POINTER :: CURR

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( ASSOCIATED(THIS%HEAD_) ) THEN

    DO

      IF ( .NOT.ASSOCIATED(THIS%TAIL_) ) THEN
        EXIT
      ENDIF

      CURR => THIS%TAIL_
      THIS%TAIL_ => THIS%TAIL_%PREV_
      IF ( ASSOCIATED(THIS%TAIL_) ) THEN
        NULLIFY(THIS%TAIL_%NEXT_)
      ENDIF

      DEALLOCATE( CURR%CB_ )
      ! Paranoid operation
      NULLIFY( CURR%CB_ )
      DEALLOCATE( CURR )
      NULLIFY(CURR)

      THIS%SIZE = THIS%SIZE - 1

    ENDDO

    NULLIFY(THIS%TAIL_)
    NULLIFY(THIS%HEAD_)
    THIS%SIZE = 0

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE CB_FREE_ALL
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'CB_INIT'
SUBROUTINE CB_INIT(THIS, CAPACITY)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(CIRCULARBUFFER_T), INTENT(OUT) :: THIS
  INTEGER(KIND=JPIB_K),   INTENT(IN)  :: CAPACITY

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  THIS%CAPACITY = CAPACITY
  ALLOCATE( THIS%BUFFER(CAPACITY) )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE CB_INIT
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'CB_ENQUEUE'
SUBROUTINE CB_ENQUEUE(THIS, VALUE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(CIRCULARBUFFER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),   INTENT(IN) :: VALUE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: TMP
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! If the buffer is full then remove the last element
  IF (THIS%SIZE .EQ. THIS%CAPACITY) THEN
    EX = CB_DEQUEUE( THIS, TMP )
  ENDIF

  ! Add element to the buffer
  THIS%BUFFER(THIS%HEAD) = VALUE

  ! Update head and tail
  THIS%HEAD = MOD(THIS%HEAD, THIS%CAPACITY) + 1
  THIS%SIZE = THIS%SIZE + 1

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE CB_ENQUEUE
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
FUNCTION CB_DEQUEUE(THIS, VALUE) RESULT(EX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(CIRCULARBUFFER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),   INTENT(OUT)   :: VALUE

  ! Function result
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( THIS%SIZE .EQ. 0 ) THEN
    EX = .FALSE.
    VALUE = -9999
  ELSE
    EX = .TRUE.
    VALUE = THIS%BUFFER(THIS%TAIL)
    THIS%TAIL = MOD(THIS%TAIL, THIS%CAPACITY) + 1
    THIS%SIZE = THIS%SIZE - 1
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
FUNCTION CB_GET(THIS, I, VALUE) RESULT(EX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(CIRCULARBUFFER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),   INTENT(IN)    :: I
  INTEGER(KIND=JPIB_K),   INTENT(OUT)   :: VALUE

  ! Function result
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( THIS%SIZE .EQ. 0 .OR. THIS%CAPACITY .LT. I) THEN
    EX = .FALSE.
    VALUE = -9999
  ELSE
    EX = .TRUE.
    VALUE = THIS%BUFFER(MOD(THIS%HEAD+THIS%CAPACITY-I-1,THIS%CAPACITY)+1)
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
FUNCTION CB_GET_ALL(THIS, SZ, VALUES) RESULT(EX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(CIRCULARBUFFER_T),             INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),               INTENT(OUT)   :: SZ
  INTEGER(KIND=JPIB_K), DIMENSION(:), INTENT(OUT)   :: VALUES

  ! Function result
  LOGICAL :: EX

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  VALUES = -9999
  SZ = MIN( SIZE(VALUES), THIS%SIZE )
  DO I = 1, SZ
    EX = .TRUE.
    VALUES(SZ-I+1) = THIS%BUFFER(MOD(THIS%HEAD+THIS%CAPACITY-I-1,THIS%CAPACITY)+1)
  ENDDO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
FUNCTION CB_SIZE(THIS) RESULT(SIZE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(CIRCULARBUFFER_T), INTENT(IN) :: THIS

  ! Function result
  INTEGER(KIND=JPIB_K) :: SIZE

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  SIZE = THIS%SIZE

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION CB_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE CIRCULAR_BUFFER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME