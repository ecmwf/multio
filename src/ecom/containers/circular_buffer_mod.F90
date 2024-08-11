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
    PRIVATE
    INTEGER(KIND=JPIB_K), ALLOCATABLE, DIMENSION(:) :: BUFFER_
    INTEGER(KIND=JPIB_K) :: HEAD_ = 1
    INTEGER(KIND=JPIB_K) :: TAIL_ = 1
    INTEGER(KIND=JPIB_K) :: SIZE_ = 0
    INTEGER(KIND=JPIB_K) :: CAPACITY_=-99
  CONTAINS
    PROCEDURE, PUBLIC, PASS :: INIT     => CB_INIT
    PROCEDURE, PUBLIC, PASS :: ENQUEUE  => CB_ENQUEUE
    PROCEDURE, PUBLIC, PASS :: DEQUEUE  => CB_DEQUEUE
    PROCEDURE, PUBLIC, PASS :: GET      => CB_GET
    PROCEDURE, PUBLIC, PASS :: GET_ALL  => CB_GET_ALL
    PROCEDURE, PUBLIC, PASS :: SIZE     => CB_SIZE
    PROCEDURE, PUBLIC, PASS :: IS_EMPTY => CB_IS_EMPTY
    PROCEDURE, PUBLIC, PASS :: FREE     => CB_FREE
  END TYPE

  ! Whitelist of public symbols (datatypes)
  PUBLIC :: CIRCULARBUFFER_T

CONTAINS


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
  CLASS(CIRCULARBUFFER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),    INTENT(IN)    :: CAPACITY

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Free memory just to avoid memory leaks
  CALL THIS%FREE()

  ! Initialize the circular buffer
  THIS%CAPACITY_ = CAPACITY
  ALLOCATE( THIS%BUFFER_(CAPACITY) )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE CB_INIT
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
#define PP_PROCEDURE_NAME 'CB_FREE'
SUBROUTINE CB_FREE( THIS )

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CIRCULARBUFFER_T), INTENT(INOUT) :: THIS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  THIS%HEAD_ = 1
  THIS%TAIL_ = 1
  THIS%SIZE_ = 0
  THIS%CAPACITY_=-99
  IF ( ALLOCATED(THIS%BUFFER_) ) THEN
    DEALLOCATE(THIS%BUFFER_)
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE CB_FREE
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
  CLASS(CIRCULARBUFFER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),    INTENT(IN)    :: VALUE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: TMP
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! If the buffer is full then remove the last element
  IF (THIS%SIZE_ .EQ. THIS%CAPACITY_) THEN
    EX = CB_DEQUEUE( THIS, TMP )
  ENDIF

  ! Add element to the buffer
  THIS%BUFFER_(THIS%HEAD_) = VALUE

  ! Update head and tail
  THIS%HEAD_ = MOD(THIS%HEAD_, THIS%CAPACITY_) + 1
  THIS%SIZE_ = THIS%SIZE_ + 1

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
  CLASS(CIRCULARBUFFER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),    INTENT(OUT)   :: VALUE

  ! Function result
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( THIS%SIZE_ .EQ. 0 ) THEN
    EX = .FALSE.
    VALUE = -9999
  ELSE
    EX = .TRUE.
    VALUE = THIS%BUFFER_(THIS%TAIL_)
    THIS%TAIL_ = MOD(THIS%TAIL_, THIS%CAPACITY_) + 1
    THIS%SIZE_ = THIS%SIZE_ - 1
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
  CLASS(CIRCULARBUFFER_T), INTENT(IN)  :: THIS
  INTEGER(KIND=JPIB_K),    INTENT(IN)  :: I
  INTEGER(KIND=JPIB_K),    INTENT(OUT) :: VALUE

  ! Function result
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( THIS%SIZE_ .EQ. 0 .OR. THIS%CAPACITY_ .LT. I) THEN
    EX = .FALSE.
    VALUE = -9999
  ELSE
    EX = .TRUE.
    VALUE = THIS%BUFFER_(MOD(THIS%HEAD_+THIS%CAPACITY_-I-1,THIS%CAPACITY_)+1)
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
  CLASS(CIRCULARBUFFER_T),            INTENT(IN)  :: THIS
  INTEGER(KIND=JPIB_K),               INTENT(OUT) :: SZ
  INTEGER(KIND=JPIB_K), DIMENSION(:), INTENT(OUT) :: VALUES

  ! Function result
  LOGICAL :: EX

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  VALUES = -9999
  SZ = MIN( SIZE(VALUES), THIS%SIZE_ )
  DO I = 1, SZ
    EX = .TRUE.
    VALUES(SZ-I+1) = THIS%BUFFER_(MOD(THIS%HEAD_+THIS%CAPACITY_-I-1,THIS%CAPACITY_)+1)
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
  CLASS(CIRCULARBUFFER_T), INTENT(IN) :: THIS

  ! Function result
  INTEGER(KIND=JPIB_K) :: SIZE

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  SIZE = THIS%SIZE_

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION CB_SIZE
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
FUNCTION CB_IS_EMPTY(THIS) RESULT(IS_EMPTY)

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CIRCULARBUFFER_T), INTENT(IN) :: THIS

  ! Function result
  LOGICAL :: IS_EMPTY

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IS_EMPTY = (THIS%SIZE_.EQ.0)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION CB_IS_EMPTY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE CIRCULAR_BUFFER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME