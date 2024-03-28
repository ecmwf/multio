!> @file stack_mod.F90
!>
!> @brief Definition of a stack data structure to be used as allocator
!>
!> @note currently not used, implemented just for future use
!>
!> @author Mirco Valentini
!> @date February 07, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'stack_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'STACK_MOD'
MODULE STACK_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

IMPLICIT NONE

! Default visibility of the module
PRIVATE

!> @brief example of a structure to used as allocator atom
TYPE :: NODE_T
  INTEGER :: I
END TYPE

!> @brief Pointer to the structure
TYPE :: STACK_DATA_T
  TYPE(NODE_T), POINTER :: P_ => NULL()
END TYPE

!> @brief datatype used to represent the stack allocator/memory
TYPE :: STACK_T
  INTEGER(JPIB_K) :: RSP
  INTEGER(JPIB_K) :: SIZE
  TYPE(STACK_DATA_T), DIMENSION(:), POINTER :: DATA_
  TYPE(NODE_T), DIMENSION(:), POINTER :: MEM_
END TYPE


! Whitelist of public symbols
PUBLIC :: STACK_INIT
PUBLIC :: STACK_FREE
PUBLIC :: STACK_PUSH
PUBLIC :: STACK_POP

CONTAINS

!>
!> @brief Initializes a stack with the specified size.
!>
!> This subroutine initializes a stack with the specified size. The stack's internal structure
!> will be allocated with memory to accommodate elements up to the given size.
!>
!> @param [inout] THIS The stack structure to be initialized.
!> @param [in]    SIZE The size of the stack, indicating the maximum number of elements it can hold.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'STACK_INIT'
SUBROUTINE STACK_INIT( THIS, SIZE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(STACK_T), INTENT(INOUT) :: THIS
  INTEGER,       INTENT(IN)    :: SIZE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  THIS%RSP  = 0
  THIS%SIZE = SIZE
  ALLOCATE( THIS%MEM_(SIZE) )
  ALLOCATE( THIS%DATA_(SIZE) )

  DO I = 1, SIZE
    THIS%RSP = THIS%RSP + 1
    THIS%MEM_(THIS%RSP)%I = I
    THIS%DATA_(THIS%RSP)%P_ => THIS%MEM_(THIS%RSP)
  ENDDO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE STACK_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees the memory allocated for a stack.
!>
!> This subroutine frees the memory allocated for the specified stack, releasing all resources
!> associated with it. After calling this subroutine, the stack should no longer be used.
!>
!> @param [inout] THIS The stack structure for which memory is to be freed.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'STACK_FREE'
SUBROUTINE STACK_FREE( THIS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(STACK_T), INTENT(INOUT) :: THIS

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  THIS%RSP  = 0
  THIS%SIZE = 0
  IF ( ASSOCIATED(THIS%MEM_) ) THEN
    DEALLOCATE( THIS%MEM_ )
  ENDIF

  IF ( ASSOCIATED(THIS%DATA_) ) THEN
    DEALLOCATE( THIS%DATA_ )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE STACK_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Pushes a new node onto the stack.
!>
!> This subroutine pushes a new node onto the top of the stack. The provided pointer to the node
!> will be added to the stack, effectively becoming the new top element.
!>
!> @param [inout] THIS The stack structure onto which the node is to be pushed.
!> @param [in]    PTR  Pointer to the node to be pushed onto the stack.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'STACK_PUSH'
SUBROUTINE STACK_PUSH( THIS, PTR )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(STACK_T),         INTENT(INOUT) :: THIS
  TYPE(NODE_T), POINTER, INTENT(IN)    :: PTR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( THIS%RSP .GE. THIS%SIZE, 1 )

  THIS%RSP = THIS%RSP + 1
  THIS%DATA_(THIS%RSP)%P_ => PTR

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Stack is full' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE STACK_PUSH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Pops the top node from the stack.
!>
!> This subroutine pops the top node from the stack, removing it from the stack structure. The pointer
!> to the popped node is returned through the PTR argument.
!>
!> @param [inout] THIS The stack structure from which the node is to be popped.
!> @param [out]   PTR  Pointer to the node popped from the stack.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'STACK_POP'
SUBROUTINE STACK_POP( THIS, PTR )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(STACK_T),         INTENT(INOUT) :: THIS
  TYPE(NODE_T), POINTER, INTENT(OUT)    :: PTR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW(THIS%RSP .LE. 1, 1 )

  PTR => THIS%DATA_(THIS%RSP)%P_
  THIS%DATA_(THIS%RSP)%P_ => NULL()
  THIS%RSP = THIS%RSP - 1

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Stack is empty' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN
END SUBROUTINE STACK_POP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE STACK_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME


! PROGRAM test
!   USE :: STACK_MOD
! IMPLICIT NONE
!   TYPE(STACK_T) :: TS
!   TYPE(NODE_T), POINTER :: P1
!   TYPE(NODE_T), POINTER :: P2
!   TYPE(NODE_T), POINTER :: P3
!   TYPE(NODE_T), POINTER :: P4
!   TYPE(NODE_T), POINTER :: P5
!   TYPE(NODE_T), POINTER :: P6
!
!   WRITE(*,*) 'Init'
!   CALL STACK_INIT( TS, 1000 )
!   WRITE(*,*) TS%RSP
!
!   CALL STACK_POP( TS, P1 )
!   WRITE(*,*) P1%I
!
!   CALL STACK_POP( TS, P2 )
!   WRITE(*,*) P2%I
!
!   CALL STACK_POP( TS, P3 )
!   WRITE(*,*) P3%I
!
!   CALL STACK_POP( TS, P4 )
!   WRITE(*,*) P4%I
!
!   CALL STACK_POP( TS, P5 )
!   WRITE(*,*) P5%I
!
!   CALL STACK_PUSH( TS, P5 )
!   WRITE(*,*) ASSOCIATED(P5)
!
!   CALL STACK_POP( TS, P6 )
!   WRITE(*,*) P6%I
!
!   CALL STACK_FREE( TS )
!   WRITE(*,*) 'Free'
!
! END PROGRAM test