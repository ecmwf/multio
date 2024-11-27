!>
!> @file debug_mod.F90
!>
!> @brief A module for managing debugging error stacks.
!>
!> This module defines a data structure `DEBUG_T` that provides facilities for
!> managing error frames and messages in a linked list format. It includes methods
!> to initialize the debug structure, free the allocated memory, and add error frames
!> and messages to the stack. Additionally, it provides functionality to print the
!> entire error stack for debugging purposes.
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>
MODULE DEBUG_MOD

IMPLICIT NONE

  !> Default visibility of the module
  PRIVATE


  !>
  !> @brief Defines a node for storing error messages in a linked list.
  !>
  !> The `ERROR_MSG_NODE` type represents a single node in a linked list of error messages.
  !> Each node contains an allocatable `MESSAGE` string and a pointer `NEXT` to the next node in the list.
  !> This allows dynamic storage of error messages in a linked structure.
  !>
  TYPE :: ERROR_MSG_NODE

    !> Message to be printed
    CHARACTER(LEN=:), ALLOCATABLE :: MESSAGE

    !> Pointer to the next message in the list
    TYPE(ERROR_MSG_NODE), POINTER :: NEXT => NULL()

  END TYPE


  !>
  !> @brief Represents a node in a linked list of error frames.
  !>
  !> The `ERROR_FRAME_NODE` type is used to store detailed information
  !> about an error frame, including the file, section, procedure, line
  !> number, and additional error metadata. Each frame can also contain
  !> a linked list of error messages. The structure supports maintaining
  !> a stack of error frames via linked pointers.
  !>
  TYPE :: ERROR_FRAME_NODE

    !> File name where the error occurred
    CHARACTER(LEN=:), ALLOCATABLE :: FILENAME
    CHARACTER(LEN=:), ALLOCATABLE :: SECTION_TYPE
    CHARACTER(LEN=:), ALLOCATABLE :: SECTION_NAME
    CHARACTER(LEN=:), ALLOCATABLE :: PROCEDURE_TYPE
    CHARACTER(LEN=:), ALLOCATABLE :: PROCEDURE_NAME
    CHARACTER(LEN=:), ALLOCATABLE :: LINE
    CHARACTER(LEN=:), ALLOCATABLE :: ERRIDX
    CHARACTER(LEN=:), ALLOCATABLE :: THREAD_ID
    CHARACTER(LEN=:), ALLOCATABLE :: NUM_THREADS

    !> Messages list
    TYPE(ERROR_MSG_NODE), POINTER :: MSG_LIST_HEAD => NULL()
    TYPE(ERROR_MSG_NODE), POINTER :: MSG_LIST_TAIL => NULL()

    !> Pointer to the next frame in the stack
    TYPE(ERROR_FRAME_NODE), POINTER :: NEXT => NULL()

  END TYPE


  !>
  !> @brief Manages error frames and messages for debugging purposes.
  !>
  !> The `DEBUG_T` type provides functionality to manage a stack of error frames,
  !> each containing detailed error information and associated messages. It allows
  !> for error frames to be pushed to the stack, error messages to be added, and
  !> the entire error stack to be printed. The structure also includes methods
  !> for initializing and freeing the error stack.
  !>
  TYPE :: DEBUG_T

    !> Default visibility
    PRIVATE

    !> Error frames
    TYPE(ERROR_FRAME_NODE), POINTER :: FRAME_STACK => NULL()

  CONTAINS

    !> Initialize the DEBUG_T structure
    PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT => DEBUG_INIT

    !> Free the error stack (both frames and messages)
    PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => DEBUG_FREE

    !> Add an error frame to the stack
    PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PUSH_ERROR_FRAME => DEBUG_PUSH_ERROR_FRAME

    !> Add an error message to the current frame
    PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PUSH_ERROR_MSG => DEBUG_PUSH_ERROR_MSG

    !> TODO: Add more options to add messages to the frame (i.e. int message, float messge, etc.)

    !> Print all error frames and their messages
    PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRINT_ERROR_STACK => DEBUG_PRINT_ERROR_STACK

  END TYPE


  !> Whitelist of public symbols
  PUBLIC :: DEBUG_T

CONTAINS

!>
!> @brief Manages the stack of error frames and messages for debugging.
!>
!> The `DEBUG_T` type is responsible for maintaining a stack of error frames, each
!> of which may contain multiple error messages. It provides methods for managing
!> this stack, including pushing new error frames and messages, as well as printing
!> the entire error stack. The stack can be initialized and freed to manage memory
!> efficiently. This type is used in debugging workflows where detailed error
!> tracking is required.
!>
!> @param [inout] THIS The `DEBUG_T` object whose error stack will be initialized.
!>
SUBROUTINE DEBUG_INIT( THIS )
IMPLICIT NONE

  !> Dummy arguments
  CLASS(DEBUG_T), INTENT(INOUT) :: THIS

  !> Reset frame stack
  THIS%FRAME_STACK => NULL()

  !> Exit point
  RETURN

END SUBROUTINE DEBUG_INIT


!>
!> @brief Frees the error stack and all associated memory.
!>
!> This subroutine deallocates the entire error stack in the `DEBUG_T` structure.
!> It frees all error frames and their associated messages, ensuring no memory
!> leaks occur after the debugging process. After calling this subroutine, the
!> `FRAME_STACK` pointer is set to `NULL()`.
!>
!> @param [inout] THIS The `DEBUG_T` object whose error stack will be freed.
!>
SUBROUTINE DEBUG_FREE( THIS )
IMPLICIT NONE

  !> Dummy arguments
  CLASS(DEBUG_T), INTENT(INOUT) :: THIS

  !> Local variables
  TYPE(ERROR_FRAME_NODE), POINTER :: CURRENT_FRAME, NEXT_FRAME
  TYPE(ERROR_MSG_NODE), POINTER :: CURRENT_MSG, NEXT_MSG

  !> Set the current frame to the top of the stack
  CURRENT_FRAME => THIS%FRAME_STACK

  !> Traverse all frames
  DO WHILE (ASSOCIATED(CURRENT_FRAME))

    !> Free all strings in the current frame
    IF (ALLOCATED(CURRENT_FRAME%FILENAME))       DEALLOCATE(CURRENT_FRAME%FILENAME)
    IF (ALLOCATED(CURRENT_FRAME%SECTION_TYPE))   DEALLOCATE(CURRENT_FRAME%SECTION_TYPE)
    IF (ALLOCATED(CURRENT_FRAME%SECTION_NAME))   DEALLOCATE(CURRENT_FRAME%SECTION_NAME)
    IF (ALLOCATED(CURRENT_FRAME%PROCEDURE_TYPE)) DEALLOCATE(CURRENT_FRAME%PROCEDURE_TYPE)
    IF (ALLOCATED(CURRENT_FRAME%PROCEDURE_NAME)) DEALLOCATE(CURRENT_FRAME%PROCEDURE_NAME)
    IF (ALLOCATED(CURRENT_FRAME%LINE))           DEALLOCATE(CURRENT_FRAME%LINE)
    IF (ALLOCATED(CURRENT_FRAME%ERRIDX))         DEALLOCATE(CURRENT_FRAME%ERRIDX)
    IF (ALLOCATED(CURRENT_FRAME%THREAD_ID))      DEALLOCATE(CURRENT_FRAME%THREAD_ID)
    IF (ALLOCATED(CURRENT_FRAME%NUM_THREADS))    DEALLOCATE(CURRENT_FRAME%NUM_THREADS)
    NEXT_FRAME => CURRENT_FRAME%NEXT

    !> Free all messages in the current frame
    CURRENT_MSG => CURRENT_FRAME%MSG_LIST_HEAD
    DO WHILE (ASSOCIATED(CURRENT_MSG))
      IF (ALLOCATED(CURRENT_MSG%MESSAGE)) DEALLOCATE(CURRENT_MSG%MESSAGE)
      NEXT_MSG => CURRENT_MSG%NEXT
      DEALLOCATE(CURRENT_MSG)
      CURRENT_MSG => NEXT_MSG
    END DO
    CURRENT_FRAME%MSG_LIST_HEAD => NULL()
    CURRENT_FRAME%MSG_LIST_TAIL => NULL()

    !> Free the current frame
    DEALLOCATE(CURRENT_FRAME)
    CURRENT_FRAME => NEXT_FRAME

  END DO

  !> Reset frame stack
  THIS%FRAME_STACK => NULL()

  !> Exit point
  RETURN

END SUBROUTINE DEBUG_FREE


!>
!> @brief Adds an error frame to the error stack in the `DEBUG_T` structure.
!>
!> This subroutine creates a new error frame with the provided details (such as
!> file name, section type, procedure name, etc.) and pushes it onto the error stack
!> in the `DEBUG_T` object. This helps in tracking where an error occurred in
!> the code, including the specific procedure and line number.
!>
!> @param [inout] THIS The `DEBUG_T` object where the error frame will be added.
!> @param [in] FILENAME The name of the file where the error occurred.
!> @param [in] SECTION_TYPE The type of the section where the error occurred (e.g., subroutine, function).
!> @param [in] SECTION_NAME The name of the section where the error occurred.
!> @param [in] PROCEDURE_TYPE The type of the procedure (e.g., function, subroutine).
!> @param [in] PROCEDURE_NAME The name of the procedure where the error occurred.
!> @param [in] LINE The line number where the error occurred.
!> @param [in] ERRIDX The error index or code associated with the error.
SUBROUTINE DEBUG_PUSH_ERROR_FRAME( THIS, FILENAME, SECTION_TYPE, &
&          SECTION_NAME, PROCEDURE_TYPE, PROCEDURE_NAME, LINE, ERRIDX )

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

#if defined(_OPENMP)
  USE :: OMP_LIB, ONLY: OMP_GET_THREAD_NUM
  USE :: OMP_LIB, ONLY: OMP_NUM_THREADS
#endif

IMPLICIT NONE

  !> Dummy arguments
  CLASS(DEBUG_T),         INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN) :: FILENAME
  CHARACTER(LEN=*),       INTENT(IN) :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN) :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN) :: PROCEDURE_TYPE
  CHARACTER(LEN=*),       INTENT(IN) :: PROCEDURE_NAME
  INTEGER(KIND=JPIB_K),   INTENT(IN) :: LINE
  INTEGER(KIND=JPIB_K),   INTENT(IN) :: ERRIDX

  !> Local variables
  TYPE(ERROR_FRAME_NODE), POINTER :: NEW_FRAME
  CHARACTER(LEN=32) :: TMP
  INTEGER(JPIB_K) :: ALLOC_STATUS

  ! Create a new error frame node
  ALLOCATE(NEW_FRAME, STAT=ALLOC_STATUS)

  IF ( ALLOC_STATUS .EQ. 0 ) THEN
    !> Fill the frame header
    NEW_FRAME%FILENAME = FILENAME
    NEW_FRAME%SECTION_TYPE = SECTION_TYPE
    NEW_FRAME%SECTION_NAME = SECTION_NAME
    NEW_FRAME%PROCEDURE_TYPE = PROCEDURE_TYPE
    NEW_FRAME%PROCEDURE_NAME = PROCEDURE_NAME
    TMP = REPEAT( ' ', 32 )
    WRITE(TMP,*) LINE
    NEW_FRAME%LINE = TRIM(ADJUSTL(TMP))
    TMP = REPEAT( ' ', 32 )
    WRITE(TMP,*) ERRIDX
    NEW_FRAME%ERRIDX = TRIM(ADJUSTL(TMP))
    TMP = REPEAT( ' ', 32 )

#if defined(_OPENMP)
    WRITE(TMP,*) OMP_GET_THREAD_NUM()
#else
    WRITE(TMP,*) 0
#endif
    NEW_FRAME%THREAD_ID = TRIM(ADJUSTL(TMP))
    TMP = REPEAT( ' ', 32 )
#if defined(_OPENMP)
    WRITE(TMP,*) OMP_GET_NUM_THREADS()
#else
    WRITE(TMP,*) 1
#endif

    NEW_FRAME%NUM_THREADS = TRIM(ADJUSTL(TMP))

    !> Insert the new frame at the top of the frame stack
    NEW_FRAME%NEXT => THIS%FRAME_STACK
    THIS%FRAME_STACK => NEW_FRAME
  ENDIF

  !> Exit point
  RETURN

END SUBROUTINE DEBUG_PUSH_ERROR_FRAME


!>
!> @brief Adds an error message to the current error frame in the `DEBUG_T` structure.
!>
!> This subroutine pushes a custom error message to the most recent error frame
!> in the `DEBUG_T` object's error stack. Each error frame can store multiple
!> error messages to provide additional context about the error.
!>
!> @param [inout] THIS The `DEBUG_T` object where the error message will be added.
!> @param [in] MSG The error message to be added to the current frame.
!>
SUBROUTINE DEBUG_PUSH_ERROR_MSG( THIS, MSG )

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
IMPLICIT NONE

  !> Dummy arguments
  CLASS(DEBUG_T),   INTENT(INOUT) :: THIS
  CHARACTER(LEN=*), INTENT(IN)    :: MSG

  !> Local variables
  TYPE(ERROR_MSG_NODE),   POINTER :: NEW_MSG
  TYPE(ERROR_FRAME_NODE), POINTER :: CURRENT_FRAME
  INTEGER(JPIB_K) :: ALLOC_STATUS

  ! Check if there is a current frame
  CURRENT_FRAME => THIS%FRAME_STACK
  IF (.NOT. ASSOCIATED(CURRENT_FRAME)) THEN
    !> If there is no current frame, push an unknown frame to avoid errors
    CALL THIS%PUSH_ERROR_FRAME( 'UNKNOWN', 'UNKNOWN', 'UNKNOWN', &
&   'UNKNOWN', 'UNKNOWN', -1_JPIB_K, -1_JPIB_K )
  ENDIF

  !> Create a new message node
  ALLOCATE(NEW_MSG, STAT=ALLOC_STATUS)

  !> I am already in the error handler, theres not so much I can do in case of error
  IF ( ALLOC_STATUS .EQ. 0 ) THEN
    NEW_MSG%MESSAGE = MSG

    !> Insert the new message at the end of the message list
    IF ( .NOT.ASSOCIATED(CURRENT_FRAME%MSG_LIST_HEAD) ) THEN
      CURRENT_FRAME%MSG_LIST_HEAD => NEW_MSG
      CURRENT_FRAME%MSG_LIST_TAIL => NEW_MSG
    ELSE
      CURRENT_FRAME%MSG_LIST_TAIL%NEXT => NEW_MSG
      CURRENT_FRAME%MSG_LIST_TAIL => NEW_MSG
    ENDIF
  ENDIF

  !> Exit point
  RETURN

END SUBROUTINE DEBUG_PUSH_ERROR_MSG


!>
!> @brief Prints all error frames and their associated messages in the `DEBUG_T` structure.
!>
!> This subroutine traverses the error stack in the `DEBUG_T` object and prints
!> each error frame along with its corresponding error messages to the specified
!> output unit. This is useful for debugging purposes, as it provides a complete
!> overview of all recorded errors.
!>
!> @param [in] THIS The `DEBUG_T` object containing the error stack to be printed.
!> @param [in] UNIT The output unit where the error stack will be printed.
!>                  This can be a file unit or the standard output.
SUBROUTINE DEBUG_PRINT_ERROR_STACK( THIS, UNIT )

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

  !> Dummy arguments
  CLASS(DEBUG_T),       INTENT(IN) :: THIS
  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT

  !> Local variables
  TYPE(ERROR_FRAME_NODE), POINTER :: CURRENT_FRAME
  TYPE(ERROR_MSG_NODE), POINTER :: CURRENT_MSG
  INTEGER(JPIB_K) :: OFFSET
  INTEGER(JPIB_K) :: LLEN
  INTEGER(JPIB_K) :: CNT
  INTEGER(JPIB_K) :: LOC_UNIT
  INTEGER(JPIB_K) :: OPEN_STATUS
  CHARACTER(LEN=32) :: TMP
  LOGICAL :: IS_OPENED

  !> Set the current frame to the top of the stack
  CURRENT_FRAME => THIS%FRAME_STACK
  IF (.NOT. ASSOCIATED(CURRENT_FRAME)) THEN
    WRITE(*,*) 'NO ERROR FRAMES IN THE STACK.'
    RETURN
  END IF

  ! Check the unit status
  INQUIRE( UNIT=UNIT, OPENED=IS_OPENED )
  IF ( IS_OPENED ) THEN
   LOC_UNIT = UNIT
  ELSE
    OPEN( NEWUNIT=LOC_UNIT, FILE='ERROR_STACK.LOG', STATUS='UNKNOWN', &
&      ACTION='WRITE', FORM='FORMATTED', IOSTAT=OPEN_STATUS )
    IF ( OPEN_STATUS .NE. 0) THEN
      WRITE(*,*) 'UABLE TO FIND A VALID UNIT FOR THE ERROR STACK.'
      RETURN
    ENDIF
  END IF

  !> Print the error stack header
  WRITE(LOC_UNIT,*) 'ERROR STACK (MOST RECENT FIRST)'
  WRITE(LOC_UNIT,*) ' '

  !> Traverse all frames
  OFFSET=1
  CNT = 0
  LLEN=100
  DO WHILE (ASSOCIATED(CURRENT_FRAME))

    !> Print the current frame
    TMP=REPEAT(' ',32)
    WRITE(TMP,*, IOSTAT=OPEN_STATUS) CNT
    LLEN=LLEN-2

    WRITE(LOC_UNIT,'(A,A)', IOSTAT=OPEN_STATUS)         &
&     REPEAT(' ',OFFSET), REPEAT('-',LLEN)

    WRITE(LOC_UNIT,'(A,A,A)', IOSTAT=OPEN_STATUS)       &
&     REPEAT(' ',OFFSET), ' ** FRAME :: ', TRIM(ADJUSTL(TMP))

    WRITE(LOC_UNIT,'(A,A,A,A)', IOSTAT=OPEN_STATUS)     &
&     REPEAT(' ',OFFSET), REPEAT('-',LLEN)

    IF ( ALLOCATED(CURRENT_FRAME%FILENAME) ) THEN
    WRITE(LOC_UNIT,'(A,A,A,A,A,A)', IOSTAT=OPEN_STATUS) &
&     REPEAT(' ',OFFSET), ' - FILE         :: "', &
&     TRIM(ADJUSTL(CURRENT_FRAME%FILENAME)), '"'
    ENDIF

    IF ( ALLOCATED(CURRENT_FRAME%SECTION_TYPE) .AND. &
&        ALLOCATED(CURRENT_FRAME%SECTION_NAME)) THEN
      WRITE(LOC_UNIT,'(A,A,A,A,A,A)', IOSTAT=OPEN_STATUS) &
&       REPEAT(' ',OFFSET), ' - SECTION      :: "', &
&       TRIM(ADJUSTL(CURRENT_FRAME%SECTION_TYPE)),  &
&       '::', TRIM(ADJUSTL(CURRENT_FRAME%SECTION_NAME)), '"'
    ENDIF

    IF ( ALLOCATED(CURRENT_FRAME%PROCEDURE_TYPE) .AND. &
         ALLOCATED(CURRENT_FRAME%PROCEDURE_NAME) ) THEN
      WRITE(LOC_UNIT,'(A,A,A,A,A,A)', IOSTAT=OPEN_STATUS) &
&       REPEAT(' ',OFFSET), ' - PROCEDURE    :: "', &
&       TRIM(ADJUSTL(CURRENT_FRAME%PROCEDURE_TYPE)), &
&       '::', TRIM(ADJUSTL(CURRENT_FRAME%PROCEDURE_NAME)), '"'
    ENDIF

    IF ( ALLOCATED(CURRENT_FRAME%LINE) ) THEN
      WRITE(LOC_UNIT,'(A,A,A)', IOSTAT=OPEN_STATUS) &
&        REPEAT(' ',OFFSET), ' - LINE         :: ', &
&        CURRENT_FRAME%LINE
    ENDIF

    IF ( ALLOCATED(CURRENT_FRAME%ERRIDX) ) THEN
      WRITE(LOC_UNIT,'(A,A,A)', IOSTAT=OPEN_STATUS) &
&        REPEAT(' ',OFFSET), ' - ERROR INDEX  :: ', &
&        CURRENT_FRAME%ERRIDX
    ENDIF

    IF ( ALLOCATED(CURRENT_FRAME%THREAD_ID) ) THEN
      WRITE(LOC_UNIT,'(A,A,A)', IOSTAT=OPEN_STATUS) &
&        REPEAT(' ',OFFSET), ' - THREAD ID.   :: ', &
&        CURRENT_FRAME%THREAD_ID
    ENDIF

    IF ( ALLOCATED(CURRENT_FRAME%NUM_THREADS) ) THEN
      WRITE(LOC_UNIT,'(A,A,A)', IOSTAT=OPEN_STATUS) &
&        REPEAT(' ',OFFSET), ' - NUM. THREADS :: ', &
&        CURRENT_FRAME%NUM_THREADS
    ENDIF


    WRITE(LOC_UNIT,'(A,A)', IOSTAT=OPEN_STATUS)   &
&      REPEAT(' ',OFFSET), ' ** MESSAGES:'

    !> Print all messages in the current frame
    CURRENT_MSG => CURRENT_FRAME%MSG_LIST_HEAD
    DO WHILE (ASSOCIATED(CURRENT_MSG))
      IF ( ALLOCATED(CURRENT_MSG%MESSAGE) ) THEN
        WRITE(LOC_UNIT,'(A,A,A)', IOSTAT=OPEN_STATUS) &
&          REPEAT(' ',OFFSET), "    - ", CURRENT_MSG%MESSAGE
      ENDIF
      CURRENT_MSG => CURRENT_MSG%NEXT

    END DO
    WRITE(LOC_UNIT,*, IOSTAT=OPEN_STATUS) ' '

    !> Move to the next frame
    CURRENT_FRAME => CURRENT_FRAME%NEXT

    !> Update counters
    OFFSET = OFFSET + 2
    CNT = CNT + 1
  END DO

  !> If necessary close the file
  IF ( .NOT.IS_OPENED ) THEN
    CLOSE(UNIT=LOC_UNIT)
  ENDIF

  !> Exit point
  RETURN

END SUBROUTINE DEBUG_PRINT_ERROR_STACK

END MODULE DEBUG_MOD
