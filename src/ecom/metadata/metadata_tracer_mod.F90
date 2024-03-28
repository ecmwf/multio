#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'metadata_tracer_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'METADATA_TRACER_MOD'
MODULE METADATA_TRACER_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

IMPLICIT NONE

! Default visibility
PRIVATE

! Module parameters
INTEGER, PARAMETER :: MAX_LINE = 512
INTEGER, PARAMETER :: MAX_LINE_LENGTH = 512


! Class used to trace all metadata events
TYPE :: METADATA_TRACER_T

  CHARACTER(LEN=MAX_LINE_LENGTH), DIMENSION(MAX_LINE) :: LINES_
  CHARACTER(LEN=MAX_LINE_LENGTH) :: FNAME_
  INTEGER(KIND=JPIB_K) :: LINE_IDX_ = -99

CONTAINS

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT           => MT_INIT
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: APPEND_TO_LINE => MT_APPEND_TO_LINE
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: ADVANCE_LINE   => MT_ADVANCE_LINE
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: COPY           => MT_COPY
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: DUMP           => MT_DUMP

END TYPE

! Whitelist of public symbols
PUBLIC :: METADATA_TRACER_T

CONTAINS

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MT_INIT'
SUBROUTINE MT_INIT(THIS, FNAME)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_TRACER_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: FNAME

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialise all the write buffer
  DO I = 1, MAX_LINE
    THIS%LINES_(I) = REPEAT(' ',MAX_LINE_LENGTH)
  END DO

  ! Initialise the line index
  THIS%LINE_IDX_ = 1

  ! Initialise the file name
  THIS%FNAME_ = FNAME

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MT_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MT_APPEND_TO_LINE'
SUBROUTINE MT_APPEND_TO_LINE(THIS, STRING)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_TRACER_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN) :: STRING

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Compute the length of the string
  LO = LEN_TRIM(THIS%LINES_(THIS%LINE_IDX_)) + 1
  HI = LO + LEN_TRIM(STRING) - 1
  PP_DEBUG_CRITICAL_COND_THROW( HI .GT. MAX_LINE_LENGTH, 1 )

  ! Concatenate string to the current line
  THIS%LINES_(THIS%LINE_IDX_)(LO:HI) = TRIM(STRING)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Concatenation exeed line length' )
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
END SUBROUTINE MT_APPEND_TO_LINE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MT_ADVANCE_LINE'
SUBROUTINE MT_ADVANCE_LINE(THIS)

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_TRACER_T), INTENT(INOUT) :: THIS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Move to the next line
  THIS%LINE_IDX_ = THIS%LINE_IDX_ + 1

  ! REset the next line
  THIS%LINES_(THIS%LINE_IDX_) = REPEAT(' ',MAX_LINE_LENGTH)

  ! Check if the idx exeed the number of lines
  PP_DEBUG_CRITICAL_COND_THROW(THIS%LINE_IDX_ .GT. MAX_LINE, 1)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Maximum number of lines exeeded' )
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

END SUBROUTINE MT_ADVANCE_LINE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MT_COPY'
SUBROUTINE MT_COPY(THIS, OTHER)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_TRACER_T), INTENT(INOUT) :: THIS
  CLASS(METADATA_TRACER_T), INTENT(IN)    :: OTHER

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! copy the content of 'other' to 'this'
  THIS%FNAME_ = OTHER%FNAME_
  DO I = 1, MAX_LINE
    THIS%LINES_(I) = REPEAT(' ',MAX_LINE_LENGTH)
    THIS%LINES_(I) = OTHER%LINES_(I)
  END DO
  THIS%LINE_IDX_ = OTHER%LINE_IDX_

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MT_COPY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MT_DUMP'
SUBROUTINE MT_DUMP(THIS)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_TRACER_T), INTENT(IN) :: THIS

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: UNIT
  INTEGER(KIND=JPIB_K) :: STAT


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Open the file
  OPEN( NEWUNIT=UNIT, FILE=TRIM(THIS%FNAME_), STATUS='UNKNOWN', ACTION='WRITE', FORM='FORMATTED', IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW(STAT.NE.0, 2)

  ! Write the content of the buffer
  DO I = 1, THIS%LINE_IDX_-1
    WRITE(UNIT,'(I4.4,A)') I, ' '//TRIM(THIS%LINES_(I))
  END DO

  ! Close the file
  CLOSE(UNIT,IOSTAT=STAT)
  PP_DEBUG_CRITICAL_COND_THROW(STAT.NE.0, 2)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to open the encoder trace file' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to close the encoder trace file' )
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

END SUBROUTINE MT_DUMP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE METADATA_TRACER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
