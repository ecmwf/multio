! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'configuration_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'CONFIGURATION_UTILS_MOD'
MODULE CONFIGURATION_UTILS_MOD

  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRM_K

IMPLICIT NONE

!> Default symbols visibility
PRIVATE

TYPE :: INT_CONTAINER_T
  INTEGER(KIND=JPIB_K) :: PAR
  TYPE(INT_CONTAINER_T), POINTER :: NEXT=>NULL()
END TYPE

TYPE :: REAL64_CONTAINER_T
  REAL(KIND=JPRD_K) :: PAR
  TYPE(REAL64_CONTAINER_T), POINTER :: NEXT=>NULL()
END TYPE

TYPE :: REAL32_CONTAINER_T
  REAL(KIND=JPRM_K) :: PAR
  TYPE(REAL32_CONTAINER_T), POINTER :: NEXT=>NULL()
END TYPE

! Exposed interface
INTERFACE STRING_TO_REAL
  MODULE PROCEDURE STRING_TO_REAL32
  MODULE PROCEDURE STRING_TO_REAL64
END INTERFACE


INTERFACE STRING_TO_REAL_ARRAY
  MODULE PROCEDURE STRING_TO_REAL32_ARRAY
  MODULE PROCEDURE STRING_TO_REAL64_ARRAY
END INTERFACE

! Whitelist of public symbols
PUBLIC :: STRING_TO_REAL
PUBLIC :: STRING_TO_REAL_ARRAY
PUBLIC :: STRING_IS_INTEGER
PUBLIC :: STRING_IS_INTEGER_RANGE
PUBLIC :: STRING_IS_INTEGER_RANGE_BY
PUBLIC :: STRING_TO_INTEGER
PUBLIC :: STRING_TO_INTEGER_ARRAY
PUBLIC :: STRING_TO_INTEGER_RANGE
PUBLIC :: STRING_TO_INTEGER_RANGE_BY

CONTAINS

!>
!> @brief Checks if a string contains an integer.
!>
!> @section interface
!> @param [in] STRING   Input string representing the range (e.g. "1:10").
!> @param [out] MATCH   True if the string has the format "LO:HI".
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [PROCEDURE] READ_INTEGER_PATTERNS
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see STRING_IS_INTEGER_RANGE
!> @see STRING_IS_INTEGER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRING_IS_INTEGER'
PP_THREAD_SAFE FUNCTION STRING_IS_INTEGER( STRING, MATCH, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*), INTENT(IN)    :: STRING
  LOGICAL,          INTENT(OUT)   :: MATCH
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: COUNT
  LOGICAL :: LTMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_ERROR=1_JPIB_K

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

  ! Parse the string
  PP_TRYCALL(ERRFLAG_PARSE_ERROR) READ_INTEGER_PATTERNS( STRING, LTMP, COUNT, HOOKS )

  ! Generate the match flag
  IF ( LTMP .AND. COUNT .NE. 1 ) THEN
    MATCH = .FALSE.
  ELSE
    MATCH = LTMP
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_PARSE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to parse string: '//TRIM(ADJUSTL(STRING)) )
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

  ! Exit point on error
  RETURN

END FUNCTION STRING_IS_INTEGER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Check if a string contains a range of integers with a specified step.
!>
!> This function checks if a string represents a range in the form of "LO:HI"
!>
!> @section interface
!> @param [in] STRING   Input string representing the range (e.g. "1:10").
!> @param [out] MATCH   True if the string has the format "LO:HI".
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [PROCEDURE] READ_INTEGER_PATTERNS
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see STRING_IS_INTEGER_RANGE
!> @see STRING_IS_INTEGER
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'STRING_IS_INTEGER_RANGE'
PP_THREAD_SAFE FUNCTION STRING_IS_INTEGER_RANGE( STRING, MATCH, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*), INTENT(IN)    :: STRING
  LOGICAL,          INTENT(OUT)   :: MATCH
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: COUNT
  INTEGER(KIND=JPIB_K), DIMENSION(2) :: TMP
  LOGICAL :: LTMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_ERROR=1_JPIB_K

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

  ! Parse the string
  PP_TRYCALL(ERRFLAG_PARSE_ERROR) READ_INTEGER_PATTERNS( STRING, LTMP, COUNT, HOOKS, VALUES=TMP )

  ! Generate the match flag
  MATCH = LTMP

  IF ( MATCH .AND. COUNT .NE. 2 ) THEN
    MATCH = .FALSE.
  ENDIF

  IF ( MATCH .AND. TMP(1).GT.TMP(2) ) THEN
    MATCH = .FALSE.
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_PARSE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to parse string: '//TRIM(ADJUSTL(STRING)) )
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

  ! Exit point on error
  RETURN

END FUNCTION STRING_IS_INTEGER_RANGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Check if a string contains a range of integers with a specified step.
!>
!> This function checks if a string represents a range in the form of "LO:HI:BY"
!>
!> @section interface
!> @param [in] STRING   Input string representing the range (e.g. "1:10:2").
!> @param [out] MATCH   True if the string has the format "LO:HI:BY".
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [PROCEDURE] READ_INTEGER_PATTERNS
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see STRING_IS_INTEGER_RANGE
!> @see STRING_IS_INTEGER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRING_IS_INTEGER_RANGE_BY'
PP_THREAD_SAFE FUNCTION STRING_IS_INTEGER_RANGE_BY( STRING, MATCH, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*), INTENT(IN)    :: STRING
  LOGICAL,          INTENT(OUT)   :: MATCH
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: COUNT
  INTEGER(KIND=JPIB_K), DIMENSION(3) :: TMP
  LOGICAL :: LTMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_ERROR=1_JPIB_K

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

  ! Parse the string
  PP_TRYCALL(ERRFLAG_PARSE_ERROR) READ_INTEGER_PATTERNS( STRING, LTMP, COUNT, HOOKS, VALUES=TMP )

  ! Generate the match flag
  MATCH = LTMP

  IF ( MATCH .AND. COUNT .NE. 3 ) THEN
    MATCH = .FALSE.
  ENDIF

  IF ( MATCH .AND. TMP(1).GT.TMP(2) ) THEN
    MATCH = .FALSE.
  ENDIF

  IF ( MATCH .AND. TMP(3).LT.0 ) THEN
    MATCH = .FALSE.
  ENDIF

  IF ( MATCH .AND.TMP(3).GT.(TMP(2)-TMP(1)) ) THEN
    MATCH = .FALSE.
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_PARSE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to parse string: '//TRIM(ADJUSTL(STRING)) )
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

  ! Exit point on error
  RETURN

END FUNCTION STRING_IS_INTEGER_RANGE_BY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Converts a string to an integer value.
!>
!> This function parses the input `STRING` and converts it to an integer value (`VALUE`).
!> If the conversion fails, the function returns an error code.
!>
!> @section interface
!> @param [in] STRING   Input string representing the integer value.
!> @param [out] VALUE   The resulting integer value.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [PROCEDURE] READ_INTEGER_PATTERNS
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see STRING_TO_INTEGER_RANGE
!> @see STRING_IS_INTEGER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRING_TO_INTEGER'
PP_THREAD_SAFE FUNCTION STRING_TO_INTEGER( STRING, VALUE, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: STRING
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: COUNT
  INTEGER(KIND=JPIB_K), DIMENSION(1) :: TMP
  LOGICAL :: MATCH

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_ERROR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_MATCH=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_COUNT=3_JPIB_K

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

  ! Parse the string
  PP_TRYCALL(ERRFLAG_PARSE_ERROR) READ_INTEGER_PATTERNS( STRING, MATCH, COUNT, HOOKS, VALUES=TMP )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.MATCH, ERRFLAG_NO_MATCH )
  PP_DEBUG_CRITICAL_COND_THROW( COUNT.NE.1, ERRFLAG_WRONG_COUNT )

  ! Assign the values
  VALUE=TMP(1)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    CHARACTER(LEN=32) :: TMPSTR1

    ! Initializa a new error frame
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_PARSE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to parse string: '//TRIM(ADJUSTL(STRING)) )
    CASE (ERRFLAG_NO_MATCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'No match between string and expected pattern: '//TRIM(ADJUSTL(STRING)) )
    CASE (ERRFLAG_WRONG_COUNT)
      TMPSTR1 = REPEAT(' ', 32)
      WRITE(TMPSTR1, '(I32)') COUNT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong count:  expected=1, got='//TRIM(ADJUSTL(TMPSTR1)) )
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

  ! Exit point on error
  RETURN

END FUNCTION STRING_TO_INTEGER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Converts a string to an integer value.
!>
!> This function parses the input `STRING` and converts it to an integer value (`VALUE`).
!> If the conversion fails, the function returns an error code.
!>
!> @section interface
!> @param [in] STRING   Input string representing the integer value.
!> @param [out] VALUE   The resulting integer value.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [PROCEDURE] READ_INTEGER_PATTERNS
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see STRING_TO_INTEGER_RANGE
!> @see STRING_IS_INTEGER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRING_TO_INTEGER_ARRAY'
PP_THREAD_SAFE FUNCTION STRING_TO_INTEGER_ARRAY( STRING, VALUE, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),                                INTENT(IN)    :: STRING
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),                                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: POS
  CHARACTER(LEN=1) :: S

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_ERROR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(VALUE), ERRFLAG_ALREADY_ALLOCATED )

  ! Parse the string
  POS=1
  PP_TRYCALL(ERRFLAG_PARSE_ERROR) PARSE_INTEGER_ARRAY( STRING, POS, ']', VALUE, S, HOOKS )


  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(VALUE), ERRFLAG_NOT_ALLOCATED )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    CHARACTER(LEN=32) :: TMPSTR1

    ! Initializa a new error frame
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_PARSE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to parse string: '//TRIM(ADJUSTL(STRING)) )
    CASE (ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output array already allocated' )
    CASE (ERRFLAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output array not allocated after parsing' )
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

  ! Exit point on error
  RETURN

END FUNCTION STRING_TO_INTEGER_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRING_TO_REAL64_ARRAY'
PP_THREAD_SAFE FUNCTION STRING_TO_REAL64_ARRAY( STRING, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                             INTENT(IN)    :: STRING
  REAL(KIND=JPRD_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),                                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: POS
  CHARACTER(LEN=1) :: S

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_ERROR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(VALUE), ERRFLAG_ALREADY_ALLOCATED )

  ! Parse the string
  POS=1
  PP_TRYCALL(ERRFLAG_PARSE_ERROR) PARSE_REAL64_ARRAY( STRING, POS, ']', VALUE, S, HOOKS )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(VALUE), ERRFLAG_NOT_ALLOCATED )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    CHARACTER(LEN=32) :: TMPSTR1

    ! Initializa a new error frame
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_PARSE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to parse string: '//TRIM(ADJUSTL(STRING)) )
    CASE (ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output array already allocated' )
    CASE (ERRFLAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output array not allocated after parsing' )
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

  ! Exit point on error
  RETURN

END FUNCTION STRING_TO_REAL64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRING_TO_REAL32_ARRAY'
PP_THREAD_SAFE FUNCTION STRING_TO_REAL32_ARRAY( STRING, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                             INTENT(IN)    :: STRING
  REAL(KIND=JPRM_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),                                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: POS
  CHARACTER(LEN=1) :: S

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_ERROR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(VALUE), ERRFLAG_ALREADY_ALLOCATED )

  ! Parse the string
  POS=1
  PP_TRYCALL(ERRFLAG_PARSE_ERROR) PARSE_REAL32_ARRAY( STRING, POS, ']', VALUE, S, HOOKS )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(VALUE), ERRFLAG_NOT_ALLOCATED )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    CHARACTER(LEN=32) :: TMPSTR1

    ! Initializa a new error frame
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_PARSE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to parse string: '//TRIM(ADJUSTL(STRING)) )
    CASE (ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output array already allocated' )
    CASE (ERRFLAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output array not allocated after parsing' )
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

  ! Exit point on error
  RETURN

END FUNCTION STRING_TO_REAL32_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRING_TO_REAL32'
PP_THREAD_SAFE FUNCTION STRING_TO_REAL32( STRING, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),  INTENT(IN)    :: STRING
  REAL(KIND=JPRM_K), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local varaibles
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_ERROR=1_JPIB_K

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

  ! Parse the string
  READ(STRING,*,IOSTAT=WRITE_STATUS) VALUE
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_PARSE_ERROR )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    CHARACTER(LEN=32) :: TMPSTR1

    ! Initializa a new error frame
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_PARSE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to parse string: '//TRIM(ADJUSTL(STRING)) )
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

  ! Exit point on error
  RETURN

END FUNCTION STRING_TO_REAL32
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRING_TO_REAL64'
PP_THREAD_SAFE FUNCTION STRING_TO_REAL64( STRING, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),  INTENT(IN)    :: STRING
  REAL(KIND=JPRD_K), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local varaibles
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_ERROR=1_JPIB_K

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

  ! Parse the string
  READ(STRING,*,IOSTAT=WRITE_STATUS) VALUE
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_PARSE_ERROR )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    CHARACTER(LEN=32) :: TMPSTR1

    ! Initializa a new error frame
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_PARSE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to parse string: '//TRIM(ADJUSTL(STRING)) )
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

  ! Exit point on error
  RETURN

END FUNCTION STRING_TO_REAL64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Converts a string into a range of integers.
!>
!> This function parses a string representing a range in the form of "LO:HI" and
!> converts it into integers `LO` and `HI`.
!>
!> @section interface
!> @param [in] STRING   Input string representing the range (e.g., "1:10").
!> @param [out] LO      The lower bound of the range (integer).
!> @param [out] HI      The upper bound of the range (integer).
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsubsection module dependencies
!>   - @dependency [PROCEDURE] READ_INTEGER_PATTERNS
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see STRING_TO_INTEGER_RANGE_BY
!> @see STRING_IS_INTEGER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRING_TO_INTEGER_RANGE'
PP_THREAD_SAFE FUNCTION STRING_TO_INTEGER_RANGE( STRING, LO, HI, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: STRING
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: LO
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: HI
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: COUNT
  INTEGER(KIND=JPIB_K), DIMENSION(2) :: TMP
  LOGICAL :: MATCH

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_ERROR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_MATCH=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_COUNT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_RANGE=4_JPIB_K

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

  ! Parse the string
  PP_TRYCALL(ERRFLAG_PARSE_ERROR) READ_INTEGER_PATTERNS( STRING, MATCH, COUNT, HOOKS, VALUES=TMP )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.MATCH, ERRFLAG_NO_MATCH )
  PP_DEBUG_CRITICAL_COND_THROW( COUNT.NE.2, ERRFLAG_WRONG_COUNT )
  PP_DEBUG_CRITICAL_COND_THROW( TMP(1).GT.TMP(2), ERRFLAG_WRONG_RANGE )

  ! Assign the values
  LO=TMP(1)
  HI=TMP(2)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    CHARACTER(LEN=32) :: TMPSTR1
    CHARACTER(LEN=32) :: TMPSTR2

    ! Initializa a new error frame
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_PARSE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to parse string: '//TRIM(ADJUSTL(STRING)) )
    CASE (ERRFLAG_NO_MATCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'No match between string and expected pattern: '//TRIM(ADJUSTL(STRING)) )
    CASE (ERRFLAG_WRONG_COUNT)
      TMPSTR1 = REPEAT(' ', 32)
      WRITE(TMPSTR1, '(I32)') COUNT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong count:  expected=2, got='//TRIM(ADJUSTL(TMPSTR1)) )
    CASE (ERRFLAG_WRONG_RANGE)
      TMPSTR1 = REPEAT(' ', 32)
      WRITE(TMPSTR1, '(I32)') TMP(1)
      TMPSTR2 = REPEAT(' ', 32)
      WRITE(TMPSTR2, '(I32)') TMP(2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong range lower bound bigger than the upper bound: lb='//TRIM(ADJUSTL(TMPSTR1))//', ub='//TRIM(ADJUSTL(TMPSTR2)) )
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

  ! Exit point on error
  RETURN

END FUNCTION STRING_TO_INTEGER_RANGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Converts a string into a range of integers with a specified step.
!>
!> This function parses a string representing a range in the form of "LO:HI:BY" and
!> converts it into integers `LO`, `HI`, and `BY`.
!>
!> @section interface
!> @param [in] STRING   Input string representing the range (e.g. "1:10:2" ).
!> @param [out] LO      The lower bound of the range (integer).
!> @param [out] HI      The upper bound of the range (integer).
!> @param [out] BY      The step size between the lower and upper bounds (integer).
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [PROCEDURE] READ_INTEGER_PATTERNS
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see STRING_TO_INTEGER_RANGE
!> @see STRING_TO_INTEGER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRING_TO_INTEGER_RANGE_BY'
PP_THREAD_SAFE FUNCTION STRING_TO_INTEGER_RANGE_BY( STRING, LO, HI, BY, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: STRING
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: LO
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: HI
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: BY
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: COUNT
  INTEGER(KIND=JPIB_K), DIMENSION(3) :: TMP
  LOGICAL :: MATCH

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_ERROR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_MATCH=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_COUNT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_RANGE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STEP_SIGN=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STEP_SIZE=6_JPIB_K

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

  ! Parse the string
  PP_TRYCALL(ERRFLAG_PARSE_ERROR) READ_INTEGER_PATTERNS( STRING, MATCH, COUNT, HOOKS, VALUES=TMP )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.MATCH, ERRFLAG_NO_MATCH )
  PP_DEBUG_CRITICAL_COND_THROW( COUNT.NE.3, ERRFLAG_WRONG_COUNT )
  PP_DEBUG_CRITICAL_COND_THROW( TMP(1).GT.TMP(2), ERRFLAG_WRONG_RANGE )
  PP_DEBUG_CRITICAL_COND_THROW( TMP(3).LT.0, ERRFLAG_STEP_SIGN )
  PP_DEBUG_CRITICAL_COND_THROW( TMP(3).GT.(TMP(2)-TMP(1)), ERRFLAG_STEP_SIZE )

  ! Assign the values
  LO=TMP(1)
  HI=TMP(2)
  BY=TMP(3)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    CHARACTER(LEN=32) :: TMPSTR1
    CHARACTER(LEN=32) :: TMPSTR2
    CHARACTER(LEN=32) :: TMPSTR3

    ! Initializa a new error frame
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_PARSE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to parse string: '//TRIM(ADJUSTL(STRING)) )
    CASE (ERRFLAG_NO_MATCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'No match between string and expected pattern: '//TRIM(ADJUSTL(STRING)) )
    CASE (ERRFLAG_WRONG_COUNT)
      TMPSTR1 = REPEAT(' ', 32)
      WRITE(TMPSTR1, '(I32)') COUNT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong count:  expected=3, got='//TRIM(ADJUSTL(TMPSTR1)) )
    CASE (ERRFLAG_WRONG_RANGE)
      TMPSTR1 = REPEAT(' ', 32)
      WRITE(TMPSTR1, '(I32)') TMP(1)
      TMPSTR2 = REPEAT(' ', 32)
      WRITE(TMPSTR2, '(I32)') TMP(2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong range lower bound bigger than the upper bound: lb='//TRIM(ADJUSTL(TMPSTR1))//', ub='//TRIM(ADJUSTL(TMPSTR2)) )
    CASE (ERRFLAG_STEP_SIGN)
      TMPSTR1 = REPEAT(' ', 32)
      WRITE(TMPSTR1, '(I32)') TMP(3)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Step must be greater than 0: '//TRIM(ADJUSTL(TMPSTR1)) )
    CASE (ERRFLAG_STEP_SIZE)
      TMPSTR1 = REPEAT(' ', 32)
      WRITE(TMPSTR1, '(I32)') TMP(1)
      TMPSTR2 = REPEAT(' ', 32)
      WRITE(TMPSTR2, '(I32)') TMP(2)
      TMPSTR3 = REPEAT(' ', 32)
      WRITE(TMPSTR3, '(I32)') TMP(3)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Step size too big: lb='//TRIM(ADJUSTL(TMPSTR1))//', ub='//TRIM(ADJUSTL(TMPSTR2))//', step='//TRIM(ADJUSTL(TMPSTR3)) )
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

  ! Exit point on error
  RETURN

END FUNCTION STRING_TO_INTEGER_RANGE_BY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Reads and validates integer patterns in a string.
!>
!> This function reads and validates integer patterns from the input `STRING`. It returns
!> the count of valid integers found and optionally populates the `VALUES` array. It uses
!> a state machine to process the string, checking for valid separators and number formats.
!>
!> @section interface
!> @param [in]  STRING  Input string containing integer patterns to be parsed.
!> @param [out] MATCH   Logical flag set to `.TRUE.` if valid patterns are found, `.FALSE.` otherwise.
!> @param [out] COUNT   Number of valid integer patterns found in the `STRING`.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!> @param [out] VALUES  Optional array that will be populated with the parsed integers.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [PROCEDURE] READ_INTEGER
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] "DATAKINDS_DEF_MOD::JPIB_K"
!>
!> @subsection special dependencies
!>   - @dependency [*] "OM_DEBUG_MOD::*"
!>   - @dependency [*] "OM_TRACE_MOD::*"
!>   - @dependency [*] "OM_LOG_MOD::*"
!>
!> @see READ_INTEGER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_INTEGER_PATTERNS'
PP_THREAD_SAFE FUNCTION  READ_INTEGER_PATTERNS( INP_STRING, MATCH, COUNT, HOOKS, VALUES ) RESULT(RET)

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
  CHARACTER(LEN=*),                             INTENT(IN)    :: INP_STRING
  LOGICAL,                                      INTENT(OUT)   :: MATCH
  INTEGER(KIND=JPIB_K),                         INTENT(OUT)   :: COUNT
  TYPE(HOOKS_T),                                INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K), DIMENSION(:), OPTIONAL, INTENT(OUT)   :: VALUES

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=LEN_TRIM(INP_STRING)) :: STRING
  INTEGER(KIND=JPIB_K) :: SZ
  INTEGER(KIND=JPIB_K) :: GLB_IDX
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  INTEGER(KIND=JPIB_K) :: TMP
  INTEGER(KIND=JPIB_K) :: OLD_STATE
  INTEGER(KIND=JPIB_K) :: STATE
  INTEGER(KIND=JPIB_K) :: ERR_IDX
  CHARACTER(LEN=1) :: C
  LOGICAL :: LOOP
  LOGICAL :: THROW

  ! Error Codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OK=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CHAR_IN_CHECK_FIRST_NUM=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CHAR_IN_SEARCH_SEPARATOR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CHAR_IN_SEPARATOR=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OOB_IN_OUT_VALUES=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNEXPECTED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FAILED_READ_INTEGER=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GLB_IDX_LOWER_THAN_1=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GLB_IDX_BIGGER_THAN_STRING=8_JPIB_K

  ! States
  INTEGER(KIND=JPIB_K), PARAMETER :: ENTER_STATE=1
  INTEGER(KIND=JPIB_K), PARAMETER :: CHECK_PREFIX_STATE=2
  INTEGER(KIND=JPIB_K), PARAMETER :: CHECK_FIRST_NUMBER_NO_PULL_STATE=3
  INTEGER(KIND=JPIB_K), PARAMETER :: CHECK_FIRST_NUMBER_STATE=4
  INTEGER(KIND=JPIB_K), PARAMETER :: SEARCH_SEPARATOR_STATE=5
  INTEGER(KIND=JPIB_K), PARAMETER :: VALID_VALUE_STATE=6
  INTEGER(KIND=JPIB_K), PARAMETER :: LAST_VALID_VALUE_STATE=7
  INTEGER(KIND=JPIB_K), PARAMETER :: EXIT_STATE=8
  INTEGER(KIND=JPIB_K), PARAMETER :: ERROR_STATE=9
  INTEGER(KIND=JPIB_K), PARAMETER :: INVALID_STATE=666

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for loging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Initialization
  STRING = REPEAT(' ', LEN_TRIM(INP_STRING))
  SZ = LEN_TRIM(ADJUSTL(INP_STRING))
  STRING(1:SZ) = TRIM(ADJUSTL(INP_STRING))
  MATCH = .FALSE.
  COUNT = 0
  GLB_IDX = 1
  ERR_IDX = 0
  LO = 0
  HI = 0
  LOOP = .TRUE.
  THROW=.FALSE.
  OLD_STATE = INVALID_STATE
  STATE = ENTER_STATE
  StatesLoop: DO WHILE (LOOP)

    ! Save the old state
    OLD_STATE = STATE

    ! Check for out of bounds
    IF ( GLB_IDX .GT. LEN_TRIM(STRING) ) THEN
      ERR_IDX = ERRFLAG_GLB_IDX_BIGGER_THAN_STRING
      STATE=ERROR_STATE
    ELSEIF ( GLB_IDX .LT. 1 ) THEN
      ERR_IDX = ERRFLAG_GLB_IDX_LOWER_THAN_1
      STATE=ERROR_STATE
    ENDIF


    !> State machine
    SELECT CASE (STATE)

    !> Enter in the state machine
    CASE (ENTER_STATE)

      COUNT = 0
      GLB_IDX = 1
      STATE = CHECK_PREFIX_STATE

    !> Check the prefix (it can be '+', '-' or nothing)
    CASE (CHECK_PREFIX_STATE)

      IF ( GLB_IDX .LT. 1 ) THEN
        ERR_IDX = ERRFLAG_GLB_IDX_LOWER_THAN_1
        STATE=ERROR_STATE
        CYCLE StatesLoop
      ENDIF
      IF ( GLB_IDX .GT. LEN_TRIM(STRING) ) THEN
        ERR_IDX = ERRFLAG_GLB_IDX_BIGGER_THAN_STRING
        STATE=ERROR_STATE
        CYCLE StatesLoop
      ENDIF
      C = STRING(GLB_IDX:GLB_IDX)
      LO = GLB_IDX
      IF ( (C.NE.'+') .AND. (C.NE.'-') ) THEN
        STATE = CHECK_FIRST_NUMBER_NO_PULL_STATE
      ELSE
        STATE = CHECK_FIRST_NUMBER_STATE
      ENDIF

    !> Check the first digit of the integer (it can be 1..9)
    CASE (CHECK_FIRST_NUMBER_NO_PULL_STATE)

      IF ( GLB_IDX .LT. 1 ) THEN
        ERR_IDX = ERRFLAG_GLB_IDX_LOWER_THAN_1
        STATE=ERROR_STATE
        CYCLE StatesLoop
      ENDIF
      IF ( GLB_IDX .GT. LEN_TRIM(STRING) ) THEN
        ERR_IDX = ERRFLAG_GLB_IDX_BIGGER_THAN_STRING
        STATE=ERROR_STATE
        CYCLE StatesLoop
      ENDIF
      C = STRING(GLB_IDX:GLB_IDX)
      IF ( GLB_IDX.EQ.LEN_TRIM(STRING) .AND. (C.GE.'0') .AND. (C.LE.'9') ) THEN
        STATE = LAST_VALID_VALUE_STATE
      ELSEIF ( GLB_IDX.LT.LEN_TRIM(STRING) .AND. (C.GE.'1') .AND. (C.LE.'9') ) THEN
        STATE = SEARCH_SEPARATOR_STATE
      ELSE
        ERR_IDX = ERRFLAG_INVALID_CHAR_IN_CHECK_FIRST_NUM
        STATE=ERROR_STATE
      ENDIF

    !> Check the first digit of the integer (it can be 1..9)
    CASE (CHECK_FIRST_NUMBER_STATE)

      GLB_IDX = GLB_IDX + 1
      IF ( GLB_IDX .LT. 1 ) THEN
        ERR_IDX = ERRFLAG_GLB_IDX_LOWER_THAN_1
        STATE=ERROR_STATE
        CYCLE StatesLoop
      ENDIF
      IF ( GLB_IDX .GT. LEN_TRIM(STRING) ) THEN
        ERR_IDX = ERRFLAG_GLB_IDX_BIGGER_THAN_STRING
        STATE=ERROR_STATE
        CYCLE StatesLoop
      ENDIF
      C = STRING(GLB_IDX:GLB_IDX)
      IF ( GLB_IDX.EQ.LEN_TRIM(STRING) .AND. (C.GE.'0') .AND. (C.LE.'9') ) THEN
        STATE = LAST_VALID_VALUE_STATE
      ELSEIF ( GLB_IDX.LT.LEN_TRIM(STRING) .AND. (C.GE.'1') .AND. (C.LE.'9') ) THEN
        STATE = SEARCH_SEPARATOR_STATE
      ELSE
        ERR_IDX = ERRFLAG_INVALID_CHAR_IN_CHECK_FIRST_NUM
        STATE=ERROR_STATE
      ENDIF

    !> Search the separator (loop until a separator is found valid characters are 0..9)
    CASE (SEARCH_SEPARATOR_STATE)

      GLB_IDX = GLB_IDX + 1
      IF ( GLB_IDX .LT. 1 ) THEN
        ERR_IDX = ERRFLAG_GLB_IDX_LOWER_THAN_1
        STATE=ERROR_STATE
        CYCLE StatesLoop
      ENDIF
      IF ( GLB_IDX .GT. LEN_TRIM(STRING) ) THEN
        ERR_IDX = ERRFLAG_GLB_IDX_BIGGER_THAN_STRING
        STATE=ERROR_STATE
        CYCLE StatesLoop
      ENDIF
      C = STRING(GLB_IDX:GLB_IDX)
      IF ( (C.GE.'0') .AND. (C.LE.'9') ) THEN
        IF (GLB_IDX.EQ.LEN_TRIM(STRING)) THEN
          STATE = LAST_VALID_VALUE_STATE
        ELSE
          STATE = SEARCH_SEPARATOR_STATE
        ENDIF
      ELSEIF ( C.EQ.':' ) THEN
        STATE = VALID_VALUE_STATE
      ELSE
        ERR_IDX = ERRFLAG_INVALID_CHAR_IN_SEARCH_SEPARATOR
        STATE = ERROR_STATE
      ENDIF

    !> Read the value, and search the next value or exit
    CASE (VALID_VALUE_STATE)

      HI = GLB_IDX - 1
      COUNT = COUNT + 1

      IF ( PRESENT(VALUES) ) THEN
        IF ( READ_INTEGER( STRING(LO:HI), TMP, HOOKS ) .NE. 0 ) THEN
          ERR_IDX = ERRFLAG_FAILED_READ_INTEGER
          STATE = ERROR_STATE
        ELSE
          IF ( COUNT .GT. SIZE(VALUES) ) THEN
            ERR_IDX = ERRFLAG_OOB_IN_OUT_VALUES
            STATE = ERROR_STATE
          ELSE
            VALUES(COUNT) = TMP
          ENDIF
        ENDIF
      ENDIF

      GLB_IDX = GLB_IDX + 1
      STATE = CHECK_PREFIX_STATE

    !> Read the value, and search the next value or exit
    CASE (LAST_VALID_VALUE_STATE)

      HI = GLB_IDX
      COUNT = COUNT + 1

      IF ( PRESENT(VALUES) ) THEN
        IF ( READ_INTEGER( STRING(LO:HI), TMP, HOOKS ) .NE. 0 ) THEN
          ERR_IDX = ERRFLAG_FAILED_READ_INTEGER
          STATE = ERROR_STATE
        ELSE
          IF ( COUNT .GT. SIZE(VALUES) ) THEN
            ERR_IDX = ERRFLAG_OOB_IN_OUT_VALUES
            STATE = ERROR_STATE
          ELSE
            VALUES(COUNT) = TMP
          ENDIF
        ENDIF
      ENDIF

      STATE = EXIT_STATE

    !> Exit on success from the state machine
    CASE (EXIT_STATE)

      MATCH = .TRUE.
      LOOP  = .FALSE.
      THROW = .FALSE.
      ERR_IDX = ERRFLAG_OK

    !> Exit on error from the state machine
    CASE (ERROR_STATE)

      COUNT = -ERR_IDX
      MATCH = .FALSE.
      LOOP  = .FALSE.
      IF ( PRESENT(VALUES) ) THEN
        THROW = .TRUE.
      ELSE
        THROW = .FALSE.
      ENDIF

    !> Unhandled case
    CASE DEFAULT

      ERR_IDX = ERRFLAG_UNEXPECTED
      STATE = ERROR_STATE

    END SELECT

  ENDDO StatesLoop

  ! Error handling
  IF ( THROW ) THEN
    PP_DEBUG_CRITICAL_COND_THROW( ERR_IDX.NE.ERRFLAG_OK, ERR_IDX )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    CHARACTER(LEN=32) :: CPOS
    CHARACTER(LEN=32) :: CSTATE
    CHARACTER(LEN=32) :: CSTATEID
    CHARACTER(LEN=4096) :: TMPSTR

    ! Initializa a new error frame
    PP_DEBUG_PUSH_FRAME()

    ! Local variables initialization
    TMPSTR=REPEAT(' ',4096)
    CPOS=REPEAT(' ',32)
    CSTATE=REPEAT(' ',32)
    CSTATEID=REPEAT(' ',32)

    WRITE(CPOS,'(I16)') GLB_IDX

    ! Handle different states
    SELECT CASE(OLD_STATE)
    CASE (ENTER_STATE)
      CSTATE='ENTER_STATE'
      CSTATEID='1'
    CASE (CHECK_FIRST_NUMBER_NO_PULL_STATE)
      CSTATE='CHECK_FIRST_NUMBER_NO_PULL_STATE'
      CSTATEID='2'
    CASE (CHECK_PREFIX_STATE)
      CSTATE='CHECK_PREFIX_STATE'
      CSTATEID='3'
    CASE (CHECK_FIRST_NUMBER_STATE)
      CSTATE='CHECK_FIRST_NUMBER_STATE'
      CSTATEID='4'
    CASE (SEARCH_SEPARATOR_STATE)
      CSTATE='SEARCH_SEPARATOR_STATE'
      CSTATEID='5'
    CASE (VALID_VALUE_STATE)
      CSTATE='VALID_VALUE_STATE'
      CSTATEID='6'
    CASE (LAST_VALID_VALUE_STATE)
      CSTATE='LAST_VALID_VALUE_STATE'
      CSTATEID='7'
    CASE (EXIT_STATE)
      CSTATE='EXIT_STATE'
      CSTATEID='8'
    CASE (ERROR_STATE)
      CSTATE='ERROR_STATE'
      CSTATEID='9'
    CASE ( INVALID_STATE )
      CSTATE='INVALID_STATE'
      CSTATEID='666'
    CASE DEFAULT
      CSTATE='UNKNOWN_STATE'
      CSTATEID='0'
    END SELECT

    ! Create the descriptive part of the error message
    TMPSTR='('
    TMPSTR=TRIM(ADJUSTL(TMPSTR))//'state="'//TRIM(ADJUSTL(CSTATE))//'"'
    TMPSTR=TRIM(ADJUSTL(TMPSTR))//', state_id='//TRIM(ADJUSTL(CSTATEID))
    TMPSTR=TRIM(ADJUSTL(TMPSTR))//', position='//TRIM(ADJUSTL(CPOS))
    TMPSTR=TRIM(ADJUSTL(TMPSTR))//', string="'//TRIM(ADJUSTL(STRING))//'"'
    TMPSTR=')'

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_INVALID_CHAR_IN_CHECK_FIRST_NUM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid character in check first number: '//TRIM(ADJUSTL(TMPSTR)) )
    CASE (ERRFLAG_INVALID_CHAR_IN_SEARCH_SEPARATOR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid character in search separator: '//TRIM(ADJUSTL(TMPSTR)) )
    CASE (ERRFLAG_INVALID_CHAR_IN_SEPARATOR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid character in separator: '//TRIM(ADJUSTL(TMPSTR)) )
    CASE (ERRFLAG_OOB_IN_OUT_VALUES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Out of bounds in out values: '//TRIM(ADJUSTL(TMPSTR)) )
    CASE (ERRFLAG_UNEXPECTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unexpected error: '//TRIM(ADJUSTL(TMPSTR)) )
    CASE (ERRFLAG_FAILED_READ_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to read an integer: '//TRIM(ADJUSTL(TMPSTR)) )
    CASE (ERRFLAG_GLB_IDX_LOWER_THAN_1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Global index lower than 1: '//TRIM(ADJUSTL(TMPSTR)) )
    CASE (ERRFLAG_GLB_IDX_BIGGER_THAN_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Global index bigger than string: '//TRIM(ADJUSTL(TMPSTR)) )
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

  ! Exit point on error
  RETURN

END FUNCTION READ_INTEGER_PATTERNS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Converts a substring to an integer value.
!>
!> This function attempts to read an integer value from the provided substring and store
!> it in `NUM`. The function returns a status code indicating whether the conversion was successful.
!>
!> @section interface
!> @param [in]  SUBSTR   Input substring to be parsed.
!> @param [out] NUM      Parsed integer value.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return [Integer] Error code indicating success or failure of the operation.
!>         - `0`: Success
!>         - `1`: Failure to read an integer
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] "DATAKINDS_DEF_MOD::JPIB_K"
!>
!> @subsection special dependencies
!>   - @dependency [*] "OM_DEBUG_MOD::*"
!>   - @dependency [*] "OM_TRACE_MOD::*"
!>   - @dependency [*] "OM_LOG_MOD::*"
!>
!> @see READ_INTEGER_PATTERNS
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_INTEGER'
PP_THREAD_SAFE FUNCTION  READ_INTEGER( S, NUM, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: S
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: NUM
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING=1_JPIB_K

  ! Local variables
  INTEGER :: IOS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for loging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Read the integer from the string
  READ(S, *, IOSTAT=IOS) NUM
  PP_DEBUG_CRITICAL_COND_THROW( IOS .NE. 0, ERRFLAG_INVALID_STRING )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_INVALID_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read an integer from a string: '//TRIM(ADJUSTL(S)) )
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

  ! Exit point on error
  RETURN

END FUNCTION READ_INTEGER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARSE_INTEGER_ARRAY'
PP_THREAD_SAFE FUNCTION PARSE_INTEGER_ARRAY( UNIT, POS, TERM_, N, S, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                                INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),                            INTENT(INOUT) :: POS
  CHARACTER(LEN=*),                                INTENT(IN)    :: TERM_
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: N
  CHARACTER(LEN=1),                                INTENT(OUT)   :: S
  TYPE(HOOKS_T),                                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local Variables
  LOGICAL :: EX
  TYPE(INT_CONTAINER_T), POINTER :: HEAD=>NULL()
  TYPE(INT_CONTAINER_T), POINTER :: THIS=>NULL()
  LOGICAL :: SCALAR
  LOGICAL :: R
  LOGICAL :: END_
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: TERM
  CHARACTER(LEN=1) :: BUF
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_READ_INTEGER = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_READING_INTEGER_ARRAY = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FLAG = 5_JPIB_K

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

  !...Initialization
  EX = .TRUE.
  R = .TRUE.
  TERM = TERM_
  END_ = .FALSE.

  ! Read Data From UNIT
  CNT = 0
  S = ' '
  main : DO

    ! if thre is an already readed character skip read operation
    IF ( R ) THEN
      POS = POS+1
      IF ( POS .GT. LEN(UNIT) ) THEN
        STAT = -1
      ELSE
        STAT = 0
        BUF = UNIT(POS:POS)
      ENDIF
    ELSE
      BUF = S
      STAT = 0
    ENDIF

    ! End of file exception
    IF ( STAT .EQ. -1 ) THEN
      R = .TRUE.
      EXIT main
    ENDIF

    ! Check if the token is an array
    IF ( BUF .EQ. '[' .AND. CNT .EQ. 0 ) THEN
      SCALAR = .FALSE.
      TERM = TERM//']'
      CNT = CNT + 1
      ALLOCATE( HEAD, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )
      THIS => HEAD
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER) PARSE_INTEGER( UNIT, POS, TERM, THIS%PAR, S, HOOKS )
      R = .FALSE.
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_INVALID_FLAG)
      ! IF (.NOT. EX ) EXIT main
      CYCLE main
    ENDIF

    ! Check if the token is not an array
    IF ( BUF .NE. '[' .AND. &
         BUF .NE. ' ' .AND. &
         CNT .EQ. 0 ) THEN
      SCALAR = .TRUE.
      CNT = CNT + 1
      ALLOCATE( HEAD, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )
      THIS => HEAD
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER) PARSE_INTEGER( UNIT, POS, TERM, THIS%PAR, S, HOOKS, BUF )
      R = .FALSE.
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_INVALID_FLAG)
      ! IF (.NOT. EX ) EXIT main
      CYCLE main
    ENDIF

    ! Read another token
    IF ( BUF .EQ. ',' ) THEN
      CNT = CNT + 1
      ! Coherence check
      IF ( CNT .GT. 1 .AND. SCALAR ) THEN
        DO J = 1, LEN_TRIM(TERM_)
          IF ( BUF .EQ. TERM_(J:J) ) THEN
            S = BUF
            CNT = CNT - 1
            EXIT main
          ENDIF
        ENDDO
        EX = .FALSE.
        CNT = CNT - 1
        PP_DEBUG_CRITICAL_THROW( ERRFLAG_ERROR_READING_INTEGER_ARRAY )
        EXIT main
      END IF
      IF ( END_ ) THEN
        DO J = 1, LEN_TRIM(TERM_)
          IF ( BUF .EQ. TERM_(J:J) ) THEN
            S = BUF
            CNT = CNT - 1
            EXIT main
          ENDIF
        ENDDO
        CYCLE main
      END IF
      ALLOCATE( THIS%NEXT, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )
      THIS => THIS%NEXT
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER) PARSE_INTEGER( UNIT, POS, TERM, THIS%PAR, S, HOOKS )
      R = .FALSE.
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_INVALID_FLAG)
      ! IF (.NOT. EX ) EXIT main
      CYCLE main
    ENDIF

    ! jump the close bracket
    IF ( BUF .EQ. ']' ) THEN
      R = .TRUE.
      END_ =.TRUE.
      CYCLE main
    ENDIF

    ! Exit condition
    DO J = 1, LEN_TRIM(TERM_)
      IF ( BUF .EQ. TERM_(J:J) ) THEN
        S = BUF
        EXIT main
      END IF
    ENDDO

    R = .TRUE.

  ENDDO main

  ! Error handling
  IF ( CNT .EQ. 0 .OR. .NOT.EX ) THEN
    S = ' '
    EX = .FALSE.
    THIS=>HEAD
    DO
      IF ( .NOT. ASSOCIATED(THIS) ) THEN
        EXIT
      ENDIF
      THIS => THIS%NEXT
      DEALLOCATE(HEAD, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      HEAD => THIS
    END DO
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_ERROR_READING_INTEGER_ARRAY )
  ENDIF

  ! Allocate output memory
  ALLOCATE( N(CNT), STAT=ALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )

  ! Fill the integer array
  THIS=>HEAD
  CNT = 0
  DO

    CNT = CNT + 1

    IF ( .NOT. ASSOCIATED(THIS) ) THEN
      EXIT
    ENDIF

    N(CNT) = THIS%PAR

    THIS => THIS%NEXT

    DEALLOCATE(HEAD, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )

    HEAD => THIS

  ENDDO

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call read integer' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate' )
      IF ( ALLOCATED( ERRMSG ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate' )
      IF ( ALLOCATED( ERRMSG ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE(ERRFLAG_ERROR_READING_INTEGER_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading integer array' )
    CASE(ERRFLAG_INVALID_FLAG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid flag' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION PARSE_INTEGER_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARSE_INTEGER'
PP_THREAD_SAFE FUNCTION PARSE_INTEGER( UNIT, POS, TERM, N, S, HOOKS, F ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),           INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),       INTENT(INOUT) :: POS
  CHARACTER(LEN=*),           INTENT(IN)    :: TERM
  INTEGER(KIND=JPIB_K),       INTENT(OUT)   :: N
  CHARACTER(LEN=1),           INTENT(OUT)   :: S
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=1), OPTIONAL, INTENT(IN)    :: F

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local Variables
  LOGICAL :: EX
  LOGICAL :: READ_
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: TMP
  CHARACTER(LEN=1) :: BUF
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  INTEGER(KIND=JPIB_K) :: READ_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_READING_INTEGER = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_INTEGER_NOT_ALLOCATED = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CHARACTER = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STATE = 5_JPIB_K

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
  EX = .TRUE.
  TMP=''

  ! Read Data From UNIT
  CNT = 0
  READ_ = .TRUE.
  main : DO

    IF ( PRESENT(F) .AND. READ_ ) THEN

      BUF = F
      READ_ = .FALSE.
      STAT = 0

    ELSE

      POS = POS+1
      IF ( POS .GT. LEN(UNIT) ) THEN
        STAT = -1
      ELSE
        STAT = 0
        BUF = UNIT(POS:POS)
      END IF

    END IF

    ! End of file
    IF ( STAT .EQ. -1 ) THEN
      EXIT main
    END IF

    ! Check for special terminators
    DO J = 1, LEN(TERM)
      IF ( BUF .EQ. TERM(J:J) ) THEN
        S = BUF
        EXIT main
      END IF
    END DO

    ! Check for standard terminator
    IF ( BUF .EQ. ',' ) THEN
      S = BUF
      EXIT main
    END IF

    ! Update token
    IF ( BUF .EQ. ' ' .AND. CNT .EQ. 0 ) THEN
      CYCLE main
    ELSE
      CNT = CNT + 1
      TMP = TMP(:)//BUF
    END IF

  END DO main

  ! Error handling
  IF ( STAT .EQ. -2 ) THEN
    IF ( ALLOCATED(TMP) ) THEN
      DEALLOCATE(TMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
    END IF
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_STATE )
  END IF

  ! Create output variable
  TMP = TRIM(ADJUSTL(TMP(:)))
  DO I = 1 , LEN(TMP)
    IF ( (IACHAR( TMP(I:I) ) .LT. 48 .OR. IACHAR( TMP(I:I) ) .GT. 57) .AND. IACHAR( TMP(I:I) ) .NE. 45 ) THEN
      EX = .FALSE.
      IF ( ALLOCATED(TMP) ) THEN
        DEALLOCATE(TMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
        PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      END IF
      PP_DEBUG_CRITICAL_THROW(ERRFLAG_INVALID_CHARACTER)
    END IF
  END DO

  ! Read the number and free memory
  IF ( ALLOCATED(TMP) ) THEN
    ! WRITE(*,*) ' + Reading: ', TMP(:)
    READ(TMP(:),*, IOSTAT=READ_STAT) N
    PP_DEBUG_CRITICAL_COND_THROW( READ_STAT .NE. 0, ERRFLAG_ERROR_READING_INTEGER )
    DEALLOCATE(TMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  ELSE
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_ERROR_INTEGER_NOT_ALLOCATED )
  END IF

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STAT)
      END IF
    CASE(ERRFLAG_ERROR_READING_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading integer' )
    CASE(ERRFLAG_ERROR_INTEGER_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error integer not allocated' )
    CASE(ERRFLAG_INVALID_CHARACTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid character' )
    CASE(ERRFLAG_INVALID_STATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid state' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION PARSE_INTEGER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE







#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARSE_REAL64_ARRAY'
PP_THREAD_SAFE FUNCTION PARSE_REAL64_ARRAY( UNIT, POS, TERM_, F64, S, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                             INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),                         INTENT(INOUT) :: POS
  CHARACTER(LEN=*),                             INTENT(IN)    :: TERM_
  REAL(KIND=JPRD_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: F64
  CHARACTER(LEN=1),                             INTENT(OUT)   :: S
  TYPE(HOOKS_T),                                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local Variables
  LOGICAL :: EX
  TYPE(REAL64_CONTAINER_T), POINTER :: HEAD=>NULL()
  TYPE(REAL64_CONTAINER_T), POINTER :: THIS=>NULL()
  LOGICAL :: SCALAR
  LOGICAL :: R
  LOGICAL :: END_
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: TERM
  CHARACTER(LEN=1) :: BUF
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_READ_INTEGER = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_READING_INTEGER_ARRAY = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FLAG = 5_JPIB_K

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

  !...Initialization
  EX = .TRUE.
  R = .TRUE.
  TERM = TERM_
  END_ = .FALSE.

  ! Read Data From UNIT
  CNT = 0
  S = ' '
  main : DO

    ! if thre is an already readed character skip read operation
    IF ( R ) THEN
      POS = POS+1
      IF ( POS .GT. LEN(UNIT) ) THEN
        STAT = -1
      ELSE
        STAT = 0
        BUF = UNIT(POS:POS)
      ENDIF
    ELSE
      BUF = S
      STAT = 0
    ENDIF

    ! End of file exception
    IF ( STAT .EQ. -1 ) THEN
      R = .TRUE.
      EXIT main
    ENDIF

    ! Check if the token is an array
    IF ( BUF .EQ. '[' .AND. CNT .EQ. 0 ) THEN
      SCALAR = .FALSE.
      TERM = TERM//']'
      CNT = CNT + 1
      ALLOCATE( HEAD, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )
      THIS => HEAD
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER) PARSE_REAL64( UNIT, POS, TERM, THIS%PAR, S, HOOKS )
      R = .FALSE.
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_INVALID_FLAG)
      ! IF (.NOT. EX ) EXIT main
      CYCLE main
    ENDIF

    ! Check if the token is not an array
    IF ( BUF .NE. '[' .AND. &
         BUF .NE. ' ' .AND. &
         CNT .EQ. 0 ) THEN
      SCALAR = .TRUE.
      CNT = CNT + 1
      ALLOCATE( HEAD, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )
      THIS => HEAD
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER) PARSE_REAL64( UNIT, POS, TERM, THIS%PAR, S, HOOKS, BUF )
      R = .FALSE.
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_INVALID_FLAG)
      ! IF (.NOT. EX ) EXIT main
      CYCLE main
    ENDIF

    ! Read another token
    IF ( BUF .EQ. ',' ) THEN
      CNT = CNT + 1
      ! Coherence check
      IF ( CNT .GT. 1 .AND. SCALAR ) THEN
        DO J = 1, LEN_TRIM(TERM_)
          IF ( BUF .EQ. TERM_(J:J) ) THEN
            S = BUF
            CNT = CNT - 1
            EXIT main
          ENDIF
        ENDDO
        EX = .FALSE.
        CNT = CNT - 1
        PP_DEBUG_CRITICAL_THROW( ERRFLAG_ERROR_READING_INTEGER_ARRAY )
        EXIT main
      END IF
      IF ( END_ ) THEN
        DO J = 1, LEN_TRIM(TERM_)
          IF ( BUF .EQ. TERM_(J:J) ) THEN
            S = BUF
            CNT = CNT - 1
            EXIT main
          ENDIF
        ENDDO
        CYCLE main
      END IF
      ALLOCATE( THIS%NEXT, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )
      THIS => THIS%NEXT
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER) PARSE_REAL64( UNIT, POS, TERM, THIS%PAR, S, HOOKS )
      R = .FALSE.
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_INVALID_FLAG)
      ! IF (.NOT. EX ) EXIT main
      CYCLE main
    ENDIF

    ! jump the close bracket
    IF ( BUF .EQ. ']' ) THEN
      R = .TRUE.
      END_ =.TRUE.
      CYCLE main
    ENDIF

    ! Exit condition
    DO J = 1, LEN_TRIM(TERM_)
      IF ( BUF .EQ. TERM_(J:J) ) THEN
        S = BUF
        EXIT main
      END IF
    ENDDO

    R = .TRUE.

  ENDDO main

  ! Error handling
  IF ( CNT .EQ. 0 .OR. .NOT.EX ) THEN
    S = ' '
    EX = .FALSE.
    THIS=>HEAD
    DO
      IF ( .NOT. ASSOCIATED(THIS) ) THEN
        EXIT
      ENDIF
      THIS => THIS%NEXT
      DEALLOCATE(HEAD, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      HEAD => THIS
    END DO
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_ERROR_READING_INTEGER_ARRAY )
  ENDIF

  ! Allocate output memory
  ALLOCATE( F64(CNT), STAT=ALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )

  ! Fill the integer array
  THIS=>HEAD
  CNT = 0
  DO

    CNT = CNT + 1

    IF ( .NOT. ASSOCIATED(THIS) ) THEN
      EXIT
    ENDIF

    F64(CNT) = THIS%PAR

    THIS => THIS%NEXT

    DEALLOCATE(HEAD, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )

    HEAD => THIS

  ENDDO

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call read integer' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate' )
      IF ( ALLOCATED( ERRMSG ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate' )
      IF ( ALLOCATED( ERRMSG ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE(ERRFLAG_ERROR_READING_INTEGER_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading integer array' )
    CASE(ERRFLAG_INVALID_FLAG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid flag' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION PARSE_REAL64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARSE_REAL64'
PP_THREAD_SAFE FUNCTION PARSE_REAL64( UNIT, POS, TERM, F64, S, HOOKS, F ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),           INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),       INTENT(INOUT) :: POS
  CHARACTER(LEN=*),           INTENT(IN)    :: TERM
  REAL(KIND=JPRD_K),          INTENT(OUT)   :: F64
  CHARACTER(LEN=1),           INTENT(OUT)   :: S
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=1), OPTIONAL, INTENT(IN)    :: F

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local Variables
  LOGICAL :: EX
  LOGICAL :: READ_
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: TMP
  CHARACTER(LEN=1) :: BUF
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  INTEGER(KIND=JPIB_K) :: READ_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_READING_INTEGER = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_INTEGER_NOT_ALLOCATED = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CHARACTER = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STATE = 5_JPIB_K

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
  EX = .TRUE.
  TMP=''

  ! Read Data From UNIT
  CNT = 0
  READ_ = .TRUE.
  main : DO

    IF ( PRESENT(F) .AND. READ_ ) THEN

      BUF = F
      READ_ = .FALSE.
      STAT = 0

    ELSE

      POS = POS+1
      IF ( POS .GT. LEN(UNIT) ) THEN
        STAT = -1
      ELSE
        STAT = 0
        BUF = UNIT(POS:POS)
      END IF

    END IF

    ! End of file
    IF ( STAT .EQ. -1 ) THEN
      EXIT main
    END IF

    ! Check for special terminators
    DO J = 1, LEN(TERM)
      IF ( BUF .EQ. TERM(J:J) ) THEN
        S = BUF
        EXIT main
      END IF
    END DO

    ! Check for standard terminator
    IF ( BUF .EQ. ',' ) THEN
      S = BUF
      EXIT main
    END IF

    ! Update token
    IF ( BUF .EQ. ' ' .AND. CNT .EQ. 0 ) THEN
      CYCLE main
    ELSE
      CNT = CNT + 1
      TMP = TMP(:)//BUF
    END IF

  END DO main

  ! Error handling
  IF ( STAT .EQ. -2 ) THEN
    IF ( ALLOCATED(TMP) ) THEN
      DEALLOCATE(TMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
    END IF
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_STATE )
  END IF

  ! Create output variable
  TMP = TRIM(ADJUSTL(TMP(:)))
  DO I = 1 , LEN(TMP)
    IF ( (IACHAR( TMP(I:I) ) .LT. 48 .OR. IACHAR( TMP(I:I) ) .GT. 57) .AND. &
&     (IACHAR( TMP(I:I) ) .NE. 45 .AND. IACHAR( TMP(I:I) ) .NE. 46 .AND. IACHAR( TMP(I:I) ) .NE. 69 .AND. IACHAR( TMP(I:I) ) .NE. 101)  ) THEN
      EX = .FALSE.
      IF ( ALLOCATED(TMP) ) THEN
        DEALLOCATE(TMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
        PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      END IF
      PP_DEBUG_CRITICAL_THROW(ERRFLAG_INVALID_CHARACTER)
    END IF
  END DO

  ! Read the number and free memory
  IF ( ALLOCATED(TMP) ) THEN
    ! WRITE(*,*) ' + Reading: ', TMP(:)
    READ(TMP(:),*, IOSTAT=READ_STAT) F64
    PP_DEBUG_CRITICAL_COND_THROW( READ_STAT .NE. 0, ERRFLAG_ERROR_READING_INTEGER )
    DEALLOCATE(TMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  ELSE
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_ERROR_INTEGER_NOT_ALLOCATED )
  END IF

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STAT)
      END IF
    CASE(ERRFLAG_ERROR_READING_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading integer' )
    CASE(ERRFLAG_ERROR_INTEGER_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error integer not allocated' )
    CASE(ERRFLAG_INVALID_CHARACTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid character' )
    CASE(ERRFLAG_INVALID_STATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid state' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION PARSE_REAL64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE








#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARSE_REAL32_ARRAY'
PP_THREAD_SAFE FUNCTION PARSE_REAL32_ARRAY( UNIT, POS, TERM_, F32, S, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRM_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                             INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),                         INTENT(INOUT) :: POS
  CHARACTER(LEN=*),                             INTENT(IN)    :: TERM_
  REAL(KIND=JPRM_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: F32
  CHARACTER(LEN=1),                             INTENT(OUT)   :: S
  TYPE(HOOKS_T),                                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local Variables
  LOGICAL :: EX
  TYPE(REAL32_CONTAINER_T), POINTER :: HEAD=>NULL()
  TYPE(REAL32_CONTAINER_T), POINTER :: THIS=>NULL()
  LOGICAL :: SCALAR
  LOGICAL :: R
  LOGICAL :: END_
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: TERM
  CHARACTER(LEN=1) :: BUF
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_READ_INTEGER = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_READING_INTEGER_ARRAY = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FLAG = 5_JPIB_K

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

  !...Initialization
  EX = .TRUE.
  R = .TRUE.
  TERM = TERM_
  END_ = .FALSE.

  ! Read Data From UNIT
  CNT = 0
  S = ' '
  main : DO

    ! if thre is an already readed character skip read operation
    IF ( R ) THEN
      POS = POS+1
      IF ( POS .GT. LEN(UNIT) ) THEN
        STAT = -1
      ELSE
        STAT = 0
        BUF = UNIT(POS:POS)
      ENDIF
    ELSE
      BUF = S
      STAT = 0
    ENDIF

    ! End of file exception
    IF ( STAT .EQ. -1 ) THEN
      R = .TRUE.
      EXIT main
    ENDIF

    ! Check if the token is an array
    IF ( BUF .EQ. '[' .AND. CNT .EQ. 0 ) THEN
      SCALAR = .FALSE.
      TERM = TERM//']'
      CNT = CNT + 1
      ALLOCATE( HEAD, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )
      THIS => HEAD
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER) PARSE_REAL32( UNIT, POS, TERM, THIS%PAR, S, HOOKS )
      R = .FALSE.
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_INVALID_FLAG)
      ! IF (.NOT. EX ) EXIT main
      CYCLE main
    ENDIF

    ! Check if the token is not an array
    IF ( BUF .NE. '[' .AND. &
         BUF .NE. ' ' .AND. &
         CNT .EQ. 0 ) THEN
      SCALAR = .TRUE.
      CNT = CNT + 1
      ALLOCATE( HEAD, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )
      THIS => HEAD
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER) PARSE_REAL32( UNIT, POS, TERM, THIS%PAR, S, HOOKS, BUF )
      R = .FALSE.
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_INVALID_FLAG)
      ! IF (.NOT. EX ) EXIT main
      CYCLE main
    ENDIF

    ! Read another token
    IF ( BUF .EQ. ',' ) THEN
      CNT = CNT + 1
      ! Coherence check
      IF ( CNT .GT. 1 .AND. SCALAR ) THEN
        DO J = 1, LEN_TRIM(TERM_)
          IF ( BUF .EQ. TERM_(J:J) ) THEN
            S = BUF
            CNT = CNT - 1
            EXIT main
          ENDIF
        ENDDO
        EX = .FALSE.
        CNT = CNT - 1
        PP_DEBUG_CRITICAL_THROW( ERRFLAG_ERROR_READING_INTEGER_ARRAY )
        EXIT main
      END IF
      IF ( END_ ) THEN
        DO J = 1, LEN_TRIM(TERM_)
          IF ( BUF .EQ. TERM_(J:J) ) THEN
            S = BUF
            CNT = CNT - 1
            EXIT main
          ENDIF
        ENDDO
        CYCLE main
      END IF
      ALLOCATE( THIS%NEXT, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )
      THIS => THIS%NEXT
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER) PARSE_REAL32( UNIT, POS, TERM, THIS%PAR, S, HOOKS )
      R = .FALSE.
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_INVALID_FLAG)
      ! IF (.NOT. EX ) EXIT main
      CYCLE main
    ENDIF

    ! jump the close bracket
    IF ( BUF .EQ. ']' ) THEN
      R = .TRUE.
      END_ =.TRUE.
      CYCLE main
    ENDIF

    ! Exit condition
    DO J = 1, LEN_TRIM(TERM_)
      IF ( BUF .EQ. TERM_(J:J) ) THEN
        S = BUF
        EXIT main
      END IF
    ENDDO

    R = .TRUE.

  ENDDO main

  ! Error handling
  IF ( CNT .EQ. 0 .OR. .NOT.EX ) THEN
    S = ' '
    EX = .FALSE.
    THIS=>HEAD
    DO
      IF ( .NOT. ASSOCIATED(THIS) ) THEN
        EXIT
      ENDIF
      THIS => THIS%NEXT
      DEALLOCATE(HEAD, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      HEAD => THIS
    END DO
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_ERROR_READING_INTEGER_ARRAY )
  ENDIF

  ! Allocate output memory
  ALLOCATE( F32(CNT), STAT=ALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )

  ! Fill the integer array
  THIS=>HEAD
  CNT = 0
  DO

    CNT = CNT + 1

    IF ( .NOT. ASSOCIATED(THIS) ) THEN
      EXIT
    ENDIF

    F32(CNT) = THIS%PAR

    THIS => THIS%NEXT

    DEALLOCATE(HEAD, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )

    HEAD => THIS

  ENDDO

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call read integer' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate' )
      IF ( ALLOCATED( ERRMSG ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate' )
      IF ( ALLOCATED( ERRMSG ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE(ERRFLAG_ERROR_READING_INTEGER_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading integer array' )
    CASE(ERRFLAG_INVALID_FLAG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid flag' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION PARSE_REAL32_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARSE_REAL32'
PP_THREAD_SAFE FUNCTION PARSE_REAL32( UNIT, POS, TERM, F32, S, HOOKS, F ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),           INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),       INTENT(INOUT) :: POS
  CHARACTER(LEN=*),           INTENT(IN)    :: TERM
  REAL(KIND=JPRM_K),          INTENT(OUT)   :: F32
  CHARACTER(LEN=1),           INTENT(OUT)   :: S
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=1), OPTIONAL, INTENT(IN)    :: F

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local Variables
  LOGICAL :: EX
  LOGICAL :: READ_
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: TMP
  CHARACTER(LEN=1) :: BUF
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  INTEGER(KIND=JPIB_K) :: READ_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_READING_INTEGER = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_INTEGER_NOT_ALLOCATED = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CHARACTER = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STATE = 5_JPIB_K

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
  EX = .TRUE.
  TMP=''

  ! Read Data From UNIT
  CNT = 0
  READ_ = .TRUE.
  main : DO

    IF ( PRESENT(F) .AND. READ_ ) THEN

      BUF = F
      READ_ = .FALSE.
      STAT = 0

    ELSE

      POS = POS+1
      IF ( POS .GT. LEN(UNIT) ) THEN
        STAT = -1
      ELSE
        STAT = 0
        BUF = UNIT(POS:POS)
      END IF

    END IF

    ! End of file
    IF ( STAT .EQ. -1 ) THEN
      EXIT main
    END IF

    ! Check for special terminators
    DO J = 1, LEN(TERM)
      IF ( BUF .EQ. TERM(J:J) ) THEN
        S = BUF
        EXIT main
      END IF
    END DO

    ! Check for standard terminator
    IF ( BUF .EQ. ',' ) THEN
      S = BUF
      EXIT main
    END IF

    ! Update token
    IF ( BUF .EQ. ' ' .AND. CNT .EQ. 0 ) THEN
      CYCLE main
    ELSE
      CNT = CNT + 1
      TMP = TMP(:)//BUF
    END IF

  END DO main

  ! Error handling
  IF ( STAT .EQ. -2 ) THEN
    IF ( ALLOCATED(TMP) ) THEN
      DEALLOCATE(TMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
    END IF
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_STATE )
  END IF

  ! Create output variable
  TMP = TRIM(ADJUSTL(TMP(:)))
  DO I = 1 , LEN(TMP)
    IF ( (IACHAR( TMP(I:I) ) .LT. 48 .OR. IACHAR( TMP(I:I) ) .GT. 57) .AND. &
&     (IACHAR( TMP(I:I) ) .NE. 45 .AND. IACHAR( TMP(I:I) ) .NE. 46 .AND. IACHAR( TMP(I:I) ) .NE. 69 .AND. IACHAR( TMP(I:I) ) .NE. 101)  ) THEN
      EX = .FALSE.
      IF ( ALLOCATED(TMP) ) THEN
        DEALLOCATE(TMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
        PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      END IF
      PP_DEBUG_CRITICAL_THROW(ERRFLAG_INVALID_CHARACTER)
    END IF
  END DO

  ! Read the number and free memory
  IF ( ALLOCATED(TMP) ) THEN
    ! WRITE(*,*) ' + Reading: ', TMP(:)
    READ(TMP(:),*, IOSTAT=READ_STAT) F32
    PP_DEBUG_CRITICAL_COND_THROW( READ_STAT .NE. 0, ERRFLAG_ERROR_READING_INTEGER )
    DEALLOCATE(TMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  ELSE
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_ERROR_INTEGER_NOT_ALLOCATED )
  END IF

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STAT)
      END IF
    CASE(ERRFLAG_ERROR_READING_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading integer' )
    CASE(ERRFLAG_ERROR_INTEGER_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error integer not allocated' )
    CASE(ERRFLAG_INVALID_CHARACTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid character' )
    CASE(ERRFLAG_INVALID_STATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid state' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION PARSE_REAL32
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE CONFIGURATION_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME