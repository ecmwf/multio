! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'log_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'LOG_UTILS_MOD'
MODULE LOG_UTILS_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

! Default visibility
PRIVATE

!> Local parameters
INTEGER(KIND=JPIB_K), PARAMETER :: MAX_STR_LEN = 32_JPIB_K


!> Local overloading of the intrinsic function TO_STRING
INTERFACE TO_STRING
  MODULE PROCEDURE BOOL_TO_STRING
  MODULE PROCEDURE INT8_TO_STRING
  MODULE PROCEDURE INT16_TO_STRING
  MODULE PROCEDURE INT32_TO_STRING
  MODULE PROCEDURE INT64_TO_STRING
  MODULE PROCEDURE REAL32_TO_STRING
  MODULE PROCEDURE REAL64_TO_STRING
  MODULE PROCEDURE BOOL_TO_STRING_ARRAY
  MODULE PROCEDURE INT8_TO_STRING_ARRAY
  MODULE PROCEDURE INT16_TO_STRING_ARRAY
  MODULE PROCEDURE INT32_TO_STRING_ARRAY
  MODULE PROCEDURE INT64_TO_STRING_ARRAY
  MODULE PROCEDURE REAL32_TO_STRING_ARRAY
  MODULE PROCEDURE REAL64_TO_STRING_ARRAY
  MODULE PROCEDURE BOOL_ARRAY_TO_STRING_ARRAY
  MODULE PROCEDURE INT8_ARRAY_TO_STRING_ARRAY
  MODULE PROCEDURE INT16_ARRAY_TO_STRING_ARRAY
  MODULE PROCEDURE INT32_ARRAY_TO_STRING_ARRAY
  MODULE PROCEDURE INT64_ARRAY_TO_STRING_ARRAY
  MODULE PROCEDURE REAL32_ARRAY_TO_STRING_ARRAY
  MODULE PROCEDURE REAL64_ARRAY_TO_STRING_ARRAY
  MODULE PROCEDURE BOOL_ARRAY_TO_STRING
  MODULE PROCEDURE INT8_ARRAY_TO_STRING
  MODULE PROCEDURE INT16_ARRAY_TO_STRING
  MODULE PROCEDURE INT32_ARRAY_TO_STRING
  MODULE PROCEDURE INT64_ARRAY_TO_STRING
  MODULE PROCEDURE REAL32_ARRAY_TO_STRING
  MODULE PROCEDURE REAL64_ARRAY_TO_STRING
END INTERFACE TO_STRING


!> Whitelist of public symbols
PUBLIC :: MAX_STR_LEN
PUBLIC :: TO_STRING
PUBLIC :: BYTES_TO_STRING
PUBLIC :: STRING_ARRAY_TO_STRING

CONTAINS



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'STRING_ARRAY_TO_STRING'
PP_THREAD_SAFE FUNCTION STRING_ARRAY_TO_STRING( STRING_ARRAY, STRING, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), INTENT(IN)    :: STRING_ARRAY
  CHARACTER(LEN=:), ALLOCATABLE,            INTENT(INOUT) :: STRING
  TYPE(HOOKS_T),                            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: SZ
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  CHARACTER(LEN=2) :: SEP
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=2_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STRING), ERRFLAG_ALREADY_ALLOCATED )

  ! Comput the total length of the output string
  SZ = 2
  DO I = 1, SIZE(STRING_ARRAY)
    SZ = SZ + LEN_TRIM(ADJUSTL(STRING_ARRAY(I))) + 2
  ENDDO

  ! Allocate the output string
  ALLOCATE( CHARACTER(LEN=SZ)::STRING, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  ! Initialize the output string
  STRING = REPEAT( ' ', SZ)

  ! Fill the output string
  LO=0
  HI=0
  SEP = '[ '
  DO I = 1, SIZE(STRING_ARRAY)
    LO = HI + 1
    HI = LO + LEN_TRIM(ADJUSTL(STRING_ARRAY(I))) + 2 - 1
    STRING(LO:HI) = SEP//TRIM(ADJUSTL(STRING_ARRAY(I)))
    SEP=', '
  ENDDO
  LO = HI + 1
  HI = LO + 1
  STRING(LO:HI) = ' ]'

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
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
    CASE (ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output string already allocated' )
    CASE (ERRFLAG_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error allocating the output string' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=ALLOC_STATUS )
      ENDIF
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

END FUNCTION STRING_ARRAY_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'BYTES_TO_STRING'
PP_THREAD_SAFE FUNCTION BYTES_TO_STRING( BYTES, STR, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: BYTES
  CHARACTER(LEN=MAX_STR_LEN), INTENT(OUT)   :: STR
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  REAL(KIND=JPRD_K) :: MEM
  INTEGER(KIND=JPIB_K) :: SUFFIX
  INTEGER(KIND=JPIB_K) :: I
  CHARACTER(LEN=MAX_STR_LEN) :: TMP

  ! define the suffixes for memory units
  CHARACTER(LEN=4), DIMENSION(7), PARAMETER :: UNITS(7) = ['[B] ', '[KB]', '[MB]', '[GB]', '[TB]', '[PB]', '[EB]']

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

  ! convert bytes to the appropriate unit
  MEM = REAL(BYTES, JPRD_K)
  DO I = 1, SIZE(UNITS)
    IF ( MEM .LT. 1024.0_JPRD_K ) THEN
      SUFFIX = I
      EXIT
    ENDIF
    MEM = MEM / 1024.0_JPRD_K
  END DO

  ! format the memory value with the appropriate suffix
  TMP = REPEAT(' ', MAX_STR_LEN)
  STR = REPEAT(' ', MAX_STR_LEN)
  WRITE(TMP, '(F10.2)') MEM
  STR = TRIM(ADJUSTL(TMP))//' '//TRIM(ADJUSTL(UNITS(I)))

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION BYTES_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'BOOL_TO_STRING'
PP_THREAD_SAFE FUNCTION BOOL_TO_STRING( VAL, STR, HOOKS ) RESULT(RET)

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
  LOGICAL,                    INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), INTENT(OUT)   :: STR
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

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

  ! Initializing the output string
  STR = REPEAT(' ',MAX_STR_LEN)

  ! Conversion bool to string
  IF ( VAL ) THEN
    STR = '.TRUE.'
  ELSE
    STR = '.FALSE.'
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
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

END FUNCTION BOOL_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT8_TO_STRING'
PP_THREAD_SAFE FUNCTION INT8_TO_STRING( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

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
  INTEGER(KIND=INT8),         INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), INTENT(OUT)   :: STR
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET


  ! Local variables
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  CHARACTER(LEN=MAX_STR_LEN) :: TMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_INT8_TO_STRING=2_JPIB_K

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

  ! Initializing the output string
  TMP = REPEAT(' ',MAX_STR_LEN)
  STR = REPEAT(' ',MAX_STR_LEN)

  ! Convert Integer to string
  WRITE(TMP, '(I32)', IOSTAT=WRITE_STATUS) VAL
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_UNABLE_TO_CONVERT_INT8_TO_STRING )

  STR=ADJUSTL(TMP)

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
    CASE(ERRFLAG_UNABLE_TO_CONVERT_INT8_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert INT8 to string' )
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

END FUNCTION INT8_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT16_TO_STRING'
PP_THREAD_SAFE FUNCTION INT16_TO_STRING( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

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
  INTEGER(KIND=INT16),        INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), INTENT(OUT)   :: STR
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  CHARACTER(LEN=MAX_STR_LEN) :: TMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_INT16_TO_STRING=2_JPIB_K

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

  ! Initializing the output string
  STR = REPEAT(' ',MAX_STR_LEN)
  TMP = REPEAT(' ',MAX_STR_LEN)

  ! Convert Integer to string
  WRITE(TMP, '(I32)', IOSTAT=WRITE_STATUS) VAL
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_UNABLE_TO_CONVERT_INT16_TO_STRING )
  STR=ADJUSTL(TMP)


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
    CASE(ERRFLAG_UNABLE_TO_CONVERT_INT16_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert INT16 to string' )
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

END FUNCTION INT16_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT32_TO_STRING'
PP_THREAD_SAFE FUNCTION INT32_TO_STRING( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

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
  INTEGER(KIND=INT32),        INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), INTENT(OUT)   :: STR
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  CHARACTER(LEN=MAX_STR_LEN) :: TMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_INT32_TO_STRING=2_JPIB_K

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

  ! Initializing the output string
  TMP = REPEAT(' ',MAX_STR_LEN)
  STR = REPEAT(' ',MAX_STR_LEN)

  ! Convert Integer to string
  WRITE(TMP, '(I32)', IOSTAT=WRITE_STATUS) VAL
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_UNABLE_TO_CONVERT_INT32_TO_STRING )

  STR=ADJUSTL(TMP)

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
    CASE(ERRFLAG_UNABLE_TO_CONVERT_INT32_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert INT32 to string' )
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

END FUNCTION INT32_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT64_TO_STRING'
PP_THREAD_SAFE FUNCTION INT64_TO_STRING( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=INT64),        INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), INTENT(OUT)   :: STR
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  CHARACTER(LEN=MAX_STR_LEN) :: TMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_INT64_TO_STRING=2_JPIB_K

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

  ! Initializing the output string
  STR = REPEAT(' ',MAX_STR_LEN)
  TMP = REPEAT(' ',MAX_STR_LEN)

  ! Convert Integer to string
  WRITE(TMP, '(I32)', IOSTAT=WRITE_STATUS) VAL
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_UNABLE_TO_CONVERT_INT64_TO_STRING )

  STR=ADJUSTL(TMP)

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
    CASE(ERRFLAG_UNABLE_TO_CONVERT_INT64_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert INT64 to string' )
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

END FUNCTION INT64_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REAL32_TO_STRING'
PP_THREAD_SAFE FUNCTION REAL32_TO_STRING( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

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
  REAL(KIND=REAL32),          INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), INTENT(OUT)   :: STR
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  CHARACTER(LEN=MAX_STR_LEN)  :: TMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_REAL32_TO_STRING=2_JPIB_K

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

  ! Initializing the output string
  STR = REPEAT(' ',MAX_STR_LEN)
  TMP = REPEAT(' ',MAX_STR_LEN)

  ! Convert Integer to string
  WRITE(TMP, '(F22.8)', IOSTAT=WRITE_STATUS) VAL
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_UNABLE_TO_CONVERT_REAL32_TO_STRING )

  STR=ADJUSTL(TMP)


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
    CASE(ERRFLAG_UNABLE_TO_CONVERT_REAL32_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert REAL32 to string' )
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

END FUNCTION REAL32_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REAL64_TO_STRING'
PP_THREAD_SAFE FUNCTION REAL64_TO_STRING( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

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
  REAL(KIND=REAL64),          INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), INTENT(OUT)   :: STR
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  CHARACTER(LEN=MAX_STR_LEN) :: TMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_REAL64_TO_STRING=2_JPIB_K

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

  ! Initializing the output string
  STR = REPEAT(' ',MAX_STR_LEN)
  TMP = REPEAT(' ',MAX_STR_LEN)

  ! Convert Integer to string
  WRITE(TMP, '(F22.8)', IOSTAT=WRITE_STATUS) VAL
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_UNABLE_TO_CONVERT_REAL64_TO_STRING )

  STR=ADJUSTL(TMP)


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
    CASE(ERRFLAG_UNABLE_TO_CONVERT_REAL64_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert REAL64 to string' )
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

END FUNCTION REAL64_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'BOOL_TO_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION BOOL_TO_STRING_ARRAY( VAL, STR, HOOKS ) RESULT(RET)

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
  LOGICAL,                                               INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: N
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  INTEGER(KIND=JPIB_K)  :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_OUPUT_VARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_BOOL_TO_STRING=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the size of the input string
  N = 1_JPIB_K

  ! Allocate the output string
  ALLOCATE(STR(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_OUPUT_VARS )

  ! Convert bool to string
  PP_TRYCALL(ERRFLAG_CONVERT_BOOL_TO_STRING) BOOL_TO_STRING( VAL, STR(1), HOOKS )


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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_ALLOCATE_OUPUT_VARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate output variables' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_CONVERT_BOOL_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert bool to string' )
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

END FUNCTION BOOL_TO_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'BOOL_ARRAY_TO_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION BOOL_ARRAY_TO_STRING_ARRAY( VAL, STR, HOOKS ) RESULT(RET)

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
  LOGICAL, DIMENSION(:),                                 INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: N
  INTEGER(KIND=JPIB_K)  :: I
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  INTEGER(KIND=JPIB_K)  :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_OUPUT_VARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_BOOL_TO_STRING=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the size of the input string
  N = SIZE(VAL)

  ! Allocate the output string
  ALLOCATE(STR(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_OUPUT_VARS )

  ! Loop over the input array
  DO I = 1, N

    ! Convert bool to string
    PP_TRYCALL(ERRFLAG_CONVERT_BOOL_TO_STRING) BOOL_TO_STRING( VAL(I), STR(I), HOOKS )

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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_ALLOCATE_OUPUT_VARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate output variables' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_CONVERT_BOOL_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert bool to string' )
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

END FUNCTION BOOL_ARRAY_TO_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'BOOL_ARRAY_TO_STRING'
PP_THREAD_SAFE FUNCTION BOOL_ARRAY_TO_STRING( VAL, STR, HOOKS ) RESULT(RET)

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
  LOGICAL, DIMENSION(:),         INTENT(IN)    :: VAL
  CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE :: STR_ARRAY

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_STRING_ARRAY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATE_STRING_ARRAY=5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the string array from the BOOL_ARRAY
  PP_TRYCALL(ERRFLAG_GET_STRING_ARRAY) BOOL_ARRAY_TO_STRING_ARRAY(VAL, STR_ARRAY, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(STR_ARRAY), ERRFLAG_NOT_ALLOCATED )

  ! Convert the string array to a single allocatable string
  PP_TRYCALL(ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING) STRING_ARRAY_TO_STRING(STR_ARRAY, STR, HOOKS)

  ! Deallocate the string array
  DEALLOCATE(STR_ARRAY, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATE_STRING_ARRAY )

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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_GET_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get string array' )
    CASE(ERRFLAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array not allocated after conversion' )
    CASE(ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert string array to string' )
    CASE(ERRFLAG_DEALLOCATE_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free string array' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=DEALLOC_STATUS)
      ENDIF
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

END FUNCTION BOOL_ARRAY_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT8_TO_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION INT8_TO_STRING_ARRAY( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

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
  INTEGER(KIND=INT8),                                    INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: N
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  INTEGER(KIND=JPIB_K)  :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_OUPUT_VARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_INT8_TO_STRING=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the size of the input string
  N = 1_JPIB_K

  ! Allocate the output string
  ALLOCATE(STR(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_OUPUT_VARS )

  ! Convert int8 to string
  PP_TRYCALL(ERRFLAG_CONVERT_INT8_TO_STRING) INT8_TO_STRING( VAL, STR(1), HOOKS )


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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_ALLOCATE_OUPUT_VARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate output variables' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_CONVERT_INT8_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert int8 to string' )
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

END FUNCTION INT8_TO_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT8_ARRAY_TO_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION INT8_ARRAY_TO_STRING_ARRAY( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

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
  INTEGER(KIND=INT8), DIMENSION(:),                      INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: N
  INTEGER(KIND=JPIB_K)  :: I
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  INTEGER(KIND=JPIB_K)  :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_OUPUT_VARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_INT8_TO_STRING=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the size of the input string
  N = SIZE(VAL)

  ! Allocate the output string
  ALLOCATE(STR(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_OUPUT_VARS )

  ! Loop over the input array
  DO I = 1, N

    ! Convert int8 to string
    PP_TRYCALL(ERRFLAG_CONVERT_INT8_TO_STRING) INT8_TO_STRING( VAL(I), STR(I), HOOKS )

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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_ALLOCATE_OUPUT_VARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate output variables' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_CONVERT_INT8_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert int8 to string' )
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

END FUNCTION INT8_ARRAY_TO_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT8_ARRAY_TO_STRING'
PP_THREAD_SAFE FUNCTION INT8_ARRAY_TO_STRING( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

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
  INTEGER(KIND=INT8), DIMENSION(:), INTENT(IN)    :: VAL
  CHARACTER(LEN=:), ALLOCATABLE,    INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE :: STR_ARRAY

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_STRING_ARRAY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATE_STRING_ARRAY=5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the string array from the INT8_ARRAY
  PP_TRYCALL(ERRFLAG_GET_STRING_ARRAY) INT8_ARRAY_TO_STRING_ARRAY(VAL, STR_ARRAY, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(STR_ARRAY), ERRFLAG_NOT_ALLOCATED )

  ! Convert the string array to a single allocatable string
  PP_TRYCALL(ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING) STRING_ARRAY_TO_STRING(STR_ARRAY, STR, HOOKS)

  ! Deallocate the string array
  DEALLOCATE(STR_ARRAY, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATE_STRING_ARRAY )

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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_GET_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get string array' )
    CASE(ERRFLAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array not allocated after conversion' )
    CASE(ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert string array to string' )
    CASE(ERRFLAG_DEALLOCATE_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free string array' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=DEALLOC_STATUS)
      ENDIF
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

END FUNCTION INT8_ARRAY_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT16_TO_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION INT16_TO_STRING_ARRAY( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

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
  INTEGER(KIND=INT16),                                   INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: N
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  INTEGER(KIND=JPIB_K)  :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_OUPUT_VARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_INT16_TO_STRING=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the size of the input string
  N = 1_JPIB_K

  ! Allocate the output string
  ALLOCATE(STR(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_OUPUT_VARS )

  ! Convert int16 to string
  PP_TRYCALL(ERRFLAG_CONVERT_INT16_TO_STRING) INT16_TO_STRING( VAL, STR(1), HOOKS )


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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_ALLOCATE_OUPUT_VARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate output variables' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_CONVERT_INT16_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert int16 to string' )
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

END FUNCTION INT16_TO_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT16_ARRAY_TO_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION INT16_ARRAY_TO_STRING_ARRAY( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

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
  INTEGER(KIND=INT16), DIMENSION(:),                     INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: N
  INTEGER(KIND=JPIB_K)  :: I
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  INTEGER(KIND=JPIB_K)  :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_OUPUT_VARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_INT16_TO_STRING=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the size of the input string
  N = SIZE(VAL)

  ! Allocate the output string
  ALLOCATE(STR(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_OUPUT_VARS )

  ! Loop over the input array
  DO I = 1, N

    ! Convert int16 to string
    PP_TRYCALL(ERRFLAG_CONVERT_INT16_TO_STRING) INT16_TO_STRING( VAL(I), STR(I), HOOKS )

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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_ALLOCATE_OUPUT_VARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate output variables' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_CONVERT_INT16_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert int16 to string' )
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

END FUNCTION INT16_ARRAY_TO_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT16_ARRAY_TO_STRING'
PP_THREAD_SAFE FUNCTION INT16_ARRAY_TO_STRING( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

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
  INTEGER(KIND=INT16), DIMENSION(:), INTENT(IN)    :: VAL
  CHARACTER(LEN=:), ALLOCATABLE,     INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE :: STR_ARRAY

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_STRING_ARRAY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATE_STRING_ARRAY=5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the string array from the INT16_ARRAY
  PP_TRYCALL(ERRFLAG_GET_STRING_ARRAY) INT16_ARRAY_TO_STRING_ARRAY(VAL, STR_ARRAY, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(STR_ARRAY), ERRFLAG_NOT_ALLOCATED )

  ! Convert the string array to a single allocatable string
  PP_TRYCALL(ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING) STRING_ARRAY_TO_STRING(STR_ARRAY, STR, HOOKS)

  ! Deallocate the string array
  DEALLOCATE(STR_ARRAY, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATE_STRING_ARRAY )

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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_GET_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get string array' )
    CASE(ERRFLAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array not allocated after conversion' )
    CASE(ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert string array to string' )
    CASE(ERRFLAG_DEALLOCATE_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free string array' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=DEALLOC_STATUS)
      ENDIF
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

END FUNCTION INT16_ARRAY_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT32_TO_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION INT32_TO_STRING_ARRAY( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

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
  INTEGER(KIND=INT32),                                   INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: N
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  INTEGER(KIND=JPIB_K)  :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_OUPUT_VARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_INT32_TO_STRING=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the size of the input string
  N = 1_JPIB_K

  ! Allocate the output string
  ALLOCATE(STR(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_OUPUT_VARS )

  ! Convert int32 to string
  PP_TRYCALL(ERRFLAG_CONVERT_INT32_TO_STRING) INT32_TO_STRING( VAL, STR(1), HOOKS )


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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_ALLOCATE_OUPUT_VARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate output variables' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_CONVERT_INT32_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert int32 to string' )
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

END FUNCTION INT32_TO_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT32_ARRAY_TO_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION INT32_ARRAY_TO_STRING_ARRAY( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

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
  INTEGER(KIND=INT32), DIMENSION(:),                     INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: N
  INTEGER(KIND=JPIB_K)  :: I
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  INTEGER(KIND=JPIB_K)  :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_OUPUT_VARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_INT32_TO_STRING=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the size of the input string
  N = SIZE(VAL)

  ! Allocate the output string
  ALLOCATE(STR(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_OUPUT_VARS )

  ! Loop over the input array
  DO I = 1, N

    ! Convert int32 to string
    PP_TRYCALL(ERRFLAG_CONVERT_INT32_TO_STRING) INT32_TO_STRING( VAL(I), STR(I), HOOKS )

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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_ALLOCATE_OUPUT_VARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate output variables' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_CONVERT_INT32_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert int32 to string' )
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

END FUNCTION INT32_ARRAY_TO_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT32_ARRAY_TO_STRING'
PP_THREAD_SAFE FUNCTION INT32_ARRAY_TO_STRING( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

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
  INTEGER(KIND=INT32), DIMENSION(:), INTENT(IN)    :: VAL
  CHARACTER(LEN=:), ALLOCATABLE,     INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE :: STR_ARRAY

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_STRING_ARRAY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATE_STRING_ARRAY=5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the string array from the INT32_ARRAY
  PP_TRYCALL(ERRFLAG_GET_STRING_ARRAY) INT32_ARRAY_TO_STRING_ARRAY(VAL, STR_ARRAY, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(STR_ARRAY), ERRFLAG_NOT_ALLOCATED )

  ! Convert the string array to a single allocatable string
  PP_TRYCALL(ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING) STRING_ARRAY_TO_STRING(STR_ARRAY, STR, HOOKS)

  ! Deallocate the string array
  DEALLOCATE(STR_ARRAY, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATE_STRING_ARRAY )

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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_GET_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get string array' )
    CASE(ERRFLAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array not allocated after conversion' )
    CASE(ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert string array to string' )
    CASE(ERRFLAG_DEALLOCATE_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free string array' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=DEALLOC_STATUS)
      ENDIF
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

END FUNCTION INT32_ARRAY_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT64_TO_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION INT64_TO_STRING_ARRAY( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=INT64),                                   INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: N
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  INTEGER(KIND=JPIB_K)  :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_OUPUT_VARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_INT64_TO_STRING=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the size of the input string
  N = 1_JPIB_K

  ! Allocate the output string
  ALLOCATE(STR(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_OUPUT_VARS )

  ! Convert int64 to string
  PP_TRYCALL(ERRFLAG_CONVERT_INT64_TO_STRING) INT64_TO_STRING( VAL, STR(1), HOOKS )


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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_ALLOCATE_OUPUT_VARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate output variables' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_CONVERT_INT64_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert int64 to string' )
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

END FUNCTION INT64_TO_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT64_ARRAY_TO_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION INT64_ARRAY_TO_STRING_ARRAY( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=INT64), DIMENSION(:),                     INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: N
  INTEGER(KIND=JPIB_K)  :: I
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  INTEGER(KIND=JPIB_K)  :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_OUPUT_VARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_INT64_TO_STRING=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the size of the input string
  N = SIZE(VAL)

  ! Allocate the output string
  ALLOCATE(STR(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_OUPUT_VARS )

  ! Loop over the input array
  DO I = 1, N

    ! Convert int64 to string
    PP_TRYCALL(ERRFLAG_CONVERT_INT64_TO_STRING) INT64_TO_STRING( VAL(I), STR(I), HOOKS )

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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_ALLOCATE_OUPUT_VARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate output variables' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_CONVERT_INT64_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert int64 to string' )
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

END FUNCTION INT64_ARRAY_TO_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INT64_ARRAY_TO_STRING'
PP_THREAD_SAFE FUNCTION INT64_ARRAY_TO_STRING( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  INTEGER(KIND=INT64), DIMENSION(:), INTENT(IN)    :: VAL
  CHARACTER(LEN=:), ALLOCATABLE,     INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE :: STR_ARRAY

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_STRING_ARRAY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATE_STRING_ARRAY=5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the string array from the INT64_ARRAY
  PP_TRYCALL(ERRFLAG_GET_STRING_ARRAY) INT64_ARRAY_TO_STRING_ARRAY(VAL, STR_ARRAY, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(STR_ARRAY), ERRFLAG_NOT_ALLOCATED )

  ! Convert the string array to a single allocatable string
  PP_TRYCALL(ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING) STRING_ARRAY_TO_STRING(STR_ARRAY, STR, HOOKS)

  ! Deallocate the string array
  DEALLOCATE(STR_ARRAY, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATE_STRING_ARRAY )

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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_GET_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get string array' )
    CASE(ERRFLAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array not allocated after conversion' )
    CASE(ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert string array to string' )
    CASE(ERRFLAG_DEALLOCATE_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free string array' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=DEALLOC_STATUS)
      ENDIF
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

END FUNCTION INT64_ARRAY_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REAL32_TO_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION REAL32_TO_STRING_ARRAY( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

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
  REAL(KIND=REAL32),                                     INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: N
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  INTEGER(KIND=JPIB_K)  :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_OUPUT_VARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_REAL32_TO_STRING=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the size of the input string
  N = 1_JPIB_K

  ! Allocate the output string
  ALLOCATE(STR(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_OUPUT_VARS )

  ! Convert real32 to string
  PP_TRYCALL(ERRFLAG_CONVERT_REAL32_TO_STRING) REAL32_TO_STRING( VAL, STR(1), HOOKS )


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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_ALLOCATE_OUPUT_VARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate output variables' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_CONVERT_REAL32_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert real32 to string' )
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

END FUNCTION REAL32_TO_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REAL32_ARRAY_TO_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION REAL32_ARRAY_TO_STRING_ARRAY( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

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
  REAL(KIND=REAL32), DIMENSION(:),                       INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: N
  INTEGER(KIND=JPIB_K)  :: I
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  INTEGER(KIND=JPIB_K)  :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_OUPUT_VARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_REAL32_TO_STRING=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the size of the input string
  N = SIZE(VAL)

  ! Allocate the output string
  ALLOCATE(STR(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_OUPUT_VARS )

  ! Loop over the input array
  DO I = 1, N

    ! Convert real32 to string
    PP_TRYCALL(ERRFLAG_CONVERT_REAL32_TO_STRING) REAL32_TO_STRING( VAL(I), STR(I), HOOKS )

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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_ALLOCATE_OUPUT_VARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate output variables' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_CONVERT_REAL32_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert real32 to string' )
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

END FUNCTION REAL32_ARRAY_TO_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REAL32_ARRAY_TO_STRING'
PP_THREAD_SAFE FUNCTION REAL32_ARRAY_TO_STRING( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

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
  REAL(KIND=REAL32), DIMENSION(:), INTENT(IN)    :: VAL
  CHARACTER(LEN=:), ALLOCATABLE,   INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE :: STR_ARRAY

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_STRING_ARRAY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATE_STRING_ARRAY=5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the string array from the REAL32_ARRAY
  PP_TRYCALL(ERRFLAG_GET_STRING_ARRAY) REAL32_ARRAY_TO_STRING_ARRAY(VAL, STR_ARRAY, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(STR_ARRAY), ERRFLAG_NOT_ALLOCATED )

  ! Convert the string array to a single allocatable string
  PP_TRYCALL(ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING) STRING_ARRAY_TO_STRING(STR_ARRAY, STR, HOOKS)

  ! Deallocate the string array
  DEALLOCATE(STR_ARRAY, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATE_STRING_ARRAY )

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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_GET_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get string array' )
    CASE(ERRFLAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array not allocated after conversion' )
    CASE(ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert string array to string' )
    CASE(ERRFLAG_DEALLOCATE_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free string array' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=DEALLOC_STATUS)
      ENDIF
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

END FUNCTION REAL32_ARRAY_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REAL64_TO_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION REAL64_TO_STRING_ARRAY( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

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
  REAL(KIND=REAL64),                                     INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: N
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  INTEGER(KIND=JPIB_K)  :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_OUPUT_VARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_REAL64_TO_STRING=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the size of the input string
  N = 1_JPIB_K

  ! Allocate the output string
  ALLOCATE(STR(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_OUPUT_VARS )

  ! Convert real64 to string
  PP_TRYCALL(ERRFLAG_CONVERT_REAL64_TO_STRING) REAL64_TO_STRING( VAL, STR(1), HOOKS )


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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_ALLOCATE_OUPUT_VARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate output variables' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_CONVERT_REAL64_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert real64 to string' )
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

END FUNCTION REAL64_TO_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REAL64_ARRAY_TO_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION REAL64_ARRAY_TO_STRING_ARRAY( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

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
  REAL(KIND=REAL64), DIMENSION(:),                       INTENT(IN)    :: VAL
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: N
  INTEGER(KIND=JPIB_K)  :: I
  INTEGER(KIND=JPIB_K)  :: WRITE_STATUS
  INTEGER(KIND=JPIB_K)  :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=MAX_STR_LEN) :: TMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_OUPUT_VARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_REAL64_TO_STRING=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the size of the input string
  N = SIZE(VAL)

  ! Allocate the output string
  ALLOCATE(STR(N), STAT=ALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATE_OUPUT_VARS )

  ! Loop over the input array
  DO I = 1, N

    ! Convert real64 to string
    PP_TRYCALL(ERRFLAG_CONVERT_REAL64_TO_STRING) REAL64_TO_STRING( VAL(I), TMP, HOOKS )

    ! Copy the string to the output array
    STR(I) = TMP

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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_ALLOCATE_OUPUT_VARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate output variables' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=ALLOC_STATUS)
      ENDIF
    CASE(ERRFLAG_CONVERT_REAL64_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert real64 to string' )
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

END FUNCTION REAL64_ARRAY_TO_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'REAL64_ARRAY_TO_STRING'
PP_THREAD_SAFE FUNCTION REAL64_ARRAY_TO_STRING( VAL, STR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

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
  REAL(KIND=REAL64), DIMENSION(:), INTENT(IN)    :: VAL
  CHARACTER(LEN=:), ALLOCATABLE,   INTENT(INOUT) :: STR
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), ALLOCATABLE :: STR_ARRAY

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_STRING_ARRAY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATE_STRING_ARRAY=5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(STR), ERRFLAG_ALREADY_ALLOCATED )

  ! Get the string array from the REAL64_ARRAY
  PP_TRYCALL(ERRFLAG_GET_STRING_ARRAY) REAL64_ARRAY_TO_STRING_ARRAY(VAL, STR_ARRAY, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(STR_ARRAY), ERRFLAG_NOT_ALLOCATED )

  ! Convert the string array to a single allocatable string
  PP_TRYCALL(ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING) STRING_ARRAY_TO_STRING(STR_ARRAY, STR, HOOKS)

  ! Deallocate the string array
  DEALLOCATE(STR_ARRAY, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATE_STRING_ARRAY )

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
    CASE(ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array already allocated' )
    CASE(ERRFLAG_GET_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get string array' )
    CASE(ERRFLAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array not allocated after conversion' )
    CASE(ERRFLAG_CONVERT_STRING_ARRAY_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert string array to string' )
    CASE(ERRFLAG_DEALLOCATE_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free string array' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG,STAT=DEALLOC_STATUS)
      ENDIF
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

END FUNCTION REAL64_ARRAY_TO_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE LOG_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
