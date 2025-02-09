! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'encoder_tracer_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ENCODER_TRACER_MOD'
MODULE ENCODER_TRACER_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN

IMPLICIT NONE

! Default visibility
PRIVATE

! Define possible callback types
INTEGER(KIND=JPIB_K), PARAMETER :: SET_BOOL=1_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_INT8=2_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_INT16=3_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_INT32=4_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_INT64=5_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_REAL32=6_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_REAL64=7_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_INT8_ARRAY=8_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_INT16_ARRAY=9_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_INT32_ARRAY=10_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_INT64_ARRAY=11_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_REAL32_ARRAY=12_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_REAL64_ARRAY=13_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_BOOL_ARRAY=14_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_STRING=15_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SET_MISSING=16_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: LOAD_SAMPLE=17_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: COPY_SAMPLE=18_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: START_ENCODING=19_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ENTER_PROCEDURE=20_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: EXIT_PROCEDURE=21_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: END_ENCODING=22_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: LAST_CALLBACK=23_JPIB_K

TYPE :: ENCODER_TRACER_NODE_T
  CHARACTER(LEN=:), ALLOCATABLE :: SECTION_TYPE
  CHARACTER(LEN=:), ALLOCATABLE :: SECTION_NAME
  CHARACTER(LEN=:), ALLOCATABLE :: PROC_TYPE
  CHARACTER(LEN=:), ALLOCATABLE :: PROC_NAME
  INTEGER(KIND=JPIB_K) :: LINE
  INTEGER(KIND=JPIB_K) :: CALLBACK
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), POINTER :: PAYLOAD => NULL()
  TYPE(ENCODER_TRACER_NODE_T), POINTER :: PREV => NULL()
  TYPE(ENCODER_TRACER_NODE_T), POINTER :: NEXT => NULL()
END TYPE


TYPE :: ENCODER_TRACER_T
  PRIVATE
  TYPE(ENCODER_TRACER_NODE_T), POINTER :: HEAD => NULL()
  TYPE(ENCODER_TRACER_NODE_T), POINTER :: TAIL => NULL()
  INTEGER(KIND=JPIB_K) :: SIZE
CONTAINS
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: PUSH    => ENCODER_TRACER_PUSH
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: COPY_TO => ENCODER_TRACER_COPY_TO
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: TO_YAML => ENCODER_TRACER_TO_YAML
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: FREE    => ENCODER_TRACER_FREE
END TYPE

!> Whitelist of public symbols (datatypes)
PUBLIC :: ENCODER_TRACER_T

!> Whitelist of public symbols (procedures)
PUBLIC :: ICALLBACK2CCALLBACK
PUBLIC :: CCALLBACK2ICALLBACK

!> Whitelist of public symbols (parameters)
PUBLIC :: SET_BOOL
PUBLIC :: SET_INT8
PUBLIC :: SET_INT16
PUBLIC :: SET_INT32
PUBLIC :: SET_INT64
PUBLIC :: SET_REAL32
PUBLIC :: SET_REAL64
PUBLIC :: SET_INT8_ARRAY
PUBLIC :: SET_INT16_ARRAY
PUBLIC :: SET_INT32_ARRAY
PUBLIC :: SET_INT64_ARRAY
PUBLIC :: SET_REAL32_ARRAY
PUBLIC :: SET_REAL64_ARRAY
PUBLIC :: SET_BOOL_ARRAY
PUBLIC :: SET_STRING
PUBLIC :: SET_MISSING
PUBLIC :: LOAD_SAMPLE
PUBLIC :: COPY_SAMPLE
PUBLIC :: START_ENCODING
PUBLIC :: ENTER_PROCEDURE
PUBLIC :: EXIT_PROCEDURE
PUBLIC :: END_ENCODING

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ICALLBACK2CCALLBACK'
PP_THREAD_SAFE FUNCTION ICALLBACK2CCALLBACK( ICALLBACK, CCALLBACK, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ICALLBACK
  CHARACTER(LEN=16),    INTENT(OUT)   :: CCALLBACK
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_CALLBACK=1_JPIB_K

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

  !> Initialization of the output variable
  CCALLBACK = REPEAT(' ', 16)

  !> Select the prefix
  SELECT CASE ( ICALLBACK )

  CASE (  SET_BOOL )
    CCALLBACK = 'set_bool'

  CASE ( SET_INT8 )
    CCALLBACK = 'set_int8'

  CASE (  SET_INT16 )
    CCALLBACK = 'set_int16'

  CASE ( SET_INT32 )
    CCALLBACK = 'set_int32'

  CASE (  SET_INT64 )
    CCALLBACK = 'set_int64'

  CASE (  SET_REAL32 )
    CCALLBACK = 'set_real32'

  CASE (  SET_REAL64 )
    CCALLBACK = 'set_real64'

  CASE ( SET_INT8_ARRAY )
    CCALLBACK = 'set_int8_array'

  CASE ( SET_INT16_ARRAY )
    CCALLBACK = 'set_int8_array'

  CASE ( SET_INT32_ARRAY )
    CCALLBACK = 'set_int8_array'

  CASE ( SET_INT64_ARRAY )
    CCALLBACK = 'set_int8_array'

  CASE ( SET_REAL32_ARRAY )
    CCALLBACK = 'set_real32_array'

  CASE ( SET_REAL64_ARRAY )
    CCALLBACK = 'set_real64_array'

  CASE ( SET_BOOL_ARRAY )
    CCALLBACK = 'set_bool_array'

  CASE ( SET_STRING )
    CCALLBACK = 'set_string'

  CASE ( SET_MISSING )
    CCALLBACK = 'set_missing'

  CASE ( LOAD_SAMPLE )
    CCALLBACK = 'load_sample'

  CASE ( COPY_SAMPLE )
    CCALLBACK = 'copy_sample'

  CASE ( START_ENCODING )
    CCALLBACK = 'start_encoding'

  CASE ( ENTER_PROCEDURE )
    CCALLBACK = 'enter_procedure'

  CASE ( EXIT_PROCEDURE )
    CCALLBACK = 'exit_procedure'

  CASE (  END_ENCODING )
    CCALLBACK = 'end_encoding'

  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_CALLBACK )
  END SELECT

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
    CHARACTER(LEN=16) :: TMPSTR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNKNOWN_CALLBACK)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) ICALLBACK
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown callback: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION ICALLBACK2CCALLBACK
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CCALLBACK2ICALLBACK'
PP_THREAD_SAFE FUNCTION CCALLBACK2ICALLBACK( CCALLBACK, ICALLBACK, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: CCALLBACK
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: ICALLBACK
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CCALLBACK)) :: LOC_CCALLBACK

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_ICALLBACK=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=2_JPIB_K


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )


  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CCALLBACK, LOC_CCALLBACK, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CCALLBACK)) )

  CASE ( 'set_bool' )
    ICALLBACK = SET_BOOL
  CASE ( 'set_int8' )
    ICALLBACK = SET_INT8
  CASE ( 'set_int16' )
    ICALLBACK = SET_INT16
  CASE ( 'set_int32' )
    ICALLBACK = SET_INT32
  CASE ( 'set_int64' )
    ICALLBACK = SET_INT64
  CASE ( 'set_real32' )
    ICALLBACK = SET_REAL32
  CASE ( 'set_real64' )
    ICALLBACK = SET_REAL64
  CASE ( 'set_int8_array' )
    ICALLBACK = SET_INT8_ARRAY
  CASE ( 'set_int16_array' )
    ICALLBACK = SET_INT16_ARRAY
  CASE ( 'set_int32_array' )
    ICALLBACK = SET_INT32_ARRAY
  CASE ( 'set_int64_array' )
    ICALLBACK = SET_INT64_ARRAY
  CASE ( 'set_real32_array' )
    ICALLBACK = SET_REAL32_ARRAY
  CASE ( 'set_real64_array' )
    ICALLBACK = SET_REAL64_ARRAY
  CASE ( 'set_bool_array' )
    ICALLBACK = SET_BOOL_ARRAY
  CASE ( 'set_string' )
    ICALLBACK = SET_STRING
  CASE ( 'set_missing' )
    ICALLBACK = SET_MISSING
  CASE ( 'load_sample' )
    ICALLBACK = LOAD_SAMPLE
  CASE ( 'copy_sample' )
    ICALLBACK = COPY_SAMPLE
  CASE ( 'start_encoding' )
    ICALLBACK = START_ENCODING
  CASE ( 'enter_procedure' )
    ICALLBACK = ENTER_PROCEDURE
  CASE ( 'exit_procedure' )
    ICALLBACK = EXIT_PROCEDURE
  CASE ( 'end_encoding' )
    ICALLBACK = END_ENCODING
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_ICALLBACK )
  END SELECT

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
    CASE (ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
    CASE (ERRFLAG_UNKNOWN_ICALLBACK)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown ccallback: '//TRIM(ADJUSTL(CCALLBACK)) )
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

END FUNCTION CCALLBACK2ICALLBACK
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODER_TRACER_PUSH'
PP_THREAD_SAFE FUNCTION ENCODER_TRACER_PUSH( THIS, &
&      SECTION_NAME, SECTION_TYPE, PROC_NAME, PROC_TYPE, &
&      IN_LINE, CALLBACK, HOOKS, PAYLOAD ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ENCODER_TRACER_T),                                     INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                                            INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),                                            INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),                                            INTENT(IN)    :: PROC_NAME
  CHARACTER(LEN=*),                                            INTENT(IN)    :: PROC_TYPE
  INTEGER(KIND=JPIB_K),                                        INTENT(IN)    :: IN_LINE
  INTEGER(KIND=JPIB_K),                                        INTENT(IN)    :: CALLBACK
  TYPE(HOOKS_T),                                               INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=MAX_STR_LEN), DIMENSION(:), POINTER, OPTIONAL, INTENT(IN)    :: PAYLOAD

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  TYPE(ENCODER_TRACER_NODE_T), POINTER :: NODE

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NODE_ALLOC_FAILED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_CALLBACK = 2_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Check if the callback is valid
  PP_DEBUG_CRITICAL_COND_THROW( CALLBACK.LT.SET_INT8,    ERRFLAG_WRONG_CALLBACK )
  PP_DEBUG_CRITICAL_COND_THROW( CALLBACK.GT.LOAD_SAMPLE, ERRFLAG_WRONG_CALLBACK )
  ! Allocate a new node
  ALLOCATE( NODE, STAT=STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( STATUS.NE.0, ERRFLAG_NODE_ALLOC_FAILED )

  ! Initialize the new node
  NODE%SECTION_NAME = TRIM(ADJUSTL(SECTION_NAME))
  NODE%SECTION_TYPE = TRIM(ADJUSTL(SECTION_TYPE))
  NODE%PROC_TYPE = TRIM(ADJUSTL(PROC_TYPE))
  NODE%PROC_NAME = TRIM(ADJUSTL(PROC_NAME))
  NODE%LINE = IN_LINE
  NODE%CALLBACK = CALLBACK
  IF ( PRESENT(PAYLOAD) ) THEN
    NODE%PAYLOAD => PAYLOAD
  ELSE
    NODE%PAYLOAD => NULL()
  ENDIF
  NODE%NEXT => NULL()
  NODE%PREV => NULL()

  ! Add the new node to the list
  IF ( .NOT.ASSOCIATED(THIS%HEAD) ) THEN
    THIS%HEAD => NODE
    THIS%TAIL => NODE
    THIS%SIZE = 0_JPIB_K
  ELSE
    THIS%TAIL%NEXT => NODE
    NODE%PREV => THIS%TAIL
    THIS%TAIL => NODE
    THIS%SIZE = THIS%SIZE + 1_JPIB_K
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

    CHARACTER(LEN=32) :: TMP1
    CHARACTER(LEN=32) :: TMP2
    CHARACTER(LEN=32) :: TMP3

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_WRONG_CALLBACK)
      TMP1 = REPEAT(' ', 32)
      WRITE(TMP1, '(I32)', IOSTAT=STATUS) CALLBACK
      TMP2 = REPEAT(' ', 32)
      WRITE(TMP2, '(I32)', IOSTAT=STATUS) SET_INT8
      TMP3 = REPEAT(' ', 32)
      WRITE(TMP3, '(I32)', IOSTAT=STATUS) LAST_CALLBACK
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong callback type' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'callback: '//TRIM(ADJUSTL(TMP1)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'valid range: ['//TRIM(ADJUSTL(TMP2))//':'//TRIM(ADJUSTL(TMP3))//']' )
    CASE(ERRFLAG_NODE_ALLOC_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to allocate a new node' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=STATUS)
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

END FUNCTION ENCODER_TRACER_PUSH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODER_TRACER_COPY_TO'
PP_THREAD_SAFE FUNCTION ENCODER_TRACER_COPY_TO( THIS, OTHER, HOOKS ) RESULT(RET)

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
  CLASS(ENCODER_TRACER_T), INTENT(INOUT) :: THIS
  CLASS(ENCODER_TRACER_T), INTENT(INOUT) :: OTHER
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(ENCODER_TRACER_NODE_T), POINTER :: TMP

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NODE_DEALLOC_FAILED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_OTHER = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PUSH_OTHER = 3_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  !> Free other tracer
  PP_TRYCALL(ERRFLAG_FREE_OTHER) OTHER%FREE( HOOKS )

  ! Free all nodes
  TMP => THIS%HEAD
  DO WHILE ( ASSOCIATED(TMP) )

    ! Push current node
    IF ( ASSOCIATED(TMP%PAYLOAD) ) THEN
      PP_TRYCALL(ERRFLAG_PUSH_OTHER) OTHER%PUSH( TMP%SECTION_NAME, &
&        TMP%SECTION_TYPE, TMP%PROC_NAME, TMP%PROC_TYPE, TMP%LINE, &
&        TMP%CALLBACK, HOOKS, TMP%PAYLOAD )
    ELSE
      PP_TRYCALL(ERRFLAG_PUSH_OTHER) OTHER%PUSH( TMP%SECTION_NAME, &
&        TMP%SECTION_TYPE, TMP%PROC_NAME, TMP%PROC_TYPE, TMP%LINE, &
&        TMP%CALLBACK, HOOKS )
    ENDIF

    ! Go to next pointer
    TMP => TMP%NEXT

  ENDDO

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
    CASE (ERRFLAG_NODE_DEALLOC_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to deallocate a node' )
    CASE (ERRFLAG_FREE_OTHER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to free other tracer' )
    CASE (ERRFLAG_PUSH_OTHER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to push a node to other tracer' )
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

END FUNCTION ENCODER_TRACER_COPY_TO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODER_TRACER_TO_YAML'
PP_THREAD_SAFE FUNCTION ENCODER_TRACER_TO_YAML( THIS, UNIT, HOOKS ) RESULT(RET)

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
  CLASS(ENCODER_TRACER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),    INTENT(INOUT) :: UNIT
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: OFFSET
  CHARACTER(LEN=16) :: CCALLBACK
  CHARACTER(LEN=32) :: CTMP
  CHARACTER(LEN=2) :: SEP
  TYPE(ENCODER_TRACER_NODE_T), POINTER :: TMP

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NODE_DEALLOC_FAILED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_FAILED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ICALLBACK2CCALLBACK = 3_JPIB_K


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  WRITE(UNIT, '(A)', IOSTAT=WRITE_STATUS) 'encoding-report:'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_FAILED )
  OFFSET = 2

  ! Free all nodes
  TMP => THIS%HEAD
  DO WHILE ( ASSOCIATED(TMP) )

    ! Get the name of the callback
    PP_TRYCALL(ERRFLAG_ICALLBACK2CCALLBACK) ICALLBACK2CCALLBACK( TMP%CALLBACK, CCALLBACK, HOOKS )

    ! Write the next Item
    WRITE(UNIT, '(A)', IOSTAT=WRITE_STATUS) REPEAT( ' ', OFFSET )//'- encoding-item: "'//TRIM(ADJUSTL(CCALLBACK))//'"'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_FAILED )
    OFFSET = OFFSET + 2

    ! Write the section name
    WRITE(UNIT, '(A)', IOSTAT=WRITE_STATUS) REPEAT( ' ', OFFSET )//'section-name: "'//TRIM(ADJUSTL(TMP%SECTION_NAME))//'"'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_FAILED )

    ! Write the section type
    WRITE(UNIT, '(A)', IOSTAT=WRITE_STATUS) REPEAT( ' ', OFFSET )//'section-type: "'//TRIM(ADJUSTL(TMP%SECTION_TYPE))//'"'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_FAILED )

    ! Write the procedure name
    WRITE(UNIT, '(A)', IOSTAT=WRITE_STATUS) REPEAT( ' ', OFFSET )//'procedure-name: "'//TRIM(ADJUSTL(TMP%PROC_NAME))//'"'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_FAILED )

    ! Write the procedure type
    WRITE(UNIT, '(A)', IOSTAT=WRITE_STATUS) REPEAT( ' ', OFFSET )//'procedure-type: "'//TRIM(ADJUSTL(TMP%PROC_TYPE))//'"'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_FAILED )

    ! Write the line number
    CTMP = REPEAT( ' ', 32 )
    WRITE(CTMP,'(I32)') TMP%LINE
    WRITE(UNIT, '(A,I8)', IOSTAT=WRITE_STATUS) REPEAT( ' ', OFFSET )//'line: '//TRIM(ADJUSTL(CTMP))

    ! Write the payload
    IF ( ASSOCIATED(TMP%PAYLOAD) ) THEN
      IF ( SIZE(TMP%PAYLOAD) .EQ. 1 ) THEN
        WRITE(UNIT, '(A)', IOSTAT=WRITE_STATUS) REPEAT( ' ', OFFSET )//'payload: '//TRIM(ADJUSTL(TMP%PAYLOAD(1)))
        PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_FAILED )
      ELSE
        WRITE(UNIT, '(A)', IOSTAT=WRITE_STATUS) REPEAT( ' ', OFFSET )//'payload: ['
        PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_FAILED )
        OFFSET = OFFSET + 2
        SEP = ', '
        DO I = 1, SIZE(TMP%PAYLOAD)
          IF ( I.EQ.1 .OR. MOD( I-1, 10 ) .EQ. 0 ) THEN
            IF ( I .EQ. SIZE(TMP%PAYLOAD) ) THEN
              WRITE(UNIT, '(A)', IOSTAT=WRITE_STATUS, ADVANCE='NO') REPEAT( ' ', OFFSET )//TRIM(ADJUSTL(TMP%PAYLOAD(I)))
              PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_FAILED )
            ELSE
              WRITE(UNIT, '(A)', IOSTAT=WRITE_STATUS, ADVANCE='NO') REPEAT( ' ', OFFSET )//TRIM(ADJUSTL(TMP%PAYLOAD(I)))//SEP
              PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_FAILED )
            ENDIF
          ELSE
            IF ( I .EQ. SIZE(TMP%PAYLOAD) ) THEN
              WRITE(UNIT, '(A)', IOSTAT=WRITE_STATUS) REPEAT( ' ', OFFSET )//TRIM(ADJUSTL(TMP%PAYLOAD(I)))
              PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_FAILED )
            ELSE
              WRITE(UNIT, '(A)', IOSTAT=WRITE_STATUS) REPEAT( ' ', OFFSET )//TRIM(ADJUSTL(TMP%PAYLOAD(I)))//SEP
              PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_FAILED )
            ENDIF
          ENDIF
        END DO
        OFFSET = OFFSET - 2_JPIB_K
        WRITE(UNIT, '(A)', IOSTAT=WRITE_STATUS) REPEAT( ' ', OFFSET )//']'
        PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_FAILED )
        WRITE(UNIT, '(A)', IOSTAT=WRITE_STATUS) REPEAT( ' ', OFFSET )
        PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS.NE.0, ERRFLAG_WRITE_FAILED )
      ENDIF
    ENDIF
    OFFSET = OFFSET - 2_JPIB_K

    ! Go to next pointer
    TMP => TMP%NEXT

  ENDDO

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
    CASE (ERRFLAG_NODE_DEALLOC_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to deallocate a node' )
    CASE (ERRFLAG_WRITE_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to write to the unit' )
    CASE (ERRFLAG_ICALLBACK2CCALLBACK)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to convert icallback to ccallback' )
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

END FUNCTION ENCODER_TRACER_TO_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODER_TRACER_FREE'
PP_THREAD_SAFE FUNCTION ENCODER_TRACER_FREE( THIS, HOOKS ) RESULT(RET)

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
  CLASS(ENCODER_TRACER_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  TYPE(ENCODER_TRACER_NODE_T), POINTER :: NODE
  CLASS(ENCODER_TRACER_NODE_T), POINTER :: TMP

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NODE_DEALLOC_FAILED = 1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Free all nodes
  DO WHILE ( ASSOCIATED(THIS%HEAD) )

    ! Save head node
    TMP => THIS%HEAD
    THIS%HEAD => TMP%NEXT

    ! Deallocate the node members
    IF (ALLOCATED(TMP%SECTION_NAME)) THEN
      DEALLOCATE(TMP%SECTION_NAME, STAT=STATUS, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( STATUS.NE.0, ERRFLAG_NODE_DEALLOC_FAILED )
    ENDIF

    IF (ALLOCATED(TMP%SECTION_TYPE)) THEN
      DEALLOCATE(TMP%SECTION_TYPE, STAT=STATUS, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( STATUS.NE.0, ERRFLAG_NODE_DEALLOC_FAILED )
    ENDIF

    IF (ALLOCATED(TMP%PROC_NAME)) THEN
      DEALLOCATE(TMP%PROC_NAME, STAT=STATUS, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( STATUS.NE.0, ERRFLAG_NODE_DEALLOC_FAILED )
    ENDIF

    IF (ALLOCATED(TMP%PROC_TYPE)) THEN
      DEALLOCATE(TMP%PROC_TYPE, STAT=STATUS, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( STATUS.NE.0, ERRFLAG_NODE_DEALLOC_FAILED )
    ENDIF

    IF (ASSOCIATED(TMP%PAYLOAD)) THEN
      NULLIFY(TMP%PAYLOAD)
    ENDIF

    IF (ASSOCIATED(TMP%PREV)) THEN
      NULLIFY(TMP%PREV)
    ENDIF

    IF (ASSOCIATED(TMP%NEXT)) THEN
      NULLIFY(TMP%NEXT)
    ENDIF

    ! Deallocate the node itself
    DEALLOCATE( TMP, STAT=STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( STATUS.NE.0, ERRFLAG_NODE_DEALLOC_FAILED )

    ! Reduce the size
    TMP => NULL()
    THIS%SIZE = THIS%SIZE - 1_JPIB_K

  ENDDO

  ! Reset the class
  THIS%HEAD => NULL()
  THIS%TAIL => NULL()
  THIS%SIZE = 0_JPIB_K

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
    CASE(ERRFLAG_NODE_DEALLOC_FAILED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to deallocate a node' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=STATUS)
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

END FUNCTION ENCODER_TRACER_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE ENCODER_TRACER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
