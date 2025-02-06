!> @file om_message_mod.F90
!>
!> @brief Definition of a message for the output manager
!>
!> This module defines the message used by the output manager
!> and some utitlities to handle these messages
!>
!> @author Mirco Valentini
!> @date February 10, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


! Definition of the module
#define PP_FILE_NAME 'ifs_par_serialization_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'IFS_PAR_SERIALIZATION_MOD'
MODULE IFS_PAR_SERIALIZATION_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K

IMPLICIT NONE

! Default visibility of the module
PRIVATE


! Serialized parameters
TYPE :: MODEL_PAR_SERIAL_T
  INTEGER(KIND=JPIB_K), DIMENSION(44) :: DIMS_
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: IBUF_
  REAL(KIND=JPRD_K),    DIMENSION(:), ALLOCATABLE :: RBUF_
  CHARACTER(LEN=16),    DIMENSION(:), ALLOCATABLE :: CBUF_
END TYPE


! Whitelist of public symbols (datatypes)
PUBLIC :: MODEL_PAR_SERIAL_T

! Whitelist of public symbols (procedures)
PUBLIC :: PAR_SERIALIZE
PUBLIC :: PAR_DESERIALIZE

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PAR_SERIALIZE'
PP_THREAD_SAFE FUNCTION PAR_SERIALIZE( DATA_RAW, DATA_SERIAL, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),        INTENT(IN)    :: DATA_RAW
  TYPE(MODEL_PAR_SERIAL_T), INTENT(OUT)   :: DATA_SERIAL
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_IBUF=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_RBUF=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED_CBUF=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATING_IBUF=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATING_RBUF=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATING_CBUF=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SERIALIZE_IPAR=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SERIALIZE_RPAR=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SERIALIZE_CPAR=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_COMPUTE_SIZE=10_JPIB_K

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

  ! Compute the size of the arrays
  PP_TRYCALL(ERRFLAG_COMPUTE_SIZE) COMPUTE_SIZE( DATA_RAW, DATA_SERIAL%DIMS_, HOOKS)

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED( DATA_SERIAL%IBUF_ ), ERRFLAG_ALREADY_ALLOCATED_IBUF )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED( DATA_SERIAL%RBUF_ ), ERRFLAG_ALREADY_ALLOCATED_RBUF )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED( DATA_SERIAL%CBUF_ ), ERRFLAG_ALREADY_ALLOCATED_CBUF )

  ! Allocate arrays
  ALLOCATE( DATA_SERIAL%IBUF_(DATA_SERIAL%DIMS_(1)), STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATING_IBUF )
  ALLOCATE( DATA_SERIAL%RBUF_(DATA_SERIAL%DIMS_(2)), STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATING_RBUF )
  ALLOCATE( DATA_SERIAL%CBUF_(DATA_SERIAL%DIMS_(3)), STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATING_CBUF )

  ! Fill serialized arrays
  PP_TRYCALL(ERRFLAG_SERIALIZE_IPAR) SERIALIZE_INTEGER_PARAMS( DATA_RAW, DATA_SERIAL%IBUF_, HOOKS )
  PP_TRYCALL(ERRFLAG_SERIALIZE_RPAR) SERIALIZE_REAL_PARAMS(    DATA_RAW, DATA_SERIAL%RBUF_, HOOKS )
  PP_TRYCALL(ERRFLAG_SERIALIZE_CPAR) SERIALIZE_STRING_PARAMS(  DATA_RAW, DATA_SERIAL%CBUF_, HOOKS )

  ! Paranoid check
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG, STAT=STAT)
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
    CASE (ERRFLAG_ALREADY_ALLOCATED_IBUF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Integer buffer already allocated' )
    CASE (ERRFLAG_ALREADY_ALLOCATED_RBUF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Real buffer already allocated' )
    CASE (ERRFLAG_ALREADY_ALLOCATED_CBUF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String buffer already allocated' )
    CASE (ERRFLAG_ALLOCATING_IBUF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error allocating integer buffer' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( ERRMSG )
        DEALLOCATE(ERRMSG, STAT=STAT)
      ENDIF
    CASE (ERRFLAG_ALLOCATING_RBUF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error allocating real buffer' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( ERRMSG )
        DEALLOCATE(ERRMSG, STAT=STAT)
      ENDIF
    CASE (ERRFLAG_ALLOCATING_CBUF)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error allocating string buffer' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( ERRMSG )
        DEALLOCATE(ERRMSG, STAT=STAT)
      ENDIF
    CASE (ERRFLAG_SERIALIZE_IPAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error serializing integer parameters' )
    CASE (ERRFLAG_SERIALIZE_RPAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error serializing real parameters' )
    CASE (ERRFLAG_SERIALIZE_CPAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error serializing string parameters' )
    CASE (ERRFLAG_COMPUTE_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error computing size of the arrays' )
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

END FUNCTION PAR_SERIALIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PAR_DESERIALIZE'
PP_THREAD_SAFE FUNCTION PAR_DESERIALIZE( DATA_SERIAL, DATA_RAW, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_SERIAL_T), INTENT(IN)    :: DATA_SERIAL
  TYPE(MODEL_PAR_T),        INTENT(OUT)   :: DATA_RAW
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IBUF_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RBUF_NOT_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CBUF_NOT_ALLOCATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DESERIALIZE_IPAR=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DESERIALIZE_RPAR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DESERIALIZE_CPAR=6_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_SERIAL%IBUF_ ), ERRFLAG_IBUF_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_SERIAL%RBUF_ ), ERRFLAG_RBUF_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_SERIAL%CBUF_ ), ERRFLAG_CBUF_NOT_ALLOCATED )

  ! Fill serialized arrays
  PP_TRYCALL(ERRFLAG_DESERIALIZE_IPAR) DESERIALIZE_INTEGER_PARAMS( DATA_SERIAL%IBUF_, DATA_SERIAL%DIMS_, DATA_RAW, HOOKS )
  PP_TRYCALL(ERRFLAG_DESERIALIZE_RPAR) DESERIALIZE_REAL_PARAMS(    DATA_SERIAL%RBUF_, DATA_SERIAL%DIMS_, DATA_RAW, HOOKS )
  PP_TRYCALL(ERRFLAG_DESERIALIZE_CPAR) DESERIALIZE_STRING_PARAMS(  DATA_SERIAL%CBUF_, DATA_SERIAL%DIMS_, DATA_RAW, HOOKS )

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
    CASE (ERRFLAG_IBUF_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Integer buffer not allocated' )
    CASE (ERRFLAG_RBUF_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Real buffer not allocated' )
    CASE (ERRFLAG_CBUF_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String buffer not allocated' )
    CASE (ERRFLAG_DESERIALIZE_IPAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error deserializing integer parameters' )
    CASE (ERRFLAG_DESERIALIZE_RPAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error deserializing real parameters' )
    CASE (ERRFLAG_DESERIALIZE_CPAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error deserializing string parameters' )
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

END FUNCTION PAR_DESERIALIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMPUTE_SIZE'
PP_THREAD_SAFE FUNCTION  COMPUTE_SIZE( DATA_RAW, SZ, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),                   INTENT(IN)    :: DATA_RAW
  INTEGER(KIND=JPIB_K), DIMENSION(44), INTENT(OUT)   :: SZ
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Function result
  INTEGER(KIND=JPIB_K), PARAMETER :: N_LOGICAL_SCALAR=9
  INTEGER(KIND=JPIB_K), PARAMETER :: N_INTEGER_SCALAR=70
  INTEGER(KIND=JPIB_K), PARAMETER :: N_REAL_SCALAR=19
  INTEGER(KIND=JPIB_K), PARAMETER :: N_STRING_SCALAR=9

  INTEGER(KIND=JPIB_K), PARAMETER :: N_INTEGER_ARRAY=7
  INTEGER(KIND=JPIB_K), PARAMETER :: N_REAL_ARRAY=4
  INTEGER(KIND=JPIB_K), PARAMETER :: N_STRING_ARRAY=0

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

  SZ(4) = N_LOGICAL_SCALAR+N_INTEGER_SCALAR
  SZ(5) = N_REAL_SCALAR
  SZ(6) = N_STRING_SCALAR

  SZ(7) = N_INTEGER_ARRAY
  SZ(8) = N_REAL_ARRAY
  SZ(9) = N_STRING_ARRAY

  ! NSFLEVS
  IF ( ALLOCATED(DATA_RAW%GEO_%NSFLEVS) ) THEN
    SZ(10) = SIZE(DATA_RAW%GEO_%NSFLEVS)
    SZ(11) = LBOUND(DATA_RAW%GEO_%NSFLEVS,1)
    SZ(12) = UBOUND(DATA_RAW%GEO_%NSFLEVS,1)
    SZ(13) = LBOUND(DATA_RAW%GEO_%NSFLEVS,2)
    SZ(14) = UBOUND(DATA_RAW%GEO_%NSFLEVS,2)
  ELSE
    SZ(10) = INT(0,JPIB_K)
    SZ(11) = INT(-99,JPIB_K)
    SZ(12) = INT(-99,JPIB_K)
    SZ(13) = INT(-99,JPIB_K)
    SZ(14) = INT(-99,JPIB_K)
  ENDIF


  ! ILOENG
  IF ( ALLOCATED(DATA_RAW%GEO_%ILOENG) ) THEN
    SZ(15) = SIZE(DATA_RAW%GEO_%ILOENG)
    SZ(16) = LBOUND(DATA_RAW%GEO_%ILOENG,1)
    SZ(17) = UBOUND(DATA_RAW%GEO_%ILOENG,1)
  ELSE
    SZ(15) = INT(0,JPIB_K)
    SZ(16) = INT(-99,JPIB_K)
    SZ(17) = INT(-99,JPIB_K)
  ENDIF


  ! NLONRGG
  IF ( ALLOCATED(DATA_RAW%WAM_%NLONRGG) ) THEN
    SZ(18) = SIZE(DATA_RAW%WAM_%NLONRGG)
    SZ(19) = LBOUND(DATA_RAW%WAM_%NLONRGG,1)
    SZ(20) = UBOUND(DATA_RAW%WAM_%NLONRGG,1)
  ELSE
    SZ(18) = INT(0,JPIB_K)
    SZ(19) = INT(-99,JPIB_K)
    SZ(20) = INT(-99,JPIB_K)
  ENDIF

  ! MSERIES
  IF ( ALLOCATED(DATA_RAW%SAT_%MSERIES) ) THEN
    SZ(21) = SIZE(DATA_RAW%SAT_%MSERIES)
    SZ(22) = LBOUND(DATA_RAW%SAT_%MSERIES,1)
    SZ(23) = UBOUND(DATA_RAW%SAT_%MSERIES,1)
  ELSE
    SZ(22) = INT(0,JPIB_K)
    SZ(22) = INT(-99,JPIB_K)
    SZ(23) = INT(-99,JPIB_K)
  ENDIF

  ! MSATID
  IF ( ALLOCATED(DATA_RAW%SAT_%MSATID) ) THEN
    SZ(24) = SIZE(DATA_RAW%SAT_%MSATID)
    SZ(25) = LBOUND(DATA_RAW%SAT_%MSATID,1)
    SZ(26) = UBOUND(DATA_RAW%SAT_%MSATID,1)
  ELSE
    SZ(24) = INT(0,JPIB_K)
    SZ(25) = INT(-99,JPIB_K)
    SZ(26) = INT(-99,JPIB_K)
  ENDIF

  ! MINST
  IF ( ALLOCATED(DATA_RAW%SAT_%MINST) ) THEN
    SZ(27) = SIZE(DATA_RAW%SAT_%MINST)
    SZ(28) = LBOUND(DATA_RAW%SAT_%MINST,1)
    SZ(29) = UBOUND(DATA_RAW%SAT_%MINST,1)
  ELSE
    SZ(17) = INT(0,JPIB_K)
    SZ(28) = INT(-99,JPIB_K)
    SZ(29) = INT(-99,JPIB_K)
  ENDIF

  ! MCHAN
  IF ( ALLOCATED(DATA_RAW%SAT_%MCHAN) ) THEN
    SZ(30) = SIZE(DATA_RAW%SAT_%MCHAN)
    SZ(31) = LBOUND(DATA_RAW%SAT_%MCHAN,1)
    SZ(32) = UBOUND(DATA_RAW%SAT_%MCHAN,1)
  ELSE
    SZ(30) = INT(0,JPIB_K)
    SZ(31) = INT(-99,JPIB_K)
    SZ(32) = INT(-99,JPIB_K)
  ENDIF


  ! ZVERT
  IF ( ALLOCATED(DATA_RAW%GEO_%ZVERT) ) THEN
    SZ(33) = SIZE(DATA_RAW%GEO_%ZVERT)
    SZ(34) = LBOUND(DATA_RAW%GEO_%ZVERT,1)
    SZ(35) = UBOUND(DATA_RAW%GEO_%ZVERT,1)
  ELSE
    SZ(33) = INT(0,JPIB_K)
    SZ(34) = INT(-99,JPIB_K)
    SZ(35) = INT(-99,JPIB_K)
  ENDIF

  ! FR
  IF ( ALLOCATED(DATA_RAW%WAM_%FR) ) THEN
    SZ(36) = SIZE(DATA_RAW%WAM_%FR)
    SZ(37) = LBOUND(DATA_RAW%WAM_%FR,1)
    SZ(38) = UBOUND(DATA_RAW%WAM_%FR,1)
  ELSE
    SZ(36) = INT(0,JPIB_K)
    SZ(37) = INT(-99,JPIB_K)
    SZ(38) = INT(-99,JPIB_K)
  ENDIF

  ! TH
  IF ( ALLOCATED(DATA_RAW%WAM_%TH) ) THEN
    SZ(39) = SIZE(DATA_RAW%WAM_%TH)
    SZ(40) = LBOUND(DATA_RAW%WAM_%TH,1)
    SZ(41) = UBOUND(DATA_RAW%WAM_%TH,1)
  ELSE
    SZ(39) = INT(0,JPIB_K)
    SZ(40) = INT(-99,JPIB_K)
    SZ(41) = INT(-99,JPIB_K)
  ENDIF

  ! RCWM
  IF ( ALLOCATED(DATA_RAW%SAT_%RCWN) ) THEN
    SZ(42) = SIZE(DATA_RAW%SAT_%RCWN)
    SZ(43) = LBOUND(DATA_RAW%SAT_%RCWN,1)
    SZ(44) = UBOUND(DATA_RAW%SAT_%RCWN,1)
  ELSE
    SZ(42) = INT(0,JPIB_K)
    SZ(43) = INT(-99,JPIB_K)
    SZ(44) = INT(-99,JPIB_K)
  ENDIF

  ! Total size of the arrys to be sent
  SZ(1) = SZ(4) + SZ(10) + SZ(15) + SZ(18) + SZ(21) + SZ(24) + SZ(27) + SZ(30)
  SZ(2) = SZ(5) + SZ(33) + SZ(36) + SZ(39) + SZ(42)
  SZ(3) = SZ(6)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION COMPUTE_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SERIALIZE_INTEGER_PARAMS'
PP_THREAD_SAFE FUNCTION SERIALIZE_INTEGER_PARAMS( DATA_RAW, BUF, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),                  INTENT(IN)    :: DATA_RAW
  INTEGER(KIND=JPIB_K), DIMENSION(:), INTENT(OUT)   :: BUF
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=1_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_RAW%GEO_%ILOENG ), ERRFLAG_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_RAW%WAM_%NLONRGG ), ERRFLAG_NOT_ALLOCATED )

  ! Initialize the buffer
  DO I = 1, SIZE(BUF)
    BUF(I) = -999999_JPIB_K
  ENDDO

  ! Fill the buffer
  BUF( 1) = INT( DATA_RAW%SIM_%IGRIB2_TABLES_VERSION_LATEST, JPIB_K )
  BUF( 2) = INT( DATA_RAW%SIM_%NCYCLE, JPIB_K )
  BUF( 3) = INT( DATA_RAW%SIM_%NLOCGRB, JPIB_K )
  BUF( 4) = INT( DATA_RAW%SIM_%NBITSSHLNSP, JPIB_K )
  BUF( 5) = INT( DATA_RAW%SIM_%NBITSEXPR, JPIB_K )
  BUF( 6) = INT( DATA_RAW%SIM_%NLEG, JPIB_K )
  BUF( 7) = INT( DATA_RAW%SIM_%NTOTENS, JPIB_K )
  BUF( 8) = INT( DATA_RAW%SIM_%NENSFNB, JPIB_K )
  BUF( 9) = INT( DATA_RAW%SIM_%NWINOFF, JPIB_K )
  BUF(10) = INT( DATA_RAW%SIM_%NJDIAG, JPIB_K )
  BUF(11) = INT( DATA_RAW%SIM_%NJDOMAI, JPIB_K )
  BUF(12) = INT( DATA_RAW%SIM_%NJITER, JPIB_K )
  BUF(13) = INT( DATA_RAW%SIM_%NSTREAM, JPIB_K )
  BUF(14) = INT( DATA_RAW%SIM_%NSYSTEM, JPIB_K )
  BUF(15) = INT( DATA_RAW%SIM_%NMETHOD, JPIB_K )
  BUF(16) = INT( DATA_RAW%SIM_%NREFERENCE, JPIB_K )
  BUF(17) = INT( DATA_RAW%SIM_%NCONSENSUS, JPIB_K )
  BUF(18) = INT( DATA_RAW%SIM_%NDWD, JPIB_K )
  BUF(19) = INT( DATA_RAW%SIM_%NMFR, JPIB_K )
  BUF(20) = INT( DATA_RAW%SIM_%NNCEP, JPIB_K )
  BUF(21) = INT( DATA_RAW%SIM_%NUKM, JPIB_K )
  BUF(22) = INT( DATA_RAW%SIM_%NINDAT, JPIB_K )
  BUF(23) = INT( DATA_RAW%SIM_%NSSSSS, JPIB_K )
  BUF(24) = INT( DATA_RAW%SIM_%NCONF, JPIB_K )
  BUF(25) = INT( DATA_RAW%SIM_%NSTEPINI, JPIB_K )
  BUF(26) = INT( DATA_RAW%SIM_%NFCHO_TRUNC_INI, JPIB_K )
  BUF(27) = INT( DATA_RAW%GEO_%JPMXLE, JPIB_K )
  BUF(28) = INT( DATA_RAW%GEO_%JPMXGL, JPIB_K )
  BUF(29) = INT( DATA_RAW%GEO_%ISMAX, JPIB_K )
  BUF(30) = INT( DATA_RAW%GEO_%ILATS, JPIB_K )
  BUF(31) = INT( DATA_RAW%GEO_%ILONS, JPIB_K )
  BUF(32) = INT( DATA_RAW%GEO_%IDGNH, JPIB_K )
  BUF(33) = INT( DATA_RAW%GEO_%IFLEV, JPIB_K )
  BUF(34) = INT( DATA_RAW%GEO_%NHTYP, JPIB_K )
  BUF(35) = INT( DATA_RAW%GEO_%NSTTYP, JPIB_K )
  BUF(36) = INT( DATA_RAW%WAM_%NGX, JPIB_K )
  BUF(37) = INT( DATA_RAW%WAM_%NGY, JPIB_K )
  BUF(38) = INT( DATA_RAW%WAM_%NANG, JPIB_K )
  BUF(39) = INT( DATA_RAW%WAM_%NFRE_RED, JPIB_K )
  BUF(40) = INT( DATA_RAW%WAM_%IMDLGRBID_G, JPIB_K )
  BUF(41) = INT( DATA_RAW%WAM_%IMDLGRBID_M, JPIB_K )
  BUF(42) = INT( DATA_RAW%WAM_%NDATE_TIME_WINDOW_END, JPIB_K )
  BUF(43) = INT( DATA_RAW%WAM_%NWINOFF, JPIB_K )
  BUF(44) = INT( DATA_RAW%WAM_%NGRIB_VERSION, JPIB_K )
  BUF(45) = INT( DATA_RAW%WAM_%NTENCODE, JPIB_K )
  BUF(46) = INT( DATA_RAW%WAM_%NGRBRESI, JPIB_K )
  BUF(47) = INT( DATA_RAW%WAM_%NGRBRESS, JPIB_K )
  BUF(48) = INT( DATA_RAW%WAM_%IRGG, JPIB_K )
  BUF(49) = INT( DATA_RAW%WAM_%IQGAUSS, JPIB_K )
  BUF(50) = INT( DATA_RAW%WAM_%NENSFNB, JPIB_K )
  BUF(51) = INT( DATA_RAW%WAM_%NTOTENS, JPIB_K )
  BUF(52) = INT( DATA_RAW%WAM_%NSYSNB, JPIB_K )
  BUF(53) = INT( DATA_RAW%WAM_%NMETNB, JPIB_K )
  BUF(54) = INT( DATA_RAW%WAM_%ISTREAM, JPIB_K )
  BUF(55) = INT( DATA_RAW%WAM_%NLOCGRB, JPIB_K )
  BUF(56) = INT( DATA_RAW%WAM_%NCONSENSUS, JPIB_K )
  BUF(57) = INT( DATA_RAW%WAM_%NDWD, JPIB_K )
  BUF(58) = INT( DATA_RAW%WAM_%NMFR, JPIB_K )
  BUF(59) = INT( DATA_RAW%WAM_%NNCEP, JPIB_K )
  BUF(60) = INT( DATA_RAW%WAM_%NUKM, JPIB_K )
  BUF(61) = INT( DATA_RAW%WAM_%IREFDATE, JPIB_K )
  BUF(62) = INT( DATA_RAW%WAM_%NSPEC2TAB, JPIB_K )
  BUF(63) = INT( DATA_RAW%WAM_%NSPEC2TMPD, JPIB_K )
  BUF(64) = INT( DATA_RAW%WAM_%NSPEC2TMPP, JPIB_K )
  BUF(65) = INT( DATA_RAW%WAM_%NTRG2TMPD, JPIB_K )
  BUF(66) = INT( DATA_RAW%WAM_%NTRG2TMPP, JPIB_K )
  BUF(67) = INT( DATA_RAW%WAM_%ITMIN, JPIB_K )
  BUF(68) = INT( DATA_RAW%WAM_%ITMAX, JPIB_K )
  BUF(69) = INT( DATA_RAW%SAT_%NSATSIM, JPIB_K )
  BUF(70) = INT( DATA_RAW%SIM_%NPROC_IO, JPIB_K )
  BUF(71) = L2I( DATA_RAW%SIM_%LPPSTEPS )
  BUF(72) = L2I( DATA_RAW%SIM_%LOBSC1 )
  BUF(73) = L2I( DATA_RAW%SIM_%LVAREPS )
  BUF(74) = L2I( DATA_RAW%SIM_%LDMCC04 )
  BUF(75) = L2I( DATA_RAW%WAM_%LWCOUSAMEGRID )
  BUF(76) = L2I( DATA_RAW%WAM_%LGRHDIFS )
  BUF(77) = L2I( DATA_RAW%WAM_%LNEWLVTP )
  BUF(78) = L2I( DATA_RAW%WAM_%LPADPOLES )
  BUF(79) = L2I( DATA_RAW%WAM_%LL_GRID_SIMPLE_MATRIX )
  CNT = 79

  IF ( ALLOCATED(DATA_RAW%GEO_%ILOENG) ) THEN
    DO I = LBOUND(DATA_RAW%GEO_%ILOENG,1), UBOUND(DATA_RAW%GEO_%ILOENG,1)
      CNT = CNT + 1
      BUF(CNT) = INT( DATA_RAW%GEO_%ILOENG(I), JPIB_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%GEO_%NSFLEVS) ) THEN
    DO I = LBOUND(DATA_RAW%GEO_%NSFLEVS,1), UBOUND(DATA_RAW%GEO_%NSFLEVS,1)
      DO J = LBOUND(DATA_RAW%GEO_%NSFLEVS,2), UBOUND(DATA_RAW%GEO_%NSFLEVS,2)
        CNT = CNT + 1
        BUF(CNT) = INT( DATA_RAW%GEO_%NSFLEVS(I,J), JPIB_K )
      ENDDO
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%WAM_%NLONRGG) ) THEN
    DO I = LBOUND(DATA_RAW%WAM_%NLONRGG,1), UBOUND(DATA_RAW%WAM_%NLONRGG,1)
      CNT = CNT + 1
      BUF(CNT) = INT( DATA_RAW%WAM_%NLONRGG(I), JPIB_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%SAT_%MSERIES) ) THEN
    DO I = LBOUND(DATA_RAW%SAT_%MSERIES,1), UBOUND(DATA_RAW%SAT_%MSERIES,1)
      CNT = CNT + 1
      BUF(CNT) = INT( DATA_RAW%SAT_%MSERIES(I), JPIB_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%SAT_%MSATID) ) THEN
    DO I = LBOUND(DATA_RAW%SAT_%MSATID,1), UBOUND(DATA_RAW%SAT_%MSATID,1)
      CNT = CNT + 1
      BUF(CNT) = INT( DATA_RAW%SAT_%MSATID(I), JPIB_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%SAT_%MINST) ) THEN
    DO I = LBOUND(DATA_RAW%SAT_%MINST,1), UBOUND(DATA_RAW%SAT_%MINST,1)
      CNT = CNT + 1
      BUF(CNT) = INT( DATA_RAW%SAT_%MINST(I), JPIB_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%SAT_%MCHAN) ) THEN
    DO I = LBOUND(DATA_RAW%SAT_%MCHAN,1), UBOUND(DATA_RAW%SAT_%MCHAN,1)
      CNT = CNT + 1
      BUF(CNT) = INT( DATA_RAW%SAT_%MCHAN(I), JPIB_K )
    ENDDO
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
    CASE (ERRFLAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong size of the buffer' )
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

END FUNCTION SERIALIZE_INTEGER_PARAMS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SERIALIZE_REAL_PARAMS'
PP_THREAD_SAFE FUNCTION SERIALIZE_REAL_PARAMS( DATA_RAW, BUF, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)  :: DATA_RAW
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(OUT) :: BUF
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_RAW%GEO_%ZVERT ), ERRFLAG_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_RAW%WAM_%FR ), ERRFLAG_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED( DATA_RAW%WAM_%TH ), ERRFLAG_NOT_ALLOCATED )

  ! Initialize the buffer
  DO I = 1, SIZE(BUF)
    BUF(I) = -999999.9999_JPRD_K
  ENDDO

  ! Fill the buffer
  BUF( 1) = REAL( DATA_RAW%SIM_%RTIMST, JPRD_K )
  BUF( 2) = REAL( DATA_RAW%SIM_%TSTEP, JPRD_K )
  BUF( 3) = REAL( DATA_RAW%GEO_%ZNLAT, JPRD_K )
  BUF( 4) = REAL( DATA_RAW%GEO_%ZSLAT, JPRD_K )
  BUF( 5) = REAL( DATA_RAW%GEO_%RMUCEN, JPRD_K )
  BUF( 6) = REAL( DATA_RAW%GEO_%RLOCEN, JPRD_K )
  BUF( 7) = REAL( DATA_RAW%GEO_%RSTRET, JPRD_K )
  BUF( 8) = REAL( DATA_RAW%WAM_%PPMISS, JPRD_K )
  BUF( 9) = REAL( DATA_RAW%WAM_%PPEPS, JPRD_K )
  BUF(10) = REAL( DATA_RAW%WAM_%PPREC, JPRD_K )
  BUF(11) = REAL( DATA_RAW%WAM_%PPRESOL, JPRD_K )
  BUF(12) = REAL( DATA_RAW%WAM_%PPMIN_RESET, JPRD_K )
  BUF(13) = REAL( DATA_RAW%WAM_%AMOWEP, JPRD_K )
  BUF(14) = REAL( DATA_RAW%WAM_%AMOSOP, JPRD_K )
  BUF(15) = REAL( DATA_RAW%WAM_%AMOEAP, JPRD_K )
  BUF(16) = REAL( DATA_RAW%WAM_%AMONOP, JPRD_K )
  BUF(17) = REAL( DATA_RAW%WAM_%XDELLA, JPRD_K )
  BUF(18) = REAL( DATA_RAW%WAM_%XDELLO, JPRD_K )
  BUF(19) = REAL( DATA_RAW%WAM_%ZMISS, JPRD_K )
  CNT = 19

  IF ( ALLOCATED(DATA_RAW%GEO_%ZVERT) ) THEN
    DO I = LBOUND(DATA_RAW%GEO_%ZVERT,1), UBOUND(DATA_RAW%GEO_%ZVERT,1)
      CNT = CNT + 1
      BUF(CNT) = REAL( DATA_RAW%GEO_%ZVERT(I), JPRD_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%WAM_%FR) ) THEN
    DO I = LBOUND(DATA_RAW%WAM_%FR,1), UBOUND(DATA_RAW%WAM_%FR,1)
      CNT = CNT + 1
      BUF(CNT) = REAL( DATA_RAW%WAM_%FR(I), JPRD_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%WAM_%TH) ) THEN
    DO I = LBOUND(DATA_RAW%WAM_%TH,1), UBOUND(DATA_RAW%WAM_%TH,1)
      CNT = CNT + 1
      BUF(CNT) = REAL( DATA_RAW%WAM_%TH(I), JPRD_K )
    ENDDO
  ENDIF

  IF ( ALLOCATED(DATA_RAW%SAT_%RCWN) ) THEN
    DO I = LBOUND(DATA_RAW%SAT_%RCWN,1), UBOUND(DATA_RAW%SAT_%RCWN,1)
      CNT = CNT + 1
      BUF(CNT) = REAL( DATA_RAW%SAT_%RCWN(I), JPRD_K )
    ENDDO
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
    CASE (1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong size of the buffer' )
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

END FUNCTION SERIALIZE_REAL_PARAMS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SERIALIZE_STRING_PARAMS'
PP_THREAD_SAFE FUNCTION SERIALIZE_STRING_PARAMS( DATA_RAW, BUF, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),               INTENT(IN)  :: DATA_RAW
  CHARACTER(LEN=16), DIMENSION(:), INTENT(OUT) :: BUF
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_SIZE=1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(BUF).LT.9, ERRFLAG_WRONG_SIZE )

  ! Initialize the buffer
  DO I = 1, SIZE(BUF)
    BUF(I) = REPEAT(' ',16)
  ENDDO

  ! Fill the buffer
  BUF( 1)(1:2)  = DATA_RAW%SIM_%CTYPE(1:2)
  BUF( 2)(1:2)  = DATA_RAW%SIM_%CFCLASS(1:2)
  BUF( 3)(1:16) = DATA_RAW%SIM_%CNMEXP(1:16)
  BUF( 4)(1:1)  = DATA_RAW%WAM_%CLDOMAIN(1:1)
  BUF( 5)(1:1)  = DATA_RAW%WAM_%HOPERI(1:1)
  BUF( 6)(1:1)  = DATA_RAW%WAM_%HOPERS(1:1)
  BUF( 7)(1:2)  = DATA_RAW%WAM_%MARSTYPE(1:2)
  BUF( 8)(1:2)  = DATA_RAW%WAM_%YCLASS(1:2)
  BUF( 9)(1:3)  = DATA_RAW%WAM_%YEXPVER(1:3)

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
    CASE (ERRFLAG_WRONG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong size of the buffer' )
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

END FUNCTION SERIALIZE_STRING_PARAMS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DESERIALIZE_INTEGER_PARAMS'
PP_THREAD_SAFE FUNCTION DESERIALIZE_INTEGER_PARAMS( BUF, SZ, DATA_RAW, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), DIMENSION(:),  INTENT(IN)    :: BUF
  INTEGER(KIND=JPIB_K), DIMENSION(44), INTENT(IN)    :: SZ
  TYPE(MODEL_PAR_T),                   INTENT(INOUT) :: DATA_RAW
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: LO1
  INTEGER(KIND=JPIB_K) :: LO2
  INTEGER(KIND=JPIB_K) :: HI1
  INTEGER(KIND=JPIB_K) :: HI2

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_SIZE=1_JPIB_K
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
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(BUF).LT.79, ERRFLAG_WRONG_SIZE )

  ! Free memory if allocated
  IF ( ALLOCATED(DATA_RAW%GEO_%ILOENG) ) THEN
    DEALLOCATE(DATA_RAW%GEO_%ILOENG, STAT=STAT)
  ENDIF
  IF ( ALLOCATED(DATA_RAW%GEO_%NSFLEVS) ) THEN
    DEALLOCATE(DATA_RAW%GEO_%NSFLEVS, STAT=STAT)
  ENDIF
  IF ( ALLOCATED(DATA_RAW%WAM_%NLONRGG) ) THEN
    DEALLOCATE(DATA_RAW%WAM_%NLONRGG, STAT=STAT)
  ENDIF

  ! Fill the buffer
  DATA_RAW%SIM_%IGRIB2_TABLES_VERSION_LATEST = INT( BUF(1),  JPIB_K )
  DATA_RAW%SIM_%NCYCLE                       = INT( BUF(2),  JPIB_K )
  DATA_RAW%SIM_%NLOCGRB                      = INT( BUF(3),  JPIB_K )
  DATA_RAW%SIM_%NBITSSHLNSP                  = INT( BUF(4),  JPIB_K )
  DATA_RAW%SIM_%NBITSEXPR                    = INT( BUF(5),  JPIB_K )
  DATA_RAW%SIM_%NLEG                         = INT( BUF(6),  JPIB_K )
  DATA_RAW%SIM_%NTOTENS                      = INT( BUF(7),  JPIB_K )
  DATA_RAW%SIM_%NENSFNB                      = INT( BUF(8),  JPIB_K )
  DATA_RAW%SIM_%NWINOFF                      = INT( BUF(9),  JPIB_K )
  DATA_RAW%SIM_%NJDIAG                       = INT( BUF(10), JPIB_K )
  DATA_RAW%SIM_%NJDOMAI                      = INT( BUF(11), JPIB_K )
  DATA_RAW%SIM_%NJITER                       = INT( BUF(12), JPIB_K )
  DATA_RAW%SIM_%NSTREAM                      = INT( BUF(13), JPIB_K )
  DATA_RAW%SIM_%NSYSTEM                      = INT( BUF(14), JPIB_K )
  DATA_RAW%SIM_%NMETHOD                      = INT( BUF(15), JPIB_K )
  DATA_RAW%SIM_%NREFERENCE                   = INT( BUF(16), JPIB_K )
  DATA_RAW%SIM_%NCONSENSUS                   = INT( BUF(17), JPIB_K )
  DATA_RAW%SIM_%NDWD                         = INT( BUF(18), JPIB_K )
  DATA_RAW%SIM_%NMFR                         = INT( BUF(19), JPIB_K )
  DATA_RAW%SIM_%NNCEP                        = INT( BUF(20), JPIB_K )
  DATA_RAW%SIM_%NUKM                         = INT( BUF(21), JPIB_K )
  DATA_RAW%SIM_%NINDAT                       = INT( BUF(22), JPIB_K )
  DATA_RAW%SIM_%NSSSSS                       = INT( BUF(23), JPIB_K )
  DATA_RAW%SIM_%NCONF                        = INT( BUF(24), JPIB_K )
  DATA_RAW%SIM_%NSTEPINI                     = INT( BUF(25), JPIB_K )
  DATA_RAW%SIM_%NFCHO_TRUNC_INI              = INT( BUF(26), JPIB_K )
  DATA_RAW%GEO_%JPMXLE                       = INT( BUF(27), JPIB_K )
  DATA_RAW%GEO_%JPMXGL                       = INT( BUF(28), JPIB_K )
  DATA_RAW%GEO_%ISMAX                        = INT( BUF(29), JPIB_K )
  DATA_RAW%GEO_%ILATS                        = INT( BUF(30), JPIB_K )
  DATA_RAW%GEO_%ILONS                        = INT( BUF(31), JPIB_K )
  DATA_RAW%GEO_%IDGNH                        = INT( BUF(32), JPIB_K )
  DATA_RAW%GEO_%IFLEV                        = INT( BUF(33), JPIB_K )
  DATA_RAW%GEO_%NHTYP                        = INT( BUF(34), JPIB_K )
  DATA_RAW%GEO_%NSTTYP                       = INT( BUF(35), JPIB_K )
  DATA_RAW%WAM_%NGX                          = INT( BUF(36), JPIB_K )
  DATA_RAW%WAM_%NGY                          = INT( BUF(37), JPIB_K )
  DATA_RAW%WAM_%NANG                         = INT( BUF(38), JPIB_K )
  DATA_RAW%WAM_%NFRE_RED                     = INT( BUF(39), JPIB_K )
  DATA_RAW%WAM_%IMDLGRBID_G                  = INT( BUF(40), JPIB_K )
  DATA_RAW%WAM_%IMDLGRBID_M                  = INT( BUF(41), JPIB_K )
  DATA_RAW%WAM_%NDATE_TIME_WINDOW_END        = INT( BUF(42), JPIB_K )
  DATA_RAW%WAM_%NWINOFF                      = INT( BUF(43), JPIB_K )
  DATA_RAW%WAM_%NGRIB_VERSION                = INT( BUF(44), JPIB_K )
  DATA_RAW%WAM_%NTENCODE                     = INT( BUF(45), JPIB_K )
  DATA_RAW%WAM_%NGRBRESI                     = INT( BUF(46), JPIB_K )
  DATA_RAW%WAM_%NGRBRESS                     = INT( BUF(47), JPIB_K )
  DATA_RAW%WAM_%IRGG                         = INT( BUF(48), JPIB_K )
  DATA_RAW%WAM_%IQGAUSS                      = INT( BUF(49), JPIB_K )
  DATA_RAW%WAM_%NENSFNB                      = INT( BUF(50), JPIB_K )
  DATA_RAW%WAM_%NTOTENS                      = INT( BUF(51), JPIB_K )
  DATA_RAW%WAM_%NSYSNB                       = INT( BUF(52), JPIB_K )
  DATA_RAW%WAM_%NMETNB                       = INT( BUF(53), JPIB_K )
  DATA_RAW%WAM_%ISTREAM                      = INT( BUF(54), JPIB_K )
  DATA_RAW%WAM_%NLOCGRB                      = INT( BUF(55), JPIB_K )
  DATA_RAW%WAM_%NCONSENSUS                   = INT( BUF(56), JPIB_K )
  DATA_RAW%WAM_%NDWD                         = INT( BUF(57), JPIB_K )
  DATA_RAW%WAM_%NMFR                         = INT( BUF(58), JPIB_K )
  DATA_RAW%WAM_%NNCEP                        = INT( BUF(59), JPIB_K )
  DATA_RAW%WAM_%NUKM                         = INT( BUF(60), JPIB_K )
  DATA_RAW%WAM_%IREFDATE                     = INT( BUF(61), JPIB_K )
  DATA_RAW%WAM_%NSPEC2TAB                    = INT( BUF(62), JPIB_K )
  DATA_RAW%WAM_%NSPEC2TMPD                   = INT( BUF(63), JPIB_K )
  DATA_RAW%WAM_%NSPEC2TMPP                   = INT( BUF(64), JPIB_K )
  DATA_RAW%WAM_%NTRG2TMPD                    = INT( BUF(65), JPIB_K )
  DATA_RAW%WAM_%NTRG2TMPP                    = INT( BUF(66), JPIB_K )
  DATA_RAW%WAM_%ITMIN                        = INT( BUF(67), JPIB_K )
  DATA_RAW%WAM_%ITMAX                        = INT( BUF(68), JPIB_K )
  DATA_RAW%SAT_%NSATSIM                      = INT( BUF(69), JPIB_K )
  DATA_RAW%SIM_%NPROC_IO                     = INT( BUF(70), JPIB_K )
  DATA_RAW%SIM_%LPPSTEPS                     = I2L( BUF(71) )
  DATA_RAW%SIM_%LOBSC1                       = I2L( BUF(72) )
  DATA_RAW%SIM_%LVAREPS                      = I2L( BUF(73) )
  DATA_RAW%SIM_%LDMCC04                      = I2L( BUF(74) )
  DATA_RAW%WAM_%LWCOUSAMEGRID                = I2L( BUF(75) )
  DATA_RAW%WAM_%LGRHDIFS                     = I2L( BUF(76) )
  DATA_RAW%WAM_%LNEWLVTP                     = I2L( BUF(77) )
  DATA_RAW%WAM_%LPADPOLES                    = I2L( BUF(78) )
  DATA_RAW%WAM_%LL_GRID_SIMPLE_MATRIX        = I2L( BUF(79) )
  CNT = 79

  ! ILOENG
  IF ( SZ(15) .GT. 0 ) THEN
    LO1 = SZ(16)
    HI1 = SZ(17)
    ALLOCATE(DATA_RAW%GEO_%ILOENG(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATION_ERROR )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%GEO_%ILOENG(I) = INT( BUF(CNT), JPIB_K )
    ENDDO
  ENDIF

  ! NSFLEVS
  IF ( SZ(10) .GT. 0 ) THEN
    LO1 = SZ(11)
    HI1 = SZ(12)
    LO2 = SZ(13)
    HI2 = SZ(14)
    ALLOCATE(DATA_RAW%GEO_%NSFLEVS(LO1:HI1,LO2:HI2), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATION_ERROR )
    DO I = LO1, HI1
      DO J = LO2, HI2
        CNT = CNT + 1
        DATA_RAW%GEO_%NSFLEVS(I,J) = INT( BUF(CNT), JPIB_K )
      ENDDO
    ENDDO
  ENDIF

  ! NLONRGG
  IF ( SZ(18) .GT. 0 ) THEN
    LO1 = SZ(19)
    HI1 = SZ(20)
    ALLOCATE(DATA_RAW%WAM_%NLONRGG(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATION_ERROR )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%WAM_%NLONRGG(I) = INT( BUF(CNT), JPIB_K )
    ENDDO
  ENDIF

  ! MSERIES
  IF ( SZ(21) .GT. 0 ) THEN
    LO1 = SZ(22)
    HI1 = SZ(23)
    ALLOCATE(DATA_RAW%SAT_%MSERIES(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATION_ERROR )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%SAT_%MSERIES(I) = INT( BUF(CNT), JPIB_K )
    ENDDO
  ENDIF

  ! MSATID
  IF ( SZ(24) .GT. 0 ) THEN
    LO1 = SZ(25)
    HI1 = SZ(26)
    ALLOCATE(DATA_RAW%SAT_%MSATID(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATION_ERROR )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%SAT_%MSATID(I) = INT( BUF(CNT), JPIB_K )
    ENDDO
  ENDIF

  ! MINST
  IF ( SZ(27) .GT. 0 ) THEN
    LO1 = SZ(28)
    HI1 = SZ(29)
    ALLOCATE(DATA_RAW%SAT_%MINST(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATION_ERROR )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%SAT_%MINST(I) = INT( BUF(CNT), JPIB_K )
    ENDDO
  ENDIF

  ! MCHAN
  IF ( SZ(30) .GT. 0 ) THEN
    LO1 = SZ(31)
    HI1 = SZ(32)
    ALLOCATE(DATA_RAW%SAT_%MCHAN(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATION_ERROR )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%SAT_%MCHAN(I) = INT( BUF(CNT), JPIB_K )
    ENDDO
  ENDIF

  ! Paranoid checks
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG)
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
    CASE (ERRFLAG_WRONG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong size of the buffer' )
    CASE (ERRFLAG_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'allocation error' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=STAT)
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

END FUNCTION DESERIALIZE_INTEGER_PARAMS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DESERIALIZE_REAL_PARAMS'
PP_THREAD_SAFE FUNCTION DESERIALIZE_REAL_PARAMS( BUF, SZ, DATA_RAW, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  REAL(KIND=JPRD_K),    DIMENSION(:),  INTENT(IN)    :: BUF
  INTEGER(KIND=JPIB_K), DIMENSION(44), INTENT(IN)    :: SZ
  TYPE(MODEL_PAR_T),                   INTENT(INOUT) :: DATA_RAW
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: LO1
  INTEGER(KIND=JPIB_K) :: HI1

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_SIZE=1_JPIB_K
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
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(BUF).LT.19, ERRFLAG_WRONG_SIZE )

  ! Free memory if allocated
  IF ( ALLOCATED(DATA_RAW%GEO_%ZVERT) ) THEN
    DEALLOCATE(DATA_RAW%GEO_%ZVERT, STAT=STAT)
  ENDIF
  IF ( ALLOCATED(DATA_RAW%WAM_%FR) ) THEN
    DEALLOCATE(DATA_RAW%WAM_%FR, STAT=STAT)
  ENDIF
  IF ( ALLOCATED(DATA_RAW%WAM_%TH) ) THEN
    DEALLOCATE(DATA_RAW%WAM_%TH, STAT=STAT)
  ENDIF

  ! Fill the buffer
  DATA_RAW%SIM_%RTIMST      = REAL( BUF(1), JPRD_K )
  DATA_RAW%SIM_%TSTEP       = REAL( BUF(2), JPRD_K )
  DATA_RAW%GEO_%ZNLAT       = REAL( BUF(3), JPRD_K )
  DATA_RAW%GEO_%ZSLAT       = REAL( BUF(4), JPRD_K )
  DATA_RAW%GEO_%RMUCEN      = REAL( BUF(5), JPRD_K )
  DATA_RAW%GEO_%RLOCEN      = REAL( BUF(6), JPRD_K )
  DATA_RAW%GEO_%RSTRET      = REAL( BUF(7), JPRD_K )
  DATA_RAW%WAM_%PPMISS      = REAL( BUF(8), JPRD_K )
  DATA_RAW%WAM_%PPEPS       = REAL( BUF(9), JPRD_K )
  DATA_RAW%WAM_%PPREC       = REAL( BUF(10), JPRD_K )
  DATA_RAW%WAM_%PPRESOL     = REAL( BUF(11), JPRD_K )
  DATA_RAW%WAM_%PPMIN_RESET = REAL( BUF(12), JPRD_K )
  DATA_RAW%WAM_%AMOWEP      = REAL( BUF(13), JPRD_K )
  DATA_RAW%WAM_%AMOSOP      = REAL( BUF(14), JPRD_K )
  DATA_RAW%WAM_%AMOEAP      = REAL( BUF(15), JPRD_K )
  DATA_RAW%WAM_%AMONOP      = REAL( BUF(16), JPRD_K )
  DATA_RAW%WAM_%XDELLA      = REAL( BUF(17), JPRD_K )
  DATA_RAW%WAM_%XDELLO      = REAL( BUF(18), JPRD_K )
  DATA_RAW%WAM_%ZMISS       = REAL( BUF(19), JPRD_K )
  CNT = 19

  ! ZVERT
  IF ( SZ(33) .GT. 0 ) THEN
    LO1 = SZ(34)
    HI1 = SZ(35)
    ALLOCATE(DATA_RAW%GEO_%ZVERT(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATION_ERROR )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%GEO_%ZVERT(I) = REAL( BUF(CNT), JPRD_K )
    ENDDO
  ENDIF

  ! FR
  IF ( SZ(36) .GT. 0 ) THEN
    LO1 = SZ(37)
    HI1 = SZ(38)
    ALLOCATE(DATA_RAW%WAM_%FR(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATION_ERROR )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%WAM_%FR(I) = REAL( BUF(CNT), JPRD_K )
    ENDDO
  ENDIF

  ! TH
  IF ( SZ(39) .GT. 0 ) THEN
    LO1 = SZ(40)
    HI1 = SZ(41)
    ALLOCATE(DATA_RAW%WAM_%TH(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATION_ERROR )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%WAM_%TH(I) = REAL( BUF(CNT), JPRD_K )
    ENDDO
  ENDIF

  ! TH
  IF ( SZ(42) .GT. 0 ) THEN
    LO1 = SZ(43)
    HI1 = SZ(44)
    ALLOCATE(DATA_RAW%SAT_%RCWN(LO1:HI1), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATION_ERROR )
    DO I = LO1, HI1
      CNT = CNT + 1
      DATA_RAW%SAT_%RCWN(I) = REAL( BUF(CNT), JPRD_K )
    ENDDO
  ENDIF

  ! Paranoid checks
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG, STAT=STAT)
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
    CASE (ERRFLAG_WRONG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong size of the buffer' )
    CASE (ERRFLAG_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'allocation error' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=STAT)
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

END FUNCTION DESERIALIZE_REAL_PARAMS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DESERIALIZE_STRING_PARAMS'
PP_THREAD_SAFE FUNCTION DESERIALIZE_STRING_PARAMS( BUF, SZ, DATA_RAW, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=16),    DIMENSION(:),  INTENT(IN)    :: BUF
  INTEGER(KIND=JPIB_K), DIMENSION(44), INTENT(IN)    :: SZ
  TYPE(MODEL_PAR_T),                   INTENT(INOUT) :: DATA_RAW
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_SIZE=1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(BUF).LT.9, ERRFLAG_WRONG_SIZE )

  ! Fill the buffer
  DATA_RAW%SIM_%CTYPE(1:2)    = BUF( 1)(1:2)
  DATA_RAW%SIM_%CFCLASS(1:2)  = BUF( 2)(1:2)
  DATA_RAW%SIM_%CNMEXP(1:16)  = BUF( 3)(1:16)
  DATA_RAW%WAM_%CLDOMAIN(1:1) = BUF( 4)(1:1)
  DATA_RAW%WAM_%HOPERI(1:1)   = BUF( 5)(1:1)
  DATA_RAW%WAM_%HOPERS(1:1)   = BUF( 6)(1:1)
  DATA_RAW%WAM_%MARSTYPE(1:2) = BUF( 7)(1:2)
  DATA_RAW%WAM_%YCLASS(1:2)   = BUF( 8)(1:2)
  DATA_RAW%WAM_%YEXPVER(1:3)  = BUF( 9)(1:3)

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
    CASE (ERRFLAG_WRONG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong size of the buffer' )
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

END FUNCTION DESERIALIZE_STRING_PARAMS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE







#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'L2I'
PP_THREAD_SAFE FUNCTION L2I( L ) RESULT(I)
  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
IMPLICIT NONE
LOGICAL, INTENT(IN) :: L
INTEGER(KIND=JPIB_K) :: I
IF ( L ) THEN
  I = 1_JPIB_K
ELSE
  I = 0_JPIB_K
ENDIF
END FUNCTION L2I
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'I2L'
PP_THREAD_SAFE FUNCTION I2L( I ) RESULT(L)
  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
IMPLICIT NONE
INTEGER(KIND=JPIB_K), INTENT(IN) :: I
LOGICAL :: L
IF ( I.EQ.0 ) THEN
  L = .FALSE.
ELSE
  L = .TRUE.
ENDIF
END FUNCTION I2L
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE IFS_PAR_SERIALIZATION_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME