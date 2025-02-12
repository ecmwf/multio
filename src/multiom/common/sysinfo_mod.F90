! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'sysinfo_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'SYSINFO_MOD'
MODULE SYSINFO_MOD

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

! Whitelist of public symbols
PUBLIC :: GET_HOSTNAME
PUBLIC :: GET_PID
PUBLIC :: GET_TID
PUBLIC :: GET_NUM_THREADS
PUBLIC :: GET_MEM
PUBLIC :: TIC
PUBLIC :: TOC
PUBLIC :: IS_LITTLE_ENDIAN

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GET_HOSTNAME'
PP_THREAD_SAFE FUNCTION GET_HOSTNAME( CDHOSTNAME, HOOKS ) RESULT(RET)


  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR


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
  CHARACTER(LEN=*), INTENT(OUT)   :: CDHOSTNAME
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_SIZE_T)  :: C_LENGTH
  INTEGER(KIND=C_INT)     :: C_STATUS
  CHARACTER(KIND=C_CHAR,LEN=LEN(CDHOSTNAME)+1), TARGET :: C_CLHOSTNAME

  ! Local parameters
  INTEGER(C_INT), PARAMETER :: HOST_NAME_MAX = 255

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_HOSTNAME=1_JPIB_K

  ! Explicit interfaces
  INTERFACE
    FUNCTION C_GETHOSTNAME( C_CDVAR, C_NDLEN ) RESULT(C_STATUS) BIND(C, NAME="gethostname")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
    IMPLICIT NONE
      TYPE(C_PTR),            VALUE, INTENT(IN) :: C_CDVAR
      INTEGER(KIND=C_SIZE_T), VALUE, INTENT(IN) :: C_NDLEN
      INTEGER(C_INT) :: C_STATUS
    END FUNCTION C_GETHOSTNAME
  END INTERFACE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Initialise c buffers
  C_CLHOSTNAME = REPEAT(C_NULL_CHAR,LEN(C_CLHOSTNAME))
  C_LENGTH = INT( LEN(C_CLHOSTNAME), C_INT )

  ! Call the c function
  C_STATUS = C_GETHOSTNAME( C_LOC(C_CLHOSTNAME), C_LENGTH )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( C_STATUS.NE.0, ERRFLAG_UNABLE_TO_READ_HOSTNAME )

  ! Copy the hostname back to a fortran string
  CDHOSTNAME = REPEAT(' ', LEN(CDHOSTNAME))
  I = 1
  DO
    IF ( I .LE. LEN(CDHOSTNAME)  .AND. C_CLHOSTNAME(I:I) .NE. C_NULL_CHAR) THEN
      CDHOSTNAME(I:I) = C_CLHOSTNAME(I:I)
      I = I + 1
    ELSE
      EXIT
    ENDIF
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
    CASE (ERRFLAG_UNABLE_TO_READ_HOSTNAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read hostname' )
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

END FUNCTION GET_HOSTNAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GET_PID'
PP_THREAD_SAFE FUNCTION GET_PID( PID, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT

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

  ! Function result
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: PID
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT)  :: C_PID

  ! Local parameters (Default permissions to be used to create a directory)
  INTEGER(C_INT), PARAMETER :: S_IRWXU = 448

  ! Explicit interfaces
  INTERFACE
    FUNCTION C_GETPID( ) RESULT(C_PID) BIND(C, NAME="getpid")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      INTEGER(C_INT) :: C_PID
    END FUNCTION C_GETPID
  END INTERFACE

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

  ! Call the c function
  C_PID = C_GETPID()

  ! Cast the result
  PID = INT(C_PID, KIND(PID) )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION GET_PID
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GET_TID'
PP_THREAD_SAFE FUNCTION GET_TID( TID, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT

  ! Symbols imported from other modules within the project
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

#if defined(_OPENMP)
  USE :: OMP_LIB, ONLY: OMP_GET_THREAD_NUM
#endif

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: TID
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER :: THREAD_ID

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

#if defined(_OPENMP)
  THREAD_ID = OMP_GET_THREAD_NUM()
#else
  THREAD_ID = 0  ! Default to thread ID 0 when OpenMP is not enabled
#endif

  ! Set dummy argument
  TID = INT(THREAD_ID, KIND=JPIB_K)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION GET_TID
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GET_NUM_THREADS'
PP_THREAD_SAFE FUNCTION GET_NUM_THREADS( NTH, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

#ifdef _OPENMP
  USE :: OMP_LIB, ONLY: OMP_GET_THREAD_NUM
#endif

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: NTH
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER :: NUM_THREADS

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

#ifdef _OPENMP
  NUM_THREADS = OMP_GET_NUM_THREADS()
#else
  NUM_THREADS = 1  ! Default to thread ID 0 when OpenMP is not enabled
#endif

  ! Set dummy argument
  NTH = INT(NUM_THREADS, KIND=JPIB_K)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION GET_NUM_THREADS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC'
PP_THREAD_SAFE FUNCTION TOC( NSEC, DNSEC, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT64_T
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_LOC

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

  ! Function result
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: NSEC
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: DNSEC
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT64_T), TARGET :: C_NSEC
  INTEGER(KIND=C_INT64_T), TARGET :: C_DNSEC

  ! Explicit interfaces
  INTERFACE
    SUBROUTINE C_TOC( C_NSEC, C_DNSEC) BIND(C, NAME="om_toc")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT64_T
    IMPLICIT NONE
      INTEGER(C_INT64_T), VALUE, INTENT(IN) :: C_NSEC
      TYPE(C_PTR),        VALUE, INTENT(IN) :: C_DNSEC
    END SUBROUTINE C_TOC
  END INTERFACE

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

  ! Initialisation of the variables
  C_NSEC   = INT( NSEC, C_INT64_T)
  C_DNSEC  = INT( -99,  C_INT64_T)

  ! Call the utility to read the memory state
  CALL C_TOC( C_NSEC, C_LOC(C_DNSEC) )

  ! Cast the result
  DNSEC = INT( C_DNSEC, JPIB_K)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION TOC
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TIC'
PP_THREAD_SAFE FUNCTION TIC( NSEC, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT64_T
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_LOC

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
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: NSEC
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT64_T), TARGET :: C_NSEC

  ! Explicit interfaces
  INTERFACE
    SUBROUTINE C_TIC( C_NSEC ) BIND(C, NAME="om_tic")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: C_NSEC
    END SUBROUTINE C_TIC
  END INTERFACE

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

  ! Initialisation of the variables
  C_NSEC  = INT( -99,  C_INT64_T)

  ! Call the utility to read the memory state
  CALL C_TIC( C_LOC(C_NSEC) )

  ! Cast the result
  NSEC = INT( C_NSEC, JPIB_K)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION TIC
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GET_MEM'
PP_THREAD_SAFE FUNCTION GET_MEM( TOT_MEM, SYS_USAGE, TASK_USAGE, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT64_T
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_LOC

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
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: TOT_MEM
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: SYS_USAGE
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: TASK_USAGE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT64_T), TARGET :: C_TOT_MEM
  INTEGER(KIND=C_INT64_T), TARGET :: C_SYS_USAGE
  INTEGER(KIND=C_INT64_T), TARGET :: C_TASK_USAGE

  ! Explicit interfaces
  INTERFACE
    SUBROUTINE C_GET_MEM_USAGE( C_TOT_MEM, C_SYS_USAGE, C_TASK_USAGE) BIND(C, NAME="om_get_mem_usage")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: C_TOT_MEM
      TYPE(C_PTR), VALUE, INTENT(IN) :: C_SYS_USAGE
      TYPE(C_PTR), VALUE, INTENT(IN) :: C_TASK_USAGE
    END SUBROUTINE C_GET_MEM_USAGE
  END INTERFACE

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

  ! Initialisation of the variables
  C_TOT_MEM    = INT( -99, C_INT64_T)
  C_SYS_USAGE  = INT( -99, C_INT64_T)
  C_TASK_USAGE = INT( -99, C_INT64_T)

  ! Call the utility to read the memory state
  CALL C_GET_MEM_USAGE( C_LOC(C_TOT_MEM), C_LOC(C_SYS_USAGE), C_LOC(C_TASK_USAGE) )

  ! Cast the result
  TOT_MEM    = INT( C_TOT_MEM,    JPIB_K)
  SYS_USAGE  = INT( C_SYS_USAGE,  JPIB_K)
  TASK_USAGE = INT( C_TASK_USAGE, JPIB_K)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION GET_MEM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IS_LITTLE_ENDIAN'
PP_THREAD_SAFE FUNCTION IS_LITTLE_ENDIAN( LDRET, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

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

  ! Function result
  LOGICAL,       INTENT(OUT)   :: LDRET
  TYPE(HOOKS_T), INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT8_T), TARGET :: C_ISLITTLE

  ! Explicit interfaces
  INTERFACE
    SUBROUTINE C_IS_LITTLE_ENDIAN( C_ISLITTLE ) BIND(C, NAME="om_is_little_endian")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: C_ISLITTLE
    END SUBROUTINE C_IS_LITTLE_ENDIAN
  END INTERFACE

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

  ! Initialisation of the variables
  C_ISLITTLE  = INT( -99,  C_INT8_T)

  ! Call the utility to read the memory state
  CALL C_IS_LITTLE_ENDIAN( C_LOC(C_ISLITTLE) )

  ! Cast the result
  IF ( C_ISLITTLE .EQ. 0_C_INT8_T ) THEN
    LDRET = .FALSE.
  ELSE
    LDRET = .TRUE.
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION IS_LITTLE_ENDIAN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE SYSINFO_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME