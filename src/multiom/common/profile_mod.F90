! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'profile_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'PROFILE_MOD'
MODULE PROFILE_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

! Default visibility
PRIVATE

! Prifile data type
TYPE :: PROFILE_T
  INTEGER(KIND=JPIB_K) :: PROFUNIT_
  INTEGER(KIND=JPIB_K) :: SIMULATION_TIC_
  INTEGER(KIND=JPIB_K) :: FLUSH_TIC_
  INTEGER(KIND=JPIB_K) :: MESSAGE_TIC_
END TYPE


! Whitelist of public symbols (types)
PUBLIC :: PROFILE_T

! Whitelist of public symbols (procedures)
PUBLIC :: PROFILE_START_SIMULATION
PUBLIC :: PROFILE_MESSAGE
PUBLIC :: PROFILE_FLUSH
PUBLIC :: PROFILE_FLUSH_AND_RESTART
PUBLIC :: PROFILE_FLUSH_LAST_STEP
PUBLIC :: PROFILE_END_SIMULATION


CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PROFILE_START_SIMULATION'
PP_THREAD_SAFE FUNCTION PROFILE_START_SIMULATION( PROFILE, DIRECTORY, &
&  PROC_ID, THREAD_ID, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: SYSINFO_MOD,       ONLY: TIC
  USE :: SYSINFO_MOD,       ONLY: GET_MEM

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(PROFILE_T),      INTENT(INOUT) :: PROFILE
  CHARACTER(LEN=*),     INTENT(IN)    :: DIRECTORY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PROC_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: THREAD_ID
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variabels
  INTEGER(KIND=JPIB_K) :: NOW
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: TMP
  INTEGER(KIND=JPIB_K) :: DUMMY

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_TIC=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_MEMORY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN_PROFILE_FILE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_PROFILE_DATA=4_JPIB_K

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

  TMP = 0

  ! Get the current time
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_TIC)  TIC( NOW, HOOKS )

  PROFILE%SIMULATION_TIC_ = NOW
  PROFILE%FLUSH_TIC_      = NOW
  PROFILE%MESSAGE_TIC_    = NOW

  ! Inititlise timing
  TMP(1) = 0
  TMP(2) = 0
  TMP(3) = 0

  ! Get the current memory consumption
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_MEMORY) GET_MEM( TMP(4), TMP(5), TMP(6), HOOKS )

  ! Write profile data
  DUMMY = -99
  PP_TRYCALL(ERRFLAG_UNABLE_TO_OPEN_PROFILE_FILE) PROFILE_WOPEN( DIRECTORY, PROC_ID, THREAD_ID, PROFILE%PROFUNIT_, NOW, TMP(4), HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_PROFILE_DATA) PROFILE_WRITE_DATA( PROFILE%PROFUNIT_, 'START SIM.', TMP, TMP(1), DUMMY, DUMMY, HOOKS )

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_GET_TIC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get the current time' )
    CASE (ERRFLAG_UNABLE_TO_READ_MEMORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read the memory consumption' )
    CASE (ERRFLAG_UNABLE_TO_OPEN_PROFILE_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to open the profile file' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_PROFILE_DATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to write the profile data' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION PROFILE_START_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PROFILE_MESSAGE'
PP_THREAD_SAFE FUNCTION PROFILE_MESSAGE( PROFILE, STEP, PARAMID, UID, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: SYSINFO_MOD,       ONLY: TIC
  USE :: SYSINFO_MOD,       ONLY: TOC
  USE :: SYSINFO_MOD,       ONLY: GET_MEM

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(PROFILE_T),      INTENT(INOUT) :: PROFILE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PARAMID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UID
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variabels
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: TMP
  INTEGER(KIND=JPIB_K) :: NOW

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_SIM=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_MSG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_GET_MEM=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TIC=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_DATA=6_JPIB_K

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

  ! Time elapsed from the beginning of the simulation
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_SIM) TOC( PROFILE%SIMULATION_TIC_, TMP(1), HOOKS )

  ! Time elapsed from the last flush
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH) TOC( PROFILE%FLUSH_TIC_, TMP(2), HOOKS )

  ! Time elapsed from the last message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_MSG) TOC( PROFILE%MESSAGE_TIC_, TMP(3), HOOKS )

  ! Get the current memory consumption
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_GET_MEM) GET_MEM( TMP(4), TMP(5), TMP(6), HOOKS )

  ! Get the current time
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TIC)  TIC( NOW, HOOKS )

  ! Update the last message time
  PROFILE%MESSAGE_TIC_ = NOW

  ! Write profile data
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_DATA) PROFILE_WRITE_DATA( PROFILE%PROFUNIT_, &
&            'MESSAGE', TMP, STEP, PARAMID, UID, HOOKS )

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_SIM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for simulation time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for flush time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for message time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_GET_MEM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call GET_MEM' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TIC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TIC' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_DATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to write the profile data' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION PROFILE_MESSAGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PROFILE_FLUSH'
PP_THREAD_SAFE FUNCTION PROFILE_FLUSH( PROFILE, STEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: SYSINFO_MOD,       ONLY: TIC
  USE :: SYSINFO_MOD,       ONLY: TOC
  USE :: SYSINFO_MOD,       ONLY: GET_MEM

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(PROFILE_T),      INTENT(INOUT) :: PROFILE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variabels
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: TMP
  INTEGER(KIND=JPIB_K) :: NOW
  INTEGER(KIND=JPIB_K) :: DUMMY

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_SIM=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_MSG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_GET_MEM=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TIC=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_DATA=6_JPIB_K

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

  ! Time elapsed from the beginning of the simulation
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_SIM) TOC( PROFILE%SIMULATION_TIC_, TMP(1), HOOKS )

  ! Time elapsed from the last flush
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH) TOC( PROFILE%FLUSH_TIC_, TMP(2), HOOKS )

  ! Time elapsed from the last message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_MSG) TOC( PROFILE%MESSAGE_TIC_, TMP(3), HOOKS )

  ! Get the current memory consumption
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_GET_MEM) GET_MEM( TMP(4), TMP(5), TMP(6), HOOKS )

  ! Get the current time
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TIC) TIC( NOW, HOOKS )

  ! Update the last message time
  PROFILE%MESSAGE_TIC_ = NOW
  PROFILE%FLUSH_TIC_   = NOW

  ! Write profile data
  DUMMY = -99
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_DATA) PROFILE_WRITE_DATA( PROFILE%PROFUNIT_, &
&    'FLUSH', TMP, STEP, DUMMY, DUMMY, HOOKS )

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_SIM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for simulation time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for flush time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for message time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_GET_MEM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call GET_MEM' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TIC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TIC' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_DATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to write the profile data' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION PROFILE_FLUSH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PROFILE_FLUSH_AND_RESTART'
PP_THREAD_SAFE FUNCTION PROFILE_FLUSH_AND_RESTART( PROFILE, STEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: SYSINFO_MOD,       ONLY: TIC
  USE :: SYSINFO_MOD,       ONLY: TOC
  USE :: SYSINFO_MOD,       ONLY: GET_MEM

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(PROFILE_T),      INTENT(INOUT) :: PROFILE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variabels
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: TMP
  INTEGER(KIND=JPIB_K) :: NOW
  INTEGER(KIND=JPIB_K) :: DUMMY

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_SIM=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_MSG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_GET_MEM=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TIC=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_DATA=6_JPIB_K

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

  ! Time elapsed from the beginning of the simulation
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_SIM) TOC( PROFILE%SIMULATION_TIC_, TMP(1), HOOKS )

  ! Time elapsed from the last flush
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH) TOC( PROFILE%FLUSH_TIC_, TMP(2), HOOKS )

  ! Time elapsed from the last message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_MSG) TOC( PROFILE%MESSAGE_TIC_, TMP(3), HOOKS )

  ! Get the current memory consumption
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_GET_MEM) GET_MEM( TMP(4), TMP(5), TMP(6), HOOKS )

  ! Get the current time
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TIC) TIC( NOW, HOOKS )

  ! Update the last message time
  PROFILE%MESSAGE_TIC_ = NOW
  PROFILE%FLUSH_TIC_   = NOW

  ! Write profile data
  DUMMY = -99
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_DATA) PROFILE_WRITE_DATA( PROFILE%PROFUNIT_, &
&   'FLUSH/RESTART', TMP, STEP, DUMMY, DUMMY, HOOKS )

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_SIM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for simulation time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for flush time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for message time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_GET_MEM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call GET_MEM' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TIC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TIC' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_DATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to write the profile data' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION PROFILE_FLUSH_AND_RESTART
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PROFILE_FLUSH_LAST_STEP'
PP_THREAD_SAFE FUNCTION PROFILE_FLUSH_LAST_STEP( PROFILE, STEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: SYSINFO_MOD,       ONLY: TIC
  USE :: SYSINFO_MOD,       ONLY: TOC
  USE :: SYSINFO_MOD,       ONLY: GET_MEM

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(PROFILE_T),      INTENT(INOUT) :: PROFILE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variabels
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: TMP
  INTEGER(KIND=JPIB_K) :: NOW
  INTEGER(KIND=JPIB_K) :: DUMMY

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_SIM=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_MSG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_GET_MEM=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TIC=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_DATA=6_JPIB_K

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

  ! Time elapsed from the beginning of the simulation
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_SIM) TOC( PROFILE%SIMULATION_TIC_, TMP(1), HOOKS )

  ! Time elapsed from the last flush
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH) TOC( PROFILE%FLUSH_TIC_, TMP(2), HOOKS )

  ! Time elapsed from the last message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_MSG) TOC( PROFILE%MESSAGE_TIC_, TMP(3), HOOKS )

  ! Get the current memory consumption
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_GET_MEM) GET_MEM( TMP(4), TMP(5), TMP(6), HOOKS )

  ! Get the current time
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TIC) TIC( NOW, HOOKS )

  ! Update the last message time
  PROFILE%MESSAGE_TIC_ = NOW
  PROFILE%FLUSH_TIC_   = NOW

  ! Write profile data
  DUMMY = -99
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_DATA) PROFILE_WRITE_DATA( PROFILE%PROFUNIT_, &
&         'LAST STEP', TMP, STEP, DUMMY, DUMMY, HOOKS )

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_SIM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for simulation time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for flush time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for message time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_GET_MEM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call GET_MEM' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TIC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TIC' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_DATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to write the profile data' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION PROFILE_FLUSH_LAST_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PROFILE_END_SIMULATION'
PP_THREAD_SAFE FUNCTION PROFILE_END_SIMULATION( PROFILE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: SYSINFO_MOD,       ONLY: TIC
  USE :: SYSINFO_MOD,       ONLY: TOC
  USE :: SYSINFO_MOD,       ONLY: GET_MEM

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(PROFILE_T), INTENT(INOUT) :: PROFILE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variabels
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: TMP
  INTEGER(KIND=JPIB_K) :: NOW
  INTEGER(KIND=JPIB_K) :: DUMMY

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_SIM=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TOC_MSG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_GET_MEM=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_DATA=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CLOSE_PROFILE_FILE=6_JPIB_K

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

  ! Time elapsed from the beginning of the simulation
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_SIM) TOC( PROFILE%SIMULATION_TIC_, TMP(1), HOOKS )

  ! Time elapsed from the last flush
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH) TOC( PROFILE%FLUSH_TIC_, TMP(2), HOOKS )

  ! Time elapsed from the last message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TOC_MSG) TOC( PROFILE%MESSAGE_TIC_, TMP(3), HOOKS )

  ! Get the current memory consumption
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_GET_MEM) GET_MEM( TMP(4), TMP(5), TMP(6), HOOKS )

  ! Write profile data
  DUMMY = -99
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_DATA) PROFILE_WRITE_DATA( PROFILE%PROFUNIT_, &
&            'END OF SIM.', TMP, DUMMY, DUMMY, DUMMY, HOOKS )

  ! Close the file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CLOSE_PROFILE_FILE) PROFILE_CLOSE( PROFILE%PROFUNIT_, HOOKS )

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_SIM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for simulation time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_FLUSH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for flush time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_TOC_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call TOC for message time' )
    CASE (ERRFLAG_UNABLE_TO_CALL_GET_MEM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call GET_MEM' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_DATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to write the profile data' )
    CASE (ERRFLAG_UNABLE_TO_CLOSE_PROFILE_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to close the profile file' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION PROFILE_END_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PROFILE_WOPEN'
PP_THREAD_SAFE FUNCTION PROFILE_WOPEN( DIRECTORY, PROC_ID, THREAD_ID, PROFUNIT, NOW, TOTMEM, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: SYSINFO_MOD,       ONLY: GET_HOSTNAME

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)  :: DIRECTORY
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: PROC_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: THREAD_ID
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: PROFUNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: NOW
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: TOTMEM
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=128)   :: FILENAME
  CHARACTER(LEN=256)   :: HOSTNAME
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_HOSTNAME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_FILENAME=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN_FILE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE=4_JPIB_K

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

  ! Initialise filename
  FILENAME = REPEAT(' ',128)
  HOSTNAME = REPEAT(' ',256)
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_HOSTNAME) GET_HOSTNAME( HOSTNAME, HOOKS )

  ! Create the filename
  WRITE( FILENAME, '(A,A,I6.6,A,I6.6,A)', IOSTAT=STAT ) TRIM(DIRECTORY), '/profile_', PROC_ID, '_', THREAD_ID ,'.dat'
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_FILENAME )

  ! Open the file
  OPEN( NEWUNIT=PROFUNIT, FILE=FILENAME, STATUS='REPLACE', ACTION='WRITE', FORM='FORMATTED', IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_OPEN_FILE )

  ! Write the header
  WRITE(PROFUNIT,'(A,A)', IOSTAT=STAT )  '# HOSTNAME ::', TRIM(ADJUSTL(HOSTNAME))
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A,I8)', IOSTAT=STAT ) '# PROC_IDX ::', PROC_ID
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A,I8)', IOSTAT=STAT ) '# THREAD_IDX ::', THREAD_ID
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A)', IOSTAT=STAT )    '# '
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A)', IOSTAT=STAT )    '# This file has been automatically geneated by:'
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A,A)', IOSTAT=STAT )  '# -> file........... ::', PP_FILE_NAME
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A,A)', IOSTAT=STAT )  '# -> sectionType.... ::', PP_SECTION_TYPE
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A,A)', IOSTAT=STAT )  '# -> sectionName.... ::', PP_SECTION_NAME
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A,A)', IOSTAT=STAT )  '# -> procedureType.. ::', PP_PROCEDURE_TYPE
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A,A)', IOSTAT=STAT )  '# -> procedureName.. ::', PP_PROCEDURE_NAME
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A,I8)', IOSTAT=STAT ) '# -> line........... ::', __LINE__
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A)', IOSTAT=STAT )    '# '
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A)', IOSTAT=STAT )    '# TAG :: Position at which the time is measured'
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A)', IOSTAT=STAT )    '# T1  :: Time from the beginning of the simulation'
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A)', IOSTAT=STAT )    '# T2  :: Time from the last flush'
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A)', IOSTAT=STAT )    '# T3  :: Time from the last message'
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A)', IOSTAT=STAT )    '# M1  :: System memory usage in Bytes'
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A)', IOSTAT=STAT )    '# M2  :: Task memory usage in Bytes'
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A,I16)', IOSTAT=STAT ) '# '
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A,I16)', IOSTAT=STAT ) '# Time of start of the simulation in [ns]:     ', NOW
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A,I16)', IOSTAT=STAT ) '# Total memory available on the node in bytes: ', TOTMEM
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A,I16)', IOSTAT=STAT ) '# '
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A1,A)', IOSTAT=STAT ) '#', REPEAT('-',153)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1)', IOSTAT=STAT ) '|', &
&  '   TAG          ',  '|', &
&  '   STEP         ',  '|', &
&  '   PARAM_ID     ',  '|', &
&  '   UID          ',  '|', &
&  '   T1 [ns]      ',  '|', &
&  '   T2 [ns]      ',  '|', &
&  '   T3 [ns]      ',  '|', &
&  '   M1 [B]       ',  '|', &
&  '   M2 [B]       ',  '|'
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(PROFUNIT,'(A1,A)', IOSTAT=STAT ) '#', REPEAT('-',153)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  FLUSH(PROFUNIT)

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
    CASE (ERRFLAG_UNABLE_TO_GET_HOSTNAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the hostname' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_FILENAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the filename' )
    CASE (ERRFLAG_UNABLE_TO_OPEN_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open profile file: '//TRIM(ADJUSTL(FILENAME)) )
    CASE (ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write to profile file' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION PROFILE_WOPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PROFILE_WRITE_DATA'
PP_THREAD_SAFE FUNCTION PROFILE_WRITE_DATA( PROFUNIT, PROFTAG, PROFDATA, STEP, PARAM_ID, UID, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K),               INTENT(IN)    :: PROFUNIT
  CHARACTER(LEN=*),                   INTENT(IN)    :: PROFTAG
  INTEGER(KIND=JPIB_K), DIMENSION(6), INTENT(IN)    :: PROFDATA
  INTEGER(KIND=JPIB_K),               INTENT(IN)    :: STEP
  INTEGER(KIND=JPIB_K),               INTENT(IN)    :: PARAM_ID
  INTEGER(KIND=JPIB_K),               INTENT(IN)    :: UID
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: EX
  LOGICAL :: OPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_NOT_OPENED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE=2_JPIB_K

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

  ! Initialise filename
  INQUIRE( UNIT=PROFUNIT, OPENED=EX )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_FILE_NOT_OPENED )

  WRITE(PROFUNIT,'(A1,A16,A1,I16,A1,I16,A1,I16,A1,I16,A1,I16,A1,I16,A1,I16,A1,I16,A1)', IOSTAT=STAT) '|', &
&  PROFTAG,     '|', &
&  STEP,        '|', &
&  PARAM_ID,    '|', &
&  UID,         '|', &
&  PROFDATA(1), '|', &
&  PROFDATA(2), '|', &
&  PROFDATA(3), '|', &
&  PROFDATA(5), '|', &
&  PROFDATA(6), '|'
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_UNABLE_TO_WRITE )

  FLUSH(PROFUNIT)

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
    CASE (ERRFLAG_FILE_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Profile file not opened' )
    CASE (ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write to profile file' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION PROFILE_WRITE_DATA
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PROFILE_CLOSE'
PP_THREAD_SAFE FUNCTION PROFILE_CLOSE( PROFUNIT, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PROFUNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: EX
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_NOT_OPENED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CLOSE_PROFILE_FILE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE=3_JPIB_K

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

  ! Initialise filename
  INQUIRE( UNIT=PROFUNIT, OPENED=EX )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_FILE_NOT_OPENED )

  ! Footer
  WRITE(PROFUNIT,'(A1,A)', IOSTAT=STAT) '#', REPEAT('-',153)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  ! Open the file
  CLOSE( UNIT=PROFUNIT, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_CLOSE_PROFILE_FILE )


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
    CASE (ERRFLAG_FILE_NOT_OPENED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Profile file not opened' )
    CASE (ERRFLAG_UNABLE_TO_CLOSE_PROFILE_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close the profile file' )
    CASE (ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write to profile file' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION PROFILE_CLOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE PROFILE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
