!> @file noon_output_manager_mod.F90
!>
!> @brief Definition of an empty output manager
!>
!> @author Mirco Valentini
!> @date February 12, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'dump_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'DUMP_MOD'
MODULE DUMP_MOD

  ! Symbols imported from other modules within the project.
  USE :: OUTUPUT_MANAGER_BASE_MOD, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,          ONLY: UNDEF_PARAM_E
  USE :: IFS_PAR_MOD,              ONLY: PROC_TOPO_T
  USE :: IFS_PAR_MOD,              ONLY: MODEL_PAR_T
  USE :: PROFILE_MOD,              ONLY: PROFILE_T

IMPLICIT NONE

! Default visibility
PRIVATE

!> Output manager name
CHARACTER(LEN=*), PARAMETER :: DUMP_OMNAME='DUMP-FORTRAN-DATA-REPRODUCER'

!>
!> @brief Definition of the `DUMP_OUTPUT_MANAGER_T` derived type.
!>
!> The `DUMP_OUTPUT_MANAGER_T` type is a derived type that extends the
!> functionality of the `OUTPUT_MANAGER_BASE_A` abstract interface to be
!> used to demonstrate behavior without I/O when addressing complaints
!> or testing scenarios.
!>
!> @see OUTPUT_MANAGER_BASE_A
!>
TYPE, EXTENDS(OUTPUT_MANAGER_BASE_A) :: DUMP_OUTPUT_MANAGER_T

  !> Default visibility
  PRIVATE

  !> Process ID
  INTEGER(KIND=JPIB_K) :: PID_=UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: TID_=UNDEF_PARAM_E
  INTEGER(KIND=JPIB_K) :: NUM_THREADS_=UNDEF_PARAM_E

  !> Hostname
  CHARACTER(LEN=256) :: HOSTNAME_=REPEAT(' ',256)

  !> All the profiling data
  TYPE(PROFILE_T) :: PROFILE_DATA_

  !> Multiprocessor topology
  TYPE(PROC_TOPO_T), POINTER :: TOPOLOGY_ => NULL()

  !> Model parameters
  TYPE(MODEL_PAR_T), POINTER :: MODEL_PAR_ => NULL()

  !> Counter used to update the message file
  INTEGER(KIND=JPIB_K) :: CNT_ = UNDEF_PARAM_E

  !> Folder used to dump the files
  CHARACTER(LEN=1024)  :: OUT_DIR_ = REPEAT(' ',1024)

  !> Unit used to write toc
  INTEGER(KIND=JPIB_K) :: TOC_UNIT_ = UNDEF_PARAM_E

  !> Unit used to write messages
  INTEGER(KIND=JPIB_K) :: MSG_UNIT_ = UNDEF_PARAM_E

  !> Unit used to write values
  INTEGER(KIND=JPIB_K) :: VAL_UNIT_ = UNDEF_PARAM_E

  !> Counter of the toc entries
  INTEGER(KIND=JPIB_K) :: TOC_COUNTER_ = UNDEF_PARAM_E

  !> Write position in the toc
  INTEGER(KIND=JPIB_K) :: TOC_WRITPOS_ = UNDEF_PARAM_E

  !> Enable profiling
  LOGICAL :: PROFILE_ = .FALSE.

  !> Enable verbose output for debug
  LOGICAL :: VERBOSE_ = .FALSE.

  !> If false values are not dumped
  LOGICAL :: DUMP_VALUES_ = .FALSE.

  !> Unit used for logging purposes (if needed)
  INTEGER(KIND=JPIB_K) :: LOG_UNIT_ = -99

  !> File used for logging purposes (if needed)
  CHARACTER(LEN=1024)  :: LOG_FNAME_ = REPEAT(' ',1024)

  !> Files used for dump
  CHARACTER(LEN=1024)  :: IO_SERV_DIR_ = REPEAT(' ',1024)
  CHARACTER(LEN=1024)  :: MSG_FNAME_ = REPEAT(' ',1024)
  CHARACTER(LEN=1024)  :: VAL_FNAME_ = REPEAT(' ',1024)
  CHARACTER(LEN=1024)  :: PAR_FNAME_ = REPEAT(' ',1024)
  CHARACTER(LEN=1024)  :: TOC_FNAME_ = REPEAT(' ',1024)

CONTAINS

  !> @brief Setup of the output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: SETUP => DUMP_SETUP

  !> @brief Write fields for the output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_ATM_DP => DUMP_WRITE_ATM_DP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_ATM_SP => DUMP_WRITE_ATM_SP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_WAM_DP => DUMP_WRITE_WAM_DP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_WAM_SP => DUMP_WRITE_WAM_SP

  !> @brief Notify that a step is finished
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_STEP => DUMP_FLUSH_STEP

  !> @brief Notify that a step is finished and it was the last step
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_LAST_STEP => DUMP_FLUSH_LAST_STEP

  !> @brief Notify that a step is finished and it is necessary to dump a restart
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_STEP_AND_TRIGGER_RESTART => DUMP_FLUSH_STEP_AND_TRIGGER_RESTART

  !> @brief Finalise ancd cleanup teh output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FINALISE => DUMP_FINALISE


  !> @brief Read the configuration from YAML using fckit
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_CFG_FROM_YAML => DUMP_READ_CFG_FROM_YAML
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_PATH_FROM_YAML => DUMP_READ_PATH_FROM_YAML
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_VERBOSE_FROM_YAML => DUMP_READ_VERBOSE_FROM_YAML
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_PROFILE_FROM_YAML => DUMP_READ_PROFILE_FROM_YAML
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_DUMP_VALUES_FROM_YAML => DUMP_READ_DUMP_VALUES_FROM_YAML

  END TYPE

  ! Whitelist of public symbols
  PUBLIC :: DUMP_OUTPUT_MANAGER_T
  PUBLIC :: DUMP_OMNAME

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_READ_CFG_FROM_YAML'
PP_THREAD_SAFE FUNCTION DUMP_READ_CFG_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD,   ONLY: TOLOWER
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_LOGICAL

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(YAML_CONFIGURATION_T) :: DUMP_CFG
  CHARACTER(LEN=LEN(DUMP_OMNAME)) :: DUMP_OMNAME_LC
  LOGICAL :: HAS_SUBKEY
  LOGICAL :: HAS_KEY

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_LOWER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HAS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_SUBCFG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_PATH=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_VERBOSE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_PROFILE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_DUMP_VALUES=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTRY_CFG=8_JPIB_K

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

  ! Convert the kind of output manager to lowercase
  DUMP_OMNAME_LC = REPEAT(' ',LEN(DUMP_OMNAME_LC))
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_LOWER) TOLOWER( DUMP_OMNAME, DUMP_OMNAME_LC, HOOKS )

  ! Get sub-configuration from input file
  PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, DUMP_OMNAME_LC, HAS_KEY, HOOKS )

  ! If has specific configuration then try to read it
  IF ( HAS_KEY ) THEN

    ! Get the specific sub-configuration
    PP_TRYCALL(ERRFLAG_GET_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, DUMP_OMNAME_LC, DUMP_CFG, HOOKS )

    ! Read options from configuration file
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_PATH) THIS%READ_PATH_FROM_YAML( DUMP_CFG, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VERBOSE) THIS%READ_VERBOSE_FROM_YAML( DUMP_CFG, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_PROFILE) THIS%READ_PROFILE_FROM_YAML( DUMP_CFG, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_DUMP_VALUES) THIS%READ_DUMP_VALUES_FROM_YAML( DUMP_CFG, HOOKS )

    ! Deallocate the dump-output-manager object
    PP_TRYCALL(ERRFLAG_UNABLE_TO_DESTRY_CFG) YAML_DELETE_CONFIGURATION( DUMP_CFG, HOOKS )

  ELSE

    ! Deallocate the dump-output-manager object (paranoid)
    THIS%OUT_DIR_ = './'
    THIS%PROFILE_ = .FALSE.
    THIS%VERBOSE_ = .FALSE.
    THIS%DUMP_VALUES_ = .FALSE.

  ENDIF

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
    CASE (ERRFLAG_UNABLE_TO_CONVERT_TO_LOWER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert the output manager name to lowercase' )
    CASE (ERRFLAG_HAS_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find the output manager name in the configuration file' )
    CASE (ERRFLAG_GET_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the sub-configuration for the output manager' )
    CASE (ERRFLAG_UNABLE_TO_READ_PATH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the path from the configuration file' )
    CASE (ERRFLAG_UNABLE_TO_READ_VERBOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the verbose flag' )
    CASE (ERRFLAG_UNABLE_TO_READ_PROFILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the profile flag' )
    CASE (ERRFLAG_UNABLE_TO_READ_DUMP_VALUES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the dump values flag' )
    CASE (ERRFLAG_UNABLE_TO_DESTRY_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to destroy the sub-configuration for the output manager' )
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

  ! Exit point on error
  RETURN

END FUNCTION DUMP_READ_CFG_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_READ_PATH_FROM_YAML'
PP_THREAD_SAFE FUNCTION DUMP_READ_PATH_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING_WITH_ENV_EXPANSION
  USE :: GENERAL_UTILS_MOD,   ONLY: IS_DIRECTORY

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: HAS_KEY
  CHARACTER(LEN=:), ALLOCATABLE :: PATH
  INTEGER(KIND=JPIB_K) :: STAT
  LOGICAL :: IS_DIR

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HAS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_SUBCFG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED_AFTER_READ=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOO_LONG=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHECK_OUT_DIR=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_PATH_DOES_NOT_EXIST=7_JPIB_K

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

  ! Read from YAML file the flag to enable verbose execution
  PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, 'output-path', HAS_KEY, HOOKS )

  ! Read or apply the default configuration
  THIS%OUT_DIR_ = REPEAT(' ',LEN(THIS%OUT_DIR_))
  IF ( HAS_KEY ) THEN
    IF ( ALLOCATED(PATH) ) THEN
      DEALLOCATE(PATH, STAT=STAT)
    ENDIF
    PP_TRYCALL(ERRFLAG_GET_SUBCFG) YAML_READ_STRING_WITH_ENV_EXPANSION( CFG, 'output-path', PATH, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(PATH), ERRFLAG_NOT_ALLOCATED_AFTER_READ )
    PP_DEBUG_CRITICAL_COND_THROW( LEN(PATH).GT.LEN(THIS%OUT_DIR_), ERRFLAG_TOO_LONG )
    THIS%OUT_DIR_ = PATH(:)
    IF ( ALLOCATED(PATH) ) THEN
      DEALLOCATE( PATH, STAT=STAT )
    ENDIF
    PP_TRYCALL(ERRFLAG_CHECK_OUT_DIR) IS_DIRECTORY( THIS%OUT_DIR_, IS_DIR, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_DIR, ERRFLAG_OUTPUT_PATH_DOES_NOT_EXIST )
  ELSE
    THIS%OUT_DIR_ = './'
  ENDIF

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
    CASE (ERRFLAG_HAS_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find the "output-path" keyword in the configuration file' )
    CASE (ERRFLAG_GET_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the "output-path" sub-configuration for the output manager' )
    CASE (ERRFLAG_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The string has not been allocated after reading' )
    CASE (ERRFLAG_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The string is too long' )
    CASE (ERRFLAG_CHECK_OUT_DIR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check if output path exist' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'dirname: '//TRIM(ADJUSTL(THIS%OUT_DIR_))//'"' )
    CASE (ERRFLAG_OUTPUT_PATH_DOES_NOT_EXIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The output path does not exist' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'dirname: '//TRIM(ADJUSTL(THIS%OUT_DIR_))//'"' )
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

  ! Exit point on error
  RETURN

END FUNCTION DUMP_READ_PATH_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_READ_VERBOSE_FROM_YAML'
PP_THREAD_SAFE FUNCTION DUMP_READ_VERBOSE_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_LOGICAL

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: HAS_KEY

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HAS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_SUBCFG=3_JPIB_K

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

  ! Read from YAML file the flag to enable verbose execution
  PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, 'verbose', HAS_KEY, HOOKS )

  ! Read or apply the default configuration
  IF ( HAS_KEY ) THEN
    PP_TRYCALL(ERRFLAG_GET_SUBCFG) YAML_READ_LOGICAL( CFG, 'verbose', THIS%VERBOSE_, HOOKS )
  ELSE
    THIS%VERBOSE_ = .FALSE.
  ENDIF

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
    CASE (ERRFLAG_HAS_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find the "verbose" keyword in the configuration file' )
    CASE (ERRFLAG_GET_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the "verbose" sub-configuration for the output manager' )
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

  ! Exit point on error
  RETURN

END FUNCTION DUMP_READ_VERBOSE_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_READ_PROFILE_FROM_YAML'
PP_THREAD_SAFE FUNCTION DUMP_READ_PROFILE_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_LOGICAL

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: HAS_KEY

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HAS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_SUBCFG=3_JPIB_K

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

  ! Read from YAML file the flag to enable profile execution
  PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, 'profile', HAS_KEY, HOOKS )

  ! Read or apply the default configuration
  IF ( HAS_KEY ) THEN
    PP_TRYCALL(ERRFLAG_GET_SUBCFG) YAML_READ_LOGICAL( CFG, 'profile', THIS%PROFILE_, HOOKS )
  ELSE
    THIS%PROFILE_ = .FALSE.
  ENDIF

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
    CASE (ERRFLAG_HAS_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find the "profile" keyword in the configuration file' )
    CASE (ERRFLAG_GET_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the "profile" sub-configuration for the output manager' )
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

  ! Exit point on error
  RETURN

END FUNCTION DUMP_READ_PROFILE_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_READ_DUMP_VALUES_FROM_YAML'
PP_THREAD_SAFE FUNCTION DUMP_READ_DUMP_VALUES_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING_WITH_ENV_EXPANSION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: HAS_KEY
  CHARACTER(LEN=:), ALLOCATABLE :: CDUMPVALUES
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED_AFTER_READ=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HAS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_SUBCFG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_DUMP_VALUES=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=5_JPIB_K

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

  ! Read from YAML file the flag to enable dump_values execution
  PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, 'dump-values', HAS_KEY, HOOKS )

  ! Read or apply the default configuration
  IF ( HAS_KEY ) THEN
    IF ( ALLOCATED(CDUMPVALUES) ) THEN
      DEALLOCATE(CDUMPVALUES, STAT=STAT, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE )
    ENDIF
    PP_TRYCALL(ERRFLAG_GET_SUBCFG) YAML_READ_STRING_WITH_ENV_EXPANSION( CFG, 'dump-values', CDUMPVALUES, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CDUMPVALUES), ERRFLAG_NOT_ALLOCATED_AFTER_READ )
    READ( CDUMPVALUES, *, IOSTAT=STAT ) THIS%DUMP_VALUES_
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_DUMP_VALUES )
    IF ( ALLOCATED(CDUMPVALUES) ) THEN
      DEALLOCATE( CDUMPVALUES, STAT=STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE )
    ENDIF


  ELSE
    THIS%DUMP_VALUES_ = .FALSE.
  ENDIF

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
    CASE (ERRFLAG_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The string has not been allocated after reading' )
    CASE (ERRFLAG_HAS_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find the "dump-values" keyword in the configuration file' )
    CASE (ERRFLAG_GET_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the "dump-values" sub-configuration for the output manager' )
    CASE (ERRFLAG_UNABLE_TO_READ_DUMP_VALUES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the dump values flag' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate the string' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: "'//TRIM(ADJUSTL(ERRMSG))//'"' )
        DEALLOCATE( ERRMSG, STAT=STAT)
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

  ! Exit point on error
  RETURN

END FUNCTION DUMP_READ_DUMP_VALUES_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Initializes the output manager from an instance of the IO server.
!>
!> This procedure initializes the object using the information
!> contained in the instance of the IOServer. As a `DUMP`
!> output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!> @param [in]    PROCESSOR_TOPO Processor topology to be used in a multiprocessor run
!> @param [in]    MODEL_PARAMS   Model parameters that are frozen during the simulation
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_SETUP'
PP_THREAD_SAFE FUNCTION DUMP_SETUP( THIS, YAMLFNAME, PROCESSOR_TOPO, MODEL_PARAMS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  USE :: IFS_PAR_MOD, ONLY: PROC_TOPO_T
  USE :: IFS_PAR_MOD, ONLY: MODEL_PAR_T
  USE :: IFS_PAR_MOD, ONLY: PAR_PRINT

  USE :: SYSINFO_MOD, ONLY: GET_TID
  USE :: SYSINFO_MOD, ONLY: GET_PID
  USE :: SYSINFO_MOD, ONLY: GET_NUM_THREADS
  USE :: SYSINFO_MOD, ONLY: GET_HOSTNAME
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: GENERAL_UTILS_MOD, ONLY: IS_DIRECTORY
  USE :: GENERAL_UTILS_MOD, ONLY: MAKE_DIRECTORY
  USE :: GENERAL_UTILS_MOD, ONLY: FILE_SET_PERMISSION

  USE :: LOG_INFO_MOD, ONLY: LOG_VERSION
  USE :: LOG_INFO_MOD, ONLY: LOG_CURR_TIME
  USE :: LOG_INFO_MOD, ONLY: LOG_SYSINFO

  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_NEW_CONFIGURATION_FROM_FILE
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION

  USE :: PROFILE_MOD, ONLY: PROFILE_START_SIMULATION

  USE :: IFS_PAR_MOD, ONLY: PAR_CREATE_NAME
  USE :: IFS_PAR_MOD, ONLY: PAR_WOPEN
  USE :: IFS_PAR_MOD, ONLY: PAR_WRITE
  USE :: IFS_PAR_MOD, ONLY: PAR_CLOSE

  USE :: IFS_MSG_MOD, ONLY: MSG_CREATE_NAME
  USE :: IFS_MSG_MOD, ONLY: MSG_WOPEN

  USE :: IFS_VAL_MOD, ONLY: VAL_CREATE_NAME
  USE :: IFS_VAL_MOD, ONLY: VAL_WOPEN

  USE :: IFS_TOC_MOD, ONLY: TOC_CREATE_NAME
  USE :: IFS_TOC_MOD, ONLY: TOC_WOPEN
  USE :: IFS_TOC_MOD, ONLY: TOC_WRITE_FLUSH_BEGIN_OF_SIMULATION

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: YAMLFNAME
  TYPE(PROC_TOPO_T), TARGET,    INTENT(IN)    :: PROCESSOR_TOPO
  TYPE(MODEL_PAR_T), TARGET,    INTENT(IN)    :: MODEL_PARAMS
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: FEXISTS
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: I
  TYPE(YAML_CONFIGURATION_T) :: MAIN_CFG
  CHARACTER(LEN=LEN(DUMP_OMNAME)) :: DUMP_OMNAME_LC
  INTEGER(KIND=JPIB_K) :: PAR_UNIT
  LOGICAL :: IS_DIR

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONFIGURATION_FILE_NOT_FOUND=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN_CFG_FILE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SPECIFIC_CFG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_LOG_FILENAME=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN_LOG_FILE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_CURRTIME=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_VERSION=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_SYSINFO=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_PARAMETERS=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_PID=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_HOSTNAME=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_CFG=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_LOWER=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_START=14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CREATE_PAR_FILE_NAME=15_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WOPEN_PAR_FILE=16_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_PAR_FILE=17_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CLOSE_PAR_FILE=18_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CREATE_MSG_FILE_NAME=19_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WOPEN_MSG_FILE=20_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CREATE_VAL_FILE_NAME=21_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WOPEN_VAL_FILE=22_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CREATE_TOC_FILE_NAME=23_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WOPEN_TOC_FILE=24_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_FLUSH_BEGIN_OF_SIMULATION=25_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_TID=26_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_NUM_THREADS=27_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAKE_DUMP_DIR=28_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHECK_DUMP_DIR=29_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_DUMP_DIR_NAME=30_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHANGE_PERMISSION=31_JPIB_K


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

  ! Initialise topology/parameters
  THIS%TOPOLOGY_  => PROCESSOR_TOPO
  THIS%MODEL_PAR_ => MODEL_PARAMS

  ! Initialize debug variables
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_PID) GET_PID( THIS%PID_, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_TID) GET_TID( THIS%TID_, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NUM_THREADS) GET_NUM_THREADS( THIS%NUM_THREADS_, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_HOSTNAME) GET_HOSTNAME( THIS%HOSTNAME_, HOOKS )

  ! Check if the file exsts
  INQUIRE( FILE=TRIM(YAMLFNAME), EXIST=FEXISTS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.FEXISTS, ERRFLAG_CONFIGURATION_FILE_NOT_FOUND )


  ! Open the filename
  PP_TRYCALL(ERRFLAG_UNABLE_TO_OPEN_CFG_FILE) YAML_NEW_CONFIGURATION_FROM_FILE( TRIM(YAMLFNAME), MAIN_CFG, HOOKS )

  ! Read the specific configuration for "DUMP" output manager
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SPECIFIC_CFG) THIS%READ_CFG_FROM_YAML( MAIN_CFG, HOOKS )

  ! Deallocate the dump-output-manager object
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DESTROY_CFG) YAML_DELETE_CONFIGURATION( MAIN_CFG, HOOKS )

  ! Logging
  PP_LOG_DEVELOP_STR( 'Readed configuration file: '//TRIM(YAMLFNAME) )

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    PP_TRYCALL(ERRFLAG_PROFILE_START) PROFILE_START_SIMULATION( THIS%PROFILE_DATA_, TRIM(ADJUSTL(THIS%OUT_DIR_)), &
&                                     PROCESSOR_TOPO%MYPROC_IO, THIS%TID_, HOOKS )
  ENDIF

  ! Logging
  IF ( THIS%VERBOSE_ ) THEN

    ! Convert the kind of output manager to lowercase
    DUMP_OMNAME_LC = REPEAT(' ',LEN(DUMP_OMNAME_LC))
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_LOWER) TOLOWER( DUMP_OMNAME, DUMP_OMNAME_LC, HOOKS )

    ! Create log file name
    THIS%LOG_FNAME_ = REPEAT(' ',LEN(THIS%LOG_FNAME_))
    WRITE(THIS%LOG_FNAME_,'(A,I8.8,A)', IOSTAT=STAT) TRIM(ADJUSTL(DUMP_OMNAME_LC))//'-output-manager-', PROCESSOR_TOPO%MYPROC_IO, '.log'
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_CREATE_LOG_FILENAME )

    ! Open the log file
    OPEN( FILE=TRIM(THIS%LOG_FNAME_), NEWUNIT=THIS%LOG_UNIT_, ACTION='WRITE', STATUS='REPLACE', IOSTAT=STAT )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_OPEN_LOG_FILE )

    ! Print logging information to the log file
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_CURRTIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'SETUP OF THE DUMP OUTPUT MANAGER', HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_VERSION) LOG_VERSION( THIS%LOG_UNIT_, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_SYSINFO) LOG_SYSINFO( THIS%LOG_UNIT_, PROCESSOR_TOPO%NPROC_IO, PROCESSOR_TOPO%MYPROC_IO, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_PARAMETERS) PAR_PRINT( MODEL_PARAMS, THIS%LOG_UNIT_, HOOKS )
  ENDIF

  ! Create the out directory name for the current processor and create the directory
  THIS%IO_SERV_DIR_ = REPEAT(' ',LEN(THIS%IO_SERV_DIR_))
  I = LEN_TRIM(THIS%OUT_DIR_)
  IF ( THIS%OUT_DIR_(I:I) .EQ. '/' ) THEN
    WRITE(THIS%IO_SERV_DIR_, '(A,A,I6.6,A)', IOSTAT=STAT) TRIM(ADJUSTL(THIS%OUT_DIR_)), 'io_serv.', PROCESSOR_TOPO%MYPROC_IO, '.d'
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_CREATE_DUMP_DIR_NAME )
  ELSE
    WRITE(THIS%IO_SERV_DIR_, '(A,A,I6.6,A)', IOSTAT=STAT) TRIM(ADJUSTL(THIS%OUT_DIR_)), '/io_serv.', PROCESSOR_TOPO%MYPROC_IO, '.d'
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_CREATE_DUMP_DIR_NAME )
  ENDIF

  PP_TRYCALL(ERRFLAG_CHECK_DUMP_DIR) IS_DIRECTORY( TRIM(THIS%IO_SERV_DIR_), IS_DIR, HOOKS )
  IF ( .NOT.IS_DIR ) THEN
    PP_TRYCALL(ERRFLAG_MAKE_DUMP_DIR) MAKE_DIRECTORY( TRIM(THIS%IO_SERV_DIR_), 'rwxrwxrwx', HOOKS )
  ENDIF

  ! Parameters
  THIS%PAR_FNAME_ = REPEAT(' ',1024)
  PP_TRYCALL(ERRFLAG_CREATE_PAR_FILE_NAME) PAR_CREATE_NAME( THIS%IO_SERV_DIR_, PROCESSOR_TOPO%MYPROC_IO, THIS%PAR_FNAME_, HOOKS )
  PP_TRYCALL(ERRFLAG_WOPEN_PAR_FILE) PAR_WOPEN( TRIM(THIS%PAR_FNAME_), PAR_UNIT, HOOKS )
  PP_TRYCALL(ERRFLAG_WRITE_PAR_FILE) PAR_WRITE( MODEL_PARAMS, PAR_UNIT, HOOKS )
  PP_TRYCALL(ERRFLAG_CLOSE_PAR_FILE) PAR_CLOSE( PAR_UNIT, HOOKS )
  PP_TRYCALL(ERRFLAG_CHANGE_PERMISSION) FILE_SET_PERMISSION( THIS%PAR_FNAME_, 'rw-rw-rw-', HOOKS )

  ! Messages
  THIS%CNT_ = 0
  THIS%MSG_FNAME_ = REPEAT(' ',1024)
  PP_TRYCALL(ERRFLAG_CREATE_MSG_FILE_NAME) MSG_CREATE_NAME( THIS%IO_SERV_DIR_, THIS%CNT_, PROCESSOR_TOPO%MYPROC_IO, THIS%MSG_FNAME_, HOOKS )
  PP_TRYCALL(ERRFLAG_WOPEN_MSG_FILE) MSG_WOPEN( TRIM(THIS%MSG_FNAME_), THIS%MSG_UNIT_, HOOKS )

  ! Values
  IF ( THIS%DUMP_VALUES_ ) THEN
    THIS%VAL_FNAME_ = REPEAT(' ',1024)
    PP_TRYCALL(ERRFLAG_CREATE_VAL_FILE_NAME) VAL_CREATE_NAME( THIS%IO_SERV_DIR_, THIS%CNT_, PROCESSOR_TOPO%MYPROC_IO, THIS%VAL_FNAME_, HOOKS )
    PP_TRYCALL(ERRFLAG_WOPEN_VAL_FILE) VAL_WOPEN( TRIM(THIS%VAL_FNAME_), THIS%VAL_UNIT_, HOOKS )
  ENDIF

  ! Create the toc file
  PP_TRYCALL(ERRFLAG_CREATE_TOC_FILE_NAME) TOC_CREATE_NAME( THIS%IO_SERV_DIR_, PROCESSOR_TOPO%MYPROC_IO, THIS%TOC_FNAME_, HOOKS )
  PP_TRYCALL(ERRFLAG_WOPEN_TOC_FILE) TOC_WOPEN( TRIM(THIS%TOC_FNAME_), THIS%TOC_UNIT_, THIS%TOC_WRITPOS_, THIS%TOC_COUNTER_, HOOKS )

  ! Add flush begin of simulation to toc
  PP_TRYCALL(ERRFLAG_TOC_FLUSH_BEGIN_OF_SIMULATION) TOC_WRITE_FLUSH_BEGIN_OF_SIMULATION( THIS%TOC_UNIT_, THIS%TOC_WRITPOS_, THIS%TOC_COUNTER_, HOOKS )

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
    CASE (ERRFLAG_CONFIGURATION_FILE_NOT_FOUND)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Configuration file not found' )
    CASE (ERRFLAG_UNABLE_TO_OPEN_CFG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the configuration file' )
    CASE (ERRFLAG_UNABLE_TO_READ_SPECIFIC_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the specific configuration for the output manager' )
    CASE (ERRFLAG_UNABLE_TO_CREATE_LOG_FILENAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the log filename' )
    CASE (ERRFLAG_UNABLE_TO_OPEN_LOG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the log file' )
    CASE (ERRFLAG_UNABLE_TO_LOG_CURRTIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_TO_LOG_VERSION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the version' )
    CASE (ERRFLAG_UNABLE_TO_LOG_SYSINFO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the system information' )
    CASE (ERRFLAG_UNABLE_TO_LOG_PARAMETERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the model parameters' )
    CASE (ERRFLAG_UNABLE_TO_GET_PID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the process ID' )
    CASE (ERRFLAG_UNABLE_TO_GET_HOSTNAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the hostname' )
    CASE (ERRFLAG_UNABLE_TO_DESTROY_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to destroy the configuration' )
    CASE (ERRFLAG_UNABLE_TO_CONVERT_TO_LOWER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert the output manager name to lowercase' )
    CASE (ERRFLAG_PROFILE_START)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to start the profiling' )
    CASE (ERRFLAG_CREATE_PAR_FILE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the parameter file name' )
    CASE (ERRFLAG_WOPEN_PAR_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the parameter file' )
    CASE (ERRFLAG_WRITE_PAR_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the parameter file' )
    CASE (ERRFLAG_CLOSE_PAR_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close the parameter file' )
    CASE (ERRFLAG_CREATE_MSG_FILE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the message file name' )
    CASE (ERRFLAG_WOPEN_MSG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the message file' )
    CASE (ERRFLAG_CREATE_VAL_FILE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the values file name' )
    CASE (ERRFLAG_WOPEN_VAL_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the values file' )
    CASE (ERRFLAG_CREATE_TOC_FILE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the TOC file name' )
    CASE (ERRFLAG_WOPEN_TOC_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the TOC file' )
    CASE (ERRFLAG_TOC_FLUSH_BEGIN_OF_SIMULATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to flush the begin of simulation to the TOC file' )
    CASE (ERRFLAG_UNABLE_TO_GET_TID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the thread ID' )
    CASE (ERRFLAG_UNABLE_TO_GET_NUM_THREADS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the number of threads' )
    CASE (ERRFLAG_MAKE_DUMP_DIR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the dump directory' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'dirname: "'//TRIM(ADJUSTL(THIS%IO_SERV_DIR_))//'"' )
    CASE (ERRFLAG_CHECK_DUMP_DIR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check if the dump directory exists' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'dirname: "'//TRIM(ADJUSTL(THIS%IO_SERV_DIR_))//'"' )
    CASE (ERRFLAG_UNABLE_TO_CREATE_DUMP_DIR_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the dump directory name' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'base dirname: "'//TRIM(ADJUSTL(THIS%OUT_DIR_))//'"' )
    CASE (ERRFLAG_CHANGE_PERMISSION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to change the permission of the parameter file' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'filename: "'//TRIM(ADJUSTL(THIS%PAR_FNAME_))//'"' )
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

  ! Exit point on error
  RETURN

END FUNCTION DUMP_SETUP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> As a `DUMP` output manager, this routine is intentionally left empty,
!> it acts jus as a logger
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_WRITE_ATM_DP'
PP_THREAD_SAFE FUNCTION DUMP_WRITE_ATM_DP( THIS, YDMSG, VALUES_DP, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,    ONLY: HOOKS_T
  USE :: PROFILE_MOD,  ONLY: PROFILE_MESSAGE
  USE :: IFS_MSG_MOD,  ONLY: OM_ATM_MSG_T
  USE :: IFS_MSG_MOD,  ONLY: MSG_PRINT_ATM
  USE :: IFS_MSG_MOD,  ONLY: MSG_WRITE_ATM
  USE :: LOG_INFO_MOD, ONLY: LOG_CURR_TIME
  USE :: IFS_VAL_MOD,  ONLY: VAL_WRITE_DP
  USE :: IFS_TOC_MOD,  ONLY: TOC_WRITE_ATM
  USE :: ENUMERATORS_MOD, ONLY: VALUES_DP_E

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_ATM_MSG_T),                   INTENT(IN)    :: YDMSG
  REAL(KIND=JPRD_K), DIMENSION(:),      INTENT(IN)    :: VALUES_DP
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  INTEGER(KIND=INT64) :: MSGADDR
  INTEGER(KIND=INT64) :: VALADDR

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_MSG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_MSG_ATM=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_VAL_ATM=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_TOC_ATM=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_MSG=6_JPIB_K


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

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    PP_TRYCALL(ERRFLAG_PROFILE_MSG) PROFILE_MESSAGE( THIS%PROFILE_DATA_, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_, HOOKS )
  ENDIF

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE ATMOSPHERE MESSAGE USING DOUBLE PRECISION VALUES', HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_MSG) MSG_PRINT_ATM( YDMSG, THIS%LOG_UNIT_, HOOKS )
  ENDIF

  ! Write message
  PP_TRYCALL(ERRFLAG_WRITE_MSG_ATM) MSG_WRITE_ATM( THIS%MSG_UNIT_, YDMSG, MSGADDR, HOOKS )

  ! Write values
  IF ( THIS%DUMP_VALUES_ ) THEN
    PP_TRYCALL(ERRFLAG_WRITE_VAL_ATM) VAL_WRITE_DP( THIS%VAL_UNIT_, VALUES_DP, VALADDR, HOOKS )
  ELSE
    ! Negative address means no value saved
    VALADDR = -1_INT64
  ENDIF

  ! Update toc
  LO = LBOUND(VALUES_DP,1)
  HI = UBOUND(VALUES_DP,1)
  PP_TRYCALL(ERRFLAG_WRITE_TOC_ATM) TOC_WRITE_ATM( &
&           THIS%TOC_UNIT_,           &
&           YDMSG%PARAM_ID_,          &
&           YDMSG%IUID_,              &
&           YDMSG%ISTEP_,             &
&           THIS%TOPOLOGY_%MYPROC_IO, &
&           YDMSG%IREPRES_,           &
&           YDMSG%IPREF_,             &
&           MSGADDR,                  &
&           VALUES_DP_E,              &
&           LO,                       &
&           HI,                       &
&           VALADDR,                  &
&           THIS%CNT_,                &
&           THIS%TOC_WRITPOS_,        &
&           THIS%TOC_COUNTER_,        &
&           HOOKS                     &
&  )

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
    CASE (ERRFLAG_UNABLE_TO_LOG_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the message' )
    CASE (ERRFLAG_WRITE_MSG_ATM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the message' )
    CASE (ERRFLAG_WRITE_VAL_ATM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the values' )
    CASE (ERRFLAG_WRITE_TOC_ATM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the TOC' )
    CASE (ERRFLAG_PROFILE_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to profile the message' )
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

  ! Exit point on error
  RETURN

END FUNCTION DUMP_WRITE_ATM_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> As a `DUMP` output manager, this routine is intentionally left empty,
!> it acts jus as a logger
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_WRITE_ATM_SP'
PP_THREAD_SAFE FUNCTION DUMP_WRITE_ATM_SP( THIS, YDMSG, VALUES_SP, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRM_K
  USE :: HOOKS_MOD,    ONLY: HOOKS_T
  USE :: PROFILE_MOD,  ONLY: PROFILE_MESSAGE
  USE :: IFS_MSG_MOD,  ONLY: OM_ATM_MSG_T
  USE :: IFS_MSG_MOD,  ONLY: MSG_PRINT_ATM
  USE :: IFS_MSG_MOD,  ONLY: MSG_WRITE_ATM
  USE :: LOG_INFO_MOD, ONLY: LOG_CURR_TIME
  USE :: IFS_VAL_MOD,  ONLY: VAL_WRITE_SP
  USE :: IFS_TOC_MOD,  ONLY: TOC_WRITE_ATM
  USE :: ENUMERATORS_MOD, ONLY: VALUES_SP_E

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_ATM_MSG_T),                   INTENT(IN)    :: YDMSG
  REAL(KIND=JPRM_K), DIMENSION(:),      INTENT(IN)    :: VALUES_SP
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  INTEGER(KIND=INT64) :: MSGADDR
  INTEGER(KIND=INT64) :: VALADDR

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_MSG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_MSG_ATM=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_VAL_ATM=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_TOC_ATM=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_MSG=6_JPIB_K

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

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    PP_TRYCALL(ERRFLAG_PROFILE_MSG) PROFILE_MESSAGE( THIS%PROFILE_DATA_, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_, HOOKS )
  ENDIF

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE ATMOSPHERE MESSAGE USING SINGLE PRECISION VALUES', HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_MSG) MSG_PRINT_ATM( YDMSG, THIS%LOG_UNIT_, HOOKS )
  ENDIF

  ! Write message
  PP_TRYCALL(ERRFLAG_WRITE_MSG_ATM) MSG_WRITE_ATM( THIS%MSG_UNIT_, YDMSG, MSGADDR, HOOKS )

  ! Write values
  IF ( THIS%DUMP_VALUES_ ) THEN
    PP_TRYCALL(ERRFLAG_WRITE_VAL_ATM) VAL_WRITE_SP( THIS%VAL_UNIT_, VALUES_SP, VALADDR, HOOKS )
  ELSE
    ! Negative address means no value saved
    VALADDR = -1_INT64
  ENDIF

  ! Update toc
  LO = LBOUND(VALUES_SP,1)
  HI = UBOUND(VALUES_SP,1)
  PP_TRYCALL(ERRFLAG_WRITE_TOC_ATM) TOC_WRITE_ATM( &
&           THIS%TOC_UNIT_,           &
&           YDMSG%PARAM_ID_,          &
&           YDMSG%IUID_,              &
&           YDMSG%ISTEP_,             &
&           THIS%TOPOLOGY_%MYPROC_IO, &
&           YDMSG%IREPRES_,           &
&           YDMSG%IPREF_,             &
&           MSGADDR,                  &
&           VALUES_SP_E,              &
&           LO,                       &
&           HI,                       &
&           VALADDR,                  &
&           THIS%CNT_,                &
&           THIS%TOC_WRITPOS_,        &
&           THIS%TOC_COUNTER_,        &
&           HOOKS                     &
&  )

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

    CASE (ERRFLAG_UNABLE_TO_LOG_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the message' )
    CASE (ERRFLAG_WRITE_MSG_ATM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the message' )
    CASE (ERRFLAG_WRITE_VAL_ATM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the values' )
    CASE (ERRFLAG_WRITE_TOC_ATM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the TOC' )
    CASE (ERRFLAG_PROFILE_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to profile the message' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME(  'Unhandled error' )
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

END FUNCTION DUMP_WRITE_ATM_SP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> As a `DUMP` output manager, this routine is intentionally left empty,
!> it acts jus as a logger
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_WRITE_WAM_DP'
PP_THREAD_SAFE FUNCTION DUMP_WRITE_WAM_DP( THIS, YDMSG, VALUES_DP, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,    ONLY: HOOKS_T
  USE :: PROFILE_MOD,  ONLY: PROFILE_MESSAGE
  USE :: IFS_MSG_MOD,  ONLY: OM_WAM_MSG_T
  USE :: IFS_MSG_MOD,  ONLY: MSG_PRINT_WAM
  USE :: IFS_MSG_MOD,  ONLY: MSG_WRITE_WAM
  USE :: LOG_INFO_MOD, ONLY: LOG_CURR_TIME
  USE :: IFS_VAL_MOD,  ONLY: VAL_WRITE_DP
  USE :: IFS_TOC_MOD,  ONLY: TOC_WRITE_WAM
  USE :: ENUMERATORS_MOD, ONLY: VALUES_DP_E

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_WAM_MSG_T),                   INTENT(IN)    :: YDMSG
  REAL(KIND=JPRD_K), DIMENSION(:),      INTENT(IN)    :: VALUES_DP
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  INTEGER(KIND=INT64) :: MSGADDR
  INTEGER(KIND=INT64) :: VALADDR

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_MSG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_MSG_WAM=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_VAL_WAM=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_TOC_WAM=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_MSG=6_JPIB_K

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

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    PP_TRYCALL(ERRFLAG_PROFILE_MSG) PROFILE_MESSAGE( THIS%PROFILE_DATA_, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_, HOOKS )
  ENDIF

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE WAVE MESSAGE USING DOUBLE PRECISION VALUES', HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_MSG) MSG_PRINT_WAM( YDMSG, THIS%LOG_UNIT_, HOOKS )
  ENDIF

  ! Write message
  PP_TRYCALL(ERRFLAG_WRITE_MSG_WAM) MSG_WRITE_WAM( THIS%MSG_UNIT_, YDMSG, MSGADDR, HOOKS )

  ! Write values
  IF ( THIS%DUMP_VALUES_ ) THEN
    PP_TRYCALL(ERRFLAG_WRITE_VAL_WAM) VAL_WRITE_DP( THIS%VAL_UNIT_, VALUES_DP, VALADDR, HOOKS )
  ELSE
    ! Negative address means no value saved
    VALADDR = -1_INT64
  ENDIF

  ! Update toc
  LO = LBOUND(VALUES_DP,1)
  HI = UBOUND(VALUES_DP,1)
  PP_TRYCALL(ERRFLAG_WRITE_TOC_WAM) TOC_WRITE_WAM( &
&           THIS%TOC_UNIT_,           &
&           YDMSG%PARAM_ID_,          &
&           YDMSG%IUID_,              &
&           YDMSG%ISTEP_,             &
&           THIS%TOPOLOGY_%MYPROC_IO, &
&           YDMSG%IREPRES_,           &
&           YDMSG%IPREF_,             &
&           MSGADDR,                  &
&           VALUES_DP_E,              &
&           LO,                       &
&           HI,                       &
&           VALADDR,                  &
&           THIS%CNT_,                &
&           THIS%TOC_WRITPOS_,        &
&           THIS%TOC_COUNTER_,        &
&           HOOKS                     &
&  )

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
    CASE (ERRFLAG_UNABLE_TO_LOG_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the message' )
    CASE (ERRFLAG_WRITE_MSG_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the message' )
    CASE (ERRFLAG_WRITE_VAL_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the values' )
    CASE (ERRFLAG_WRITE_TOC_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the TOC' )
    CASE (ERRFLAG_PROFILE_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to profile the message' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME(  'Unhandled error' )
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

END FUNCTION DUMP_WRITE_WAM_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE






!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> As a `DUMP` output manager, this routine is intentionally left empty,
!> it acts jus as a logger
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_WRITE_WAM_SP'
PP_THREAD_SAFE FUNCTION DUMP_WRITE_WAM_SP( THIS, YDMSG, VALUES_SP, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRM_K
  USE :: HOOKS_MOD,    ONLY: HOOKS_T
  USE :: PROFILE_MOD,  ONLY: PROFILE_MESSAGE
  USE :: IFS_MSG_MOD,  ONLY: OM_WAM_MSG_T
  USE :: IFS_MSG_MOD,  ONLY: MSG_PRINT_WAM
  USE :: IFS_MSG_MOD,  ONLY: MSG_WRITE_WAM
  USE :: LOG_INFO_MOD, ONLY: LOG_CURR_TIME
  USE :: IFS_VAL_MOD,  ONLY: VAL_WRITE_SP
  USE :: IFS_TOC_MOD,  ONLY: TOC_WRITE_WAM
  USE :: ENUMERATORS_MOD, ONLY: VALUES_SP_E

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_WAM_MSG_T),                   INTENT(IN)    :: YDMSG
  REAL(KIND=JPRM_K), DIMENSION(:),      INTENT(IN)    :: VALUES_SP
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  INTEGER(KIND=INT64) :: MSGADDR
  INTEGER(KIND=INT64) :: VALADDR

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_MSG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_MSG_WAM=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_VAL_WAM=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_TOC_WAM=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_MSG=6_JPIB_K

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

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    PP_TRYCALL(ERRFLAG_PROFILE_MSG) PROFILE_MESSAGE( THIS%PROFILE_DATA_, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_, HOOKS )
  ENDIF

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE WAVE MESSAGE USING SINGLE PRECISION VALUES', HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_MSG) MSG_PRINT_WAM( YDMSG, THIS%LOG_UNIT_, HOOKS )
  ENDIF

  ! Write message
  PP_TRYCALL(ERRFLAG_WRITE_MSG_WAM) MSG_WRITE_WAM( THIS%MSG_UNIT_, YDMSG, MSGADDR, HOOKS )

  ! Write values
  IF ( THIS%DUMP_VALUES_ ) THEN
    PP_TRYCALL(ERRFLAG_WRITE_VAL_WAM) VAL_WRITE_SP( THIS%VAL_UNIT_, VALUES_SP, VALADDR, HOOKS )
  ELSE
    ! Negative address means no value saved
    VALADDR = -1_INT64
  ENDIF

  ! Update toc
  LO = LBOUND(VALUES_SP,1)
  HI = UBOUND(VALUES_SP,1)
  PP_TRYCALL(ERRFLAG_WRITE_TOC_WAM) TOC_WRITE_WAM( &
&           THIS%TOC_UNIT_,           &
&           YDMSG%PARAM_ID_,          &
&           YDMSG%IUID_,              &
&           YDMSG%ISTEP_,             &
&           THIS%TOPOLOGY_%MYPROC_IO, &
&           YDMSG%IREPRES_,           &
&           YDMSG%IPREF_,             &
&           MSGADDR,                  &
&           VALUES_SP_E,              &
&           LO,                       &
&           HI,                       &
&           VALADDR,                  &
&           THIS%CNT_,                &
&           THIS%TOC_WRITPOS_,        &
&           THIS%TOC_COUNTER_,        &
&           HOOKS                     &
&  )

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
    CASE (ERRFLAG_UNABLE_TO_LOG_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the message' )
    CASE (ERRFLAG_WRITE_MSG_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the message' )
    CASE (ERRFLAG_WRITE_VAL_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the values' )
    CASE (ERRFLAG_WRITE_TOC_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the TOC' )
    CASE (ERRFLAG_PROFILE_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to profile the message' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME(  'Unhandled error' )
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

END FUNCTION DUMP_WRITE_WAM_SP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




!>
!> @brief Notify to the sinks the beginning of a new step started
!>
!> As a `DUMP` output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!> @param [in]    kstep Step at which teh function has been called
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_FLUSH_STEP'
PP_THREAD_SAFE FUNCTION DUMP_FLUSH_STEP( THIS, KSTEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: LOG_INFO_MOD,  ONLY: LOG_CURR_TIME
  USE :: LOG_UTILS_MOD, ONLY: TO_STRING
  USE :: LOG_UTILS_MOD, ONLY: MAX_STR_LEN
  USE :: HOOKS_MOD,     ONLY: HOOKS_T
  USE :: PROFILE_MOD,   ONLY: PROFILE_FLUSH
  USE :: IFS_MSG_MOD, ONLY: MSG_CLOSE
  USE :: IFS_MSG_MOD, ONLY: MSG_CREATE_NAME
  USE :: IFS_MSG_MOD, ONLY: MSG_WOPEN
  USE :: IFS_VAL_MOD, ONLY: VAL_CLOSE
  USE :: IFS_VAL_MOD, ONLY: VAL_CREATE_NAME
  USE :: IFS_VAL_MOD, ONLY: VAL_WOPEN
  USE :: IFS_TOC_MOD, ONLY: TOC_WRITE_FLUSH_STEP
  USE :: GENERAL_UTILS_MOD, ONLY: FILE_SET_PERMISSION

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: KSTEP
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=MAX_STR_LEN) :: CLTMP

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_STEP=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_CURR_TIME=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MSG_CLOSE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MSG_CREATE_NAME=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MSG_WOPEN=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VAL_CLOSE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VAL_CREATE_NAME=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VAL_WOPEN=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_FLUSH_STEP=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_FLUSH=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHANGE_PERMISSION=11_JPIB_K

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

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    PP_TRYCALL(ERRFLAG_PROFILE_FLUSH) PROFILE_FLUSH( THIS%PROFILE_DATA_, KSTEP, HOOKS )
  ENDIF

  ! If needed log step
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_STEP) TO_STRING( KSTEP, CLTMP, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_CURR_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH STEP: '//TRIM(ADJUSTL(CLTMP)), HOOKS )
  ENDIF

  ! Messages
  THIS%CNT_ = THIS%CNT_ + 1
  PP_TRYCALL(ERRFLAG_MSG_CLOSE) MSG_CLOSE( THIS%MSG_UNIT_, HOOKS )
  PP_TRYCALL(ERRFLAG_CHANGE_PERMISSION) FILE_SET_PERMISSION( THIS%MSG_FNAME_, 'rw-rw-rw-', HOOKS )
  THIS%MSG_FNAME_ = REPEAT(' ',1024)
  PP_TRYCALL(ERRFLAG_MSG_CREATE_NAME) MSG_CREATE_NAME( THIS%IO_SERV_DIR_, THIS%CNT_, THIS%TOPOLOGY_%MYPROC_IO, THIS%MSG_FNAME_, HOOKS )
  PP_TRYCALL(ERRFLAG_MSG_WOPEN) MSG_WOPEN( TRIM(THIS%MSG_FNAME_), THIS%MSG_UNIT_, HOOKS )


  ! Values
  IF ( THIS%DUMP_VALUES_ ) THEN
    PP_TRYCALL(ERRFLAG_VAL_CLOSE)  VAL_CLOSE( THIS%VAL_UNIT_, HOOKS )
    PP_TRYCALL(ERRFLAG_CHANGE_PERMISSION) FILE_SET_PERMISSION( THIS%VAL_FNAME_, 'rw-rw-rw-', HOOKS )
    THIS%VAL_FNAME_ = REPEAT(' ',1024)
    PP_TRYCALL(ERRFLAG_VAL_CREATE_NAME)  VAL_CREATE_NAME( THIS%IO_SERV_DIR_, THIS%CNT_, THIS%TOPOLOGY_%MYPROC_IO, THIS%VAL_FNAME_, HOOKS )
    PP_TRYCALL(ERRFLAG_VAL_WOPEN)  VAL_WOPEN( TRIM(THIS%VAL_FNAME_), THIS%VAL_UNIT_, HOOKS )
  ENDIF

  ! Update toc
  PP_TRYCALL(ERRFLAG_TOC_FLUSH_STEP)  TOC_WRITE_FLUSH_STEP( THIS%TOC_UNIT_, KSTEP, THIS%TOC_WRITPOS_, THIS%TOC_COUNTER_, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_WRITE_STEP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the step number' )
    CASE (ERRFLAG_UNABLE_TO_LOG_CURR_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_MSG_CLOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close the message file' )
    CASE (ERRFLAG_MSG_CREATE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the message file name' )
    CASE (ERRFLAG_MSG_WOPEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the message file' )
    CASE (ERRFLAG_VAL_CLOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close the values file' )
    CASE (ERRFLAG_VAL_CREATE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the values file name' )
    CASE (ERRFLAG_VAL_WOPEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the values file' )
    CASE (ERRFLAG_TOC_FLUSH_STEP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the TOC' )
    CASE (ERRFLAG_PROFILE_FLUSH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to profile the flush' )
    CASE (ERRFLAG_CHANGE_PERMISSION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to change the permission of the file' )
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

END FUNCTION DUMP_FLUSH_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Notify to the sinks the the last step
!>
!> As a `DUMP` output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!> @param [in]    kstep Step at which teh function has been called
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_FLUSH_LAST_STEP'
PP_THREAD_SAFE FUNCTION DUMP_FLUSH_LAST_STEP( THIS, KSTEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: LOG_INFO_MOD,  ONLY: LOG_CURR_TIME
  USE :: LOG_UTILS_MOD, ONLY: TO_STRING
  USE :: LOG_UTILS_MOD, ONLY: MAX_STR_LEN
  USE :: HOOKS_MOD,     ONLY: HOOKS_T
  USE :: PROFILE_MOD,   ONLY: PROFILE_FLUSH_LAST_STEP
  USE :: IFS_MSG_MOD, ONLY: MSG_CLOSE
  USE :: IFS_MSG_MOD, ONLY: MSG_CREATE_NAME
  USE :: IFS_MSG_MOD, ONLY: MSG_WOPEN
  USE :: IFS_VAL_MOD, ONLY: VAL_CLOSE
  USE :: IFS_VAL_MOD, ONLY: VAL_CREATE_NAME
  USE :: IFS_VAL_MOD, ONLY: VAL_WOPEN
  USE :: IFS_TOC_MOD, ONLY: TOC_WRITE_FLUSH_LAST_STEP
  USE :: GENERAL_UTILS_MOD, ONLY: FILE_SET_PERMISSION

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: KSTEP
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=MAX_STR_LEN) :: CLTMP

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_STEP=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_CURR_TIME=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MSG_CLOSE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MSG_CREATE_NAME=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MSG_WOPEN=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VAL_CLOSE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VAL_CREATE_NAME=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VAL_WOPEN=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_FLUSH_STEP=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_FLUSH=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHANGE_PERMISSION=11_JPIB_K

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

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    PP_TRYCALL(ERRFLAG_PROFILE_FLUSH) PROFILE_FLUSH_LAST_STEP( THIS%PROFILE_DATA_, KSTEP, HOOKS )
  ENDIF

  ! If needed log step
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_STEP) TO_STRING( KSTEP, CLTMP, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_CURR_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH LAST STEP: '//TRIM(ADJUSTL(CLTMP)), HOOKS )
  ENDIF

  ! Messages
  THIS%CNT_ = THIS%CNT_ + 1
  PP_TRYCALL(ERRFLAG_MSG_CLOSE) MSG_CLOSE( THIS%MSG_UNIT_, HOOKS )
  PP_TRYCALL(ERRFLAG_CHANGE_PERMISSION) FILE_SET_PERMISSION( THIS%MSG_FNAME_, 'rw-rw-rw-', HOOKS )
  THIS%MSG_FNAME_ = REPEAT(' ',1024)
  PP_TRYCALL(ERRFLAG_MSG_CREATE_NAME) MSG_CREATE_NAME( THIS%IO_SERV_DIR_, THIS%CNT_, THIS%TOPOLOGY_%MYPROC_IO, THIS%MSG_FNAME_, HOOKS )
  PP_TRYCALL(ERRFLAG_MSG_WOPEN) MSG_WOPEN( TRIM(THIS%MSG_FNAME_), THIS%MSG_UNIT_, HOOKS )


  ! Values
  IF ( THIS%DUMP_VALUES_ ) THEN
    PP_TRYCALL(ERRFLAG_VAL_CLOSE)  VAL_CLOSE( THIS%VAL_UNIT_, HOOKS )
    PP_TRYCALL(ERRFLAG_CHANGE_PERMISSION) FILE_SET_PERMISSION( THIS%VAL_FNAME_, 'rw-rw-rw-', HOOKS )
    THIS%VAL_FNAME_ = REPEAT(' ',1024)
    PP_TRYCALL(ERRFLAG_VAL_CREATE_NAME)  VAL_CREATE_NAME( THIS%IO_SERV_DIR_, THIS%CNT_, THIS%TOPOLOGY_%MYPROC_IO,  THIS%VAL_FNAME_, HOOKS )
    PP_TRYCALL(ERRFLAG_VAL_WOPEN)  VAL_WOPEN( TRIM( THIS%VAL_FNAME_), THIS%VAL_UNIT_, HOOKS )
  ENDIF

  ! Update toc
  PP_TRYCALL(ERRFLAG_TOC_FLUSH_STEP)  TOC_WRITE_FLUSH_LAST_STEP( THIS%TOC_UNIT_, KSTEP, THIS%TOC_WRITPOS_, THIS%TOC_COUNTER_, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_WRITE_STEP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the step number' )
    CASE (ERRFLAG_UNABLE_TO_LOG_CURR_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_MSG_CLOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close the message file' )
    CASE (ERRFLAG_MSG_CREATE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the message file name' )
    CASE (ERRFLAG_MSG_WOPEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the message file' )
    CASE (ERRFLAG_VAL_CLOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close the values file' )
    CASE (ERRFLAG_VAL_CREATE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the values file name' )
    CASE (ERRFLAG_VAL_WOPEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the values file' )
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

END FUNCTION DUMP_FLUSH_LAST_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Notify to the sinks the beginning of a new step and the
!>        request to dump a checkpoint
!>
!> As a `DUMP` output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!> @param [in]    kstep Step at which teh function has been called
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_FLUSH_STEP_AND_TRIGGER_RESTART'
PP_THREAD_SAFE FUNCTION DUMP_FLUSH_STEP_AND_TRIGGER_RESTART( THIS, KSTEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: LOG_INFO_MOD,  ONLY: LOG_CURR_TIME
  USE :: LOG_UTILS_MOD, ONLY: TO_STRING
  USE :: LOG_UTILS_MOD, ONLY: MAX_STR_LEN
  USE :: HOOKS_MOD,     ONLY: HOOKS_T
  USE :: PROFILE_MOD,   ONLY: PROFILE_FLUSH_AND_RESTART
  USE :: IFS_MSG_MOD, ONLY: MSG_CLOSE
  USE :: IFS_MSG_MOD, ONLY: MSG_CREATE_NAME
  USE :: IFS_MSG_MOD, ONLY: MSG_WOPEN
  USE :: IFS_VAL_MOD, ONLY: VAL_CLOSE
  USE :: IFS_VAL_MOD, ONLY: VAL_CREATE_NAME
  USE :: IFS_VAL_MOD, ONLY: VAL_WOPEN
  USE :: IFS_TOC_MOD, ONLY: TOC_WRITE_FLUSH_STEP_AND_RESTART
  USE :: GENERAL_UTILS_MOD, ONLY: FILE_SET_PERMISSION

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: KSTEP
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=MAX_STR_LEN) :: CLTMP

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_STEP=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_CURR_TIME=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MSG_CLOSE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MSG_CREATE_NAME=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MSG_WOPEN=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VAL_CLOSE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VAL_CREATE_NAME=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VAL_WOPEN=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_FLUSH_STEP=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_FLUSH=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHANGE_PERMISSION=11_JPIB_K


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

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    PP_TRYCALL(ERRFLAG_PROFILE_FLUSH) PROFILE_FLUSH_AND_RESTART( THIS%PROFILE_DATA_, KSTEP, HOOKS )
  ENDIF

  ! If needed log step
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_STEP) TO_STRING( KSTEP, CLTMP, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_CURR_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH STEP AND RESTART: '//TRIM(ADJUSTL(CLTMP)), HOOKS )
  ENDIF

  ! Messages
  THIS%CNT_ = THIS%CNT_ + 1
  PP_TRYCALL(ERRFLAG_MSG_CLOSE) MSG_CLOSE( THIS%MSG_UNIT_, HOOKS )
  PP_TRYCALL(ERRFLAG_CHANGE_PERMISSION) FILE_SET_PERMISSION( THIS%MSG_FNAME_, 'rw-rw-rw-', HOOKS )
  THIS%MSG_FNAME_ = REPEAT(' ',1024)
  PP_TRYCALL(ERRFLAG_MSG_CREATE_NAME) MSG_CREATE_NAME( THIS%IO_SERV_DIR_, THIS%CNT_, THIS%TOPOLOGY_%MYPROC_IO, THIS%MSG_FNAME_, HOOKS )
  PP_TRYCALL(ERRFLAG_MSG_WOPEN) MSG_WOPEN( TRIM(THIS%MSG_FNAME_), THIS%MSG_UNIT_, HOOKS )


  ! Values
  IF ( THIS%DUMP_VALUES_ ) THEN
    PP_TRYCALL(ERRFLAG_VAL_CLOSE)  VAL_CLOSE( THIS%VAL_UNIT_, HOOKS )
    PP_TRYCALL(ERRFLAG_CHANGE_PERMISSION) FILE_SET_PERMISSION( THIS%VAL_FNAME_, 'rw-rw-rw-', HOOKS )
    THIS%VAL_FNAME_ = REPEAT(' ',1024)
    PP_TRYCALL(ERRFLAG_VAL_CREATE_NAME)  VAL_CREATE_NAME( THIS%IO_SERV_DIR_, THIS%CNT_, THIS%TOPOLOGY_%MYPROC_IO, THIS%VAL_FNAME_, HOOKS )
    PP_TRYCALL(ERRFLAG_VAL_WOPEN)  VAL_WOPEN( TRIM(THIS%VAL_FNAME_), THIS%VAL_UNIT_, HOOKS )
  ENDIF

  ! Update toc
  PP_TRYCALL(ERRFLAG_TOC_FLUSH_STEP)  TOC_WRITE_FLUSH_STEP_AND_RESTART( THIS%TOC_UNIT_, KSTEP, THIS%TOC_WRITPOS_, THIS%TOC_COUNTER_, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_WRITE_STEP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the step number' )
    CASE (ERRFLAG_UNABLE_TO_LOG_CURR_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_MSG_CLOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close the message file' )
    CASE (ERRFLAG_MSG_CREATE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the message file name' )
    CASE (ERRFLAG_MSG_WOPEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the message file' )
    CASE (ERRFLAG_VAL_CLOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close the values file' )
    CASE (ERRFLAG_VAL_CREATE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the values file name' )
    CASE (ERRFLAG_VAL_WOPEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the values file' )
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

END FUNCTION DUMP_FLUSH_STEP_AND_TRIGGER_RESTART
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Finalisation of the output manager
!>
!> As a `DUMP` output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DUMP_FINALISE'
PP_THREAD_SAFE FUNCTION DUMP_FINALISE( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: LOG_INFO_MOD,      ONLY: LOG_CURR_TIME
  USE :: LOG_UTILS_MOD,     ONLY: TO_STRING
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: PROFILE_MOD,       ONLY: PROFILE_END_SIMULATION
  USE :: IFS_TOC_MOD,       ONLY: TOC_WRITE_FLUSH_END_OF_SIMULATION
  USE :: IFS_TOC_MOD,       ONLY: TOC_CLOSE
  USE :: GENERAL_UTILS_MOD, ONLY: FILE_SET_PERMISSION

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CLOSE_LOG_FILE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_FLUSH_STEP=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PROFILE_END_OF_SIMULATION=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHANGE_PERMISSION=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOC_CLOSE=6_JPIB_K


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

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_PROFILE_END_OF_SIMULATION) PROFILE_END_SIMULATION( THIS%PROFILE_DATA_, HOOKS )
  ENDIF

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'FINALISATION', HOOKS )
    CLOSE( THIS%LOG_UNIT_, IOSTAT=STAT )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_CLOSE_LOG_FILE )
    PP_TRYCALL(ERRFLAG_CHANGE_PERMISSION) FILE_SET_PERMISSION( THIS%LOG_FNAME_, 'rw-rw-rw-', HOOKS )
  ENDIF

  ! Update toc
  PP_TRYCALL(ERRFLAG_TOC_FLUSH_STEP)  TOC_WRITE_FLUSH_END_OF_SIMULATION( THIS%TOC_UNIT_, THIS%TOC_WRITPOS_, THIS%TOC_COUNTER_, HOOKS )

  ! Close toc
  PP_TRYCALL(ERRFLAG_TOC_CLOSE) TOC_CLOSE( THIS%TOC_UNIT_, HOOKS )
  PP_TRYCALL(ERRFLAG_CHANGE_PERMISSION) FILE_SET_PERMISSION( THIS%TOC_FNAME_, 'rw-rw-rw-', HOOKS )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_LOG_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_TO_CLOSE_LOG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close log file' )
    CASE (ERRFLAG_TOC_FLUSH_STEP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the TOC' )
    CASE (ERRFLAG_UNABLE_TO_PROFILE_END_OF_SIMULATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to profile the end of simulation' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME(  'Unhandled error' )
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

END FUNCTION DUMP_FINALISE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE DUMP_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
