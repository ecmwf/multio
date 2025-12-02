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
#define PP_FILE_NAME 'noop_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'NOOP_MOD'
MODULE NOOP_MOD

  ! Symbols imported from other modules within the project.
  USE :: OUTUPUT_MANAGER_BASE_MOD, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,          ONLY: UNDEF_PARAM_E
  USE :: IFS_PAR_MOD,              ONLY: PROC_TOPO_T
  USE :: IFS_PAR_MOD,              ONLY: MODEL_PAR_T

IMPLICIT NONE

! Default visibility
PRIVATE

!> Output manager name
CHARACTER(LEN=*), PARAMETER :: NOOP_OMNAME='NO-IO-INFO-LOG'

!>
!> @brief Definition of the `NOOP_OUTPUT_MANAGER_T` derived type.
!>
!> The `NOOP_OUTPUT_MANAGER_T` type is a derived type that extends the
!> functionality of the `OUTPUT_MANAGER_BASE_A` abstract interface to be
!> used to demonstrate behavior without I/O when addressing complaints
!> or testing scenarios.
!>
!> @see OUTPUT_MANAGER_BASE_A
!>
TYPE, EXTENDS(OUTPUT_MANAGER_BASE_A) :: NOOP_OUTPUT_MANAGER_T

  !> Default visibility
  PRIVATE

  !> Process ID
  INTEGER(KIND=JPIB_K) :: PID_=UNDEF_PARAM_E

  !> Hostname
  CHARACTER(LEN=256) :: HOSTNAME_=REPEAT(' ',256)

  !> Multiprocessor topology
  TYPE(PROC_TOPO_T), POINTER :: TOPOLOGY_ => NULL()

  !> Model parameters
  TYPE(MODEL_PAR_T), POINTER :: MODEL_PAR_ => NULL()

  !> Test track encoders, to be used for:
  LOGICAL :: VERBOSE_ = .FALSE.

  !> Unit used for logging purposes (if needed)
  INTEGER(KIND=JPIB_K) :: LOG_UNIT_ = -99

  !> File used for logging purposes (if needed)
  CHARACTER(LEN=1024)  :: LOG_FNAME_ = REPEAT(' ',1024)

CONTAINS

  !> @brief Setup of the output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: SETUP => NOOP_SETUP

  !> @brief Write fields for the output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_ATM_DP => NOOP_WRITE_ATM_DP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_ATM_SP => NOOP_WRITE_ATM_SP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_WAM_DP => NOOP_WRITE_WAM_DP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_WAM_SP => NOOP_WRITE_WAM_SP

  !> @brief Notify that a step is finished
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_STEP => NOOP_FLUSH_STEP

  !> @brief Notify that a step is finished and it was the last step
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_LAST_STEP => NOOP_FLUSH_LAST_STEP

  !> @brief Notify that a step is finished and it is necessary to dump a restart
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_STEP_AND_TRIGGER_RESTART => NOOP_FLUSH_STEP_AND_TRIGGER_RESTART

  !> @brief Finalise ancd cleanup teh output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FINALISE => NOOP_FINALISE


  !> @brief Read the configuration from YAML using fckit
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_CFG_FROM_YAML => NOOP_READ_CFG_FROM_YAML
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_VERBOSE_FROM_YAML => NOOP_READ_VERBOSE_FROM_YAML

  END TYPE

  ! Whitelist of public symbols
  PUBLIC :: NOOP_OUTPUT_MANAGER_T
  PUBLIC :: NOOP_OMNAME

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NOOP_READ_CFG_FROM_YAML'
PP_THREAD_SAFE FUNCTION NOOP_READ_CFG_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

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
  CLASS(NOOP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(YAML_CONFIGURATION_T) :: NOOP_CFG
  CHARACTER(LEN=LEN(NOOP_OMNAME)) :: NOOP_OMNAME_LC
  LOGICAL :: HAS_SUBKEY
  LOGICAL :: HAS_KEY

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_LOWER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HAS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_SUBCFG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_VERBOSE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTRY_CFG=5_JPIB_K

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
  NOOP_OMNAME_LC = REPEAT(' ',LEN(NOOP_OMNAME_LC))
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_LOWER) TOLOWER( NOOP_OMNAME, NOOP_OMNAME_LC, HOOKS )

  ! Get sub-configuration from input file
  PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, NOOP_OMNAME_LC, HAS_KEY, HOOKS )

  ! If has specific configuration then try to read it
  IF ( HAS_KEY ) THEN

    ! Get the specific sub-configuration
    PP_TRYCALL(ERRFLAG_GET_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, NOOP_OMNAME_LC, NOOP_CFG, HOOKS )

    ! Read the verbose flag
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VERBOSE) THIS%READ_VERBOSE_FROM_YAML( NOOP_CFG, HOOKS )

    ! Deallocate the dump-output-manager object
    PP_TRYCALL(ERRFLAG_UNABLE_TO_DESTRY_CFG) YAML_DELETE_CONFIGURATION( NOOP_CFG, HOOKS )

  ELSE

    ! Default value for the options
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
    CASE (ERRFLAG_UNABLE_TO_CONVERT_TO_LOWER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert the output manager name to lowercase' )
    CASE (ERRFLAG_HAS_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find the output manager name in the configuration file' )
    CASE (ERRFLAG_GET_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the sub-configuration for the output manager' )
    CASE (ERRFLAG_UNABLE_TO_READ_VERBOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the verbose flag' )
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

END FUNCTION NOOP_READ_CFG_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NOOP_READ_VERBOSE_FROM_YAML'
PP_THREAD_SAFE FUNCTION NOOP_READ_VERBOSE_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

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
  CLASS(NOOP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find the output manager name in the configuration file' )
    CASE (ERRFLAG_GET_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the sub-configuration for the output manager' )
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

END FUNCTION NOOP_READ_VERBOSE_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Initializes the output manager from an instance of the IO server.
!>
!> This procedure initializes the object using the information
!> contained in the instance of the IOServer. As a `NOOP`
!> output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!> @param [in]    PROCESSOR_TOPO Processor topology to be used in a multiprocessor run
!> @param [in]    MODEL_PARAMS   Model parameters that are frozen during the simulation
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NOOP_SETUP'
PP_THREAD_SAFE FUNCTION NOOP_SETUP( THIS, YAMLFNAME, PROCESSOR_TOPO, MODEL_PARAMS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  USE :: IFS_PAR_MOD, ONLY: PROC_TOPO_T
  USE :: IFS_PAR_MOD, ONLY: MODEL_PAR_T
  USE :: IFS_PAR_MOD, ONLY: PAR_PRINT

  USE :: SYSINFO_MOD, ONLY: GET_PID
  USE :: SYSINFO_MOD, ONLY: GET_HOSTNAME
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER

  USE :: LOG_INFO_MOD, ONLY: LOG_VERSION
  USE :: LOG_INFO_MOD, ONLY: LOG_CURR_TIME
  USE :: LOG_INFO_MOD, ONLY: LOG_SYSINFO

  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_NEW_CONFIGURATION_FROM_FILE
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(NOOP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: YAMLFNAME
  TYPE(PROC_TOPO_T), TARGET,    INTENT(IN)    :: PROCESSOR_TOPO
  TYPE(MODEL_PAR_T), TARGET,    INTENT(IN)    :: MODEL_PARAMS
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: FEXISTS
  INTEGER(KIND=JPIB_K) :: STAT
  TYPE(YAML_CONFIGURATION_T) :: MAIN_CFG
  CHARACTER(LEN=LEN(NOOP_OMNAME)) :: NOOP_OMNAME_LC

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



  ! Logging
  IF ( THIS%VERBOSE_ ) THEN

    ! Convert the kind of output manager to lowercase
    NOOP_OMNAME_LC = REPEAT(' ',LEN(NOOP_OMNAME_LC))
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_LOWER) TOLOWER( NOOP_OMNAME, NOOP_OMNAME_LC, HOOKS )

    ! Create log file name
    THIS%LOG_FNAME_ = REPEAT(' ',LEN(THIS%LOG_FNAME_))
    WRITE(THIS%LOG_FNAME_,'(A,I8.8,A)', IOSTAT=STAT) TRIM(ADJUSTL(NOOP_OMNAME_LC))//'-output-manager-', PROCESSOR_TOPO%MYPROC_IO, '.log'
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_CREATE_LOG_FILENAME )

    ! Open the log file
    OPEN( FILE=TRIM(THIS%LOG_FNAME_), NEWUNIT=THIS%LOG_UNIT_, ACTION='WRITE', STATUS='REPLACE', IOSTAT=STAT )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_OPEN_LOG_FILE )

    ! Print logging information to the log file
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_CURRTIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'SETUP OF THE NOOP OUTPUT MANAGER', HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_VERSION) LOG_VERSION( THIS%LOG_UNIT_, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_SYSINFO) LOG_SYSINFO( THIS%LOG_UNIT_, PROCESSOR_TOPO%NPROC_IO, PROCESSOR_TOPO%MYPROC_IO, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_PARAMETERS) PAR_PRINT( MODEL_PARAMS, THIS%LOG_UNIT_, HOOKS )
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

END FUNCTION NOOP_SETUP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> As a `NOOP` output manager, this routine is intentionally left empty,
!> it acts jus as a logger
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NOOP_WRITE_ATM_DP'
PP_THREAD_SAFE FUNCTION NOOP_WRITE_ATM_DP( THIS, YDMSG, VALUES_DP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  USE :: IFS_MSG_MOD,  ONLY: OM_ATM_MSG_T
  USE :: IFS_MSG_MOD,  ONLY: MSG_PRINT_ATM
  USE :: LOG_INFO_MOD, ONLY: LOG_CURR_TIME

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(NOOP_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_ATM_MSG_T),                   INTENT(IN)    :: YDMSG
  REAL(KIND=JPRD_K), DIMENSION(:),      INTENT(IN)    :: VALUES_DP
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_MSG=2_JPIB_K

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

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE ATMOSPHERE MESSAGE USING DOUBLE PRECISION VALUES', HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_MSG) MSG_PRINT_ATM( YDMSG, THIS%LOG_UNIT_, HOOKS )
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
    CASE (ERRFLAG_UNABLE_TO_LOG_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the message' )
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

END FUNCTION NOOP_WRITE_ATM_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> As a `NOOP` output manager, this routine is intentionally left empty,
!> it acts jus as a logger
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NOOP_WRITE_ATM_SP'
PP_THREAD_SAFE FUNCTION NOOP_WRITE_ATM_SP( THIS, YDMSG, VALUES_SP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  USE :: IFS_MSG_MOD,  ONLY: OM_ATM_MSG_T
  USE :: IFS_MSG_MOD,  ONLY: MSG_PRINT_ATM
  USE :: LOG_INFO_MOD, ONLY: LOG_CURR_TIME

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(NOOP_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_ATM_MSG_T),                   INTENT(IN)    :: YDMSG
  REAL(KIND=JPRM_K), DIMENSION(:),      INTENT(IN)    :: VALUES_SP
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_MSG=2_JPIB_K

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

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE ATMOSPHERE MESSAGE USING SINGLE PRECISION VALUES', HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_MSG) MSG_PRINT_ATM( YDMSG, THIS%LOG_UNIT_, HOOKS )
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
    CASE (ERRFLAG_UNABLE_TO_LOG_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the message' )
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

END FUNCTION NOOP_WRITE_ATM_SP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> As a `NOOP` output manager, this routine is intentionally left empty,
!> it acts jus as a logger
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NOOP_WRITE_WAM_DP'
PP_THREAD_SAFE FUNCTION NOOP_WRITE_WAM_DP( THIS, YDMSG, VALUES_DP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  USE :: IFS_MSG_MOD,  ONLY: OM_WAM_MSG_T
  USE :: IFS_MSG_MOD,  ONLY: MSG_PRINT_WAM
  USE :: LOG_INFO_MOD, ONLY: LOG_CURR_TIME

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(NOOP_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_WAM_MSG_T),                   INTENT(IN)    :: YDMSG
  REAL(KIND=JPRD_K), DIMENSION(:),      INTENT(IN)    :: VALUES_DP
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_MSG=2_JPIB_K

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

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE WAVE MESSAGE USING DOUBLE PRECISION VALUES', HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_MSG) MSG_PRINT_WAM( YDMSG, THIS%LOG_UNIT_, HOOKS )
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
    CASE (ERRFLAG_UNABLE_TO_LOG_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the message' )
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

END FUNCTION NOOP_WRITE_WAM_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE






!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> As a `NOOP` output manager, this routine is intentionally left empty,
!> it acts jus as a logger
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NOOP_WRITE_WAM_SP'
PP_THREAD_SAFE FUNCTION NOOP_WRITE_WAM_SP( THIS, YDMSG, VALUES_SP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  USE :: IFS_MSG_MOD,  ONLY: OM_WAM_MSG_T
  USE :: IFS_MSG_MOD,  ONLY: MSG_PRINT_WAM
  USE :: LOG_INFO_MOD, ONLY: LOG_CURR_TIME

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(NOOP_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_WAM_MSG_T),                   INTENT(IN)    :: YDMSG
  REAL(KIND=JPRM_K), DIMENSION(:),      INTENT(IN)    :: VALUES_SP
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_MSG=2_JPIB_K

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

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE WAVE MESSAGE USING SINGLE PRECISION VALUES', HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_MSG) MSG_PRINT_WAM( YDMSG, THIS%LOG_UNIT_, HOOKS )
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
    CASE (ERRFLAG_UNABLE_TO_LOG_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the message' )
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

END FUNCTION NOOP_WRITE_WAM_SP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




!>
!> @brief Notify to the sinks the beginning of a new step started
!>
!> As a `NOOP` output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!> @param [in]    kstep Step at which teh function has been called
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NOOP_FLUSH_STEP'
PP_THREAD_SAFE FUNCTION NOOP_FLUSH_STEP( THIS, KSTEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: LOG_INFO_MOD,      ONLY: LOG_CURR_TIME
  USE :: LOG_UTILS_MOD,     ONLY: TO_STRING
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(NOOP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: KSTEP
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=MAX_STR_LEN) :: CLTMP

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_STEP=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_CURR_TIME=2_JPIB_K

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

  ! If needed log step
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_STEP) TO_STRING( KSTEP, CLTMP, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_CURR_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH STEP: '//TRIM(ADJUSTL(CLTMP)), HOOKS )
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
    CASE (ERRFLAG_UNABLE_TO_WRITE_STEP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the step number' )
    CASE (ERRFLAG_UNABLE_TO_LOG_CURR_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
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

END FUNCTION NOOP_FLUSH_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Notify to the sinks the the last step
!>
!> As a `NOOP` output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!> @param [in]    kstep Step at which teh function has been called
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NOOP_FLUSH_LAST_STEP'
PP_THREAD_SAFE FUNCTION NOOP_FLUSH_LAST_STEP( THIS, KSTEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: LOG_INFO_MOD,      ONLY: LOG_CURR_TIME
  USE :: LOG_UTILS_MOD,     ONLY: TO_STRING
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(NOOP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: KSTEP
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=MAX_STR_LEN) :: CLTMP

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_STEP=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_CURR_TIME=2_JPIB_K

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

  ! If needed log step
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_STEP) TO_STRING( KSTEP, CLTMP, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_CURR_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH LAST STEP: '//TRIM(ADJUSTL(CLTMP)), HOOKS )
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
    CASE (ERRFLAG_UNABLE_TO_WRITE_STEP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the step number' )
    CASE (ERRFLAG_UNABLE_TO_LOG_CURR_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
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

END FUNCTION NOOP_FLUSH_LAST_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Notify to the sinks the beginning of a new step and the
!>        request to dump a checkpoint
!>
!> As a `NOOP` output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!> @param [in]    kstep Step at which teh function has been called
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NOOP_FLUSH_STEP_AND_TRIGGER_RESTART'
PP_THREAD_SAFE FUNCTION NOOP_FLUSH_STEP_AND_TRIGGER_RESTART( THIS, KSTEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: LOG_INFO_MOD,      ONLY: LOG_CURR_TIME
  USE :: LOG_UTILS_MOD,     ONLY: TO_STRING
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(NOOP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: KSTEP
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=MAX_STR_LEN) :: CLTMP

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_STEP=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_CURR_TIME=2_JPIB_K

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

  ! If needed log step
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_STEP) TO_STRING( KSTEP, CLTMP, HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_CURR_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH STEP AND RESTART: '//TRIM(ADJUSTL(CLTMP)), HOOKS )
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
    CASE (ERRFLAG_UNABLE_TO_WRITE_STEP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the step number' )
    CASE (ERRFLAG_UNABLE_TO_LOG_CURR_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
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

END FUNCTION NOOP_FLUSH_STEP_AND_TRIGGER_RESTART
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Finalisation of the output manager
!>
!> As a `NOOP` output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'NOOP_FINALISE'
PP_THREAD_SAFE FUNCTION NOOP_FINALISE( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: LOG_INFO_MOD,      ONLY: LOG_CURR_TIME
  USE :: LOG_UTILS_MOD,     ONLY: TO_STRING
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(NOOP_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CLOSE_LOG_FILE=2_JPIB_K

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

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'FINALISATION', HOOKS )
    CLOSE( THIS%LOG_UNIT_, IOSTAT=STAT )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_CLOSE_LOG_FILE )
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_LOG_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_TO_CLOSE_LOG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close log file' )
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

END FUNCTION NOOP_FINALISE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE NOOP_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
