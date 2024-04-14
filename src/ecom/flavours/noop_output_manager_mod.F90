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
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: OUTUPUT_MANAGER_BASE_MOD, ONLY: OUTPUT_MANAGER_BASE_A

IMPLICIT NONE

! Default visibility
PRIVATE

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

  !> Multiprocessor topology
  TYPE(PROC_TOPO_T), POINTER :: TOPOLOGY_ => NULL()

  !> Model parameters
  TYPE(MODEL_PAR_T), POINTER :: MODEL_PAR_ => NULL()

  !> Test grib info module and service (mainly to be used with valgrind to check for memory leacks)
  LOGICAL :: TEST_GRIB_INFO_ = .FALSE.

  !> Test track time functionlaity (mainly to be used with valgrind to check for memory leacks)
  LOGICAL :: TEST_TRACK_TIME_ = .FALSE.

  !> Test track encoders, to be used for:
  LOGICAL :: VERBOSE_ = .FALSE.

  !> Unit used for logging purposes (if needed)
  INTEGER(KIND=JPIB_K) :: LOG_UNIT_ = -99

  !> File used for logging purposes (if needed)
  CHARACTER(LEN=1024)  :: LOG_FNAME_ = REPEAT(' ',1024)

CONTAINS

  !> @brief Setup of the output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: SETUP => NOOP_SETUP

  !> @brief Read the configuration from YAML using fckit
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: READ_CFG_FROM_YAML => NOOP_READ_CFG_FROM_YAML

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

  END TYPE

  ! Whitelist of public symbols
  PUBLIC :: NOOP_OUTPUT_MANAGER_T

CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'NOOP_READ_CFG_FROM_YAML'
SUBROUTINE NOOP_READ_CFG_FROM_YAML( THIS, CFG )

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(NOOP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(FCKIT_CONFIGURATION),    INTENT(IN)    :: CFG

  ! Local variables
  TYPE(FCKIT_CONFIGURATION) :: NOOP_CFG
  LOGICAL :: LTMP

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Read the options from YAML
  IF ( CFG%GET( 'noop-output-manager', NOOP_CFG ) ) THEN

    ! Read from YAML file the flag to enable grib info testing
    IF ( NOOP_CFG%GET( 'test-grib-info', LTMP  ) ) THEN
      THIS%TEST_GRIB_INFO_ = LTMP
    ELSE
      THIS%TEST_GRIB_INFO_ = .FALSE.
    ENDIF

    ! Read from YAML file the flag to enable track time testing
    IF ( NOOP_CFG%GET( 'test-track-time', LTMP  ) ) THEN
      THIS%TEST_TRACK_TIME_ = LTMP
    ELSE
      THIS%TEST_TRACK_TIME_ = .FALSE.
    ENDIF

    ! Read from YAML file the flag to enable verbose execution
    IF ( NOOP_CFG%GET( 'verbose', LTMP  ) ) THEN
      THIS%VERBOSE_ = LTMP
    ELSE
      THIS%VERBOSE_ = .FALSE.
    ENDIF

    ! Deallocate the dump-output-manager object
    CALL NOOP_CFG%FINAL()

  ELSE

    ! Deallocate the dump-output-manager object
    CALL NOOP_CFG%FINAL()

    ! Default value for the options
    THIS%TEST_GRIB_INFO_  = .FALSE.
    THIS%TEST_TRACK_TIME_ = .FALSE.
    THIS%VERBOSE_         = .FALSE.

  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Allocatable character not allocated after reading operation' )
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

END SUBROUTINE NOOP_READ_CFG_FROM_YAML
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'NOOP_SETUP'
SUBROUTINE NOOP_SETUP( THIS, YAMLFNAME, PROCESSOR_TOPO, MODEL_PARAMS )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_CORE_MOD,          ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,          ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,          ONLY: OM_INIT_DEBUG_VARS
  USE :: PAR_UTILS_MOD,        ONLY: PAR_WRITE
  USE :: TRACK_TIME_MOD,       ONLY: SUTRAK_TIME
  USE :: GRIB_INFO_MOD,        ONLY: SUGRIB_INFO_YAML
  USE :: PAR_UTILS_MOD,        ONLY: PAR_PRINT
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_VERSION
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_SYSINFO
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_GETPID
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_GET_HOSTNAME

  ! Symbols imported from other libraries
  USE :: FCKIT_PATHNAME_MODULE,      ONLY: FCKIT_PATHNAME
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_YAMLCONFIGURATION
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

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

  ! Local variables
  CHARACTER(LEN=256) :: HOSTNAME
  TYPE(FCKIT_CONFIGURATION) :: CFG
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: PID
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialise topology
  THIS%TOPOLOGY_  => PROCESSOR_TOPO

  ! Initialise model parameters
  THIS%MODEL_PAR_ => MODEL_PARAMS

  ! Initialize debug variables
  PID = OM_GETPID()
  CALL OM_GET_HOSTNAME( HOSTNAME )
  CALL OM_INIT_DEBUG_VARS( HOSTNAME, PID, PROCESSOR_TOPO%MYPROC_IO, PROCESSOR_TOPO%NPROC_IO )

  ! Check if the file exsts
  INQUIRE( FILE=TRIM(YAMLFNAME), EXIST=EX )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 1 )

  ! Open the filename
  CFG = FCKIT_YAMLCONFIGURATION( FCKIT_PATHNAME( TRIM(YAMLFNAME) ) )

  ! Read the specific configuration for "DUMP" output manager
  CALL THIS%READ_CFG_FROM_YAML( CFG )

  ! Initialise grib info
  IF ( THIS%TEST_GRIB_INFO_ ) THEN
    PP_LOG_DEVELOP_STR( 'Initialise grib info functionalities...' )
    CALL SUGRIB_INFO_YAML( CFG, PROCESSOR_TOPO, MODEL_PARAMS )
  ENDIF

  ! Initialise time tracker
  IF ( THIS%TEST_TRACK_TIME_ ) THEN
    PP_LOG_DEVELOP_STR( 'Initialise track time functionalities...' )
    CALL SUTRAK_TIME( CFG, MODEL_PARAMS )
  ENDIF

  ! Destroy the fckit configuration object
  CALL CFG%FINAL()

  PP_LOG_DEVELOP_STR( 'Readed configuration file: '//TRIM(YAMLFNAME) )

  ! Logging
  IF ( THIS%VERBOSE_ ) THEN
    THIS%LOG_FNAME_ = REPEAT(' ',LEN(THIS%LOG_FNAME_))
    WRITE(THIS%LOG_FNAME_,'(A,I8.8,A)', IOSTAT=STAT) 'noop_output_manager_', PROCESSOR_TOPO%MYPROC_IO, '.log'
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2 )
    OPEN( FILE=TRIM(THIS%LOG_FNAME_), NEWUNIT=THIS%LOG_UNIT_, ACTION='WRITE', STATUS='REPLACE', IOSTAT=STAT )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3 )
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'SETUP OF THE OUTPUT MANAGER' )
    CALL LOG_VERSION( THIS%LOG_UNIT_ )
    CALL LOG_SYSINFO( THIS%LOG_UNIT_, PROCESSOR_TOPO%NPROC_IO, PROCESSOR_TOPO%MYPROC_IO )
    CALL PAR_PRINT( MODEL_PARAMS, THIS%LOG_UNIT_ )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to find YAML main configuration file: "'//TRIM(ADJUSTL(YAMLFNAME))//'"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to open (write) the binary parameter file' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to close (write) the binary parameter file' )
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

END SUBROUTINE NOOP_SETUP
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'NOOP_WRITE_ATM_DP'
SUBROUTINE NOOP_WRITE_ATM_DP( THIS, YDMSG, VALUES_DP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_CORE_MOD,          ONLY: JPRD_K
  USE :: OM_CORE_MOD,          ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD,          ONLY: OM_SET_CURRENT_MESSAGE_ATM
  USE :: GRIB_INFO_DATA_MOD,   ONLY: GRIB_INFO_T
  USE :: GRIB_INFO_MOD,        ONLY: GRIB_INFO_GET
  USE :: GRIB_INFO_MOD,        ONLY: GRIB_INFO_PRINT
  USE :: TRACK_TIME_MOD,       ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,       ONLY: TRACK_TIME_ACCESS_OR_CREATE
  USE :: TRACK_TIME_MOD,       ONLY: TRACK_TIME_PRINT
  USE :: MSG_UTILS_MOD,        ONLY: MSG_PRINT_ATM
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME

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

  ! Local variables
  TYPE(TIME_HISTORY_T) :: TIME_HIST
  TYPE(GRIB_INFO_T), POINTER :: GRIB_INFO

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Configure the debug manager
  CALL OM_SET_CURRENT_MESSAGE_ATM( YDMSG, MSG_PRINT_ATM )

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE ATMOSPHERE MESSAGE USING DOUBLE PRECISION VALUES' )
    CALL MSG_PRINT_ATM( YDMSG, THIS%LOG_UNIT_ )
  ENDIF

  ! Collect grib info of the curent field
  IF ( THIS%TEST_GRIB_INFO_ ) THEN
    PP_LOG_DEVELOP_STR( 'Collect grib info of the current field' )
    CALL GRIB_INFO_GET( YDMSG%PARAM_ID_, GRIB_INFO )
  ENDIF

  ! If needed log grib info
  IF ( THIS%VERBOSE_ .AND. THIS%TEST_GRIB_INFO_ ) THEN
    PP_LOG_DEVELOP_STR( 'Log the grib informations' )
    CALL GRIB_INFO_PRINT( GRIB_INFO, THIS%LOG_UNIT_ )
  ENDIF

  ! Get the last postprocessing step of the current field and update the history
  IF ( THIS%TEST_TRACK_TIME_ ) THEN
    PP_LOG_DEVELOP_STR( 'Get the last postprocessing step of the current field and update the history' )
    CALL TRACK_TIME_ACCESS_OR_CREATE( YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%ISTEP_, TIME_HIST )
  ENDIF

  ! If needed log optput history of the field
  IF ( THIS%VERBOSE_ .AND. THIS%TEST_TRACK_TIME_ ) THEN
    PP_LOG_DEVELOP_STR( 'Log the postprocessing history of the field' )
    CALL TRACK_TIME_PRINT( TIME_HIST, THIS%LOG_UNIT_ )
  ENDIF

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown type' )
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

END SUBROUTINE NOOP_WRITE_ATM_DP
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'NOOP_WRITE_ATM_SP'
SUBROUTINE NOOP_WRITE_ATM_SP( THIS, YDMSG, VALUES_SP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_CORE_MOD,          ONLY: JPRM_K
  USE :: OM_CORE_MOD,          ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD,          ONLY: OM_SET_CURRENT_MESSAGE_ATM
  USE :: GRIB_INFO_DATA_MOD,   ONLY: GRIB_INFO_T
  USE :: GRIB_INFO_MOD,        ONLY: GRIB_INFO_GET
  USE :: GRIB_INFO_MOD,        ONLY: GRIB_INFO_PRINT
  USE :: TRACK_TIME_MOD,       ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,       ONLY: TRACK_TIME_ACCESS_OR_CREATE
  USE :: TRACK_TIME_MOD,       ONLY: TRACK_TIME_PRINT
  USE :: MSG_UTILS_MOD,        ONLY: MSG_PRINT_ATM
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME

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

  ! Local variables
  TYPE(TIME_HISTORY_T) :: TIME_HIST
  TYPE(GRIB_INFO_T), POINTER :: GRIB_INFO

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Configure the debug manager
  CALL OM_SET_CURRENT_MESSAGE_ATM( YDMSG, MSG_PRINT_ATM )

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE ATMOSPHERE MESSAGE USING SINGLE PRECISION VALUES' )
    CALL MSG_PRINT_ATM( YDMSG, THIS%LOG_UNIT_ )
  ENDIF

  ! Collect grib info of the curent field
  IF ( THIS%TEST_GRIB_INFO_ ) THEN
    PP_LOG_DEVELOP_STR( 'Collect grib info of the current field' )
    CALL GRIB_INFO_GET( YDMSG%PARAM_ID_, GRIB_INFO )
  ENDIF

  ! If needed log grib info
  IF ( THIS%VERBOSE_ .AND. THIS%TEST_GRIB_INFO_ ) THEN
    PP_LOG_DEVELOP_STR( 'Log the grib informations' )
    CALL GRIB_INFO_PRINT( GRIB_INFO, THIS%LOG_UNIT_ )
  ENDIF

  ! Get the last postprocessing step of the current field and update the history
  IF ( THIS%TEST_TRACK_TIME_ ) THEN
    PP_LOG_DEVELOP_STR( 'Get the last postprocessing step of the current field and update the history' )
    CALL TRACK_TIME_ACCESS_OR_CREATE( YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%ISTEP_, TIME_HIST )
  ENDIF

  ! If needed log optput history of the field
  IF ( THIS%VERBOSE_ .AND. THIS%TEST_TRACK_TIME_ ) THEN
    PP_LOG_DEVELOP_STR( 'Log the postprocessing history of the field' )
    CALL TRACK_TIME_PRINT( TIME_HIST, THIS%LOG_UNIT_ )
  ENDIF

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown type' )
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

END SUBROUTINE NOOP_WRITE_ATM_SP
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'NOOP_WRITE_WAM_DP'
SUBROUTINE NOOP_WRITE_WAM_DP( THIS, YDMSG, VALUES_DP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_CORE_MOD,          ONLY: JPRD_K
  USE :: OM_CORE_MOD,          ONLY: OM_WAM_MSG_T
  USE :: OM_CORE_MOD,          ONLY: OM_SET_CURRENT_MESSAGE_WAM
  USE :: GRIB_INFO_DATA_MOD,   ONLY: GRIB_INFO_T
  USE :: GRIB_INFO_MOD,        ONLY: GRIB_INFO_GET
  USE :: GRIB_INFO_MOD,        ONLY: GRIB_INFO_PRINT
  USE :: TRACK_TIME_MOD,       ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,       ONLY: TRACK_TIME_ACCESS_OR_CREATE
  USE :: TRACK_TIME_MOD,       ONLY: TRACK_TIME_PRINT
  USE :: MSG_UTILS_MOD,        ONLY: MSG_PRINT_WAM
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME

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

  ! Local variables
  TYPE(TIME_HISTORY_T) :: TIME_HIST
  TYPE(GRIB_INFO_T), POINTER :: GRIB_INFO

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Configure the debug manager
  CALL OM_SET_CURRENT_MESSAGE_WAM( YDMSG, MSG_PRINT_WAM )

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE WAVE MESSAGE USING DOUBLE PRECISION VALUES' )
    CALL MSG_PRINT_WAM( YDMSG, THIS%LOG_UNIT_ )
  ENDIF

  ! Collect grib info of the curent field
  IF ( THIS%TEST_GRIB_INFO_ ) THEN
    PP_LOG_DEVELOP_STR( 'Collect grib info of the current field' )
    CALL GRIB_INFO_GET( YDMSG%PARAM_ID_, GRIB_INFO )
  ENDIF

  ! If needed log grib info
  IF ( THIS%VERBOSE_ .AND. THIS%TEST_GRIB_INFO_ ) THEN
    PP_LOG_DEVELOP_STR( 'Log the grib informations' )
    CALL GRIB_INFO_PRINT( GRIB_INFO, THIS%LOG_UNIT_ )
  ENDIF

  ! Get the last postprocessing step of the current field and update the history
  IF ( THIS%TEST_TRACK_TIME_ ) THEN
    PP_LOG_DEVELOP_STR( 'Get the last postprocessing step of the current field and update the history' )
    CALL TRACK_TIME_ACCESS_OR_CREATE( YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%ISTEP_, TIME_HIST )
  ENDIF

  ! If needed log optput history of the field
  IF ( THIS%VERBOSE_ .AND. THIS%TEST_TRACK_TIME_ ) THEN
    PP_LOG_DEVELOP_STR( 'Log the postprocessing history of the field' )
    CALL TRACK_TIME_PRINT( TIME_HIST, THIS%LOG_UNIT_ )
  ENDIF

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown type' )
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

END SUBROUTINE NOOP_WRITE_WAM_DP
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'NOOP_WRITE_WAM_SP'
SUBROUTINE NOOP_WRITE_WAM_SP( THIS, YDMSG, VALUES_SP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_CORE_MOD,          ONLY: JPRM_K
  USE :: OM_CORE_MOD,          ONLY: OM_WAM_MSG_T
  USE :: OM_CORE_MOD,          ONLY: OM_SET_CURRENT_MESSAGE_WAM
  USE :: GRIB_INFO_DATA_MOD,   ONLY: GRIB_INFO_T
  USE :: GRIB_INFO_MOD,        ONLY: GRIB_INFO_GET
  USE :: GRIB_INFO_MOD,        ONLY: GRIB_INFO_PRINT
  USE :: TRACK_TIME_MOD,       ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,       ONLY: TRACK_TIME_ACCESS_OR_CREATE
  USE :: TRACK_TIME_MOD,       ONLY: TRACK_TIME_PRINT
  USE :: MSG_UTILS_MOD,        ONLY: MSG_PRINT_WAM
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME

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

  ! Local variables
  TYPE(TIME_HISTORY_T) :: TIME_HIST
  TYPE(GRIB_INFO_T), POINTER :: GRIB_INFO

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Configure the debug manager
  CALL OM_SET_CURRENT_MESSAGE_WAM( YDMSG, MSG_PRINT_WAM )

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE WAVE MESSAGE USING SINGLE PRECISION VALUES' )
    CALL MSG_PRINT_WAM( YDMSG, THIS%LOG_UNIT_ )
  ENDIF

  ! Collect grib info of the curent field
  IF ( THIS%TEST_GRIB_INFO_ ) THEN
    PP_LOG_DEVELOP_STR( 'Collect grib info of the current field' )
    CALL GRIB_INFO_GET( YDMSG%PARAM_ID_, GRIB_INFO )
  ENDIF

  ! If needed log grib info
  IF ( THIS%VERBOSE_ .AND. THIS%TEST_GRIB_INFO_ ) THEN
    PP_LOG_DEVELOP_STR( 'Log the grib informations' )
    CALL GRIB_INFO_PRINT( GRIB_INFO, THIS%LOG_UNIT_ )
  ENDIF

  ! Get the last postprocessing step of the current field and update the history
  IF ( THIS%TEST_TRACK_TIME_ ) THEN
    PP_LOG_DEVELOP_STR( 'Get the last postprocessing step of the current field and update the history' )
    CALL TRACK_TIME_ACCESS_OR_CREATE( YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%ISTEP_, TIME_HIST )
  ENDIF

  ! If needed log optput history of the field
  IF ( THIS%VERBOSE_ .AND. THIS%TEST_TRACK_TIME_ ) THEN
    PP_LOG_DEVELOP_STR( 'Log the postprocessing history of the field' )
    CALL TRACK_TIME_PRINT( TIME_HIST, THIS%LOG_UNIT_ )
  ENDIF

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown type' )
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

END SUBROUTINE NOOP_WRITE_WAM_SP
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'NOOP_FLUSH_STEP'
SUBROUTINE NOOP_FLUSH_STEP( THIS, KSTEP )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(NOOP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: KSTEP

  ! Local variables
  CHARACTER(LEN=128) :: CLTMP

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! If needed log step
  IF ( THIS%VERBOSE_ ) THEN
    CLTMP = REPEAT(' ',128)
    WRITE(CLTMP, '(I10)' ) KSTEP
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH STEP: '//TRIM(ADJUSTL(CLTMP)) )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN
END SUBROUTINE NOOP_FLUSH_STEP
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'NOOP_FLUSH_LAST_STEP'
SUBROUTINE NOOP_FLUSH_LAST_STEP( THIS, KSTEP )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(NOOP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: KSTEP

  ! Local variables
  CHARACTER(LEN=128) :: CLTMP

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! If needed log step
  IF ( THIS%VERBOSE_ ) THEN
    CLTMP = REPEAT(' ',128)
    WRITE(CLTMP, '(I10)' ) KSTEP
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH LAST STEP'//TRIM(ADJUSTL(CLTMP)) )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE NOOP_FLUSH_LAST_STEP
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'NOOP_FLUSH_STEP_AND_TRIGGER_RESTART'
SUBROUTINE NOOP_FLUSH_STEP_AND_TRIGGER_RESTART( THIS, KSTEP )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(NOOP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: KSTEP

  ! Local variables
  CHARACTER(LEN=128) :: CLTMP

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CLTMP = REPEAT(' ',128)
    WRITE(CLTMP, '(I10)' ) KSTEP
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH STEP AND RESTART: '//TRIM(ADJUSTL(CLTMP)) )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE NOOP_FLUSH_STEP_AND_TRIGGER_RESTART
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Finalisation of the output manager
!>
!> As a `NOOP` output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'NOOP_FINALISE'
SUBROUTINE NOOP_FINALISE( THIS )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: GRIB_INFO_MOD,            ONLY: GRIB_INFO_FREE
  USE :: TRACK_TIME_MOD,           ONLY: TRACK_TIME_FREE
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: DESTROY_ENCODERS
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_CURR_TIME

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(NOOP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'FINALISATION' )
    CLOSE( THIS%LOG_UNIT_, IOSTAT=STAT )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 1 )
  ENDIF

  IF ( THIS%TEST_GRIB_INFO_ ) THEN
    PP_LOG_DEVELOP_STR( 'Free grib info' )
    CALL GRIB_INFO_FREE()
  ENDIF

  IF ( THIS%TEST_TRACK_TIME_ ) THEN
    PP_LOG_DEVELOP_STR( 'Free track time' )
    CALL TRACK_TIME_FREE()
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to close log files' )
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

END SUBROUTINE NOOP_FINALISE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE NOOP_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
