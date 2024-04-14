!> @file
!>
!> @brief Definition of an empty output manager
!>
!> @author Mirco Valentini
!> @date January 12, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'gribx_bin_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIBX_BIN_MOD'
MODULE GRIBX_BIN_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIM_K
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,              ONLY: MODEL_PAR_T
  USE :: OUTUPUT_MANAGER_BASE_MOD, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: GRIB_ENCODER_CONTAINER_T
  USE :: GRIB_METADATA_MOD,        ONLY: GRIB_METADATA_T
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_T

IMPLICIT NONE

! Default visibility
PRIVATE

!>
!> @brief Definition of the `GRIBX_BINARY_OUTPUT_MANAGER_T` derived type.
!>
!> The `GRIBX_BINARY_OUTPUT_MANAGER_T` type is a derived type that extends the
!> functionality of the `OUTPUT_MANAGER_BASE_A` abstract interface to be
!> used to demonstrate behavior without I/O when addressing complaints
!> or testing scenarios.
!>
!> @see OUTPUT_MANAGER_BASE_A
!>
TYPE, EXTENDS(OUTPUT_MANAGER_BASE_A) :: GRIBX_BINARY_OUTPUT_MANAGER_T

  !> All the profiling data
  TYPE(PROFILE_T) :: PROFILE_DATA_

  !
  ! Multiprocessor topology
  TYPE(PROC_TOPO_T), POINTER :: TOPOLOGY_ => NULL()

  !
  ! Model parameters
  TYPE(MODEL_PAR_T), POINTER :: MODEL_PAR_ => NULL()

  !
  ! Metadata objects used for the encoding
  TYPE(GRIB_METADATA_T)   :: GMD_

  !
  ! Grib file handle
  CHARACTER(LEN=1024) :: FIELDS_FILE_ = REPEAT(' ',1024)

  !
  ! Grib file handle
  INTEGER(KIND=JPIM_K) :: GRIB_FILE_HANDLE_ = -99

  !
  ! True if we need to flush every message
  LOGICAL :: FLUSH_EVERY_MESSAGE_ = .FALSE.

  !> Enable profiling
  LOGICAL :: PROFILE_ = .FALSE.

  !
  ! True for verbose execution
  LOGICAL :: VERBOSE_ = .FALSE.

  !> Unit used for logging purposes (if needed)
  INTEGER(KIND=JPIB_K) :: LOG_UNIT_ = -99

  !> File used for logging purposes (if needed)
  CHARACTER(LEN=1024)  :: LOG_FNAME_ = REPEAT(' ',1024)

CONTAINS

  !> @brief Setup of the output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: SETUP => GRIBX_BIN_SETUP

  !> @brief Read the configuration from YAML using fckit
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: READ_CFG_FROM_YAML => GRIBX_BINARY_READ_CFG_FROM_YAML

  !> @brief Write fields for the output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_ATM_DP => GRIBX_BIN_WRITE_ATM_DP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_ATM_SP => GRIBX_BIN_WRITE_ATM_SP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_WAM_DP => GRIBX_BIN_WRITE_WAM_DP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_WAM_SP => GRIBX_BIN_WRITE_WAM_SP

  !> @brief Notify that a step is finished
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_STEP => GRIBX_BIN_FLUSH_STEP

  !> @brief Notify that a step is finished and it was the last step
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_LAST_STEP => GRIBX_BIN_FLUSH_LAST_STEP

  !> @brief Notify that a step is finished and it is necessary to dump a restart
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_STEP_AND_TRIGGER_RESTART => GRIBX_BIN_FLUSH_STEP_AND_TRIGGER_RESTART

  !> @brief Finalise ancd cleanup teh output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FINALISE => GRIBX_BIN_FINALISE

END TYPE

! Whitelist of public symbols
PUBLIC :: GRIBX_BINARY_OUTPUT_MANAGER_T

CONTAINS





#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIBX_BINARY_READ_CFG_FROM_YAML'
SUBROUTINE GRIBX_BINARY_READ_CFG_FROM_YAML( THIS, CFG )

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX_BINARY_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(FCKIT_CONFIGURATION),            INTENT(IN)    :: CFG

  ! Local variables
  TYPE(FCKIT_CONFIGURATION) :: GRIBX_BIN_CFG
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  LOGICAL :: LTMP

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation
  THIS%FIELDS_FILE_ = REPEAT(' ',LEN(THIS%FIELDS_FILE_))

  ! Parse the configuration
  IF ( CFG%GET( 'gribx-binary-output-manager', GRIBX_BIN_CFG ) ) THEN

    IF ( ALLOCATED(CLTMP) ) DEALLOCATE(CLTMP)
    IF ( GRIBX_BIN_CFG%GET( 'fields-file-basename', CLTMP  ) ) THEN
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CLTMP), 1 )
      PP_DEBUG_CRITICAL_COND_THROW( LEN(CLTMP).GT.LEN(THIS%FIELDS_FILE_), 2 )
      WRITE(THIS%FIELDS_FILE_,'(A,A,I8.8,A)') TRIM(CLTMP), '_', THIS%TOPOLOGY_%MYPROC_IO, '.grib'
      IF ( ALLOCATED(CLTMP) ) DEALLOCATE( CLTMP )
    ELSE
      WRITE(THIS%FIELDS_FILE_,'(A,I8.8,A)') 'allfields_', THIS%TOPOLOGY_%MYPROC_IO, '.grib'
    ENDIF

    ! Read from YAML file the flag to enable track time testing
    IF ( GRIBX_BIN_CFG%GET( 'flush-every-message', LTMP  ) ) THEN
      THIS%FLUSH_EVERY_MESSAGE_ = LTMP
    ELSE
      THIS%FLUSH_EVERY_MESSAGE_ = .FALSE.
    ENDIF

    ! Read the verbose flag from YAML file
    IF ( GRIBX_BIN_CFG%GET( 'profile', LTMP  ) ) THEN
      THIS%PROFILE_ = LTMP
    ELSE
      THIS%PROFILE_ = .FALSE.
    ENDIF

    ! Read from YAML file the flag to enable track time testing
    IF ( GRIBX_BIN_CFG%GET( 'verbose', LTMP  ) ) THEN
      THIS%VERBOSE_ = LTMP
    ELSE
      THIS%VERBOSE_ = .FALSE.
    ENDIF

    ! Deallocate the dump-output-manager object
    CALL GRIBX_BIN_CFG%FINAL()
  ELSE

    ! Deallocate the dump-output-manager object
    CALL GRIBX_BIN_CFG%FINAL()
    THIS%VERBOSE_ = .FALSE.
    THIS%FLUSH_EVERY_MESSAGE_ = .FALSE.
    WRITE(THIS%FIELDS_FILE_,'(A,I8.8,A)') 'allfields_', THIS%TOPOLOGY_%MYPROC_IO, '.grib'
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
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Fields file name too long: "'//TRIM(ADJUSTL(THIS%FIELDS_FILE_))//'"' )
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

END SUBROUTINE GRIBX_BINARY_READ_CFG_FROM_YAML
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
#define PP_PROCEDURE_NAME 'GRIBX_BIN_SETUP'
SUBROUTINE GRIBX_BIN_SETUP( THIS, YAMLFNAME, PROCESSOR_TOPO, MODEL_PARAMS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: JPIM_K
  USE :: OM_CORE_MOD,              ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,              ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,              ONLY: OM_INIT_DEBUG_VARS
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: MAKE_ENCODERS
  USE :: TRACK_TIME_MOD,           ONLY: SUTRAK_TIME
  USE :: GRIB_INFO_MOD,            ONLY: SUGRIB_INFO_YAML
  USE :: PAR_UTILS_MOD,            ONLY: PAR_PRINT
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_VERSION
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_CURR_TIME
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_SYSINFO
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: OM_GETPID
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: OM_GET_HOSTNAME
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_START_SIMULATION

  ! Symbols imported from other libraries
  USE :: GRIB_API,                   ONLY: GRIB_OPEN_FILE
  USE :: GRIB_API,                   ONLY: GRIB_SUCCESS
  USE :: GRIB_API,                   ONLY: GRIB_GET_ERROR_STRING
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
  CLASS(GRIBX_BINARY_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                     INTENT(IN)    :: YAMLFNAME
  TYPE(PROC_TOPO_T), TARGET,            INTENT(IN)    :: PROCESSOR_TOPO
  TYPE(MODEL_PAR_T), TARGET,            INTENT(IN)    :: MODEL_PARAMS

  ! Local variables
  CHARACTER(LEN=256) :: HOSTNAME
  TYPE(FCKIT_CONFIGURATION) :: CFG
  INTEGER(KIND=JPIM_K) :: KRET
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

  ! Initialise topology object
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

  ! Read the specific configuration for "GRIBX_BINARY" output manager
  CALL THIS%READ_CFG_FROM_YAML( CFG )

  ! Initialise grib info
  CALL SUGRIB_INFO_YAML( CFG, PROCESSOR_TOPO, MODEL_PARAMS )

  ! Initialise time tracker for messages
  CALL SUTRAK_TIME( CFG, MODEL_PARAMS )

  ! Initialise all the encoders
  CALL MAKE_ENCODERS( CFG, MODEL_PARAMS, 'GRIB' )

  ! Destroy the fckit configuration object
  CALL CFG%FINAL()

  PP_LOG_DEVELOP_STR( 'Readed configuration file: '//TRIM(YAMLFNAME) )

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    CALL PROFILE_START_SIMULATION( THIS%PROFILE_DATA_, '.', PROCESSOR_TOPO%MYPROC_IO )
  ENDIF

  ! Logging
  IF ( THIS%VERBOSE_ ) THEN
    THIS%LOG_FNAME_ = REPEAT(' ',LEN(THIS%LOG_FNAME_))
    WRITE(THIS%LOG_FNAME_,'(A,I8.8,A)', IOSTAT=STAT) 'gribx_bin_output_manager_', PROCESSOR_TOPO%MYPROC_IO, '.log'
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2 )
    OPEN( FILE=TRIM(THIS%LOG_FNAME_), NEWUNIT=THIS%LOG_UNIT_, ACTION='WRITE', STATUS='REPLACE', IOSTAT=STAT )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3 )
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'SETUP OF THE OUTPUT MANAGER' )
    CALL LOG_VERSION( THIS%LOG_UNIT_ )
    CALL LOG_SYSINFO( THIS%LOG_UNIT_, PROCESSOR_TOPO%NPROC_IO, PROCESSOR_TOPO%MYPROC_IO )
    CALL PAR_PRINT( MODEL_PARAMS, THIS%LOG_UNIT_ )
  ENDIF

  ! Open the grib file
  IF ( .NOT.THIS%FLUSH_EVERY_MESSAGE_ ) THEN
    CALL GRIB_OPEN_FILE( THIS%GRIB_FILE_HANDLE_, TRIM(THIS%FIELDS_FILE_), 'w', STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 4 )
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
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to find YAML main configuration file: "'//TRIM(ADJUSTL(YAMLFNAME))//'"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write log filename' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to open log file' )
    CASE (4)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to open file: "'//TRIM(ADJUSTL(THIS%FIELDS_FILE_))//'"', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIBX_BIN_SETUP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> As a `NOOP` output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIBX_BIN_WRITE_ATM_DP'
SUBROUTINE GRIBX_BIN_WRITE_ATM_DP( THIS, YDMSG, VALUES_DP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIM_K
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: JPRD_K
  USE :: OM_CORE_MOD,              ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD,              ONLY: OM_SET_CURRENT_MESSAGE_ATM
  USE :: METADATA_FACTORY_MOD,     ONLY: METADATA_BASE_A
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: ENCODE_ATM
  USE :: GRIB_INFO_DATA_MOD,       ONLY: GRIB_INFO_T
  USE :: GRIB_INFO_MOD,            ONLY: GRIB_INFO_GET
  USE :: GRIB_INFO_MOD,            ONLY: GRIB_INFO_PRINT
  USE :: TRACK_TIME_MOD,           ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,           ONLY: TRACK_TIME_ACCESS_OR_CREATE
  USE :: TRACK_TIME_MOD,           ONLY: TRACK_TIME_PRINT
  USE :: MSG_UTILS_MOD,            ONLY: MSG_PRINT_ATM
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_MESSAGE

  ! Symbols imported from other libraries
  USE :: GRIB_API, ONLY: GRIB_OPEN_FILE
  USE :: GRIB_API, ONLY: GRIB_CLOSE_FILE
  USE :: GRIB_API, ONLY: GRIB_WRITE
  USE :: GRIB_API, ONLY: GRIB_SUCCESS
  USE :: GRIB_API, ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX_BINARY_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_ATM_MSG_T),                           INTENT(IN)    :: YDMSG
  REAL(KIND=JPRD_K), DIMENSION(:),              INTENT(IN)    :: VALUES_DP

  ! Local variables
  INTEGER(KIND=JPIB_K) :: UNIT
  INTEGER(KIND=JPIM_K) :: KRET
  TYPE(TIME_HISTORY_T) :: TIME_HIST
  TYPE(GRIB_INFO_T), POINTER :: GRIB_INFO
  CLASS(METADATA_BASE_A), POINTER :: PGMD

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    CALL PROFILE_MESSAGE( THIS%PROFILE_DATA_, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_ )
  ENDIF

  ! Configure the debug manager
  CALL OM_SET_CURRENT_MESSAGE_ATM( YDMSG, MSG_PRINT_ATM )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES_DP).LT.YDMSG%NVALUES_, 1 )

  ! Associate the pointers to the metadata
  PGMD => THIS%GMD_
  PP_METADATA_INIT_LOGGING( PGMD, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%IPREF_, YDMSG%IREPRES_ )

  ! Lookup grib informations related to the requested paramId
  CALL GRIB_INFO_GET( YDMSG%PARAM_ID_, GRIB_INFO )

  ! Recover information about last time the field has been encoded
  CALL TRACK_TIME_ACCESS_OR_CREATE( YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%ISTEP_, TIME_HIST )

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE ATMOSPHERE MESSAGE USING DOUBLE PRECISION VALUES' )
    CALL MSG_PRINT_ATM( YDMSG, THIS%LOG_UNIT_ )
    CALL GRIB_INFO_PRINT( GRIB_INFO, THIS%LOG_UNIT_ )
    CALL TRACK_TIME_PRINT( TIME_HIST, THIS%LOG_UNIT_ )
  ENDIF

  !
  ! Encode throws an error if an error happens, and return false if the field does not need to be emitted
  IF ( ENCODE_ATM( THIS%MODEL_PAR_, GRIB_INFO, TIME_HIST, YDMSG, PGMD ) ) THEN

    !
    ! Set global size and values into the grib handle
    CALL THIS%GMD_%SET( 'values', VALUES_DP(1:YDMSG%NVALUES_) )

    IF ( THIS%FLUSH_EVERY_MESSAGE_ ) THEN

      ! Open the grib file
      CALL GRIB_OPEN_FILE( THIS%GRIB_FILE_HANDLE_, TRIM(THIS%FIELDS_FILE_), 'a', STATUS=KRET )
      PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

    ENDIF

    ! Write to the grib file
    CALL GRIB_WRITE( THIS%GMD_%GET_HANDLE( ), THIS%GRIB_FILE_HANDLE_, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 3 )

    IF ( THIS%FLUSH_EVERY_MESSAGE_ ) THEN

      ! Close the grib file
      CALL GRIB_CLOSE_FILE( THIS%GRIB_FILE_HANDLE_, STATUS=KRET )
      PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 4 )

    ENDIF

  ENDIF

  ! should not happen but just to be sure
  IF ( THIS%GMD_%INITIALIZED() ) THEN
    PP_METADATA_FINALISE_LOGGING( PGMD )
    CALL THIS%GMD_%DESTROY()
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
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Wrong values size' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to open file: \"./allfields.grib\"', KRET, GRIB_ERROR )
    CASE (3)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to write to file', KRET, GRIB_ERROR )
    CASE (4)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to close file: \"./allfields.grib\"', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIBX_BIN_WRITE_ATM_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> As a `NOOP` output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIBX_BIN_WRITE_ATM_SP'
SUBROUTINE GRIBX_BIN_WRITE_ATM_SP( THIS, YDMSG, VALUES_SP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIM_K
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: JPRM_K
  USE :: OM_CORE_MOD,              ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD,              ONLY: OM_SET_CURRENT_MESSAGE_ATM
  USE :: METADATA_FACTORY_MOD,     ONLY: METADATA_BASE_A
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: ENCODE_ATM
  USE :: GRIB_INFO_DATA_MOD,       ONLY: GRIB_INFO_T
  USE :: GRIB_INFO_MOD,            ONLY: GRIB_INFO_GET
  USE :: GRIB_INFO_MOD,            ONLY: GRIB_INFO_PRINT
  USE :: TRACK_TIME_MOD,           ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,           ONLY: TRACK_TIME_ACCESS_OR_CREATE
  USE :: TRACK_TIME_MOD,           ONLY: TRACK_TIME_PRINT
  USE :: MSG_UTILS_MOD,            ONLY: MSG_PRINT_ATM
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_MESSAGE

  ! Symbols imported from other libraries
  USE :: GRIB_API, ONLY: GRIB_OPEN_FILE
  USE :: GRIB_API, ONLY: GRIB_CLOSE_FILE
  USE :: GRIB_API, ONLY: GRIB_WRITE
  USE :: GRIB_API, ONLY: GRIB_SUCCESS
  USE :: GRIB_API, ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX_BINARY_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_ATM_MSG_T),                           INTENT(IN)    :: YDMSG
  REAL(KIND=JPRM_K), DIMENSION(:),              INTENT(IN)    :: VALUES_SP

  ! Local variables
  INTEGER(KIND=JPIB_K) :: UNIT
  INTEGER(KIND=JPIM_K) :: KRET
  TYPE(TIME_HISTORY_T) :: TIME_HIST
  TYPE(GRIB_INFO_T), POINTER :: GRIB_INFO
  CLASS(METADATA_BASE_A), POINTER :: PGMD

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    CALL PROFILE_MESSAGE( THIS%PROFILE_DATA_, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_  )
  ENDIF

  ! Configure the debug manager
  CALL OM_SET_CURRENT_MESSAGE_ATM( YDMSG, MSG_PRINT_ATM )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES_SP).LT.YDMSG%NVALUES_, 1 )

  ! Associate the pointers to the metadata
  PGMD => THIS%GMD_
  PP_METADATA_INIT_LOGGING( PGMD, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%IPREF_, YDMSG%IREPRES_ )

  ! Lookup grib informations related to the requested paramId
  CALL GRIB_INFO_GET( YDMSG%PARAM_ID_, GRIB_INFO )

  ! Recover information about last time the field has been encoded
  CALL TRACK_TIME_ACCESS_OR_CREATE( YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%ISTEP_, TIME_HIST )

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE ATMOSPHERE MESSAGE USING SINGLE PRECISION VALUES' )
    CALL MSG_PRINT_ATM( YDMSG, THIS%LOG_UNIT_ )
    CALL GRIB_INFO_PRINT( GRIB_INFO, THIS%LOG_UNIT_ )
    CALL TRACK_TIME_PRINT( TIME_HIST, THIS%LOG_UNIT_ )
  ENDIF

  !
  ! Encode throws an error if an error happens, and return false if the field does not need to be emitted
  IF ( ENCODE_ATM( THIS%MODEL_PAR_, GRIB_INFO, TIME_HIST, YDMSG, PGMD ) ) THEN

    !
    ! Set global size and values into the grib handle
    CALL THIS%GMD_%SET( 'values', VALUES_SP(1:YDMSG%NVALUES_) )

    IF ( THIS%FLUSH_EVERY_MESSAGE_ ) THEN

      ! Open the grib file
      CALL GRIB_OPEN_FILE( THIS%GRIB_FILE_HANDLE_, TRIM(THIS%FIELDS_FILE_), 'a', STATUS=KRET )
      PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

    ENDIF

    ! Write to the grib file
    CALL GRIB_WRITE( THIS%GMD_%GET_HANDLE( ), THIS%GRIB_FILE_HANDLE_, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 3 )

    IF ( THIS%FLUSH_EVERY_MESSAGE_ ) THEN

      ! Close the grib file
      CALL GRIB_CLOSE_FILE( THIS%GRIB_FILE_HANDLE_, STATUS=KRET )
      PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 4 )

    ENDIF

  ENDIF

  ! should not happen but just to be sure
  IF ( THIS%GMD_%INITIALIZED() ) THEN
    ! Destroy the metadata object
    PP_METADATA_FINALISE_LOGGING( PGMD )
    CALL THIS%GMD_%DESTROY()
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
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Wrong values size' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to open file: \"./allfields.grib\"', KRET, GRIB_ERROR )
    CASE (3)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to write to file', KRET, GRIB_ERROR )
    CASE (4)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to close file: \"./allfields.grib\"', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIBX_BIN_WRITE_ATM_SP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE






!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> As a `NOOP` output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIBX_BIN_WRITE_WAM_DP'
SUBROUTINE GRIBX_BIN_WRITE_WAM_DP( THIS, YDMSG, VALUES_DP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIM_K
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: JPRD_K
  USE :: OM_CORE_MOD,              ONLY: OM_WAM_MSG_T
  USE :: OM_CORE_MOD,              ONLY: OM_SET_CURRENT_MESSAGE_WAM
  USE :: METADATA_FACTORY_MOD,     ONLY: METADATA_BASE_A
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: ENCODE_WAM
  USE :: GRIB_INFO_DATA_MOD,       ONLY: GRIB_INFO_T
  USE :: GRIB_INFO_MOD,            ONLY: GRIB_INFO_GET
  USE :: GRIB_INFO_MOD,            ONLY: GRIB_INFO_PRINT
  USE :: TRACK_TIME_MOD,           ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,           ONLY: TRACK_TIME_ACCESS_OR_CREATE
  USE :: TRACK_TIME_MOD,           ONLY: TRACK_TIME_PRINT
  USE :: MSG_UTILS_MOD,            ONLY: MSG_PRINT_WAM
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_MESSAGE

  ! Symbols imported from other libraries
  USE :: GRIB_API, ONLY: GRIB_OPEN_FILE
  USE :: GRIB_API, ONLY: GRIB_CLOSE_FILE
  USE :: GRIB_API, ONLY: GRIB_WRITE
  USE :: GRIB_API, ONLY: GRIB_SUCCESS
  USE :: GRIB_API, ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX_BINARY_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_WAM_MSG_T),                           INTENT(IN)    :: YDMSG
  REAL(KIND=JPRD_K), DIMENSION(:),              INTENT(IN)    :: VALUES_DP

  ! Local variables
  INTEGER(KIND=JPIB_K) :: UNIT
  INTEGER(KIND=JPIM_K) :: KRET
  TYPE(TIME_HISTORY_T) :: TIME_HIST
  TYPE(GRIB_INFO_T), POINTER :: GRIB_INFO
  CLASS(METADATA_BASE_A), POINTER :: PGMD

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    CALL PROFILE_MESSAGE( THIS%PROFILE_DATA_, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_  )
  ENDIF

  ! Configure the debug manager
  CALL OM_SET_CURRENT_MESSAGE_WAM( YDMSG, MSG_PRINT_WAM )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES_DP).LT.YDMSG%NVALUES_, 1 )

  ! Associate the pointers to the metadata
  PGMD => THIS%GMD_
  PP_METADATA_INIT_LOGGING( PGMD, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%IPREF_, YDMSG%IREPRES_ )

  ! Lookup grib informations related to the requested paramId
  CALL GRIB_INFO_GET( YDMSG%PARAM_ID_, GRIB_INFO )

  ! Recover information about last time the field has been encoded
  CALL TRACK_TIME_ACCESS_OR_CREATE( YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%ISTEP_, TIME_HIST )

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE WAVE MESSAGE USING DOUBLE PRECISION VALUES' )
    CALL MSG_PRINT_WAM( YDMSG, THIS%LOG_UNIT_ )
    CALL GRIB_INFO_PRINT( GRIB_INFO, THIS%LOG_UNIT_ )
    CALL TRACK_TIME_PRINT( TIME_HIST, THIS%LOG_UNIT_ )
  ENDIF

  !
  ! Encode throws an error if an error happens, and return false if the field does not need to be emitted
  IF ( ENCODE_WAM( THIS%MODEL_PAR_, GRIB_INFO, TIME_HIST, YDMSG, PGMD ) ) THEN

    !
    ! Set global size and values into the grib handle
    CALL THIS%GMD_%SET( 'values', VALUES_DP(1:YDMSG%NVALUES_) )

    IF ( THIS%FLUSH_EVERY_MESSAGE_ ) THEN

      ! Open the grib file
      CALL GRIB_OPEN_FILE( THIS%GRIB_FILE_HANDLE_, TRIM(THIS%FIELDS_FILE_), 'a', STATUS=KRET )
      PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

    ENDIF

    ! Write to the grib file
    CALL GRIB_WRITE( THIS%GMD_%GET_HANDLE( ), THIS%GRIB_FILE_HANDLE_, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 3 )

    IF ( THIS%FLUSH_EVERY_MESSAGE_ ) THEN

      ! Close the grib file
      CALL GRIB_CLOSE_FILE( THIS%GRIB_FILE_HANDLE_, STATUS=KRET )
      PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 4 )

    ENDIF

  ENDIF

  ! should not happen but just to be sure
  IF ( THIS%GMD_%INITIALIZED() ) THEN
    ! Destroy the metadata object
    PP_METADATA_FINALISE_LOGGING( PGMD )
    CALL THIS%GMD_%DESTROY()
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
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Wrong values size' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to open file: \"./allfields.grib\"', KRET, GRIB_ERROR )
    CASE (3)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to write to file', KRET, GRIB_ERROR )
    CASE (4)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to close file: \"./allfields.grib\"', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIBX_BIN_WRITE_WAM_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Write fields contained in a request received by the IOserver.
!>
!> As a `NOOP` output manager, this routine is intentionally left empty.
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] ydmsg Message to be encoded
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIBX_BIN_WRITE_WAM_SP'
SUBROUTINE GRIBX_BIN_WRITE_WAM_SP( THIS, YDMSG, VALUES_SP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIM_K
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: JPRM_K
  USE :: OM_CORE_MOD,              ONLY: OM_WAM_MSG_T
  USE :: OM_CORE_MOD,              ONLY: OM_SET_CURRENT_MESSAGE_WAM
  USE :: METADATA_FACTORY_MOD,     ONLY: METADATA_BASE_A
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: ENCODE_WAM
  USE :: GRIB_INFO_DATA_MOD,       ONLY: GRIB_INFO_T
  USE :: GRIB_INFO_MOD,            ONLY: GRIB_INFO_GET
  USE :: GRIB_INFO_MOD,            ONLY: GRIB_INFO_PRINT
  USE :: TRACK_TIME_MOD,           ONLY: TIME_HISTORY_T
  USE :: TRACK_TIME_MOD,           ONLY: TRACK_TIME_ACCESS_OR_CREATE
  USE :: TRACK_TIME_MOD,           ONLY: TRACK_TIME_PRINT
  USE :: MSG_UTILS_MOD,            ONLY: MSG_PRINT_WAM
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_MESSAGE


  ! Symbols imported from other libraries
  USE :: GRIB_API, ONLY: GRIB_OPEN_FILE
  USE :: GRIB_API, ONLY: GRIB_CLOSE_FILE
  USE :: GRIB_API, ONLY: GRIB_WRITE
  USE :: GRIB_API, ONLY: GRIB_SUCCESS
  USE :: GRIB_API, ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX_BINARY_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_WAM_MSG_T),                           INTENT(IN)    :: YDMSG
  REAL(KIND=JPRM_K), DIMENSION(:),              INTENT(IN)    :: VALUES_SP

  ! Local variables
  INTEGER(KIND=JPIB_K) :: UNIT
  INTEGER(KIND=JPIM_K) :: KRET
  TYPE(TIME_HISTORY_T) :: TIME_HIST
  TYPE(GRIB_INFO_T), POINTER :: GRIB_INFO
  CLASS(METADATA_BASE_A), POINTER :: PGMD

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    CALL PROFILE_MESSAGE( THIS%PROFILE_DATA_, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_  )
  ENDIF

  ! Configure the debug manager
  CALL OM_SET_CURRENT_MESSAGE_WAM( YDMSG, MSG_PRINT_WAM )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES_SP).LT.YDMSG%NVALUES_, 1 )

  ! Associate the pointers to the metadata
  PGMD => THIS%GMD_
  PP_METADATA_INIT_LOGGING( PGMD, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%IPREF_, YDMSG%IREPRES_ )

  ! Lookup grib informations related to the requested paramId
  CALL GRIB_INFO_GET( YDMSG%PARAM_ID_, GRIB_INFO )

  ! Recover information about last time the field has been encoded
  CALL TRACK_TIME_ACCESS_OR_CREATE( YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%ISTEP_, TIME_HIST )

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE WAVE MESSAGE USING DOUBLE PRECISION VALUES' )
    CALL MSG_PRINT_WAM( YDMSG, THIS%LOG_UNIT_ )
    CALL GRIB_INFO_PRINT( GRIB_INFO, THIS%LOG_UNIT_ )
    CALL TRACK_TIME_PRINT( TIME_HIST, THIS%LOG_UNIT_ )
  ENDIF

  !
  ! Encode throws an error if an error happens, and return false if the field does not need to be emitted
  IF ( ENCODE_WAM( THIS%MODEL_PAR_, GRIB_INFO, TIME_HIST, YDMSG, PGMD ) ) THEN

    !
    ! Set global size and values into the grib handle
    CALL THIS%GMD_%SET( 'values', VALUES_SP(1:YDMSG%NVALUES_) )

    IF ( THIS%FLUSH_EVERY_MESSAGE_ ) THEN

      ! Open the grib file
      CALL GRIB_OPEN_FILE( THIS%GRIB_FILE_HANDLE_, TRIM(THIS%FIELDS_FILE_), 'a', STATUS=KRET )
      PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

    ENDIF

    ! Write to the grib file
    CALL GRIB_WRITE( THIS%GMD_%GET_HANDLE( ), THIS%GRIB_FILE_HANDLE_, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 3 )

    IF ( THIS%FLUSH_EVERY_MESSAGE_ ) THEN

      ! Close the grib file
      CALL GRIB_CLOSE_FILE( THIS%GRIB_FILE_HANDLE_, STATUS=KRET )
      PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 4 )

    ENDIF

  ENDIF

  ! should not happen but just to be sure
  IF ( THIS%GMD_%INITIALIZED() ) THEN
    ! Destroy the metadata object
    PP_METADATA_FINALISE_LOGGING( PGMD )
    CALL THIS%GMD_%DESTROY()
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
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Wrong values size' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to open file: \"./allfields.grib\"', KRET, GRIB_ERROR )
    CASE (3)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to write to file', KRET, GRIB_ERROR )
    CASE (4)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to close file: \"./allfields.grib\"', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIBX_BIN_WRITE_WAM_SP
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
#define PP_PROCEDURE_NAME 'GRIBX_BIN_FLUSH_STEP'
SUBROUTINE GRIBX_BIN_FLUSH_STEP( THIS, KSTEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported from other libraries
  USE :: GRIB_API, ONLY: GRIB_OPEN_FILE
  USE :: GRIB_API, ONLY: GRIB_CLOSE_FILE
  USE :: GRIB_API, ONLY: GRIB_SUCCESS
  USE :: GRIB_API, ONLY: GRIB_GET_ERROR_STRING
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,       ONLY: PROFILE_FLUSH

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX_BINARY_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                 INTENT(IN)    :: KSTEP

  ! Local variables
  CHARACTER(LEN=128) :: CLTMP
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    CALL PROFILE_FLUSH( THIS%PROFILE_DATA_, KSTEP )
  ENDIF

  ! If needed log step
  IF ( THIS%VERBOSE_ ) THEN
    CLTMP = REPEAT(' ',128)
    WRITE(CLTMP, '(I10)' ) KSTEP
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH STEP: '//TRIM(ADJUSTL(CLTMP)) )
  ENDIF

  ! As far as I know, this is the way to flush a grib message
  IF ( .NOT.THIS%FLUSH_EVERY_MESSAGE_ ) THEN

    ! Close the grib file
    CALL GRIB_CLOSE_FILE( THIS%GRIB_FILE_HANDLE_, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 1 )

    ! Open the grib file
    CALL GRIB_OPEN_FILE( THIS%GRIB_FILE_HANDLE_, TRIM(THIS%FIELDS_FILE_), 'a', STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

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
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to close file: \"./allfields.grib\"', KRET, GRIB_ERROR )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to open file: \"./allfields.grib\"', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIBX_BIN_FLUSH_STEP
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
#define PP_PROCEDURE_NAME 'GRIBX_BIN_FLUSH_LAST_STEP'
SUBROUTINE GRIBX_BIN_FLUSH_LAST_STEP( THIS, KSTEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIM_K
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,       ONLY: PROFILE_FLUSH_LAST_STEP

  ! Symbols imported from other libraries
  USE :: GRIB_API, ONLY: GRIB_CLOSE_FILE
  USE :: GRIB_API, ONLY: GRIB_SUCCESS
  USE :: GRIB_API, ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX_BINARY_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                 INTENT(IN)    :: KSTEP

  ! Local variables
  CHARACTER(LEN=128) :: CLTMP
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    CALL PROFILE_FLUSH_LAST_STEP( THIS%PROFILE_DATA_, KSTEP )
  ENDIF

  ! If needed log step
  IF ( THIS%VERBOSE_ ) THEN
    CLTMP = REPEAT(' ',128)
    WRITE(CLTMP, '(I10)' ) KSTEP
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH LAST STEP'//TRIM(ADJUSTL(CLTMP)) )
  ENDIF


  ! As far as I know, this is the way to flush a grib message
  IF ( .NOT.THIS%FLUSH_EVERY_MESSAGE_ ) THEN

    ! Close the grib file
    CALL GRIB_CLOSE_FILE( THIS%GRIB_FILE_HANDLE_, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 1 )

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
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to close file: \"./allfields.grib\"', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIBX_BIN_FLUSH_LAST_STEP
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
#define PP_PROCEDURE_NAME 'GRIBX_BIN_FLUSH_STEP_AND_TRIGGER_RESTART'
SUBROUTINE GRIBX_BIN_FLUSH_STEP_AND_TRIGGER_RESTART( THIS, KSTEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIM_K
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,       ONLY: PROFILE_FLUSH_AND_RESTART

  ! Symbols imported from other libraries
  USE :: GRIB_API, ONLY: GRIB_OPEN_FILE
  USE :: GRIB_API, ONLY: GRIB_CLOSE_FILE
  USE :: GRIB_API, ONLY: GRIB_SUCCESS
  USE :: GRIB_API, ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS


IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX_BINARY_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                 INTENT(IN)    :: KSTEP

  ! Local variables
  CHARACTER(LEN=128)   :: CLTMP
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    CALL PROFILE_FLUSH_AND_RESTART( THIS%PROFILE_DATA_, KSTEP)
  ENDIF

  ! If needed log step and restart
  IF ( THIS%VERBOSE_ ) THEN
    CLTMP = REPEAT(' ',128)
    WRITE(CLTMP, '(I10)' ) KSTEP
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH STEP AND RESTART: '//TRIM(ADJUSTL(CLTMP)) )
  ENDIF

  ! As far as I know, this is the way to flush a grib message
  IF ( .NOT.THIS%FLUSH_EVERY_MESSAGE_ ) THEN

    ! Close the grib file
    CALL GRIB_CLOSE_FILE( THIS%GRIB_FILE_HANDLE_, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 1 )

    ! Open the grib file
    CALL GRIB_OPEN_FILE( THIS%GRIB_FILE_HANDLE_, TRIM(THIS%FIELDS_FILE_), 'a', STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

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
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to close file: \"./allfields.grib\"', KRET, GRIB_ERROR )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to open file: \"./allfields.grib\"', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIBX_BIN_FLUSH_STEP_AND_TRIGGER_RESTART
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
#define PP_PROCEDURE_NAME 'GRIBX_BIN_FINALISE'
SUBROUTINE GRIBX_BIN_FINALISE( THIS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIM_K
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: DESTROY_ENCODERS
  USE :: GRIB_INFO_MOD,            ONLY: GRIB_INFO_FREE
  USE :: TRACK_TIME_MOD,           ONLY: TRACK_TIME_FREE
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_END_SIMULATION


  ! Symbols imported from other libraries
  USE :: GRIB_API, ONLY: GRIB_CLOSE_FILE
  USE :: GRIB_API, ONLY: GRIB_SUCCESS
  USE :: GRIB_API, ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS


IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX_BINARY_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    CALL PROFILE_END_SIMULATION( THIS%PROFILE_DATA_ )
  ENDIF

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'FINALISATION' )
    CLOSE( THIS%LOG_UNIT_, IOSTAT=STAT )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 1 )
  ENDIF

  ! Destroy services
  CALL DESTROY_ENCODERS()
  CALL GRIB_INFO_FREE()
  CALL TRACK_TIME_FREE()

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

END SUBROUTINE GRIBX_BIN_FINALISE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIBX_BIN_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
