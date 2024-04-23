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
#define PP_FILE_NAME 'gribx2multio_raw_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIBX2MULTIO_RAW_MOD'
MODULE GRIBX2MULTIO_RAW_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,              ONLY: MODEL_PAR_T
  USE :: OUTUPUT_MANAGER_BASE_MOD, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: GRIB_ENCODER_CONTAINER_T
  USE :: GRIB_METADATA_MOD,        ONLY: GRIB_METADATA_T
  USE :: MULTIO_METADATA_MOD,      ONLY: MULTIO_METADATA_T
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_T

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION
  USE :: MULTIO_API,                 ONLY: MULTIO_HANDLE

IMPLICIT NONE

! Default visibility
PRIVATE

!> Output manager name
CHARACTER(LEN=*), PARAMETER :: GRIBX2MULTIO_RAW_OMNAME='grib-header-to-multio'

!>
!> @brief Definition of the `GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T` derived type.
!>
!> The `GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T` type is a derived type that extends the
!> functionality of the `OUTPUT_MANAGER_BASE_A` abstract interface to be
!> used to demonstrate behavior without I/O when addressing complaints
!> or testing scenarios.
!>
!> @see OUTPUT_MANAGER_BASE_A
!>
TYPE, EXTENDS(OUTPUT_MANAGER_BASE_A) :: GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T

  !> All the profiling data
  TYPE(PROFILE_T) :: PROFILE_DATA_

  !> Multiprocessor topology
  TYPE(PROC_TOPO_T), POINTER :: TOPOLOGY_ => NULL()

  !> Model parameters
  TYPE(MODEL_PAR_T), POINTER :: MODEL_PAR_ => NULL()

  !> Metadata object used for the encoding (grib)
  TYPE(GRIB_METADATA_T)   :: GMD_

  !> Metadata object used for the encoding (multio)
  TYPE(MULTIO_METADATA_T) :: MMD_

  !> Configuration file used from multio
  CHARACTER(LEN=1024) :: MULTIO_CONFIG_FILE_ = REPEAT(' ',1024)

  !> Enable profiling
  LOGICAL :: PROFILE_ = .FALSE.

  ! True for verbose execution
  LOGICAL :: SAVE_REPORT_ = .FALSE.

  !> True for verbose execution
  LOGICAL :: VERBOSE_ = .FALSE.

  !> Unit used for logging purposes (if needed)
  INTEGER(KIND=JPIB_K) :: LOG_UNIT_ = -99

  !> File used for logging purposes (if needed)
  CHARACTER(LEN=1024)  :: LOG_FNAME_ = REPEAT(' ',1024)

  !> Multio Handle used to interact with multio
  TYPE(MULTIO_HANDLE) :: MIO_


CONTAINS

  !> @brief Setup of the output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: SETUP => GRIBX2MULTIO_RAW_SETUP

  !> @brief Read the configuration from YAML using fckit
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: READ_CFG_FROM_YAML => GRIBX2MULTIO_RAW_READ_CFG_FROM_YAML

  !> @brief Write fields for the output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_ATM_DP => GRIBX2MULTIO_RAW_WRITE_ATM_DP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_ATM_SP => GRIBX2MULTIO_RAW_WRITE_ATM_SP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_WAM_DP => GRIBX2MULTIO_RAW_WRITE_WAM_DP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_WAM_SP => GRIBX2MULTIO_RAW_WRITE_WAM_SP

  !> @brief Notify that a step is finished
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_STEP => GRIBX2MULTIO_RAW_FLUSH_STEP

  !> @brief Notify that a step is finished and it was the last step
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_LAST_STEP => GRIBX2MULTIO_RAW_FLUSH_LAST_STEP

  !> @brief Notify that a step is finished and it is necessary to dump a restart
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_STEP_AND_TRIGGER_RESTART => GRIBX2MULTIO_RAW_FLUSH_STEP_AND_TRIGGER_RESTART

  !> @brief Finalise ancd cleanup teh output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FINALISE => GRIBX2MULTIO_RAW_FINALISE

END TYPE

! Whitelist of public symbols
PUBLIC :: GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T
PUBLIC :: GRIBX2MULTIO_RAW_OMNAME

CONTAINS

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIBX2MULTIO_RAW_READ_CFG_FROM_YAML'
SUBROUTINE GRIBX2MULTIO_RAW_READ_CFG_FROM_YAML( THIS, CFG )

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(FCKIT_CONFIGURATION),                INTENT(IN)    :: CFG

  ! Local variables
  TYPE(FCKIT_CONFIGURATION) :: GRIBX2MULTIO_RAW_CFG
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  LOGICAL :: LTMP
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( CFG%GET( GRIBX2MULTIO_RAW_OMNAME, GRIBX2MULTIO_RAW_CFG ) ) THEN

    IF ( ALLOCATED(CLTMP) ) DEALLOCATE(CLTMP)
    IF ( GRIBX2MULTIO_RAW_CFG%GET( 'multio-plans-file', CLTMP  ) ) THEN
      THIS%MULTIO_CONFIG_FILE_ = REPEAT(' ',LEN(THIS%MULTIO_CONFIG_FILE_))
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CLTMP), 1 )
      PP_DEBUG_CRITICAL_COND_THROW( LEN(CLTMP).GT.LEN(THIS%MULTIO_CONFIG_FILE_), 2 )
      THIS%MULTIO_CONFIG_FILE_ = CLTMP(:)
      IF ( ALLOCATED(CLTMP) ) DEALLOCATE( CLTMP )
      INQUIRE( FILE=TRIM(THIS%MULTIO_CONFIG_FILE_), EXIST=EX )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 3 )
    ELSE
      PP_DEBUG_CRITICAL_THROW( 4 )
    ENDIF

    ! Read the verbose flag from YAML file
    IF ( GRIBX2MULTIO_RAW_CFG%GET( 'verbose', LTMP  ) ) THEN
      THIS%VERBOSE_ = LTMP
    ELSE
      THIS%VERBOSE_ = .FALSE.
    ENDIF

    ! Read the verbose flag from YAML file
    IF ( GRIBX2MULTIO_RAW_CFG%GET( 'profile', LTMP  ) ) THEN
      THIS%PROFILE_ = LTMP
    ELSE
      THIS%PROFILE_ = .FALSE.
    ENDIF

    ! Read from YAML file the flag to enable track time testing
    IF ( GRIBX2MULTIO_RAW_CFG%GET( 'report', LTMP  ) ) THEN
      THIS%SAVE_REPORT_ = LTMP
    ELSE
      THIS%SAVE_REPORT_ = .FALSE.
    ENDIF

    ! Deallocate memory for sub-configuration
    CALL GRIBX2MULTIO_RAW_CFG%FINAL()
  ELSE
    PP_DEBUG_CRITICAL_THROW( 5 )
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Multio plans file name too long: "'//TRIM(ADJUSTL(THIS%MULTIO_CONFIG_FILE_))//'"' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to find multio plans file: "'//TRIM(ADJUSTL(THIS%MULTIO_CONFIG_FILE_))//'"' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, '"multio-plans-file" key expected in YAML' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, '"gribx2multio-binary-output-manager" key expected in YAML' )
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

END SUBROUTINE GRIBX2MULTIO_RAW_READ_CFG_FROM_YAML
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
#define PP_PROCEDURE_NAME 'GRIBX2MULTIO_RAW_SETUP'
SUBROUTINE GRIBX2MULTIO_RAW_SETUP( THIS, YAMLFNAME, PROCESSOR_TOPO, MODEL_PARAMS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,              ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,              ONLY: OM_INIT_DEBUG_VARS
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: MAKE_ENCODERS
  USE :: ENCODING_INFO_MOD,        ONLY: SUENCODING_INFO
  USE :: OM_MULTIO_UTILS_MOD,      ONLY: MULTIO_NEW
  USE :: PAR_UTILS_MOD,            ONLY: PAR_PRINT
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_VERSION
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_CURR_TIME
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_SYSINFO
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: OM_GETPID
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: OM_GET_HOSTNAME
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_START_SIMULATION


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
  CLASS(GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                         INTENT(IN)    :: YAMLFNAME
  TYPE(PROC_TOPO_T), TARGET,                INTENT(IN)    :: PROCESSOR_TOPO
  TYPE(MODEL_PAR_T), TARGET,                INTENT(IN)    :: MODEL_PARAMS

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

  ! Read the specific configuration for "DUMP" output manager
  CALL THIS%READ_CFG_FROM_YAML( CFG )

  ! Initialize enconding informations
  CALL SUENCODING_INFO( CFG, PROCESSOR_TOPO, MODEL_PARAMS, THIS%VERBOSE_ )

  ! Initialise all the encoders
  CALL MAKE_ENCODERS( CFG, MODEL_PARAMS, 'GRIB' )

  ! Destroy the fckit configuration object
  CALL CFG%FINAL()

  PP_LOG_DEVELOP_STR( 'Readed configuration file: '//TRIM(YAMLFNAME) )

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    CALL PROFILE_START_SIMULATION( THIS%PROFILE_DATA_, '.', PROCESSOR_TOPO%MYPROC_IO )
  ENDIF

  ! Initialize multio with specific plan file
  CALL MULTIO_NEW( THIS%MIO_, THIS%MULTIO_CONFIG_FILE_ )

  ! Initialise Multio metadata
  CALL THIS%MMD_%SET_MULTIO_HANDLE( THIS%MIO_ )

  ! Logging
  IF ( THIS%VERBOSE_ ) THEN
    THIS%LOG_FNAME_ = REPEAT(' ',LEN(THIS%LOG_FNAME_))
    WRITE(THIS%LOG_FNAME_,'(A,I8.8,A)', IOSTAT=STAT) GRIBX2MULTIO_RAW_OMNAME//'-output-manager-', PROCESSOR_TOPO%MYPROC_IO, '.log'
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write log filename' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to open log file' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point (on error)
  RETURN

END SUBROUTINE GRIBX2MULTIO_RAW_SETUP
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
#define PP_PROCEDURE_NAME 'GRIBX2MULTIO_RAW_WRITE_ATM_DP'
SUBROUTINE GRIBX2MULTIO_RAW_WRITE_ATM_DP( THIS, YDMSG, VALUES_DP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: JPRD_K
  USE :: OM_CORE_MOD,              ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD,              ONLY: TIME_HISTORY_T
  USE :: OM_CORE_MOD,              ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD,              ONLY: OM_SET_CURRENT_MESSAGE_ATM
  USE :: OM_CORE_MOD,              ONLY: OM_RESET_ENCODING_INFO
  USE :: ENCODING_INFO_MOD,        ONLY: ENCODING_INFO_ACCESS_OR_CREATE
  USE :: ENCODING_INFO_MOD,        ONLY: GRIB_INFO_PRINT
  USE :: ENCODING_INFO_MOD,        ONLY: TRACK_TIME_PRINT
  USE :: METADATA_FACTORY_MOD,     ONLY: METADATA_BASE_A
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: ENCODE_ATM
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_CURR_TIME
  USE :: MSG_UTILS_MOD,            ONLY: MSG_PRINT_ATM
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_MESSAGE
  USE :: OM_MULTIO_UTILS_MOD,      ONLY: MULTIO_INJECT_PARAMETERS
  USE :: OM_MULTIO_UTILS_MOD,      ONLY: MULTIO_WRITE_VALUES_DP

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_ATM_MSG_T),                               INTENT(IN)    :: YDMSG
  REAL(KIND=JPRD_K), DIMENSION(:),                  INTENT(IN)    :: VALUES_DP

  ! Local variables
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

  ! Get encoding info
  PP_LOG_DEVELOP_STR( 'Collect grib info of the current field' )
  CALL ENCODING_INFO_ACCESS_OR_CREATE( THIS%MODEL_PAR_, YDMSG%PARAM_ID_, YDMSG%IPREF_, &
&                                      YDMSG%IREPRES_,  YDMSG%IUID_, YDMSG%ISTEP_,     &
&                                      GRIB_INFO, TIME_HIST )

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

    ! Create multio metadata from a grib metadata
    CALL THIS%MMD_%INIT_FROM_METADATA( PGMD )

    ! Inject parameters in the metadata
    CALL MULTIO_INJECT_PARAMETERS( THIS%MODEL_PAR_, THIS%MMD_%GET_MULTIO_METADATA() )

    ! Write to multio plans
    CALL MULTIO_WRITE_VALUES_DP( THIS%MIO_, THIS%MMD_%GET_MULTIO_METADATA(), VALUES_DP )

  ENDIF


  ! Destroy the metadata objects
  IF ( THIS%GMD_%INITIALIZED() ) THEN
    IF ( THIS%SAVE_REPORT_ ) THEN
      PP_METADATA_FINALISE_LOGGING( PGMD )
    ENDIF
    CALL THIS%GMD_%DESTROY()
  ENDIF

  IF ( THIS%MMD_%INITIALIZED() ) THEN
    CALL THIS%MMD_%DESTROY()
  ENDIF

  ! Reset encdoing info
  CALL OM_RESET_ENCODING_INFO()

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Wrong size for the the values' )
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

END SUBROUTINE GRIBX2MULTIO_RAW_WRITE_ATM_DP
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
#define PP_PROCEDURE_NAME 'GRIBX2MULTIO_RAW_WRITE_ATM_SP'
SUBROUTINE GRIBX2MULTIO_RAW_WRITE_ATM_SP( THIS, YDMSG, VALUES_SP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: JPRM_K
  USE :: OM_CORE_MOD,              ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD,              ONLY: TIME_HISTORY_T
  USE :: OM_CORE_MOD,              ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD,              ONLY: OM_SET_CURRENT_MESSAGE_ATM
  USE :: OM_CORE_MOD,              ONLY: OM_RESET_ENCODING_INFO
  USE :: ENCODING_INFO_MOD,        ONLY: ENCODING_INFO_ACCESS_OR_CREATE
  USE :: ENCODING_INFO_MOD,        ONLY: GRIB_INFO_PRINT
  USE :: ENCODING_INFO_MOD,        ONLY: TRACK_TIME_PRINT
  USE :: METADATA_FACTORY_MOD,     ONLY: METADATA_BASE_A
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: ENCODE_ATM
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_CURR_TIME
  USE :: MSG_UTILS_MOD,            ONLY: MSG_PRINT_ATM
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_MESSAGE
  USE :: OM_MULTIO_UTILS_MOD,      ONLY: MULTIO_INJECT_PARAMETERS
  USE :: OM_MULTIO_UTILS_MOD,      ONLY: MULTIO_WRITE_VALUES_SP

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_ATM_MSG_T),                               INTENT(IN)    :: YDMSG
  REAL(KIND=JPRM_K), DIMENSION(:),                  INTENT(IN)    :: VALUES_SP

  ! Local variables
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
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES_SP).LT.YDMSG%NVALUES_, 1 )

  ! Associate the pointers to the metadata
  PGMD => THIS%GMD_
  PP_METADATA_INIT_LOGGING( PGMD, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%IPREF_, YDMSG%IREPRES_ )

  ! Get encoding info
  PP_LOG_DEVELOP_STR( 'Collect grib info of the current field' )
  CALL ENCODING_INFO_ACCESS_OR_CREATE( THIS%MODEL_PAR_, YDMSG%PARAM_ID_, YDMSG%IPREF_, &
&                                      YDMSG%IREPRES_,  YDMSG%IUID_, YDMSG%ISTEP_,     &
&                                      GRIB_INFO, TIME_HIST )

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

    ! Create multio metadata from a grib metadata
    CALL THIS%MMD_%INIT_FROM_METADATA( PGMD )

    ! Inject parameters in the metadata
    CALL MULTIO_INJECT_PARAMETERS( THIS%MODEL_PAR_, THIS%MMD_%GET_MULTIO_METADATA() )

    ! Write to multio plans
    CALL MULTIO_WRITE_VALUES_SP( THIS%MIO_, THIS%MMD_%GET_MULTIO_METADATA(), VALUES_SP )

  ENDIF


  ! Destroy the metadata objects
  IF ( THIS%GMD_%INITIALIZED() ) THEN
    IF ( THIS%SAVE_REPORT_ ) THEN
      PP_METADATA_FINALISE_LOGGING( PGMD )
    ENDIF
    CALL THIS%GMD_%DESTROY()
  ENDIF

  IF ( THIS%MMD_%INITIALIZED() ) THEN
    CALL THIS%MMD_%DESTROY()
  ENDIF

  ! Reset encdoing info
  CALL OM_RESET_ENCODING_INFO()

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Wrong size for the the values' )
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

END SUBROUTINE GRIBX2MULTIO_RAW_WRITE_ATM_SP
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
#define PP_PROCEDURE_NAME 'GRIBX2MULTIO_RAW_WRITE_WAM_DP'
SUBROUTINE GRIBX2MULTIO_RAW_WRITE_WAM_DP( THIS, YDMSG, VALUES_DP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: JPRD_K
  USE :: OM_CORE_MOD,              ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD,              ONLY: TIME_HISTORY_T
  USE :: OM_CORE_MOD,              ONLY: OM_WAM_MSG_T
  USE :: OM_CORE_MOD,              ONLY: OM_SET_CURRENT_MESSAGE_WAM
  USE :: OM_CORE_MOD,              ONLY: OM_RESET_ENCODING_INFO
  USE :: ENCODING_INFO_MOD,        ONLY: ENCODING_INFO_ACCESS_OR_CREATE
  USE :: ENCODING_INFO_MOD,        ONLY: GRIB_INFO_PRINT
  USE :: ENCODING_INFO_MOD,        ONLY: TRACK_TIME_PRINT
  USE :: METADATA_FACTORY_MOD,     ONLY: METADATA_BASE_A
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: ENCODE_WAM

  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_CURR_TIME
  USE :: MSG_UTILS_MOD,            ONLY: MSG_PRINT_WAM
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_MESSAGE
  USE :: OM_MULTIO_UTILS_MOD,      ONLY: MULTIO_INJECT_PARAMETERS
  USE :: OM_MULTIO_UTILS_MOD,      ONLY: MULTIO_WRITE_VALUES_DP

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_WAM_MSG_T),                               INTENT(IN)    :: YDMSG
  REAL(KIND=JPRD_K), DIMENSION(:),                  INTENT(IN)    :: VALUES_DP

  ! Local variables
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
  CALL OM_SET_CURRENT_MESSAGE_WAM( YDMSG, MSG_PRINT_WAM )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES_DP).LT.YDMSG%NVALUES_, 1 )

  ! Associate the pointers to the metadata
  PGMD => THIS%GMD_
  PP_METADATA_INIT_LOGGING( PGMD, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%IPREF_, YDMSG%IREPRES_ )


  ! Get encoding info
  PP_LOG_DEVELOP_STR( 'Collect grib info of the current field' )
  CALL ENCODING_INFO_ACCESS_OR_CREATE( THIS%MODEL_PAR_, YDMSG%PARAM_ID_, YDMSG%IPREF_, &
&                                      YDMSG%IREPRES_,  YDMSG%IUID_, YDMSG%ISTEP_,     &
&                                      GRIB_INFO, TIME_HIST )

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE ATMOSPHERE MESSAGE USING DOUBLE PRECISION VALUES' )
    CALL MSG_PRINT_WAM( YDMSG, THIS%LOG_UNIT_ )
    CALL GRIB_INFO_PRINT( GRIB_INFO, THIS%LOG_UNIT_ )
    CALL TRACK_TIME_PRINT( TIME_HIST, THIS%LOG_UNIT_ )
  ENDIF

  !
  ! Encode throws an error if an error happens, and return false if the field does not need to be emitted
  IF ( ENCODE_WAM( THIS%MODEL_PAR_, GRIB_INFO, TIME_HIST, YDMSG, PGMD ) ) THEN

    ! Create multio metadata from a grib metadata
    CALL THIS%MMD_%INIT_FROM_METADATA( PGMD )

    ! Inject parameters in the metadata
    CALL MULTIO_INJECT_PARAMETERS( THIS%MODEL_PAR_, THIS%MMD_%GET_MULTIO_METADATA() )

    ! Write to multio plans
    CALL MULTIO_WRITE_VALUES_DP( THIS%MIO_, THIS%MMD_%GET_MULTIO_METADATA(), VALUES_DP )

  ENDIF


  ! Destroy the metadata objects
  IF ( THIS%GMD_%INITIALIZED() ) THEN
    IF ( THIS%SAVE_REPORT_ ) THEN
      PP_METADATA_FINALISE_LOGGING( PGMD )
    ENDIF
    CALL THIS%GMD_%DESTROY()
  ENDIF

  IF ( THIS%MMD_%INITIALIZED() ) THEN
    CALL THIS%MMD_%DESTROY()
  ENDIF

  ! Reset encdoing info
  CALL OM_RESET_ENCODING_INFO()

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Wrong size for the the values' )
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

END SUBROUTINE GRIBX2MULTIO_RAW_WRITE_WAM_DP
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
#define PP_PROCEDURE_NAME 'GRIBX2MULTIO_RAW_WRITE_WAM_SP'
SUBROUTINE GRIBX2MULTIO_RAW_WRITE_WAM_SP( THIS, YDMSG, VALUES_SP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: JPRM_K
  USE :: OM_CORE_MOD,              ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD,              ONLY: TIME_HISTORY_T
  USE :: OM_CORE_MOD,              ONLY: OM_WAM_MSG_T
  USE :: OM_CORE_MOD,              ONLY: OM_SET_CURRENT_MESSAGE_WAM
  USE :: OM_CORE_MOD,              ONLY: OM_RESET_ENCODING_INFO
  USE :: ENCODING_INFO_MOD,        ONLY: ENCODING_INFO_ACCESS_OR_CREATE
  USE :: ENCODING_INFO_MOD,        ONLY: GRIB_INFO_PRINT
  USE :: ENCODING_INFO_MOD,        ONLY: TRACK_TIME_PRINT
  USE :: METADATA_FACTORY_MOD,     ONLY: METADATA_BASE_A
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: ENCODE_WAM
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_CURR_TIME
  USE :: MSG_UTILS_MOD,            ONLY: MSG_PRINT_WAM
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_MESSAGE
  USE :: OM_MULTIO_UTILS_MOD,      ONLY: MULTIO_INJECT_PARAMETERS
  USE :: OM_MULTIO_UTILS_MOD,      ONLY: MULTIO_WRITE_VALUES_SP

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_WAM_MSG_T),                               INTENT(IN)    :: YDMSG
  REAL(KIND=JPRM_K), DIMENSION(:),                  INTENT(IN)    :: VALUES_SP

  ! Local variables
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
  CALL OM_SET_CURRENT_MESSAGE_WAM( YDMSG, MSG_PRINT_WAM )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES_SP).LT.YDMSG%NVALUES_, 1 )

  ! Associate the pointers to the metadata
  PGMD => THIS%GMD_
  PP_METADATA_INIT_LOGGING( PGMD, YDMSG%ISTEP_, YDMSG%PARAM_ID_, YDMSG%IUID_, YDMSG%IPREF_, YDMSG%IREPRES_ )

  ! Get encoding info
  PP_LOG_DEVELOP_STR( 'Collect grib info of the current field' )
  CALL ENCODING_INFO_ACCESS_OR_CREATE( THIS%MODEL_PAR_, YDMSG%PARAM_ID_, YDMSG%IPREF_, &
&                                      YDMSG%IREPRES_,  YDMSG%IUID_, YDMSG%ISTEP_,     &
&                                      GRIB_INFO, TIME_HIST )

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE ATMOSPHERE MESSAGE USING DOUBLE PRECISION VALUES' )
    CALL MSG_PRINT_WAM( YDMSG, THIS%LOG_UNIT_ )
    CALL GRIB_INFO_PRINT( GRIB_INFO, THIS%LOG_UNIT_ )
    CALL TRACK_TIME_PRINT( TIME_HIST, THIS%LOG_UNIT_ )
  ENDIF

  !
  ! Encode throws an error if an error happens, and return false if the field does not need to be emitted
  IF ( ENCODE_WAM( THIS%MODEL_PAR_, GRIB_INFO, TIME_HIST, YDMSG, PGMD ) ) THEN

    ! Create multio metadata from a grib metadata
    CALL THIS%MMD_%INIT_FROM_METADATA( PGMD )

    ! Inject parameters in the metadata
    CALL MULTIO_INJECT_PARAMETERS( THIS%MODEL_PAR_, THIS%MMD_%GET_MULTIO_METADATA() )

    ! Write to multio plans
    CALL MULTIO_WRITE_VALUES_SP( THIS%MIO_, THIS%MMD_%GET_MULTIO_METADATA(), VALUES_SP )

  ENDIF


  ! Destroy the metadata objects
  IF ( THIS%GMD_%INITIALIZED() ) THEN
    IF ( THIS%SAVE_REPORT_ ) THEN
      PP_METADATA_FINALISE_LOGGING( PGMD )
    ENDIF
    CALL THIS%GMD_%DESTROY()
  ENDIF

  IF ( THIS%MMD_%INITIALIZED() ) THEN
    CALL THIS%MMD_%DESTROY()
  ENDIF

  ! Reset encdoing info
  CALL OM_RESET_ENCODING_INFO()

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Wrong size for the the values' )
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

END SUBROUTINE GRIBX2MULTIO_RAW_WRITE_WAM_SP
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
#define PP_PROCEDURE_NAME 'GRIBX2MULTIO_RAW_FLUSH_STEP'
SUBROUTINE GRIBX2MULTIO_RAW_FLUSH_STEP( THIS, KSTEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_MULTIO_UTILS_MOD,  ONLY: MULTIO_FLUSH
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,       ONLY: PROFILE_FLUSH

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                     INTENT(IN)    :: KSTEP

  ! Local variables
  CHARACTER(LEN=128) :: CLTMP

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

  ! Send to MultIO the flush for the last step
  CALL MULTIO_FLUSH( THIS%MIO_, KSTEP )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN
END SUBROUTINE GRIBX2MULTIO_RAW_FLUSH_STEP
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
#define PP_PROCEDURE_NAME 'GRIBX2MULTIO_RAW_FLUSH_LAST_STEP'
SUBROUTINE GRIBX2MULTIO_RAW_FLUSH_LAST_STEP( THIS, KSTEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_MULTIO_UTILS_MOD,  ONLY: MULTIO_FLUSH_LAST_STEP
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,       ONLY: PROFILE_FLUSH_LAST_STEP

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                     INTENT(IN)    :: KSTEP

  ! Local variables
  CHARACTER(LEN=128) :: CLTMP

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

  ! Send to MultIO the flush for the last step
  CALL MULTIO_FLUSH_LAST_STEP( THIS%MIO_, KSTEP )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()
  ! Exit point on success
  RETURN

END SUBROUTINE GRIBX2MULTIO_RAW_FLUSH_LAST_STEP
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
#define PP_PROCEDURE_NAME 'GRIBX2MULTIO_RAW_FLUSH_STEP_AND_TRIGGER_RESTART'
SUBROUTINE GRIBX2MULTIO_RAW_FLUSH_STEP_AND_TRIGGER_RESTART( THIS, KSTEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_MULTIO_UTILS_MOD,  ONLY: MULTIO_FLUSH_AND_TRIGGER_RESTART
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,       ONLY: PROFILE_FLUSH_AND_RESTART

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                     INTENT(IN)    :: KSTEP

  ! Local variables
  CHARACTER(LEN=128) :: CLTMP

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Profile
  IF ( THIS%PROFILE_ ) THEN
    CALL PROFILE_FLUSH_AND_RESTART( THIS%PROFILE_DATA_, KSTEP )
  ENDIF


  ! If needed log step and restart
  IF ( THIS%VERBOSE_ ) THEN
    CLTMP = REPEAT(' ',128)
    WRITE(CLTMP, '(I10)' ) KSTEP
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH STEP AND RESTART: '//TRIM(ADJUSTL(CLTMP)) )
  ENDIF

  ! Send to MultIO the flush for the last step
  CALL MULTIO_FLUSH_AND_TRIGGER_RESTART( THIS%MIO_, KSTEP )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE GRIBX2MULTIO_RAW_FLUSH_STEP_AND_TRIGGER_RESTART
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
#define PP_PROCEDURE_NAME 'GRIBX2MULTIO_RAW_FINALISE'
SUBROUTINE GRIBX2MULTIO_RAW_FINALISE( THIS )

  ! Symbols imported from other modules within the project.
  USE :: GRIB_ENCODER_MANAGER_MOD, ONLY: DESTROY_ENCODERS
  USE :: OM_MULTIO_UTILS_MOD,      ONLY: MULTIO_DELETE
  USE :: OM_MULTIO_UTILS_MOD,      ONLY: MULTIO_FLUSH_END_OF_SIMULATION
  USE :: ENCODING_INFO_MOD,        ONLY: ENCODING_INFO_FREE
  USE :: OM_GENERAL_UTILS_MOD,     ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_END_SIMULATION

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIBX2MULTIO_RAW_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS

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

  ! Send to MultIO the flush for the last step
  CALL MULTIO_FLUSH_END_OF_SIMULATION( THIS%MIO_ )
  CALL MULTIO_DELETE( THIS%MIO_ )
  CALL DESTROY_ENCODERS()
  CALL ENCODING_INFO_FREE()

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

END SUBROUTINE GRIBX2MULTIO_RAW_FINALISE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIBX2MULTIO_RAW_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
