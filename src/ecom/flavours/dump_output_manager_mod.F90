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
  USE :: OM_CORE_MOD,              ONLY: JPIB_K
  USE :: OM_CORE_MOD,              ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,              ONLY: MODEL_PAR_T
  USE :: OM_PROFILE_MOD,           ONLY: PROFILE_T
  USE :: OUTUPUT_MANAGER_BASE_MOD, ONLY: OUTPUT_MANAGER_BASE_A

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

  !> All the profiling data
  TYPE(PROFILE_T) :: PROFILE_DATA_

  !> Multiprocessor topology
  TYPE(PROC_TOPO_T), POINTER :: TOPOLOGY_ => NULL()

  !> Model parameters
  TYPE(MODEL_PAR_T), POINTER :: MODEL_PAR_ => NULL()

  !> Counter used to update the message file
  INTEGER(KIND=JPIB_K) :: CNT_ = -99

  !> Folder used to dump the files
  CHARACTER(LEN=1024)  :: OUT_DIR_ = REPEAT(' ',1024)

  !> Unit used to write toc
  INTEGER(KIND=JPIB_K) :: TOC_UNIT_ = -99_JPIB_K

  !> Unit used to write messages
  INTEGER(KIND=JPIB_K) :: MSG_UNIT_ = -99_JPIB_K

  !> Unit used to write values
  INTEGER(KIND=JPIB_K) :: VAL_UNIT_ = -99_JPIB_K

  !> Counter of the toc entries
  INTEGER(KIND=JPIB_K) :: TOC_COUNTER_ = -99_JPIB_K

  !> Write position in the toc
  INTEGER(KIND=JPIB_K) :: TOC_WRITPOS_ = -99_JPIB_K

  !> Enable profiling
  LOGICAL :: PROFILE_ = .FALSE.

  !> Enable verbose output for debug
  LOGICAL :: VERBOSE_ = .FALSE.

  !> If false values are not dumped
  LOGICAL :: WRITE_VALUES_ = .TRUE.

  !> Unit used for logging purposes (if needed)
  INTEGER(KIND=JPIB_K) :: LOG_UNIT_ = -99

  !> File used for logging purposes (if needed)
  CHARACTER(LEN=1024)  :: LOG_FNAME_ = REPEAT(' ',1024)

CONTAINS

  !> @brief Setup of the output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: SETUP => DUMP_SETUP

  !> @brief Read the configuration from YAML using fckit
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: READ_CFG_FROM_YAML => DUMP_READ_CFG_FROM_YAML

  !> @brief Write fields for the output manager
  !> @note Repeated code in order to be able to leverage polymorphis
  !>       and avoid multiple versions of the libraries
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

  END TYPE

  ! Whitelist of public symbols
  PUBLIC :: DUMP_OUTPUT_MANAGER_T
  PUBLIC :: DUMP_OMNAME

CONTAINS



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'DUMP_READ_CFG_FROM_YAML'
SUBROUTINE DUMP_READ_CFG_FROM_YAML( THIS, CFG )

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported from other modules within the project.
  USE :: OM_GENERAL_UTILS_MOD, ONLY: TOLOWER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(FCKIT_CONFIGURATION),    INTENT(IN)    :: CFG

  ! Local variables
  TYPE(FCKIT_CONFIGURATION) :: DUMP_CFG
  CHARACTER(LEN=:), ALLOCATABLE :: CLTMP
  LOGICAL :: LTMP
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialise
  THIS%OUT_DIR_ = REPEAT(' ',LEN(THIS%OUT_DIR_))

  ! PArse the configuration
  IF ( CFG%GET( TOLOWER(DUMP_OMNAME), DUMP_CFG ) ) THEN

    ! Read the dump directory from the YAML file
    IF ( ALLOCATED(CLTMP) ) DEALLOCATE(CLTMP)
    IF ( DUMP_CFG%GET( 'path', CLTMP  ) ) THEN
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CLTMP), 1 )
      PP_DEBUG_CRITICAL_COND_THROW( LEN(CLTMP).GT.LEN(THIS%OUT_DIR_), 2 )
      THIS%OUT_DIR_ = CLTMP(:)
      IF ( ALLOCATED(CLTMP) ) DEALLOCATE( CLTMP )
    ELSE
      THIS%OUT_DIR_ = './'
    ENDIF

    ! Read the verbose flag from YAML file
    IF ( DUMP_CFG%GET( 'verbose', LTMP  ) ) THEN
      THIS%VERBOSE_ = LTMP
    ELSE
      THIS%VERBOSE_ = .FALSE.
    ENDIF

    ! Read the verbose flag from YAML file
    IF ( DUMP_CFG%GET( 'profile', LTMP  ) ) THEN
      THIS%PROFILE_ = LTMP
    ELSE
      THIS%PROFILE_ = .FALSE.
    ENDIF

    ! Read the verbose flag from YAML file
    IF ( DUMP_CFG%GET( 'dump-values', LTMP  ) ) THEN
      THIS%WRITE_VALUES_ = LTMP
    ELSE
      THIS%WRITE_VALUES_ = .FALSE.
    ENDIF

    ! Deallocate the dump-output-manager object
    CALL DUMP_CFG%FINAL()

  ELSE
    ! Deallocate the dump-output-manager object (paranoid)
    CALL DUMP_CFG%FINAL()
    THIS%OUT_DIR_ = './'
    THIS%VERBOSE_ = .FALSE.
    THIS%WRITE_VALUES_ = .FALSE.
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Fields file name too long: "'//TRIM(ADJUSTL(THIS%OUT_DIR_))//'"' )
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

END SUBROUTINE DUMP_READ_CFG_FROM_YAML
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
#define PP_PROCEDURE_NAME 'DUMP_SETUP'
SUBROUTINE DUMP_SETUP( THIS, YAMLFNAME, PROCESSOR_TOPO, MODEL_PARAMS )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,          ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,          ONLY: OM_INIT_DEBUG_VARS
  USE :: PAR_UTILS_MOD,        ONLY: PAR_CREATE_NAME
  USE :: PAR_UTILS_MOD,        ONLY: PAR_WOPEN
  USE :: PAR_UTILS_MOD,        ONLY: PAR_WRITE
  USE :: PAR_UTILS_MOD,        ONLY: PAR_CLOSE
  USE :: PAR_UTILS_MOD,        ONLY: PAR_PRINT
  USE :: MSG_UTILS_MOD,        ONLY: MSG_CREATE_NAME
  USE :: MSG_UTILS_MOD,        ONLY: MSG_WOPEN
  USE :: VAL_UTILS_MOD,        ONLY: VAL_CREATE_NAME
  USE :: VAL_UTILS_MOD,        ONLY: VAL_WOPEN
  USE :: TOC_UTILS_MOD,        ONLY: TOC_CREATE_NAME
  USE :: TOC_UTILS_MOD,        ONLY: TOC_WOPEN
  USE :: TOC_UTILS_MOD,        ONLY: TOC_WRITE_FLUSH_BEGIN_OF_SIMULATION
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_VERSION
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_SYSINFO
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_GETPID
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_GET_HOSTNAME
  USE :: OM_PROFILE_MOD,       ONLY: PROFILE_START_SIMULATION
  USE :: OM_GENERAL_UTILS_MOD, ONLY: TOLOWER

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
  CLASS(DUMP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: YAMLFNAME
  TYPE(PROC_TOPO_T), TARGET,    INTENT(IN)    :: PROCESSOR_TOPO
  TYPE(MODEL_PAR_T), TARGET,    INTENT(IN)    :: MODEL_PARAMS

  ! Local variables
  CHARACTER(LEN=256) :: HOSTNAME
  TYPE(FCKIT_CONFIGURATION) :: CFG
  LOGICAL :: EX
  CHARACTER(LEN=128)   :: FNAME
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: UNIT
  INTEGER(KIND=JPIB_K) :: PAR_UNIT
  INTEGER(KIND=JPIB_K) :: PID
  CHARACTER(LEN=1024) :: MSG_FNAME
  CHARACTER(LEN=1024) :: VAL_FNAME
  CHARACTER(LEN=1024) :: PAR_FNAME
  CHARACTER(LEN=1024) :: TOC_FNAME

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

  ! Read the specific configuration for "DUMP_OUTPUT_MANAGER" output manager
  CALL THIS%READ_CFG_FROM_YAML( CFG )

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
    WRITE(THIS%LOG_FNAME_,'(A,I8.8,A)', IOSTAT=STAT) TOLOWER(DUMP_OMNAME)//'-output-manager-', PROCESSOR_TOPO%MYPROC_IO, '.log'
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2 )
    OPEN( FILE=TRIM(THIS%LOG_FNAME_), NEWUNIT=THIS%LOG_UNIT_, ACTION='WRITE', STATUS='REPLACE', IOSTAT=STAT )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3 )
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'SETUP OF THE OUTPUT MANAGER' )
    CALL LOG_VERSION( THIS%LOG_UNIT_ )
    CALL LOG_SYSINFO( THIS%LOG_UNIT_, PROCESSOR_TOPO%NPROC_IO, PROCESSOR_TOPO%MYPROC_IO )
    CALL PAR_PRINT( MODEL_PARAMS, THIS%LOG_UNIT_ )
  ENDIF

  ! Parameters
  CALL PAR_CREATE_NAME( THIS%OUT_DIR_, PROCESSOR_TOPO%MYPROC_IO, PAR_FNAME )
  CALL PAR_WOPEN( TRIM(PAR_FNAME), PAR_UNIT )
  CALL PAR_WRITE( MODEL_PARAMS, PAR_UNIT )
  CALL PAR_CLOSE( PAR_UNIT )

  ! Messages
  THIS%CNT_ = 0
  CALL MSG_CREATE_NAME( THIS%OUT_DIR_, THIS%CNT_, PROCESSOR_TOPO%MYPROC_IO, MSG_FNAME )
  CALL MSG_WOPEN( TRIM(MSG_FNAME), THIS%MSG_UNIT_ )

  ! Values
  IF ( THIS%WRITE_VALUES_ ) THEN
    CALL VAL_CREATE_NAME( THIS%OUT_DIR_, THIS%CNT_, PROCESSOR_TOPO%MYPROC_IO, VAL_FNAME )
    CALL VAL_WOPEN( TRIM(VAL_FNAME), THIS%VAL_UNIT_ )
  ENDIF

  ! Create the toc file
  CALL TOC_CREATE_NAME( THIS%OUT_DIR_, PROCESSOR_TOPO%MYPROC_IO, TOC_FNAME )
  CALL TOC_WOPEN( TRIM(TOC_FNAME), THIS%TOC_UNIT_, THIS%TOC_WRITPOS_, THIS%TOC_COUNTER_ )

  ! Add flush begin of simulation to toc
  CALL TOC_WRITE_FLUSH_BEGIN_OF_SIMULATION( THIS%TOC_UNIT_, THIS%TOC_WRITPOS_, THIS%TOC_COUNTER_ )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to find YAML main configuration file: "'//TRIM(ADJUSTL(YAMLFNAME))//'"' )
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

END SUBROUTINE DUMP_SETUP
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
#define PP_PROCEDURE_NAME 'DUMP_WRITE_ATM_DP'
SUBROUTINE DUMP_WRITE_ATM_DP( THIS, YDMSG, VALUES_DP )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_CORE_MOD,          ONLY: JPRD_K
  USE :: OM_CORE_MOD,          ONLY: VALUES_DP_E
  USE :: OM_CORE_MOD,          ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD,          ONLY: OM_SET_CURRENT_MESSAGE_ATM
  USE :: TOC_UTILS_MOD,        ONLY: TOC_WRITE_ATM
  USE :: MSG_UTILS_MOD,        ONLY: MSG_WRITE_ATM
  USE :: MSG_UTILS_MOD,        ONLY: MSG_PRINT_ATM
  USE :: VAL_UTILS_MOD,        ONLY: VAL_WRITE_DP
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,       ONLY: PROFILE_MESSAGE

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

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  INTEGER(KIND=INT64) :: MSGADDR
  INTEGER(KIND=INT64) :: VALADDR

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

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE ATMOSPHERE MESSAGE USING DOUBLE PRECISION VALUES' )
    CALL MSG_PRINT_ATM( YDMSG, THIS%LOG_UNIT_ )
  ENDIF

  ! Write message
  CALL MSG_WRITE_ATM( THIS%MSG_UNIT_, YDMSG, MSGADDR )

  ! Write values
  IF ( THIS%WRITE_VALUES_ ) THEN
    CALL VAL_WRITE_DP( THIS%VAL_UNIT_, VALUES_DP, VALADDR )
  ELSE
    ! Negative address means no value saved
    VALADDR = -1_INT64
  ENDIF

  ! Update toc
  LO = LBOUND(VALUES_DP,1)
  HI = UBOUND(VALUES_DP,1)
  CALL TOC_WRITE_ATM(                 &
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
&           THIS%TOC_COUNTER_         &
&  )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unexpected message type' )
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

END SUBROUTINE DUMP_WRITE_ATM_DP
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
#define PP_PROCEDURE_NAME 'DUMP_WRITE_ATM_SP'
SUBROUTINE DUMP_WRITE_ATM_SP( THIS, YDMSG, VALUES_SP )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,   ONLY: JPIB_K
  USE :: OM_CORE_MOD,   ONLY: JPIB_K
  USE :: OM_CORE_MOD,   ONLY: JPRM_K
  USE :: OM_CORE_MOD,   ONLY: VALUES_SP_E
  USE :: OM_CORE_MOD,   ONLY: OM_ATM_MSG_T
  USE :: OM_CORE_MOD,   ONLY: OM_SET_CURRENT_MESSAGE_ATM
  USE :: TOC_UTILS_MOD, ONLY: TOC_WRITE_ATM
  USE :: MSG_UTILS_MOD, ONLY: MSG_WRITE_ATM
  USE :: MSG_UTILS_MOD, ONLY: MSG_PRINT_ATM
  USE :: VAL_UTILS_MOD, ONLY: VAL_WRITE_SP
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,       ONLY: PROFILE_MESSAGE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_ATM_MSG_T),                   INTENT(IN)    :: YDMSG
  REAL(KIND=JPRM_K), DIMENSION(:),      INTENT(IN)    :: VALUES_SP

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  INTEGER(KIND=INT64) :: MSGADDR
  INTEGER(KIND=INT64) :: VALADDR

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

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE ATMOSPHERE MESSAGE USING SINGLE PRECISION VALUES' )
    CALL MSG_PRINT_ATM( YDMSG, THIS%LOG_UNIT_ )
  ENDIF

  ! Write message
  CALL MSG_WRITE_ATM( THIS%MSG_UNIT_, YDMSG, MSGADDR )

  ! Write values
  IF ( THIS%WRITE_VALUES_ ) THEN
    CALL VAL_WRITE_SP( THIS%VAL_UNIT_, VALUES_SP, VALADDR )
  ELSE
    ! Negative address means no value saved
    VALADDR = -1_INT64
  ENDIF

  ! Update toc
  LO = LBOUND(VALUES_SP,1)
  HI = UBOUND(VALUES_SP,1)
  CALL TOC_WRITE_ATM(                 &
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
&           THIS%TOC_COUNTER_         &
&  )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unexpected message type' )
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

END SUBROUTINE DUMP_WRITE_ATM_SP
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
#define PP_PROCEDURE_NAME 'DUMP_WRITE_WAM_DP'
SUBROUTINE DUMP_WRITE_WAM_DP( THIS, YDMSG, VALUES_DP )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,   ONLY: JPIB_K
  USE :: OM_CORE_MOD,   ONLY: JPIB_K
  USE :: OM_CORE_MOD,   ONLY: JPRD_K
  USE :: OM_CORE_MOD,   ONLY: VALUES_DP_E
  USE :: OM_CORE_MOD,   ONLY: OM_WAM_MSG_T
  USE :: OM_CORE_MOD,   ONLY: OM_SET_CURRENT_MESSAGE_WAM
  USE :: TOC_UTILS_MOD, ONLY: TOC_WRITE_WAM
  USE :: MSG_UTILS_MOD, ONLY: MSG_WRITE_WAM
  USE :: MSG_UTILS_MOD, ONLY: MSG_PRINT_WAM
  USE :: VAL_UTILS_MOD, ONLY: VAL_WRITE_DP
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,       ONLY: PROFILE_MESSAGE

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

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  INTEGER(KIND=INT64) :: MSGADDR
  INTEGER(KIND=INT64) :: VALADDR

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

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE WAVE MESSAGE USING DOUBLE PRECISION VALUES' )
    CALL MSG_PRINT_WAM( YDMSG, THIS%LOG_UNIT_ )
  ENDIF

  ! Write message
  CALL MSG_WRITE_WAM( THIS%MSG_UNIT_, YDMSG, MSGADDR )

  ! Write values
  IF ( THIS%WRITE_VALUES_ ) THEN
    CALL VAL_WRITE_DP( THIS%VAL_UNIT_, VALUES_DP, VALADDR )
  ELSE
    ! Negative address means no value saved
    VALADDR = -1_INT64
  ENDIF

  ! Update toc
  LO = LBOUND(VALUES_DP,1)
  HI = UBOUND(VALUES_DP,1)
  CALL TOC_WRITE_WAM(                 &
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
&           THIS%TOC_COUNTER_         &
&  )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unexpected message type' )
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

END SUBROUTINE DUMP_WRITE_WAM_DP
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
#define PP_PROCEDURE_NAME 'DUMP_WRITE_WAM_SP'
SUBROUTINE DUMP_WRITE_WAM_SP( THIS, YDMSG, VALUES_SP )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,   ONLY: JPIB_K
  USE :: OM_CORE_MOD,   ONLY: JPIB_K
  USE :: OM_CORE_MOD,   ONLY: JPRM_K
  USE :: OM_CORE_MOD,   ONLY: VALUES_SP_E
  USE :: OM_CORE_MOD,   ONLY: OM_WAM_MSG_T
  USE :: OM_CORE_MOD,   ONLY: OM_SET_CURRENT_MESSAGE_WAM
  USE :: TOC_UTILS_MOD, ONLY: TOC_WRITE_WAM
  USE :: MSG_UTILS_MOD, ONLY: MSG_WRITE_WAM
  USE :: MSG_UTILS_MOD, ONLY: MSG_PRINT_WAM
  USE :: VAL_UTILS_MOD, ONLY: VAL_WRITE_SP
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,       ONLY: PROFILE_MESSAGE

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

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  INTEGER(KIND=INT64)  :: MSGADDR
  INTEGER(KIND=INT64)  :: VALADDR

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

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'WRITE WAVE MESSAGE USING SINGLE PRECISION VALUES' )
    CALL MSG_PRINT_WAM( YDMSG, THIS%LOG_UNIT_ )
  ENDIF

  ! Write message
  CALL MSG_WRITE_WAM( THIS%MSG_UNIT_, YDMSG, MSGADDR )

  ! Write values
  IF ( THIS%WRITE_VALUES_ ) THEN
    CALL VAL_WRITE_SP( THIS%VAL_UNIT_, VALUES_SP, VALADDR )
  ELSE
    ! Negative address means no value saved
    VALADDR = -1_INT64
  ENDIF

  ! Update toc
  LO = LBOUND(VALUES_SP,1)
  HI = UBOUND(VALUES_SP,1)
  CALL TOC_WRITE_WAM(                 &
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
&           THIS%TOC_COUNTER_         &
&  )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unexpected message type' )
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

END SUBROUTINE DUMP_WRITE_WAM_SP
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
#define PP_PROCEDURE_NAME 'DUMP_FLUSH_STEP'
SUBROUTINE DUMP_FLUSH_STEP( THIS, KSTEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: TOC_UTILS_MOD,        ONLY: TOC_WRITE_FLUSH_STEP
  USE :: VAL_UTILS_MOD,        ONLY: VAL_CLOSE
  USE :: VAL_UTILS_MOD,        ONLY: VAL_CREATE_NAME
  USE :: VAL_UTILS_MOD,        ONLY: VAL_WOPEN
  USE :: MSG_UTILS_MOD,        ONLY: MSG_CLOSE
  USE :: MSG_UTILS_MOD,        ONLY: MSG_CREATE_NAME
  USE :: MSG_UTILS_MOD,        ONLY: MSG_WOPEN
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
  CLASS(DUMP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: KSTEP

  ! Local variables
  CHARACTER(LEN=128) :: CLTMP
  CHARACTER(LEN=1024) :: MSG_FNAME
  CHARACTER(LEN=1024) :: VAL_FNAME

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

  ! Messages
  THIS%CNT_ = THIS%CNT_ + 1
  CALL MSG_CLOSE( THIS%MSG_UNIT_ )
  CALL MSG_CREATE_NAME( THIS%OUT_DIR_, THIS%CNT_, THIS%TOPOLOGY_%MYPROC_IO, MSG_FNAME )
  CALL MSG_WOPEN( TRIM(MSG_FNAME), THIS%MSG_UNIT_ )


  ! Values
  IF ( THIS%WRITE_VALUES_ ) THEN
    CALL VAL_CLOSE( THIS%VAL_UNIT_ )
    CALL VAL_CREATE_NAME( THIS%OUT_DIR_, THIS%CNT_, THIS%TOPOLOGY_%MYPROC_IO, VAL_FNAME )
    CALL VAL_WOPEN( TRIM(VAL_FNAME), THIS%VAL_UNIT_ )
  ENDIF


  ! Update toc
  CALL TOC_WRITE_FLUSH_STEP( THIS%TOC_UNIT_, KSTEP, THIS%TOC_WRITPOS_, THIS%TOC_COUNTER_ )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN
END SUBROUTINE DUMP_FLUSH_STEP
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
#define PP_PROCEDURE_NAME 'DUMP_FLUSH_LAST_STEP'
SUBROUTINE DUMP_FLUSH_LAST_STEP( THIS, KSTEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: TOC_UTILS_MOD,        ONLY: TOC_WRITE_FLUSH_LAST_STEP
  USE :: VAL_UTILS_MOD,        ONLY: VAL_CLOSE
  USE :: MSG_UTILS_MOD,        ONLY: MSG_CLOSE
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,       ONLY: PROFILE_FLUSH_LAST_STEP

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

  ! Local variables
  CHARACTER(LEN=128) :: CLTMP

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

  ! Messages
  CALL MSG_CLOSE( THIS%MSG_UNIT_ )

  ! Values
  IF ( THIS%WRITE_VALUES_ ) THEN
    CALL VAL_CLOSE( THIS%VAL_UNIT_ )
  ENDIF

  ! Update toc
  CALL TOC_WRITE_FLUSH_LAST_STEP( THIS%TOC_UNIT_, KSTEP, THIS%TOC_WRITPOS_, THIS%TOC_COUNTER_ )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE DUMP_FLUSH_LAST_STEP
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
#define PP_PROCEDURE_NAME 'DUMP_FLUSH_STEP_AND_TRIGGER_RESTART'
SUBROUTINE DUMP_FLUSH_STEP_AND_TRIGGER_RESTART( THIS, KSTEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: TOC_UTILS_MOD,        ONLY: TOC_WRITE_FLUSH_STEP_AND_RESTART
  USE :: VAL_UTILS_MOD,        ONLY: VAL_CLOSE
  USE :: VAL_UTILS_MOD,        ONLY: VAL_CREATE_NAME
  USE :: VAL_UTILS_MOD,        ONLY: VAL_WOPEN
  USE :: MSG_UTILS_MOD,        ONLY: MSG_CLOSE
  USE :: MSG_UTILS_MOD,        ONLY: MSG_CREATE_NAME
  USE :: MSG_UTILS_MOD,        ONLY: MSG_WOPEN
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,       ONLY: PROFILE_FLUSH_AND_RESTART

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

  ! Local variables
  CHARACTER(LEN=128) :: CLTMP
  CHARACTER(LEN=1024) :: MSG_FNAME
  CHARACTER(LEN=1024) :: VAL_FNAME

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
    CALL PROFILE_FLUSH_AND_RESTART( THIS%PROFILE_DATA_, KSTEP )
  ENDIF

  ! If needed log step and restart
  IF ( THIS%VERBOSE_ ) THEN
    CLTMP = REPEAT(' ',128)
    WRITE(CLTMP, '(I10)' ) KSTEP
    CALL LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH STEP AND RESTART: '//TRIM(ADJUSTL(CLTMP)) )
  ENDIF


  ! Messages
  THIS%CNT_ = THIS%CNT_ + 1
  CALL MSG_CLOSE( THIS%MSG_UNIT_ )
  CALL MSG_CREATE_NAME( THIS%OUT_DIR_, THIS%CNT_, THIS%TOPOLOGY_%MYPROC_IO, MSG_FNAME )
  CALL MSG_WOPEN( TRIM(MSG_FNAME), THIS%MSG_UNIT_ )


  ! Values
  IF ( THIS%WRITE_VALUES_ ) THEN
    CALL VAL_CLOSE( THIS%VAL_UNIT_ )
    CALL VAL_CREATE_NAME( THIS%OUT_DIR_, THIS%CNT_, THIS%TOPOLOGY_%MYPROC_IO, VAL_FNAME )
    CALL VAL_WOPEN( TRIM(VAL_FNAME), THIS%VAL_UNIT_ )
  ENDIF


  ! Update toc
  CALL TOC_WRITE_FLUSH_STEP_AND_RESTART( THIS%TOC_UNIT_, KSTEP, THIS%TOC_WRITPOS_, THIS%TOC_COUNTER_ )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE DUMP_FLUSH_STEP_AND_TRIGGER_RESTART
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
#define PP_PROCEDURE_NAME 'DUMP_FINALISE'
SUBROUTINE DUMP_FINALISE( THIS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: TOC_UTILS_MOD,        ONLY: TOC_WRITE_FLUSH_END_OF_SIMULATION
  USE :: OM_GENERAL_UTILS_MOD, ONLY: LOG_CURR_TIME
  USE :: OM_PROFILE_MOD,       ONLY: PROFILE_END_SIMULATION

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(DUMP_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS

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

  ! Update toc
  CALL TOC_WRITE_FLUSH_END_OF_SIMULATION( THIS%TOC_UNIT_, THIS%TOC_WRITPOS_, THIS%TOC_COUNTER_ )

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

END SUBROUTINE DUMP_FINALISE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE DUMP_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
