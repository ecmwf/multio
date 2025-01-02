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
#define PP_FILE_NAME 'grib_header2multio_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_HEADER2MULTIO_MOD'
MODULE GRIB_HEADER2MULTIO_MOD

  ! Symbols imported from other modules within the project.
  USE :: OUTUPUT_MANAGER_BASE_MOD,   ONLY: OUTPUT_MANAGER_BASE_A
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,            ONLY: UNDEF_PARAM_E
  USE :: IFS_PAR_MOD,                ONLY: PROC_TOPO_T
  USE :: IFS_PAR_MOD,                ONLY: MODEL_PAR_T
  USE :: PROFILE_MOD,                ONLY: PROFILE_T
  USE :: GRIB_METADATA_MOD,          ONLY: GRIB_METADATA_T
  USE :: MULTIO_METADATA_MOD,        ONLY: MULTIO_METADATA_T
  USE :: MULTIOM_CACHED_ENCODER_MOD, ONLY: MULTIOM_CACHED_ENCODERS_T
  USE :: METADATA_BASE_MOD,          ONLY: METADATA_BASE_A
  USE :: REPRESENTATIONS_MOD,        ONLY: REPRES_A

  ! Symbols imported from other libraries
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE

IMPLICIT NONE

! Default visibility
PRIVATE

!> Output manager name
CHARACTER(LEN=*), PARAMETER :: GRIB_HEADER2MULTIO_OMNAME='GRIB-HEADER-TO-MULTIO'

!>
!> @brief Definition of the `GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T` derived type.
!>
!> The `GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T` type is a derived type that extends the
!> functionality of the `OUTPUT_MANAGER_BASE_A` abstract interface to be
!> used to demonstrate behavior without I/O when addressing complaints
!> or testing scenarios.
!>
!> @see OUTPUT_MANAGER_BASE_A
!>
TYPE, EXTENDS(OUTPUT_MANAGER_BASE_A) :: GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T

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

  !> Metadata object used for the encoding (grib)
  TYPE(GRIB_METADATA_T)   :: GMD_

  !> Metadata object used for the encoding (multio)
  TYPE(MULTIO_METADATA_T) :: MMD_

  !> Representations map
  CLASS(REPRES_A), POINTER :: REPRES_SH_ => NULL()
  CLASS(REPRES_A), POINTER :: REPRES_GG_ => NULL()
  CHARACTER(LEN=8) :: GG_NAME=REPEAT(' ',8)
  CHARACTER(LEN=8) :: SH_NAME=REPEAT(' ',8)

  !> Configurations
  CHARACTER(LEN=1024) :: MULTIO_PLANS_FILE_ = REPEAT(' ',1024)
  CHARACTER(LEN=1024) :: MAPPING_FILE_ = REPEAT(' ',1024)
  CHARACTER(LEN=1024) :: ENCODER_FILE_ = REPEAT(' ',1024)
  CHARACTER(LEN=1024) :: SAMPLES_PATH_ = REPEAT(' ',1024)
  CHARACTER(LEN=1024) :: ECCODES_SAMPLES_PATH_ = REPEAT(' ',1024)
  CHARACTER(LEN=1024) :: LOG_PATH_ = REPEAT(' ',1024)
  LOGICAL :: PROFILE_ = .FALSE.
  LOGICAL :: SAVE_REPORT_ = .FALSE.
  LOGICAL :: VERBOSE_ = .FALSE.


  !> Unit used for logging purposes (if needed)
  INTEGER(KIND=JPIB_K) :: LOG_UNIT_ = -99

  !> File used for logging purposes (if needed)
  CHARACTER(LEN=1024)  :: LOG_FNAME_ = REPEAT(' ',1024)

  !> Binary file
  CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE :: MESSAGE_DATA_

  !> Multio Handle used to interact with multio
  TYPE(MULTIO_HANDLE) :: MIO_


  !> Metadata object used for the encoding (multio)
  CLASS(METADATA_BASE_A), POINTER :: METADATA_ => NULL()

  !> Cached encoders
  TYPE(MULTIOM_CACHED_ENCODERS_T) :: MULTIOM_ENCODER_

CONTAINS

  !> @brief Setup of the output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: SETUP => GRIB_HEADER2MULTIO_SETUP

  !> @brief Write fields for the output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_ATM_DP => GRIB_HEADER2MULTIO_WRITE_ATM_DP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_ATM_SP => GRIB_HEADER2MULTIO_WRITE_ATM_SP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_WAM_DP => GRIB_HEADER2MULTIO_WRITE_WAM_DP
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: WRITE_WAM_SP => GRIB_HEADER2MULTIO_WRITE_WAM_SP

  !> @brief Notify that a step is finished
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_STEP => GRIB_HEADER2MULTIO_FLUSH_STEP

  !> @brief Notify that a step is finished and it was the last step
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_LAST_STEP => GRIB_HEADER2MULTIO_FLUSH_LAST_STEP

  !> @brief Notify that a step is finished and it is necessary to dump a restart
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FLUSH_STEP_AND_TRIGGER_RESTART => GRIB_HEADER2MULTIO_FLUSH_STEP_AND_TRIGGER_RESTART

  !> @brief Finalise ancd cleanup teh output manager
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FINALISE => GRIB_HEADER2MULTIO_FINALISE


  !> @brief Read the configuration from YAML using fckit
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_CFG_FROM_YAML => GRIB_HEADER2MULTIO_READ_CFG_FROM_YAML

  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_SAMPLE_PATH_FROM_YAML => GRIB_HEADER2MULTIO_READ_SAMPLES_PATH_FROM_YAML
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_LOG_PATH_FROM_YAML => GRIB_HEADER2MULTIO_READ_LOG_PATH_FROM_YAML

  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_MULTIO_PLANS_FILE_FROM_YAML => GRIB_HEADER2MULTIO_READ_MULTIO_PLANS_FILE_FROM_YAML
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_MAPPING_FILE_FROM_YAML      => GRIB_HEADER2MULTIO_READ_MAPPING_FILE_FROM_YAML
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_ENCODER_FILE_FROM_YAML      => GRIB_HEADER2MULTIO_READ_ENCODER_FILE_FROM_YAML


  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_VERBOSE_FROM_YAML => GRIB_HEADER2MULTIO_READ_VERBOSE_FROM_YAML
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_PROFILE_FROM_YAML => GRIB_HEADER2MULTIO_READ_PROFILE_FROM_YAML
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: READ_REPORT_FROM_YAML => GRIB_HEADER2MULTIO_READ_SAVE_REPORT_FROM_YAML
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: INIT_GEOMETRY_MAP => GRIB_HEADER2MULTIO_INIT_GEOMETRY_MAP

  !> @brief Extract mar dictionary and context/parametrization from the ifs data-structures
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: GET_MARS_FROM_ATM => GRIB_HEADER2MULTIO_GET_MARS_FROM_ATM
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: GET_MARS_FROM_WAM => GRIB_HEADER2MULTIO_GET_MARS_FROM_WAM

  !> @brief Sink the data to the selected sink
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: SINK_DP => GRIB_HEADER2MULTIO_READ_SINK_DP
  PROCEDURE, NON_OVERRIDABLE, PASS, PRIVATE :: SINK_SP => GRIB_HEADER2MULTIO_READ_SINK_SP

  END TYPE

  ! Whitelist of public symbols
  PUBLIC :: GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T
  PUBLIC :: GRIB_HEADER2MULTIO_OMNAME

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_READ_CFG_FROM_YAML'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_READ_CFG_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(YAML_CONFIGURATION_T) :: GRIB_HEADER2MULTIO_CFG
  CHARACTER(LEN=LEN(GRIB_HEADER2MULTIO_OMNAME)) :: GRIB_HEADER2MULTIO_OMNAME_LC
  LOGICAL :: HAS_SUBKEY
  LOGICAL :: HAS_KEY

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TO_LOWER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HAS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_SUBCFG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SAMPLE_PATH=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_LOG_PATH=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_MULTIO_PLANS_FILE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_MAPPING_FILE=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_ENCODER_PATH=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_VERBOSE=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_PROFILE=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_REPORT=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_CFG=12_JPIB_K

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
  GRIB_HEADER2MULTIO_OMNAME_LC = REPEAT(' ',LEN(GRIB_HEADER2MULTIO_OMNAME_LC))
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_LOWER) TOLOWER( GRIB_HEADER2MULTIO_OMNAME, GRIB_HEADER2MULTIO_OMNAME_LC, HOOKS )

  ! Get sub-configuration from input file
  PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, GRIB_HEADER2MULTIO_OMNAME_LC, HAS_KEY, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.HAS_KEY, ERRFLAG_HAS_KEY )

  ! Get the specific sub-configuration
  PP_TRYCALL(ERRFLAG_GET_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, GRIB_HEADER2MULTIO_OMNAME_LC, GRIB_HEADER2MULTIO_CFG, HOOKS )

  ! Read options from configuration file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SAMPLE_PATH) THIS%READ_SAMPLE_PATH_FROM_YAML( GRIB_HEADER2MULTIO_CFG, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_LOG_PATH) THIS%READ_LOG_PATH_FROM_YAML( GRIB_HEADER2MULTIO_CFG, HOOKS )

  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_MULTIO_PLANS_FILE) THIS%READ_MULTIO_PLANS_FILE_FROM_YAML( GRIB_HEADER2MULTIO_CFG, HOOKS )

  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_MAPPING_FILE) THIS%READ_MAPPING_FILE_FROM_YAML( GRIB_HEADER2MULTIO_CFG, HOOKS )

  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_ENCODER_PATH) THIS%READ_ENCODER_FILE_FROM_YAML( GRIB_HEADER2MULTIO_CFG, HOOKS )

  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_VERBOSE) THIS%READ_VERBOSE_FROM_YAML( GRIB_HEADER2MULTIO_CFG, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_PROFILE) THIS%READ_PROFILE_FROM_YAML( GRIB_HEADER2MULTIO_CFG, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_PROFILE) THIS%READ_REPORT_FROM_YAML( GRIB_HEADER2MULTIO_CFG, HOOKS )

  ! Deallocate the dump-output-manager object
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DESTROY_CFG) YAML_DELETE_CONFIGURATION( GRIB_HEADER2MULTIO_CFG, HOOKS )


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
    CASE (ERRFLAG_UNABLE_TO_READ_SAMPLE_PATH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the sample path from the configuration file' )
    CASE (ERRFLAG_UNABLE_TO_READ_LOG_PATH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the log path from the configuration file' )
    CASE (ERRFLAG_UNABLE_TO_READ_MULTIO_PLANS_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the multio plans file from the configuration file' )
    CASE (ERRFLAG_UNABLE_TO_READ_MAPPING_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the mapping file from the configuration file' )
    CASE (ERRFLAG_UNABLE_TO_READ_ENCODER_PATH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the encoder file from the configuration file' )
    CASE (ERRFLAG_UNABLE_TO_READ_VERBOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the verbose flag from the configuration file' )
    CASE (ERRFLAG_UNABLE_TO_READ_PROFILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the profile flag from the configuration file' )
    CASE (ERRFLAG_UNABLE_TO_READ_REPORT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the report flag from the configuration file' )
    CASE (ERRFLAG_UNABLE_TO_DESTROY_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to destroy the configuration object' )
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

END FUNCTION GRIB_HEADER2MULTIO_READ_CFG_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_READ_SAMPLES_PATH_FROM_YAML'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_READ_SAMPLES_PATH_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIM_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING_WITH_ENV_EXPANSION

  ! Symbols imported from other libraries
  USE :: GRIB_API, ONLY: GRIB_SET_SAMPLES_PATH
  USE :: GRIB_API, ONLY: GRIB_SUCCESS
  USE :: GRIB_API, ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),                 INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: HAS_KEY
  CHARACTER(LEN=:), ALLOCATABLE :: SAMPLES_PATH
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIM_K) :: KRET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HAS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_SUBCFG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED_AFTER_READ=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOO_LONG=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_UNABLE_TO_SET_SAMPLES_PATH=6_JPIB_K

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
  PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, 'samples-path', HAS_KEY, HOOKS )

  ! Read or apply the default configuration
  THIS%ECCODES_SAMPLES_PATH_ = REPEAT(' ',LEN(THIS%ECCODES_SAMPLES_PATH_))
  IF ( HAS_KEY ) THEN
    IF ( ALLOCATED(SAMPLES_PATH) ) THEN
      DEALLOCATE(SAMPLES_PATH, STAT=STAT)
    ENDIF
    PP_TRYCALL(ERRFLAG_GET_SUBCFG) YAML_READ_STRING_WITH_ENV_EXPANSION( CFG, 'samples-path', SAMPLES_PATH, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(SAMPLES_PATH), ERRFLAG_NOT_ALLOCATED_AFTER_READ )
    PP_DEBUG_CRITICAL_COND_THROW( LEN(SAMPLES_PATH).GT.LEN(THIS%ECCODES_SAMPLES_PATH_), ERRFLAG_TOO_LONG )
    THIS%ECCODES_SAMPLES_PATH_ = SAMPLES_PATH(:)
    IF ( ALLOCATED(SAMPLES_PATH) ) THEN
      DEALLOCATE( SAMPLES_PATH, STAT=STAT )
    ENDIF

  ! In multithread environment only one thread should set the samples path
!$omp single
    CALL GRIB_SET_SAMPLES_PATH( TRIM( THIS%ECCODES_SAMPLES_PATH_), STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERR_FLAG_UNABLE_TO_SET_SAMPLES_PATH )
!$omp end single

  ELSE
    THIS%ECCODES_SAMPLES_PATH_ = './'
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

    ! Local variables
    CHARACTER(LEN=4096) :: CGRIB_ERRMSG
    CHARACTER(LEN=32)   :: CGRIB_ERRIDX

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_HAS_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find the "samples-path" keyword in the configuration file' )
    CASE (ERRFLAG_GET_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the "samples-path" sub-configuration for the output manager' )
    CASE (ERRFLAG_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The string has not been allocated after reading' )
    CASE (ERRFLAG_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The string is too long' )
    CASE (ERR_FLAG_UNABLE_TO_SET_SAMPLES_PATH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the samples path' )
      CGRIB_ERRMSG = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, CGRIB_ERRMSG )
      CGRIB_ERRIDX = REPEAT(' ', 32)
      WRITE( CGRIB_ERRIDX, '(I32)', IOSTAT=STAT ) KRET
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'eccodes error code: '//TRIM(ADJUSTL(CGRIB_ERRIDX)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'eccodes error message: '//TRIM(ADJUSTL(CGRIB_ERRMSG)) )
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

END FUNCTION GRIB_HEADER2MULTIO_READ_SAMPLES_PATH_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_READ_MULTIO_PLANS_FILE_FROM_YAML'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_READ_MULTIO_PLANS_FILE_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),                 INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: HAS_KEY
  CHARACTER(LEN=:), ALLOCATABLE :: MULTIO_PLANS_FILE
  INTEGER(KIND=JPIB_K) :: STAT
  LOGICAL :: FEXISTS

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HAS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_SUBCFG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED_AFTER_READ=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOO_LONG=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONFIGURATION_FILE_NOT_FOUND=6_JPIB_K

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
  PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, 'multio-plans-file', HAS_KEY, HOOKS )

  ! Read or apply the default configuration
  THIS%MULTIO_PLANS_FILE_ = REPEAT(' ',LEN(THIS%MULTIO_PLANS_FILE_))
  IF ( HAS_KEY ) THEN
    IF ( ALLOCATED(MULTIO_PLANS_FILE) ) THEN
      DEALLOCATE(MULTIO_PLANS_FILE, STAT=STAT)
    ENDIF
    PP_TRYCALL(ERRFLAG_GET_SUBCFG) YAML_READ_STRING_WITH_ENV_EXPANSION( CFG, 'multio-plans-file', MULTIO_PLANS_FILE, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(MULTIO_PLANS_FILE), ERRFLAG_NOT_ALLOCATED_AFTER_READ )
    PP_DEBUG_CRITICAL_COND_THROW( LEN(MULTIO_PLANS_FILE).GT.LEN(THIS%MULTIO_PLANS_FILE_), ERRFLAG_TOO_LONG )
    THIS%MULTIO_PLANS_FILE_ = MULTIO_PLANS_FILE(:)
    IF ( ALLOCATED(MULTIO_PLANS_FILE) ) THEN
      DEALLOCATE( MULTIO_PLANS_FILE, STAT=STAT )
    ENDIF
  ELSE
    THIS%MULTIO_PLANS_FILE_ = './multio-plans.yaml'
  ENDIF

  ! Check if the file exsts
  INQUIRE( FILE=TRIM(THIS%MULTIO_PLANS_FILE_), EXIST=FEXISTS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.FEXISTS, ERRFLAG_CONFIGURATION_FILE_NOT_FOUND )

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find the "multio-plans-file" keyword in the configuration file' )
    CASE (ERRFLAG_GET_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the "multio-plans-file" sub-configuration for the output manager' )
    CASE (ERRFLAG_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The string has not been allocated after reading' )
    CASE (ERRFLAG_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The string is too long' )
    CASE (ERRFLAG_CONFIGURATION_FILE_NOT_FOUND)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The multio-plans-file file does not exist' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'File name: "'//TRIM(ADJUSTL(THIS%MULTIO_PLANS_FILE_))//'"' )
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

END FUNCTION GRIB_HEADER2MULTIO_READ_MULTIO_PLANS_FILE_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_READ_MAPPING_FILE_FROM_YAML'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_READ_MAPPING_FILE_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),                 INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: HAS_KEY
  CHARACTER(LEN=:), ALLOCATABLE :: MAPPING_FILE
  INTEGER(KIND=JPIB_K) :: STAT
  LOGICAL :: FEXISTS

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HAS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_SUBCFG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED_AFTER_READ=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOO_LONG=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONFIGURATION_FILE_NOT_FOUND=6_JPIB_K

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
  PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, 'mapping-rules-file', HAS_KEY, HOOKS )

  ! Read or apply the default configuration
  THIS%MAPPING_FILE_ = REPEAT(' ',LEN(THIS%MAPPING_FILE_))
  IF ( HAS_KEY ) THEN
    IF ( ALLOCATED(MAPPING_FILE) ) THEN
      DEALLOCATE(MAPPING_FILE, STAT=STAT)
    ENDIF
    PP_TRYCALL(ERRFLAG_GET_SUBCFG) YAML_READ_STRING_WITH_ENV_EXPANSION( CFG, 'mapping-rules-file', MAPPING_FILE, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(MAPPING_FILE), ERRFLAG_NOT_ALLOCATED_AFTER_READ )
    PP_DEBUG_CRITICAL_COND_THROW( LEN(MAPPING_FILE).GT.LEN(THIS%MAPPING_FILE_), ERRFLAG_TOO_LONG )
    THIS%MAPPING_FILE_ = MAPPING_FILE(:)
    IF ( ALLOCATED(MAPPING_FILE) ) THEN
      DEALLOCATE( MAPPING_FILE, STAT=STAT )
    ENDIF
  ELSE
    THIS%MAPPING_FILE_ = './mapping-rules.yaml'
  ENDIF

  ! Check if the file exsts
  INQUIRE( FILE=TRIM(THIS%MAPPING_FILE_), EXIST=FEXISTS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.FEXISTS, ERRFLAG_CONFIGURATION_FILE_NOT_FOUND )

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find the "mapping-file" keyword in the configuration file' )
    CASE (ERRFLAG_GET_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the "mapping-file" sub-configuration for the output manager' )
    CASE (ERRFLAG_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The string has not been allocated after reading' )
    CASE (ERRFLAG_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The string is too long' )
    CASE (ERRFLAG_CONFIGURATION_FILE_NOT_FOUND)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The mapping-rules file does not exist' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'File name: "'//TRIM(ADJUSTL(THIS%MAPPING_FILE_))//'"' )
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

END FUNCTION GRIB_HEADER2MULTIO_READ_MAPPING_FILE_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_READ_ENCODER_FILE_FROM_YAML'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_READ_ENCODER_FILE_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),                 INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: HAS_KEY
  CHARACTER(LEN=:), ALLOCATABLE :: ENCODER_FILE
  INTEGER(KIND=JPIB_K) :: STAT
  LOGICAL :: FEXISTS

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HAS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_SUBCFG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED_AFTER_READ=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOO_LONG=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONFIGURATION_FILE_NOT_FOUND=6_JPIB_K

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
  PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, 'encoding-rules-file', HAS_KEY, HOOKS )

  ! Read or apply the default configuration
  THIS%ENCODER_FILE_ = REPEAT(' ',LEN(THIS%ENCODER_FILE_))
  IF ( HAS_KEY ) THEN
    IF ( ALLOCATED(ENCODER_FILE) ) THEN
      DEALLOCATE(ENCODER_FILE, STAT=STAT)
    ENDIF
    PP_TRYCALL(ERRFLAG_GET_SUBCFG) YAML_READ_STRING_WITH_ENV_EXPANSION( CFG, 'encoding-rules-file', ENCODER_FILE, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(ENCODER_FILE), ERRFLAG_NOT_ALLOCATED_AFTER_READ )
    PP_DEBUG_CRITICAL_COND_THROW( LEN(ENCODER_FILE).GT.LEN(THIS%ENCODER_FILE_), ERRFLAG_TOO_LONG )
    THIS%ENCODER_FILE_ = ENCODER_FILE(:)
    IF ( ALLOCATED(ENCODER_FILE) ) THEN
      DEALLOCATE( ENCODER_FILE, STAT=STAT )
    ENDIF
  ELSE
    THIS%ENCODER_FILE_ = './encoding-rules.yaml'
  ENDIF

  ! Check if the file exsts
  INQUIRE( FILE=TRIM(THIS%ENCODER_FILE_), EXIST=FEXISTS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.FEXISTS, ERRFLAG_CONFIGURATION_FILE_NOT_FOUND )

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find the "encoder-file" keyword in the configuration file' )
    CASE (ERRFLAG_GET_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the "encoder-file" sub-configuration for the output manager' )
    CASE (ERRFLAG_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The string has not been allocated after reading' )
    CASE (ERRFLAG_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The string is too long' )
    CASE (ERRFLAG_CONFIGURATION_FILE_NOT_FOUND)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The encoding-rules file does not exist' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'File name: "'//TRIM(ADJUSTL(THIS%ENCODER_FILE_))//'"' )
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

END FUNCTION GRIB_HEADER2MULTIO_READ_ENCODER_FILE_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_READ_LOG_PATH_FROM_YAML'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_READ_LOG_PATH_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),                 INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: HAS_KEY
  CHARACTER(LEN=:), ALLOCATABLE :: LOG_PATH
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HAS_KEY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_SUBCFG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED_AFTER_READ=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TOO_LONG=5_JPIB_K

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
  PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, 'log-path', HAS_KEY, HOOKS )

  ! Read or apply the default configuration
  THIS%LOG_PATH_ = REPEAT(' ',LEN(THIS%LOG_PATH_))
  IF ( HAS_KEY ) THEN
    IF ( ALLOCATED(LOG_PATH) ) THEN
      DEALLOCATE(LOG_PATH, STAT=STAT)
    ENDIF
    PP_TRYCALL(ERRFLAG_GET_SUBCFG) YAML_READ_STRING_WITH_ENV_EXPANSION( CFG, 'log-path', LOG_PATH, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(LOG_PATH), ERRFLAG_NOT_ALLOCATED_AFTER_READ )
    PP_DEBUG_CRITICAL_COND_THROW( LEN(LOG_PATH).GT.LEN(THIS%LOG_PATH_), ERRFLAG_TOO_LONG )
    THIS%LOG_PATH_ = LOG_PATH(:)
    IF ( ALLOCATED(LOG_PATH) ) THEN
      DEALLOCATE( LOG_PATH, STAT=STAT )
    ENDIF
  ELSE
    THIS%LOG_PATH_ = './'
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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find the "log-path" keyword in the configuration file' )
    CASE (ERRFLAG_GET_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the "log-path" sub-configuration for the output manager' )
    CASE (ERRFLAG_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The string has not been allocated after reading' )
    CASE (ERRFLAG_TOO_LONG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The string is too long' )
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

END FUNCTION GRIB_HEADER2MULTIO_READ_LOG_PATH_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_READ_VERBOSE_FROM_YAML'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_READ_VERBOSE_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),                 INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

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

END FUNCTION GRIB_HEADER2MULTIO_READ_VERBOSE_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_READ_PROFILE_FROM_YAML'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_READ_PROFILE_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),                 INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

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

END FUNCTION GRIB_HEADER2MULTIO_READ_PROFILE_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_READ_SAVE_REPORT_FROM_YAML'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_READ_SAVE_REPORT_FROM_YAML( THIS, CFG, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),                 INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

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
  PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, 'save-report', HAS_KEY, HOOKS )

  ! Read or apply the default configuration
  IF ( HAS_KEY ) THEN
    PP_TRYCALL(ERRFLAG_GET_SUBCFG) YAML_READ_LOGICAL( CFG, 'save-report', THIS%SAVE_REPORT_, HOOKS )
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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find the "save-report" keyword in the configuration file' )
    CASE (ERRFLAG_GET_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the "save-report" sub-configuration for the output manager' )
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

END FUNCTION GRIB_HEADER2MULTIO_READ_SAVE_REPORT_FROM_YAML
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_INIT_GEOMETRY_MAP'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_INIT_GEOMETRY_MAP( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPRD_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  USE :: REPRESENTATIONS_MOD, ONLY: REPRES_A
  USE :: REPRESENTATIONS_MOD, ONLY: REDUCED_GG_T
  USE :: REPRESENTATIONS_MOD, ONLY: REGULAR_GG_T
  USE :: REPRESENTATIONS_MOD, ONLY: STRETCHED_ROTATED_SH_T
  USE :: REPRESENTATIONS_MOD, ONLY: STRETCHED_SH_T
  USE :: REPRESENTATIONS_MOD, ONLY: SH_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=16) :: CTMP
  CLASS(REPRES_A), POINTER :: REPRES
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INITIALIZE_REPRESENTATION_MAP=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_REPRES=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNEXPECTED_CLASS=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PUSH_TO_REPRESENTATION_MAP=5_JPIB_K

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

  !>
  !> Gridded data
  IF ( THIS%MODEL_PAR_%GEO_%NHTYP .GT. 0_JPIB_K ) THEN

    !> Grid is reduced_gg
    REPRES => NULL()
    ALLOCATE( REDUCED_GG_T::REPRES, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_ALLOCATE_REPRES )

    !> Generate  gg name
    CTMP=REPEAT(' ',16)
    WRITE(CTMP,'(I6)',IOSTAT=WRITE_STAT) THIS%MODEL_PAR_%GEO_%ILATS/2_JPIB_K
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(THIS%GG_NAME, '(A,A)',IOSTAT=WRITE_STAT) 'O',TRIM(ADJUSTL(CTMP))
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )

    !> Fill the general fields on the base data structure
    REPRES%DATA_REPRESENTATION_TYPE = 'reduced_gg'
    REPRES%NAME = TRIM(ADJUSTL(THIS%GG_NAME))

    !> Promote to the concrete type to set specific fields
    SELECT TYPE( R => REPRES)
    CLASS IS (REDUCED_GG_T)
      R%TRUNCATE_DEGREES = 1_JPIB_K
      R%NUMBER_OF_POINTS_ALONG_A_MERIDIAN = THIS%MODEL_PAR_%GEO_%ILATS
      R%NUMBER_OF_PARALLELS_BETWEEN_POLE_AND_EQUATOR = THIS%MODEL_PAR_%GEO_%IDGNH
      R%LAT_FIRST_GP_DEG = 180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*THIS%MODEL_PAR_%GEO_%ZNLAT
      R%LON_FIRST_GP_DEG = 0.0_JPRD_K
      R%LAT_LAST_GP_DEG = 180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*THIS%MODEL_PAR_%GEO_%ZSLAT
      R%LON_LAST_GP_DEG = 360._JPRD_K-360._JPRD_K/REAL(THIS%MODEL_PAR_%GEO_%ILONS,JPRD_K)
      R%TO_BE_DEALLOCATED = .FALSE.
      R%PL => THIS%MODEL_PAR_%GEO_%ILOENG(1:THIS%MODEL_PAR_%GEO_%ILATS)
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNEXPECTED_CLASS )
    END SELECT

    !> Add the representation to the map
    THIS%REPRES_GG_ => REPRES

  ELSE
    ! Grid is regular_gg (full)
    REPRES => NULL()
    ALLOCATE( REGULAR_GG_T::REPRES, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_ALLOCATE_REPRES )

    !> Generate  gg name
    CTMP=REPEAT(' ',16)
    WRITE(CTMP,'(I6)',IOSTAT=WRITE_STAT) THIS%MODEL_PAR_%GEO_%IDGNH
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(THIS%GG_NAME, '(A,A)',IOSTAT=WRITE_STAT) 'F',TRIM(ADJUSTL(CTMP))
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )

    !> Fill the general fields on the base data structure
    REPRES%DATA_REPRESENTATION_TYPE = 'regular_gg'
    REPRES%NAME = TRIM(ADJUSTL(THIS%GG_NAME))

    !> Promote to the concrete type to set specific fields
    SELECT TYPE( R => REPRES)
    CLASS IS (REGULAR_GG_T)
      R%TRUNCATE_DEGREES = 1_JPIB_K
      R%NUMBER_OF_POINTS_ALONG_A_MERIDIAN = THIS%MODEL_PAR_%GEO_%ILATS
      R%NUMBER_OF_POINTS_ALONG_A_PARALLEL = THIS%MODEL_PAR_%GEO_%ILONS
      R%NUMBER_OF_PARALLELS_BETWEEN_POLE_AND_EQUATOR = THIS%MODEL_PAR_%GEO_%IDGNH
      R%LAT_FIRST_GP_DEG = 180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*THIS%MODEL_PAR_%GEO_%ZNLAT
      R%LON_FIRST_GP_DEG = 0.0_JPRD_K
      R%LAT_LAST_GP_DEG = 180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*THIS%MODEL_PAR_%GEO_%ZSLAT
      R%LON_LAST_GP_DEG = 360._JPRD_K-360._JPRD_K/REAL(THIS%MODEL_PAR_%GEO_%ILONS,JPRD_K)
      R%IDIR_INC = 360.0_JPRD_K/REAL(THIS%MODEL_PAR_%GEO_%ILONS,JPRD_K)
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNEXPECTED_CLASS )
    END SELECT

    !> Add the representation to the map
    THIS%REPRES_GG_ => REPRES

  ENDIF


  !>
  !> Spherical harmonics
  IF ( THIS%MODEL_PAR_%GEO_%NSTTYP .GE. 2_JPIB_K ) THEN
    ! grid is stretched rotated spherical harmonics
    REPRES => NULL()
    ALLOCATE( STRETCHED_ROTATED_SH_T::REPRES, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_ALLOCATE_REPRES )

    !> Generate  sh name
    CTMP=REPEAT(' ',16)
    WRITE(CTMP,'(I16)',IOSTAT=WRITE_STAT) THIS%MODEL_PAR_%GEO_%ISMAX
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(THIS%SH_NAME, '(A,A)',IOSTAT=WRITE_STAT) 'TCO',TRIM(ADJUSTL(CTMP))
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )

    !> Fill the general fields on the base data structure
    REPRES%DATA_REPRESENTATION_TYPE = 'stretched_rotated_sh'
    REPRES%NAME = TRIM(ADJUSTL(THIS%SH_NAME))

    !> Promote to the concrete type to set specific fields
    SELECT TYPE( R => REPRES)
    CLASS IS (STRETCHED_ROTATED_SH_T)
      R%LAT_STRET_DEG = 180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*ASIN(REAL(THIS%MODEL_PAR_%GEO_%RMUCEN,JPRD_K))
      R%LON_STRET_DEG = 180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*REAL(THIS%MODEL_PAR_%GEO_%RLOCEN,JPRD_K)
      R%STRETCH_FACTOR = THIS%MODEL_PAR_%GEO_%RSTRET
      R%PENTAGONAL_RESOLUTIONS_PAR_J = THIS%MODEL_PAR_%GEO_%ISMAX
      R%PENTAGONAL_RESOLUTIONS_PAR_K = THIS%MODEL_PAR_%GEO_%ISMAX
      R%PENTAGONAL_RESOLUTIONS_PAR_M = THIS%MODEL_PAR_%GEO_%ISMAX
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNEXPECTED_CLASS )
    END SELECT

    !> Add the representation to the map
    THIS%REPRES_SH_ => REPRES

  ELSEIF ( ABS(THIS%MODEL_PAR_%GEO_%RSTRET-1.0_JPRD_K) .GE. 1.0E-14_JPRD_K ) THEN
    ! grid is stretched spherical harmonics
    REPRES => NULL()
    ALLOCATE( STRETCHED_SH_T::REPRES, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_ALLOCATE_REPRES )

    !> Generate  sh name
    CTMP=REPEAT(' ',16)
    WRITE(CTMP,'(I16)',IOSTAT=WRITE_STAT) THIS%MODEL_PAR_%GEO_%ISMAX
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(THIS%SH_NAME, '(A,A)',IOSTAT=WRITE_STAT) 'TCO',TRIM(ADJUSTL(CTMP))
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )

    !> Fill the general fields on the base data structure
    REPRES%DATA_REPRESENTATION_TYPE = 'stretched_sh'
    REPRES%NAME = TRIM(ADJUSTL(THIS%SH_NAME))

    !> Promote to the concrete type to set specific fields
    SELECT TYPE( R => REPRES)
    CLASS IS (STRETCHED_SH_T)
      R%PENTAGONAL_RESOLUTIONS_PAR_J = THIS%MODEL_PAR_%GEO_%ISMAX
      R%PENTAGONAL_RESOLUTIONS_PAR_K = THIS%MODEL_PAR_%GEO_%ISMAX
      R%PENTAGONAL_RESOLUTIONS_PAR_M = THIS%MODEL_PAR_%GEO_%ISMAX
      R%STRETCH_FACTOR = THIS%MODEL_PAR_%GEO_%RSTRET
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNEXPECTED_CLASS )
    END SELECT

    !> Add the representation to the map
    THIS%REPRES_SH_ => REPRES

  ELSE
    ! grid is spherical harmonics
    ! grid is stretched spherical harmonics
    ! grid is stretched rotated spherical harmonics
    REPRES => NULL()
    ALLOCATE( SH_T::REPRES, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_ALLOCATE_REPRES )

    !> Generate  sh name
    CTMP=REPEAT(' ',16)
    WRITE(CTMP,'(I16)',IOSTAT=WRITE_STAT) THIS%MODEL_PAR_%GEO_%ISMAX
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(THIS%SH_NAME, '(A,A)',IOSTAT=WRITE_STAT) 'TCO',TRIM(ADJUSTL(CTMP))
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )

    !> Fill the general fields on the base data structure
    REPRES%DATA_REPRESENTATION_TYPE = 'sh'
    REPRES%NAME = TRIM(ADJUSTL(THIS%SH_NAME))

    !> Promote to the concrete type to set specific fields
    SELECT TYPE( R => REPRES)
    CLASS IS (SH_T)
      R%PENTAGONAL_RESOLUTIONS_PAR_J = THIS%MODEL_PAR_%GEO_%ISMAX
      R%PENTAGONAL_RESOLUTIONS_PAR_K = THIS%MODEL_PAR_%GEO_%ISMAX
      R%PENTAGONAL_RESOLUTIONS_PAR_M = THIS%MODEL_PAR_%GEO_%ISMAX
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNEXPECTED_CLASS )
    END SELECT

    !> Add the representation to the map
    THIS%REPRES_SH_ => REPRES

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
    CASE (ERRFLAG_UNABLE_TO_INITIALIZE_REPRESENTATION_MAP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the representation map' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate the representation' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: "'//TRIM(ADJUSTL(ERRMSG))//'"' )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write to the string' )
    CASE (ERRFLAG_UNEXPECTED_CLASS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unexpected class' )
    CASE (ERRFLAG_UNABLE_TO_PUSH_TO_REPRESENTATION_MAP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to push the representation to the map' )
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

END FUNCTION GRIB_HEADER2MULTIO_INIT_GEOMETRY_MAP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_GET_MARS_FROM_ATM'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_GET_MARS_FROM_ATM( THIS, YDMSG, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: IFS_MSG_MOD,         ONLY: OM_ATM_MSG_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_TABLES_VERSION
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_ORIGIN
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_STREAM
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_TYPE
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_CLASS
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_EXPVER
  USE :: IFS2MARS_MOD,        ONLY: ATM2MARS_SET_LEVTYPE
  USE :: IFS2MARS_MOD,        ONLY: ATM2MARS_SET_LEVELIST
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_PACKING
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_ANALYSIS
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_ENSEMBLE
  USE :: IFS2MARS_MOD,        ONLY: ATM2MARS_SET_PARAM
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_DATETIME
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_GEOMETRY
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_IDENTIFICATION
  USE :: ENUMERATORS_MOD,     ONLY: REPRES_GAUSSIANGRID_E
  USE :: ENUMERATORS_MOD,     ONLY: REPRES_SPHERICALHARMONICS_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(OM_ATM_MSG_T),                         INTENT(IN)    :: YDMSG
  TYPE(FORTRAN_MESSAGE_T),                    INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T),                    INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL, DIMENSION(5) :: CONDITIONS

  ! Error Flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_ORIGIN=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_STREAM=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_TYPE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_CLASS=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_EXPVER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_LEVTYPE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_LEVELIST=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_PACKING=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_ANALYSIS=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_ENSEMBLE=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_PARAM=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_DATETIME=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_GEOMETRY=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_IDENTIFICATION=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNEXPECTED_REPRESENTATION=14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_TABLES_VERSION=15_JPIB_K

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

  ! Extract mars and context from message and parametrization
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_IDENTIFICATION) IFS2MARS_SET_IDENTIFICATION( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_ORIGIN)   IFS2MARS_SET_ORIGIN  ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_STREAM)   IFS2MARS_SET_STREAM  ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_TYPE)     IFS2MARS_SET_TYPE    ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_CLASS)    IFS2MARS_SET_CLASS   ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_EXPVER)   IFS2MARS_SET_EXPVER  ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_LEVTYPE)  ATM2MARS_SET_LEVTYPE ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_LEVELIST) ATM2MARS_SET_LEVELIST( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_PACKING)  IFS2MARS_SET_PACKING ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_ANALYSIS) IFS2MARS_SET_ANALYSIS( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_ENSEMBLE) IFS2MARS_SET_ENSEMBLE( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_PARAM)    ATM2MARS_SET_PARAM   ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_DATETIME) IFS2MARS_SET_DATETIME( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_TABLES_VERSION) IFS2MARS_SET_TABLES_VERSION( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )

  SELECT CASE (YDMSG%IREPRES_)

  CASE (REPRES_GAUSSIANGRID_E)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_GEOMETRY) IFS2MARS_SET_GEOMETRY( YDMSG, THIS%MODEL_PAR_, &
&    THIS%GG_NAME, THIS%REPRES_GG_, &
&    MSG, PAR, HOOKS )
  CASE (REPRES_SPHERICALHARMONICS_E)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_GEOMETRY) IFS2MARS_SET_GEOMETRY( YDMSG, THIS%MODEL_PAR_, &
&    THIS%SH_NAME, THIS%REPRES_SH_, &
&    MSG, PAR, HOOKS )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNEXPECTED_REPRESENTATION )
  END SELECT

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
    CASE (ERRFLAG_UNABLE_TO_SET_ORIGIN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the origin' )
    CASE (ERRFLAG_UNABLE_TO_SET_STREAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the stream' )
    CASE (ERRFLAG_UNABLE_TO_SET_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the type' )
    CASE (ERRFLAG_UNABLE_TO_SET_CLASS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the class' )
    CASE (ERRFLAG_UNABLE_TO_SET_EXPVER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the expver' )
    CASE (ERRFLAG_UNABLE_TO_SET_LEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the levtype' )
    CASE (ERRFLAG_UNABLE_TO_SET_LEVELIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the levlist' )
    CASE (ERRFLAG_UNABLE_TO_SET_PACKING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the packing' )
    CASE (ERRFLAG_UNABLE_TO_SET_ANALYSIS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the analysis' )
    CASE (ERRFLAG_UNABLE_TO_SET_ENSEMBLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the ensemble' )
    CASE (ERRFLAG_UNABLE_TO_SET_PARAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the param' )
    CASE (ERRFLAG_UNABLE_TO_SET_DATETIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the datetime' )
    CASE (ERRFLAG_UNABLE_TO_SET_GEOMETRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the geometry' )
    CASE (ERRFLAG_UNABLE_TO_SET_IDENTIFICATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the identification' )
    CASE (ERRFLAG_UNEXPECTED_REPRESENTATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unexpected representation' )
    CASE (ERRFLAG_UNABLE_TO_SET_TABLES_VERSION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the tables version' )
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

END FUNCTION GRIB_HEADER2MULTIO_GET_MARS_FROM_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_GET_MARS_FROM_WAM'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_GET_MARS_FROM_WAM( THIS, YDMSG, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: IFS_MSG_MOD,         ONLY: OM_WAM_MSG_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_TABLES_VERSION
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_ORIGIN
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_STREAM
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_TYPE
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_CLASS
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_EXPVER
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_PACKING
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_ANALYSIS
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_ENSEMBLE
  USE :: IFS2MARS_MOD,        ONLY: WAM2MARS_SET_PARAM
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_DATETIME
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_GEOMETRY
  USE :: IFS2MARS_MOD,        ONLY: WAM2MARS_SET_DIRFREQ
  USE :: IFS2MARS_MOD,        ONLY: IFS2MARS_SET_IDENTIFICATION
  USE :: ENUMERATORS_MOD,     ONLY: REPRES_GAUSSIANGRID_E
  USE :: ENUMERATORS_MOD,     ONLY: REPRES_SPHERICALHARMONICS_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(OM_WAM_MSG_T),                         INTENT(IN)    :: YDMSG
  TYPE(FORTRAN_MESSAGE_T),                    INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T),                    INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL, DIMENSION(5) :: CONDITIONS

  ! Error Flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_ORIGIN=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_STREAM=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_TYPE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_CLASS=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_EXPVER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_PACKING=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_ANALYSIS=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_ENSEMBLE=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_PARAM=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_DATETIME=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_GEOMETRY=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_DIRFREQ=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_IDENTIFICATION=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNEXPECTED_REPRESENTATION=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_TABLES_VERSION=15_JPIB_K

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

  ! Extract mars and context from message and parametrization
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_IDENTIFICATION) IFS2MARS_SET_IDENTIFICATION( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_ORIGIN)   IFS2MARS_SET_ORIGIN  ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_STREAM)   IFS2MARS_SET_STREAM  ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_TYPE)     IFS2MARS_SET_TYPE    ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_CLASS)    IFS2MARS_SET_CLASS   ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_EXPVER)   IFS2MARS_SET_EXPVER  ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_PACKING)  IFS2MARS_SET_PACKING ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_ANALYSIS) IFS2MARS_SET_ANALYSIS( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_ENSEMBLE) IFS2MARS_SET_ENSEMBLE( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_PARAM)    WAM2MARS_SET_PARAM   ( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_DATETIME) IFS2MARS_SET_DATETIME( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_TABLES_VERSION) IFS2MARS_SET_TABLES_VERSION( YDMSG, THIS%MODEL_PAR_, MSG, PAR, HOOKS )


  SELECT CASE (YDMSG%IREPRES_)

  CASE (REPRES_GAUSSIANGRID_E)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_GEOMETRY) IFS2MARS_SET_GEOMETRY( YDMSG, THIS%MODEL_PAR_, &
&    THIS%GG_NAME, THIS%REPRES_GG_, &
&    MSG, PAR, HOOKS )
  CASE (REPRES_SPHERICALHARMONICS_E)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_GEOMETRY) IFS2MARS_SET_GEOMETRY( YDMSG, THIS%MODEL_PAR_, &
&    THIS%SH_NAME, THIS%REPRES_SH_, &
&    MSG, PAR, HOOKS )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNEXPECTED_REPRESENTATION )
  END SELECT


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
    CASE (ERRFLAG_UNABLE_TO_SET_ORIGIN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the origin' )
    CASE (ERRFLAG_UNABLE_TO_SET_STREAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the stream' )
    CASE (ERRFLAG_UNABLE_TO_SET_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the type' )
    CASE (ERRFLAG_UNABLE_TO_SET_CLASS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the class' )
    CASE (ERRFLAG_UNABLE_TO_SET_EXPVER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the expver' )
    CASE (ERRFLAG_UNABLE_TO_SET_PACKING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the packing' )
    CASE (ERRFLAG_UNABLE_TO_SET_ANALYSIS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the analysis' )
    CASE (ERRFLAG_UNABLE_TO_SET_ENSEMBLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the ensemble' )
    CASE (ERRFLAG_UNABLE_TO_SET_PARAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the param' )
    CASE (ERRFLAG_UNABLE_TO_SET_DATETIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the datetime' )
    CASE (ERRFLAG_UNABLE_TO_SET_GEOMETRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the geometry' )
    CASE (ERRFLAG_UNABLE_TO_SET_DIRFREQ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the dirfreq' )
    CASE (ERRFLAG_UNABLE_TO_SET_IDENTIFICATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the identification' )
    CASE (ERRFLAG_UNEXPECTED_REPRESENTATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unexpected representation' )
    CASE (ERRFLAG_UNABLE_TO_SET_TABLES_VERSION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the tables version' )
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

END FUNCTION GRIB_HEADER2MULTIO_GET_MARS_FROM_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_READ_SINK_DP'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_READ_SINK_DP( THIS, MSG, PAR, &
&  NUNDEF, XUNDEF, MD, VALUES_DP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPRD_K
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: GRIB_METADATA_MOD,   ONLY: GRIB_METADATA_T

  !> Symbols imported from other libraries
  USE :: GRIB_API, ONLY: GRIB_GET
  USE :: GRIB_API, ONLY: GRIB_OPEN_FILE
  USE :: GRIB_API, ONLY: GRIB_WRITE
  USE :: GRIB_API, ONLY: GRIB_CLOSE_FILE
  USE :: GRIB_API, ONLY: GRIB_SUCCESS
  USE :: GRIB_API, ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),                    INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),                    INTENT(IN)    :: PAR
  INTEGER(KIND=JPIB_K),                       INTENT(IN)    :: NUNDEF
  REAL(KIND=JPRD_K),                          INTENT(IN)    :: XUNDEF
  CLASS(METADATA_BASE_A), POINTER,            INTENT(IN)    :: MD
  REAL(KIND=JPRD_K), DIMENSION(:),            INTENT(IN)    :: VALUES_DP
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIM_K) :: HANDLE
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIB_K) :: TMP
  INTEGER(KIND=JPIM_K) :: FID
  CHARACTER(LEN=:), ALLOCATABLE :: JSON

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_HANDLE=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA_NOT_SUPPORTED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CLOSE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERFLAG_METADATA_NOT_SUPPORTED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_GET_FAILED=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_TO_JSON=8_JPIB_K

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

  IF ( NUNDEF .GT. 0 ) THEN
    ! Set the paramId
    PP_METADATA_SET( MD, ERRFLAG_METADATA, 'bitmapPresent', 1_JPIB_K )
    PP_METADATA_SET( MD, ERRFLAG_METADATA, 'missingValue', XUNDEF )
  END IF

  ! Set global size and values into the grib handle
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SET) MD%SET( 'values', VALUES_DP, HOOKS )


  ! Paranoid verification
  SELECT TYPE(A => MD)

  CLASS IS (GRIB_METADATA_T)

    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_HANDLE) A%GET_HANDLE( HANDLE, HOOKS )

    ! Set the values to the handle
    CALL GRIB_GET( HANDLE, 'paramId', TMP, STATUS=KRET )
    PP_DEBUG_DEVELOP_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_GET_FAILED )

    IF (TMP .NE. MSG%PARAM .OR. TMP .LE. 0 ) THEN
      PP_TRYCALL(ERRFLAG_MARS_TO_JSON) MSG%TO_JSON( JSON, HOOKS )
      IF ( ALLOCATED(JSON) ) THEN
        WRITE(*,*)     ' SINK-DP :: ERROR PARAMAID DOES NOT MATCH :: ', TMP, MSG%PARAM
        WRITE(*,'(A)') ' SINK-DP :: ERROR PARAMAID DOES NOT MATCH :: '//TRIM(ADJUSTL(JSON))
        DEALLOCATE(JSON, STAT=TMP)
      ENDIF
    END IF

  CLASS DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERFLAG_METADATA_NOT_SUPPORTED )
  END SELECT




  ! Open the grib file
  CALL GRIB_OPEN_FILE( FID, 'allFields.grib', 'a', STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

  ! Paranoid verification
  SELECT TYPE(A => MD)

  CLASS IS (GRIB_METADATA_T)

    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_HANDLE) A%GET_HANDLE( HANDLE, HOOKS )

    ! Write to the grib file
    CALL GRIB_WRITE( HANDLE, FID, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_UNABLE_TO_OPEN )

  CLASS DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERFLAG_METADATA_NOT_SUPPORTED )
  END SELECT

  ! Close the grib file
  CALL GRIB_CLOSE_FILE( FID, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_UNABLE_TO_CLOSE )



  WRITE(*,*) ' * GRIB_HEADER2MULTIO_READ_SINK_DP :: '
  WRITE(*,*) ' --------------------------------------------------------'
  WRITE(*,*) ' '

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
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_GET_HANDLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the handle' )
    CASE (ERRFLAG_METADATA_NOT_SUPPORTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Metadata not supported' )
    CASE (ERRFLAG_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Metadata error' )
    CASE (ERRFLAG_UNABLE_TO_OPEN)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the file' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grib error: '//TRIM(ADJUSTL(GRIB_ERROR)) )
    CASE (ERRFLAG_UNABLE_TO_CLOSE)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close the file' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grib error: '//TRIM(ADJUSTL(GRIB_ERROR)) )
    CASE (ERRFLAG_UNABLE_TO_SET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set the values' )
    CASE (ERFLAG_METADATA_NOT_SUPPORTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Metadata not supported' )
    CASE (ERRFLAG_GRIB_GET_FAILED)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the paramId' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grib error: '//TRIM(ADJUSTL(GRIB_ERROR)) )
    CASE (ERRFLAG_MARS_TO_JSON)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert mars to json' )
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

END FUNCTION GRIB_HEADER2MULTIO_READ_SINK_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_READ_SINK_SP'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_READ_SINK_SP( THIS, MSG, PAR, &
&  NUNDEF, XUNDEF, MD, VALUES_SP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPRM_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPRD_K
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),                    INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),                    INTENT(IN)    :: PAR
  INTEGER(KIND=JPIB_K),                       INTENT(IN)    :: NUNDEF
  REAL(KIND=JPRD_K),                          INTENT(IN)    :: XUNDEF
  CLASS(METADATA_BASE_A), POINTER,            INTENT(IN)    :: MD
  REAL(KIND=JPRM_K), DIMENSION(:),            INTENT(IN)    :: VALUES_SP
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

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

END FUNCTION GRIB_HEADER2MULTIO_READ_SINK_SP
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
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_SETUP'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_SETUP( THIS, YAMLFNAME, PROCESSOR_TOPO, MODEL_PARAMS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_PAR_MOD,         ONLY: PROC_TOPO_T
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: IFS_PAR_MOD,         ONLY: PAR_PRINT
  USE :: SYSINFO_MOD,         ONLY: GET_TID
  USE :: SYSINFO_MOD,         ONLY: GET_PID
  USE :: SYSINFO_MOD,         ONLY: GET_NUM_THREADS
  USE :: SYSINFO_MOD,         ONLY: GET_HOSTNAME
  USE :: GENERAL_UTILS_MOD,   ONLY: TOLOWER
  USE :: LOG_INFO_MOD,        ONLY: LOG_VERSION
  USE :: LOG_INFO_MOD,        ONLY: LOG_CURR_TIME
  USE :: LOG_INFO_MOD,        ONLY: LOG_SYSINFO
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_NEW_CONFIGURATION_FROM_FILE
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION
  USE :: PROFILE_MOD,         ONLY: PROFILE_START_SIMULATION
  USE :: IFS_PAR_MOD,         ONLY: PAR_CREATE_NAME
  USE :: IFS_PAR_MOD,         ONLY: PAR_WOPEN
  USE :: IFS_PAR_MOD,         ONLY: PAR_WRITE
  USE :: IFS_PAR_MOD,         ONLY: PAR_CLOSE
  USE :: IFS_MSG_MOD,         ONLY: MSG_CREATE_NAME
  USE :: IFS_MSG_MOD,         ONLY: MSG_WOPEN
  USE :: IFS_VAL_MOD,         ONLY: VAL_CREATE_NAME
  USE :: IFS_VAL_MOD,         ONLY: VAL_WOPEN
  USE :: IFS_TOC_MOD,         ONLY: TOC_CREATE_NAME
  USE :: IFS_TOC_MOD,         ONLY: TOC_WOPEN
  USE :: IFS_TOC_MOD,         ONLY: TOC_WRITE_FLUSH_BEGIN_OF_SIMULATION
  USE :: MULTIO_UTILS_MOD,    ONLY: MULTIO_NEW

  USE :: METADATA_FACTORY_MOD,     ONLY: MAKE_METADATA
  USE :: FILTER_OPTIONS_MOD,       ONLY: FILTER_OPTIONS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: MAPPING_OPTIONS_MOD,      ONLY: MAPPING_OPTIONS_T
  USE :: CACHE_UTILS_MOD,          ONLY: CACHE_OPTIONS_T

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                           INTENT(IN)    :: YAMLFNAME
  TYPE(PROC_TOPO_T), TARGET,                  INTENT(IN)    :: PROCESSOR_TOPO
  TYPE(MODEL_PAR_T), TARGET,                  INTENT(IN)    :: MODEL_PARAMS
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: FEXISTS
  INTEGER(KIND=JPIB_K) :: STAT
  TYPE(YAML_CONFIGURATION_T) :: MAIN_CFG
  CHARACTER(LEN=LEN(GRIB_HEADER2MULTIO_OMNAME)) :: GRIB_HEADER2MULTIO_OMNAME_LC
  CHARACTER(LEN=1024) :: MSG_FNAME
  CHARACTER(LEN=1024) :: VAL_FNAME
  CHARACTER(LEN=1024) :: PAR_FNAME
  CHARACTER(LEN=1024) :: TOC_FNAME
  INTEGER(KIND=JPIB_K) :: PAR_UNIT

  ! Options
  TYPE(CACHE_OPTIONS_T)        :: CACHE_OPTIONS
  TYPE(GRIB_ENCODER_OPTIONS_T) :: ENCODER_OPTIONS
  TYPE(MAPPING_OPTIONS_T)      :: MAPPING_OPTIONS
  TYPE(FILTER_OPTIONS_T)       :: FILTER_OPTIONS

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
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FLUSH_BEGIN_OF_SIMULATION=25_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_MULTIO_HANDLE=26_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INITIALIZE_MULTIO_METADATA=27_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_TID=28_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_NUM_THREADS=29_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOADING_LOCAL_SAMPLE=30_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_INITIALIZE_ENCODER=31_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INT_GEOMETRY=32_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PRINT_GEOMETRY_MAP=33_JPIB_K


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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_HOSTNAME)    GET_HOSTNAME( THIS%HOSTNAME_, HOOKS )

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
    PP_TRYCALL(ERRFLAG_PROFILE_START) PROFILE_START_SIMULATION( THIS%PROFILE_DATA_, THIS%LOG_PATH_, &
&                                                       PROCESSOR_TOPO%MYPROC_IO, THIS%TID_, HOOKS )
  ENDIF

  ! Initialize multio with specific plan file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CREATE_MULTIO_HANDLE) MULTIO_NEW( THIS%MIO_, THIS%MULTIO_PLANS_FILE_, HOOKS )

  ! Initialise Multio metadata
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INITIALIZE_MULTIO_METADATA) THIS%MMD_%SET_MULTIO_HANDLE( THIS%MIO_, HOOKS )

  ! Logging
  IF ( THIS%VERBOSE_ ) THEN

    ! Convert the kind of output manager to lowercase
    GRIB_HEADER2MULTIO_OMNAME_LC = REPEAT(' ',LEN(GRIB_HEADER2MULTIO_OMNAME_LC))
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TO_LOWER) TOLOWER( GRIB_HEADER2MULTIO_OMNAME, GRIB_HEADER2MULTIO_OMNAME_LC, HOOKS )

    ! Create log file name
    THIS%LOG_FNAME_ = REPEAT(' ',LEN(THIS%LOG_FNAME_))
    WRITE(THIS%LOG_FNAME_,'(A,I8.8,A)', IOSTAT=STAT) TRIM(ADJUSTL(GRIB_HEADER2MULTIO_OMNAME_LC))//'-output-manager-', PROCESSOR_TOPO%MYPROC_IO, '.log'
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

  ! Initialize the geometry map
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INT_GEOMETRY) THIS%INIT_GEOMETRY_MAP( HOOKS )

  !> Open the configuration file
  PP_TRYCALL(ERRFLAG_LOADING_LOCAL_SAMPLE) MAKE_METADATA( THIS%METADATA_, 'grib', 'sample', HOOKS )

  ! Initialize the encoder
  PP_TRYCALL(ERRFLAG_UNABLE_INITIALIZE_ENCODER) THIS%MULTIOM_ENCODER_%INIT( &
  &   THIS%MAPPING_FILE_, THIS%ENCODER_FILE_, THIS%METADATA_, &
  &   CACHE_OPTIONS, ENCODER_OPTIONS, MAPPING_OPTIONS, FILTER_OPTIONS, &
  &   HOOKS )

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'File name: "'//TRIM(ADJUSTL(YAMLFNAME))//'"' )
    CASE (ERRFLAG_UNABLE_TO_OPEN_CFG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the configuration file' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'File name: "'//TRIM(ADJUSTL(YAMLFNAME))//'"' )
    CASE (ERRFLAG_UNABLE_TO_READ_SPECIFIC_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the specific configuration for the output manager' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'File name: "'//TRIM(ADJUSTL(YAMLFNAME))//'"' )
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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output manager name: "'//TRIM(ADJUSTL(GRIB_HEADER2MULTIO_OMNAME))//'"' )
    CASE (ERRFLAG_PROFILE_START)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to start the profiling' )
    CASE (ERRFLAG_UNABLE_TO_CREATE_MULTIO_HANDLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the multio handle' )
    CASE (ERRFLAG_UNABLE_TO_INITIALIZE_MULTIO_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the multio metadata' )
    CASE (ERRFLAG_UNABLE_TO_GET_TID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the thread ID' )
    CASE (ERRFLAG_UNABLE_TO_GET_NUM_THREADS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the number of threads' )
    CASE (ERRFLAG_LOADING_LOCAL_SAMPLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to load the local sample' )
    CASE (ERRFLAG_UNABLE_INITIALIZE_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the encoder' )
    CASE (ERRFLAG_UNABLE_TO_INT_GEOMETRY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the geometry map' )
    CASE (ERRFLAG_PRINT_GEOMETRY_MAP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the geometry map' )
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

END FUNCTION GRIB_HEADER2MULTIO_SETUP
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
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_WRITE_ATM_DP'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_WRITE_ATM_DP( THIS, YDMSG, VALUES_DP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPRD_K
  USE :: HOOKS_MOD,            ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,          ONLY: OM_ATM_MSG_T
  USE :: IFS_MSG_MOD,          ONLY: MSG_PRINT_ATM
  USE :: LOG_INFO_MOD,         ONLY: LOG_CURR_TIME
  USE :: FORTRAN_MESSAGE_MOD,  ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,  ONLY: PARAMETRIZATION_T
  USE :: PROFILE_MOD,          ONLY: PROFILE_MESSAGE
  USE :: METADATA_LIST_MOD,    ONLY: METADATA_LIST_T
  USE :: METADATA_BASE_MOD,    ONLY: METADATA_BASE_A
  USE :: METADATA_FACTORY_MOD, ONLY: DESTROY_METADATA

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_ATM_MSG_T),                                 INTENT(IN)    :: YDMSG
  REAL(KIND=JPRD_K), DIMENSION(:),                    INTENT(IN)    :: VALUES_DP
  TYPE(HOOKS_T),                                      INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_EMPTY
  TYPE(FORTRAN_MESSAGE_T) :: IN_MSG
  TYPE(PARAMETRIZATION_T) :: IN_PAR
  TYPE(FORTRAN_MESSAGE_T) :: OUT_MSG
  TYPE(PARAMETRIZATION_T) :: OUT_PAR
  TYPE(METADATA_LIST_T) :: METADATA_LIST
  CLASS(METADATA_BASE_A), POINTER :: MD
  CHARACTER(LEN=256) :: MAPPING_TAG
  CHARACTER(LEN=256) :: MAPPING_NAME
  CHARACTER(LEN=256) :: ENCODER_TAG
  CHARACTER(LEN=256) :: ENCODER_NAME
  CHARACTER(LEN=:), ALLOCATABLE :: JSON
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_MSG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_MSG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONMVERT_TO_MARS=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INITIALIZE_METADATA_LIST=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_ENCODE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODING_RULE_PRINT_ERROR=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_POP_FROM_LIST=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_MSG_ATM=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_DEALLOCATE_METADATA=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_CHECK_LIST=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_MSG=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_PAR=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_MSG=14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_PAR=15_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA_LIST_FREE=16_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_TO_JSON=17_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=18_JPIB_K

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

  ! Write the message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_MSG) IN_MSG%INIT( HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_PAR) IN_PAR%INIT( HOOKS )
  PP_TRYCALL(ERRFLAG_CONMVERT_TO_MARS) THIS%GET_MARS_FROM_ATM( YDMSG, IN_MSG, IN_PAR, HOOKS )


  PP_TRYCALL(ERRFLAG_MARS_TO_JSON) IN_MSG%TO_JSON( JSON, HOOKS )
  IF ( ALLOCATED(JSON) ) THEN
    WRITE(*,'(A,A)') ' * ORIGINAL - MARS to JSON: ', JSON
    DEALLOCATE( JSON, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  END IF


  !> Initialize the metadata list
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INITIALIZE_METADATA_LIST) METADATA_LIST%INIT( HOOKS )

  !> Encode the message
  PP_TRYCALL(ERRFLAG_ENCODER_ENCODE) THIS%MULTIOM_ENCODER_%ENCODE( IN_MSG, IN_PAR, METADATA_LIST, HOOKS )

  !> Loop over the encoding rules
  PP_TRYCALL(ERRFLAG_UNABLE_CHECK_LIST) METADATA_LIST%IS_EMPTY( IS_EMPTY, HOOKS )
  DO WHILE ( .NOT.IS_EMPTY )

    !> Get the encoding rule
    PP_TRYCALL(ERRFLAG_UNABLE_TO_POP_FROM_LIST) METADATA_LIST%POP( OUT_MSG, OUT_PAR, &
&       MAPPING_TAG, MAPPING_NAME, ENCODER_TAG, ENCODER_NAME, MD, HOOKS )

    !> Print Mapped message
    PP_TRYCALL(ERRFLAG_MARS_TO_JSON) OUT_MSG%TO_JSON( JSON, HOOKS )
    IF ( ALLOCATED(JSON) ) THEN
      WRITE(*,'(A,A)') ' * MAPPED - MARS to JSON: ', JSON
      DEALLOCATE( JSON, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
    ELSE
      WRITE(*,*) ' * MAPPED - MARS to JSON: ', 'NO JSON'
    END IF

    !> Sink the message
    PP_TRYCALL(ERRFLAG_WRITE_MSG_ATM) THIS%SINK_DP( OUT_MSG, OUT_PAR, YDMSG%NUNDF_, YDMSG%XUNDF_, MD, VALUES_DP, HOOKS )

    !> Free the encoded metadata
    PP_TRYCALL(ERRFLAG_UNABLE_DEALLOCATE_METADATA) DESTROY_METADATA( MD, HOOKS )

    !> Free the message/parametrization
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) OUT_MSG%FREE( HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_PAR) OUT_PAR%FREE( HOOKS )

    !> Increment the counter
    PP_TRYCALL(ERRFLAG_UNABLE_CHECK_LIST) METADATA_LIST%IS_EMPTY( IS_EMPTY, HOOKS )

  ENDDO

  ! PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) IN_MSG%WRITE_TO_YAML( 6_JPIB_K, 0, HOOKS )
  ! PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) IN_PAR%WRITE_TO_YAML( 6_JPIB_K, 0, HOOKS )

  !> Free the message/parametrization
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) IN_MSG%FREE( HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_PAR) IN_PAR%FREE( HOOKS )


  !> Free the metadata list
  PP_TRYCALL(ERRFLAG_METADATA_LIST_FREE) METADATA_LIST%FREE( HOOKS )

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
    CASE (ERRFLAG_PROFILE_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to profile the message' )
    CASE (ERRFLAG_CONMVERT_TO_MARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert the message to MARS' )
    CASE (ERRFLAG_UNABLE_TO_INITIALIZE_METADATA_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the metadata list' )
    CASE (ERRFLAG_ENCODER_ENCODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to encode the message' )
    CASE (ERRFLAG_ENCODING_RULE_PRINT_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the encoding rule' )
    CASE (ERRFLAG_UNABLE_TO_POP_FROM_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to pop the encoding rule from the list' )
    CASE (ERRFLAG_WRITE_MSG_ATM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the message' )
    CASE (ERRFLAG_UNABLE_DEALLOCATE_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate the metadata' )
    CASE (ERRFLAG_MARS_TO_JSON)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert MARS to JSON' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate json' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE (ERRFLAG_UNABLE_TO_FREE_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the message' )
    CASE (ERRFLAG_UNABLE_TO_FREE_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the parametrization' )
    CASE (ERRFLAG_UNABLE_TO_INIT_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to init the message' )
    CASE (ERRFLAG_UNABLE_TO_INIT_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to init the parametrization' )
    CASE (ERRFLAG_METADATA_LIST_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the metadata list' )
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

END FUNCTION GRIB_HEADER2MULTIO_WRITE_ATM_DP
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
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_WRITE_ATM_SP'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_WRITE_ATM_SP( THIS, YDMSG, VALUES_SP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPRM_K
  USE :: HOOKS_MOD,            ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,          ONLY: OM_ATM_MSG_T
  USE :: IFS_MSG_MOD,          ONLY: MSG_PRINT_ATM
  USE :: LOG_INFO_MOD,         ONLY: LOG_CURR_TIME
  USE :: FORTRAN_MESSAGE_MOD,  ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,  ONLY: PARAMETRIZATION_T
  USE :: PROFILE_MOD,          ONLY: PROFILE_MESSAGE
  USE :: METADATA_LIST_MOD,    ONLY: METADATA_LIST_T
  USE :: METADATA_BASE_MOD,    ONLY: METADATA_BASE_A
  USE :: METADATA_FACTORY_MOD, ONLY: DESTROY_METADATA

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_ATM_MSG_T),                                 INTENT(IN)    :: YDMSG
  REAL(KIND=JPRM_K), DIMENSION(:),                    INTENT(IN)    :: VALUES_SP
  TYPE(HOOKS_T),                                      INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_EMPTY
  TYPE(FORTRAN_MESSAGE_T) :: IN_MSG
  TYPE(PARAMETRIZATION_T) :: IN_PAR
  TYPE(FORTRAN_MESSAGE_T) :: OUT_MSG
  TYPE(PARAMETRIZATION_T) :: OUT_PAR
  TYPE(METADATA_LIST_T) :: METADATA_LIST

  CLASS(METADATA_BASE_A), POINTER :: MD
  CHARACTER(LEN=256) :: MAPPING_TAG
  CHARACTER(LEN=256) :: MAPPING_NAME
  CHARACTER(LEN=256) :: ENCODER_TAG
  CHARACTER(LEN=256) :: ENCODER_NAME
  CHARACTER(LEN=:), ALLOCATABLE :: JSON
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_MSG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_MSG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONMVERT_TO_MARS=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INITIALIZE_METADATA_LIST=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_ENCODE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODING_RULE_PRINT_ERROR=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_POP_FROM_LIST=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_MSG_ATM=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_DEALLOCATE_METADATA=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_CHECK_LIST=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_MSG=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_PAR=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_MSG=14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_PAR=15_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA_LIST_FREE=16_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_TO_JSON=17_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=18_JPIB_K

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


  ! Write the message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_MSG) IN_MSG%INIT( HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_PAR) IN_PAR%INIT( HOOKS )
  PP_TRYCALL(ERRFLAG_CONMVERT_TO_MARS) THIS%GET_MARS_FROM_ATM( YDMSG, IN_MSG, IN_PAR, HOOKS )


  PP_TRYCALL(ERRFLAG_MARS_TO_JSON) IN_MSG%TO_JSON( JSON, HOOKS )
  IF ( ALLOCATED(JSON) ) THEN
    WRITE(*,'(A,A)') 'MARS to JSON: ', JSON
    DEALLOCATE( JSON, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  END IF

  !> Initialize the metadata list
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INITIALIZE_METADATA_LIST) METADATA_LIST%INIT( HOOKS )

  !> Encode the message
  PP_TRYCALL(ERRFLAG_ENCODER_ENCODE) THIS%MULTIOM_ENCODER_%ENCODE( IN_MSG, IN_PAR, METADATA_LIST, HOOKS )

  !> Loop over the encoding rules
  PP_TRYCALL(ERRFLAG_UNABLE_CHECK_LIST) METADATA_LIST%IS_EMPTY( IS_EMPTY, HOOKS )
  DO WHILE ( .NOT.IS_EMPTY )

    !> Get the encoding rule
    PP_TRYCALL(ERRFLAG_UNABLE_TO_POP_FROM_LIST) METADATA_LIST%POP( OUT_MSG, OUT_PAR, &
&       MAPPING_TAG, MAPPING_NAME, ENCODER_TAG, ENCODER_NAME, MD, HOOKS )

    !> Print Mapped message
    PP_TRYCALL(ERRFLAG_MARS_TO_JSON) OUT_MSG%TO_JSON( JSON, HOOKS )
    IF ( ALLOCATED(JSON) ) THEN
      WRITE(*,'(A,A)') 'MARS to JSON: ', JSON
      DEALLOCATE( JSON, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
    ELSE
      WRITE(*,*) ' * MAPPED - MARS to JSON: ', 'NO JSON'
    END IF

#if 0
    !> Sink the message
    PP_TRYCALL(ERRFLAG_WRITE_MSG_ATM) THIS%SINK_SP( OUT_MSG, OUT_PAR, YDMSG%NUNDF_, YDMSG%XUNDF_, MD, VALUES_SP, HOOKS )
#endif
    !> Free the encoded metadata
    PP_TRYCALL(ERRFLAG_UNABLE_DEALLOCATE_METADATA) DESTROY_METADATA( MD, HOOKS )

    !> Free the message/parametrization
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) OUT_MSG%FREE( HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_PAR) OUT_PAR%FREE( HOOKS )

    !> Increment the counter
    PP_TRYCALL(ERRFLAG_UNABLE_CHECK_LIST) METADATA_LIST%IS_EMPTY( IS_EMPTY, HOOKS )

  ENDDO

  ! PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) IN_MSG%WRITE_TO_YAML( 6_JPIB_K, 0, HOOKS )
  ! PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) IN_PAR%WRITE_TO_YAML( 6_JPIB_K, 0, HOOKS )

  !> Free the message/parametrization
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) IN_MSG%FREE( HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_PAR) IN_PAR%FREE( HOOKS )


  !> Free the metadata list
  PP_TRYCALL(ERRFLAG_METADATA_LIST_FREE) METADATA_LIST%FREE( HOOKS )

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
    CASE (ERRFLAG_PROFILE_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to profile the message' )
    CASE (ERRFLAG_CONMVERT_TO_MARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert the message to MARS' )
    CASE (ERRFLAG_UNABLE_TO_INITIALIZE_METADATA_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the metadata list' )
    CASE (ERRFLAG_ENCODER_ENCODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to encode the message' )
    CASE (ERRFLAG_ENCODING_RULE_PRINT_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the encoding rule' )
    CASE (ERRFLAG_UNABLE_TO_POP_FROM_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to pop the encoding rule from the list' )
    CASE (ERRFLAG_WRITE_MSG_ATM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the message' )
    CASE (ERRFLAG_UNABLE_DEALLOCATE_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate the metadata' )
    CASE (ERRFLAG_MARS_TO_JSON)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert MARS to JSON' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate json' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE (ERRFLAG_UNABLE_TO_FREE_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the message' )
    CASE (ERRFLAG_UNABLE_TO_FREE_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the parametrization' )
    CASE (ERRFLAG_UNABLE_TO_INIT_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to init the message' )
    CASE (ERRFLAG_UNABLE_TO_INIT_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to init the parametrization' )
    CASE (ERRFLAG_METADATA_LIST_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the metadata list' )
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

END FUNCTION GRIB_HEADER2MULTIO_WRITE_ATM_SP
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
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_WRITE_WAM_DP'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_WRITE_WAM_DP( THIS, YDMSG, VALUES_DP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPRD_K
  USE :: HOOKS_MOD,            ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,          ONLY: OM_WAM_MSG_T
  USE :: IFS_MSG_MOD,          ONLY: MSG_PRINT_WAM
  USE :: LOG_INFO_MOD,         ONLY: LOG_CURR_TIME
  USE :: FORTRAN_MESSAGE_MOD,  ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,  ONLY: PARAMETRIZATION_T
  USE :: PROFILE_MOD,          ONLY: PROFILE_MESSAGE
  USE :: METADATA_LIST_MOD,    ONLY: METADATA_LIST_T
  USE :: METADATA_BASE_MOD,    ONLY: METADATA_BASE_A
  USE :: METADATA_FACTORY_MOD, ONLY: DESTROY_METADATA

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_WAM_MSG_T),                                 INTENT(IN)    :: YDMSG
  REAL(KIND=JPRD_K), DIMENSION(:),                    INTENT(IN)    :: VALUES_DP
  TYPE(HOOKS_T),                                      INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_EMPTY
  TYPE(FORTRAN_MESSAGE_T) :: IN_MSG
  TYPE(PARAMETRIZATION_T) :: IN_PAR
  TYPE(FORTRAN_MESSAGE_T) :: OUT_MSG
  TYPE(PARAMETRIZATION_T) :: OUT_PAR
  TYPE(METADATA_LIST_T) :: METADATA_LIST

  CLASS(METADATA_BASE_A), POINTER :: MD
  CHARACTER(LEN=256) :: MAPPING_TAG
  CHARACTER(LEN=256) :: MAPPING_NAME
  CHARACTER(LEN=256) :: ENCODER_TAG
  CHARACTER(LEN=256) :: ENCODER_NAME
  CHARACTER(LEN=:), ALLOCATABLE :: JSON
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_MSG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_MSG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONMVERT_TO_MARS=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INITIALIZE_METADATA_LIST=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_ENCODE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODING_RULE_PRINT_ERROR=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_POP_FROM_LIST=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_MSG_WAM=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_DEALLOCATE_METADATA=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_CHECK_LIST=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_MSG=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_PAR=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_MSG=14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_PAR=15_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA_LIST_FREE=16_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_TO_JSON=17_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=18_JPIB_K

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


  ! Write the message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_MSG) IN_MSG%INIT( HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_PAR) IN_PAR%INIT( HOOKS )
  PP_TRYCALL(ERRFLAG_CONMVERT_TO_MARS) THIS%GET_MARS_FROM_WAM( YDMSG, IN_MSG, IN_PAR, HOOKS )


  PP_TRYCALL(ERRFLAG_MARS_TO_JSON) IN_MSG%TO_JSON( JSON, HOOKS )
  IF ( ALLOCATED(JSON) ) THEN
    WRITE(*,'(A,A)') 'MARS to JSON: ', JSON
    DEALLOCATE( JSON, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  END IF

  !> Initialize the metadata list
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INITIALIZE_METADATA_LIST) METADATA_LIST%INIT( HOOKS )

  !> Encode the message
  PP_TRYCALL(ERRFLAG_ENCODER_ENCODE) THIS%MULTIOM_ENCODER_%ENCODE( IN_MSG, IN_PAR, METADATA_LIST, HOOKS )

  !> Loop over the encoding rules
  PP_TRYCALL(ERRFLAG_UNABLE_CHECK_LIST) METADATA_LIST%IS_EMPTY( IS_EMPTY, HOOKS )
  DO WHILE ( .NOT.IS_EMPTY )

    !> Get the encoding rule
    PP_TRYCALL(ERRFLAG_UNABLE_TO_POP_FROM_LIST) METADATA_LIST%POP( OUT_MSG, OUT_PAR, &
&       MAPPING_TAG, MAPPING_NAME, ENCODER_TAG, ENCODER_NAME, MD, HOOKS )

    !> Print Mapped message
    PP_TRYCALL(ERRFLAG_MARS_TO_JSON) OUT_MSG%TO_JSON( JSON, HOOKS )
    IF ( ALLOCATED(JSON) ) THEN
      WRITE(*,'(A,A)') 'MARS to JSON: ', JSON
      DEALLOCATE( JSON, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
    ELSE
      WRITE(*,*) ' * MAPPED - MARS to JSON: ', 'NO JSON'
    END IF

#if 0
    !> Sink the message
    PP_TRYCALL(ERRFLAG_WRITE_MSG_WAM) THIS%SINK_DP( OUT_MSG, OUT_PAR, YDMSG%NUNDF_, YDMSG%XUNDF_, MD, VALUES_DP, HOOKS )

#endif
    !> Free the encoded metadata
    PP_TRYCALL(ERRFLAG_UNABLE_DEALLOCATE_METADATA) DESTROY_METADATA( MD, HOOKS )

    !> Free the message/parametrization
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) OUT_MSG%FREE( HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_PAR) OUT_PAR%FREE( HOOKS )

    !> Increment the counter
    PP_TRYCALL(ERRFLAG_UNABLE_CHECK_LIST) METADATA_LIST%IS_EMPTY( IS_EMPTY, HOOKS )

  ENDDO

  ! PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) IN_MSG%WRITE_TO_YAML( 6_JPIB_K, 0, HOOKS )
  ! PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) IN_PAR%WRITE_TO_YAML( 6_JPIB_K, 0, HOOKS )

  !> Free the message/parametrization
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) IN_MSG%FREE( HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_PAR) IN_PAR%FREE( HOOKS )


  !> Free the metadata list
  PP_TRYCALL(ERRFLAG_METADATA_LIST_FREE) METADATA_LIST%FREE( HOOKS )

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
    CASE (ERRFLAG_PROFILE_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to profile the message' )
    CASE (ERRFLAG_CONMVERT_TO_MARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert the message to MARS' )
    CASE (ERRFLAG_UNABLE_TO_INITIALIZE_METADATA_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the metadata list' )
    CASE (ERRFLAG_ENCODER_ENCODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to encode the message' )
    CASE (ERRFLAG_ENCODING_RULE_PRINT_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the encoding rule' )
    CASE (ERRFLAG_UNABLE_TO_POP_FROM_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to pop the encoding rule from the list' )
    CASE (ERRFLAG_WRITE_MSG_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the message' )
    CASE (ERRFLAG_UNABLE_DEALLOCATE_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate the metadata' )
    CASE (ERRFLAG_MARS_TO_JSON)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert MARS to JSON' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate json' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE (ERRFLAG_UNABLE_TO_FREE_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the message' )
    CASE (ERRFLAG_UNABLE_TO_FREE_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the parametrization' )
    CASE (ERRFLAG_UNABLE_TO_INIT_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to init the message' )
    CASE (ERRFLAG_UNABLE_TO_INIT_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to init the parametrization' )
    CASE (ERRFLAG_METADATA_LIST_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the metadata list' )
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

END FUNCTION GRIB_HEADER2MULTIO_WRITE_WAM_DP
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
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_WRITE_WAM_SP'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_WRITE_WAM_SP( THIS, YDMSG, VALUES_SP, HOOKS ) RESULT(RET)


  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPRM_K
  USE :: HOOKS_MOD,            ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,          ONLY: OM_WAM_MSG_T
  USE :: IFS_MSG_MOD,          ONLY: MSG_PRINT_WAM
  USE :: LOG_INFO_MOD,         ONLY: LOG_CURR_TIME
  USE :: FORTRAN_MESSAGE_MOD,  ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,  ONLY: PARAMETRIZATION_T
  USE :: PROFILE_MOD,          ONLY: PROFILE_MESSAGE
  USE :: METADATA_LIST_MOD,    ONLY: METADATA_LIST_T
  USE :: METADATA_BASE_MOD,    ONLY: METADATA_BASE_A
  USE :: METADATA_FACTORY_MOD, ONLY: DESTROY_METADATA

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(OM_WAM_MSG_T),                                 INTENT(IN)    :: YDMSG
  REAL(KIND=JPRM_K), DIMENSION(:),                    INTENT(IN)    :: VALUES_SP
  TYPE(HOOKS_T),                                      INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_EMPTY
  TYPE(FORTRAN_MESSAGE_T) :: IN_MSG
  TYPE(PARAMETRIZATION_T) :: IN_PAR
  TYPE(FORTRAN_MESSAGE_T) :: OUT_MSG
  TYPE(PARAMETRIZATION_T) :: OUT_PAR
  TYPE(METADATA_LIST_T) :: METADATA_LIST

  CLASS(METADATA_BASE_A), POINTER :: MD
  CHARACTER(LEN=256) :: MAPPING_TAG
  CHARACTER(LEN=256) :: MAPPING_NAME
  CHARACTER(LEN=256) :: ENCODER_TAG
  CHARACTER(LEN=256) :: ENCODER_NAME
  CHARACTER(LEN=:), ALLOCATABLE :: JSON
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_MSG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_MSG=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONMVERT_TO_MARS=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INITIALIZE_METADATA_LIST=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_ENCODE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODING_RULE_PRINT_ERROR=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_POP_FROM_LIST=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_MSG_WAM=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_DEALLOCATE_METADATA=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_CHECK_LIST=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_MSG=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_PAR=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_MSG=14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_PAR=15_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA_LIST_FREE=16_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_TO_JSON=17_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=18_JPIB_K

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


  ! Write the message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_MSG) IN_MSG%INIT( HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_PAR) IN_PAR%INIT( HOOKS )
  PP_TRYCALL(ERRFLAG_CONMVERT_TO_MARS) THIS%GET_MARS_FROM_WAM( YDMSG, IN_MSG, IN_PAR, HOOKS )


  PP_TRYCALL(ERRFLAG_MARS_TO_JSON) IN_MSG%TO_JSON( JSON, HOOKS )
  IF ( ALLOCATED(JSON) ) THEN
    WRITE(*,'(A,A)') 'MARS to JSON: ', JSON
    DEALLOCATE( JSON, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  END IF

  !> Initialize the metadata list
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INITIALIZE_METADATA_LIST) METADATA_LIST%INIT( HOOKS )

  !> Encode the message
  PP_TRYCALL(ERRFLAG_ENCODER_ENCODE) THIS%MULTIOM_ENCODER_%ENCODE( IN_MSG, IN_PAR, METADATA_LIST, HOOKS )

  !> Loop over the encoding rules
  PP_TRYCALL(ERRFLAG_UNABLE_CHECK_LIST) METADATA_LIST%IS_EMPTY( IS_EMPTY, HOOKS )
  DO WHILE ( .NOT.IS_EMPTY )

    !> Get the encoding rule
    PP_TRYCALL(ERRFLAG_UNABLE_TO_POP_FROM_LIST) METADATA_LIST%POP( OUT_MSG, OUT_PAR, &
&       MAPPING_TAG, MAPPING_NAME, ENCODER_TAG, ENCODER_NAME, MD, HOOKS )

    !> Print Mapped message
    PP_TRYCALL(ERRFLAG_MARS_TO_JSON) OUT_MSG%TO_JSON( JSON, HOOKS )
    IF ( ALLOCATED(JSON) ) THEN
      WRITE(*,'(A,A)') 'MARS to JSON: ', JSON
      DEALLOCATE( JSON, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
    ELSE
      WRITE(*,*) ' * MAPPED - MARS to JSON: ', 'NO JSON'
    END IF

#if 0
    !> Sink the message
    PP_TRYCALL(ERRFLAG_WRITE_MSG_WAM) THIS%SINK_SP( OUT_MSG, OUT_PAR, YDMSG%NUNDF_, YDMSG%XUNDF_, MD, VALUES_SP, HOOKS )
#endif

    !> Free the encoded metadata
    PP_TRYCALL(ERRFLAG_UNABLE_DEALLOCATE_METADATA) DESTROY_METADATA( MD, HOOKS )

    !> Free the message/parametrization
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) OUT_MSG%FREE( HOOKS )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_PAR) OUT_PAR%FREE( HOOKS )

    !> Increment the counter
    PP_TRYCALL(ERRFLAG_UNABLE_CHECK_LIST) METADATA_LIST%IS_EMPTY( IS_EMPTY, HOOKS )

  ENDDO

  ! PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) IN_MSG%WRITE_TO_YAML( 6_JPIB_K, 0, HOOKS )
  ! PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) IN_PAR%WRITE_TO_YAML( 6_JPIB_K, 0, HOOKS )

  !> Free the message/parametrization
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MSG) IN_MSG%FREE( HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_PAR) IN_PAR%FREE( HOOKS )


  !> Free the metadata list
  PP_TRYCALL(ERRFLAG_METADATA_LIST_FREE) METADATA_LIST%FREE( HOOKS )

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
    CASE (ERRFLAG_PROFILE_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to profile the message' )
    CASE (ERRFLAG_CONMVERT_TO_MARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert the message to MARS' )
    CASE (ERRFLAG_UNABLE_TO_INITIALIZE_METADATA_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the metadata list' )
    CASE (ERRFLAG_ENCODER_ENCODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to encode the message' )
    CASE (ERRFLAG_ENCODING_RULE_PRINT_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the encoding rule' )
    CASE (ERRFLAG_UNABLE_TO_POP_FROM_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to pop the encoding rule from the list' )
    CASE (ERRFLAG_WRITE_MSG_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the message' )
    CASE (ERRFLAG_UNABLE_DEALLOCATE_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate the metadata' )
    CASE (ERRFLAG_MARS_TO_JSON)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert MARS to JSON' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate json' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE (ERRFLAG_UNABLE_TO_FREE_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the message' )
    CASE (ERRFLAG_UNABLE_TO_FREE_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the parametrization' )
    CASE (ERRFLAG_UNABLE_TO_INIT_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to init the message' )
    CASE (ERRFLAG_UNABLE_TO_INIT_PAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to init the parametrization' )
    CASE (ERRFLAG_METADATA_LIST_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the metadata list' )
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

END FUNCTION GRIB_HEADER2MULTIO_WRITE_WAM_SP
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
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_FLUSH_STEP'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_FLUSH_STEP( THIS, KSTEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: MULTIO_UTILS_MOD,  ONLY: MULTIO_FLUSH
  USE :: PROFILE_MOD,       ONLY: PROFILE_FLUSH
  USE :: LOG_INFO_MOD,      ONLY: LOG_CURR_TIME

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                       INTENT(IN)    :: KSTEP
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=128) :: CLTMP
  INTEGER(KIND=JPIB_K) :: STATUS

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_FLUSH=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_COMMIT=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FLUSH_MULTIO=5_JPIB_K

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
    CLTMP = REPEAT(' ',128)
    WRITE(CLTMP, '(I10)', IOSTAT=STATUS ) KSTEP
    PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH STEP: '//TRIM(ADJUSTL(CLTMP)), HOOKS )
  ENDIF

  !> Encode the message (In case of multithreaded encodr, this is the point where the
  !> new encoding rules are committed)
  ! PP_TRYCALL(ERRFLAG_ENCODER_COMMIT) THIS%MULTIOM_ENCODER_%COMMIT( HOOKS )

  ! Send to MultIO the flush for the last step
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FLUSH_MULTIO) MULTIO_FLUSH( THIS%MIO_, KSTEP, HOOKS )

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
    CASE (ERRFLAG_PROFILE_FLUSH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to profile the flush' )
    CASE (ERRFLAG_ENCODER_COMMIT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to commit the encoder' )
    CASE (ERRFLAG_UNABLE_TO_LOG_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the step' )
    CASE (ERRFLAG_UNABLE_TO_FLUSH_MULTIO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to flush MultIO' )
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

END FUNCTION GRIB_HEADER2MULTIO_FLUSH_STEP
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
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_FLUSH_LAST_STEP'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_FLUSH_LAST_STEP( THIS, KSTEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: MULTIO_UTILS_MOD,  ONLY: MULTIO_FLUSH_LAST_STEP
  USE :: PROFILE_MOD,       ONLY: PROFILE_FLUSH_LAST_STEP
  USE :: LOG_INFO_MOD,      ONLY: LOG_CURR_TIME

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                       INTENT(IN)    :: KSTEP
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=128) :: CLTMP
  INTEGER(KIND=JPIB_K) :: STATUS

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_FLUSH=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_COMMIT=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FLUSH_MULTIO=5_JPIB_K

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
    CLTMP = REPEAT(' ',128)
    WRITE(CLTMP, '(I10)', IOSTAT=STATUS ) KSTEP
    PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH STEP: '//TRIM(ADJUSTL(CLTMP)), HOOKS )
  ENDIF

  ! Send to MultIO the flush for the last step
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FLUSH_MULTIO) MULTIO_FLUSH_LAST_STEP( THIS%MIO_, KSTEP, HOOKS )

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
    CASE (ERRFLAG_PROFILE_FLUSH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to profile the flush' )
    CASE (ERRFLAG_ENCODER_COMMIT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to commit the encoder' )
    CASE (ERRFLAG_UNABLE_TO_LOG_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the step' )
    CASE (ERRFLAG_UNABLE_TO_FLUSH_MULTIO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to flush MultIO' )
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

END FUNCTION GRIB_HEADER2MULTIO_FLUSH_LAST_STEP
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
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_FLUSH_STEP_AND_TRIGGER_RESTART'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_FLUSH_STEP_AND_TRIGGER_RESTART( THIS, KSTEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: MULTIO_UTILS_MOD,  ONLY: MULTIO_FLUSH_AND_TRIGGER_RESTART
  USE :: PROFILE_MOD,       ONLY: PROFILE_FLUSH_AND_RESTART
  USE :: LOG_INFO_MOD,      ONLY: LOG_CURR_TIME

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),                       INTENT(IN)    :: KSTEP
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=128) :: CLTMP
  INTEGER(KIND=JPIB_K) :: STATUS

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_FLUSH=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_COMMIT=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FLUSH_MULTIO=5_JPIB_K


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
    CLTMP = REPEAT(' ',128)
    WRITE(CLTMP, '(I10)', IOSTAT=STATUS ) KSTEP
    PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'FLUSH STEP: '//TRIM(ADJUSTL(CLTMP)), HOOKS )
  ENDIF

  !> Encode the message (In case of multithreaded encodr, this is the point where the
  !> new encoding rules are committed)
  ! PP_TRYCALL(ERRFLAG_ENCODER_COMMIT) THIS%MULTIOM_ENCODER_%COMMIT( HOOKS )

  ! Send to MultIO the flush for the last step
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FLUSH_MULTIO) MULTIO_FLUSH_AND_TRIGGER_RESTART( THIS%MIO_, KSTEP, HOOKS )

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
    CASE (ERRFLAG_PROFILE_FLUSH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to profile the flush' )
    CASE (ERRFLAG_ENCODER_COMMIT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to commit the encoder' )
    CASE (ERRFLAG_UNABLE_TO_LOG_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the step' )
    CASE (ERRFLAG_UNABLE_TO_FLUSH_MULTIO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to flush MultIO' )
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

END FUNCTION GRIB_HEADER2MULTIO_FLUSH_STEP_AND_TRIGGER_RESTART
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
#define PP_PROCEDURE_NAME 'GRIB_HEADER2MULTIO_FINALISE'
PP_THREAD_SAFE FUNCTION GRIB_HEADER2MULTIO_FINALISE( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: PROFILE_MOD,       ONLY: PROFILE_END_SIMULATION
  USE :: LOG_INFO_MOD,      ONLY: LOG_CURR_TIME
  USE :: MULTIO_UTILS_MOD,  ONLY: MULTIO_FLUSH_END_OF_SIMULATION
  USE :: MULTIO_UTILS_MOD,  ONLY: MULTIO_DELETE
  USE :: LOG_UTILS_MOD,     ONLY: MAX_STR_LEN
  USE :: LOG_UTILS_MOD,     ONLY: BYTES_TO_STRING
  USE :: LOG_UTILS_MOD,     ONLY: TO_STRING

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_HEADER2MULTIO_OUTPUT_MANAGER_T), TARGET, INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),                                      INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  INTEGER(KIND=JPIB_K) :: BYTESIZE
  INTEGER(KIND=JPIB_K) :: SIZE
  CHARACTER(LEN=MAX_STR_LEN) :: CTMP

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PROFILE_FLUSH=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_LOG_TIME=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_CLOSE_LOG_FILE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FLUSH_END_OF_SIMULATION=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DELETE_MULTIO_HANDLE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DUMP_CACHE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COMPUTE_CACHE_SIZE=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_BYTES_TO_STRING=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TO_STRING=10_JPIB_K

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
    PP_TRYCALL(ERRFLAG_PROFILE_FLUSH) PROFILE_END_SIMULATION( THIS%PROFILE_DATA_, HOOKS )
  ENDIF

  ! If needed log message
  IF ( THIS%VERBOSE_ ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_LOG_TIME) LOG_CURR_TIME( THIS%LOG_UNIT_, 'FINALISATION', HOOKS )
    CLOSE( THIS%LOG_UNIT_, IOSTAT=STAT )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_CLOSE_LOG_FILE )
  ENDIF

  ! Dump encoding cache
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DUMP_CACHE) THIS%MULTIOM_ENCODER_%ENCODING_CACHE_DUMP( './', HOOKS )


  ! Print cache bytesize
  PP_TRYCALL(ERRFLAG_UNABLE_TO_COMPUTE_CACHE_SIZE) THIS%MULTIOM_ENCODER_%ENCODING_CACHE_BYTESIZE( BYTESIZE, HOOKS )
  PP_TRYCALL(ERRFLAG_BYTES_TO_STRING) BYTES_TO_STRING( BYTESIZE, CTMP, HOOKS )
  PP_LOG_STR( 'Cache byte-size is: '//TRIM(ADJUSTL(CTMP)) )

  ! Print cache size
  PP_TRYCALL(ERRFLAG_UNABLE_TO_COMPUTE_CACHE_SIZE) THIS%MULTIOM_ENCODER_%ENCODING_CACHE_SIZE( SIZE, HOOKS )
  PP_TRYCALL(ERRFLAG_TO_STRING) TO_STRING( SIZE, CTMP, HOOKS )
  PP_LOG_STR( 'Cache size is: '//TRIM(ADJUSTL(CTMP)) )

  ! Send to MultIO the flush for the last step
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FLUSH_END_OF_SIMULATION) MULTIO_FLUSH_END_OF_SIMULATION( THIS%MIO_, HOOKS )

  ! Delete the MultIO object
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DELETE_MULTIO_HANDLE) MULTIO_DELETE( THIS%MIO_, HOOKS )

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
    CASE (ERRFLAG_PROFILE_FLUSH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to profile the flush' )
    CASE (ERRFLAG_UNABLE_TO_LOG_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to log the current time' )
    CASE (ERRFLAG_UNABLE_CLOSE_LOG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close the log file' )
    CASE (ERRFLAG_UNABLE_TO_FLUSH_END_OF_SIMULATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to flush the end of simulation' )
    CASE (ERRFLAG_UNABLE_TO_DELETE_MULTIO_HANDLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to delete the MultIO handle' )
    CASE (ERRFLAG_UNABLE_TO_DUMP_CACHE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to dump the encoding cache' )
    CASE (ERRFLAG_UNABLE_TO_COMPUTE_CACHE_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to compute the encoding cache size' )
    CASE (ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the cache size' )
    CASE (ERRFLAG_BYTES_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert bytes to string' )
    CASE (ERRFLAG_TO_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert to string' )
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

END FUNCTION GRIB_HEADER2MULTIO_FINALISE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB_HEADER2MULTIO_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
