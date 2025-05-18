!>
!> @file grib2_section4_model_mod.F90
!>
!> @brief Module for managing GRIB2 Section 4 Model configuration operations.
!>
!> The `GRIB2_SECTION4_MODEL_MOD` module contains procedures to initialize, allocate,
!> preset, run, and clean up the resources associated with GRIB2 Section 4 model configuration objects.
!> This module provides thread-safe operations and includes extensive use of debugging,
!> logging, and tracing capabilities, making it robust for production and testing.
!>
!> The key operations covered by this module include:
!>   - Initialization of GRIB2 Section 4 model configuration objects.
!>   - Allocation of resources.
!>   - Presetting internal parameters.
!>   - Managing runtime operations based on input parameters.
!>   - Cleaning up and deallocating resources after use.
!>
!> @section interface
!>
!> The module exports the following procedures:
!>   - @see GRIB2_SECTION4_MODEL_INIT
!>   - @see GRIB2_SECTION4_MODEL_ALLOCATE
!>   - @see GRIB2_SECTION4_MODEL_PRESET
!>   - @see GRIB2_SECTION4_MODEL_RUNTIME
!>   - @see GRIB2_SECTION4_MODEL_TO_BE_ENCODED
!>   - @see GRIB2_SECTION4_MODEL_FREE
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>   - @dependency [TYPE] OM_CORE_MOD::TIME_HISTORY_T
!>   - @dependency [TYPE] OM_CORE_MOD::CURR_TIME_T
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib2_section4_model_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SECTION4_MODEL_MOD'
MODULE GRIB2_SECTION4_MODEL_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A
  USE :: ENUMERATORS_MOD,       ONLY: UNDEF_PARAM_E

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!>
!> @brief Type definition for GRIB2 Section 4 model configuration handler.
!>
!> The `GRIB2_SECTION4_MODEL_T` type extends the base class `GRIB_SECTION_BASE_A` and
!> provides concrete implementations of initialization, allocation, preset, runtime,
!> encoding checks, and cleanup operations for GRIB2 Section 4 model configuration objects.
!>
!> This type ensures that the required resources are properly managed through thread-safe,
!> non-overridable methods, providing robustness in both multi-threaded and single-threaded
!> environments.
!>
TYPE, EXTENDS(GRIB_SECTION_BASE_A) :: GRIB2_SECTION4_MODEL_T

  !> Default symbols visibility
  PRIVATE

  INTEGER(KIND=JPIB_K) :: GENERATING_PROCESS_IDENTIFIER_=UNDEF_PARAM_E

CONTAINS

  !>
  !> @brief Initializes the GRIB2 Section 4 model configuration object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The procedure starts from a yaml configuration file to construct the
  !> GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT => GRIB2_SECTION4_MODEL_INIT

  !>
  !> @brief Allocates resources for the GRIB2 Section 4 model configuration object.
  !>
  !> This procedure allocates memory and other necessary resources for
  !> the object based on provided parameters.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ALLOCATE => GRIB2_SECTION4_MODEL_ALLOCATE

  !>
  !> @brief Presets the parameters of the GRIB2 Section 4 model configuration object.
  !>
  !> This procedure configures the internal parameters of the object
  !> before runtime execution.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRESET => GRIB2_SECTION4_MODEL_PRESET

  !>
  !> @brief Manages the runtime execution of GRIB2 Section 4 model configuration operations.
  !>
  !> This procedure handles operations and computations during runtime,
  !> making use of model and metadata information.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: RUNTIME => GRIB2_SECTION4_MODEL_RUNTIME

  !>
  !> @brief Frees resources allocated for the GRIB2 Section 4 model configuration object.
  !>
  !> This procedure deallocates resources and performs cleanup after
  !> the object has been used.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => GRIB2_SECTION4_MODEL_FREE

END TYPE


!>
!> Public symbols (dataTypes)
PUBLIC :: GRIB2_SECTION4_MODEL_T

CONTAINS

!>
!> @brief Initializes GRIB2 Section 4 model configuration for a given object using the provided parameters.
!>
!> This function initializes a GRIB2 Section 4 model configuration object (`THIS`) using the provided model parameter (`MODEL`)
!> and configuration data (`CFG`). The process can be run in verbose mode if specified. The function
!> is thread-safe and returns an error code indicating the success or failure of the operation.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB2_SECTION4_MODEL_T` representing the GRIB section being initialized.
!>   @param [in]    CFG   The YAML configuration object of type `YAML_CONFIGURATION_T`.
!>   @param [in]    OPT   The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] HOOKS A structure of type `HOOKS_T` that contains hooks for initialization.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>
!> @susection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_SECTION4_MODEL_INIT
!> @see GRIB2_SECTION4_MODEL_ALLOCATE
!> @see GRIB2_SECTION4_MODEL_PRESET
!> @see GRIB2_SECTION4_MODEL_RUNTIME
!> @see GRIB2_SECTION4_MODEL_TO_BE_ENCODED
!> @see GRIB2_SECTION4_MODEL_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_MODEL_INIT'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_MODEL_INIT( THIS, &
&               CFG, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_READ_INTEGER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_MODEL_T),  INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Local variables
  LOGICAL :: HAS_GENERATING_PROCESS_IDENTIFIER
  INTEGER(KIND=JPIB_K) :: IO_ERROR

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=1_JPIB_K

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

  ! Initialise the section
  THIS%TYPE_ = 'CONFIGURATOR'
  THIS%SUBTYPE_ = 'MODEL'
  THIS%KIND_   = 'MODEL'


  ! Read the optional configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'generating-process-identifier', HAS_GENERATING_PROCESS_IDENTIFIER, HOOKS )

  ! Read the subCentre
  IF (HAS_GENERATING_PROCESS_IDENTIFIER) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( CFG, 'generating-process-identifier', THIS%GENERATING_PROCESS_IDENTIFIER_, HOOKS )
  ELSE
     THIS%GENERATING_PROCESS_IDENTIFIER_ = 16_JPIB_K
  END IF


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE ( ERRFLAG_UNABLE_TO_READ_CFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read cfg' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION4_MODEL_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Allocates resources for GRIB2 Section 4 model configuration using the provided parameters.
!>
!> This function allocates resources for a GRIB2 Section 4 model configuration object (`THIS`) using the provided model parameters (`MODEL`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the allocation process.
!>
!> @section interface
!>   @param [in]    THIS     An object of type `GRIB2_SECTION4_MODEL_T` representing the GRIB section to allocate resources for.
!>   @param [in]    MSG      All the mars keywords needed to describe the field `FORTRAN_MESSAGE_T`.
!>   @param [in]    PAR      All information outside mars keywords needed to describe the field `PARAMETRIZATION_T`.
!>   @param [in]    OPT      The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] METADATA A pointer to the metadata object of type `METADATA_BASE_A` used during allocation.
!>   @param [inout] HOOKS    A structure of type `HOOKS_T` that contains hooks for the allocation process.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_SECTION4_MODEL_ALLOCATE
!> @see GRIB2_SECTION4_MODEL_INIT
!> @see GRIB2_SECTION4_MODEL_PRESET
!> @see GRIB2_SECTION4_MODEL_RUNTIME
!> @see GRIB2_SECTION4_MODEL_TO_BE_ENCODED
!> @see GRIB2_SECTION4_MODEL_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_MODEL_ALLOCATE'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_MODEL_ALLOCATE( THIS, &
&  MSG, PAR, OPT,  METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_MODEL_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: IDX

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA, ERRFLAG_METADATA )

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(METADATA), ERRFLAG_METADATA )

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA, ERRFLAG_METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE ( ERRFLAG_METADATA )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error using metadata' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION4_MODEL_ALLOCATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Presets GRIB2 Section 4 model configuration using the provided parameters and message data.
!>
!> This function presets a GRIB2 Section 4 model configuration object (`THIS`) using the provided model parameter (`MODEL`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the preset operation.
!>
!> @section interface
!>   @param [in]    THIS     An object of type `GRIB2_SECTION4_MODEL_T` representing the GRIB section to be preset.
!>   @param [in]    MSG      The message object of type `FORTRAN_MESSAGE_T` used to handle preset-related messaging.
!>   @param [in]    PAR      The parametrization structure of type `PARAMETRIZATION_T` used for the preset operation.
!>   @param [in]    OPT      The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] METADATA A pointer to the metadata object of type `METADATA_BASE_A` used for presetting the section.
!>   @param [inout] HOOKS    A structure of type `HOOKS_T` that contains hooks for the preset operation.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_SECTION4_MODEL_PRESET
!> @see GRIB2_SECTION4_MODEL_ALLOCATE
!> @see GRIB2_SECTION4_MODEL_INIT
!> @see GRIB2_SECTION4_MODEL_RUNTIME
!> @see GRIB2_SECTION4_MODEL_TO_BE_ENCODED
!> @see GRIB2_SECTION4_MODEL_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_MODEL_PRESET'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_MODEL_PRESET( THIS, &
&  MSG, PAR, OPT,  METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIM_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,          ONLY: UNDEF_PARAM_E
  USE :: ENUMERATORS_MOD,          ONLY: IMODEL2CMODEL
  USE :: GRIB_API,                 ONLY: GRIB_SUCCESS
  USE :: GRIB_API,                 ONLY: GRIB_IS_DEFINED
  USE :: GRIB_API,                 ONLY: GRIB_GET
  USE :: GRIB_METADATA_MOD,        ONLY: GRIB_METADATA_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_MODEL_T),    INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Local variables
  CHARACTER(LEN=32)  :: TMPMODEL
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIM_K) :: HANDLE
  INTEGER(KIND=JPIM_K) :: IS_DEFINED
  INTEGER(KIND=JPIM_K) :: LOCAL_DEFINITION

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_MODEL=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CHECK_IS_DEFINED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_LOCAL_DEF=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_HANDLE=5_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA, ERRFLAG_METADATA )


  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(METADATA), ERRFLAG_METADATA )

  IF (MSG%MODEL.NE.UNDEF_PARAM_E) THEN
      SELECT TYPE( GRIB => METADATA)
          CLASS IS (GRIB_METADATA_T)
              ! Get the grib handle from the metadata
              PP_TRYCALL(ERRFLAG_GET_HANDLE) GRIB%GET_HANDLE( HANDLE, HOOKS )

              IS_DEFINED=0_JPIM_K
              KRET=GRIB_SUCCESS
              CALL GRIB_IS_DEFINED( HANDLE, 'localDefinitionNumber', IS_DEFINED, STATUS=KRET )
              PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS , ERRFLAG_UNABLE_TO_CHECK_IS_DEFINED )

              IF( IS_DEFINED .NE. 0_JPIM_K ) THEN
                  LOCAL_DEFINITION=0_JPIM_K
                  KRET=GRIB_SUCCESS
                  CALL GRIB_GET( HANDLE, 'localDefinitionNumber', LOCAL_DEFINITION, STATUS=KRET )
                  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS , ERRFLAG_UNABLE_TO_GET_LOCAL_DEF )

                  IF( LOCAL_DEFINITION.GT.0 ) THEN
                      PP_TRYCALL(ERRFLAG_CONVERT_MODEL) IMODEL2CMODEL(MSG%MODEL, TMPMODEL, HOOKS)
                      PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'model', TRIM(ADJUSTL(TMPMODEL)) )
                  ENDIF
              ENDIF
      END SELECT
  ENDIF

  IF ( PAR%GENERATING_PROCESS_IDENTIFIER .NE. UNDEF_PARAM_E ) THEN
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'generatingProcessIdentifier', PAR%GENERATING_PROCESS_IDENTIFIER )
  ELSE
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'generatingProcessIdentifier', THIS%GENERATING_PROCESS_IDENTIFIER_ )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA, ERRFLAG_METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE ( ERRFLAG_GET_HANDLE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error getting handle' )
    CASE ( ERRFLAG_UNABLE_TO_CHECK_IS_DEFINED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error checking is_defined(localDefinitionNumber)' )
    CASE ( ERRFLAG_UNABLE_TO_GET_LOCAL_DEF )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error getting localDefinitionNumber' )
    CASE ( ERRFLAG_METADATA )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error using metadata' )
    CASE ( ERRFLAG_CONVERT_MODEL )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error converting model' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION4_MODEL_PRESET
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Executes runtime processing for GRIB2 Section 4 model configuration using provided parameters, message data, and model history.
!>
!> This function performs runtime operations for a GRIB2 Section 4 model configuration object (`THIS`) using the provided model parameters (`MODEL`),
!> message structure (`MSG`), current time (`CURR_TIME`), time history (`TIME_HISTORY`), and metadata (`METADATA`).
!> The process can be run in verbose mode if specified. The function is thread-safe and returns an error code indicating
!> the success or failure of the runtime operation.
!>
!> @section interface
!>   @param [in]    THIS      An object of type `GRIB2_SECTION4_MODEL_T` representing the GRIB section for runtime execution.
!>   @param [in]    MSG       The message object of type `FORTRAN_MESSAGE_T` used to handle preset-related messaging.
!>   @param [in]    PAR       The parametrization structure of type `PARAMETRIZATION_T` used for the preset operation.
!>   @param [in]    TIME_HIST The time history object of type `TIME_HISTORY_T` providing historical time data.
!>   @param [in]    CURR_TIME The current time object of type `CURR_TIME_T` for the runtime phase.
!>   @param [in]    OPT       The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] METADATA  A pointer to the metadata object of type `METADATA_BASE_A` used during runtime.
!>   @param [inout] HOOKS     A structure of type `HOOKS_T` that contains hooks for runtime operations.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>   - @dependency [TYPE] OM_CORE_MOD::TIME_HISTORY_T
!>   - @dependency [TYPE] OM_CORE_MOD::CURR_TIME_T
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_SECTION4_MODEL_RUNTIME
!> @see GRIB2_SECTION4_MODEL_ALLOCATE
!> @see GRIB2_SECTION4_MODEL_INIT
!> @see GRIB2_SECTION4_MODEL_PRESET
!> @see GRIB2_SECTION4_MODEL_TO_BE_ENCODED
!> @see GRIB2_SECTION4_MODEL_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_MODEL_RUNTIME'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_MODEL_RUNTIME( THIS, &
&  MSG, PAR, OPT, METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIM_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,          ONLY: UNDEF_PARAM_E
  USE :: ENUMERATORS_MOD,          ONLY: IMODEL2CMODEL
  USE :: GRIB_API,                 ONLY: GRIB_SUCCESS
  USE :: GRIB_API,                 ONLY: GRIB_IS_DEFINED
  USE :: GRIB_API,                 ONLY: GRIB_GET
  USE :: GRIB_METADATA_MOD,        ONLY: GRIB_METADATA_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_MODEL_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Local
  CHARACTER(LEN=32) :: TMPMODEL
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIM_K) :: HANDLE
  INTEGER(KIND=JPIM_K) :: IS_DEFINED
  INTEGER(KIND=JPIM_K) :: LOCAL_DEFINITION

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_MODEL=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CHECK_IS_DEFINED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_LOCAL_DEF=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_HANDLE=5_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA, ERRFLAG_METADATA )

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(METADATA), ERRFLAG_METADATA )

  IF (MSG%MODEL.NE.UNDEF_PARAM_E) THEN
      SELECT TYPE( GRIB => METADATA)
          CLASS IS (GRIB_METADATA_T)
              ! Get the grib handle from the metadata
              PP_TRYCALL(ERRFLAG_GET_HANDLE) GRIB%GET_HANDLE( HANDLE, HOOKS )

              IS_DEFINED=0_JPIM_K
              KRET=GRIB_SUCCESS
              CALL GRIB_IS_DEFINED( HANDLE, 'localDefinitionNumber', IS_DEFINED, STATUS=KRET )
              PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS , ERRFLAG_UNABLE_TO_CHECK_IS_DEFINED )

              IF( IS_DEFINED .NE. 0_JPIM_K ) THEN
                  LOCAL_DEFINITION=0_JPIM_K
                  KRET=GRIB_SUCCESS
                  CALL GRIB_GET( HANDLE, 'localDefinitionNumber', LOCAL_DEFINITION, STATUS=KRET )
                  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS , ERRFLAG_UNABLE_TO_GET_LOCAL_DEF )

                  IF( LOCAL_DEFINITION.GT.0 ) THEN
                      PP_TRYCALL(ERRFLAG_CONVERT_MODEL) IMODEL2CMODEL(MSG%MODEL, TMPMODEL, HOOKS)
                      PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'model', TRIM(ADJUSTL(TMPMODEL)) )
                  ENDIF
              ENDIF
      END SELECT
  ENDIF


  IF ( PAR%GENERATING_PROCESS_IDENTIFIER .NE. UNDEF_PARAM_E ) THEN
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'generatingProcessIdentifier', PAR%GENERATING_PROCESS_IDENTIFIER )
  ELSE
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'generatingProcessIdentifier', THIS%GENERATING_PROCESS_IDENTIFIER_ )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA, ERRFLAG_METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
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
    CASE ( ERRFLAG_GET_HANDLE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error getting handle' )
    CASE ( ERRFLAG_UNABLE_TO_CHECK_IS_DEFINED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error checking is_defined(localDefinitionNumber)' )
    CASE ( ERRFLAG_UNABLE_TO_GET_LOCAL_DEF )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error getting localDefinitionNumber' )
    CASE ( ERRFLAG_METADATA )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error using metadata' )
    CASE ( ERRFLAG_CONVERT_MODEL )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error converting model' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION4_MODEL_RUNTIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees resources associated with GRIB2 Section 4 model configuration object.
!>
!> This function deallocates and cleans up resources associated with the GRIB2 Section 4 model configuration object (`THIS`).
!> The process can be run in verbose mode for additional output. The function is thread-safe and returns an
!> error code indicating the success or failure of the operation.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB2_SECTION4_MODEL_T` representing the GRIB section to be freed.
!>   @param [in]    OPT   The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_SECTION4_MODEL_INIT
!> @see GRIB2_SECTION4_MODEL_ALLOCATE
!> @see GRIB2_SECTION4_MODEL_PRESET
!> @see GRIB2_SECTION4_MODEL_RUNTIME
!> @see GRIB2_SECTION4_MODEL_TO_BE_ENCODED
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_MODEL_FREE'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_MODEL_FREE( THIS, OPT, DESTRUCTORS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_DESTRUCTOR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_MODEL_T),   INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  TYPE(GRIB_SECTION_DESTRUCTOR_T), INTENT(IN)    :: DESTRUCTORS
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

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

  ! Exit point (On success)
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

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION4_MODEL_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB2_SECTION4_MODEL_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
