!>
!> @file grib2_section1_reference_time_custom_mod.F90
!>
!> @brief Module for managing GRIB2 Section 4 time configuration operations.
!>
!> The `GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_MOD` module contains procedures to initialize, allocate,
!> preset, run, and clean up the resources associated with GRIB2 Section 4 time configuration objects.
!> This module provides thread-safe operations and includes extensive use of debugging,
!> logging, and tracing capabilities, making it robust for production and testing.
!>
!> The key operations covered by this module include:
!>   - Initialization of GRIB2 Section 4 time configuration objects.
!>   - Allocation of resources.
!>   - Presetting internal parameters.
!>   - Managing runtime operations based on input parameters.
!>   - Cleaning up and deallocating resources after use.
!>
!> @section interface
!>
!> The module exports the following procedures:
!>   - @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT
!>   - @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_ALLOCATE
!>   - @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_PRESET
!>   - @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_RUNTIME
!>   - @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_TO_BE_ENCODED
!>   - @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_FREE
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


#define PP_FILE_NAME 'grib2_section1_reference_time_custom_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_MOD'
MODULE GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,       ONLY: UNDEF_PARAM_E
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!>
!> @brief Type definition for GRIB2 Section 4 time configuration handler.
!>
!> The `GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T` type extends the base class `GRIB_SECTION_BASE_A` and
!> provides concrete implementations of initialization, allocation, preset, runtime,
!> encoding checks, and cleanup operations for GRIB2 Section 4 time configuration objects.
!>
!> This type ensures that the required resources are properly managed through thread-safe,
!> non-overridable methods, providing robustness in both multi-threaded and single-threaded
!> environments.
!>
TYPE, EXTENDS(GRIB_SECTION_BASE_A) :: GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T

  !> Default symbols visibility
  PRIVATE

  INTEGER(KIND=JPIB_K) :: DATE_=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: TIME_=0_JPIB_K
  INTEGER(KIND=JPIB_K) :: SIGNIFICANCE_OF_REFERENCE_TIME_=0_JPIB_K

CONTAINS

  !>
  !> @brief Initializes the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The procedure starts from a yaml configuration file to construct the
  !> GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT_CFG => GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT_CFG

  !>
  !> @brief Initializes the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The preocedure starts from a message and fro the parameters to construct
  !> the GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT_LAZY => GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT_LAZY

  !>
  !> @brief Allocates resources for the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure allocates memory and other necessary resources for
  !> the object based on provided parameters.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ALLOCATE => GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_ALLOCATE

  !>
  !> @brief Presets the parameters of the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure configures the internal parameters of the object
  !> before runtime execution.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRESET => GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_PRESET

  !>
  !> @brief Manages the runtime execution of GRIB2 Section 4 time configuration operations.
  !>
  !> This procedure handles operations and computations during runtime,
  !> making use of time and metadata information.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: RUNTIME => GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_RUNTIME

  !>
  !> @brief Determines if the GRIB2 Section 4 time configuration object needs to be encoded.
  !>
  !> This procedure checks whether the object should be encoded based
  !> on the provided parameters and internal state.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: TO_BE_ENCODED => GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_TO_BE_ENCODED

  !>
  !> @brief Frees resources allocated for the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure deallocates resources and performs cleanup after
  !> the object has been used.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_FREE

END TYPE


!>
!> Public symbols (dataTypes)
PUBLIC :: GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T

CONTAINS

!>
!> @brief Initializes GRIB2 Section 4 time configuration for a given object using the provided parameters.
!>
!> This function initializes a GRIB2 Section 4 time configuration object (`THIS`) using the provided model parameters (`PARAMS`)
!> and configuration data (`CFG`). The process can be run in verbose mode if specified. The function
!> is thread-safe and returns an error code indicating the success or failure of the operation.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T` representing the GRIB section being initialized.
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
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_ALLOCATE
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_PRESET
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_RUNTIME
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_TO_BE_ENCODED
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT_CFG'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT_CFG( THIS, &
&               CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_READ_INTEGER
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T),  INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_DATE
  LOGICAL :: HAS_TIME
  LOGICAL :: HAS_SIGNIFICANCE_OF_REFERENCE_TIME

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=1_JPIB_K

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
  THIS%SUBTYPE_ = 'REFERENCE_TIME'
  THIS%KIND_   = 'CUSTOM'


  ! Read the optional configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'date', HAS_DATE, HOOKS )

  ! Read the subCentre
  IF (HAS_DATE) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( CFG, 'date',  THIS%DATE_, HOOKS )
  ELSE
    THIS%DATE_ = 33_JPIB_K
  END IF


  ! Read the optional configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'time', HAS_TIME, HOOKS )

  ! Read the subCentre
  IF (HAS_TIME) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( CFG, 'time',  THIS%TIME_, HOOKS )
  ELSE
    THIS%TIME_ = 0_JPIB_K
  END IF


  ! Read the optional configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'significance-of-reference-time', HAS_SIGNIFICANCE_OF_REFERENCE_TIME, HOOKS )

  ! Read the subCentre
  IF (HAS_SIGNIFICANCE_OF_REFERENCE_TIME) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( CFG, 'significance-of-reference-time',  THIS%SIGNIFICANCE_OF_REFERENCE_TIME_, HOOKS )
  ELSE
    THIS%SIGNIFICANCE_OF_REFERENCE_TIME_ = 0_JPIB_K
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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read from configuration' )
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

END FUNCTION GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Initializes GRIB2 Section 4 time configuration for a given object using the provided parameters.
!>
!> This function initializes a GRIB2 Section 4 time configuration object (`THIS`) using the provided model parameters (`PARAMS`)
!> and configuration data (`CFG`). The process can be run in verbose mode if specified. The function
!> is thread-safe and returns an error code indicating the success or failure of the operation.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T` representing the GRIB section being initialized.
!>   @param [in]    MSG   All the mars keywords needed to describe the field `FORTRAN_MESSAGE_T`.
!>   @param [in]    PAR   All information outside mars keywords needed to describe the field `PARAMETRIZATION_T`.
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
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_ALLOCATE
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_PRESET
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_RUNTIME
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_TO_BE_ENCODED
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT_LAZY'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT_LAZY( THIS, &
&               MSG, PAR, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

#if defined( PP_HAS_GET_GRIB2_REFERENCE_TIME_CONFIGURATION_FROM_MESSAGE )
  !> Symbols imported from the mapping module
  USE :: MARS2GRIB_MAPPING_MOD, ONLY: GET_GRIB2_REFERENCE_TIME_CONFIGURATION_FROM_MESSAGE
#endif

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T),  INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ=1_JPIB_K

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
  THIS%SUBTYPE_ = 'REFERENCE_TIME'
  THIS%KIND_   = 'CUSTOM'


#if defined( PP_HAS_GET_GRIB2_REFERENCE_TIME_CONFIGURATION_FROM_MESSAGE )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ) GET_GRIB2_REFERENCE_TIME_CONFIGURATION_FROM_MESSAGE( &
&       MSG, PAR, OPT, THIS%TYPE_OF_REFERENCE_TIME_FORECAST_, THIS%PERTURBATION_NUMBER_, &
&       THIS%NUMBER_OF_FORECASTS_IN_REFERENCE_TIME_, HOOKS )
#else
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNABLE_TO_READ )
#endif


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
    CASE ( ERRFLAG_UNABLE_TO_READ )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read from message' )
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

END FUNCTION GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT_LAZY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Allocates resources for GRIB2 Section 4 time configuration using the provided parameters.
!>
!> This function allocates resources for a GRIB2 Section 4 time configuration object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the allocation process.
!>
!> @section interface
!>   @param [in]    THIS     An object of type `GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T` representing the GRIB section to allocate resources for.
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
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_ALLOCATE
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_PRESET
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_RUNTIME
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_TO_BE_ENCODED
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_ALLOCATE'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_ALLOCATE( THIS, &
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
  CLASS(GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T),     INTENT(INOUT) :: THIS
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

END FUNCTION GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_ALLOCATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Presets GRIB2 Section 4 time configuration using the provided parameters and message data.
!>
!> This function presets a GRIB2 Section 4 time configuration object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the preset operation.
!>
!> @section interface
!>   @param [in]    THIS     An object of type `GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T` representing the GRIB section to be preset.
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
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_PRESET
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_ALLOCATE
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_RUNTIME
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_TO_BE_ENCODED
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_PRESET'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_PRESET( THIS, &
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
  CLASS(GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

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

END FUNCTION GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_PRESET
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Executes runtime processing for GRIB2 Section 4 time configuration using provided parameters, message data, and time history.
!>
!> This function performs runtime operations for a GRIB2 Section 4 time configuration object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), current time (`CURR_TIME`), time history (`TIME_HISTORY`), and metadata (`METADATA`).
!> The process can be run in verbose mode if specified. The function is thread-safe and returns an error code indicating
!> the success or failure of the runtime operation.
!>
!> @section interface
!>   @param [in]    THIS      An object of type `GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T` representing the GRIB section for runtime execution.
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
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_RUNTIME
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_ALLOCATE
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_PRESET
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_TO_BE_ENCODED
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_RUNTIME'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_RUNTIME( THIS, &
&  MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: TIME_UTILS_MOD,           ONLY: TIME_HISTORY_T
  USE :: TIME_UTILS_MOD,           ONLY: CURR_TIME_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  USE :: ENUMERATORS_MOD, ONLY: TYPE_FC_E
  USE :: ENUMERATORS_MOD, ONLY: TYPE_AN_E
  USE :: DATETIME_UTILS_MOD, ONLY: YYYYMMDD_HHMMSS_TO_DATETIME

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: DATE_TIME

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNHANDLED_CASE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_DATETIME=3_JPIB_K

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

  ! Set the time
  IF ( MSG%TYPE .EQ. TYPE_FC_E .AND. MSG%STEP .GT. 0 ) THEN
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA,  'significanceOfReferenceTime', THIS%SIGNIFICANCE_OF_REFERENCE_TIME_ )
  ELSEIF ( (MSG%TYPE .EQ. TYPE_FC_E .AND. MSG%STEP .EQ. 0) .OR. MSG%TYPE .EQ. TYPE_AN_E ) THEN
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA,  'significanceOfReferenceTime', THIS%SIGNIFICANCE_OF_REFERENCE_TIME_ )
  ELSE
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNHANDLED_CASE )
  ENDIF

  ! When I generate the message (mars part of the metdatada), I need to take care of setting the date and time
  ! the proper way. Data and time represent the start of the forecast in "forecast type" (when step is greater than 0),
  ! or the current time in "analysis type".


  ! Unpack date time
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_DATETIME) YYYYMMDD_HHMMSS_TO_DATETIME( THIS%DATE_, THIS%TIME_, DATE_TIME, HOOKS )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'year',   DATE_TIME(1) )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'month',  DATE_TIME(2) )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'day',    DATE_TIME(3) )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'hour',   DATE_TIME(4) )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'minute', DATE_TIME(5) )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'second', DATE_TIME(6) )

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
    CASE ( ERRFLAG_UNHANDLED_CASE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled case' )
    CASE ( ERRFLAG_UNABLE_TO_CONVERT_DATETIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert date time' )
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

END FUNCTION GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_RUNTIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Prepares GRIB2 Section 4 time configuration for encoding based on provided parameters, message data, and time history.
!>
!> This function determines whether GRIB2 Section 4 time configuration (`THIS`) is ready to be encoded. It processes the provided model parameters
!> (`PARAMS`), message structure (`MSG`), current time (`CURR_TIME`), time history (`TIME_HISTORY`), and updates the
!> `TO_BE_ENCODED` flag accordingly. The function is thread-safe and returns an error code indicating the success or failure
!> of the operation. The process can also be run in verbose mode if specified.
!>
!> @section interface
!>   @param [inout] THIS          An object of type `GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T` representing the GRIB section being checked.
!>   @param [in]    MSG           The message object of type `FORTRAN_MESSAGE_T` used to handle preset-related messaging.
!>   @param [in]    PAR           The parametrization structure of type `PARAMETRIZATION_T` used for the preset operation.
!>   @param [in]    TIME_HIST     The time history object of type `TIME_HISTORY_T` providing historical time data.
!>   @param [in]    CURR_TIME     The current time object of type `CURR_TIME_T` for time-based encoding decisions.
!>   @param [in]    OPT           The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [out]   TO_BE_ENCODED Logical flag indicating if the GRIB section should be encoded.
!>   @param [inout] HOOKS         A structure of type `HOOKS_T` that contains hooks for managing encoding-related operations.
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
!>   - @dependency [TYPE] OM_CORE_MOD::TIME_HISTORY_T
!>   - @dependency [TYPE] OM_CORE_MOD::CURR_TIME_T
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_TO_BE_ENCODED
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_ALLOCATE
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_PRESET
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_RUNTIME
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_TO_BE_ENCODED'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_TO_BE_ENCODED( THIS, &
&  MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: TIME_UTILS_MOD,           ONLY: TIME_HISTORY_T
  USE :: TIME_UTILS_MOD,           ONLY: CURR_TIME_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T),  INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(TIME_HISTORY_T),         INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),            INTENT(IN)    :: CURR_TIME
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  LOGICAL,                      INTENT(OUT)   :: TO_BE_ENCODED
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

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

  ! Not condition applicable for section 4 time configuration to avoid encoding the field
  TO_BE_ENCODED = .TRUE.

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

END FUNCTION GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_TO_BE_ENCODED
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees resources associated with GRIB2 Section 4 time configuration object.
!>
!> This function deallocates and cleans up resources associated with the GRIB2 Section 4 time configuration object (`THIS`).
!> The process can be run in verbose mode for additional output. The function is thread-safe and returns an
!> error code indicating the success or failure of the operation.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T` representing the GRIB section to be freed.
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
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_INIT
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_ALLOCATE
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_PRESET
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_RUNTIME
!> @see GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_TO_BE_ENCODED
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_FREE'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_FREE( THIS, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T),  INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

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

END FUNCTION GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
