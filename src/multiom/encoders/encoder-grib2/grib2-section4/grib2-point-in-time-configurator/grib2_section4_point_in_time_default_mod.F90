!>
!> @file grib2_section4_point_in_time_default_mod.F90
!>
!> @brief Module for managing GRIB2 Section 4 time configuration operations.
!>
!> The `GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_MOD` module contains procedures to initialize, allocate,
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
!>   - @see G2S4_POINT_IN_TIME_DEFAULT_INIT
!>   - @see G2S4_POINT_IN_TIME_DEFAULT_ALLOCATE
!>   - @see G2S4_POINT_IN_TIME_DEFAULT_PRESET
!>   - @see G2S4_POINT_IN_TIME_DEFAULT_RUNTIME
!>   - @see G2S4_POINT_IN_TIME_DEFAULT_T
!>   - @see G2S4_POINT_IN_TIME_DEFAULT_FREE
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


#define PP_FILE_NAME 'grib2_section4_point_in_time_default_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_MOD'
MODULE GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_MOD

  !> Symbols imported from other modules within the project.
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!>
!> @brief Type definition for GRIB2 Section 4 time configuration handler.
!>
!> The `GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T` type extends the base class `GRIB_SECTION_BASE_A` and
!> provides concrete implementations of initialization, allocation, preset, runtime,
!> encoding checks, and cleanup operations for GRIB2 Section 4 time configuration objects.
!>
!> This type ensures that the required resources are properly managed through thread-safe,
!> non-overridable methods, providing robustness in both multi-threaded and single-threaded
!> environments.
!>
TYPE, EXTENDS(GRIB_SECTION_BASE_A) :: GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T

  !> Default symbols visibility
  PRIVATE

  !> Logical
  LOGICAL :: ENCODE_STEP_ZERO_ = .TRUE.

CONTAINS

  !>
  !> @brief Initializes the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The procedure starts from a yaml configuration file to construct the
  !> GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT_CFG => G2S4_POINT_IN_TIME_DEFAULT_INIT_CFG

  !>
  !> @brief Initializes the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The preocedure starts from a message and fro the parameters to construct
  !> the GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT_LAZY => G2S4_POINT_IN_TIME_DEFAULT_INIT_LAZY

  !>
  !> @brief Allocates resources for the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure allocates memory and other necessary resources for
  !> the object based on provided parameters.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ALLOCATE => G2S4_POINT_IN_TIME_DEFAULT_ALLOCATE

  !>
  !> @brief Presets the parameters of the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure configures the internal parameters of the object
  !> before runtime execution.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRESET => G2S4_POINT_IN_TIME_DEFAULT_PRESET

  !>
  !> @brief Manages the runtime execution of GRIB2 Section 4 time configuration operations.
  !>
  !> This procedure handles operations and computations during runtime,
  !> making use of time and metadata information.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: RUNTIME => G2S4_POINT_IN_TIME_DEFAULT_RUNTIME

  !>
  !> @brief Determines if the GRIB2 Section 4 time configuration object needs to be encoded.
  !>
  !> This procedure checks whether the object should be encoded based
  !> on the provided parameters and internal state.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: TO_BE_ENCODED => G2S4_POINT_IN_TIME_DEFAULT_T

  !>
  !> @brief Frees resources allocated for the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure deallocates resources and performs cleanup after
  !> the object has been used.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => G2S4_POINT_IN_TIME_DEFAULT_FREE

  !>
  !> @brief Print the configuration of the current object.
  !>
  !> This procedure print the configuration of the current object
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRINT => G2S4_POINT_IN_TIME_DEFAULT_PRINT

END TYPE


!>
!> Public symbols (dataTypes)
PUBLIC :: GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T

CONTAINS

!>
!> @brief Initializes GRIB2 Section 4 time configuration for a given object using the provided parameters.
!>
!> This function initializes a GRIB2 Section 4 time configuration object (`THIS`) using the provided model parameters (`PARAMS`)
!> and configuration data (`CFG`). The process can be run in verbose mode if specified. The function
!> is thread-safe and returns an error code indicating the success or failure of the operation.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T` representing the GRIB section being initialized.
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
!> @see G2S4_POINT_IN_TIME_DEFAULT_INIT
!> @see G2S4_POINT_IN_TIME_DEFAULT_ALLOCATE
!> @see G2S4_POINT_IN_TIME_DEFAULT_PRESET
!> @see G2S4_POINT_IN_TIME_DEFAULT_RUNTIME
!> @see G2S4_POINT_IN_TIME_DEFAULT_T
!> @see G2S4_POINT_IN_TIME_DEFAULT_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_POINT_IN_TIME_DEFAULT_INIT_CFG'
PP_THREAD_SAFE FUNCTION G2S4_POINT_IN_TIME_DEFAULT_INIT_CFG( THIS, &
&               CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_READ_LOGICAL
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T),  INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_EMIT_STEP_ZERO

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
  THIS%SUBTYPE_ = 'POINT-IN-TIME'
  THIS%KIND_   = 'DEFAULT'

  ! Read the optional configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'emit-step-zero', HAS_EMIT_STEP_ZERO, HOOKS )

  ! Set the flag
  IF (HAS_EMIT_STEP_ZERO) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_LOGICAL( CFG, 'emit-step-zero',  THIS%ENCODE_STEP_ZERO_, HOOKS )
  ELSE
    THIS%ENCODE_STEP_ZERO_ = .FALSE.
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

END FUNCTION G2S4_POINT_IN_TIME_DEFAULT_INIT_CFG
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
!>   @param [inout] THIS  An object of type `GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T` representing the GRIB section being initialized.
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
!> @see G2S4_POINT_IN_TIME_DEFAULT_INIT
!> @see G2S4_POINT_IN_TIME_DEFAULT_ALLOCATE
!> @see G2S4_POINT_IN_TIME_DEFAULT_PRESET
!> @see G2S4_POINT_IN_TIME_DEFAULT_RUNTIME
!> @see G2S4_POINT_IN_TIME_DEFAULT_T
!> @see G2S4_POINT_IN_TIME_DEFAULT_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_POINT_IN_TIME_DEFAULT_INIT_LAZY'
PP_THREAD_SAFE FUNCTION G2S4_POINT_IN_TIME_DEFAULT_INIT_LAZY( THIS, &
&               MSG, PAR, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

#if defined( PP_HAS_GET_GRIB2_POINT_IN_TIME_CONFIGURATOR_OPTIONS )
  !> Symbols imported from the mapping module
  USE :: MARS2GRIB_MAPPING_MOD, ONLY: GET_GRIB2_POINT_IN_TIME_CONFIGURATOR_OPTIONS
#endif

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T),  INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

   ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_OPTION=0_JPIB_K

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
  THIS%SUBTYPE_ = 'POINT-IN-TIME'
  THIS%KIND_   = 'DEFAULT'

  ! Get the optional value from the default configuration
#if defined( PP_HAS_GET_GRIB2_POINT_IN_TIME_CONFIGURATOR_OPTIONS )
  !> Symbols imported from the mapping module
  PP_TRYCALL(ERRFLAG_GET_OPTION) GET_GRIB2_POINT_IN_TIME_CONFIGURATOR_OPTIONS( THIS%ENCODE_STEP_ZERO_, HOOKS )
#else
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_GET_OPTION )
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
    CASE ( ERRFLAG_GET_OPTION )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error getting option' )
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

END FUNCTION G2S4_POINT_IN_TIME_DEFAULT_INIT_LAZY
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
!>   @param [in]    THIS     An object of type `GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T` representing the GRIB section to allocate resources for.
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
!> @see G2S4_POINT_IN_TIME_DEFAULT_ALLOCATE
!> @see G2S4_POINT_IN_TIME_DEFAULT_INIT
!> @see G2S4_POINT_IN_TIME_DEFAULT_PRESET
!> @see G2S4_POINT_IN_TIME_DEFAULT_RUNTIME
!> @see G2S4_POINT_IN_TIME_DEFAULT_T
!> @see G2S4_POINT_IN_TIME_DEFAULT_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_POINT_IN_TIME_DEFAULT_ALLOCATE'
PP_THREAD_SAFE FUNCTION G2S4_POINT_IN_TIME_DEFAULT_ALLOCATE( THIS, &
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
  CLASS(GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T),     INTENT(INOUT) :: THIS
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

  ! Initialise the section
  PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'hoursAfterDataCutoff' )
  PP_METADATA_SET_MISSING( METADATA, ERRFLAG_METADATA, 'minutesAfterDataCutoff' )

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

END FUNCTION G2S4_POINT_IN_TIME_DEFAULT_ALLOCATE
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
!>   @param [in]    THIS     An object of type `GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T` representing the GRIB section to be preset.
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
!> @see G2S4_POINT_IN_TIME_DEFAULT_PRESET
!> @see G2S4_POINT_IN_TIME_DEFAULT_ALLOCATE
!> @see G2S4_POINT_IN_TIME_DEFAULT_INIT
!> @see G2S4_POINT_IN_TIME_DEFAULT_RUNTIME
!> @see G2S4_POINT_IN_TIME_DEFAULT_T
!> @see G2S4_POINT_IN_TIME_DEFAULT_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_POINT_IN_TIME_DEFAULT_PRESET'
PP_THREAD_SAFE FUNCTION G2S4_POINT_IN_TIME_DEFAULT_PRESET( THIS, &
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
  CLASS(GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T),     INTENT(INOUT) :: THIS
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

  !> Preset everything that can be preset
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'indicatorOfUnitOfTimeRange', 'h' )

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

END FUNCTION G2S4_POINT_IN_TIME_DEFAULT_PRESET
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
!>   @param [in]    THIS      An object of type `GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T` representing the GRIB section for runtime execution.
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
!> @see G2S4_POINT_IN_TIME_DEFAULT_RUNTIME
!> @see G2S4_POINT_IN_TIME_DEFAULT_ALLOCATE
!> @see G2S4_POINT_IN_TIME_DEFAULT_INIT
!> @see G2S4_POINT_IN_TIME_DEFAULT_PRESET
!> @see G2S4_POINT_IN_TIME_DEFAULT_T
!> @see G2S4_POINT_IN_TIME_DEFAULT_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_POINT_IN_TIME_DEFAULT_RUNTIME'
PP_THREAD_SAFE FUNCTION G2S4_POINT_IN_TIME_DEFAULT_RUNTIME( THIS, &
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

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
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

  ! Set the forecast time
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA,  'forecastTime',  CURR_TIME%ISEC_/3600 )

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

END FUNCTION G2S4_POINT_IN_TIME_DEFAULT_RUNTIME
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
!>   @param [inout] THIS          An object of type `GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T` representing the GRIB section being checked.
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
!> @see G2S4_POINT_IN_TIME_DEFAULT_T
!> @see G2S4_POINT_IN_TIME_DEFAULT_INIT
!> @see G2S4_POINT_IN_TIME_DEFAULT_ALLOCATE
!> @see G2S4_POINT_IN_TIME_DEFAULT_PRESET
!> @see G2S4_POINT_IN_TIME_DEFAULT_RUNTIME
!> @see G2S4_POINT_IN_TIME_DEFAULT_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_POINT_IN_TIME_DEFAULT_T'
PP_THREAD_SAFE FUNCTION G2S4_POINT_IN_TIME_DEFAULT_T( THIS, &
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
  CLASS(GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T),  INTENT(INOUT) :: THIS
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
  IF ( THIS%ENCODE_STEP_ZERO_ .AND. MSG%STEP .EQ. 0_JPIB_K ) THEN
    TO_BE_ENCODED = .FALSE.
  ELSE
    TO_BE_ENCODED = .TRUE.
  ENDIF

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

END FUNCTION G2S4_POINT_IN_TIME_DEFAULT_T
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
!>   @param [inout] THIS  An object of type `GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T` representing the GRIB section to be freed.
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
!> @see G2S4_POINT_IN_TIME_DEFAULT_INIT
!> @see G2S4_POINT_IN_TIME_DEFAULT_ALLOCATE
!> @see G2S4_POINT_IN_TIME_DEFAULT_PRESET
!> @see G2S4_POINT_IN_TIME_DEFAULT_RUNTIME
!> @see G2S4_POINT_IN_TIME_DEFAULT_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_POINT_IN_TIME_DEFAULT_FREE'
PP_THREAD_SAFE FUNCTION G2S4_POINT_IN_TIME_DEFAULT_FREE( THIS, OPT, HOOKS ) RESULT(RET)

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
  CLASS(GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T),  INTENT(INOUT) :: THIS
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

END FUNCTION G2S4_POINT_IN_TIME_DEFAULT_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_POINT_IN_TIME_DEFAULT_PRINT'
PP_THREAD_SAFE FUNCTION G2S4_POINT_IN_TIME_DEFAULT_PRINT( THIS, &
& UNIT, OFFSET, OPT, HOOKS, SEPARATOR ) RESULT(RET)

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
  CLASS(GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: OFFSET
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=*), OPTIONAL,   INTENT(IN)    :: SEPARATOR

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STAT

   ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_ERROR=3_JPIB_K

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

  ! Write the section information
  IF ( OFFSET .LE. 0 ) THEN
    WRITE(UNIT,'(A,A,A,A,A,A,A)', IOSTAT=WRITE_STAT) 'GRIB_SECTION: ', &
&     TRIM(ADJUSTL(THIS%TYPE_)), '::', TRIM(ADJUSTL(THIS%SUBTYPE_)), &
&     '(', TRIM(ADJUSTL(THIS%KIND_)), '){'
    PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
  ELSE
    WRITE(UNIT,'(A,A,A,A,A,A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET), &
&     'GRIB_SECTION: ', TRIM(ADJUSTL(THIS%TYPE_)), '::', TRIM(ADJUSTL(THIS%SUBTYPE_)), &
&     '(', TRIM(ADJUSTL(THIS%KIND_)), '){'
    PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
  ENDIF

  ! Print of nested sections
  WRITE(UNIT,'(A,A,L)', IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET+2),'OPTION::EMIT_STEP_ZERO :: ', THIS%ENCODE_STEP_ZERO_
  PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )

  ! Close section
  IF ( PRESENT(SEPARATOR) ) THEN
    IF ( OFFSET .LE. 0 ) THEN
      WRITE(UNIT,'(A,A)', IOSTAT=WRITE_STAT) '}', TRIM(ADJUSTL(SEPARATOR))
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
    ELSE
      WRITE(UNIT,'(A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET), '}', TRIM(ADJUSTL(SEPARATOR))
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
    ENDIF
  ELSE
    IF ( OFFSET .LE. 0 ) THEN
      WRITE(UNIT,'(A)', IOSTAT=WRITE_STAT) '}'
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
    ELSE
      WRITE(UNIT,'(A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET), '}'
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
    ENDIF
  ENDIF

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
    CASE ( ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'paramId configurator not associated' )
    CASE (ERRFLAG_WRITE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error writing to given unit' )
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

END FUNCTION G2S4_POINT_IN_TIME_DEFAULT_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB2_SECTION4_POINT_IN_TIME_DEFAULT_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
