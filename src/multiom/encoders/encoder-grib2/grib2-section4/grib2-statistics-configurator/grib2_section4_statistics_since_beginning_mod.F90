!>
!> @file grib2_sECTION4_STATISTICS_SINCE_BEGINNING_mod.F90
!>
!> @brief Module for managing GRIB2 Section 4 time configuration operations.
!>
!> The `GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_MOD` module contains procedures to initialize, allocate,
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
!>   - @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT
!>   - @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_ALLOCATE
!>   - @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRESET
!>   - @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_RUNTIME
!>   - @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_TO_BE_ENCODED
!>   - @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_FREE
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


#define PP_FILE_NAME 'grib2_section4_statistics_since_beginning_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_MOD'
MODULE GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A
  USE :: ENUMERATORS_MOD,       ONLY: TYPE_OF_STATISTICAL_PROCESS_MISSING_E
IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!>
!> @brief Type definition for GRIB2 Section 4 time configuration handler.
!>
!> The `GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T` type extends the base class `GRIB_SECTION_BASE_A` and
!> provides concrete implementations of initialization, allocation, preset, runtime,
!> encoding checks, and cleanup operations for GRIB2 Section 4 time configuration objects.
!>
!> This type ensures that the required resources are properly managed through thread-safe,
!> non-overridable methods, providing robustness in both multi-threaded and single-threaded
!> environments.
!>
TYPE, EXTENDS(GRIB_SECTION_BASE_A) :: GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T

  !> Default symbols visibility
  PRIVATE

  !> Type-specific fields
  INTEGER(KIND=JPIB_K) :: NUMBER_OF_TIME_RANGES_ = -1_JPIB_K
  INTEGER(KIND=JPIB_K) :: OUTER_TYPE_OF_STATISTICAL_PROCESS_ = TYPE_OF_STATISTICAL_PROCESS_MISSING_E
  LOGICAL :: ENCODE_STEP_ZERO_ = .FALSE.

  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: TYPE_OF_STATISTICAL_PROCESS_
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS_

CONTAINS

  !>
  !> @brief Initializes the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The procedure starts from a yaml configuration file to construct the
  !> GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT_CFG => GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT_CFG

  !>
  !> @brief Initializes the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The preocedure starts from a message and fro the parameters to construct
  !> the GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT_LAZY => GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT_LAZY

  !>
  !> @brief Allocates resources for the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure allocates memory and other necessary resources for
  !> the object based on provided parameters.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ALLOCATE => GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_ALLOCATE

  !>
  !> @brief Presets the parameters of the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure configures the internal parameters of the object
  !> before runtime execution.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRESET => GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRESET

  !>
  !> @brief Manages the runtime execution of GRIB2 Section 4 time configuration operations.
  !>
  !> This procedure handles operations and computations during runtime,
  !> making use of time and metadata information.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: RUNTIME => GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_RUNTIME

  !>
  !> @brief Determines if the GRIB2 Section 4 time configuration object needs to be encoded.
  !>
  !> This procedure checks whether the object should be encoded based
  !> on the provided parameters and internal state.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: TO_BE_ENCODED => GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_TO_BE_ENCODED

  !>
  !> @brief Prints the GRIB2 Section 4 time configuration object needs to be encoded.
  !>
  !> This procedure prints the object based on the provided parameters and internal state.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRINT => GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRINT

  !>
  !> @brief Frees resources allocated for the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure deallocates resources and performs cleanup after
  !> the object has been used.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_FREE

  !>
  !> @brief Frees resources allocated for the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure deallocates resources and performs cleanup after
  !> the object has been used.
  !>
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: READ_FROM_CFG => GRIB2_SECTION4_READ_FROM_CFG

END TYPE


!>
!> Public symbols (dataTypes)
PUBLIC :: GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T

CONTAINS

!>
!> @brief Initializes GRIB2 Section 4 time configuration for a given object using the provided parameters.
!>
!> This function initializes a GRIB2 Section 4 time configuration object (`THIS`) using the provided model parameters (`PARAMS`)
!> and configuration data (`CFG`). The process can be run in verbose mode if specified. The function
!> is thread-safe and returns an error code indicating the success or failure of the operation.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T` representing the GRIB section being initialized.
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
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_ALLOCATE
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRESET
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_RUNTIME
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_TO_BE_ENCODED
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT_CFG'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT_CFG( THIS, &
&               CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T),  INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
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
  THIS%SUBTYPE_ = 'TIME-STATISTICS'
  THIS%KIND_   = 'SINCE_BEGINNING'

  ! Time, level and paramId subcomponents of the section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ) THIS%READ_FROM_CFG( CFG, OPT, HOOKS )

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

END FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT_CFG
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
!>   @param [inout] THIS  An object of type `GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T` representing the GRIB section being initialized.
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
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_ALLOCATE
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRESET
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_RUNTIME
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_TO_BE_ENCODED
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT_LAZY'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT_LAZY( THIS, &
&               MSG, PAR, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

#if defined( PP_HAS_GET_GRIB2_SINCE_BEGINNING_CONFIGURATION_ID_FROM_MESSAGE )
  !> Symbols imported from the mapping module
  USE :: MARS2GRIB_MAPPING_MOD, ONLY: GET_GRIB2_SINCE_BEGINNING_CONFIGURATION_ID_FROM_MESSAGE
#endif

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T),  INTENT(INOUT) :: THIS
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
  THIS%SUBTYPE_ = 'TIME-STATISTICS'
  THIS%KIND_   = 'SINCE_BEGINNING'

#if defined( PP_HAS_GET_GRIB2_SINCE_BEGINNING_CONFIGURATION_ID_FROM_MESSAGE )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ) GET_GRIB2_SINCE_BEGINNING_CONFIGURATION_ID_FROM_MESSAGE( &
&       OUTER_TYPE_OF_STATISTICAL_PROCESS_, ENCODE_STEP_ZERO_, &
&       MSG, PAR, OPT, THIS%NUMBER_OF_TIME_RANGES_, THIS%TYPE_OF_STATISTICAL_PROCESS_, &
&       THIS%OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS_, HOOKS )
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

END FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT_LAZY
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
!>   @param [in]    THIS     An object of type `GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T` representing the GRIB section to allocate resources for.
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
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_ALLOCATE
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRESET
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_RUNTIME
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_TO_BE_ENCODED
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_ALLOCATE'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_ALLOCATE( THIS, &
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
  CLASS(GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CONFIGURATION=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TO_BE_IMPLEMENTED=5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( THIS%NUMBER_OF_TIME_RANGES_.LT.1, ERRFLAG_INVALID_CONFIGURATION )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%NUMBER_OF_TIME_RANGES_.GT.1, ERRFLAG_TO_BE_IMPLEMENTED )

  ! Allocate the time ranges
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'numberOfTimeRange', THIS%NUMBER_OF_TIME_RANGES_ )

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
    CASE (ERRFLAG_INVALID_CONFIGURATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error invalid configuration' )
    CASE ( ERRFLAG_METADATA )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error using metadata' )
    CASE ( ERRFLAG_TO_BE_IMPLEMENTED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error to be implemented' )
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

END FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_ALLOCATE
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
!>   @param [in]    THIS     An object of type `GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T` representing the GRIB section to be preset.
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
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRESET
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_ALLOCATE
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_RUNTIME
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_TO_BE_ENCODED
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRESET'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRESET( THIS, &
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
  CLASS(GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CONFIGURATION=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TO_BE_IMPLEMENTED=5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( THIS%NUMBER_OF_TIME_RANGES_.LT.1, ERRFLAG_INVALID_CONFIGURATION )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%NUMBER_OF_TIME_RANGES_.GT.1, ERRFLAG_TO_BE_IMPLEMENTED )

  !> Preset everything that can be preset
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'typeOfStatisticalProcessing', THIS%OUTER_TYPE_OF_STATISTICAL_PROCESS_ )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'indicatorOfUnitOfTimeRange', 'h' )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'indicatorOfUnitForTimeRange', 'h' )

  !> @note See table 4.11 in the GRIB2 documentation for the following values
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'typeOfTimeIncrement',  2_JPIB_K )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'indicatorOfUnitForTimeIncrement',  's' )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'timeIncrement', INT(PAR%TIME%LENGTH_OF_TIME_STEP_IN_SECONDS_, KIND=JPIB_K)  )

  ! TODO: Implement the inner loops

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
    CASE ( ERRFLAG_INVALID_CONFIGURATION )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error invalid configuration' )
    CASE ( ERRFLAG_TO_BE_IMPLEMENTED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error to be implemented' )
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

END FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRESET
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
!>   @param [in]    THIS      An object of type `GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T` representing the GRIB section for runtime execution.
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
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_RUNTIME
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_ALLOCATE
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRESET
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_TO_BE_ENCODED
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_RUNTIME'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_RUNTIME( THIS, &
&  MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,             ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,             ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD,             ONLY: JPRD_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,      ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,           ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,           ONLY: PARAMETRIZATION_T
  USE :: TIME_UTILS_MOD,                ONLY: TIME_HISTORY_T
  USE :: TIME_UTILS_MOD,                ONLY: CURR_TIME_T
  USE :: METADATA_BASE_MOD,             ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                     ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,               ONLY: TYPE_FC_E
  USE :: GRIB2_SECTION4_STATISTICS_UTILS_MOD, ONLY: COMPUTE_TIME_SINCE_START
  USE :: GRIB2_SECTION4_STATISTICS_UTILS_MOD, ONLY: SET_END_OF_TIME_INTERVAL


  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T),     INTENT(INOUT) :: THIS
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
  INTEGER(KIND=JPIB_K) :: TIME_SINCE_START
  INTEGER(KIND=JPIB_K) :: FORECAST_TIME
  INTEGER(KIND=JPIB_K) :: LOC_LENGTH_OF_TIME_RANGE
  INTEGER(KIND=JPIM_K) :: KRET
  REAL(KIND=C_DOUBLE)  :: JDT

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CONFIGURATION=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FIELD=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TO_BE_IMPLEMENTED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_TO_COMPUTE_END_OF_TIME_INTERVAL=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_COMPUTE_TIME_SINCE_START=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_FC=9_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( THIS%NUMBER_OF_TIME_RANGES_.LT.1, ERRFLAG_INVALID_CONFIGURATION )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%NUMBER_OF_TIME_RANGES_.GT.1, ERRFLAG_TO_BE_IMPLEMENTED )
  PP_DEBUG_CRITICAL_COND_THROW( MSG%TYPE.NE.TYPE_FC_E, ERRFLAG_NO_FC )

  ! Compute the current time
  PP_TRYCALL(ERRFLAG_COMPUTE_TIME_SINCE_START) COMPUTE_TIME_SINCE_START( &
&        MSG, PAR, TIME_HIST, CURR_TIME, OPT, TIME_SINCE_START, HOOKS )

  ! Compute the forecast time
  FORECAST_TIME = 0_JPIB_K

  !> Should be equal to the OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS, otherwise the field should not be encoded
  LOC_LENGTH_OF_TIME_RANGE = TIME_SINCE_START - FORECAST_TIME

  !> Set the current point in time for the current loop
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'forecastTime',  FORECAST_TIME/3600 )
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'lengthOfTimeRange',  LOC_LENGTH_OF_TIME_RANGE/3600 )

  ! Unpack date time
  PP_TRYCALL(ERRFLAG_UNABLE_TO_TO_COMPUTE_END_OF_TIME_INTERVAL) SET_END_OF_TIME_INTERVAL( MSG, PAR, &
&            TIME_HIST, CURR_TIME, TIME_SINCE_START, OPT, METADATA, HOOKS )

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
    CASE ( ERRFLAG_COMPUTE_TIME_SINCE_START )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to compute time since start' )
    CASE ( ERRFLAG_METADATA )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error using metadata' )
    CASE ( ERRFLAG_INVALID_FIELD )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error invalid field' )
    CASE ( ERRFLAG_UNABLE_TO_TO_COMPUTE_END_OF_TIME_INTERVAL )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to compute end of time interval' )
    CASE ( ERRFLAG_NO_FC )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'no forecast time' )
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

END FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_RUNTIME
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
!>   @param [inout] THIS          An object of type `GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T` representing the GRIB section being checked.
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
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_TO_BE_ENCODED
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_ALLOCATE
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRESET
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_RUNTIME
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_TO_BE_ENCODED'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_TO_BE_ENCODED( THIS, &
&  MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,             ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,      ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,           ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,           ONLY: PARAMETRIZATION_T
  USE :: TIME_UTILS_MOD,                ONLY: TIME_HISTORY_T
  USE :: TIME_UTILS_MOD,                ONLY: CURR_TIME_T
  USE :: HOOKS_MOD,                     ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,               ONLY: TYPE_FC_E
  USE :: GRIB2_SECTION4_STATISTICS_UTILS_MOD, ONLY: COMPUTE_TIME_SINCE_START

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T),  INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(TIME_HISTORY_T),         INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),            INTENT(IN)    :: CURR_TIME
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  LOGICAL,                      INTENT(OUT)   :: TO_BE_ENCODED
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: FORECAST_TIME
  INTEGER(KIND=JPIB_K) :: TIME_SINCE_START

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_COMPUTE_TIME_SINCE_START=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CONFIGURATION=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TO_BE_IMPLEMENTED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NO_FC=4_JPIB_K

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

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( THIS%NUMBER_OF_TIME_RANGES_.LT.1, ERRFLAG_INVALID_CONFIGURATION )
  PP_DEBUG_CRITICAL_COND_THROW( THIS%NUMBER_OF_TIME_RANGES_.GT.1, ERRFLAG_TO_BE_IMPLEMENTED )
  PP_DEBUG_CRITICAL_COND_THROW( MSG%TYPE.NE.TYPE_FC_E, ERRFLAG_NO_FC )

  ! Compute the current time
  PP_TRYCALL(ERRFLAG_COMPUTE_TIME_SINCE_START) COMPUTE_TIME_SINCE_START( &
&        MSG, PAR, TIME_HIST, CURR_TIME, OPT, TIME_SINCE_START, HOOKS )

  ! Compute the forecast time
  FORECAST_TIME = TIME_SINCE_START ! -THIS%OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS_(1)

  IF ( FORECAST_TIME .LE. 0_JPIB_K ) THEN
    IF ( THIS%ENCODE_STEP_ZERO_ .AND. FORECAST_TIME .EQ. 0 ) THEN
      PP_LOG_STR( 'To be encoded: 0' )
      TO_BE_ENCODED = .TRUE.
    ELSE
      PP_LOG_STR( 'To be encoded: 1' )
      TO_BE_ENCODED = .FALSE.
    ENDIF
  ELSE
    PP_LOG_STR( 'To be encoded: 2' )
    TO_BE_ENCODED = .TRUE.
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
    CASE ( ERRFLAG_COMPUTE_TIME_SINCE_START )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to compute time since start' )
    CASE ( ERRFLAG_INVALID_CONFIGURATION )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error invalid configuration' )
    CASE ( ERRFLAG_TO_BE_IMPLEMENTED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error to be implemented' )
    CASE ( ERRFLAG_NO_FC )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error no forecast time' )
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

END FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_TO_BE_ENCODED
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Print informations related to the grib section
!>
!> @section interface
!>   @param [inout] THIS   An object of type `GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T` representing the GRIB section to be freed.
!>   @param [in]    UNIT   The unit number to print the information.
!>   @param [in]    OFFSET The offset to print the information.
!>   @param [in]    OPT    The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] PARAMETRIZATION_MOD::PARAMETRIZATION_T
!>   - @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>
!> @section intrinsic dependencies
!>   None.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRINT'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRINT( THIS, &
& UNIT, OFFSET, OPT, HOOKS, SEPARATOR ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: ENUMERATORS_MOD,          ONLY: ITYPE_OF_STATISTICAL_PROCESS2CTYPE_OF_STATISTICAL_PROCESS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T),   INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: OFFSET
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=*), OPTIONAL,   INTENT(IN)    :: SEPARATOR

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  INTEGER(KIND=JPIB_K) :: LOC_OFFSET
  CHARACTER(LEN=32) :: CTMP
  CHARACTER(LEN=16) :: CTYPE_OF_STATISTICAL_PROCESS

   ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_ERROR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENUM2STRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CONFIGURATION=4_JPIB_K

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


  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( THIS%NUMBER_OF_TIME_RANGES_.LT.1, ERRFLAG_INVALID_CONFIGURATION )

  ! Write the section information
  IF ( OFFSET .LE. 0 ) THEN
    WRITE(UNIT,'(A,A,A,A,A,A,A)', IOSTAT=WRITE_STAT) 'GRIB_SECTION: ', &
&   TRIM(ADJUSTL(THIS%TYPE_)), '::', TRIM(ADJUSTL(THIS%SUBTYPE_)), '(', &
&   TRIM(ADJUSTL(THIS%KIND_)), '){'
    PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
  ELSE
    WRITE(UNIT,'(A,A,A,A,A,A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET), &
&  'GRIB_SECTION: ', TRIM(ADJUSTL(THIS%TYPE_)), '::', TRIM(ADJUSTL(THIS%SUBTYPE_)), &
&  '(', TRIM(ADJUSTL(THIS%KIND_)), '){'
    PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
  ENDIF

  LOC_OFFSET=OFFSET+2



  PP_TRYCALL(ERRFLAG_ENUM2STRING) ITYPE_OF_STATISTICAL_PROCESS2CTYPE_OF_STATISTICAL_PROCESS( THIS%OUTER_TYPE_OF_STATISTICAL_PROCESS_, CTYPE_OF_STATISTICAL_PROCESS, HOOKS )
  WRITE(UNIT,'(A,A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',LOC_OFFSET), &
&   'OUTER_TYPE_OF_STATISTICAL_PROCESS: "', TRIM(ADJUSTL(CTYPE_OF_STATISTICAL_PROCESS)), '"'
  PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )


  CTMP=REPEAT(' ',32)
  WRITE(CTMP,*, IOSTAT=WRITE_STAT) THIS%ENCODE_STEP_ZERO_
  WRITE(UNIT,'(A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',LOC_OFFSET), &
&  'ENCODE_STEP_ZERO: ', TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )



  IF ( THIS%NUMBER_OF_TIME_RANGES_ .GT. 1_JPIB_K ) THEN
    CTMP=REPEAT(' ',32)
    WRITE(CTMP,*, IOSTAT=WRITE_STAT) THIS%NUMBER_OF_TIME_RANGES_
    WRITE(UNIT,'(A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',LOC_OFFSET), &
&    'NUMBER OF TIME RANGES: ', TRIM(ADJUSTL(CTMP))
    PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )

    WRITE(UNIT,'(A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',LOC_OFFSET), &
&    'TIME RANGES: {'
    PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
    LOC_OFFSET=OFFSET+2

    DO I = 1, THIS%NUMBER_OF_TIME_RANGES_
      CTMP=REPEAT(' ',32)
      WRITE(CTMP,*, IOSTAT=WRITE_STAT) I
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
      WRITE(UNIT,'(A,A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',LOC_OFFSET), &
&      '- TIME_RANGE[', TRIM(ADJUSTL(CTMP)), ']'
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )

      PP_TRYCALL(ERRFLAG_ENUM2STRING) ITYPE_OF_STATISTICAL_PROCESS2CTYPE_OF_STATISTICAL_PROCESS( THIS%TYPE_OF_STATISTICAL_PROCESS_(I), CTYPE_OF_STATISTICAL_PROCESS, HOOKS )
      WRITE(UNIT,'(A,A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',LOC_OFFSET), &
&      '  TYPE_OF_STATISTICAL_PROCESS: "', TRIM(ADJUSTL(CTYPE_OF_STATISTICAL_PROCESS)), '"'
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )

      CTMP=REPEAT(' ',32)
      WRITE(CTMP,*, IOSTAT=WRITE_STAT) THIS%OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS_(I)
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
      WRITE(UNIT,'(A,A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',LOC_OFFSET), &
&      '  OVERALL_LENGTH_OF_TIMERANGE: ', TRIM(ADJUSTL(CTMP)), 's'
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )

      IF ( I .LT. THIS%NUMBER_OF_TIME_RANGES_ ) THEN
        WRITE(UNIT,'(A)', IOSTAT=WRITE_STAT) REPEAT(' ',LOC_OFFSET)
        PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
      ELSE
        WRITE(UNIT,'(A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',LOC_OFFSET), '}'
        PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
      ENDIF

    ENDDO
  ENDIF



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
    CASE (ERRFLAG_WRITE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error writing to given unit' )
    CASE (ERRFLAG_ENUM2STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error converting enum to string' )
    CASE (ERRFLAG_INVALID_CONFIGURATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error invalid configuration' )
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

END FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRINT
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
!>   @param [inout] THIS  An object of type `GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T` representing the GRIB section to be freed.
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
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_ALLOCATE
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRESET
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_RUNTIME
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_TO_BE_ENCODED
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_FREE'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_FREE( THIS, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: ENUMERATORS_MOD,          ONLY: UNDEF_PARAM_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_OF_STATISTICAL_PROCESS_MISSING_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T),  INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOC=1_JPIB_K

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

  ! Deallocate memory
  THIS%NUMBER_OF_TIME_RANGES_ = UNDEF_PARAM_E
  THIS%OUTER_TYPE_OF_STATISTICAL_PROCESS_ = TYPE_OF_STATISTICAL_PROCESS_MISSING_E
  THIS%ENCODE_STEP_ZERO_ = .FALSE.

  IF ( ALLOCATED(THIS%TYPE_OF_STATISTICAL_PROCESS_) ) THEN
    DEALLOCATE( THIS%TYPE_OF_STATISTICAL_PROCESS_, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_DEALLOC )
  END IF

  IF ( ALLOCATED(THIS%OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS_) ) THEN
    DEALLOCATE( THIS%OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS_, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_DEALLOC )
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
    CASE ( ERRFLAG_DEALLOC )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating memory' )
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

END FUNCTION GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE







!>
!> @brief REad the configuration parameters of this class from a CFG
!>
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T` representing the GRIB section to be freed.
!>   @param [in]    CFG   The encoder options structure of type `ENCODER_OPTIONS_T`.
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
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_INIT
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_ALLOCATE
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_PRESET
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_RUNTIME
!> @see GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_TO_BE_ENCODED
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_READ_FROM_CFG'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_READ_FROM_CFG( THIS, CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_CONFIGURATIONS_T
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_GET_SUBCONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_GET_CONFIGURATIONS_SIZE
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_GET_CONFIGURATION_BY_ID
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_READ_LOGICAL
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_READ_INTEGER
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_READ_INTEGER_WITH_FILTER
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_DELETE_CONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD,            ONLY: FUN_C2I_IF
  USE :: ENUMERATORS_MOD,                ONLY: CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS
  USE :: ENUMERATORS_MOD,                ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_T), INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T),                 INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),                   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_TIMERANGES
  LOGICAL :: HAS_TYPE_OF_STATISTICAL_PROCESSING
  LOGICAL :: HAS_OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS
  LOGICAL :: HAS_OVERALL_LENGTH_OF_TIMERANGE
  LOGICAL :: HAS_ENCODE_STEP_ZERO
  INTEGER(KIND=JPIB_K) :: NUMBER_OF_TIMERANGES
  INTEGER(KIND=JPIB_K) :: I
  TYPE(YAML_CONFIGURATION_T)  :: TIME_RANGE_CONFIGURATION
  TYPE(YAML_CONFIGURATIONS_T) :: TIME_RANGES_CONFIGURATION
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  PROCEDURE(FUN_C2I_IF), POINTER :: P_CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS
  PROCEDURE(FUN_C2I_IF), POINTER :: P_CLENGTH_OF_TIMERANGE2ILENGTH_OF_TIMERANGE

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_NUMBER_OF_TIMERANGES=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UDEFINED_TYPE_OF_STATISTICAL_PROCESSING=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UDEFINED_LENGTH_OF_TIMERANGE=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OVERDEFINED_LENGTH_OF_TIMERANGE=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG_BY_ID=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIME_RANGE_DEALLOCATION_ERROR=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIME_RANGES_DEALLOCATION_ERROR=13_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%TYPE_OF_STATISTICAL_PROCESS_), ERRFLAG_CFG_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS_), ERRFLAG_CFG_NOT_ALLOCATED )


  !> Read the encoder configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'time-ranges', HAS_TIMERANGES, HOOKS )

  IF ( HAS_TIMERANGES  ) THEN

    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'type-of-statistical-processing', HAS_TYPE_OF_STATISTICAL_PROCESSING, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_TYPE_OF_STATISTICAL_PROCESSING, ERRFLAG_UDEFINED_TYPE_OF_STATISTICAL_PROCESSING )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'encode-step-zero', HAS_ENCODE_STEP_ZERO, HOOKS )

    !> Read the type of statistical processing
    P_CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS => CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER_WITH_FILTER( CFG, 'type-of-statistical-processing', THIS%OUTER_TYPE_OF_STATISTICAL_PROCESS_, P_CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS, HOOKS )
    IF ( HAS_ENCODE_STEP_ZERO ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'encode-step-zero', THIS%ENCODE_STEP_ZERO_, HOOKS )
    ELSE
      THIS%ENCODE_STEP_ZERO_ = .FALSE.
    ENDIF

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATIONS( CFG, 'time-ranges', TIME_RANGES_CONFIGURATION, HOOKS )

    !> Get the sections size
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE) YAML_GET_CONFIGURATIONS_SIZE( TIME_RANGES_CONFIGURATION, NUMBER_OF_TIMERANGES, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( NUMBER_OF_TIMERANGES .LE. 0, ERRFLAG_WRONG_NUMBER_OF_TIMERANGES )

   !> Number of time ranges needs to be grater then 1 (the outer one has no length)
    THIS%NUMBER_OF_TIME_RANGES_ = NUMBER_OF_TIMERANGES + 1_JPIB_K

    !> Read the type of statistical processing
    P_CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS => CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER_WITH_FILTER( CFG, 'type-of-statistical-processing', THIS%TYPE_OF_STATISTICAL_PROCESS_(I), P_CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS, HOOKS )


    !> Allocate statistics informations
    ALLOCATE( THIS%TYPE_OF_STATISTICAL_PROCESS_(THIS%NUMBER_OF_TIME_RANGES_), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )
    THIS%TYPE_OF_STATISTICAL_PROCESS_ = UNDEF_PARAM_E

    ALLOCATE( THIS%OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS_(THIS%NUMBER_OF_TIME_RANGES_), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )
    THIS%OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS_ = -1_JPIB_K

    !> Loop over sections
    DO I = 1 , THIS%NUMBER_OF_TIME_RANGES_

      !> Get section configuration by ID
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG_BY_ID) YAML_GET_CONFIGURATION_BY_ID( TIME_RANGES_CONFIGURATION, I, TIME_RANGE_CONFIGURATION, HOOKS )

      !> Check if configuration has the section keyword
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( TIME_RANGE_CONFIGURATION, 'type-of-statistical-processing', HAS_TYPE_OF_STATISTICAL_PROCESSING, HOOKS )
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( TIME_RANGE_CONFIGURATION, 'overall-length-of-timerange-in-seconds', HAS_OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS, HOOKS )
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( TIME_RANGE_CONFIGURATION, 'overall-length-of-timerange', HAS_OVERALL_LENGTH_OF_TIMERANGE, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_TYPE_OF_STATISTICAL_PROCESSING, ERRFLAG_UDEFINED_TYPE_OF_STATISTICAL_PROCESSING )
      PP_DEBUG_CRITICAL_COND_THROW( ALL( [.NOT.HAS_OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS, .NOT.HAS_OVERALL_LENGTH_OF_TIMERANGE]) , ERRFLAG_UDEFINED_LENGTH_OF_TIMERANGE )
      PP_DEBUG_CRITICAL_COND_THROW( ALL( [HAS_OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS, HAS_OVERALL_LENGTH_OF_TIMERANGE]) , ERRFLAG_OVERDEFINED_LENGTH_OF_TIMERANGE )

      !> Read the type of statistical processing
      P_CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS => CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER_WITH_FILTER( TIME_RANGE_CONFIGURATION, 'type-of-statistical-processing', THIS%TYPE_OF_STATISTICAL_PROCESS_(I), P_CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS, HOOKS )

      !> Read the overall length of timerange in seconds
      IF ( HAS_OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS ) THEN
        PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER( TIME_RANGE_CONFIGURATION, 'overall-length-of-timerange-in-seconds', THIS%OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS_(I), HOOKS )
      ELSEIF ( HAS_OVERALL_LENGTH_OF_TIMERANGE ) THEN
        P_CLENGTH_OF_TIMERANGE2ILENGTH_OF_TIMERANGE => CLENGTH_OF_TIMERANGE2ILENGTH_OF_TIMERANGE
        PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER_WITH_FILTER( TIME_RANGE_CONFIGURATION, 'overall-length-of-timerange', THIS%OVERALL_LENGTH_OF_TIMERANGE_IN_SECONDS_(I), P_CLENGTH_OF_TIMERANGE2ILENGTH_OF_TIMERANGE, HOOKS )
      END IF

      !> Deallocate section configuration
      PP_TRYCALL( ERRFLAG_TIME_RANGE_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( TIME_RANGE_CONFIGURATION, HOOKS )

    ENDDO

    !> Deallocate sections
    PP_TRYCALL( ERRFLAG_TIME_RANGES_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATIONS( TIME_RANGES_CONFIGURATION, HOOKS )
  ELSE

    !> Number of time ranges is assumed to be 1
    THIS%NUMBER_OF_TIME_RANGES_ = 1_JPIB_K

    !> REad configuration of r the outer loop
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'type-of-statistical-processing', HAS_TYPE_OF_STATISTICAL_PROCESSING, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_TYPE_OF_STATISTICAL_PROCESSING, ERRFLAG_UDEFINED_TYPE_OF_STATISTICAL_PROCESSING )
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'encode-step-zero', HAS_ENCODE_STEP_ZERO, HOOKS )

    !> Read the type of statistical processing
    P_CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS => CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_INTEGER_WITH_FILTER( CFG, 'type-of-statistical-processing', THIS%OUTER_TYPE_OF_STATISTICAL_PROCESS_, P_CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS, HOOKS )
    IF ( HAS_ENCODE_STEP_ZERO ) THEN
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'encode-step-zero', THIS%ENCODE_STEP_ZERO_, HOOKS )
    ELSE
      THIS%ENCODE_STEP_ZERO_ = .FALSE.
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
    CASE ( ERRFLAG_UNABLE_TO_ALLOCATE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating memory' )
    CASE ( ERRFLAG_CFG_NOT_ALLOCATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error internal state already allocated' )
    CASE ( ERRFLAG_UNABLE_TO_READ_CFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error reading configuration' )
    CASE ( ERRFLAG_UNABLE_TO_READ_SUBCFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error reading subconfigurations' )
    CASE ( ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error getting subconfigurations size' )
    CASE ( ERRFLAG_WRONG_NUMBER_OF_TIMERANGES )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error wrong number of timeranges' )
    CASE ( ERRFLAG_UDEFINED_TYPE_OF_STATISTICAL_PROCESSING )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error undefined type of statistical processing' )
    CASE ( ERRFLAG_UDEFINED_LENGTH_OF_TIMERANGE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error undefined length of timerange' )
    CASE ( ERRFLAG_OVERDEFINED_LENGTH_OF_TIMERANGE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error overdefined length of timerange' )
    CASE ( ERRFLAG_UNABLE_TO_READ_CFG_BY_ID )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error reading configuration by ID' )
    CASE ( ERRFLAG_TIME_RANGE_DEALLOCATION_ERROR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating time range type' )
    CASE ( ERRFLAG_TIME_RANGES_DEALLOCATION_ERROR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating time ranges type' )
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

END FUNCTION GRIB2_SECTION4_READ_FROM_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Converts a string representation of a time range into an integer representing length in seconds.
!>
!> This function takes a string input that specifies a length of time in a defined format (e.g., "5h" for 5 hours),
!> converts it into an integer representing the total length in seconds, and outputs this value through an output parameter.
!>
!> The function also processes input for a variety of time units:
!> - Seconds (s or S)
!> - Minutes (m or M)
!> - Hours (h or H)
!> - Days (d or D)
!>
!> @param [in] CLENGTH_OF_TIMERANGE The string representing the length of time in the specified format.
!> @param [out] ILENGTH_OF_TIMERANGE The integer output that will contain the length of time in seconds.
!> @param [inout] HOOKS A structure for hooks that may be used for debugging, logging, or other functionalities.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CLENGTH_OF_TIMERANGE2ILENGTH_OF_TIMERANGE'
PP_THREAD_SAFE FUNCTION CLENGTH_OF_TIMERANGE2ILENGTH_OF_TIMERANGE( CLENGTH_OF_TIMERANGE, ILENGTH_OF_TIMERANGE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: CLENGTH_OF_TIMERANGE
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: ILENGTH_OF_TIMERANGE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: SCALE_FACTOR
  INTEGER(KIND=JPIB_K) :: TMP
  INTEGER(KIND=JPIB_K) :: READ_STATUS
  CHARACTER(LEN=LEN(CLENGTH_OF_TIMERANGE)) :: LOC_CLENGTH_OF_TIMERANGE
  CHARACTER(LEN=LEN(CLENGTH_OF_TIMERANGE)) :: CLENGTH_OF_TIMERANGE_DIGITS
  CHARACTER(LEN=1) :: CLENGTH_OF_TIMERANGE_SCALE_FACTOR

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_LENGTH_OF_TIMERANGE=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_SCALE_FACTOR_FOR_TIMERANGE=2_JPIB_K

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

  !> Convert prefix to lowercase
  LOC_CLENGTH_OF_TIMERANGE = REPEAT(' ', LEN( CLENGTH_OF_TIMERANGE ) )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( TRIM(ADJUSTL(CLENGTH_OF_TIMERANGE)), LOC_CLENGTH_OF_TIMERANGE, HOOKS )
  N =  LEN_TRIM(LOC_CLENGTH_OF_TIMERANGE)

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( N.LT.2, ERRFLAG_INVALID_LENGTH_OF_TIMERANGE )

  !> Split the string into digits and scale factor
  CLENGTH_OF_TIMERANGE_DIGITS = REPEAT(' ', LEN( CLENGTH_OF_TIMERANGE_DIGITS ) )
  CLENGTH_OF_TIMERANGE_DIGITS = LOC_CLENGTH_OF_TIMERANGE(1:N-1)
  CLENGTH_OF_TIMERANGE_SCALE_FACTOR = LOC_CLENGTH_OF_TIMERANGE(N:N)

  !> Assign the scale factor
  SELECT CASE( CLENGTH_OF_TIMERANGE_SCALE_FACTOR )
  CASE ( 's')
    SCALE_FACTOR = 1_JPIB_K
  CASE ( 'm')
    SCALE_FACTOR = 60_JPIB_K
  CASE ( 'h')
    SCALE_FACTOR = 3600_JPIB_K
  CASE ( 'd')
    SCALE_FACTOR = 86400_JPIB_K
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_SCALE_FACTOR_FOR_TIMERANGE )
  END SELECT

  !> Convert the digits to integer
  READ( CLENGTH_OF_TIMERANGE_DIGITS, *, IOSTAT=READ_STATUS) TMP
  PP_DEBUG_CRITICAL_COND_THROW( READ_STATUS .NE. 0, ERRFLAG_INVALID_LENGTH_OF_TIMERANGE )

  !> Calculate the length of the timerange
  ILENGTH_OF_TIMERANGE = TMP * SCALE_FACTOR

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ILENGTH_OF_TIMERANGE .LT. 1, ERRFLAG_INVALID_LENGTH_OF_TIMERANGE )
  PP_DEBUG_CRITICAL_COND_THROW( MOD(ILENGTH_OF_TIMERANGE,3600_JPIB_K).NE.0, ERRFLAG_INVALID_LENGTH_OF_TIMERANGE )

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
    CASE ( ERRFLAG_INVALID_LENGTH_OF_TIMERANGE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid length of timerange' )
    CASE ( ERRFLAG_UNABLE_TO_CONVERT_LC )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
    CASE ( ERRFLAG_INVALID_SCALE_FACTOR_FOR_TIMERANGE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid scale factor for timerange' )
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

END FUNCTION CLENGTH_OF_TIMERANGE2ILENGTH_OF_TIMERANGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GRIB2_SECTION4_STATISTICS_SINCE_BEGINNING_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
