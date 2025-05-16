!>
!> @file grib2_section4_paramid_mod.F90
!>
!> @brief Module for managing GRIB2 Section 4 time configuration operations.
!>
!> The `GRIB2_SECTION4_PARAMID_MOD` module contains procedures to initialize, allocate,
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
!>   - @see GRIB2_SECTION4_PARAMID_INIT
!>   - @see GRIB2_SECTION4_PARAMID_ALLOCATE
!>   - @see GRIB2_SECTION4_PARAMID_PRESET
!>   - @see GRIB2_SECTION4_PARAMID_RUNTIME
!>   - @see GRIB2_SECTION4_PARAMID_TO_BE_ENCODED
!>   - @see GRIB2_SECTION4_PARAMID_FREE
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


#define PP_FILE_NAME 'grib2_section4_paramid_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SECTION4_PARAMID_MOD'
MODULE GRIB2_SECTION4_PARAMID_MOD

  !> Symbols imported from other modules within the project.
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!>
!> @brief Type definition for GRIB2 Section 4 time configuration handler.
!>
!> The `GRIB2_SECTION4_PARAMID_T` type extends the base class `GRIB_SECTION_BASE_A` and
!> provides concrete implementations of initialization, allocation, preset, runtime,
!> encoding checks, and cleanup operations for GRIB2 Section 4 time configuration objects.
!>
!> This type ensures that the required resources are properly managed through thread-safe,
!> non-overridable methods, providing robustness in both multi-threaded and single-threaded
!> environments.
!>
TYPE, EXTENDS(GRIB_SECTION_BASE_A) :: GRIB2_SECTION4_PARAMID_T

  !> Default symbols visibility
  PRIVATE

  CHARACTER(LEN=16) :: DATASET_FOR_LOCAL_ = REPEAT(' ',16)    !< The type of the section

CONTAINS

  !>
  !> @brief Initializes the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The procedure starts from a yaml configuration file to construct the
  !> GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT_CFG => GRIB2_SECTION4_PARAMID_INIT_CFG

  !>
  !> @brief Initializes the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The preocedure starts from a message and fro the parameters to construct
  !> the GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT_LAZY => GRIB2_SECTION4_PARAMID_INIT_LAZY

  !>
  !> @brief Allocates resources for the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure allocates memory and other necessary resources for
  !> the object based on provided parameters.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ALLOCATE => GRIB2_SECTION4_PARAMID_ALLOCATE

  !>
  !> @brief Presets the parameters of the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure configures the internal parameters of the object
  !> before runtime execution.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRESET => GRIB2_SECTION4_PARAMID_PRESET

  !>
  !> @brief Manages the runtime execution of GRIB2 Section 4 time configuration operations.
  !>
  !> This procedure handles operations and computations during runtime,
  !> making use of time and metadata information.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: RUNTIME => GRIB2_SECTION4_PARAMID_RUNTIME

  !>
  !> @brief Determines if the GRIB2 Section 4 time configuration object needs to be encoded.
  !>
  !> This procedure checks whether the object should be encoded based
  !> on the provided parameters and internal state.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: TO_BE_ENCODED => GRIB2_SECTION4_PARAMID_TO_BE_ENCODED

  !>
  !> @brief Frees resources allocated for the GRIB2 Section 4 time configuration object.
  !>
  !> This procedure deallocates resources and performs cleanup after
  !> the object has been used.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => GRIB2_SECTION4_PARAMID_FREE

END TYPE


!>
!> Public symbols (dataTypes)
PUBLIC :: GRIB2_SECTION4_PARAMID_T

CONTAINS

!>
!> @brief Initializes GRIB2 Section 4 time configuration for a given object using the provided parameters.
!>
!> This function initializes a GRIB2 Section 4 time configuration object (`THIS`) using the provided model parameters (`PARAMS`)
!> and configuration data (`CFG`). The process can be run in verbose mode if specified. The function
!> is thread-safe and returns an error code indicating the success or failure of the operation.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB2_SECTION4_PARAMID_T` representing the GRIB section being initialized.
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
!> @see GRIB2_SECTION4_PARAMID_INIT
!> @see GRIB2_SECTION4_PARAMID_ALLOCATE
!> @see GRIB2_SECTION4_PARAMID_PRESET
!> @see GRIB2_SECTION4_PARAMID_RUNTIME
!> @see GRIB2_SECTION4_PARAMID_TO_BE_ENCODED
!> @see GRIB2_SECTION4_PARAMID_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_PARAMID_INIT_CFG'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_PARAMID_INIT_CFG( THIS, &
&               CFG, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_READ_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_PARAMID_T),  INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_DATASET_FOR_LOCAL
  CHARACTER(LEN=:), ALLOCATABLE :: DATASET_FOR_LOCAL
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=2_JPIB_K

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
  THIS%SUBTYPE_ = 'PARAM'
  THIS%KIND_   = 'PARAMID'


  ! Read the optional configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'dataset-for-local', HAS_DATASET_FOR_LOCAL, HOOKS )

  ! Read the subCentre
  IF (HAS_DATASET_FOR_LOCAL) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_READ_STRING( CFG, 'dataset-for-local', DATASET_FOR_LOCAL, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(DATASET_FOR_LOCAL), ERRFLAG_UNABLE_TO_READ_CFG )
    PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(DATASET_FOR_LOCAL).LE.0, ERRFLAG_UNABLE_TO_READ_CFG )
    THIS%DATASET_FOR_LOCAL_ = TRIM(ADJUSTL(DATASET_FOR_LOCAL))
    DEALLOCATE(DATASET_FOR_LOCAL, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  ELSE
    THIS%DATASET_FOR_LOCAL_ = REPEAT(' ',16)
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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
    CASE ( ERRFLAG_UNABLE_TO_DEALLOCATE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate memory' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STAT)
      ENDIF
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

END FUNCTION GRIB2_SECTION4_PARAMID_INIT_CFG
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
!>   @param [inout] THIS  An object of type `GRIB2_SECTION4_PARAMID_T` representing the GRIB section being initialized.
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
!> @see GRIB2_SECTION4_PARAMID_INIT
!> @see GRIB2_SECTION4_PARAMID_ALLOCATE
!> @see GRIB2_SECTION4_PARAMID_PRESET
!> @see GRIB2_SECTION4_PARAMID_RUNTIME
!> @see GRIB2_SECTION4_PARAMID_TO_BE_ENCODED
!> @see GRIB2_SECTION4_PARAMID_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_PARAMID_INIT_LAZY'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_PARAMID_INIT_LAZY( THIS, &
&               MSG, PAR, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_PARAMID_T),  INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
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

  ! Initialise the section
  THIS%TYPE_ = 'CONFIGURATOR'
  THIS%SUBTYPE_ = 'PARAM'
  THIS%KIND_   = 'PARAMID'


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

END FUNCTION GRIB2_SECTION4_PARAMID_INIT_LAZY
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
!>   @param [in]    THIS     An object of type `GRIB2_SECTION4_PARAMID_T` representing the GRIB section to allocate resources for.
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
!> @see GRIB2_SECTION4_PARAMID_ALLOCATE
!> @see GRIB2_SECTION4_PARAMID_INIT
!> @see GRIB2_SECTION4_PARAMID_PRESET
!> @see GRIB2_SECTION4_PARAMID_RUNTIME
!> @see GRIB2_SECTION4_PARAMID_TO_BE_ENCODED
!> @see GRIB2_SECTION4_PARAMID_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_PARAMID_ALLOCATE'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_PARAMID_ALLOCATE( THIS, &
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
  CLASS(GRIB2_SECTION4_PARAMID_T),     INTENT(INOUT) :: THIS
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

END FUNCTION GRIB2_SECTION4_PARAMID_ALLOCATE
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
!>   @param [in]    THIS     An object of type `GRIB2_SECTION4_PARAMID_T` representing the GRIB section to be preset.
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
!> @see GRIB2_SECTION4_PARAMID_PRESET
!> @see GRIB2_SECTION4_PARAMID_ALLOCATE
!> @see GRIB2_SECTION4_PARAMID_INIT
!> @see GRIB2_SECTION4_PARAMID_RUNTIME
!> @see GRIB2_SECTION4_PARAMID_TO_BE_ENCODED
!> @see GRIB2_SECTION4_PARAMID_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_PARAMID_PRESET'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_PARAMID_PRESET( THIS, &
&  MSG, PAR, OPT,  METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIM_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_METADATA_MOD,        ONLY: GRIB_METADATA_T

  !> Symbols imported from other libraries
  USE :: GRIB_API, ONLY: GRIB_GET
  USE :: GRIB_API, ONLY: GRIB_SUCCESS
  USE :: GRIB_API, ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_PARAMID_T), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIM_K) :: HANDLE
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIB_K) :: TMP
  CHARACTER(LEN=:), ALLOCATABLE :: JSON
  CHARACTER(LEN=32) :: CPARAM

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DUMP=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_HANDLE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_GET_FAILED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERFLAG_METADATA_NOT_SUPPORTED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_TO_JSON = 6_JPIB_K

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
#if 0
  ! Sometimes before setting the paramId we need to set the datasetForLocal for unknown reasons
  !IF ( LEN_TRIM(THIS%DATASET_FOR_LOCAL_) .GT. 0 ) THEN
  !  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'datasetForLocal', TRIM(ADJUSTL(THIS%DATASET_FOR_LOCAL_)) )
  !END IF

  CPARAM = REPEAT(' ',32)
  WRITE(CPARAM,'(I32)') MSG%PARAM
  PP_TRYCALL(ERRFLAG_METADATA) METADATA%DUMP_SAMPLE( 'msg_'//TRIM(ADJUSTL(CPARAM))//'.grib', HOOKS )
#endif
  ! Set the paramId
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'paramId', MSG%PARAM )
#if 0
  ! Paranoid verification
  SELECT TYPE(A => METADATA)

  CLASS IS (GRIB_METADATA_T)

    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_HANDLE) A%GET_HANDLE( HANDLE, HOOKS )

    ! Set the values to the handle
    CALL GRIB_GET( HANDLE, 'paramId', TMP, STATUS=KRET )
    PP_DEBUG_DEVELOP_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_GET_FAILED )

    IF (TMP .NE. MSG%PARAM .OR. TMP .LE. 0 ) THEN
      PP_TRYCALL(ERRFLAG_MARS_TO_JSON) MSG%TO_JSON( JSON, HOOKS )
      IF ( ALLOCATED(JSON) ) THEN
        WRITE(*,*)     ' PARAM-PRE-SETTER :: ERROR PARAMAID DOES NOT MATCH :: ', TMP, MSG%PARAM
        WRITE(*,'(A)') ' PARAM-PRE-SETTER :: ERROR PARAMAID DOES NOT MATCH :: '//TRIM(ADJUSTL(JSON))
        DEALLOCATE(JSON, STAT=TMP)
      ENDIF
    END IF

  CLASS DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERFLAG_METADATA_NOT_SUPPORTED )
  END SELECT
#endif
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
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE ( ERRFLAG_METADATA )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error using metadata' )
    CASE ( ERRFLAG_DUMP )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error dumping metadata' )
    CASE ( ERRFLAG_UNABLE_TO_GET_HANDLE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get handle' )
    CASE ( ERRFLAG_GRIB_GET_FAILED )
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error getting paramId' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grib error: '//TRIM(ADJUSTL(GRIB_ERROR)) )
    CASE ( ERFLAG_METADATA_NOT_SUPPORTED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'metadata not supported' )
    CASE ( ERRFLAG_MARS_TO_JSON )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error converting mars to json' )
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

END FUNCTION GRIB2_SECTION4_PARAMID_PRESET
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
!>   @param [in]    THIS      An object of type `GRIB2_SECTION4_PARAMID_T` representing the GRIB section for runtime execution.
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
!> @see GRIB2_SECTION4_PARAMID_RUNTIME
!> @see GRIB2_SECTION4_PARAMID_ALLOCATE
!> @see GRIB2_SECTION4_PARAMID_INIT
!> @see GRIB2_SECTION4_PARAMID_PRESET
!> @see GRIB2_SECTION4_PARAMID_TO_BE_ENCODED
!> @see GRIB2_SECTION4_PARAMID_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_PARAMID_RUNTIME'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_PARAMID_RUNTIME( THIS, &
&  MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIM_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: TIME_UTILS_MOD,           ONLY: TIME_HISTORY_T
  USE :: TIME_UTILS_MOD,           ONLY: CURR_TIME_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_METADATA_MOD,        ONLY: GRIB_METADATA_T

  !> Symbols imported from other libraries
  USE :: GRIB_API, ONLY: GRIB_GET
  USE :: GRIB_API, ONLY: GRIB_SUCCESS
  USE :: GRIB_API, ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION4_PARAMID_T),     INTENT(INOUT) :: THIS
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
  INTEGER(KIND=JPIM_K) :: HANDLE
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIB_K) :: TMP
  CHARACTER(LEN=:), ALLOCATABLE :: JSON

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DUMP=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_HANDLE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_GET_FAILED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERFLAG_METADATA_NOT_SUPPORTED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_TO_JSON = 6_JPIB_K

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

  IF ( LEN_TRIM(THIS%DATASET_FOR_LOCAL_) .GT. 0 ) THEN
    PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'datasetForLocal', TRIM(ADJUSTL(THIS%DATASET_FOR_LOCAL_)) )
  END IF

#if 0
  PP_TRYCALL(ERRFLAG_MARS_TO_JSON) METADATA%DUMP_SAMPLE( 'before_paramID.grib', HOOKS )
#endif

  ! Set the paramId
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'paramId', MSG%PARAM )

#if 0
  ! Paranoid verification
  SELECT TYPE(A => METADATA)

  CLASS IS (GRIB_METADATA_T)

    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_HANDLE) A%GET_HANDLE( HANDLE, HOOKS )

    ! Set the values to the handle
    CALL GRIB_GET( HANDLE, 'paramId', TMP, STATUS=KRET )
    PP_DEBUG_DEVELOP_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_GET_FAILED )

    IF (TMP .NE. MSG%PARAM .OR. TMP .LE. 0 ) THEN
      PP_TRYCALL(ERRFLAG_MARS_TO_JSON) MSG%TO_JSON( JSON, HOOKS )
      IF ( ALLOCATED(JSON) ) THEN
        WRITE(*,*)     ' PARAM-RT-SETTER :: ERROR PARAMAID DOES NOT MATCH :: ', TMP, MSG%PARAM
        WRITE(*,'(A)') ' PARAM-RT-SETTER :: ERROR PARAMAID DOES NOT MATCH :: '//TRIM(ADJUSTL(JSON))
        DEALLOCATE(JSON, STAT=TMP)
      ENDIF
    END IF

  CLASS DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERFLAG_METADATA_NOT_SUPPORTED )
  END SELECT

  PP_TRYCALL(ERRFLAG_MARS_TO_JSON) METADATA%DUMP_SAMPLE( 'after_paramID.grib', HOOKS )
#endif

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
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE ( ERRFLAG_METADATA )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error using metadata' )
    CASE ( ERRFLAG_UNABLE_TO_GET_HANDLE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get handle' )
    CASE ( ERRFLAG_GRIB_GET_FAILED )
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error getting paramId' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grib error: '//TRIM(ADJUSTL(GRIB_ERROR)) )
    CASE ( ERFLAG_METADATA_NOT_SUPPORTED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'metadata not supported' )
    CASE ( ERRFLAG_MARS_TO_JSON )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error converting mars to json' )
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

END FUNCTION GRIB2_SECTION4_PARAMID_RUNTIME
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
!>   @param [inout] THIS          An object of type `GRIB2_SECTION4_PARAMID_T` representing the GRIB section being checked.
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
!> @see GRIB2_SECTION4_PARAMID_TO_BE_ENCODED
!> @see GRIB2_SECTION4_PARAMID_INIT
!> @see GRIB2_SECTION4_PARAMID_ALLOCATE
!> @see GRIB2_SECTION4_PARAMID_PRESET
!> @see GRIB2_SECTION4_PARAMID_RUNTIME
!> @see GRIB2_SECTION4_PARAMID_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_PARAMID_TO_BE_ENCODED'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_PARAMID_TO_BE_ENCODED( THIS, &
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
  CLASS(GRIB2_SECTION4_PARAMID_T),  INTENT(INOUT) :: THIS
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

END FUNCTION GRIB2_SECTION4_PARAMID_TO_BE_ENCODED
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
!>   @param [inout] THIS  An object of type `GRIB2_SECTION4_PARAMID_T` representing the GRIB section to be freed.
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
!> @see GRIB2_SECTION4_PARAMID_INIT
!> @see GRIB2_SECTION4_PARAMID_ALLOCATE
!> @see GRIB2_SECTION4_PARAMID_PRESET
!> @see GRIB2_SECTION4_PARAMID_RUNTIME
!> @see GRIB2_SECTION4_PARAMID_TO_BE_ENCODED
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_PARAMID_FREE'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_PARAMID_FREE( THIS, OPT, DESTRUCTORS, HOOKS ) RESULT(RET)

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
  CLASS(GRIB2_SECTION4_PARAMID_T), INTENT(INOUT) :: THIS
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

END FUNCTION GRIB2_SECTION4_PARAMID_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB2_SECTION4_PARAMID_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
