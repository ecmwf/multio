!>
!> @file grib2_section1_000_mod.F90
!>
!> @brief Module for managing GRIB2 SECTION 1 operations.
!>
!> The `GRIB2_SECTION1_000_MOD` module contains procedures to initialize, allocate,
!> preset, run, and clean up the resources associated with GRIB2 SECTION 1 objects.
!> This module provides thread-safe operations and includes extensive use of debugging,
!> logging, and tracing capabilities, making it robust for production and testing.
!>
!> The key operations covered by this module include:
!>   - Initialization of GRIB2 SECTION 1 objects.
!>   - Allocation of resources.
!>   - Presetting internal parameters.
!>   - Managing runtime operations based on input parameters.
!>   - Cleaning up and deallocating resources after use.
!>
!> @section interface
!>
!> The module exports the following procedures:
!>   - @see GRIB2_SECTION1_000_INIT
!>   - @see GRIB2_SECTION1_000_ALLOCATE
!>   - @see GRIB2_SECTION1_000_PRESET
!>   - @see GRIB2_SECTION1_000_RUNTIME
!>   - @see GRIB2_SECTION1_000_TO_BE_ENCODED
!>   - @see GRIB2_SECTION1_000_FREE
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


#define PP_FILE_NAME 'grib2_section1_000_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SECTION1_000_MOD'
MODULE GRIB2_SECTION1_000_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!>
!> @brief Type definition for GRIB2 SECTION 1 handler.
!>
!> The `GRIB2_SECTION1_000_T` type extends the base class `GRIB_SECTION_BASE_A` and
!> provides concrete implementations of initialization, allocation, preset, runtime,
!> encoding checks, and cleanup operations for GRIB2 SECTION 1 objects.
!>
!> This type ensures that the required resources are properly managed through thread-safe,
!> non-overridable methods, providing robustness in both multi-threaded and single-threaded
!> environments.
!>
TYPE, EXTENDS(GRIB_SECTION_BASE_A) :: GRIB2_SECTION1_000_T

  !> Default symbols visibility
  PRIVATE

  !> Template number
  INTEGER(KIND=JPIB_K) :: TEMPLATE_NUMBER_ = 0_JPIB_K

  !> Configurations to be loaded from the YAML file
  CLASS(GRIB_SECTION_BASE_A), POINTER :: ORIGIN_ => NULL()
  CLASS(GRIB_SECTION_BASE_A), POINTER :: TABLES_ => NULL()
  CLASS(GRIB_SECTION_BASE_A), POINTER :: REFERENCE_TIME_ => NULL()
  CLASS(GRIB_SECTION_BASE_A), POINTER :: DATA_TYPE_ => NULL()

CONTAINS

  !>
  !> @brief Initializes the GRIB2 SECTION 1 object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The procedure starts from a yaml configuration file to construct the
  !> GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT_CFG => GRIB2_SECTION1_000_INIT_CFG

  !>
  !> @brief Initializes the GRIB2 SECTION 1 object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The preocedure starts from a message and from the parameters to construct
  !> the GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT_LAZY => GRIB2_SECTION1_000_INIT_LAZY

  !>
  !> @brief Allocates resources for the GRIB2 SECTION 1 object.
  !>
  !> This procedure allocates memory and other necessary resources for
  !> the object based on provided parameters.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ALLOCATE => GRIB2_SECTION1_000_ALLOCATE

  !>
  !> @brief Presets the parameters of the GRIB2 SECTION 1 object.
  !>
  !> This procedure configures the internal parameters of the object
  !> before runtime execution.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRESET => GRIB2_SECTION1_000_PRESET

  !>
  !> @brief Manages the runtime execution of GRIB2 SECTION 1 operations.
  !>
  !> This procedure handles operations and computations during runtime,
  !> making use of time and metadata information.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: RUNTIME => GRIB2_SECTION1_000_RUNTIME

  !>
  !> @brief Determines if the GRIB2 SECTION 1 object needs to be encoded.
  !>
  !> This procedure checks whether the object should be encoded based
  !> on the provided parameters and internal state.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: TO_BE_ENCODED => GRIB2_SECTION1_000_TO_BE_ENCODED

  !>
  !> @brief Frees resources allocated for the GRIB2 SECTION 1 object.
  !>
  !> This procedure deallocates resources and performs cleanup after
  !> the object has been used.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => GRIB2_SECTION1_000_FREE

  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRINT => GRIB2_SECTION1_000_PRINT


  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: BUILD_ORIGIN_FROM_CFG         => GRIB2_SECTION1_000_BUILD_ORIGIN_FROM_CFG
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: BUILD_TABLES_FROM_CFG         => GRIB2_SECTION1_000_BUILD_TABLES_FROM_CFG
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: BUILD_REFERENCE_TIME_FROM_CFG => GRIB2_SECTION1_000_BUILD_REFERENCE_TIME_FROM_CFG
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: BUILD_DATA_TYPE_FROM_CFG      => GRIB2_SECTION1_000_BUILD_DATA_TYPE_FROM_CFG

END TYPE


!>
!> Public symbols (dataTypes)
PUBLIC :: GRIB2_SECTION1_000_T

CONTAINS

!>
!> @brief Initializes GRIB2 SECTION 1 for a given object using the provided parameters.
!>
!> This function initializes a GRIB2 SECTION 1 object (`THIS`) using YAML configuration data (`CFG`).
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB2_SECTION1_@XXX@_T` representing the GRIB section being initialized.
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
!> @see GRIB2_SECTION1_000_INIT
!> @see GRIB2_SECTION1_000_ALLOCATE
!> @see GRIB2_SECTION1_000_PRESET
!> @see GRIB2_SECTION1_000_RUNTIME
!> @see GRIB2_SECTION1_000_TO_BE_ENCODED
!> @see GRIB2_SECTION1_000_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_000_INIT_CFG'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_000_INIT_CFG( THIS, &
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
  CLASS(GRIB2_SECTION1_000_T),  INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_ORIGIN_HANDLER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_TABLES_HANDLER=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_REFERENCE_TIME_HANDLER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_DATA_TYPE_HANDLER=4_JPIB_K

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
  THIS%TYPE_ = 'SECTION'
  THIS%SUBTYPE_ = 'IDENTIFICATION_SECTION'
  THIS%KIND_ = '1.0'

  ! Logging informations
  PP_LOG_TODO( 'Lazy initialization of GRIB2 Section: '//TRIM(ADJUSTL(THIS%TYPE_))//'::'//TRIM(ADJUSTL(THIS%SUBTYPE_))//'('//TRIM(ADJUSTL(THIS%KIND_))//')' )


  ! Initialize sub sections
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_ORIGIN_HANDLER)         THIS%BUILD_ORIGIN_FROM_CFG( CFG, OPT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_TABLES_HANDLER)         THIS%BUILD_TABLES_FROM_CFG( CFG, OPT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_REFERENCE_TIME_HANDLER) THIS%BUILD_REFERENCE_TIME_FROM_CFG( CFG, OPT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_DATA_TYPE_HANDLER)      THIS%BUILD_DATA_TYPE_FROM_CFG( CFG, OPT, HOOKS )

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
    CASE ( ERRFLAG_UNABLE_TO_INIT_ORIGIN_HANDLER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error initializing origin handler' )
    CASE ( ERRFLAG_UNABLE_TO_INIT_TABLES_HANDLER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error initializing tables handler' )
    CASE ( ERRFLAG_UNABLE_TO_INIT_REFERENCE_TIME_HANDLER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error initializing reference time handler' )
    CASE ( ERRFLAG_UNABLE_TO_INIT_DATA_TYPE_HANDLER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error initializing data type handler' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION1_000_INIT_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Initializes GRIB2 SECTION 1 for a given object using the provided parameters.
!>
!> This function initializes a GRIB2 SECTION 1 object (`THIS`) using the provided model message (`MSG`)
!> and the provided model parameters (`PARAMS`) and the options object (`OPT`).
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB_SECTION_BASE_A` representing the GRIB section being initialized.
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
!> @see GRIB2_SECTION1_000_INIT
!> @see GRIB2_SECTION1_000_ALLOCATE
!> @see GRIB2_SECTION1_000_PRESET
!> @see GRIB2_SECTION1_000_RUNTIME
!> @see GRIB2_SECTION1_000_TO_BE_ENCODED
!> @see GRIB2_SECTION1_000_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_000_INIT_LAZY'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_000_INIT_LAZY( THIS, &
&               MSG, PAR, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB2_SECTION1_ORIGIN_FACTORY_MOD,         ONLY: MAKE_GRIB2_ORIGIN_CONFIGURATOR
  USE :: GRIB2_SECTION1_TABLES_FACTORY_MOD,         ONLY: MAKE_GRIB2_TABLES_CONFIGURATOR
  USE :: GRIB2_SECTION1_REFERENCE_TIME_FACTORY_MOD, ONLY: MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR
  USE :: GRIB2_SECTION1_DATA_TYPE_FACTORY_MOD,      ONLY: MAKE_GRIB2_DATA_TYPE_CONFIGURATOR

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION1_000_T),  INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_ORIGIN_HANDLER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_TABLES_HANDLER=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_REFERENCE_TIME_HANDLER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_DATA_TYPE_HANDLER=4_JPIB_K

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
  THIS%TYPE_ = 'SECTION'
  THIS%SUBTYPE_ = 'IDENTIFICATION_SECTION'
  THIS%KIND_ = '1.0'


  ! Logging informations
  PP_LOG_TODO( 'Lazy initialization of GRIB2 Section: '//TRIM(ADJUSTL(THIS%TYPE_))//'::'//TRIM(ADJUSTL(THIS%SUBTYPE_))//'('//TRIM(ADJUSTL(THIS%KIND_))//')' )

  ! Build the subsections handlers
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_ORIGIN_HANDLER) MAKE_GRIB2_ORIGIN_CONFIGURATOR( THIS%ORIGIN_, THIS%TEMPLATE_NUMBER_, MSG, PAR, OPT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_TABLES_HANDLER) MAKE_GRIB2_TABLES_CONFIGURATOR( THIS%TABLES_, THIS%TEMPLATE_NUMBER_, MSG, PAR, OPT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_REFERENCE_TIME_HANDLER) MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR( THIS%REFERENCE_TIME_, THIS%TEMPLATE_NUMBER_, MSG, PAR, OPT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_DATA_TYPE_HANDLER) MAKE_GRIB2_DATA_TYPE_CONFIGURATOR( THIS%DATA_TYPE_, THIS%TEMPLATE_NUMBER_, MSG, PAR, OPT, HOOKS )

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
    CASE ( ERRFLAG_UNABLE_TO_BUILD_ORIGIN_HANDLER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error building origin handler' )
    CASE ( ERRFLAG_UNABLE_TO_BUILD_TABLES_HANDLER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error building tables handler' )
    CASE ( ERRFLAG_UNABLE_TO_BUILD_REFERENCE_TIME_HANDLER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error building reference time handler' )
    CASE ( ERRFLAG_UNABLE_TO_BUILD_DATA_TYPE_HANDLER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error building data type handler' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION1_000_INIT_LAZY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Allocates resources for GRIB2 SECTION 1 using the provided parameters.
!>
!> This function allocates resources for a GRIB2 SECTION 1 object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the allocation process.
!>
!> @section interface
!>   @param [in]    THIS     An object of type `GRIB2_SECTION1_@XXX@_T` representing the GRIB section to allocate resources for.
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
!> @see GRIB2_SECTION1_000_ALLOCATE
!> @see GRIB2_SECTION1_000_INIT
!> @see GRIB2_SECTION1_000_PRESET
!> @see GRIB2_SECTION1_000_RUNTIME
!> @see GRIB2_SECTION1_000_TO_BE_ENCODED
!> @see GRIB2_SECTION1_000_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_000_ALLOCATE'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_000_ALLOCATE( THIS, &
&  MSG, PAR, OPT, METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
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
  CLASS(GRIB2_SECTION1_000_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_ORIGIN=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_TABLES=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_REFERENCE_TIME=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_DATA_TYPE=9_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%ORIGIN_), ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%TABLES_), ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%REFERENCE_TIME_), ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%DATA_TYPE_), ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED )

  ! Logging informations
  PP_LOG_INFO( 'Allocating GRIB2 Section: '//TRIM(ADJUSTL(THIS%TYPE_))//'.'//TRIM(ADJUSTL(THIS%SUBTYPE_)) )

  ! Allocate the subsections
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE_ORIGIN)         THIS%ORIGIN_%ALLOCATE( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE_TABLES)         THIS%TABLES_%ALLOCATE( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE_REFERENCE_TIME) THIS%REFERENCE_TIME_%ALLOCATE( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE_DATA_TYPE)      THIS%DATA_TYPE_%ALLOCATE( MSG, PAR, OPT, METADATA, HOOKS )

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
    CASE ( ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'origin handler not associated' )
    CASE ( ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'tables handler not associated' )
    CASE ( ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'reference time handler not associated' )
    CASE ( ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data type handler not associated' )
    CASE ( ERRFLAG_UNABLE_TO_ALLOCATE_ORIGIN )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating origin handler' )
    CASE ( ERRFLAG_UNABLE_TO_ALLOCATE_TABLES )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating tables handler' )
    CASE ( ERRFLAG_UNABLE_TO_ALLOCATE_REFERENCE_TIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating reference time handler' )
    CASE ( ERRFLAG_UNABLE_TO_ALLOCATE_DATA_TYPE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating data type handler' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION1_000_ALLOCATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Presets GRIB2 SECTION 1 using the provided parameters and message data.
!>
!> This function presets a GRIB2 SECTION 1 object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the preset operation.
!>
!> @section interface
!>   @param [in]    THIS     An object of type `GRIB2_SECTION1_000_T` representing the GRIB section to be preset.
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
!> @see GRIB2_SECTION1_000_PRESET
!> @see GRIB2_SECTION1_000_ALLOCATE
!> @see GRIB2_SECTION1_000_INIT
!> @see GRIB2_SECTION1_000_RUNTIME
!> @see GRIB2_SECTION1_000_TO_BE_ENCODED
!> @see GRIB2_SECTION1_000_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_000_PRESET'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_000_PRESET( THIS, &
&  MSG, PAR, OPT, METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION1_000_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRESET_ORIGIN=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRESET_TABLES=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRESET_REFERENCE_TIME=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRESET_DATA_TYPE=9_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%ORIGIN_), ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%TABLES_), ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%REFERENCE_TIME_), ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%DATA_TYPE_), ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED )

  ! Logging informations
  PP_LOG_INFO( 'Runtime GRIB2 Section: '//TRIM(ADJUSTL(THIS%TYPE_))//'::'//TRIM(ADJUSTL(THIS%SUBTYPE_)) )

  ! Preset the subsections
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRESET_ORIGIN)         THIS%ORIGIN_%PRESET( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRESET_TABLES)         THIS%TABLES_%PRESET( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRESET_REFERENCE_TIME) THIS%REFERENCE_TIME_%PRESET( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRESET_DATA_TYPE)      THIS%DATA_TYPE_%PRESET( MSG, PAR, OPT, METADATA, HOOKS )

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
    CASE ( ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'origin handler not associated' )
    CASE ( ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'tables handler not associated' )
    CASE ( ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'reference time handler not associated' )
    CASE ( ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data type handler not associated' )
    CASE ( ERRFLAG_UNABLE_TO_PRESET_ORIGIN )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error preset origin handler' )
    CASE ( ERRFLAG_UNABLE_TO_PRESET_TABLES )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error preset tables handler' )
    CASE ( ERRFLAG_UNABLE_TO_PRESET_REFERENCE_TIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error preset reference time handler' )
    CASE ( ERRFLAG_UNABLE_TO_PRESET_DATA_TYPE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error preset data type handler' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION1_000_PRESET
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Executes runtime processing for GRIB2 SECTION 1 using provided parameters, message data, and time history.
!>
!> This function performs runtime operations for a GRIB2 SECTION 1 object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), current time (`CURR_TIME`), time history (`TIME_HISTORY`), and metadata (`METADATA`).
!> The process can be run in verbose mode if specified. The function is thread-safe and returns an error code indicating
!> the success or failure of the runtime operation.
!>
!> @section interface
!>
!> @param [in]    THIS          An object of type `GRIB2_SECTION1_000_T` representing the GRIB section to be allocated.
!> @param [in]    PARAMS        Model parameters used during the runtime process.
!> @param [in]    MSG           Message structure providing necessary information.
!> @param [in]    CURR_TIME     Current time used in the runtime process.
!> @param [in]    TIME_HISTORY  Time history information for the runtime process.
!> @param [inout] METADATA      Pointer to metadata involved in the runtime process.
!> @param [in]    VERBOSE       Logical flag for verbose output during the runtime operation.
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
!> @see GRIB2_SECTION1_000_RUNTIME
!> @see GRIB2_SECTION1_000_ALLOCATE
!> @see GRIB2_SECTION1_000_INIT
!> @see GRIB2_SECTION1_000_PRESET
!> @see GRIB2_SECTION1_000_TO_BE_ENCODED
!> @see GRIB2_SECTION1_000_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_000_RUNTIME'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_000_RUNTIME( THIS, &
&  MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
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
  CLASS(GRIB2_SECTION1_000_T),     INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_RUNTIME_ORIGIN=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_RUNTIME_TABLES=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_RUNTIME_REFERENCE_TIME=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_RUNTIME_DATA_TYPE=9_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%ORIGIN_), ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%TABLES_), ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%REFERENCE_TIME_), ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%DATA_TYPE_), ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED )

  ! Logging informations
  PP_LOG_INFO( 'Runtime GRIB2 Section: '//TRIM(ADJUSTL(THIS%TYPE_))//'::'//TRIM(ADJUSTL(THIS%SUBTYPE_)) )

  ! Runtime the subsections
  PP_TRYCALL(ERRFLAG_UNABLE_TO_RUNTIME_ORIGIN)         THIS%ORIGIN_%RUNTIME(  MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_RUNTIME_TABLES)         THIS%TABLES_%RUNTIME(  MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_RUNTIME_REFERENCE_TIME) THIS%REFERENCE_TIME_%RUNTIME(  MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_RUNTIME_DATA_TYPE)      THIS%DATA_TYPE_%RUNTIME(  MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )

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
    CASE ( ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'origin handler not associated' )
    CASE ( ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'tables handler not associated' )
    CASE ( ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'reference time handler not associated' )
    CASE ( ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data type handler not associated' )
    CASE ( ERRFLAG_UNABLE_TO_RUNTIME_ORIGIN )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error runtime origin handler' )
    CASE ( ERRFLAG_UNABLE_TO_RUNTIME_TABLES )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error runtime tables handler' )
    CASE ( ERRFLAG_UNABLE_TO_RUNTIME_REFERENCE_TIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error runtime reference time handler' )
    CASE ( ERRFLAG_UNABLE_TO_RUNTIME_DATA_TYPE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error runtime data type handler' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION1_000_RUNTIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Prepares GRIB2 SECTION 1 for encoding based on provided parameters, message data, and time history.
!>
!> This function determines whether GRIB2 SECTION 1 (`THIS`) is ready to be encoded. It processes the provided model parameters
!> (`PARAMS`), message structure (`MSG`), current time (`CURR_TIME`), time history (`TIME_HISTORY`), and updates the
!> `TO_BE_ENCODED` flag accordingly. The function is thread-safe and returns an error code indicating the success or failure
!> of the operation. The process can also be run in verbose mode if specified.
!>
!> @section interface
!>   @param [inout] THIS          An object of type `GRIB2_SECTION1_000_T` representing the GRIB section being checked.
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
!> @see GRIB2_SECTION1_000_TO_BE_ENCODED
!> @see GRIB2_SECTION1_000_INIT
!> @see GRIB2_SECTION1_000_ALLOCATE
!> @see GRIB2_SECTION1_000_PRESET
!> @see GRIB2_SECTION1_000_RUNTIME
!> @see GRIB2_SECTION1_000_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_000_TO_BE_ENCODED'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_000_TO_BE_ENCODED( THIS, &
&  MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: TIME_UTILS_MOD,      ONLY: TIME_HISTORY_T
  USE :: TIME_UTILS_MOD,      ONLY: CURR_TIME_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION1_000_T),  INTENT(INOUT) :: THIS
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
  LOGICAL, DIMENSION(4) :: TO_BE_ENCODED_SUBS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BE_ENCODED_ORIGIN=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BE_ENCODED_TABLES=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BE_ENCODED_REFERENCE_TIME=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BE_ENCODED_DATA_TYPE=9_JPIB_K

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

  ! Logging informations
  PP_LOG_TODO( 'ToBeEncoded GRIB2 Section: '//TRIM(ADJUSTL(THIS%TYPE_))//'::'//TRIM(ADJUSTL(THIS%SUBTYPE_)) )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%ORIGIN_), ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%TABLES_), ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%REFERENCE_TIME_), ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%DATA_TYPE_), ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED )

  ! To be encoded the subsections
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BE_ENCODED_ORIGIN)         THIS%ORIGIN_%TO_BE_ENCODED(  MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED_SUBS(1), HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BE_ENCODED_TABLES)         THIS%TABLES_%TO_BE_ENCODED(  MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED_SUBS(2), HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BE_ENCODED_REFERENCE_TIME) THIS%REFERENCE_TIME_%TO_BE_ENCODED(  MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED_SUBS(3), HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BE_ENCODED_DATA_TYPE)      THIS%DATA_TYPE_%TO_BE_ENCODED(  MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED_SUBS(4), HOOKS )

  ! Generate output flag
  TO_BE_ENCODED = ALL( TO_BE_ENCODED_SUBS )

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
    CASE ( ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'origin handler not associated' )
    CASE ( ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'tables handler not associated' )
    CASE ( ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'reference time handler not associated' )
    CASE ( ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data type handler not associated' )
    CASE ( ERRFLAG_UNABLE_TO_BE_ENCODED_ORIGIN )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error to be encoded origin handler' )
    CASE ( ERRFLAG_UNABLE_TO_BE_ENCODED_TABLES )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error to be encoded tables handler' )
    CASE ( ERRFLAG_UNABLE_TO_BE_ENCODED_REFERENCE_TIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error to be encoded reference time handler' )
    CASE ( ERRFLAG_UNABLE_TO_BE_ENCODED_DATA_TYPE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error to be encoded data type handler' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION1_000_TO_BE_ENCODED
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees resources associated with GRIB2 SECTION 1 object.
!>
!> This function deallocates and cleans up resources associated with the GRIB2 SECTION 1 object (`THIS`).
!> The process can be run in verbose mode for additional output. The function is thread-safe and returns an
!> error code indicating the success or failure of the operation.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB2_SECTION1_@XXX@_T` representing the GRIB section to be freed.
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
!> @see GRIB2_SECTION1_000_INIT
!> @see GRIB2_SECTION1_000_ALLOCATE
!> @see GRIB2_SECTION1_000_PRESET
!> @see GRIB2_SECTION1_000_RUNTIME
!> @see GRIB2_SECTION1_000_TO_BE_ENCODED
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_000_FREE'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_000_FREE(  THIS, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: GRIB2_SECTION1_ORIGIN_FACTORY_MOD, ONLY: DESTROY_GRIB2_ORIGIN_CONFIGURATOR
  USE :: GRIB2_SECTION1_TABLES_FACTORY_MOD, ONLY: DESTROY_GRIB2_TABLES_CONFIGURATOR
  USE :: GRIB2_SECTION1_REFERENCE_TIME_FACTORY_MOD, ONLY: DESTROY_GRIB2_REFERENCE_TIME_CONFIGURATOR
  USE :: GRIB2_SECTION1_DATA_TYPE_FACTORY_MOD, ONLY: DESTROY_GRIB2_DATA_TYPE_CONFIGURATOR

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION1_000_T), INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_ORIGIN=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_TABLES=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_REFERENCE_TIME=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_DATA_TYPE=9_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%ORIGIN_), ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%TABLES_), ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%REFERENCE_TIME_), ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%DATA_TYPE_), ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED )

  ! Logging informations
  PP_LOG_TODO( 'Free GRIB2 Section: '//TRIM(ADJUSTL(THIS%TYPE_))//'::'//TRIM(ADJUSTL(THIS%SUBTYPE_)) )

  !> Destroy subsections
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DESTROY_ORIGIN)         DESTROY_GRIB2_ORIGIN_CONFIGURATOR( THIS%ORIGIN_, OPT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DESTROY_TABLES)         DESTROY_GRIB2_TABLES_CONFIGURATOR( THIS%TABLES_, OPT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DESTROY_REFERENCE_TIME) DESTROY_GRIB2_REFERENCE_TIME_CONFIGURATOR( THIS%REFERENCE_TIME_, OPT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DESTROY_DATA_TYPE)      DESTROY_GRIB2_DATA_TYPE_CONFIGURATOR( THIS%DATA_TYPE_, OPT, HOOKS )

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
    CASE ( ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'origin handler not associated' )
    CASE ( ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'tables handler not associated' )
    CASE ( ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'reference time handler not associated' )
    CASE ( ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data type handler not associated' )
    CASE ( ERRFLAG_UNABLE_TO_DESTROY_ORIGIN )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error destroy origin handler' )
    CASE ( ERRFLAG_UNABLE_TO_DESTROY_TABLES )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error destroy tables handler' )
    CASE ( ERRFLAG_UNABLE_TO_DESTROY_REFERENCE_TIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error destroy reference time handler' )
    CASE ( ERRFLAG_UNABLE_TO_DESTROY_DATA_TYPE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error destroy data type handler' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION1_000_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_000_BUILD_ORIGIN_FROM_CFG'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_000_BUILD_ORIGIN_FROM_CFG( THIS, &
&               CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,        ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_DELETE_CONFIGURATION
  USE :: GRIB2_SECTION1_ORIGIN_FACTORY_MOD, ONLY: MAKE_GRIB2_ORIGIN_CONFIGURATOR

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION1_000_T),  INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_ORIGIN_CONFIGURATOR
  TYPE(YAML_CONFIGURATION_T) :: ORIGIN_CONFIGURATOR_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: ORIGIN_CONFIGURATOR_NAME='origin-configurator'

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ORIGIN_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ORIGIN_CONFIGURATOR_CFG_NOT_PRESENT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_ORIGIN_CONFIGURATOR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%ORIGIN_), ERRFLAG_ORIGIN_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Check if a configuration for the indicator-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, ORIGIN_CONFIGURATOR_NAME, HAS_ORIGIN_CONFIGURATOR, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_ORIGIN_CONFIGURATOR, ERRFLAG_ORIGIN_CONFIGURATOR_CFG_NOT_PRESENT )

  !> Read the subconfiguration for the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, ORIGIN_CONFIGURATOR_NAME, ORIGIN_CONFIGURATOR_CFG, HOOKS )

  !> Make the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_ORIGIN_CONFIGURATOR) MAKE_GRIB2_ORIGIN_CONFIGURATOR( THIS%ORIGIN_, THIS%TEMPLATE_NUMBER_, ORIGIN_CONFIGURATOR_CFG, OPT, HOOKS )

  !> Deallocate the indicator-section configuration
  PP_TRYCALL( ERRFLAG_UNABLE_TO_DEALLOCATE ) YAML_DELETE_CONFIGURATION( ORIGIN_CONFIGURATOR_CFG, HOOKS )

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
    CASE ( ERRFLAG_ORIGIN_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'origin configurator already associated' )
    CASE ( ERRFLAG_UNABLE_TO_READ_CFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
    CASE ( ERRFLAG_ORIGIN_CONFIGURATOR_CFG_NOT_PRESENT )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'origin configurator configuration not present' )
    CASE ( ERRFLAG_UNABLE_TO_READ_SUBCFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfiguration' )
    CASE ( ERRFLAG_UNABLE_TO_MAKE_ORIGIN_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make origin configurator' )
    CASE ( ERRFLAG_UNABLE_TO_DEALLOCATE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate origin configurator object' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION1_000_BUILD_ORIGIN_FROM_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_000_BUILD_TABLES_FROM_CFG'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_000_BUILD_TABLES_FROM_CFG( THIS, &
&               CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,        ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_DELETE_CONFIGURATION
  USE :: GRIB2_SECTION1_TABLES_FACTORY_MOD, ONLY: MAKE_GRIB2_TABLES_CONFIGURATOR

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION1_000_T),  INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_TABLES_CONFIGURATOR
  TYPE(YAML_CONFIGURATION_T) :: TABLES_CONFIGURATOR_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: TABLES_CONFIGURATOR_NAME='tables-configurator'

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TABLES_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TABLES_CONFIGURATOR_CFG_NOT_PRESENT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_TABLES_CONFIGURATOR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%TABLES_), ERRFLAG_TABLES_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Check if a configuration for the indicator-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, TABLES_CONFIGURATOR_NAME, HAS_TABLES_CONFIGURATOR, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_TABLES_CONFIGURATOR, ERRFLAG_TABLES_CONFIGURATOR_CFG_NOT_PRESENT )

  !> Read the subconfiguration for the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, TABLES_CONFIGURATOR_NAME, TABLES_CONFIGURATOR_CFG, HOOKS )

  !> Make the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_TABLES_CONFIGURATOR) MAKE_GRIB2_TABLES_CONFIGURATOR( THIS%TABLES_, THIS%TEMPLATE_NUMBER_, TABLES_CONFIGURATOR_CFG, OPT, HOOKS )

  !> Deallocate the indicator-section configuration
  PP_TRYCALL( ERRFLAG_UNABLE_TO_DEALLOCATE ) YAML_DELETE_CONFIGURATION( TABLES_CONFIGURATOR_CFG, HOOKS )

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
    CASE ( ERRFLAG_TABLES_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'tables configurator already associated' )
    CASE ( ERRFLAG_UNABLE_TO_READ_CFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
    CASE ( ERRFLAG_TABLES_CONFIGURATOR_CFG_NOT_PRESENT )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'tables configurator configuration not present' )
    CASE ( ERRFLAG_UNABLE_TO_READ_SUBCFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfiguration' )
    CASE ( ERRFLAG_UNABLE_TO_MAKE_TABLES_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make tables configurator' )
    CASE ( ERRFLAG_UNABLE_TO_DEALLOCATE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate tables configurator object' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION1_000_BUILD_TABLES_FROM_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_000_BUILD_REFERENCE_TIME_FROM_CFG'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_000_BUILD_REFERENCE_TIME_FROM_CFG( THIS, &
&               CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,        ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_DELETE_CONFIGURATION
  USE :: GRIB2_SECTION1_REFERENCE_TIME_FACTORY_MOD, ONLY: MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION1_000_T),  INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_REFERENCE_TIME_CONFIGURATOR
  TYPE(YAML_CONFIGURATION_T) :: REFERENCE_TIME_CONFIGURATOR_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: REFERENCE_TIME_CONFIGURATOR_NAME='reference-time-configurator'

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_REFERENCE_TIME_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_REFERENCE_TIME_CONFIGURATOR_CFG_NOT_PRESENT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_REFERENCE_TIME_CONFIGURATOR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%REFERENCE_TIME_), ERRFLAG_REFERENCE_TIME_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Check if a configuration for the indicator-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, REFERENCE_TIME_CONFIGURATOR_NAME, HAS_REFERENCE_TIME_CONFIGURATOR, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_REFERENCE_TIME_CONFIGURATOR, ERRFLAG_REFERENCE_TIME_CONFIGURATOR_CFG_NOT_PRESENT )

  !> Read the subconfiguration for the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, REFERENCE_TIME_CONFIGURATOR_NAME, REFERENCE_TIME_CONFIGURATOR_CFG, HOOKS )

  !> Make the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_REFERENCE_TIME_CONFIGURATOR) MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR( THIS%REFERENCE_TIME_, THIS%TEMPLATE_NUMBER_, REFERENCE_TIME_CONFIGURATOR_CFG, OPT, HOOKS )

  !> Deallocate the indicator-section configuration
  PP_TRYCALL( ERRFLAG_UNABLE_TO_DEALLOCATE ) YAML_DELETE_CONFIGURATION( REFERENCE_TIME_CONFIGURATOR_CFG, HOOKS )

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
    CASE ( ERRFLAG_REFERENCE_TIME_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'reference time configurator already associated' )
    CASE ( ERRFLAG_UNABLE_TO_READ_CFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
    CASE ( ERRFLAG_REFERENCE_TIME_CONFIGURATOR_CFG_NOT_PRESENT )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'reference time configurator configuration not present' )
    CASE ( ERRFLAG_UNABLE_TO_READ_SUBCFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfiguration' )
    CASE ( ERRFLAG_UNABLE_TO_MAKE_REFERENCE_TIME_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make reference time configurator' )
    CASE ( ERRFLAG_UNABLE_TO_DEALLOCATE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate reference time configurator object' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION1_000_BUILD_REFERENCE_TIME_FROM_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_000_BUILD_DATA_TYPE_FROM_CFG'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_000_BUILD_DATA_TYPE_FROM_CFG( THIS, &
&               CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,               ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,        ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                       ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,             ONLY: YAML_DELETE_CONFIGURATION
  USE :: GRIB2_SECTION1_DATA_TYPE_FACTORY_MOD, ONLY: MAKE_GRIB2_DATA_TYPE_CONFIGURATOR

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_SECTION1_000_T),  INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_DATA_TYPE_CONFIGURATOR
  TYPE(YAML_CONFIGURATION_T) :: DATA_TYPE_CONFIGURATOR_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: DATA_TYPE_CONFIGURATOR_NAME='data-type-configurator'

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DATA_TYPE_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DATA_TYPE_CONFIGURATOR_CFG_NOT_PRESENT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_DATA_TYPE_CONFIGURATOR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%DATA_TYPE_), ERRFLAG_DATA_TYPE_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Check if a configuration for the indicator-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, DATA_TYPE_CONFIGURATOR_NAME, HAS_DATA_TYPE_CONFIGURATOR, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_DATA_TYPE_CONFIGURATOR, ERRFLAG_DATA_TYPE_CONFIGURATOR_CFG_NOT_PRESENT )

  !> Read the subconfiguration for the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, DATA_TYPE_CONFIGURATOR_NAME, DATA_TYPE_CONFIGURATOR_CFG, HOOKS )

  !> Make the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_DATA_TYPE_CONFIGURATOR) MAKE_GRIB2_DATA_TYPE_CONFIGURATOR( THIS%DATA_TYPE_, THIS%TEMPLATE_NUMBER_, DATA_TYPE_CONFIGURATOR_CFG, OPT, HOOKS )

  !> Deallocate the indicator-section configuration
  PP_TRYCALL( ERRFLAG_UNABLE_TO_DEALLOCATE ) YAML_DELETE_CONFIGURATION( DATA_TYPE_CONFIGURATOR_CFG, HOOKS )

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
    CASE ( ERRFLAG_DATA_TYPE_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data type configurator already associated' )
    CASE ( ERRFLAG_UNABLE_TO_READ_CFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
    CASE ( ERRFLAG_DATA_TYPE_CONFIGURATOR_CFG_NOT_PRESENT )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data type configurator configuration not present' )
    CASE ( ERRFLAG_UNABLE_TO_READ_SUBCFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfiguration' )
    CASE ( ERRFLAG_UNABLE_TO_MAKE_DATA_TYPE_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make data type configurator' )
    CASE ( ERRFLAG_UNABLE_TO_DEALLOCATE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate data type configurator object' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION1_000_BUILD_DATA_TYPE_FROM_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION1_000_PRINT'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION1_000_PRINT( THIS, &
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
  CLASS(GRIB2_SECTION1_000_T),  INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: OFFSET
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=*), OPTIONAL,   INTENT(IN)    :: SEPARATOR

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STAT

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_ERROR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_ORIGIN=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_TABLES=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_REFERENCE_TIME=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_DATA_TYPE=9_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%ORIGIN_), ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%TABLES_), ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%REFERENCE_TIME_), ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%DATA_TYPE_), ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED )

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
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_ORIGIN)         THIS%ORIGIN_%PRINT( UNIT, OFFSET+2, OPT, HOOKS, ', ...' )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_TABLES)         THIS%TABLES_%PRINT( UNIT, OFFSET+2, OPT, HOOKS, ', ...' )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_REFERENCE_TIME) THIS%REFERENCE_TIME_%PRINT( UNIT, OFFSET+2, OPT, HOOKS, ', ...' )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_DATA_TYPE)      THIS%DATA_TYPE_%PRINT( UNIT, OFFSET+2, OPT, HOOKS )

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
    CASE ( ERRFLAG_WRITE_ERROR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'write error' )
    CASE ( ERRFLAG_ORIGIN_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'origin handler not associated' )
    CASE ( ERRFLAG_TABLES_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'tables handler not associated' )
    CASE ( ERRFLAG_REFERENCE_TIME_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'reference time handler not associated' )
    CASE ( ERRFLAG_DATA_TYPE_HANDLER_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data type handler not associated' )
    CASE ( ERRFLAG_UNABLE_TO_PRINT_ORIGIN )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error print origin handler' )
    CASE ( ERRFLAG_UNABLE_TO_PRINT_TABLES )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error print tables handler' )
    CASE ( ERRFLAG_UNABLE_TO_PRINT_REFERENCE_TIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error print reference time handler' )
    CASE ( ERRFLAG_UNABLE_TO_PRINT_DATA_TYPE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error print data type handler' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION GRIB2_SECTION1_000_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB2_SECTION1_000_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
