!>
!> @file grib2_encoder_mod.F90
!>
!> @brief Module for managing GRIB2 Encoder operations.
!>
!> The `GRIB2_ENCODER_MOD` module contains procedures to initialize, allocate,
!> preset, run, and clean up the resources associated with GRIB2 Encoder objects.
!> This module provides thread-safe operations and includes extensive use of debugging,
!> logging, and tracing capabilities, making it robust for production and testing.
!>
!> The key operations covered by this module include:
!>   - Initialization of GRIB2 Encoder objects.
!>   - Allocation of resources.
!>   - Presetting internal parameters.
!>   - Managing runtime operations based on input parameters.
!>   - Cleaning up and deallocating resources after use.
!>
!> @section interface
!>
!> The module exports the following procedures:
!>   - @see GRIB2_ENCODER_INIT
!>   - @see GRIB2_ENCODER_ALLOCATE
!>   - @see GRIB2_ENCODER_PRESET
!>   - @see GRIB2_ENCODER_RUNTIME
!>   - @see GRIB2_ENCODER_TO_BE_ENCODED
!>   - @see GRIB2_ENCODER_FREE
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


#define PP_FILE_NAME 'grib2_encoder_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_ENCODER_MOD'
MODULE GRIB2_ENCODER_MOD

  !> Symbols imported from other modules within the project.
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!>
!> @brief Type definition for GRIB2 Encoder handler.
!>
!> The `GRIB2_ENCODER_T` type extends the base class `GRIB_SECTION_BASE_A` and
!> provides concrete implementations of initialization, allocation, preset, runtime,
!> encoding checks, and cleanup operations for GRIB2 Encoder objects.
!>
!> This type ensures that the required resources are properly managed through thread-safe,
!> non-overridable methods, providing robustness in both multi-threaded and single-threaded
!> environments.
!>
TYPE, EXTENDS(GRIB_SECTION_BASE_A) :: GRIB2_ENCODER_T

  !> Default visibility of the class members
  PRIVATE

  !>
  !> @brief Pointer to the indicator section of the GRIB2 message.
  !>
  !> This points to the section that contains general metadata about the GRIB2 message,
  !> including message length, GRIB version, and discipline.
  CLASS(GRIB_SECTION_BASE_A), POINTER :: INDICATOR_SECTION_ => NULL()

  !>
  !> @brief Pointer to the identification section of the GRIB2 message.
  !>
  !> This points to the section containing information about the originating center,
  !> subcenter, date, time, and type of forecast.
  CLASS(GRIB_SECTION_BASE_A), POINTER :: IDENTIFICATION_SECTION_ => NULL()

  !>
  !> @brief Pointer to the local use section of the GRIB2 message.
  !>
  !> This points to the optional local use section where specific centers
  !> can add custom data, if applicable.
  CLASS(GRIB_SECTION_BASE_A), POINTER :: LOCAL_USE_SECTION_ => NULL()

  !>
  !> @brief Pointer to the grid definition section of the GRIB2 message.
  !>
  !> This points to the section that defines the spatial grid used for
  !> the data representation, including grid dimensions and resolution.
  CLASS(GRIB_SECTION_BASE_A), POINTER :: GRID_DEFINITION_SECTION_ => NULL()

  !>
  !> @brief Pointer to the product definition section of the GRIB2 message.
  !>
  !> This points to the section that defines the parameter being represented,
  !> such as temperature or pressure, and its time range.
  CLASS(GRIB_SECTION_BASE_A), POINTER :: PRODUCT_DEFINITION_SECTION_ => NULL()

  !>
  !> @brief Pointer to the data representation section of the GRIB2 message.
  !>
  !> This points to the section that defines how the data values are packed
  !> or scaled in the GRIB2 message, including bit-widths and precision.
  CLASS(GRIB_SECTION_BASE_A), POINTER :: DATA_REPRESENTATION_SECTION_ => NULL()

  !>
  !> @brief Pointer to the bitmap section of the GRIB2 message.
  !>
  !> This points to the section that marks grid points as either missing
  !> or present, depending on the presence of the bitmap section.
  CLASS(GRIB_SECTION_BASE_A), POINTER :: BITMAP_SECTION_ => NULL()

CONTAINS

  !>
  !> @brief Initializes the GRIB2 Encoder object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The procedure starts from a yaml configuration file to construct the
  !> GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT_CFG => GRIB2_ENCODER_INIT_CFG

  !>
  !> @brief Initializes the GRIB2 Encoder object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The preocedure starts from a message and fro the parameters to construct
  !> the GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT_LAZY => GRIB2_ENCODER_INIT_LAZY

  !>
  !> @brief Allocates resources for the GRIB2 Encoder object.
  !>
  !> This procedure allocates memory and other necessary resources for
  !> the object based on provided parameters.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ALLOCATE => GRIB2_ENCODER_ALLOCATE

  !>
  !> @brief Presets the parameters of the GRIB2 Encoder object.
  !>
  !> This procedure configures the internal parameters of the object
  !> before runtime execution.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRESET => GRIB2_ENCODER_PRESET

  !>
  !> @brief Manages the runtime execution of GRIB2 Encoder operations.
  !>
  !> This procedure handles operations and computations during runtime,
  !> making use of time and metadata information.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: RUNTIME => GRIB2_ENCODER_RUNTIME

  !>
  !> @brief Determines if the GRIB2 Encoder object needs to be encoded.
  !>
  !> This procedure checks whether the object should be encoded based
  !> on the provided parameters and internal state.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: TO_BE_ENCODED => GRIB2_ENCODER_TO_BE_ENCODED

  !>
  !> @brief Frees resources allocated for the GRIB2 Encoder object.
  !>
  !> This procedure deallocates resources and performs cleanup after
  !> the object has been used.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => GRIB2_ENCODER_FREE

  !>
  !> @brief Print informations related to the section
  !>
  !> This procedure print informatin about the section and eventually call
  !> the print method of the nested sub-sections
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRINT => GRIB2_ENCODER_PRINT


  !>
  !> @brief Builds the indicator section of the GRIB2 message.
  !>
  !> This procedure constructs the indicator section, which provides general
  !> metadata about the GRIB2 message, including length and version.
  !>
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: BUILD_INDICATOR_SECTION => GRIB2_ENCODER_BUILD_INDICATOR_SECTION

  !>
  !> @brief Builds the identification section of the GRIB2 message.
  !>
  !> This procedure assembles the identification section, containing details
  !> about the originating center, subcenter, and date/time of data generation.
  !>
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: BUILD_IDENTIFICATION_SECTION => GRIB2_ENCODER_BUILD_IDENTIFICATION_SECTION

  !>
  !> @brief Builds the local use section of the GRIB2 message.
  !>
  !> This procedure constructs the optional local use section, allowing for
  !> custom data to be added by specific centers or users.
  !>
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: BUILD_LOCAL_USE_SECTION => GRIB2_ENCODER_BUILD_LOCAL_USE_SECTION

  !>
  !> @brief Builds the grid definition section of the GRIB2 message.
  !>
  !> This procedure assembles the grid definition section, specifying the
  !> grid system and resolution used in the data.
  !>
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: BUILD_GRID_DEFINITION_SECTION => GRIB2_ENCODER_BUILD_GRID_DEFINITION_SECTION

  !>
  !> @brief Builds the product definition section of the GRIB2 message.
  !>
  !> This procedure constructs the product definition section, which
  !> describes the product being represented, such as the variable and time interval.
  !>
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: BUILD_PRODUCT_DEFINITION_SECTION => GRIB2_ENCODER_BUILD_PRODUCT_DEFINITION_SECTION

  !>
  !> @brief Builds the data representation section of the GRIB2 message.
  !>
  !> This procedure assembles the data representation section, defining
  !> the packing and scaling details for the stored data values.
  !>
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: BUILD_DATA_REPRESENTATION_SECTION => GRIB2_ENCODER_BUILD_DATA_REPRESENTATION_SECTION
#if 0

  !>
  !> @brief Builds the bitmap section of the GRIB2 message.
  !>
  !> This procedure constructs the bitmap section, used to indicate missing
  !> data points in the grid, if applicable.
  !>
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: BUILD_BITMAP_SECTION => GRIB2_ENCODER_BUILD_BITMAP_SECTION
#endif
END TYPE


!>
!> Public symbols (dataTypes)
PUBLIC :: GRIB2_ENCODER_T

CONTAINS

!>
!> @brief Initializes GRIB2 Encoder for a given object using the provided parameters.
!>
!> This function initializes a GRIB2 Encoder object (`THIS`) using the provided model parameters (`PARAMS`)
!> and configuration data (`CFG`). The process can be run in verbose mode if specified. The function
!> is thread-safe and returns an error code indicating the success or failure of the operation.
!>
!> @section interface
!>
!> @param [inout] THIS    GRIB2 Encoder object to be initialized.
!> @param [in]    PARAMS  Model parameters used during initialization.
!> @param [in]    CFG     YAML configuration data for initialization.
!> @param [inout] HOOKS A structure of type `HOOKS_T` that contains hooks for initialization.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] GRIB_ENCODER_OPTIONS_MOD::GRIB_ENCODER_OPTIONS_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_ENCODER_INIT
!> @see GRIB2_ENCODER_ALLOCATE
!> @see GRIB2_ENCODER_PRESET
!> @see GRIB2_ENCODER_RUNTIME
!> @see GRIB2_ENCODER_TO_BE_ENCODED
!> @see GRIB2_ENCODER_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_INIT_CFG'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_INIT_CFG( THIS, &
&               CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,   ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_ENCODER_T),       INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_INDICATOR_SECTION = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_IDENTIFICATION_SECTION = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_LOCAL_USE_SECTION = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_GRID_DEFINITION_SECTION = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_PRODUCT_DEFINITION_SECTION = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_DATA_REPRESENTATION_SECTION = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_BITMAP_SECTION = 7_JPIB_K

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

  ! Initialize the GRIB2 encoder object
  THIS%TYPE_ = 'ENCODER'
  THIS%SUBTYPE_ = 'GRIB2'
  THIS%KIND_ = '000'

  !> Build the indicator section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_INDICATOR_SECTION) &
&          THIS%BUILD_INDICATOR_SECTION( CFG, OPT, HOOKS )

  !> Build the identification section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_IDENTIFICATION_SECTION) &
&          THIS%BUILD_IDENTIFICATION_SECTION( CFG, OPT, HOOKS )

  !> Build the local use section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_LOCAL_USE_SECTION) &
&          THIS%BUILD_LOCAL_USE_SECTION( CFG, OPT, HOOKS )

  !> Build the grid definition section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_GRID_DEFINITION_SECTION) &
&           THIS%BUILD_GRID_DEFINITION_SECTION( CFG, OPT, HOOKS )

  !> Build the product definition section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_PRODUCT_DEFINITION_SECTION) &
&           THIS%BUILD_PRODUCT_DEFINITION_SECTION( CFG, OPT, HOOKS )

  !> Build the data representation section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_DATA_REPRESENTATION_SECTION) &
&           THIS%BUILD_DATA_REPRESENTATION_SECTION( CFG, OPT, HOOKS )

#if 0
  !> Build the bitmap section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_BITMAP_SECTION) &
&           THIS%BUILD_BITMAP_SECTION( CFG, OPT, HOOKS )
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
    CASE(ERRFLAG_UNABLE_TO_BUILD_INDICATOR_SECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build indicator section' )
    CASE(ERRFLAG_UNABLE_TO_BUILD_IDENTIFICATION_SECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build identification section' )
    CASE(ERRFLAG_UNABLE_TO_BUILD_LOCAL_USE_SECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build local use section' )
    CASE(ERRFLAG_UNABLE_TO_BUILD_GRID_DEFINITION_SECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build grid definition section' )
    CASE(ERRFLAG_UNABLE_TO_BUILD_PRODUCT_DEFINITION_SECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build product definition section' )
    CASE(ERRFLAG_UNABLE_TO_BUILD_DATA_REPRESENTATION_SECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build data representation section' )
    CASE(ERRFLAG_UNABLE_TO_BUILD_BITMAP_SECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build bitmap section' )
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

END FUNCTION GRIB2_ENCODER_INIT_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Initializes GRIB2 Encoder for a given object using the provided parameters.
!>
!> This function initializes a GRIB2 Encoder object (`THIS`) using the provided model parameters (`PARAMS`)
!> and configuration data (`CFG`). The process can be run in verbose mode if specified. The function
!> is thread-safe and returns an error code indicating the success or failure of the operation.
!>
!> @section interface
!>
!> @param [inout] THIS    GRIB2 Encoder object to be initialized.
!> @param [in]    MSG     All the mars keywords needed to describe the field `FORTRAN_MESSAGE_T`.
!> @param [in]    PAR     All information outside mars keywords needed to describe the field `PARAMETRIZATION_T`.
!> @param [in]    OPT     The encoder options structure of type `ENCODER_OPTIONS_T`.
!> @param [inout] HOOKS A structure of type `HOOKS_T` that contains hooks for initialization.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] GRIB_ENCODER_OPTIONS_MOD::GRIB_ENCODER_OPTIONS_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_ENCODER_INIT
!> @see GRIB2_ENCODER_ALLOCATE
!> @see GRIB2_ENCODER_PRESET
!> @see GRIB2_ENCODER_RUNTIME
!> @see GRIB2_ENCODER_TO_BE_ENCODED
!> @see GRIB2_ENCODER_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_INIT_LAZY'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_INIT_LAZY( THIS, &
&               MSG, PAR, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

#if 0
  !> Import the factories
  USE :: GRIB2_SECTION0_FACTORY_MOD, ONLY: MAKE_GRIB2_SECTION0
  USE :: GRIB2_SECTION1_FACTORY_MOD, ONLY: READ_GRIB2_SECTION1
  USE :: GRIB2_SECTION2_FACTORY_MOD, ONLY: READ_GRIB2_SECTION2
  USE :: GRIB2_SECTION3_FACTORY_MOD, ONLY: READ_GRIB2_SECTION3
  USE :: GRIB2_SECTION4_FACTORY_MOD, ONLY: READ_GRIB2_SECTION4
  USE :: GRIB2_SECTION5_FACTORY_MOD, ONLY: READ_GRIB2_SECTION5
  USE :: GRIB2_SECTION6_FACTORY_MOD, ONLY: READ_GRIB2_SECTION6
#endif
  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_ENCODER_T),       INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_INDICATOR_SECTION = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_IDENTIFICATION_SECTION = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_LOCAL_USE_SECTION = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_GRID_DEFINITION_SECTION = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_PRODUCT_DEFINITION_SECTION = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_DATA_REPRESENTATION_SECTION = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_BITMAP_SECTION = 7_JPIB_K

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

  ! Initialize the GRIB2 encoder object
  THIS%TYPE_ = 'ENCODER'
  THIS%SUBTYPE_ = 'GRIB2'
  THIS%KIND_ = '000'
#if 0
  !> Build the indicator section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_INDICATOR_SECTION) &
&          MAKE_GRIB2_SECTION0( THIS%INDICATOR_SECTION_, MSG, PAR, OPT, HOOKS )

  !> Build the identification section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_IDENTIFICATION_SECTION) &
&          READ_GRIB2_SECTION1( THIS%INDICATOR_SECTION_, MSG, PAR, OPT, HOOKS )


  !> Build the local use section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_LOCAL_USE_SECTION) &
&          READ_GRIB2_SECTION2( THIS%IDENTIFICATION_SECTION_, MSG, PAR, OPT, HOOKS )

  !> Build the grid definition section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_GRID_DEFINITION_SECTION) &
&           READ_GRIB2_SECTION3( THIS%GRID_DEFINITION_SECTION_, MSG, PAR, OPT, HOOKS )

  !> Build the product definition section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_PRODUCT_DEFINITION_SECTION) &
&           READ_GRIB2_SECTION4( THIS%PRODUCT_DEFINITION_SECTION_, MSG, PAR, OPT, HOOKS )

  !> Build the data representation section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_DATA_REPRESENTATION_SECTION) &
&           READ_GRIB2_SECTION5( THIS%DATA_REPRESENTATION_SECTION_, MSG, PAR, OPT, HOOKS )

  !> Build the bitmap section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_BITMAP_SECTION) &
&           READ_GRIB2_SECTION6( THIS%BITMAP_SECTION_, MSG, PAR, OPT, HOOKS )
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
    CASE(ERRFLAG_UNABLE_TO_BUILD_INDICATOR_SECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build indicator section' )
    CASE(ERRFLAG_UNABLE_TO_BUILD_IDENTIFICATION_SECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build identification section' )
    CASE(ERRFLAG_UNABLE_TO_BUILD_LOCAL_USE_SECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build local use section' )
    CASE(ERRFLAG_UNABLE_TO_BUILD_GRID_DEFINITION_SECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build grid definition section' )
    CASE(ERRFLAG_UNABLE_TO_BUILD_PRODUCT_DEFINITION_SECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build product definition section' )
    CASE(ERRFLAG_UNABLE_TO_BUILD_DATA_REPRESENTATION_SECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build data representation section' )
    CASE(ERRFLAG_UNABLE_TO_BUILD_BITMAP_SECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build bitmap section' )
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

END FUNCTION GRIB2_ENCODER_INIT_LAZY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Build the indicator section of a GRIB2 message.
!>
!> This function constructs the indicator section (Section 0) of a GRIB2 message.
!> It utilizes the provided YAML configuration, encoder options, and hooks to create
!> and populate the indicator section, which is added to the GRIB2 encoder object.
!>
!> @pre The GRIB2 encoder object (`THIS`) must be initialized. The YAML configuration (`ENCODER_CFG`)
!> should contain the necessary keys required for building the indicator section.
!>
!> @param [inout] THIS         The GRIB2 encoder object being populated with sections.
!> @param [in]    OPT          The GRIB encoder options that affect the creation of this section.
!> @param [in]    ENCODER_CFG  The YAML configuration object containing the necessary keys for this section.
!> @param [inout] HOOKS        A structure providing hooks for external dependencies or callback functions.
!>
!> @return Integer error code (`RET`) indicating success or failure.
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!> - @dependency [PROCEDURE] YAML_CONFIGURATION_HAS_KEY
!> - @dependency [PROCEDURE] YAML_GET_SUBCONFIGURATION
!> - @dependency [PROCEDURE] YAML_DELETE_CONFIGURATION
!> - @dependency [PROCEDURE] READ_GRIB2_SECTION0_TYPE
!> - @dependency [PROCEDURE] MAKE_GRIB2_SECTION0
!>
!> @section special dependencies:
!>   - @dependency [*] PP_DEBUG_USE_VARS
!>   - @dependency [*] PP_LOG_USE_VARS
!>   - @dependency [*] PP_TRACE_USE_VARS
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_BUILD_INDICATOR_SECTION'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_BUILD_INDICATOR_SECTION( THIS, &
&               ENCODER_CFG, ENCODER_OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,   ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_DELETE_CONFIGURATION
  USE :: GRIB2_SECTION0_FACTORY_MOD, ONLY: MAKE_GRIB2_SECTION0

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_ENCODER_T),       INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: ENCODER_OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: ENCODER_CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: CFG_HAS_INDICATOR_SECTION
  TYPE(YAML_CONFIGURATION_T) :: INDICATOR_SECTION_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: INDICATOR_SECTION_NAME = 'indicator-section'

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_SEC0 = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECTION_DEALLOCATION_ERROR = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC0_ALREADY_ASSOCIATED = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SEC0_TYPE = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC0 = 7_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%INDICATOR_SECTION_), ERRFLAG_SEC0_ALREADY_ASSOCIATED )

  !> Check if a configuration for the indicator-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( ENCODER_CFG, INDICATOR_SECTION_NAME, CFG_HAS_INDICATOR_SECTION, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. CFG_HAS_INDICATOR_SECTION, ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC0 )

  !> Read the subconfiguration for the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( ENCODER_CFG, INDICATOR_SECTION_NAME, INDICATOR_SECTION_CFG, HOOKS )

  !> Make the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_SEC0) MAKE_GRIB2_SECTION0( THIS%INDICATOR_SECTION_, INDICATOR_SECTION_CFG, ENCODER_OPT, HOOKS )

  !> Deallocate the indicator-section configuration
  PP_TRYCALL( ERRFLAG_SECTION_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( INDICATOR_SECTION_CFG, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration for indicator-section' )
    CASE(ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfigurations for indicator-section' )
    CASE(ERRFLAG_UNABLE_TO_MAKE_SEC0)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make indicator-section' )
    CASE(ERRFLAG_SECTION_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating indicator-section configuration' )
    CASE (ERRFLAG_SEC0_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'indicator-section already associated' )
    CASE (ERRFLAG_UNABLE_TO_READ_SEC0_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read indicator-section' )
    CASE (ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC0)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration for indicator-section' )
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

END FUNCTION GRIB2_ENCODER_BUILD_INDICATOR_SECTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Build the identification section of a GRIB2 message.
!>
!> This function constructs the identification section (Section 1) of a GRIB2 message.
!> It utilizes the provided YAML configuration, encoder options, and hooks to create
!> and populate the identification section, which is added to the GRIB2 encoder object.
!>
!> @pre The GRIB2 encoder object (`THIS`) must be initialized. The YAML configuration (`ENCODER_CFG`)
!> should contain the necessary keys required for building the identification section.
!>
!> @param [inout] THIS         The GRIB2 encoder object being populated with sections.
!> @param [in]    OPT          The GRIB encoder options that affect the creation of this section.
!> @param [in]    ENCODER_CFG  The YAML configuration object containing the necessary keys for this section.
!> @param [inout] HOOKS        A structure providing hooks for external dependencies or callback functions.
!>
!> @return Integer error code (`RET`) indicating success or failure.
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!> - @dependency [PROCEDURE] YAML_CONFIGURATION_HAS_KEY
!> - @dependency [PROCEDURE] YAML_GET_SUBCONFIGURATION
!> - @dependency [PROCEDURE] YAML_DELETE_CONFIGURATION
!> - @dependency [PROCEDURE] READ_GRIB2_SECTION1_TYPE
!> - @dependency [PROCEDURE] MAKE_GRIB2_SECTION1
!>
!> @section special dependencies:
!>   - @dependency [*] PP_DEBUG_USE_VARS
!>   - @dependency [*] PP_LOG_USE_VARS
!>   - @dependency [*] PP_TRACE_USE_VARS
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_BUILD_IDENTIFICATION_SECTION'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_BUILD_IDENTIFICATION_SECTION( THIS, &
&               ENCODER_CFG, ENCODER_OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,   ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_DELETE_CONFIGURATION
  USE :: GRIB2_SECTION1_FACTORY_MOD, ONLY: MAKE_GRIB2_SECTION1

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_ENCODER_T),       INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: ENCODER_OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: ENCODER_CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: CFG_HAS_IDENTIFICATION_SECTION
  TYPE(YAML_CONFIGURATION_T) :: IDENTIFICATION_SECTION_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: IDENTIFICATION_SECTION_NAME = 'identification-section'

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_SEC1 = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECTION_DEALLOCATION_ERROR = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC1_ASSOCIATED = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SEC1_TYPE = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC1 = 7_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%IDENTIFICATION_SECTION_), ERRFLAG_SEC1_ASSOCIATED )

  !> Check if a configuration for the identification-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( ENCODER_CFG, IDENTIFICATION_SECTION_NAME, CFG_HAS_IDENTIFICATION_SECTION, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. CFG_HAS_IDENTIFICATION_SECTION, ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC1 )

  !> Read the subconfiguration for the identification-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( ENCODER_CFG, IDENTIFICATION_SECTION_NAME, IDENTIFICATION_SECTION_CFG, HOOKS )

  !> Make the identification-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_SEC1) MAKE_GRIB2_SECTION1( THIS%IDENTIFICATION_SECTION_, IDENTIFICATION_SECTION_CFG, ENCODER_OPT, HOOKS )

  !> Deallocate the identification-section configuration
  PP_TRYCALL( ERRFLAG_SECTION_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( IDENTIFICATION_SECTION_CFG, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration for identification-section' )
    CASE(ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfigurations for identification-section' )
    CASE(ERRFLAG_UNABLE_TO_MAKE_SEC1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make identification-section' )
    CASE(ERRFLAG_SECTION_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating identification-section configuration' )
    CASE (ERRFLAG_SEC1_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'identification-section already associated' )
    CASE (ERRFLAG_UNABLE_TO_READ_SEC1_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read identification-section' )
    CASE (ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration for identification-section' )
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

END FUNCTION GRIB2_ENCODER_BUILD_IDENTIFICATION_SECTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Build the local use section of a GRIB2 message.
!>
!> This function constructs the local use section (Section 2) of a GRIB2 message.
!> It utilizes the provided YAML configuration, encoder options, and hooks to create
!> and populate the local use section, which is added to the GRIB2 encoder object.
!>
!> @pre The GRIB2 encoder object (`THIS`) must be initialized. The YAML configuration (`ENCODER_CFG`)
!> should contain the necessary keys required for building the local use section.
!>
!> @param [inout] THIS         The GRIB2 encoder object being populated with sections.
!> @param [in]    OPT          The GRIB encoder options that affect the creation of this section.
!> @param [in]    ENCODER_CFG  The YAML configuration object containing the necessary keys for this section.
!> @param [inout] HOOKS        A structure providing hooks for external dependencies or callback functions.
!>
!> @return Integer error code (`RET`) indicating success or failure.
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!> - @dependency [PROCEDURE] YAML_CONFIGURATION_HAS_KEY
!> - @dependency [PROCEDURE] YAML_GET_SUBCONFIGURATION
!> - @dependency [PROCEDURE] YAML_DELETE_CONFIGURATION
!> - @dependency [PROCEDURE] READ_GRIB2_SECTION2_TYPE
!> - @dependency [PROCEDURE] MAKE_GRIB2_SECTION2
!>
!> @section special dependencies:
!>   - @dependency [*] PP_DEBUG_USE_VARS
!>   - @dependency [*] PP_LOG_USE_VARS
!>   - @dependency [*] PP_TRACE_USE_VARS
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_BUILD_LOCAL_USE_SECTION'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_BUILD_LOCAL_USE_SECTION( THIS, &
&               ENCODER_CFG, ENCODER_OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,   ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_DELETE_CONFIGURATION
  USE :: GRIB2_SECTION2_FACTORY_MOD, ONLY: MAKE_GRIB2_SECTION2

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_ENCODER_T),       INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: ENCODER_OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: ENCODER_CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: CFG_HAS_LOCAL_USE_SECTION
  TYPE(YAML_CONFIGURATION_T) :: LOCAL_USE_SECTION_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: LOCAL_USE_SECTION_NAME = 'local-use-section'

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_SEC2 = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECTION_DEALLOCATION_ERROR = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC2_ALREADY_ASSOCIATED = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SEC2_TYPE = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC2 = 7_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%LOCAL_USE_SECTION_), ERRFLAG_SEC2_ALREADY_ASSOCIATED )

  !> Check if a configuration for the local-use-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( ENCODER_CFG, LOCAL_USE_SECTION_NAME, CFG_HAS_LOCAL_USE_SECTION, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. CFG_HAS_LOCAL_USE_SECTION, ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC2 )

  !> Read the subconfiguration for the local-use-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( ENCODER_CFG, LOCAL_USE_SECTION_NAME, LOCAL_USE_SECTION_CFG, HOOKS )

  !> Make the local-use-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_SEC2) MAKE_GRIB2_SECTION2( THIS%LOCAL_USE_SECTION_, LOCAL_USE_SECTION_CFG, ENCODER_OPT, HOOKS )

  !> Deallocate the local-use-section configuration
  PP_TRYCALL( ERRFLAG_SECTION_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( LOCAL_USE_SECTION_CFG, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration for local-use-section' )
    CASE(ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfigurations for local-use-section' )
    CASE(ERRFLAG_UNABLE_TO_MAKE_SEC2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make local-use-section' )
    CASE(ERRFLAG_SECTION_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating local-use-section configuration' )
    CASE (ERRFLAG_SEC2_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'local-use-section already associated' )
    CASE (ERRFLAG_UNABLE_TO_READ_SEC2_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read local-use-section' )
    CASE (ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration for local-use-section' )
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

END FUNCTION GRIB2_ENCODER_BUILD_LOCAL_USE_SECTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Build the grid definition section of a GRIB2 message.
!>
!> This function constructs the grid definition section (Section 3) of a GRIB2 message.
!> It uses the provided YAML configuration, encoder options, and hooks to create and
!> populate the grid definition section, which is added to the GRIB2 encoder object.
!>
!> @pre The GRIB2 encoder object (`THIS`) must be initialized. The YAML configuration (`ENCODER_CFG`)
!> should contain the necessary keys required for building the grid definition section.
!>
!> @param [inout] THIS         The GRIB2 encoder object being populated with sections.
!> @param [in]    OPT          The GRIB encoder options that affect the creation of this section.
!> @param [in]    ENCODER_CFG  The YAML configuration object containing the necessary keys for this section.
!> @param [inout] HOOKS        A structure providing hooks for external dependencies or callback functions.
!>
!> @return Integer error code (`RET`) indicating success or failure.
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!> - @dependency [PROCEDURE] YAML_CONFIGURATION_HAS_KEY
!> - @dependency [PROCEDURE] YAML_GET_SUBCONFIGURATION
!> - @dependency [PROCEDURE] YAML_DELETE_CONFIGURATION
!> - @dependency [PROCEDURE] READ_GRIB2_SECTION3_TYPE
!> - @dependency [PROCEDURE] MAKE_GRIB2_SECTION3
!>
!> @section special dependencies:
!>   - @dependency [*] PP_DEBUG_USE_VARS
!>   - @dependency [*] PP_LOG_USE_VARS
!>   - @dependency [*] PP_TRACE_USE_VARS
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_BUILD_GRID_DEFINITION_SECTION'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_BUILD_GRID_DEFINITION_SECTION( THIS, &
&               ENCODER_CFG, ENCODER_OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,   ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_DELETE_CONFIGURATION
  USE :: GRIB2_SECTION3_FACTORY_MOD, ONLY: MAKE_GRIB2_SECTION3

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_ENCODER_T),       INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: ENCODER_OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: ENCODER_CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: CFG_HAS_GRID_DEFINITION_SECTION
  TYPE(YAML_CONFIGURATION_T) :: GRID_DEFINITION_SECTION_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: GRID_DEFINITION_SECTION_NAME = 'grid-definition-section'

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_SEC3 = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECTION_DEALLOCATION_ERROR = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC3_ALREADY_ASSOCIATED = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SEC3_TYPE = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC3 = 7_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%GRID_DEFINITION_SECTION_), ERRFLAG_SEC3_ALREADY_ASSOCIATED )

  !> Check if a configuration for the grid-definition-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( ENCODER_CFG, GRID_DEFINITION_SECTION_NAME, CFG_HAS_GRID_DEFINITION_SECTION, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. CFG_HAS_GRID_DEFINITION_SECTION, ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC3 )

  !> Read the subconfiguration for the grid-definition-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( ENCODER_CFG, GRID_DEFINITION_SECTION_NAME, GRID_DEFINITION_SECTION_CFG, HOOKS )

  !> Make the grid-definition-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_SEC3) MAKE_GRIB2_SECTION3( THIS%GRID_DEFINITION_SECTION_, GRID_DEFINITION_SECTION_CFG, ENCODER_OPT, HOOKS )

  !> Deallocate the grid-definition-section configuration
  PP_TRYCALL( ERRFLAG_SECTION_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( GRID_DEFINITION_SECTION_CFG, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration for grid-definition-section' )
    CASE(ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfigurations for grid-definition-section' )
    CASE(ERRFLAG_UNABLE_TO_MAKE_SEC3)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make grid-definition-section' )
    CASE(ERRFLAG_SECTION_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating grid-definition-section configuration' )
    CASE (ERRFLAG_SEC3_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grid-definition-section already associated' )
    CASE (ERRFLAG_UNABLE_TO_READ_SEC3_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read grid-definition-section' )
    CASE (ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC3)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration for grid-definition-section' )
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

END FUNCTION GRIB2_ENCODER_BUILD_GRID_DEFINITION_SECTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Build the product definition section of a GRIB2 message.
!>
!> This function constructs the product definition section (Section 4) of a GRIB2 message.
!> It utilizes the provided YAML configuration, encoder options, and hooks to create the structure
!> and content of the product definition section and adds it to the GRIB2 encoder object.
!>
!> @pre The GRIB2 encoder object (`THIS`) should be initialized and ready for adding sections.
!> The YAML configuration (`ENCODER_CFG`) must contain the necessary keys for building the product
!> definition section.
!>
!> @param [inout] THIS         The GRIB2 encoder object being populated with sections.
!> @param [in]    OPT          The GRIB encoder options that affect the creation of this section.
!> @param [in]    ENCODER_CFG  The YAML configuration object containing the necessary keys for this section.
!> @param [inout] HOOKS        A structure providing hooks for external dependencies or callback functions.
!>
!> @return Integer error code (`RET`) indicating success or failure.
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!> - @dependency [PROCEDURE] YAML_CONFIGURATION_HAS_KEY
!> - @dependency [PROCEDURE] YAML_GET_SUBCONFIGURATION
!> - @dependency [PROCEDURE] YAML_DELETE_CONFIGURATION
!> - @dependency [PROCEDURE] READ_GRIB2_SECTION4_TYPE
!> - @dependency [PROCEDURE] MAKE_GRIB2_SECTION4
!>
!> @section special dependencies:
!>   - @dependency [*] PP_DEBUG_USE_VARS
!>   - @dependency [*] PP_LOG_USE_VARS
!>   - @dependency [*] PP_TRACE_USE_VARS
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_BUILD_PRODUCT_DEFINITION_SECTION'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_BUILD_PRODUCT_DEFINITION_SECTION( THIS, &
&               ENCODER_CFG, ENCODER_OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,   ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_DELETE_CONFIGURATION
  USE :: GRIB2_SECTION4_FACTORY_MOD, ONLY: MAKE_GRIB2_SECTION4

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_ENCODER_T),       INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: ENCODER_OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: ENCODER_CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: CFG_HAS_PRODUCT_DEFINITION_SECTION
  TYPE(YAML_CONFIGURATION_T) :: PRODUCT_DEFINITION_SECTION_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: PRODUCT_DEFINITION_SECTION_NAME = 'product-definition-section'

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_SEC4 = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECTION_DEALLOCATION_ERROR = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC4_ALREADY_ASSOCIATED = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SEC4_TYPE = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC4 = 7_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%PRODUCT_DEFINITION_SECTION_), ERRFLAG_SEC4_ALREADY_ASSOCIATED )

  !> Check if a configuration for the product-definition-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( ENCODER_CFG, PRODUCT_DEFINITION_SECTION_NAME, CFG_HAS_PRODUCT_DEFINITION_SECTION, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. CFG_HAS_PRODUCT_DEFINITION_SECTION, ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC4 )

  !> Read the subconfiguration for the product-definition-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( ENCODER_CFG, PRODUCT_DEFINITION_SECTION_NAME, PRODUCT_DEFINITION_SECTION_CFG, HOOKS )

  !> Make the product-definition-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_SEC4) MAKE_GRIB2_SECTION4( THIS%PRODUCT_DEFINITION_SECTION_, PRODUCT_DEFINITION_SECTION_CFG, ENCODER_OPT, HOOKS )

  !> Deallocate the product-definition-section configuration
  PP_TRYCALL( ERRFLAG_SECTION_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( PRODUCT_DEFINITION_SECTION_CFG, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration for product-definition-section' )
    CASE(ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfigurations for product-definition-section' )
    CASE(ERRFLAG_UNABLE_TO_MAKE_SEC4)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make product-definition-section' )
    CASE(ERRFLAG_SECTION_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating product-definition-section configuration' )
    CASE (ERRFLAG_SEC4_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'product-definition-section already associated' )
    CASE (ERRFLAG_UNABLE_TO_READ_SEC4_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read product-definition-section' )
    CASE (ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC4)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration for product-definition-section' )
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

END FUNCTION GRIB2_ENCODER_BUILD_PRODUCT_DEFINITION_SECTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Build the data representation section of a GRIB2 message.
!>
!> This function constructs the data representation section (Section 5) of a GRIB2 message.
!> It uses the provided YAML configuration, encoder options, and hooks to determine the structure
!> and content of the data representation section, and adds it to the GRIB2 encoder object.
!>
!> @pre The GRIB2 encoder object (`THIS`) should be initialized and ready for adding sections.
!> The YAML configuration (`ENCODER_CFG`) must contain the necessary keys for building the data
!> representation section.
!>
!> @param [inout] THIS         The GRIB2 encoder object being populated with sections.
!> @param [in]    OPT          The GRIB encoder options influencing the creation of this section.
!> @param [in]    ENCODER_CFG  The YAML configuration object with the relevant keys for this section.
!> @param [inout] HOOKS        A structure providing hooks for external dependencies or callback functions.
!>
!> @return Integer error code (`RET`) indicating success or failure.
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!> - @dependency [PROCEDURE] YAML_CONFIGURATION_HAS_KEY
!> - @dependency [PROCEDURE] YAML_GET_SUBCONFIGURATION
!> - @dependency [PROCEDURE] YAML_DELETE_CONFIGURATION
!> - @dependency [PROCEDURE] READ_GRIB2_SECTION5_TYPE
!> - @dependency [PROCEDURE] MAKE_GRIB2_SECTION5
!>
!> @section special dependencies:
!>   - @dependency [*] PP_DEBUG_USE_VARS
!>   - @dependency [*] PP_LOG_USE_VARS
!>   - @dependency [*] PP_TRACE_USE_VARS
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_BUILD_DATA_REPRESENTATION_SECTION'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_BUILD_DATA_REPRESENTATION_SECTION( THIS, &
&               ENCODER_CFG, ENCODER_OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,   ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_DELETE_CONFIGURATION
  USE :: GRIB2_SECTION5_FACTORY_MOD, ONLY: MAKE_GRIB2_SECTION5

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_ENCODER_T),       INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: ENCODER_OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: ENCODER_CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: CFG_HAS_DATA_REPRESENTATION_SECTION
  TYPE(YAML_CONFIGURATION_T) :: DATA_REPRESENTATION_SECTION_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: DATA_REPRESENTATION_SECTION_NAME = 'data-representation-section'

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_SEC5 = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECTION_DEALLOCATION_ERROR = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC5_ALREADY_ASSOCIATED = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC5 = 7_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%DATA_REPRESENTATION_SECTION_), ERRFLAG_SEC5_ALREADY_ASSOCIATED )

  !> Check if a configuration for the data-represntation-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( ENCODER_CFG, DATA_REPRESENTATION_SECTION_NAME, CFG_HAS_DATA_REPRESENTATION_SECTION, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. CFG_HAS_DATA_REPRESENTATION_SECTION, ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC5 )

  !> Read the subconfiguration for the data-represntation-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( ENCODER_CFG, DATA_REPRESENTATION_SECTION_NAME, DATA_REPRESENTATION_SECTION_CFG, HOOKS )

  !> Make the data-represntation-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_SEC5) MAKE_GRIB2_SECTION5( THIS%DATA_REPRESENTATION_SECTION_, DATA_REPRESENTATION_SECTION_CFG, ENCODER_OPT, HOOKS )

  !> Deallocate the data-represntation-section configuration
  PP_TRYCALL( ERRFLAG_SECTION_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( DATA_REPRESENTATION_SECTION_CFG, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration for data-represntation-section' )
    CASE(ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfigurations for data-represntation-section' )
    CASE(ERRFLAG_UNABLE_TO_MAKE_SEC5)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make data-represntation-section' )
    CASE(ERRFLAG_SECTION_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating data-represntation-section configuration' )
    CASE (ERRFLAG_SEC5_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data-represntation-section already associated' )
    CASE (ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC5)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration for data-represntation-section' )
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

END FUNCTION GRIB2_ENCODER_BUILD_DATA_REPRESENTATION_SECTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#if 0
!>
!> @brief Build the bitmap section of a GRIB2 message.
!>
!> This function builds the bitmap section (Section 6) of a GRIB2 message based on the provided
!> YAML configuration, options, and hooks. It reads the necessary parameters from the configuration,
!> creates the bitmap section, and integrates it into the GRIB2 encoder object.
!>
!> @pre This function assumes that the GRIB2 encoder object (`THIS`) has been properly initialized.
!> It also expects the configuration (`ENCODER_CFG`) to contain the necessary keys for
!> building the bitmap section.
!>
!> @param [inout] THIS         The GRIB2 encoder object that holds the current state of the GRIB2 message being encoded.
!> @param [in]    OPT          The GRIB encoder options that influence the encoding process.
!> @param [in]    ENCODER_CFG  The YAML configuration object containing the settings for building the bitmap section.
!> @param [inout] HOOKS        A structure that contains hooks for managing external dependencies or callbacks during encoding.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies Dependencies of this function:
!> - @dependency [PROCEDURE] YAML_CONFIGURATION_HAS_KEY
!> - @dependency [PROCEDURE] YAML_GET_SUBCONFIGURATION
!> - @dependency [PROCEDURE] YAML_DELETE_CONFIGURATION
!> - @dependency [PROCEDURE] READ_GRIB2_SECTION6_TYPE
!> - @dependency [PROCEDURE] MAKE_GRIB2_SECTION6
!>
!> @section special dependencies:
!>   - @dependency [*] PP_DEBUG_USE_VARS
!>   - @dependency [*] PP_LOG_USE_VARS
!>   - @dependency [*] PP_TRACE_USE_VARS
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_BUILD_BITMAP_SECTION'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_BUILD_BITMAP_SECTION( THIS, &
&               ENCODER_CFG, ENCODER_OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,   ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,        ONLY: YAML_DELETE_CONFIGURATION
  USE :: GRIB2_SECTION6_FACTORY_MOD, ONLY: READ_GRIB2_SECTION6_TYPE
  USE :: GRIB2_SECTION6_FACTORY_MOD, ONLY: MAKE_GRIB2_SECTION6

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_ENCODER_T),       INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: ENCODER_OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: ENCODER_CFG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: CFG_HAS_BITMAP_SECTION
  INTEGER(KIND=JPIB_K) :: BITMAP_SECTION_TYPE
  TYPE(YAML_CONFIGURATION_T) :: BITMAP_SECTION_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: BITMAP_SECTION_NAME = 'bitmap-section'

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_SEC6 = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECTION_DEALLOCATION_ERROR = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC6_ALREADY_ASSOCIATED = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SEC6_TYPE = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC6 = 7_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%BITMAP_SECTION_), ERRFLAG_SEC6_ALREADY_ASSOCIATED )

  !> Check if a configuration for the bitmap-section-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( ENCODER_CFG, BITMAP_SECTION_NAME, CFG_HAS_BITMAP_SECTION, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. CFG_HAS_BITMAP_SECTION, ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC6 )

  !> Read the subconfiguration for the bitmap-section-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( ENCODER_CFG, BITMAP_SECTION_NAME, BITMAP_SECTION_CFG, HOOKS )

  !> Read the bitmap-section-section type
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SEC6_TYPE) READ_GRIB2_SECTION6_TYPE( BITMAP_SECTION_CFG, BITMAP_SECTION_TYPE, HOOKS )

  !> Make the bitmap-section-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_SEC6) MAKE_GRIB2_SECTION6( THIS%BITMAP_SECTION_, BITMAP_SECTION_TYPE, BITMAP_SECTION_CFG, ENCODER_OPT, HOOKS )

  !> Deallocate the bitmap-section-section configuration
  PP_TRYCALL( ERRFLAG_SECTION_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( BITMAP_SECTION_CFG, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration for bitmap-section-section' )
    CASE(ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfigurations for bitmap-section-section' )
    CASE(ERRFLAG_UNABLE_TO_MAKE_SEC6)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make bitmap-section-section' )
    CASE(ERRFLAG_SECTION_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating bitmap-section-section configuration' )
    CASE (ERRFLAG_SEC6_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'bitmap-section-section already associated' )
    CASE (ERRFLAG_UNABLE_TO_READ_SEC6_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read bitmap-section-section' )
    CASE (ERRFLAG_UNABLE_TO_READ_CFG_FOR_SEC6)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration for bitmap-section-section' )
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

END FUNCTION GRIB2_ENCODER_BUILD_BITMAP_SECTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE
#endif

!>
!> @brief Allocates resources for GRIB2 Encoder using the provided parameters.
!>
!> This function allocates resources for a GRIB2 Encoder object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the allocation process.
!>
!> @section interface
!>
!> @param [in]    THIS      GRIB2 Encoder object for which resources are allocated.
!> @param [in]    MSG All the mars keywords needed to describe the field `FORTRAN_MESSAGE_T`.
!> @param [in]    PAR All information outside mars keywords needed to describe the field `PARAMETRIZATION_T`.
!> @param [in]    OPT The encoder options structure of type `ENCODER_OPTIONS_T`.
!> @param [inout] METADATA  Pointer to metadata used during allocation.
!> @param [in]    VERBOSE   Logical flag for verbose output during allocation.
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
!> @see GRIB2_ENCODER_ALLOCATE
!> @see GRIB2_ENCODER_INIT
!> @see GRIB2_ENCODER_PRESET
!> @see GRIB2_ENCODER_RUNTIME
!> @see GRIB2_ENCODER_TO_BE_ENCODED
!> @see GRIB2_ENCODER_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_ALLOCATE'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_ALLOCATE( THIS, &
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
  CLASS(GRIB2_ENCODER_T),          INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC0_ALLOC=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: EERRFLAG_CALL_SEC1_ALLOC=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: EERRFLAG_CALL_SEC2_ALLOC=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC3_ALLOC=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC4_ALLOC=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC5_ALLOC=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC6_ALLOC=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC0_ASSOCIATED=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC1_ASSOCIATED=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC2_ASSOCIATED=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC3_ASSOCIATED=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC4_ASSOCIATED=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC5_ASSOCIATED=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC6_ASSOCIATED=14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=15_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED( METADATA ), ERRFLAG_METADATA )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%INDICATOR_SECTION_), ERRFLAG_SEC0_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%IDENTIFICATION_SECTION_), ERRFLAG_SEC1_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%LOCAL_USE_SECTION_), ERRFLAG_SEC2_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%GRID_DEFINITION_SECTION_), ERRFLAG_SEC3_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%PRODUCT_DEFINITION_SECTION_), ERRFLAG_SEC4_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%DATA_REPRESENTATION_SECTION_), ERRFLAG_SEC5_ASSOCIATED )

  ! Add space in the log
  PP_LOG_INFO( ' ' )
  PP_LOG_INFO( '------------------------------------------------------------------------------------' )

  ! Allocate the sections
  PP_TRYCALL(ERRFLAG_CALL_SEC0_ALLOC)  THIS%INDICATOR_SECTION_%ALLOCATE          ( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(EERRFLAG_CALL_SEC1_ALLOC) THIS%IDENTIFICATION_SECTION_%ALLOCATE     ( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(EERRFLAG_CALL_SEC2_ALLOC) THIS%LOCAL_USE_SECTION_%ALLOCATE          ( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC3_ALLOC)  THIS%GRID_DEFINITION_SECTION_%ALLOCATE    ( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC4_ALLOC)  THIS%PRODUCT_DEFINITION_SECTION_%ALLOCATE ( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC5_ALLOC)  THIS%DATA_REPRESENTATION_SECTION_%ALLOCATE( MSG, PAR, OPT, METADATA, HOOKS )

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
    CASE(ERRFLAG_CALL_SEC0_ALLOC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call indicator section allocation' )
    CASE(EERRFLAG_CALL_SEC1_ALLOC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call identification section allocation' )
    CASE(EERRFLAG_CALL_SEC2_ALLOC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call local use section allocation' )
    CASE(ERRFLAG_CALL_SEC3_ALLOC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call grid definition section allocation' )
    CASE(ERRFLAG_CALL_SEC4_ALLOC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call product definition section allocation' )
    CASE(ERRFLAG_CALL_SEC5_ALLOC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call data representation section allocation' )
    CASE(ERRFLAG_CALL_SEC6_ALLOC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call bitmap section allocation' )
    CASE(ERRFLAG_SEC0_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'indicator section not associated' )
    CASE(ERRFLAG_SEC1_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'identification section not associated' )
    CASE(ERRFLAG_SEC2_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'local use section not associated' )
    CASE(ERRFLAG_SEC3_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grid definition section not associated' )
    CASE(ERRFLAG_SEC4_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'product definition section not associated' )
    CASE(ERRFLAG_SEC5_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data representation section not associated' )
    CASE(ERRFLAG_SEC6_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'bitmap section not associated' )
    CASE(ERRFLAG_METADATA)
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

END FUNCTION GRIB2_ENCODER_ALLOCATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Presets GRIB2 Encoder using the provided parameters and message data.
!>
!> This function presets a GRIB2 Encoder object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the preset operation.
!>
!> @section interface
!>
!> @param [in]    THIS      GRIB2 Encoder object to be preset.
!> @param [in]    PARAMS    Model parameters used during the preset process.
!> @param [in]    MSG       Message structure providing necessary information.
!> @param [inout] METADATA  Pointer to metadata involved in the preset process.
!> @param [in]    VERBOSE   Logical flag for verbose output during the preset operation.
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
!> @see GRIB2_ENCODER_PRESET
!> @see GRIB2_ENCODER_ALLOCATE
!> @see GRIB2_ENCODER_INIT
!> @see GRIB2_ENCODER_RUNTIME
!> @see GRIB2_ENCODER_TO_BE_ENCODED
!> @see GRIB2_ENCODER_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_PRESET'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_PRESET( THIS, &
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
  CLASS(GRIB2_ENCODER_T),          INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC0_PRESET=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: EERRFLAG_CALL_SEC1_PRESET=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: EERRFLAG_CALL_SEC2_PRESET=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC3_PRESET=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC4_PRESET=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC5_PRESET=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC6_PRESET=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC0_ASSOCIATED=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC1_ASSOCIATED=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC2_ASSOCIATED=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC3_ASSOCIATED=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC4_ASSOCIATED=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC5_ASSOCIATED=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC6_ASSOCIATED=14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=15_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED( METADATA ), ERRFLAG_METADATA )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%INDICATOR_SECTION_), ERRFLAG_SEC0_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%IDENTIFICATION_SECTION_), ERRFLAG_SEC1_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%LOCAL_USE_SECTION_), ERRFLAG_SEC2_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%GRID_DEFINITION_SECTION_), ERRFLAG_SEC3_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%PRODUCT_DEFINITION_SECTION_), ERRFLAG_SEC4_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%DATA_REPRESENTATION_SECTION_), ERRFLAG_SEC5_ASSOCIATED )


  ! Add space in the log
  PP_LOG_INFO( ' ' )
  PP_LOG_INFO( '------------------------------------------------------------------------------------' )

  ! Allocate the sections
  PP_TRYCALL(ERRFLAG_CALL_SEC0_PRESET) THIS%INDICATOR_SECTION_%PRESET          ( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(EERRFLAG_CALL_SEC1_PRESET) THIS%IDENTIFICATION_SECTION_%PRESET    ( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(EERRFLAG_CALL_SEC2_PRESET) THIS%LOCAL_USE_SECTION_%PRESET         ( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC3_PRESET) THIS%GRID_DEFINITION_SECTION_%PRESET    ( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC4_PRESET) THIS%PRODUCT_DEFINITION_SECTION_%PRESET ( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC5_PRESET) THIS%DATA_REPRESENTATION_SECTION_%PRESET( MSG, PAR, OPT, METADATA, HOOKS )

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
    CASE(ERRFLAG_CALL_SEC0_PRESET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call indicator section preset' )
    CASE(EERRFLAG_CALL_SEC1_PRESET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call identification section preset' )
    CASE(EERRFLAG_CALL_SEC2_PRESET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call local use section preset' )
    CASE(ERRFLAG_CALL_SEC3_PRESET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call grid definition section preset' )
    CASE(ERRFLAG_CALL_SEC4_PRESET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call product definition section preset' )
    CASE(ERRFLAG_CALL_SEC5_PRESET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call data representation section preset' )
    CASE(ERRFLAG_CALL_SEC6_PRESET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call bitmap section preset' )
    CASE(ERRFLAG_SEC0_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'indicator section not associated' )
    CASE(ERRFLAG_SEC1_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'identification section not associated' )
    CASE(ERRFLAG_SEC2_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'local use section not associated' )
    CASE(ERRFLAG_SEC3_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grid definition section not associated' )
    CASE(ERRFLAG_SEC4_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'product definition section not associated' )
    CASE(ERRFLAG_SEC5_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data representation section not associated' )
    CASE(ERRFLAG_SEC6_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'bitmap section not associated' )
    CASE(ERRFLAG_METADATA)
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

END FUNCTION GRIB2_ENCODER_PRESET
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Executes runtime processing for GRIB2 Encoder using provided parameters, message data, and time history.
!>
!> This function performs runtime operations for a GRIB2 Encoder object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), current time (`CURR_TIME`), time history (`TIME_HISTORY`), and metadata (`METADATA`).
!> The process can be run in verbose mode if specified. The function is thread-safe and returns an error code indicating
!> the success or failure of the runtime operation.
!>
!> @section interface
!>
!> @param [in]    THIS          GRIB2 Encoder object for runtime execution.
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
!> @see GRIB2_ENCODER_RUNTIME
!> @see GRIB2_ENCODER_ALLOCATE
!> @see GRIB2_ENCODER_INIT
!> @see GRIB2_ENCODER_PRESET
!> @see GRIB2_ENCODER_TO_BE_ENCODED
!> @see GRIB2_ENCODER_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_RUNTIME'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_RUNTIME( THIS, &
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
  CLASS(GRIB2_ENCODER_T),          INTENT(INOUT) :: THIS
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
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC0_RUNTIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: EERRFLAG_CALL_SEC1_RUNTIME=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: EERRFLAG_CALL_SEC2_RUNTIME=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC3_RUNTIME=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC4_RUNTIME=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC5_RUNTIME=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC6_RUNTIME=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC0_ASSOCIATED=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC1_ASSOCIATED=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC2_ASSOCIATED=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC3_ASSOCIATED=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC4_ASSOCIATED=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC5_ASSOCIATED=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC6_ASSOCIATED=14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=15_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED( METADATA ), ERRFLAG_METADATA )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%INDICATOR_SECTION_), ERRFLAG_SEC0_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%IDENTIFICATION_SECTION_), ERRFLAG_SEC1_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%LOCAL_USE_SECTION_), ERRFLAG_SEC2_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%GRID_DEFINITION_SECTION_), ERRFLAG_SEC3_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%PRODUCT_DEFINITION_SECTION_), ERRFLAG_SEC4_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%DATA_REPRESENTATION_SECTION_), ERRFLAG_SEC5_ASSOCIATED )
  ! PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%BITMAP_SECTION_), ERRFLAG_SEC6_ASSOCIATED )

  ! Add space in the log
  PP_LOG_INFO( ' ' )
  PP_LOG_INFO( '------------------------------------------------------------------------------------' )

  ! Allocate the sections
  PP_TRYCALL(ERRFLAG_CALL_SEC0_RUNTIME) THIS%INDICATOR_SECTION_%RUNTIME          ( MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )
  PP_TRYCALL(EERRFLAG_CALL_SEC1_RUNTIME) THIS%IDENTIFICATION_SECTION_%RUNTIME    ( MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )
  PP_TRYCALL(EERRFLAG_CALL_SEC2_RUNTIME) THIS%LOCAL_USE_SECTION_%RUNTIME         ( MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC3_RUNTIME) THIS%GRID_DEFINITION_SECTION_%RUNTIME    ( MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC4_RUNTIME) THIS%PRODUCT_DEFINITION_SECTION_%RUNTIME ( MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC5_RUNTIME) THIS%DATA_REPRESENTATION_SECTION_%RUNTIME( MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )
  ! PP_TRYCALL(ERRFLAG_CALL_SEC6_RUNTIME) THIS%BITMAP_SECTION_%RUNTIME             ( MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )

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
    CASE(ERRFLAG_CALL_SEC0_RUNTIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call indicator section runtime' )
    CASE(EERRFLAG_CALL_SEC1_RUNTIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call identification section runtime' )
    CASE(EERRFLAG_CALL_SEC2_RUNTIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call local use section runtime' )
    CASE(ERRFLAG_CALL_SEC3_RUNTIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call grid definition section runtime' )
    CASE(ERRFLAG_CALL_SEC4_RUNTIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call product definition section runtime' )
    CASE(ERRFLAG_CALL_SEC5_RUNTIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call data representation section runtime' )
    CASE(ERRFLAG_CALL_SEC6_RUNTIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call bitmap section runtime' )
    CASE(ERRFLAG_SEC0_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'indicator section not associated' )
    CASE(ERRFLAG_SEC1_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'identification section not associated' )
    CASE(ERRFLAG_SEC2_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'local use section not associated' )
    CASE(ERRFLAG_SEC3_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grid definition section not associated' )
    CASE(ERRFLAG_SEC4_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'product definition section not associated' )
    CASE(ERRFLAG_SEC5_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data representation section not associated' )
    CASE(ERRFLAG_SEC6_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'bitmap section not associated' )
    CASE(ERRFLAG_METADATA)
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

END FUNCTION GRIB2_ENCODER_RUNTIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Prepares GRIB2 Encoder for encoding based on provided parameters, message data, and time history.
!>
!> This function determines whether GRIB2 Encoder (`THIS`) is ready to be encoded. It processes the provided model parameters
!> (`PARAMS`), message structure (`MSG`), current time (`CURR_TIME`), time history (`TIME_HISTORY`), and updates the
!> `TO_BE_ENCODED` flag accordingly. The function is thread-safe and returns an error code indicating the success or failure
!> of the operation. The process can also be run in verbose mode if specified.
!>
!> @section interface
!>
!> @param [inout] THIS          GRIB2 Encoder object to be checked for encoding readiness.
!> @param [in]    PARAMS        Model parameters used during the encoding preparation.
!> @param [in]    MSG           Message structure providing necessary information.
!> @param [in]    CURR_TIME     Current time used in the encoding process.
!> @param [in]    TIME_HISTORY  Time history information for the encoding process.
!> @param [inout] TO_BE_ENCODED Logical flag indicating whether the section is ready to be encoded.
!> @param [in]    VERBOSE       Logical flag for verbose output during the operation.
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
!> @see GRIB2_ENCODER_TO_BE_ENCODED
!> @see GRIB2_ENCODER_INIT
!> @see GRIB2_ENCODER_ALLOCATE
!> @see GRIB2_ENCODER_PRESET
!> @see GRIB2_ENCODER_RUNTIME
!> @see GRIB2_ENCODER_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_TO_BE_ENCODED'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_TO_BE_ENCODED( THIS, &
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
  CLASS(GRIB2_ENCODER_T),          INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  LOGICAL,                         INTENT(OUT)   :: TO_BE_ENCODED
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL, DIMENSION(6) :: LOC_TO_BE_ENCODED

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC0_TO_BE_ENCODED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: EERRFLAG_CALL_SEC1_TO_BE_ENCODED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: EERRFLAG_CALL_SEC2_TO_BE_ENCODED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC3_TO_BE_ENCODED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC4_TO_BE_ENCODED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC5_TO_BE_ENCODED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC6_TO_BE_ENCODED=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC0_ASSOCIATED=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC1_ASSOCIATED=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC2_ASSOCIATED=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC3_ASSOCIATED=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC4_ASSOCIATED=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC5_ASSOCIATED=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC6_ASSOCIATED=14_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%INDICATOR_SECTION_), ERRFLAG_SEC0_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%IDENTIFICATION_SECTION_), ERRFLAG_SEC1_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%LOCAL_USE_SECTION_), ERRFLAG_SEC2_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%GRID_DEFINITION_SECTION_), ERRFLAG_SEC3_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%PRODUCT_DEFINITION_SECTION_), ERRFLAG_SEC4_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%DATA_REPRESENTATION_SECTION_), ERRFLAG_SEC5_ASSOCIATED )
  ! PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%BITMAP_SECTION_), ERRFLAG_SEC6_ASSOCIATED )

  ! Allocate the sections
  PP_TRYCALL(ERRFLAG_CALL_SEC0_TO_BE_ENCODED) THIS%INDICATOR_SECTION_%TO_BE_ENCODED          ( MSG, PAR, TIME_HIST, CURR_TIME, OPT, LOC_TO_BE_ENCODED(1), HOOKS )
  PP_TRYCALL(EERRFLAG_CALL_SEC1_TO_BE_ENCODED) THIS%IDENTIFICATION_SECTION_%TO_BE_ENCODED    ( MSG, PAR, TIME_HIST, CURR_TIME, OPT, LOC_TO_BE_ENCODED(2), HOOKS )
  PP_TRYCALL(EERRFLAG_CALL_SEC2_TO_BE_ENCODED) THIS%LOCAL_USE_SECTION_%TO_BE_ENCODED         ( MSG, PAR, TIME_HIST, CURR_TIME, OPT, LOC_TO_BE_ENCODED(3), HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC3_TO_BE_ENCODED) THIS%GRID_DEFINITION_SECTION_%TO_BE_ENCODED    ( MSG, PAR, TIME_HIST, CURR_TIME, OPT, LOC_TO_BE_ENCODED(4), HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC4_TO_BE_ENCODED) THIS%PRODUCT_DEFINITION_SECTION_%TO_BE_ENCODED ( MSG, PAR, TIME_HIST, CURR_TIME, OPT, LOC_TO_BE_ENCODED(5), HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC5_TO_BE_ENCODED) THIS%DATA_REPRESENTATION_SECTION_%TO_BE_ENCODED( MSG, PAR, TIME_HIST, CURR_TIME, OPT, LOC_TO_BE_ENCODED(6), HOOKS )
  ! PP_TRYCALL(ERRFLAG_CALL_SEC6_TO_BE_ENCODED) THIS%BITMAP_SECTION_%TO_BE_ENCODED             ( MSG, PAR, TIME_HIST, CURR_TIME, OPT, LOC_TO_BE_ENCODED(7), HOOKS )

  ! Compute the output flag
  WRITE(*,*) 'MAIN::LOC_TO_BE_ENCODED = ', LOC_TO_BE_ENCODED
  TO_BE_ENCODED = ALL( LOC_TO_BE_ENCODED )

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
    CASE(ERRFLAG_CALL_SEC0_TO_BE_ENCODED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call indicator section to be encoded' )
    CASE(EERRFLAG_CALL_SEC1_TO_BE_ENCODED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call identification section to be encoded' )
    CASE(EERRFLAG_CALL_SEC2_TO_BE_ENCODED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call local use section to be encoded' )
    CASE(ERRFLAG_CALL_SEC3_TO_BE_ENCODED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call grid definition section to be encoded' )
    CASE(ERRFLAG_CALL_SEC4_TO_BE_ENCODED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call product definition section to be encoded' )
    CASE(ERRFLAG_CALL_SEC5_TO_BE_ENCODED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call data representation section to be encoded' )
    CASE(ERRFLAG_CALL_SEC6_TO_BE_ENCODED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call bitmap section to be encoded' )
    CASE(ERRFLAG_SEC0_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'indicator section not associated' )
    CASE(ERRFLAG_SEC1_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'identification section not associated' )
    CASE(ERRFLAG_SEC2_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'local use section not associated' )
    CASE(ERRFLAG_SEC3_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grid definition section not associated' )
    CASE(ERRFLAG_SEC4_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'product definition section not associated' )
    CASE(ERRFLAG_SEC5_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data representation section not associated' )
    CASE(ERRFLAG_SEC6_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'bitmap section not associated' )
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

END FUNCTION GRIB2_ENCODER_TO_BE_ENCODED
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees resources associated with GRIB2 Encoder object.
!>
!> This function deallocates and cleans up resources associated with the GRIB2 Encoder object (`THIS`).
!> The process can be run in verbose mode for additional output. The function is thread-safe and returns an
!> error code indicating the success or failure of the operation.
!>
!> @section interface
!>
!> @param [inout] THIS    GRIB2 Encoder object to be deallocated and freed.
!> @param [in]    VERBOSE Logical flag for verbose output during resource cleanup.
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
!> @see GRIB2_ENCODER_INIT
!> @see GRIB2_ENCODER_ALLOCATE
!> @see GRIB2_ENCODER_PRESET
!> @see GRIB2_ENCODER_RUNTIME
!> @see GRIB2_ENCODER_TO_BE_ENCODED
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_FREE'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_FREE( THIS, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD,   ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: GRIB2_SECTION0_FACTORY_MOD, ONLY: DESTROY_GRIB2_SECTION0
  USE :: GRIB2_SECTION1_FACTORY_MOD, ONLY: DESTROY_GRIB2_SECTION1
  USE :: GRIB2_SECTION2_FACTORY_MOD, ONLY: DESTROY_GRIB2_SECTION2
  USE :: GRIB2_SECTION3_FACTORY_MOD, ONLY: DESTROY_GRIB2_SECTION3
  USE :: GRIB2_SECTION4_FACTORY_MOD, ONLY: DESTROY_GRIB2_SECTION4
  USE :: GRIB2_SECTION5_FACTORY_MOD, ONLY: DESTROY_GRIB2_SECTION5
#if 0
  USE :: GRIB2_SECTION6_FACTORY_MOD, ONLY: DESTROY_GRIB2_SECTION6
#endif

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_ENCODER_T),       INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC0_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC1_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC2_ASSOCIATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC3_ASSOCIATED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC4_ASSOCIATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC5_ASSOCIATED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC6_ASSOCIATED=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC0_DESTRUCTOR=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: EERRFLAG_CALL_SEC1_DESTRUCTOR=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: EERRFLAG_CALL_SEC2_DESTRUCTOR=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC3_DESTRUCTOR=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC4_DESTRUCTOR=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC5_DESTRUCTOR=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC6_DESTRUCTOR=14_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%INDICATOR_SECTION_), ERRFLAG_SEC0_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%IDENTIFICATION_SECTION_), ERRFLAG_SEC1_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%LOCAL_USE_SECTION_), ERRFLAG_SEC2_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%GRID_DEFINITION_SECTION_), ERRFLAG_SEC3_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%PRODUCT_DEFINITION_SECTION_), ERRFLAG_SEC4_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%DATA_REPRESENTATION_SECTION_), ERRFLAG_SEC5_ASSOCIATED )
#if 0
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%BITMAP_SECTION_), ERRFLAG_SEC6_ASSOCIATED )
#endif

  ! Allocate the sections
  PP_TRYCALL(ERRFLAG_CALL_SEC0_DESTRUCTOR)  DESTROY_GRIB2_SECTION0( THIS%INDICATOR_SECTION_, OPT, HOOKS )
  PP_TRYCALL(EERRFLAG_CALL_SEC1_DESTRUCTOR) DESTROY_GRIB2_SECTION1( THIS%IDENTIFICATION_SECTION_, OPT, HOOKS )
  PP_TRYCALL(EERRFLAG_CALL_SEC2_DESTRUCTOR) DESTROY_GRIB2_SECTION2( THIS%LOCAL_USE_SECTION_, OPT, HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC3_DESTRUCTOR)  DESTROY_GRIB2_SECTION3( THIS%GRID_DEFINITION_SECTION_, OPT, HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC4_DESTRUCTOR)  DESTROY_GRIB2_SECTION4( THIS%PRODUCT_DEFINITION_SECTION_, OPT, HOOKS )
  PP_TRYCALL(ERRFLAG_CALL_SEC5_DESTRUCTOR)  DESTROY_GRIB2_SECTION5( THIS%DATA_REPRESENTATION_SECTION_, OPT, HOOKS )
#if 0
  PP_TRYCALL(ERRFLAG_CALL_SEC6_DESTRUCTOR)  DESTROY_GRIB2_SECTION6( THIS%BITMAP_SECTION_, OPT, HOOKS )
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
    CASE(ERRFLAG_SEC0_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'indicator section not associated' )
    CASE(ERRFLAG_SEC1_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'identification section not associated' )
    CASE(ERRFLAG_SEC2_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'local use section not associated' )
    CASE(ERRFLAG_SEC3_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grid definition section not associated' )
    CASE(ERRFLAG_SEC4_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'product definition section not associated' )
    CASE(ERRFLAG_SEC5_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data representation section not associated' )
    CASE(ERRFLAG_SEC6_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'bitmap section not associated' )
    CASE(ERRFLAG_CALL_SEC0_DESTRUCTOR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call indicator section destructor' )
    CASE(EERRFLAG_CALL_SEC1_DESTRUCTOR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call identification section destructor' )
    CASE(EERRFLAG_CALL_SEC2_DESTRUCTOR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call local use section destructor' )
    CASE(ERRFLAG_CALL_SEC3_DESTRUCTOR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call grid definition section destructor' )
    CASE(ERRFLAG_CALL_SEC4_DESTRUCTOR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call product definition section destructor' )
    CASE(ERRFLAG_CALL_SEC5_DESTRUCTOR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call data representation section destructor' )
    CASE(ERRFLAG_CALL_SEC6_DESTRUCTOR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call bitmap section destructor' )
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

END FUNCTION GRIB2_ENCODER_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Print informations related to the grib section
!>
!> @section interface
!>   @param [inout] THIS An object of type `GRIB_SECTION_BASE_A` representing the GRIB section to be freed.
!>   @param [in]    UNIT The unit to which the information will be printed.
!>   @param [in]    OFFSET The offset to be used for indentation.
!>   @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
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
#define PP_PROCEDURE_NAME 'GRIB2_ENCODER_PRINT'
PP_THREAD_SAFE FUNCTION GRIB2_ENCODER_PRINT( THIS, &
&  UNIT, OFFSET, OPT, HOOKS, SEPARATOR ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB2_ENCODER_T),       INTENT(INOUT) :: THIS
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
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC0_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC1_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC2_ASSOCIATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC3_ASSOCIATED=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC4_ASSOCIATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC5_ASSOCIATED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SEC6_ASSOCIATED=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_BASE_PRINT=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC0_PRINT=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: EERRFLAG_CALL_SEC1_PRINT=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: EERRFLAG_CALL_SEC2_PRINT=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC3_PRINT=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC4_PRINT=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC5_PRINT=14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_SEC6_PRINT=15_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_ERROR=16_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%INDICATOR_SECTION_), ERRFLAG_SEC0_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%IDENTIFICATION_SECTION_), ERRFLAG_SEC1_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%LOCAL_USE_SECTION_), ERRFLAG_SEC2_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%GRID_DEFINITION_SECTION_), ERRFLAG_SEC3_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%PRODUCT_DEFINITION_SECTION_), ERRFLAG_SEC4_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%DATA_REPRESENTATION_SECTION_), ERRFLAG_SEC5_ASSOCIATED )
  ! PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%BITMAP_SECTION_), ERRFLAG_SEC6_ASSOCIATED )

  ! Write the section information
  IF ( OFFSET .LE. 0 ) THEN
    WRITE(UNIT,'(A,A,A,A,A,A,A)', IOSTAT=WRITE_STAT) 'GRIB_SECTION: ', TRIM(ADJUSTL(THIS%TYPE_)), '::', TRIM(ADJUSTL(THIS%SUBTYPE_)), '(', TRIM(ADJUSTL(THIS%KIND_)), '){'
    PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
  ELSE
    WRITE(UNIT,'(A,A,A,A,A,A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET), 'GRIB_SECTION: ', TRIM(ADJUSTL(THIS%TYPE_)), '::', TRIM(ADJUSTL(THIS%SUBTYPE_)), '(', TRIM(ADJUSTL(THIS%KIND_)), '){'
    PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
  ENDIF

  !> Call nested print sections
  PP_TRYCALL(ERRFLAG_CALL_SEC0_PRINT) THIS%INDICATOR_SECTION_%PRINT          ( UNIT, OFFSET+2, OPT, HOOKS, ', ...' )
  PP_TRYCALL(EERRFLAG_CALL_SEC1_PRINT) THIS%IDENTIFICATION_SECTION_%PRINT    ( UNIT, OFFSET+2, OPT, HOOKS, ', ...' )
  PP_TRYCALL(EERRFLAG_CALL_SEC2_PRINT) THIS%LOCAL_USE_SECTION_%PRINT         ( UNIT, OFFSET+2, OPT, HOOKS, ', ...' )
  PP_TRYCALL(ERRFLAG_CALL_SEC3_PRINT) THIS%GRID_DEFINITION_SECTION_%PRINT    ( UNIT, OFFSET+2, OPT, HOOKS, ', ...' )
  PP_TRYCALL(ERRFLAG_CALL_SEC4_PRINT) THIS%PRODUCT_DEFINITION_SECTION_%PRINT ( UNIT, OFFSET+2, OPT, HOOKS, ', ...' )
  PP_TRYCALL(ERRFLAG_CALL_SEC5_PRINT) THIS%DATA_REPRESENTATION_SECTION_%PRINT( UNIT, OFFSET+2, OPT, HOOKS )
  ! PP_TRYCALL(ERRFLAG_CALL_SEC6_PRINT) THIS%BITMAP_SECTION_%PRINT             ( UNIT, OFFSET+2, OPT, HOOKS )

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
    CASE(ERRFLAG_SEC0_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'indicator section not associated' )
    CASE(ERRFLAG_SEC1_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'identification section not associated' )
    CASE(ERRFLAG_SEC2_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'local use section not associated' )
    CASE(ERRFLAG_SEC3_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'grid definition section not associated' )
    CASE(ERRFLAG_SEC4_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'product definition section not associated' )
    CASE(ERRFLAG_SEC5_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'data representation section not associated' )
    CASE(ERRFLAG_SEC6_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'bitmap section not associated' )
    CASE (ERRFLAG_WRITE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error writing to given unit' )
    CASE(ERRFLAG_CALL_BASE_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call base print' )
    CASE(ERRFLAG_CALL_SEC0_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call indicator section print' )
    CASE(EERRFLAG_CALL_SEC1_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call identification section print' )
    CASE(EERRFLAG_CALL_SEC2_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call local use section print' )
    CASE(ERRFLAG_CALL_SEC3_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call grid definition section print' )
    CASE(ERRFLAG_CALL_SEC4_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call product definition section print' )
    CASE(ERRFLAG_CALL_SEC5_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call data representation section print' )
    CASE(ERRFLAG_CALL_SEC6_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call bitmap section print' )
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

END FUNCTION GRIB2_ENCODER_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB2_ENCODER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
