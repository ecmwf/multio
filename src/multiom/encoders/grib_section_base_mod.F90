!>
!> @brief Module containing GRIB section management interfaces.
!>
!> The `GRIB_SECTION_IF_MOD` module provides the abstract interface definitions and
!> implementation of various operations for managing GRIB sections, such as initialization,
!> allocation, presetting, runtime operations, encoding, and freeing. These interfaces
!> ensure that different GRIB sections are handled uniformly within the software.
!>
!> Each operation is thread-safe and performs its task based on the parameters passed,
!> while supporting debug, trace, and logging features via conditional dependencies.
!>
!> @section interface
!>   This module defines several thread-safe interfaces, all returning an integer error code:
!>   - `GRIB2_SECTION_INIT_IF`: Initializes a GRIB section.
!>   - `GRIB_SECTION_ALLOCATE_IF`: Allocates resources for a GRIB section.
!>   - `GRIB_SECTION_PRESET_IF`: Prepares the GRIB section before runtime.
!>   - `GRIB_SECTION_RUNTIME_IF`: Performs runtime operations on the GRIB section.
!>   - `GRIB2_SECTION_TO_BE_ENCODED_IF`: Handles the encoding status of a GRIB section.
!>   - `GRIB_SECTION_FREE_IF`: Frees resources associated with the GRIB section.
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] PARAMETRIZATION_MOD::PARAMETRIZATION_T
!>   - @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] ENCODER_OPTIONS_MOD::ENCODER_OPTIONS_T
!>   - @dependency [TYPE] TIME_UTILS_MOD::TIME_HISTORY_T
!>   - @dependency [TYPE] TIME_UTILS_MOD::CURR_TIME_T
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section external dependencies
!>   - None.
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @section intrinsic dependencies
!>   - None.
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib_section_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_SECTION_BASE_MOD'
MODULE GRIB_SECTION_BASE_MOD

IMPLICIT NONE

!> Default visibility of the module members
PRIVATE


!>
!> @brief Abstract base class for GRIB section operations.
!>
!> This abstract type `GRIB_SECTION_BASE_A` serves as the base for all GRIB section-related operations.
!> It defines the following deferred procedures that must be implemented by derived types:
!>
!> - `INIT`: Initializes the GRIB section.
!> - `ALLOCATE`: Allocates resources for the GRIB section.
!> - `PRESET`: Sets preset values for the GRIB section.
!> - `RUNTIME`: Executes runtime operations for the GRIB section.
!> - `TO_BE_ENCODED`: Marks the GRIB section for encoding.
!> - `FREE`: Frees allocated resources for the GRIB section.
!>
!> @section interface
!>   The abstract type contains the following deferred procedures:
!>
!>   @dependency [INTERFACE] GRIB_SECTION_INIT_IF "Deferred procedure for initialization."
!>   @dependency [INTERFACE] GRIB_SECTION_ALLOCATE_IF "Deferred procedure for allocation."
!>   @dependency [INTERFACE] GRIB_SECTION_PRESET_IF "Deferred procedure for presetting values."
!>   @dependency [INTERFACE] GRIB_SECTION_RUNTIME_IF "Deferred procedure for runtime operations."
!>   @dependency [INTERFACE] GRIB_SECTION_TO_BE_ENCODED_IF "Deferred procedure for encoding the section."
!>   @dependency [INTERFACE] GRIB_SECTION_FREE_IF "Deferred procedure for freeing resources."
!>
!> @section local dependencies
!>   None.
!>
!> @section external dependencies
!>   None.
!>
!> @section intrinsic dependencies
!>   None.
!>
!> @section Error codes
!>   This abstract type does not handle errors directly, but errors may be generated in the implementations of the deferred methods.
!>
TYPE, ABSTRACT :: GRIB_SECTION_BASE_A

  !< Name of the GRIB section
  CHARACTER(LEN=32) :: TYPE_ = REPEAT(' ', 32)

  !< Name of the GRIB section
  CHARACTER(LEN=32) :: SUBTYPE_ = REPEAT(' ', 32)

  !< Name of the GRIB section
  CHARACTER(LEN=32) :: KIND_ = REPEAT(' ', 32)


CONTAINS

  !> Deferred procedures for GRIB section operations

  !>
  !> @brief Initializes the section
  PROCEDURE(GRIB_SECTION_INIT_CFG_IF), DEFERRED, PASS, PUBLIC :: INIT_CFG

  !>
  !> @brief Initializes the section
  PROCEDURE(GRIB_SECTION_INIT_LAZY_IF), DEFERRED, PASS, PUBLIC :: INIT_LAZY

  !>
  !> @brief Initializes the section
  GENERIC :: INIT => INIT_CFG
  GENERIC :: INIT => INIT_LAZY

  !>
  !> @brief Allocates resources for the section
  PROCEDURE(GRIB_SECTION_ALLOCATE_IF), DEFERRED, PASS, PUBLIC :: ALLOCATE

  !>
  !> @brief Presets the parameters of the section
  PROCEDURE(GRIB_SECTION_PRESET_IF), DEFERRED, PASS, PUBLIC :: PRESET

  !>
  !> @brief Manages the runtime execution of section
  PROCEDURE(GRIB_SECTION_RUNTIME_IF), DEFERRED, PASS, PUBLIC :: RUNTIME

  !>
  !> @brief Determines if the grib message needs to be encoded.
  PROCEDURE(GRIB_SECTION_TO_BE_ENCODED_IF), DEFERRED, PASS, PUBLIC :: TO_BE_ENCODED

  !>
  !> @brief Frees resources allocated for the section
  PROCEDURE(GRIB_SECTION_FREE_IF), DEFERRED, PASS, PUBLIC :: FREE


  !> Concrete procedures for GRIB section operations

  !>
  !> @brief Return the type of the section
  PROCEDURE, PUBLIC, PASS :: TYPE => GRIB_SECTION_TYPE

  !>
  !> @brief REturn the subtype of the section
  PROCEDURE, PUBLIC, PASS :: SUBTYPE => GRIB_SECTION_SUBTYPE

  !>
  !> @brief REturn the subtype of the section
  PROCEDURE, PUBLIC, PASS :: KIND => GRIB_SECTION_KIND

  !>
  !> @brief Print the section type/subtype/kind on the given unit
  PROCEDURE, PUBLIC, PASS :: PRINT => GRIB2_SECTION_PRINT

END TYPE


ABSTRACT INTERFACE

!>
!> @brief Initializes a GRIB section with the given parameters and configuration.
!>
!> The `GRIB_SECTION_INIT_CFG_IF` function initializes a GRIB section represented by the `THIS` object.
!> It takes a parametrization, encoder options, YAML configuration, and hooks to complete the initialization.
!> The function returns an integer indicating the success or failure of the initialization process.
!>
!> This function is thread-safe.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB_SECTION_BASE_A` representing the GRIB section being initialized.
!>   @param [in]    CFG   The YAML configuration object of type `YAML_CONFIGURATION_T`.
!>   @param [in]    OPT   The encoder options structure of type `GRIB_ENCODER_OPTIONS_T`.
!>   @param [inout] HOOKS A structure of type `HOOKS_T` that contains hooks for initialization.
!>   @return Integer error code (`RET`) indicating success (`0`) or failure (`1`).
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] GRIB_ENCODER_OPTIONS_MOD::GRIB_ENCODER_OPTIONS_T
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>
!> @section intrinsic dependencies
!>   None.
!>
!> @section Error codes
!>   - `0`: Success.
!>   - `1`: Failure in initializing the GRIB section.
!>
PP_THREAD_SAFE FUNCTION GRIB_SECTION_INIT_CFG_IF( THIS, CFG, &
&               OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A),   INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION GRIB_SECTION_INIT_CFG_IF


!>
!> @brief Initializes a GRIB section with the given parameters and configuration.
!>
!> The `GRIB_SECTION_INIT_LAZY_IF` function initializes a GRIB section represented by the `THIS` object.
!> It takes a parametrization, encoder options, YAML configuration, and hooks to complete the initialization.
!> The function returns an integer indicating the success or failure of the initialization process.
!>
!> This function is thread-safe.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB_SECTION_BASE_A` representing the GRIB section being initialized.
!>   @param [in]    MSG   All the mars keywords needed to describe the field `FORTRAN_MESSAGE_T`.
!>   @param [in]    PAR   All information outside mars keywords needed to describe the field `PARAMETRIZATION_T`.
!>   @param [in]    OPT   The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] HOOKS A structure of type `HOOKS_T` that contains hooks for initialization.
!>   @return Integer error code (`RET`) indicating success (`0`) or failure (`1`).
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] PARAMETRIZATION_MOD::PARAMETRIZATION_T
!>   - @dependency [TYPE] ENCODER_OPTIONS_MOD::ENCODER_OPTIONS_T
!>   - @dependency [TYPE] GRIB_ENCODER_OPTIONS_MOD::GRIB_ENCODER_OPTIONS_T
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>
!> @section intrinsic dependencies
!>   None.
!>
!> @section Error codes
!>   - `0`: Success.
!>   - `1`: Failure in initializing the GRIB section.
!>
PP_THREAD_SAFE FUNCTION GRIB_SECTION_INIT_LAZY_IF( THIS, &
&               MSG, PAR, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A),   INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION GRIB_SECTION_INIT_LAZY_IF



!>
!> @brief Allocates resources for a GRIB section based on the given parametrization and metadata.
!>
!> The `GRIB_SECTION_ALLOCATE_IF` function allocates memory and resources for the GRIB section represented by the `THIS` object.
!> It utilizes a parametrization, message, metadata, and hooks to perform the allocation.
!> This function is thread-safe and returns an integer indicating the success or failure of the allocation process.
!>
!> @section interface
!>   @param [in]    THIS     An object of type `GRIB_SECTION_BASE_A` representing the GRIB section to allocate resources for.
!>   @param [in]    MSG      All the mars keywords needed to describe the field `FORTRAN_MESSAGE_T`.
!>   @param [in]    PAR      All information outside mars keywords needed to describe the field `PARAMETRIZATION_T`.
!>   @param [in]    OPT      The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] METADATA A pointer to the metadata object of type `METADATA_BASE_A` used during allocation.
!>   @param [inout] HOOKS    A structure of type `HOOKS_T` that contains hooks for the allocation process.
!>   @return Integer error code (`RET`) indicating success (`0`) or failure (`1`).
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] PARAMETRIZATION_MOD::PARAMETRIZATION_T
!>   - @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!>   - @dependency [TYPE] GRIB_ENCODER_OPTIONS_MOD::GRIB_ENCODER_OPTIONS_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>
!> @section intrinsic dependencies
!>   None.
!>
!> @section Error codes
!>   - `0`: Success.
!>   - `1`: Failure during resource allocation.
!>
PP_THREAD_SAFE FUNCTION GRIB_SECTION_ALLOCATE_IF( THIS, &
&  MSG, PAR, OPT,  METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A),      INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION GRIB_SECTION_ALLOCATE_IF


!>
!> @brief Presets the GRIB section with the given parameters and metadata before runtime execution.
!>
!> The `GRIB_SECTION_PRESET_IF` function applies preset values and configurations to the GRIB section represented by the `THIS` object.
!> It uses a parametrization, message, metadata, and hooks to prepare the section for subsequent runtime operations.
!> This function is thread-safe and returns an integer indicating the success or failure of the preset operation.
!>
!> @section interface
!>   @param [in]    THIS     An object of type `GRIB_SECTION_BASE_A` representing the GRIB section to be preset.
!>   @param [in]    MSG      The message object of type `FORTRAN_MESSAGE_T` used to handle preset-related messaging.
!>   @param [in]    PAR      The parametrization structure of type `PARAMETRIZATION_T` used for the preset operation.
!>   @param [in]    OPT      The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] METADATA A pointer to the metadata object of type `METADATA_BASE_A` used for presetting the section.
!>   @param [inout] HOOKS    A structure of type `HOOKS_T` that contains hooks for the preset operation.
!>   @return Integer error code (`RET`) indicating success (`0`) or failure (`1`).
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] PARAMETRIZATION_MOD::PARAMETRIZATION_T
!>   - @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!>   - @dependency [TYPE] GRIB_ENCODER_OPTIONS_MOD::GRIB_ENCODER_OPTIONS_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>
!> @section intrinsic dependencies
!>   None.
!>
!> @section Error codes
!>   - `0`: Success.
!>   - `1`: Failure during preset operation.
!>
PP_THREAD_SAFE FUNCTION GRIB_SECTION_PRESET_IF( THIS, &
&  MSG, PAR, OPT,  METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A),      INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION GRIB_SECTION_PRESET_IF


!>
!> @brief Executes runtime operations for a GRIB section based on the provided parameters and time information.
!>
!> The `GRIB_SECTION_RUNTIME_IF` function carries out the runtime phase for the GRIB section represented by the `THIS` object.
!> It uses a parametrization, message, current time, time history, metadata, and hooks to perform the runtime operations.
!> This function is thread-safe and returns an integer indicating the success or failure of the runtime execution.
!>
!> @section interface
!>   @param [in]    THIS      An object of type `GRIB_SECTION_BASE_A` representing the GRIB section for runtime execution.
!>   @param [in]    MSG       The message object of type `FORTRAN_MESSAGE_T` used to handle preset-related messaging.
!>   @param [in]    PAR       The parametrization structure of type `PARAMETRIZATION_T` used for the preset operation.
!>   @param [in]    TIME_HIST The time history object of type `TIME_HISTORY_T` providing historical time data.
!>   @param [in]    CURR_TIME The current time object of type `CURR_TIME_T` for the runtime phase.
!>   @param [in]    OPT       The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] METADATA  A pointer to the metadata object of type `METADATA_BASE_A` used during runtime.
!>   @param [inout] HOOKS     A structure of type `HOOKS_T` that contains hooks for runtime operations.
!>   @return Integer error code (`RET`) indicating success (`0`) or failure (`1`).
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] PARAMETRIZATION_MOD::PARAMETRIZATION_T
!>   - @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!>   - @dependency [TYPE] TIME_UTILS_MOD::TIME_HISTORY_T
!>   - @dependency [TYPE] TIME_UTILS_MOD::CURR_TIME_T
!>   - @dependency [TYPE] GRIB_ENCODER_OPTIONS_MOD::GRIB_ENCODER_OPTIONS_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>
!> @section intrinsic dependencies
!>   None.
!>
!> @section Error codes
!>   - `0`: Success.
!>   - `1`: Failure during runtime execution.
!>
PP_THREAD_SAFE FUNCTION GRIB_SECTION_RUNTIME_IF( THIS, &
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

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A),      INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION GRIB_SECTION_RUNTIME_IF


!>
!> @brief Determines if a GRIB section should be encoded based on the current parameters and time.
!>
!> The `GRIB_SECTION_TO_BE_ENCODED_IF` function assesses whether the GRIB section represented by `THIS`
!> is ready to be encoded. This decision is made based on the provided parametrization, current time,
!> time history, and hooks. The result is stored in the logical `TO_BE_ENCODED` flag, which indicates
!> if encoding should proceed. This function is thread-safe.
!>
!> @section interface
!>   @param [inout] THIS          An object of type `GRIB_SECTION_BASE_A` representing the GRIB section being checked.
!>   @param [in]    MSG           The message object of type `FORTRAN_MESSAGE_T` used to handle preset-related messaging.
!>   @param [in]    PAR           The parametrization structure of type `PARAMETRIZATION_T` used for the preset operation.
!>   @param [in]    TIME_HIST     The time history object of type `TIME_HISTORY_T` providing historical time data.
!>   @param [in]    CURR_TIME     The current time object of type `CURR_TIME_T` for time-based encoding decisions.
!>   @param [in]    OPT           The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [out]   TO_BE_ENCODED Logical flag indicating if the GRIB section should be encoded.
!>   @param [inout] HOOKS         A structure of type `HOOKS_T` that contains hooks for managing encoding-related operations.
!>   @return Integer error code (`RET`) indicating success (`0`) or failure (`1`).
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] PARAMETRIZATION_MOD::PARAMETRIZATION_T
!>   - @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!>   - @dependency [TYPE] TIME_UTILS_MOD::TIME_HISTORY_T
!>   - @dependency [TYPE] TIME_UTILS_MOD::CURR_TIME_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>
!> @section intrinsic dependencies
!>   None.
!>
!> @section Error codes
!>   - `0`: Success, encoding decision made.
!>   - `1`: Failure in determining encoding status.
!>
PP_THREAD_SAFE FUNCTION GRIB_SECTION_TO_BE_ENCODED_IF( THIS, &
&  MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: TIME_UTILS_MOD,           ONLY: TIME_HISTORY_T
  USE :: TIME_UTILS_MOD,           ONLY: CURR_TIME_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A),      INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  LOGICAL,                         INTENT(OUT)   :: TO_BE_ENCODED
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION GRIB_SECTION_TO_BE_ENCODED_IF


!>
!> @brief Frees resources associated with a GRIB section.
!>
!> The `GRIB_SECTION_FREE_IF` function is responsible for deallocating any resources or memory
!> associated with the GRIB section represented by `THIS`. It provides an optional `VERBOSE`
!> flag that enables more detailed output during the resource cleanup. This function is
!> thread-safe and ensures that all resources tied to the GRIB section are released properly.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `GRIB_SECTION_BASE_A` representing the GRIB section to be freed.
!>   @param [in]    OPT   The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>   @return Integer error code (`RET`) indicating success (`0`) or failure (`1`).
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] GRIB_ENCODER_OPTIONS_MOD::GRIB_ENCODER_OPTIONS_T
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>
!> @section intrinsic dependencies
!>   None.
!>
!> @section Error codes
!>   - `0`: Success, resources were freed successfully.
!>   - `1`: Failure, an issue occurred during resource deallocation.
!>
PP_THREAD_SAFE FUNCTION GRIB_SECTION_FREE_IF( THIS, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A),   INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION GRIB_SECTION_FREE_IF

END INTERFACE


INTERFACE
PP_THREAD_SAFE FUNCTION SECTION_ALLOCATOR_IF( SECTION, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: GRIB_SECTION_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A), POINTER,  INTENT(INOUT) :: SECTION
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION SECTION_ALLOCATOR_IF
END INTERFACE


!> Whitelist of public symbols (Interfaces)
PUBLIC :: GRIB_SECTION_BASE_A
PUBLIC :: SECTION_ALLOCATOR_IF

CONTAINS




!>
!> @brief Return the type of the section
!>
!> @section interface
!>   @param [inout] THIS   An object of type `GRIB_SECTION_BASE_A` representing the GRIB section to be freed.
!>   @param [in]    TYPE   The the type of the section
!>   @param [in]    OPT    The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>   @return Integer error code (`RET`) indicating success (`0`) or failure (`1`).
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] GRIB_ENCODER_OPTIONS_MOD::GRIB_ENCODER_OPTIONS_T
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>
!> @section intrinsic dependencies
!>   None.
!>
!> @section Error codes
!>   - `0`: Success, resources were freed successfully.
!>   - `1`: Failure, an issue occurred during resource deallocation.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_TYPE'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_TYPE( THIS, TYPE, OPT, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_SECTION_BASE_A),   INTENT(INOUT) :: THIS
  CHARACTER(LEN=32),            INTENT(OUT)   :: TYPE
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

  ! Return the section type
  TYPE = REPEAT( ' ', 32 )
  TYPE = TRIM(ADJUSTL(THIS%TYPE_))

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN



END FUNCTION GRIB_SECTION_TYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Return the subtype of the section
!>
!> @section interface
!>   @param [inout] THIS     An object of type `GRIB_SECTION_BASE_A` representing the GRIB section to be freed.
!>   @param [in]    SUBTYPE  The the subtype of the section
!>   @param [in]    OPT      The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] HOOKS    Utilities to be used for logging, debugging, tracing and option handling
!>   @return Integer error code (`RET`) indicating success (`0`) or failure (`1`).
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>
!> @section intrinsic dependencies
!>   None.
!>
!> @section Error codes
!>   - `0`: Success, resources were freed successfully.
!>   - `1`: Failure, an issue occurred during resource deallocation.
!>
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_SUBTYPE'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_SUBTYPE( THIS, SUBTYPE, OPT, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_SECTION_BASE_A),   INTENT(INOUT) :: THIS
  CHARACTER(LEN=32),            INTENT(OUT)   :: SUBTYPE
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

  ! Return the section type
  SUBTYPE = REPEAT( ' ', 32 )
  SUBTYPE = TRIM(ADJUSTL(THIS%SUBTYPE_))

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION GRIB_SECTION_SUBTYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Return the kind of the section
!>
!> @section interface
!>   @param [inout] THIS   An object of type `GRIB_SECTION_BASE_A` representing the GRIB section to be freed.
!>   @param [in]    TYPE   The the type of the section
!>   @param [in]    OPT    The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>   @return Integer error code (`RET`) indicating success (`0`) or failure (`1`).
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>
!> @section intrinsic dependencies
!>   None.
!>
!> @section Error codes
!>   - `0`: Success, resources were freed successfully.
!>   - `1`: Failure, an issue occurred during resource deallocation.
!>
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_SECTION_KIND'
PP_THREAD_SAFE FUNCTION GRIB_SECTION_KIND( THIS, KIND, OPT, HOOKS ) RESULT(RET)

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
  CLASS(GRIB_SECTION_BASE_A),   INTENT(INOUT) :: THIS
  CHARACTER(LEN=32),            INTENT(OUT)   :: KIND
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

  ! Return the section type
  KIND = REPEAT( ' ', 32 )
  KIND = TRIM(ADJUSTL(THIS%KIND_))

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION GRIB_SECTION_KIND
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Print informations related to the grib section
!>
!> @section interface
!>   @param [inout] THIS   An object of type `GRIB_SECTION_BASE_A` representing the GRIB section to be freed.
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
#define PP_PROCEDURE_NAME 'GRIB2_SECTION_PRINT'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION_PRINT( THIS, &
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
  CLASS(GRIB_SECTION_BASE_A),   INTENT(INOUT) :: THIS
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
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_ERROR=1_JPIB_K

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
  PP_LOG_INFO( 'Print GRIB Section' )

  ! Write the section information
  IF ( PRESENT(SEPARATOR) ) THEN
    IF ( OFFSET .LE. 0 ) THEN
      WRITE(UNIT,'(A,A,A,A,A,A,A,A)', IOSTAT=WRITE_STAT) 'GRIB_SECTION: ', &
&     TRIM(ADJUSTL(THIS%TYPE_)), '::', TRIM(ADJUSTL(THIS%SUBTYPE_)), &
&     '(', TRIM(ADJUSTL(THIS%KIND_)), ')', TRIM(ADJUSTL(SEPARATOR))
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
    ELSE
      WRITE(UNIT,'(A,A,A,A,A,A,A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET), &
&     'GRIB_SECTION: ', TRIM(ADJUSTL(THIS%TYPE_)), '::', TRIM(ADJUSTL(THIS%SUBTYPE_)), &
&     '(', TRIM(ADJUSTL(THIS%KIND_)), ')', TRIM(ADJUSTL(SEPARATOR))
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
    ENDIF
  ELSE
    IF ( OFFSET .LE. 0 ) THEN
      WRITE(UNIT,'(A,A,A,A,A,A,A)', IOSTAT=WRITE_STAT) 'GRIB_SECTION: ', &
&     TRIM(ADJUSTL(THIS%TYPE_)), '::', TRIM(ADJUSTL(THIS%SUBTYPE_)), &
&     '(', TRIM(ADJUSTL(THIS%KIND_)), ')'
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
    ELSE
      WRITE(UNIT,'(A,A,A,A,A,A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET), &
&     'GRIB_SECTION: ', TRIM(ADJUSTL(THIS%TYPE_)), '::', TRIM(ADJUSTL(THIS%SUBTYPE_)), &
&     '(', TRIM(ADJUSTL(THIS%KIND_)), ')'
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

END FUNCTION GRIB2_SECTION_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GRIB_SECTION_BASE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
