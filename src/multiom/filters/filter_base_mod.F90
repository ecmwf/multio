!>
!> @file filter_base_mod.F90
!>
!>
!> @brief Module defining filter containers and operations for managing filters.
!>
!> This module provides the definition and interface for managing filters used
!> in a variety of applications. It includes:
!>   - `FILTER_BASE_A`: An abstract class that defines the interface for all
!>     filters, including initialization, matching, printing, and cleanup.
!>   - `FILTER_CONTAINER_T`: A type that holds a filter object, allowing dynamic
!>     management of different filter implementations.
!>   - Several deferred procedures that must be implemented by any concrete
!>     filter class: `INIT`, `MATCH`, `FREE`, and `PRINT`.
!>
!> The module also integrates `HOOKS_T` for logging, debugging, and tracing
!> purposes, offering flexibility in error reporting and message management.
!>
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'filter_base_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'FILTER_BASE_MOD'
MODULE FILTER_BASE_MOD
IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!>
!> @brief Abstract base class for filter operations.
!>
!> This abstract type defines a base class for filter operations. It declares
!> deferred procedures for initializing, matching, freeing, and printing filter
!> objects. Concrete implementations must provide definitions for these deferred
!> methods.
!>
TYPE, ABSTRACT :: FILTER_BASE_A
CONTAINS

  PROCEDURE(FILTER_INIT_IF),  DEFERRED, PUBLIC, PASS :: INIT
  PROCEDURE(FILTER_MATCH_IF), DEFERRED, PUBLIC, PASS :: MATCH
  PROCEDURE(FILTER_FREE_IF),  DEFERRED, PUBLIC, PASS :: FREE
  PROCEDURE(FILTER_PRINT_IF), DEFERRED, PUBLIC, PASS :: PRINT

END TYPE


ABSTRACT INTERFACE

!>
!> @brief Initializes a filter using configuration data and hooks.
!>
!> This function is responsible for initializing a filter object (`THIS`) based on the
!> provided YAML configuration (`CFG`). It returns an
!> integer status code indicating the success or failure of the initialization process.
!>
!> @section interface
!> - @param [inout] THIS  - The filter object to be initialized (must be of type `FILTER_BASE_A`).
!> - @param [in]    CFG   - The YAML configuration data used to initialize the filter (of type `YAML_CONFIGURATION_T`).
!> - @param [in]    OPT   - The generic options to be used to initialize the filter.
!> - @param [inout] HOOKS - The hooks used for debugging/tracing/logging (of type `HOOKS_T`).
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section intrinsic dependencies
!> None.
!>
!> @section local dependencies
!> - @dependency [READ] DATAKINDS_DEF_MOD::JPIB_K
!> - @dependency [READ] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!> - @dependency [READ] HOOKS_MOD::HOOKS_T
!> - @dependency [TYPE] FILTER_BASE_A
!>
!> @section external dependencies
!> None.
!>
!> @section special dependencies
!> None.
!>
PP_THREAD_SAFE FUNCTION FILTER_INIT_IF( THIS, CFG, OPT, HOOKS ) RESULT( RET )

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: FILTER_OPTIONS_MOD,       ONLY: FILTER_OPTIONS_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  !> Imported abstract class
  IMPORT :: FILTER_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FILTER_BASE_A),       INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  TYPE(FILTER_OPTIONS_T),     INTENT(IN)    :: OPT
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION FILTER_INIT_IF


!>
!> @brief Matches a filter against a message and parameterization.
!>
!> This function evaluates whether the filter (`THIS`) matches a given message (`MSG`)
!> and parameterization (`PAR`). The result is stored in the logical variable `MATCH`.
!> The function also supports debugging, tracing, and logging through the `HOOKS` structure.
!>
!> @section interface
!> - @param [inout] THIS - The filter object (must be of type `FILTER_BASE_A`) that performs the matching.
!> - @param [in] MSG - A Fortran message object used for matching (of type `FORTRAN_MESSAGE_T`).
!> - @param [in] PAR - A parameterization object used for matching (of type `PARAMETRIZATION_T`).
!> - @param [out] MATCH - A logical flag that will be set to `.TRUE.` if the filter matches the message and parameterization, or `.FALSE.` otherwise.
!> - @param [inout] HOOKS - A structure for debugging, tracing, and logging (of type `HOOKS_T`).
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section intrinsic dependencies
!> None.
!>
!> @section local dependencies
!> - @dependency [READ] DATAKINDS_DEF_MOD::JPIB_K
!> - @dependency [READ] PARAMETRIZATION_MOD::PARAMETRIZATION_T
!> - @dependency [READ] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!> - @dependency [READ] HOOKS_MOD::HOOKS_T
!> - @dependency [TYPE] FILTER_BASE_A
!>
!> @section external dependencies
!> None.
!>
!> @section special dependencies
!> None.
!>
PP_THREAD_SAFE FUNCTION FILTER_MATCH_IF( THIS, MSG, PAR, MATCH, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  !> Imported abstract class
  IMPORT :: FILTER_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FILTER_BASE_A),    INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T), INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(IN)    :: PAR
  LOGICAL,                 INTENT(OUT)   :: MATCH
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION FILTER_MATCH_IF


!>
!> @brief Releases resources associated with the filter object.
!>
!> This function deallocates any dynamically allocated memory or resources
!> related to the `FILTER_BASE_A` object. It ensures that the filter object is
!> properly freed when no longer in use. The `HOOKS` argument is used for
!> debugging, tracing, and logging purposes during the free operation.
!>
!> @param[inout] THIS The filter object to be freed.
!> @param[inout] HOOKS Debugging, logging, and tracing hooks used during
!>                      the free operation.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section local dependencies
!>   - @dependency [READWRITE] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [READWRITE] HOOKS_MOD::HOOKS_T
!>
PP_THREAD_SAFE FUNCTION FILTER_FREE_IF( THIS, HOOKS ) RESULT( RET )

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  !> Imported abstract class
  IMPORT :: FILTER_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FILTER_BASE_A), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION FILTER_FREE_IF

!>
!> @brief Prints the state of the filter object to the specified output unit.
!>
!> This function outputs the current state of the `FILTER_BASE_A` object to the
!> provided output `UNIT`, with a specified indentation `OFFSET` for formatting.
!> The `HOOKS` argument is used for logging, debugging, or tracing purposes during
!> the print operation. The output can be useful for debugging or tracing the filter
!> behavior.
!>
!> @param[inout] THIS The filter object whose state is to be printed.
!> @param[in] UNIT The Fortran I/O unit to which the filter state will be printed.
!> @param[in] OFFSET The indentation offset for formatting the output.
!> @param[inout] HOOKS Debugging, logging, and tracing hooks used during
!>                      the print operation.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section local dependencies
!>   - @dependency [READWRITE] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [READWRITE] HOOKS_MOD::HOOKS_T
!>
PP_THREAD_SAFE FUNCTION FILTER_PRINT_IF( THIS, UNIT, OFFSET, HOOKS ) RESULT( RET )

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  !> Imported abstract class
  IMPORT :: FILTER_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(FILTER_BASE_A), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION FILTER_PRINT_IF

END INTERFACE

!>
!> @brief Container structure that holds a filter object of type `FILTER_BASE_A`.
!>
!> This container type, `FILTER_CONTAINER_T`, is used to manage a filter object
!> that is based on the abstract class `FILTER_BASE_A`. The filter object is
!> stored as a pointer, allowing dynamic assignment and management of various
!> filter types during runtime. Initially, the filter pointer is set to `NULL()`.
!>
!> The purpose of this container is to provide flexibility in assigning and
!> manipulating different filter implementations without knowing the exact filter
!> type at compile time.
!>
TYPE :: FILTER_CONTAINER_T

  CLASS(FILTER_BASE_A), POINTER :: FILTER_ => NULL()

END TYPE

!> White list of public symbols
PUBLIC :: FILTER_BASE_A
PUBLIC :: FILTER_CONTAINER_T

END MODULE FILTER_BASE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
