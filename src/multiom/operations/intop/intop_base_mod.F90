!>
!> @file intop_base_mod.F90
!>
!>
!> @brief Module defining operation containers and operations for managing operations.
!>
!> This module provides the definition and interface for managing operations used
!> in a variety of applications. It includes:
!>   - `INTOP_BASE_A`: An abstract class that defines the interface for all
!>     operations, including initialization, matching, printing, and cleanup.
!>   - `OPERATION_CONTAINER_T`: A type that holds a operation object, allowing dynamic
!>     management of different operation implementations.
!>   - Several deferred procedures that must be implemented by any concrete
!>     operation class: `INIT`, `MATCH`, `FREE`, and `PRINT`.
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


#define PP_FILE_NAME 'intop_base_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'INTOP_BASE_MOD'
MODULE INTOP_BASE_MOD
IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!>
!> @brief Abstract base class for operation operations.
!>
!> This abstract type defines a base class for operation operations. It declares
!> deferred procedures for initializing, matching, freeing, and printing operation
!> objects. Concrete implementations must provide definitions for these deferred
!> methods.
!>
TYPE, ABSTRACT :: INTOP_BASE_A
CONTAINS

  PROCEDURE(OPERATION_INIT_IF),  DEFERRED, PUBLIC, PASS :: INIT
  PROCEDURE(OPERATION_EVAL_IF),  DEFERRED, PUBLIC, PASS :: EVAL
  PROCEDURE(OPERATION_PRINT_IF), DEFERRED, PUBLIC, PASS :: PRINT
  PROCEDURE(OPERATION_FREE_IF),  DEFERRED, PUBLIC, PASS :: FREE

END TYPE


ABSTRACT INTERFACE

!>
!> @brief Initializes a operation using configuration data and hooks.
!>
!> This function is responsible for initializing a operation object (`THIS`) based on the
!> provided YAML configuration (`CFG`). It returns an
!> integer status code indicating the success or failure of the initialization process.
!>
!> @section interface
!> - @param [inout] THIS  - The operation object to be initialized (must be of type `INTOP_BASE_A`).
!> - @param [in]    CFG   - The YAML configuration data used to initialize the operation (of type `YAML_CONFIGURATION_T`).
!> - @param [in]    OPT   - The generic options to be used to initialize the operation.
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
!> - @dependency [TYPE] INTOP_BASE_A
!>
!> @section external dependencies
!> None.
!>
!> @section special dependencies
!> None.
!>
PP_THREAD_SAFE FUNCTION OPERATION_INIT_IF( THIS, CFG, HOOKS ) RESULT( RET )

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  !> Imported abstract class
  IMPORT :: INTOP_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_BASE_A), INTENT(INOUT) :: THIS
  TYPE(YAML_CONFIGURATION_T),  INTENT(IN)    :: CFG
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION OPERATION_INIT_IF


!>
!> @brief Evaluate an operation against a message and parameterization.
!>
!> This function evaluates an operation (`THIS`) given message (`MSG`)
!> and parameterization (`PAR`). The result is stored in the integer variable `RESULT`.
!> The function also supports debugging, tracing, and logging through the `HOOKS` structure.
!>
!> @section interface
!> - @param [inout] THIS   - The operation object (must be of type `INTOP_BASE_A`) that performs the matching.
!> - @param [in]    MSG    - A Fortran message object used for matching (of type `FORTRAN_MESSAGE_T`).
!> - @param [in]    PAR    - A parameterization object used for matching (of type `PARAMETRIZATION_T`).
!> - @param [out]   RESULT - A integer variable that contains the result of the operation
!> - @param [inout] HOOKS  - A structure for debugging, tracing, and logging (of type `HOOKS_T`).
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
!> - @dependency [TYPE] INTOP_BASE_A
!>
!> @section external dependencies
!> None.
!>
!> @section special dependencies
!> None.
!>
PP_THREAD_SAFE FUNCTION OPERATION_EVAL_IF( THIS, MSG, PAR, RESULT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  !> Imported abstract class
  IMPORT :: INTOP_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_BASE_A), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),     INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),     INTENT(IN)    :: PAR
  INTEGER(KIND=JPIB_K),        INTENT(OUT)   :: RESULT
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION OPERATION_EVAL_IF


!>
!> @brief Releases resources associated with the operation object.
!>
!> This function deallocates any dynamically allocated memory or resources
!> related to the `INTOP_BASE_A` object. It ensures that the operation object is
!> properly freed when no longer in use. The `HOOKS` argument is used for
!> debugging, tracing, and logging purposes during the free operation.
!>
!> @param[inout] THIS The operation object to be freed.
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
PP_THREAD_SAFE FUNCTION OPERATION_FREE_IF( THIS, HOOKS ) RESULT( RET )

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  !> Imported abstract class
  IMPORT :: INTOP_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_BASE_A), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION OPERATION_FREE_IF

!>
!> @brief Prints the state of the operation object to the specified output unit.
!>
!> This function outputs the current state of the `INTOP_BASE_A` object to the
!> provided output `UNIT`, with a specified indentation `OFFSET` for formatting.
!> The `HOOKS` argument is used for logging, debugging, or tracing purposes during
!> the print operation. The output can be useful for debugging or tracing the operation
!> behavior.
!>
!> @param[inout] THIS The operation object whose state is to be printed.
!> @param[in] UNIT The Fortran I/O unit to which the operation state will be printed.
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
PP_THREAD_SAFE FUNCTION OPERATION_PRINT_IF( THIS, UNIT, OFFSET, HOOKS, SEPARATOR ) RESULT( RET )

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  !> Imported abstract class
  IMPORT :: INTOP_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(INTOP_BASE_A),         INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),        INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),        INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=*), OPTIONAL,  INTENT(IN)    :: SEPARATOR

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION OPERATION_PRINT_IF

END INTERFACE

!>
!> @brief Container structure that holds a operation object of type `INTOP_BASE_A`.
!>
!> This container type, `OPERATION_CONTAINER_T`, is used to manage a operation object
!> that is based on the abstract class `INTOP_BASE_A`. The operation object is
!> stored as a pointer, allowing dynamic assignment and management of various
!> operation types during runtime. Initially, the operation pointer is set to `NULL()`.
!>
!> The purpose of this container is to provide flexibility in assigning and
!> manipulating different operation implementations without knowing the exact operation
!> type at compile time.
!>
TYPE :: INTOP_CONTAINER_T

  CLASS(INTOP_BASE_A), POINTER :: OPERATION_ => NULL()

END TYPE

!> White list of public symbols
PUBLIC :: INTOP_BASE_A
PUBLIC :: INTOP_CONTAINER_T

END MODULE INTOP_BASE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME