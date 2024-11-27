!> @file
!>
!> @brief Definition of the abstract interface METADATA_BASE_A, which outlines methods
!>        for initializing, cloning, destroying, and setting various types of values.
!>
!> The METADATA_BASE_A interface serves as a template for objects that can be
!> initialized from a file, cloned, destroyed, and modified with different types of values.
!>
!> @note This interface is designed to be implemented by concrete classes, providing
!>       flexibility and extensibility for handling different types of data.
!>
!> @todo Add the read and distribute functionality in order to handle the preset phase
!>
!> @author Mirco Valentini
!> @date   January 12, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'metadata_base_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'METADATA_BASE_WITH_TRACE_MOD'
MODULE METADATA_BASE_WITH_TRACE_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

!> @brief Definition of the abstract interface METADATA_BASE_A, which outlines methods
!>        for initializing, cloning, destroying, and setting various types of values.
!>
!> The METADATA_BASE_A interface serves as a template for objects that can be
!> initialized from a file, cloned, destroyed, and modified with different types of values.
TYPE, ABSTRACT, EXTENDS(METADATA_BASE_A) :: METADATA_BASE_WITH_TRACE_A
CONTAINS

  !> @brief Initialisation status of the object
  PROCEDURE(INITIALIZED_IF), DEFERRED, PUBLIC, PASS :: INITIALIZED

  !> @brief Initializes the object with default values.
  PROCEDURE(INIT_DEFAULT_IF), DEFERRED, PUBLIC, PASS :: INIT_DEFAULT

  !> @brief Initializes the object from another metadata object.
  PROCEDURE(INIT_FROM_METADATA_IF), DEFERRED, PUBLIC, PASS :: INIT_FROM_METADATA

  !> @brief Initializes the object from an eccodes sample (already loaded in memory).
  PROCEDURE(INIT_FROM_SAMPLE_IF), DEFERRED, PUBLIC, PASS :: INIT_FROM_SAMPLE

  !> @brief Initializes the object from an eccodes sample load the file.
  PROCEDURE(INIT_FROM_SAMPLE_NAME_IF), DEFERRED, PUBLIC, PASS :: INIT_FROM_SAMPLE_NAME

  !> @brief Destroys the object.
  PROCEDURE(DESTROY_IF), DEFERRED, PUBLIC, PASS :: DESTROY

  !> @brief Sets a string value.
  PROCEDURE(SET_MISSING_IF), DEFERRED, PUBLIC, PASS :: SET_MISSING

  !> @brief Sets a string value.
  PROCEDURE(SET_STRING_IF), DEFERRED, PUBLIC, PASS :: SET_STRING

  !> @brief Sets a boolean value.
  PROCEDURE(SET_BOOL_IF), DEFERRED, PUBLIC, PASS :: SET_BOOL

  !> @brief Sets an 8-bit integer value.
  PROCEDURE(SET_INT8_IF), DEFERRED, PUBLIC, PASS :: SET_INT8

  !> @brief Sets a 16-bit integer value.
  PROCEDURE(SET_INT16_IF), DEFERRED, PUBLIC, PASS :: SET_INT16

  !> @brief Sets a 32-bit integer value.
  PROCEDURE(SET_INT32_IF), DEFERRED, PUBLIC, PASS :: SET_INT32

  !> @brief Sets a 64-bit integer value.
  PROCEDURE(SET_INT64_IF), DEFERRED, PUBLIC, PASS :: SET_INT64

  !> @brief Sets a 32-bit real value.
  PROCEDURE(SET_REAL32_IF), DEFERRED, PUBLIC, PASS :: SET_REAL32

  !> @brief Sets a 64-bit real value.
  PROCEDURE(SET_REAL64_IF), DEFERRED, PUBLIC, PASS :: SET_REAL64

  !> @brief Sets an array of string values.
  PROCEDURE(SET_STRING_ARRAY_IF), DEFERRED, PUBLIC, PASS :: SET_STRING_ARRAY

  !> @brief Sets an array of boolean values.
  PROCEDURE(SET_BOOL_ARRAY_IF), DEFERRED, PUBLIC, PASS :: SET_BOOL_ARRAY

  !> @brief Sets an array of 8-bit integer values.
  PROCEDURE(SET_INT8_ARRAY_IF), DEFERRED, PUBLIC, PASS :: SET_INT8_ARRAY

  !> @brief Sets an array of 16-bit integer values.
  PROCEDURE(SET_INT16_ARRAY_IF), DEFERRED, PUBLIC, PASS :: SET_INT16_ARRAY

  !> @brief Sets an array of 32-bit integer values.
  PROCEDURE(SET_INT32_ARRAY_IF), DEFERRED, PUBLIC, PASS :: SET_INT32_ARRAY

  !> @brief Sets an array of 64-bit integer values.
  PROCEDURE(SET_INT64_ARRAY_IF), DEFERRED, PUBLIC, PASS :: SET_INT64_ARRAY

  !> @brief Sets an array of 32-bit real values.
  PROCEDURE(SET_REAL32_ARRAY_IF), DEFERRED, PUBLIC, PASS :: SET_REAL32_ARRAY

  !> @brief Sets an array of 64-bit real values.
  PROCEDURE(SET_REAL64_ARRAY_IF), DEFERRED, PUBLIC, PASS :: SET_REAL64_ARRAY

END TYPE

CONTIANS


END MODULE METADATA_BASE_WITH_TRACE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME