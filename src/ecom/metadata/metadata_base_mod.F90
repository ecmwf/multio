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
#define PP_SECTION_NAME 'METADATA_BASE_MOD'
MODULE METADATA_BASE_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD,    ONLY: JPIB_K
  USE :: METADATA_TRACER_MOD, ONLY: METADATA_TRACER_T

IMPLICIT NONE

! Default visibility
PRIVATE


INTEGER(KIND=JPIB_K), PARAMETER :: TAB_SIZE = 4_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: VALUES_PER_LINE = 20_JPIB_K

!> @brief Definition of the abstract interface METADATA_BASE_A, which outlines methods
!>        for initializing, cloning, destroying, and setting various types of values.
!>
!> The METADATA_BASE_A interface serves as a template for objects that can be
!> initialized from a file, cloned, destroyed, and modified with different types of values.
TYPE, ABSTRACT :: METADATA_BASE_A

  !> @brief Tracer object for logging
  TYPE(METADATA_TRACER_T) :: TRACER_

  !> @brief Number of nesting levels for the log file
  INTEGER(KIND=JPIB_K) :: OFFSET_ = 0

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

  !> @brief Sets a 128-bit real value.
  PROCEDURE(SET_REAL128_IF), DEFERRED, PUBLIC, PASS :: SET_REAL128

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

  !> @brief Sets an array of 128-bit real values.
  PROCEDURE(SET_REAL128_ARRAY_IF), DEFERRED, PUBLIC, PASS :: SET_REAL128_ARRAY

  !> @brief Generic set procedure that can handle all supported data types.
  GENERIC, PUBLIC :: SET => SET_STRING
  GENERIC, PUBLIC :: SET => SET_BOOL
  GENERIC, PUBLIC :: SET => SET_INT8
  GENERIC, PUBLIC :: SET => SET_INT16
  GENERIC, PUBLIC :: SET => SET_INT32
  GENERIC, PUBLIC :: SET => SET_INT64
  GENERIC, PUBLIC :: SET => SET_REAL32
  GENERIC, PUBLIC :: SET => SET_REAL64
  GENERIC, PUBLIC :: SET => SET_REAL128
  GENERIC, PUBLIC :: SET => SET_STRING_ARRAY
  GENERIC, PUBLIC :: SET => SET_BOOL_ARRAY
  GENERIC, PUBLIC :: SET => SET_INT8_ARRAY
  GENERIC, PUBLIC :: SET => SET_INT16_ARRAY
  GENERIC, PUBLIC :: SET => SET_INT32_ARRAY
  GENERIC, PUBLIC :: SET => SET_INT64_ARRAY
  GENERIC, PUBLIC :: SET => SET_REAL32_ARRAY
  GENERIC, PUBLIC :: SET => SET_REAL64_ARRAY
  GENERIC, PUBLIC :: SET => SET_REAL128_ARRAY


  !> @brief Generic initialization function combining default and file-based initialization.
  GENERIC, PUBLIC :: INIT => INIT_DEFAULT
  GENERIC, PUBLIC :: INIT => INIT_FROM_METADATA
  GENERIC, PUBLIC :: INIT => INIT_FROM_SAMPLE


  !> @brief Init from a grib handle and logging
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: INIT_FROM_SAMPLE_LOGGING => INIT_FROM_SAMPLE_LOGGING

  !> @brief Init from grib sample name and logging
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: INIT_FROM_SAMPLE_NAME_LOGGING => INIT_FROM_SAMPLE_NAME_LOGGING

  !> @brief Init from other metadata and logging
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: INIT_FROM_METADATA_LOGGING => INIT_FROM_METADATA_LOGGING


  !> @brief Sets a string value with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_STRING_LOGGING => SET_STRING_LOGGING

  !> @brief Sets a boolean value with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_BOOL_LOGGING => SET_BOOL_LOGGING

  !> @brief Sets an 8-bit integer value with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_INT8_LOGGING => SET_INT8_LOGGING

  !> @brief Sets a 16-bit integer value with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_INT16_LOGGING => SET_INT16_LOGGING

  !> @brief Sets a 32-bit integer value with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_INT32_LOGGING => SET_INT32_LOGGING

  !> @brief Sets a 64-bit integer value with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_INT64_LOGGING => SET_INT64_LOGGING

  !> @brief Sets a 32-bit real value with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_REAL32_LOGGING => SET_REAL32_LOGGING

  !> @brief Sets a 64-bit real value with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_REAL64_LOGGING => SET_REAL64_LOGGING

  !> @brief Sets a 128-bit real value with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_REAL128_LOGGING => SET_REAL128_LOGGING

  !> @brief Sets an array of string values with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_STRING_ARRAY_LOGGING => SET_STRING_ARRAY_LOGGING

  !> @brief Sets an array of boolean values with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_BOOL_ARRAY_LOGGING => SET_BOOL_ARRAY_LOGGING

  !> @brief Sets an array of 8-bit integer values with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_INT8_ARRAY_LOGGING => SET_INT8_ARRAY_LOGGING

  !> @brief Sets an array of 16-bit integer values with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_INT16_ARRAY_LOGGING => SET_INT16_ARRAY_LOGGING

  !> @brief Sets an array of 32-bit integer values with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_INT32_ARRAY_LOGGING => SET_INT32_ARRAY_LOGGING

  !> @brief Sets an array of 64-bit integer values with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_INT64_ARRAY_LOGGING => SET_INT64_ARRAY_LOGGING

  !> @brief Sets an array of 32-bit real values with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_REAL32_ARRAY_LOGGING => SET_REAL32_ARRAY_LOGGING

  !> @brief Sets an array of 64-bit real values with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_REAL64_ARRAY_LOGGING => SET_REAL64_ARRAY_LOGGING

  !> @brief Sets an array of 128-bit real values with logging.
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: SET_REAL128_ARRAY_LOGGING => SET_REAL128_ARRAY_LOGGING


  !> @brief Generic set_logging procedure that can handle all supported data types.
  GENERIC, PUBLIC :: SET_LOGGING => SET_STRING_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_BOOL_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_INT8_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_INT16_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_INT32_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_INT64_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_REAL32_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_REAL64_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_REAL128_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_STRING_ARRAY_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_BOOL_ARRAY_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_INT8_ARRAY_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_INT16_ARRAY_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_INT32_ARRAY_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_INT64_ARRAY_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_REAL32_ARRAY_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_REAL64_ARRAY_LOGGING
  GENERIC, PUBLIC :: SET_LOGGING => SET_REAL128_ARRAY_LOGGING

  !> @brief Logging management procedures
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: INIT_LOGGING     => INIT_LOGGING
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: FINALISE_LOGGING => FINALISE_LOGGING
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: ENTER_PROCEDURE  => ENTER_PROCEDURE
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: EXIT_PROCEDURE   => EXIT_PROCEDURE

END TYPE


ABSTRACT INTERFACE
!> @brief Return the initialisation status of the object
!>
!> @param [in] this  The object to be checked.
!>
!> @result        ex    Initialization status of the object
!>
FUNCTION INITIALIZED_IF( THIS ) RESULT(EX)
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(IN) :: THIS
  LOGICAL :: EX
END FUNCTION INITIALIZED_IF

!> @brief Initializes the object by copying metadata from another object.
!>
!> This routine initializes the object by copying metadata from another
!> metadata object.
!>
!> @param [inout] this         The object to be initialized.
!> @param [in]    metadata     Metadata object to be used for data copy.
!>
SUBROUTINE INIT_FROM_METADATA_IF( THIS, METADATA )
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),          INTENT(INOUT) :: THIS
  CLASS(METADATA_BASE_A), POINTER, INTENT(IN)    :: METADATA
END SUBROUTINE INIT_FROM_METADATA_IF

!> @brief Initializes the object from an eccodes sample.
!>
!> This procedure initializes the object by loading data from an eccodes sample.
!>
!> @attention the sample must be already loaded in memory
!>
!> @param [inout] this          The object to be initialized.
!> @param [in]    sample_name   The name of the sample file from which to load data.
!>
SUBROUTINE INIT_FROM_SAMPLE_NAME_IF( THIS, SAMPLE_NAME )
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: SAMPLE_NAME
END SUBROUTINE INIT_FROM_SAMPLE_NAME_IF

!> @brief Initializes the object from an eccodes sample.
!>
!> This procedure initializes the object by loading data from an eccodes sample.
!>
!> @attention the sample must be already loaded in memory
!>
!> @param [inout] this          The object to be initialized.
!> @param [in]    sample_name   The name of the sample file from which to load data.
!> @param [in]    sample_handle The sample already loaded
!>
SUBROUTINE INIT_FROM_SAMPLE_IF( THIS, SAMPLE_NAME, SAMPLE_HANDLE )
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: SAMPLE_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: SAMPLE_HANDLE
END SUBROUTINE INIT_FROM_SAMPLE_IF

!> @brief Initializes the object with default values.
!>
!> This procedure initializes the object with default values.
!>
!> @param [inout] this The object to be initialized.
!>
SUBROUTINE INIT_DEFAULT_IF( THIS )
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
END SUBROUTINE INIT_DEFAULT_IF

!> @brief Destroys the object.
!>
!> This procedure deallocates resources associated with the object.
!>
!> @param [inout] this The object to be destroyed.
!>
SUBROUTINE DESTROY_IF( THIS )
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
END SUBROUTINE DESTROY_IF

!> @brief Sets a string value.
!>
!> This procedure sets a string value associated with a specified key.
!>
!> @param [inout] this The object where the string value is to be set.
!> @param [in]    key  The key used to store the string value.
!> @param [in]    val  The string value to be stored.
!>
SUBROUTINE SET_STRING_IF( THIS, KEY, VAL )
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  CHARACTER(LEN=*),       INTENT(IN)    :: VAL
END SUBROUTINE SET_STRING_IF

!> @brief Sets a boolean value.
!>
!> This procedure sets a boolean value associated with a specified key.
!>
!> @param [inout] this The object where the boolean value is to be set.
!> @param [in]    key  The key used to store the boolean value.
!> @param [in]    val  The boolean value to be stored.
!>
SUBROUTINE SET_BOOL_IF( THIS, KEY, VAL )
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  LOGICAL,                INTENT(IN)    :: VAL
END SUBROUTINE SET_BOOL_IF

!> @brief Sets an 8-bit integer value.
!>
!> This procedure sets an 8-bit integer value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 8-bit integer value to be stored.
!>
SUBROUTINE SET_INT8_IF( THIS, KEY, VAL )
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT8),     INTENT(IN)    :: VAL
END SUBROUTINE SET_INT8_IF

!> @brief Sets an 16-bit integer value.
!>
!> This procedure sets an 16-bit integer value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 16-bit integer value to be stored.
!>
SUBROUTINE SET_INT16_IF( THIS, KEY, VAL )
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT16),    INTENT(IN)    :: VAL
END SUBROUTINE SET_INT16_IF

!> @brief Sets an 32-bit integer value.
!>
!> This procedure sets an 32-bit integer value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 32-bit integer value to be stored.
!>
SUBROUTINE SET_INT32_IF( THIS, KEY, VAL )
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT32),    INTENT(IN)    :: VAL
END SUBROUTINE SET_INT32_IF

!> @brief Sets an 64-bit integer value.
!>
!> This procedure sets an 64-bit integer value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 64-bit integer value to be stored.
!>
SUBROUTINE SET_INT64_IF( THIS, KEY, VAL )
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT64),    INTENT(IN)    :: VAL
END SUBROUTINE SET_INT64_IF

!> @brief Sets a 32-bit real value.
!>
!> This procedure sets a 32-bit real value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 32-bit real value to be stored.
!>
SUBROUTINE SET_REAL32_IF( THIS, KEY, VAL )
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  REAL(KIND=REAL32),      INTENT(IN)    :: VAL
END SUBROUTINE SET_REAL32_IF

!> @brief Sets a 64-bit real value.
!>
!> This procedure sets a 64-bit real value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 64-bit real value to be stored.
!>
SUBROUTINE SET_REAL64_IF( THIS, KEY, VAL )
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  REAL(KIND=REAL64),      INTENT(IN)    :: VAL
END SUBROUTINE SET_REAL64_IF

!> @brief Sets a 128-bit real value.
!>
!> This procedure sets a 128-bit real value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 128-bit real value to be stored.
!>
SUBROUTINE SET_REAL128_IF( THIS, KEY, VAL )
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  REAL(KIND=REAL128),     INTENT(IN)    :: VAL
END SUBROUTINE SET_REAL128_IF

!> @brief Sets an array of strings values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of strings values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of strings values representing the metadata to be stored.
!>
SUBROUTINE SET_STRING_ARRAY_IF( THIS, KEY, VALUES )
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),         INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),               INTENT(IN)    :: KEY
  CHARACTER(LEN=*), DIMENSION(:), INTENT(IN)    :: VALUES
END SUBROUTINE SET_STRING_ARRAY_IF

!> @brief Sets an array of logical values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of logical values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of logical values representing the metadata to be stored.
!>
SUBROUTINE SET_BOOL_ARRAY_IF( THIS, KEY, VALUES )
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  LOGICAL, DIMENSION(:),  INTENT(IN)    :: VALUES
END SUBROUTINE SET_BOOL_ARRAY_IF

!> @brief Sets an array of 8-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 8-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 8-bit integer values representing the metadata to be stored.
!>
SUBROUTINE SET_INT8_ARRAY_IF( THIS, KEY, VALUES )
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),           INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                 INTENT(IN)    :: KEY
  INTEGER(KIND=INT8), DIMENSION(:), INTENT(IN)    :: VALUES
END SUBROUTINE SET_INT8_ARRAY_IF

!> @brief Sets an array of 16-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 16-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 16-bit integer values representing the metadata to be stored.
!>
SUBROUTINE SET_INT16_ARRAY_IF( THIS, KEY, VALUES )
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT16), DIMENSION(:), INTENT(IN)    :: VALUES
END SUBROUTINE SET_INT16_ARRAY_IF

!> @brief Sets an array of 32-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 32-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 32-bit integer values representing the metadata to be stored.
!>
SUBROUTINE SET_INT32_ARRAY_IF( THIS, KEY, VALUES )
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT32), DIMENSION(:), INTENT(IN)    :: VALUES
END SUBROUTINE SET_INT32_ARRAY_IF

!> @brief Sets an array of 64-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 64-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 64-bit integer values representing the metadata to be stored.
!>
SUBROUTINE SET_INT64_ARRAY_IF( THIS, KEY, VALUES )
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT64), DIMENSION(:), INTENT(IN)    :: VALUES
END SUBROUTINE SET_INT64_ARRAY_IF

!> @brief Sets an array of 32-bit real values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 32-bit real values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 32-bit real values representing the metadata to be stored.
!>
SUBROUTINE SET_REAL32_ARRAY_IF( THIS, KEY, VALUES )
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL32), DIMENSION(:), INTENT(IN)    :: VALUES
END SUBROUTINE SET_REAL32_ARRAY_IF

!> @brief Sets an array of 64-bit real values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 64-bit real values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 64-bit real values representing the metadata to be stored.
!>
SUBROUTINE SET_REAL64_ARRAY_IF( THIS, KEY, VALUES )
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL64), DIMENSION(:), INTENT(IN)    :: VALUES
END SUBROUTINE SET_REAL64_ARRAY_IF

!> @brief Sets an array of 128-bit real values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 128-bit real values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 128-bit real values representing the metadata to be stored.
!>
SUBROUTINE SET_REAL128_ARRAY_IF( THIS, KEY, VALUES )
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                 INTENT(IN)    :: KEY
  REAL(KIND=REAL128), DIMENSION(:), INTENT(IN)    :: VALUES
END SUBROUTINE SET_REAL128_ARRAY_IF

END INTERFACE

! Whitelist of public symbols
PUBLIC :: METADATA_BASE_A

CONTAINS

!> @brief Initializes the object from an eccodes sample.
!>
!> This procedure initializes the object by loading data from an eccodes sample.
!>
!> @attention the sample must be already loaded in memory
!>
!> @param [inout] this          The object to be initialized.
!> @param [in]    sample_name   The name of the sample file from which to load data.
!> @param [in]    sample_handle The sample already loaded
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'INIT_FROM_SAMPLE_LOGGING'
SUBROUTINE INIT_FROM_SAMPLE_LOGGING( THIS, SAMPLE_NAME, SAMPLE_HANDLE, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: SAMPLE_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: SAMPLE_HANDLE
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=1024) :: STR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'LOADING_SAMPLE "'//TRIM(SAMPLE_NAME)//'"' )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the actual procedure
  CALL THIS%INIT_FROM_SAMPLE( SAMPLE_NAME, SAMPLE_HANDLE )

  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE INIT_FROM_SAMPLE_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Initializes the object from an eccodes sample.
!>
!> This procedure initializes the object by loading data from an eccodes sample.
!>
!> @attention the sample must be already loaded in memory
!>
!> @param [inout] this          The object to be initialized.
!> @param [in]    sample_name   The name of the sample file from which to load data.
!> @param [in]    sample_handle The sample already loaded
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'INIT_FROM_SAMPLE_LOGGING_NAME'
SUBROUTINE INIT_FROM_SAMPLE_NAME_LOGGING( THIS, SAMPLE_NAME, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: SAMPLE_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=1024) :: STR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'LOADING_SAMPLE "'//TRIM(SAMPLE_NAME)//'"' )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the actual procedure
  CALL THIS%INIT_FROM_SAMPLE_NAME( SAMPLE_NAME )

  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE INIT_FROM_SAMPLE_NAME_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Initializes the object from an eccodes sample.
!>
!> This procedure initializes the object by loading data from an eccodes sample.
!>
!> @attention the sample must be already loaded in memory
!>
!> @param [inout] this          The object to be initialized.
!> @param [in]    sample_name   The name of the sample file from which to load data.
!> @param [in]    sample_handle The sample already loaded
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'INIT_FROM_METADATA_LOGGING'
SUBROUTINE INIT_FROM_METADATA_LOGGING( THIS, OTHER, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A),          INTENT(INOUT) :: THIS
  CLASS(METADATA_BASE_A), POINTER, INTENT(IN)    :: OTHER
  CHARACTER(LEN=*),                INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),                INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),                INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),                INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),                INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),            INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=1024) :: STR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'INIT_FROM_METADATA' )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the actual procedure
  CALL THIS%INIT_FROM_METADATA( OTHER )

  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE INIT_FROM_METADATA_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'INIT_LOGGING'
SUBROUTINE INIT_LOGGING( THIS, STEP, PARAMID, UID, PREFIX, REPRES, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),   INTENT(IN)    :: STEP
  INTEGER(KIND=JPIB_K),   INTENT(IN)    :: PARAMID
  INTEGER(KIND=JPIB_K),   INTENT(IN)    :: UID
  INTEGER(KIND=JPIB_K),   INTENT(IN)    :: PREFIX
  INTEGER(KIND=JPIB_K),   INTENT(IN)    :: REPRES
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=2048) :: LOC_FNAME
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  LOC_FNAME = REPEAT(' ',2048)
  IF ( UID .GT. 0 ) THEN
    WRITE(LOC_FNAME,'(A9,I8.8,A1,I8.8,A2,I12.12,A1,I8.8,A1,I8.8,A7)') &
&   'encoding_', STEP, '_', PARAMID, '_P', ABS(UID), '_', PREFIX, '_', REPRES, '.report'
  ELSE
    WRITE(LOC_FNAME,'(A9,I8.8,A1,I8.8,A2,I12.12,A1,I8.8,A1,I8.8,A7)') &
&   'encoding_', STEP, '_', PARAMID, '_M', ABS(UID), '_', PREFIX, '_', REPRES, '.report'
  END IF

  ! Initialise tracer
  CALL THIS%TRACER_%INIT( TRIM(LOC_FNAME) )

  THIS%OFFSET_ = 0

  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE INIT_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'FINALISE_LOGGING'
SUBROUTINE FINALISE_LOGGING( THIS, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  LOGICAL :: OPENED

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Finalise the tracer and dump the file
  CALL THIS%TRACER_%DUMP()

  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE FINALISE_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENTER_PROCEDURE'
SUBROUTINE ENTER_PROCEDURE( THIS, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=1024) :: STR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'ENTER :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()

  THIS%OFFSET_ = THIS%OFFSET_ + 1

  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE ENTER_PROCEDURE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'EXIT_PROCEDURE'
SUBROUTINE EXIT_PROCEDURE( THIS, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=1024) :: STR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  THIS%OFFSET_ = THIS%OFFSET_ - 1

  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'EXIT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()

  PP_DEBUG_DEVELOP_COND_THROW( THIS%OFFSET_.LT.0,1 )

  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'offset lower than 0' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE EXIT_PROCEDURE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!> @brief Sets a string value.
!>
!> This procedure sets a string value associated with a specified key.
!>
!> @param [inout] this The object where the string value is to be set.
!> @param [in]    key  The key used to store the string value.
!> @param [in]    val  The string value to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_STRING_LOGGING'
SUBROUTINE SET_STRING_LOGGING( THIS, KEY, VAL, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  CHARACTER(LEN=*),       INTENT(IN)    :: VAL
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=1024) :: STR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_STRING '//TRIM(KEY)//' = '//TRIM(VAL) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_STRING( KEY, VAL )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_STRING_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets a boolean value.
!>
!> This procedure sets a boolean value associated with a specified key.
!>
!> @param [inout] this The object where the boolean value is to be set.
!> @param [in]    key  The key used to store the boolean value.
!> @param [in]    val  The boolean value to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_BOOL_LOGGING'
SUBROUTINE SET_BOOL_LOGGING( THIS, KEY, VAL, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  LOGICAL,                INTENT(IN)    :: VAL
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=1024) :: STR
  CHARACTER(LEN=16)   :: STR2

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  STR2 = REPEAT(' ',16)
  WRITE(STR2,'(L)') VAL
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_BOOL '//TRIM(KEY)//' = '//TRIM(ADJUSTL(STR2)) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_BOOL( KEY, VAL )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_BOOL_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an 8-bit integer value.
!>
!> This procedure sets an 8-bit integer value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 8-bit integer value to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_INT8_LOGGING'
SUBROUTINE SET_INT8_LOGGING( THIS, KEY, VAL, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT8),     INTENT(IN)    :: VAL
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=1024) :: STR
  CHARACTER(LEN=16)   :: STR2

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  STR2 = REPEAT(' ',16)
  WRITE(STR2,'(I8)') VAL
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_INT8 '//TRIM(KEY)//' = '//TRIM(ADJUSTL(STR2)) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_INT8( KEY, VAL )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_INT8_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an 16-bit integer value.
!>
!> This procedure sets an 16-bit integer value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 16-bit integer value to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_INT16_LOGGING'
SUBROUTINE SET_INT16_LOGGING( THIS, KEY, VAL, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT16),    INTENT(IN)    :: VAL
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=1024) :: STR
  CHARACTER(LEN=16)   :: STR2

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  STR2 = REPEAT(' ',16)
  WRITE(STR2,'(I8)') VAL
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_INT16 '//TRIM(KEY)//' = '//TRIM(ADJUSTL(STR2)) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_INT16( KEY, VAL )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_INT16_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an 32-bit integer value.
!>
!> This procedure sets an 32-bit integer value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 32-bit integer value to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_INT32_LOGGING'
SUBROUTINE SET_INT32_LOGGING( THIS, KEY, VAL, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT32),    INTENT(IN)    :: VAL
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=1024) :: STR
  CHARACTER(LEN=16)   :: STR2

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  STR2 = REPEAT(' ',16)
  WRITE(STR2,'(I8)') VAL
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_INT32 '//TRIM(KEY)//' = '//TRIM(ADJUSTL(STR2)) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_INT32( KEY, VAL )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_INT32_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an 64-bit integer value.
!>
!> This procedure sets an 64-bit integer value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 64-bit integer value to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_INT64_LOGGING'
SUBROUTINE SET_INT64_LOGGING( THIS, KEY, VAL, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT64),    INTENT(IN)    :: VAL
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=1024) :: STR
  CHARACTER(LEN=16)   :: STR2

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  STR2 = REPEAT(' ',16)
  WRITE(STR2,'(I8)') VAL
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_INT64 '//TRIM(KEY)//' = '//TRIM(ADJUSTL(STR2)) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_INT64( KEY, VAL )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_INT64_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets a 32-bit real value.
!>
!> This procedure sets a 32-bit real value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 32-bit real value to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_REAL32_LOGGING'
SUBROUTINE SET_REAL32_LOGGING( THIS, KEY, VAL, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  REAL(KIND=REAL32),      INTENT(IN)    :: VAL
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=1024) :: STR
  CHARACTER(LEN=16)   :: STR2

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  STR2 = REPEAT(' ',16)
  WRITE(STR2,'(F11.4)') VAL
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_REAL32 '//TRIM(KEY)//' = '//TRIM(ADJUSTL(STR2)) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_REAL32( KEY, VAL )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_REAL32_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets a 64-bit real value.
!>
!> This procedure sets a 64-bit real value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 64-bit real value to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_REAL64_LOGGING'
SUBROUTINE SET_REAL64_LOGGING( THIS, KEY, VAL, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  REAL(KIND=REAL64),      INTENT(IN)    :: VAL
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=1024) :: STR
  CHARACTER(LEN=16)   :: STR2

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  STR2 = REPEAT(' ',16)
  WRITE(STR2,'(F11.4)') VAL
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_REAL64 '//TRIM(KEY)//' = '//TRIM(ADJUSTL(STR2)) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_REAL64( KEY, VAL )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_REAL64_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!> @brief Sets a 128-bit real value.
!>
!> This procedure sets a 128-bit real value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 128-bit real value to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_REAL128_LOGGING'
SUBROUTINE SET_REAL128_LOGGING( THIS, KEY, VAL, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  REAL(KIND=REAL128),     INTENT(IN)    :: VAL
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=1024) :: STR
  CHARACTER(LEN=16)   :: STR2

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  STR2 = REPEAT(' ',16)
  WRITE(STR2,'(F11.4)') VAL
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_REAL32 '//TRIM(KEY)//' = '//TRIM(ADJUSTL(STR2)) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_REAL128( KEY, VAL )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_REAL128_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an array of strings values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of strings values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of strings values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_STRING_ARRAY_LOGGING'
SUBROUTINE SET_STRING_ARRAY_LOGGING( THIS, KEY, VALUES, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A),         INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),               INTENT(IN)    :: KEY
  CHARACTER(LEN=*), DIMENSION(:), INTENT(IN)    :: VALUES
  CHARACTER(LEN=*),               INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),               INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),               INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),               INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),               INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),           INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=16)    :: TMP
  CHARACTER(LEN=1024)  :: STR
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Length of the array
  TMP = REPEAT(' ',16)
  WRITE(TMP,'(I16)') SIZE(VALUES)

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_STRING_ARRAY('//TRIM(ADJUSTL(TMP))//') '//TRIM(KEY)//' = [ ...' )
  CALL THIS%TRACER_%ADVANCE_LINE()
  CNT = 0
  WriteLoop: DO
    DO I = 1, MIN( VALUES_PER_LINE, SIZE(VALUES)-CNT)
      CNT = CNT + 1
      IF ( I .EQ. 1 ) THEN
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(VALUES(CNT))//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(VALUES(CNT)) )
        ENDIF
      ELSE
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(VALUES(CNT))//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(VALUES(CNT)) )
        ENDIF
      ENDIF
    ENDDO
    IF ( CNT .GE. SIZE(VALUES) ) THEN
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ] ' )
      CALL THIS%TRACER_%ADVANCE_LINE()
      EXIT WriteLoop
    ELSE
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ...' )
      CALL THIS%TRACER_%ADVANCE_LINE()
    ENDIF
  ENDDO WriteLoop
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_STRING_ARRAY( KEY, VALUES )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_STRING_ARRAY_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an array of logical values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of logical values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of logical values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_BOOL_ARRAY_LOGGING'
SUBROUTINE SET_BOOL_ARRAY_LOGGING( THIS, KEY, VALUES, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  LOGICAL, DIMENSION(:),  INTENT(IN)    :: VALUES
  CHARACTER(LEN=*),       INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),       INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=16)    :: TMP
  CHARACTER(LEN=1024)  :: STR
  CHARACTER(LEN=16)    :: STR2
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Length of the array
  TMP = REPEAT(' ',16)
  WRITE(TMP,'(I16)') SIZE(VALUES)

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_BOOL_ARRAY('//TRIM(ADJUSTL(TMP))//') '//TRIM(KEY)//' = [ ...' )
  CALL THIS%TRACER_%ADVANCE_LINE()
  CNT = 0
  WriteLoop: DO
    DO I = 1, MIN( VALUES_PER_LINE, SIZE(VALUES)-CNT)
      CNT = CNT + 1
      STR2 = REPEAT(' ',16)
      WRITE(STR2,'(L)') VALUES(CNT)
      IF ( I .EQ. 1 ) THEN
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2) )
        ENDIF
      ELSE
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2) )
        ENDIF
      ENDIF
    ENDDO
    IF ( CNT .GE. SIZE(VALUES) ) THEN
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ] ' )
      CALL THIS%TRACER_%ADVANCE_LINE()
      EXIT WriteLoop
    ELSE
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ...' )
      CALL THIS%TRACER_%ADVANCE_LINE()
    ENDIF
  ENDDO WriteLoop
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_BOOL_ARRAY( KEY, VALUES )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_BOOL_ARRAY_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an array of 8-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 8-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 8-bit integer values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_INT8_ARRAY_LOGGING'
SUBROUTINE SET_INT8_ARRAY_LOGGING( THIS, KEY, VALUES, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A),           INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                 INTENT(IN)    :: KEY
  INTEGER(KIND=INT8), DIMENSION(:), INTENT(IN)    :: VALUES
  CHARACTER(LEN=*),                 INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),                 INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),                 INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),                 INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),                 INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),             INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=16)    :: TMP
  CHARACTER(LEN=1024)  :: STR
  CHARACTER(LEN=16)    :: STR2
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Length of the array
  TMP = REPEAT(' ',16)
  WRITE(TMP,'(I16)') SIZE(VALUES)

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_INT8_ARRAY('//TRIM(ADJUSTL(TMP))//') '//TRIM(KEY)//' = [ ...' )
  CALL THIS%TRACER_%ADVANCE_LINE()
  CNT = 0
  WriteLoop: DO
    DO I = 1, MIN( VALUES_PER_LINE, SIZE(VALUES)-CNT)
      CNT = CNT + 1
      STR2 = REPEAT(' ',16)
      WRITE(STR2,'(I8)') VALUES(CNT)
      IF ( I .EQ. 1 ) THEN
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2) )
        ENDIF
      ELSE
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2) )
        ENDIF
      ENDIF
    ENDDO
    IF ( CNT .GE. SIZE(VALUES) ) THEN
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ] ' )
      CALL THIS%TRACER_%ADVANCE_LINE()
      EXIT WriteLoop
    ELSE
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ...' )
      CALL THIS%TRACER_%ADVANCE_LINE()
    ENDIF
  ENDDO WriteLoop
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_INT8_ARRAY( KEY, VALUES )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_INT8_ARRAY_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an array of 16-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 16-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 16-bit integer values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_INT16_ARRAY_LOGGING'
SUBROUTINE SET_INT16_ARRAY_LOGGING( THIS, KEY, VALUES, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT16), DIMENSION(:), INTENT(IN)    :: VALUES
  CHARACTER(LEN=*),                  INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),                  INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),                  INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),                  INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),                  INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),              INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=16)    :: TMP
  CHARACTER(LEN=1024)  :: STR
  CHARACTER(LEN=16)    :: STR2
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Length of the array
  TMP = REPEAT(' ',16)
  WRITE(TMP,'(I16)') SIZE(VALUES)

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_INT16_ARRAY('//TRIM(ADJUSTL(TMP))//') '//TRIM(KEY)//' = [ ...' )
  CALL THIS%TRACER_%ADVANCE_LINE()
  CNT = 0
  WriteLoop: DO
    DO I = 1, MIN( VALUES_PER_LINE, SIZE(VALUES)-CNT)
      CNT = CNT + 1
      STR2 = REPEAT(' ',16)
      WRITE(STR2,'(I8)') VALUES(CNT)
      IF ( I .EQ. 1 ) THEN
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE(  REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2) )
        ENDIF
      ELSE
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2) )
        ENDIF
      ENDIF
    ENDDO
    IF ( CNT .GE. SIZE(VALUES) ) THEN
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ] ' )
      CALL THIS%TRACER_%ADVANCE_LINE()
      EXIT WriteLoop
    ELSE
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ...' )
      CALL THIS%TRACER_%ADVANCE_LINE()
    ENDIF
  ENDDO WriteLoop
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_INT16_ARRAY( KEY, VALUES )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_INT16_ARRAY_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an array of 32-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 32-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 32-bit integer values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_INT32_ARRAY_LOGGING'
SUBROUTINE SET_INT32_ARRAY_LOGGING( THIS, KEY, VALUES, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT32), DIMENSION(:), INTENT(IN)    :: VALUES
  CHARACTER(LEN=*),                  INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),                  INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),                  INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),                  INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),                  INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),              INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=16)    :: TMP
  CHARACTER(LEN=1024)  :: STR
  CHARACTER(LEN=16)    :: STR2
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Length of the array
  TMP = REPEAT(' ',16)
  WRITE(TMP,'(I16)') SIZE(VALUES)

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_INT32_ARRAY('//TRIM(ADJUSTL(TMP))//') '//TRIM(KEY)//' = [ ...' )
  CALL THIS%TRACER_%ADVANCE_LINE()
  CNT = 0
  WriteLoop: DO
    DO I = 1, MIN( VALUES_PER_LINE, SIZE(VALUES)-CNT)
      CNT = CNT + 1
      STR2 = REPEAT(' ',16)
      WRITE(STR2,'(I8)') VALUES(CNT)
      IF ( I .EQ. 1 ) THEN
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2) )
        ENDIF
      ELSE
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2) )
        ENDIF
      ENDIF
    ENDDO
    IF ( CNT .GE. SIZE(VALUES) ) THEN
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ] ' )
      CALL THIS%TRACER_%ADVANCE_LINE()
      EXIT WriteLoop
    ELSE
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ...' )
      CALL THIS%TRACER_%ADVANCE_LINE()
    ENDIF
  ENDDO WriteLoop
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_INT32_ARRAY( KEY, VALUES )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_INT32_ARRAY_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an array of 64-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 64-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 64-bit integer values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_INT64_ARRAY_LOGGING'
SUBROUTINE SET_INT64_ARRAY_LOGGING( THIS, KEY, VALUES, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT64), DIMENSION(:), INTENT(IN)    :: VALUES
  CHARACTER(LEN=*),                  INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),                  INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),                  INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),                  INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),                  INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),              INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=16)    :: TMP
  CHARACTER(LEN=1024)  :: STR
  CHARACTER(LEN=16)    :: STR2
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Length of the array
  TMP = REPEAT(' ',16)
  WRITE(TMP,'(I16)') SIZE(VALUES)

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_INT64_ARRAY('//TRIM(ADJUSTL(TMP))//') '//TRIM(KEY)//' = [ ...' )
  CALL THIS%TRACER_%ADVANCE_LINE()
  CNT = 0
  WriteLoop: DO
    DO I = 1, MIN( VALUES_PER_LINE, SIZE(VALUES)-CNT)
      CNT = CNT + 1
      STR2 = REPEAT(' ',16)
      WRITE(STR2,'(I8)') VALUES(CNT)
      IF ( I .EQ. 1 ) THEN
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2) )
        ENDIF
      ELSE
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2) )
        ENDIF
      ENDIF
    ENDDO
    IF ( CNT .GE. SIZE(VALUES) ) THEN
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ] ' )
      CALL THIS%TRACER_%ADVANCE_LINE()
      EXIT WriteLoop
    ELSE
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ...' )
      CALL THIS%TRACER_%ADVANCE_LINE()
    ENDIF
  ENDDO WriteLoop
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_INT64_ARRAY( KEY, VALUES )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_INT64_ARRAY_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an array of 32-bit real values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 32-bit real values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 32-bit real values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_REAL32_ARRAY_LOGGING'
SUBROUTINE SET_REAL32_ARRAY_LOGGING( THIS, KEY, VALUES, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL32), DIMENSION(:), INTENT(IN)    :: VALUES
  CHARACTER(LEN=*),                INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),                INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),                INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),                INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),                INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),            INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=16)    :: TMP
  CHARACTER(LEN=1024)  :: STR
  CHARACTER(LEN=16)    :: STR2
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Length of the array
  TMP = REPEAT(' ',16)
  WRITE(TMP,'(I16)') SIZE(VALUES)

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_REAL32_ARRAY('//TRIM(ADJUSTL(TMP))//') '//TRIM(KEY)//' = [ ...' )
  CALL THIS%TRACER_%ADVANCE_LINE()
  CNT = 0
  WriteLoop: DO
    DO I = 1, MIN( VALUES_PER_LINE, SIZE(VALUES)-CNT)
      CNT = CNT + 1
      STR2 = REPEAT(' ',16)
      WRITE(STR2,'(F11.4)') VALUES(CNT)
      IF ( I .EQ. 1 ) THEN
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2) )
        ENDIF
      ELSE
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2) )
        ENDIF
      ENDIF
    ENDDO
    IF ( CNT .GE. SIZE(VALUES) ) THEN
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ] ' )
      CALL THIS%TRACER_%ADVANCE_LINE()
      EXIT WriteLoop
    ELSE
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ...' )
      CALL THIS%TRACER_%ADVANCE_LINE()
    ENDIF
  ENDDO WriteLoop
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_REAL32_ARRAY( KEY, VALUES )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_REAL32_ARRAY_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an array of 64-bit real values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 64-bit real values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 64-bit real values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_REAL64_ARRAY_LOGGING'
SUBROUTINE SET_REAL64_ARRAY_LOGGING( THIS, KEY, VALUES, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL64), DIMENSION(:), INTENT(IN)    :: VALUES
  CHARACTER(LEN=*),                INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),                INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),                INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),                INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),                INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),            INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=16)    :: TMP
  CHARACTER(LEN=1024)  :: STR
  CHARACTER(LEN=16)    :: STR2
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Length of the array
  TMP = REPEAT(' ',16)
  WRITE(TMP,'(I16)') SIZE(VALUES)

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_REAL64_ARRAY('//TRIM(ADJUSTL(TMP))//') '//TRIM(KEY)//' = [ ...' )
  CALL THIS%TRACER_%ADVANCE_LINE()
  CNT = 0
  WriteLoop: DO
    DO I = 1, MIN( VALUES_PER_LINE, SIZE(VALUES)-CNT)
      CNT = CNT + 1
      STR2 = REPEAT(' ',16)
      WRITE(STR2,'(F11.4)') VALUES(CNT)
      IF ( I .EQ. 1 ) THEN
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2) )
        ENDIF
      ELSE
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2) )
        ENDIF
      ENDIF
    ENDDO
    IF ( CNT .GE. SIZE(VALUES) ) THEN
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ] ' )
      CALL THIS%TRACER_%ADVANCE_LINE()
      EXIT WriteLoop
    ELSE
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ...' )
      CALL THIS%TRACER_%ADVANCE_LINE()
    ENDIF
  ENDDO WriteLoop
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_REAL64_ARRAY( KEY, VALUES )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_REAL64_ARRAY_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an array of 128-bit real values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 128-bit real values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 128-bit real values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_REAL128_ARRAY_LOGGING'
SUBROUTINE SET_REAL128_ARRAY_LOGGING( THIS, KEY, VALUES, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE )

  ! Symbols imported from other modules within the project.
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A),           INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                 INTENT(IN)    :: KEY
  REAL(KIND=REAL128), DIMENSION(:), INTENT(IN)    :: VALUES
  CHARACTER(LEN=*),                 INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),                 INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),                 INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),                 INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),                 INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K),             INTENT(IN)    :: CLINE

  ! Local variables
  CHARACTER(LEN=16)    :: TMP
  CHARACTER(LEN=1024)  :: STR
  CHARACTER(LEN=16)    :: STR2
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the position in the code
  CALL WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Length of the array
  TMP = REPEAT(' ',16)
  WRITE(TMP,'(I16)') SIZE(VALUES)

  ! Write the key, value pair to the log file
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'AT :: '//TRIM(STR) )
  CALL THIS%TRACER_%ADVANCE_LINE()
  THIS%OFFSET_ = THIS%OFFSET_ + 1
  CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//'SET_REAL128_ARRAY('//TRIM(ADJUSTL(TMP))//') '//TRIM(KEY)//' = [ ...' )
  CALL THIS%TRACER_%ADVANCE_LINE()
  CNT = 0
  WriteLoop: DO
    DO I = 1, MIN( VALUES_PER_LINE, SIZE(VALUES)-CNT)
      CNT = CNT + 1
      STR2 = REPEAT(' ',16)
      WRITE(STR2,'(F11.4)') VALUES(CNT)
      IF ( I .EQ. 1 ) THEN
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( REPEAT(' ', THIS%OFFSET_*TAB_SIZE)//TRIM(STR2) )
        ENDIF
      ELSE
        IF ( CNT .LT. SIZE(VALUES) ) THEN
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2)//',' )
        ELSE
          CALL THIS%TRACER_%APPEND_TO_LINE( TRIM(STR2) )
        ENDIF
      ENDIF
    ENDDO
    IF ( CNT .GE. SIZE(VALUES) ) THEN
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ] ' )
      CALL THIS%TRACER_%ADVANCE_LINE()
      EXIT WriteLoop
    ELSE
      CALL THIS%TRACER_%APPEND_TO_LINE( ' ...' )
      CALL THIS%TRACER_%ADVANCE_LINE()
    ENDIF
  ENDDO WriteLoop
  THIS%OFFSET_ = THIS%OFFSET_ - 1

  ! Call the proper set routine
  CALL THIS%SET_REAL128_ARRAY( KEY, VALUES )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE SET_REAL128_ARRAY_LOGGING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'WRITE_POS'
SUBROUTINE WRITE_POS( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, CLINE  )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(INOUT) :: STR
  CHARACTER(LEN=*),     INTENT(IN)    :: FNAME
  CHARACTER(LEN=*),     INTENT(IN)    :: SECTION_TYPE
  CHARACTER(LEN=*),     INTENT(IN)    :: SECTION_NAME
  CHARACTER(LEN=*),     INTENT(IN)    :: PROC_TYPE
  CHARACTER(LEN=*),     INTENT(IN)    :: PROC_NAME
  INTEGER(KIND=JPIM_K), INTENT(IN)    :: CLINE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ERR_LEN
  CHARACTER(LEN=16)    :: TMP
  CHARACTER(LEN=16)    :: CLLINE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Convert integers to strings
  CLLINE = REPEAT(' ',16)
  WRITE(CLLINE,'(I16)') CLINE

  ERR_LEN = &
  & LEN(FNAME) + &
  & LEN(SECTION_TYPE) + &
  & LEN(SECTION_NAME) + &
  & LEN(PROC_TYPE) + &
  & LEN(PROC_NAME) + &
  & LEN(CLLINE)

  PP_DEBUG_CRITICAL_COND_THROW( ERR_LEN.GT.LEN(STR), 1 )

   STR = &
&  'file:"'//TRIM(FNAME)//'"; '// &
&  'sectionType:"'//TRIM(SECTION_TYPE)//'"; '// &
&  'sectionName:"'//TRIM(SECTION_NAME)//'"; '// &
&  'procedureType:"'//TRIM(PROC_TYPE)//'"; '// &
&  'procedureName:"'//TRIM(PROC_NAME)//'"; '// &
&  'line:'//TRIM(ADJUSTL(CLLINE))//'; '

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'String too short to fit position' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE WRITE_POS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE METADATA_BASE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
