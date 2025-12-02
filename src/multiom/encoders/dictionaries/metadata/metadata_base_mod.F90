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

IMPLICIT NONE

! Default visibility
PRIVATE

!> @brief Definition of the abstract interface METADATA_BASE_A, which outlines methods
!>        for initializing, cloning, destroying, and setting various types of values.
!>
!> The METADATA_BASE_A interface serves as a template for objects that can be
!> initialized from a file, cloned, destroyed, and modified with different types of values.
TYPE, ABSTRACT :: METADATA_BASE_A

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

  !> @brief Dump the sample to disk (for debugging and checking purposes)
  PROCEDURE(DUMP_SAMPLE_IF), DEFERRED, PUBLIC, PASS :: DUMP_SAMPLE

  !> @brief Get the size in bytes of the sample
  PROCEDURE(SAMPLE_SIZE_IF), DEFERRED, PUBLIC, PASS :: SAMPLE_SIZE

  !> @brief Get the size in bytes of the sample
  PROCEDURE(SAFE_LOAD_IF), DEFERRED, PUBLIC, PASS :: SAFE_LOAD

  !> @brief Generic set procedure that can handle all supported data types.
  GENERIC, PUBLIC :: SET => SET_STRING
  GENERIC, PUBLIC :: SET => SET_BOOL
  GENERIC, PUBLIC :: SET => SET_INT8
  GENERIC, PUBLIC :: SET => SET_INT16
  GENERIC, PUBLIC :: SET => SET_INT32
  GENERIC, PUBLIC :: SET => SET_INT64
  GENERIC, PUBLIC :: SET => SET_REAL32
  GENERIC, PUBLIC :: SET => SET_REAL64
  GENERIC, PUBLIC :: SET => SET_STRING_ARRAY
  GENERIC, PUBLIC :: SET => SET_BOOL_ARRAY
  GENERIC, PUBLIC :: SET => SET_INT8_ARRAY
  GENERIC, PUBLIC :: SET => SET_INT16_ARRAY
  GENERIC, PUBLIC :: SET => SET_INT32_ARRAY
  GENERIC, PUBLIC :: SET => SET_INT64_ARRAY
  GENERIC, PUBLIC :: SET => SET_REAL32_ARRAY
  GENERIC, PUBLIC :: SET => SET_REAL64_ARRAY


  !> @brief Generic initialization function combining default and file-based initialization.
  GENERIC, PUBLIC :: INIT => INIT_DEFAULT
  GENERIC, PUBLIC :: INIT => INIT_FROM_METADATA
  GENERIC, PUBLIC :: INIT => INIT_FROM_SAMPLE

END TYPE


ABSTRACT INTERFACE

!> @brief Return the initialisation status of the object
!>
!> @param [in]    this  The object to be checked.
!> @param [out]   ex    Initialization status of the object
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION INITIALIZED_IF( THIS, EX, HOOKS ) RESULT(RET)
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(IN)    :: THIS
  LOGICAL,                INTENT(OUT)   :: EX
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION INITIALIZED_IF

!> @brief Initializes the object by copying metadata from another object.
!>
!> This routine initializes the object by copying metadata from another
!> metadata object.
!>
!> @param [inout] this     The object to be initialized.
!> @param [in]    metadata Metadata object to be used for data copy.
!> @param [inout] HOOKS    Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION INIT_FROM_METADATA_IF( THIS, METADATA, HOOKS ) RESULT(RET)
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),          INTENT(INOUT) :: THIS
  CLASS(METADATA_BASE_A), POINTER, INTENT(IN)    :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION INIT_FROM_METADATA_IF

!> @brief Initializes the object from an eccodes sample.
!>
!> This procedure initializes the object by loading data from an eccodes sample.
!>
!> @attention the sample must be already loaded in memory
!>
!> @param [inout] this          The object to be initialized.
!> @param [in]    sample_name   The name of the sample file from which to load data.
!> @param [inout] HOOKS         Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION INIT_FROM_SAMPLE_NAME_IF( THIS, SAMPLE_NAME, HOOKS ) RESULT(RET)
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: SAMPLE_NAME
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION INIT_FROM_SAMPLE_NAME_IF

!> @brief Initializes the object from an eccodes sample.
!>
!> This procedure initializes the object by loading data from an eccodes sample.
!>
!> @attention the sample must be already loaded in memory
!>
!> @param [inout] this          The object to be initialized.
!> @param [in]    sample_name   The name of the sample file from which to load data.
!> @param [in]    sample_handle The sample already loaded
!> @param [inout] HOOKS         Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION INIT_FROM_SAMPLE_IF( THIS, SAMPLE_NAME, SAMPLE_HANDLE, HOOKS ) RESULT(RET)
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: SAMPLE_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: SAMPLE_HANDLE
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION INIT_FROM_SAMPLE_IF

!> @brief Initializes the object with default values.
!>
!> This procedure initializes the object with default values.
!>
!> @param [inout] this  The object to be initialized.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION INIT_DEFAULT_IF( THIS, HOOKS ) RESULT(RET)
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION INIT_DEFAULT_IF

!> @brief Destroys the object.
!>
!> This procedure deallocates resources associated with the object.
!>
!> @param [inout] this  The object to be destroyed.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION DESTROY_IF( THIS, HOOKS ) RESULT(RET)
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION DESTROY_IF


!> @brief Sets a string value.
!>
!> This procedure sets a mising value associated to a key.
!>
!> @param [inout] this  The object where the missing value is to be set.
!> @param [in]    key   The key used to store the missing value.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_MISSING_IF( THIS, KEY, HOOKS ) RESULT(RET)
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_MISSING_IF

!> @brief Sets a string value.
!>
!> This procedure sets a string value associated with a specified key.
!>
!> @param [inout] this  The object where the string value is to be set.
!> @param [in]    key   The key used to store the string value.
!> @param [in]    val   The string value to be stored.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_STRING_IF( THIS, KEY, VAL, HOOKS ) RESULT(RET)
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  CHARACTER(LEN=*),       INTENT(IN)    :: VAL
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_STRING_IF

!> @brief Sets a boolean value.
!>
!> This procedure sets a boolean value associated with a specified key.
!>
!> @param [inout] this  The object where the boolean value is to be set.
!> @param [in]    key   The key used to store the boolean value.
!> @param [in]    val   The boolean value to be stored.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_BOOL_IF( THIS, KEY, VAL, HOOKS ) RESULT(RET)
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  LOGICAL,                INTENT(IN)    :: VAL
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_BOOL_IF

!> @brief Sets an 8-bit integer value.
!>
!> This procedure sets an 8-bit integer value associated with a specified key.
!>
!> @param [inout] this  The object where the integer value is to be set.
!> @param [in]    key   The key used to store the integer value.
!> @param [in]    val   The 8-bit integer value to be stored.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_INT8_IF( THIS, KEY, VAL, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT8),     INTENT(IN)    :: VAL
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_INT8_IF

!> @brief Sets an 16-bit integer value.
!>
!> This procedure sets an 16-bit integer value associated with a specified key.
!>
!> @param [inout] this  The object where the integer value is to be set.
!> @param [in]    key   The key used to store the integer value.
!> @param [in]    val   The 16-bit integer value to be stored.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_INT16_IF( THIS, KEY, VAL, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT16),    INTENT(IN)    :: VAL
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_INT16_IF

!> @brief Sets an 32-bit integer value.
!>
!> This procedure sets an 32-bit integer value associated with a specified key.
!>
!> @param [inout] this  The object where the integer value is to be set.
!> @param [in]    key   The key used to store the integer value.
!> @param [in]    val   The 32-bit integer value to be stored.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_INT32_IF( THIS, KEY, VAL, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT32),    INTENT(IN)    :: VAL
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_INT32_IF

!> @brief Sets an 64-bit integer value.
!>
!> This procedure sets an 64-bit integer value associated with a specified key.
!>
!> @param [inout] this  The object where the integer value is to be set.
!> @param [in]    key   The key used to store the integer value.
!> @param [in]    val   The 64-bit integer value to be stored.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_INT64_IF( THIS, KEY, VAL, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT64),    INTENT(IN)    :: VAL
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_INT64_IF

!> @brief Sets a 32-bit real value.
!>
!> This procedure sets a 32-bit real value associated with a specified key.
!>
!> @param [inout] this  The object where the integer value is to be set.
!> @param [in]    key   The key used to store the integer value.
!> @param [in]    val   The 32-bit real value to be stored.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_REAL32_IF( THIS, KEY, VAL, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  REAL(KIND=REAL32),      INTENT(IN)    :: VAL
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_REAL32_IF

!> @brief Sets a 64-bit real value.
!>
!> This procedure sets a 64-bit real value associated with a specified key.
!>
!> @param [inout] this  The object where the integer value is to be set.
!> @param [in]    key   The key used to store the integer value.
!> @param [in]    val   The 64-bit real value to be stored.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_REAL64_IF( THIS, KEY, VAL, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  REAL(KIND=REAL64),      INTENT(IN)    :: VAL
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_REAL64_IF

!> @brief Sets an array of strings values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of strings values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of strings values representing the metadata to be stored.
!> @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_STRING_ARRAY_IF( THIS, KEY, VALUES, HOOKS ) RESULT(RET)
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),         INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),               INTENT(IN)    :: KEY
  CHARACTER(LEN=*), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_STRING_ARRAY_IF

!> @brief Sets an array of logical values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of logical values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of logical values representing the metadata to be stored.
!> @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_BOOL_ARRAY_IF( THIS, KEY, VALUES, HOOKS ) RESULT(RET)
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  LOGICAL, DIMENSION(:),  INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_BOOL_ARRAY_IF

!> @brief Sets an array of 8-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 8-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 8-bit integer values representing the metadata to be stored.
!> @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_INT8_ARRAY_IF( THIS, KEY, VALUES, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),           INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                 INTENT(IN)    :: KEY
  INTEGER(KIND=INT8), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_INT8_ARRAY_IF

!> @brief Sets an array of 16-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 16-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 16-bit integer values representing the metadata to be stored.
!> @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_INT16_ARRAY_IF( THIS, KEY, VALUES, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT16), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_INT16_ARRAY_IF

!> @brief Sets an array of 32-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 32-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 32-bit integer values representing the metadata to be stored.
!> @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_INT32_ARRAY_IF( THIS, KEY, VALUES, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT32), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_INT32_ARRAY_IF

!> @brief Sets an array of 64-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 64-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 64-bit integer values representing the metadata to be stored.
!> @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_INT64_ARRAY_IF( THIS, KEY, VALUES, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT64), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_INT64_ARRAY_IF

!> @brief Sets an array of 32-bit real values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 32-bit real values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 32-bit real values representing the metadata to be stored.
!> @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_REAL32_ARRAY_IF( THIS, KEY, VALUES, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL32), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_REAL32_ARRAY_IF

!> @brief Sets an array of 64-bit real values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 64-bit real values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 64-bit real values representing the metadata to be stored.
!> @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SET_REAL64_ARRAY_IF( THIS, KEY, VALUES, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL64), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SET_REAL64_ARRAY_IF

!> @brief Write the sample to disk (for debugging purposes).
!>
!> This procedure allows the user to write the metadata to disk.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    name   The name to be given to the file.
!> @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION DUMP_SAMPLE_IF( THIS, NAME, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: NAME
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION DUMP_SAMPLE_IF

!> @brief Get the size in bytes of the sample.
!>
!> This procedure allows the user to inquire the size in bytes of the sample.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [out]   size   Size of the sample in bytes.
!> @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SAMPLE_SIZE_IF( THIS, SIZE, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),   INTENT(OUT)   :: SIZE
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SAMPLE_SIZE_IF



!> @brief Sanity call which should have no side effects
!>
!> Sometimes it is not possible to set specific eccodes keys because eccodes
!> is not preparing an handle properly.
!>
!> This calls avoids these problems by dumping to binary and reloading a sample.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
PP_THREAD_SAFE FUNCTION SAFE_LOAD_IF( THIS, HOOKS ) RESULT(RET)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  IMPORT :: METADATA_BASE_A
IMPLICIT NONE
  CLASS(METADATA_BASE_A), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K) :: RET
END FUNCTION SAFE_LOAD_IF

END INTERFACE

! Whitelist of public symbols
PUBLIC :: METADATA_BASE_A

END MODULE METADATA_BASE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
