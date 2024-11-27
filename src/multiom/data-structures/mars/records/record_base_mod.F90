! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'record_base_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'RECORD_BASE_MOD'
MODULE RECORD_BASE_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

!> Default visibility of the module members
PRIVATE

TYPE :: RECORD_COLLECTION_T
  CHARACTER(LEN=128) :: NAME_= REPEAT( ' ', 16 )
  CLASS(RECORD_BASE_A), ALLOCATABLE :: RECORD_ => NULL()
END TYPE


TYPE, ABSTRACT :: RECORD_BASE_A

  !> Symbols imported from other modules within the project.
  LOGICAL :: INITIALIZED_ = .FALSE.

  !> Value

CONTAINS

  !> @brief Initializes the record
  PROCEDURE(RECORD_INIT_IF), DEFERRED, PASS, PUBLIC :: INIT

  !> @brief Checks if the record has been initialized
  PROCEDURE(RECORD_HAS_IF), DEFERRED, PASS, PUBLIC :: HAS

  !> @brief Checks if the record has been initialized
  PROCEDURE(RECORD_IS_RANGE_IF), DEFERRED, PASS, PUBLIC :: IS_SCALAR
  PROCEDURE(RECORD_IS_RANGE_IF), DEFERRED, PASS, PUBLIC :: IS_RANGE
  PROCEDURE(RECORD_IS_RANGE_IF), DEFERRED, PASS, PUBLIC :: IS_ARRAY

  !> @brief Reset to value to an unitialized value
  PROCEDURE(RECORD_RESET_IF), DEFERRED, PASS, PUBLIC :: RESET

  !> @brief Set the value of the record
  PROCEDURE(RECORD_SET_STRING_IF),       DEFERRED, PASS, PUBLIC :: SET_STRING
  PROCEDURE(RECORD_SET_BOOL_IF),         DEFERRED, PASS, PUBLIC :: SET_BOOL
  PROCEDURE(RECORD_SET_INT64_IF),        DEFERRED, PASS, PUBLIC :: SET_INT64
  PROCEDURE(RECORD_SET_REAL32_IF),       DEFERRED, PASS, PUBLIC :: SET_REAL64
  PROCEDURE(RECORD_SET_INT64_RANGE_IF),  DEFERRED, PASS, PUBLIC :: SET_INT64_RANGE
  PROCEDURE(RECORD_SET_INT64_ARRAY_IF),  DEFERRED, PASS, PUBLIC :: SET_INT64_ARRAY
  PROCEDURE(RECORD_SET_REAL32_ARRAY_IF), DEFERRED, PASS, PUBLIC :: SET_REAL64_ARRAY

  !> @brief Get the value of the record
  PROCEDURE(RECORD_GET_STRING_IF),       DEFERRED, PASS, PUBLIC :: GET_STRING
  PROCEDURE(RECORD_GET_BOOL_IF),         DEFERRED, PASS, PUBLIC :: GET_BOOL
  PROCEDURE(RECORD_GET_INT64_IF),        DEFERRED, PASS, PUBLIC :: GET_INT64
  PROCEDURE(RECORD_GET_REAL32_IF),       DEFERRED, PASS, PUBLIC :: GET_REAL64
  PROCEDURE(RECORD_GET_INT64_RANGE_IF),  DEFERRED, PASS, PUBLIC :: GET_INT64_RANGE
  PROCEDURE(RECORD_GET_INT64_ARRAY_IF),  DEFERRED, PASS, PUBLIC :: GET_INT64_ARRAY
  PROCEDURE(RECORD_GET_REAL64_ARRAY_IF), DEFERRED, PASS, PUBLIC :: GET_REAL64_ARRAY

  !> @brief Convert the record to a string to be printed
  PROCEDURE(RECORD_TO_STRING_IF), DEFERRED, PASS, PUBLIC :: TO_STRING

  !> @brief Compare two records for equality
  PROCEDURE(RECORD_IS_EQUAL_TO_IF), DEFERRED, PASS, PUBLIC :: IS_EQUAL_TO

  !> @brief Compare two records for ordering
  PROCEDURE(RECORD_IS_LOWER_THAN_IF), DEFERRED, PASS, PUBLIC :: IS_LOWER_THAN

  !> @brief Copy state to other
  PROCEDURE(RECORD_COPY_FROM_IF), DEFERRED, PASS, PUBLIC :: COPY_FROM

  !> @brief Free the record (reset all internal fields)
  PROCEDURE(RECORD_FREE_IF), DEFERRED, PASS, PUBLIC :: FREE

  ! Generic interface for setting values
  GENERIC, PUBLIC :: SET => SET_STRING
  GENERIC, PUBLIC :: SET => SET_BOOL
  GENERIC, PUBLIC :: SET => SET_INT64
  GENERIC, PUBLIC :: SET => SET_INT64_ARRAY
  GENERIC, PUBLIC :: SET => SET_INT_64_RANGE
  GENERIC, PUBLIC :: SET => SET_REAL64
  GENERIC, PUBLIC :: SET => SET_REAL64_ARRAY


  ! Generic interface for setting values
  GENERIC, PUBLIC :: GET => GET_STRING
  GENERIC, PUBLIC :: GET => GET_BOOL
  GENERIC, PUBLIC :: GET => GET_INT64
  GENERIC, PUBLIC :: GET => GET_INT64_ARRAY
  GENERIC, PUBLIC :: GET => GET_INT_64_RANGE
  GENERIC, PUBLIC :: GET => GET_REAL64
  GENERIC, PUBLIC :: GET => GET_REAL64_ARRAY

END TYPE


ABSTRACT INTERFACE

PP_THREAD_SAFE FUNCTION RECORD_INIT_IF( THIS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_INIT_IF

PP_THREAD_SAFE FUNCTION RECORD_HAS_IF( THIS, HAS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  LOGICAL,              INTENT(OUT)   :: HAS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_HAS_IF

PP_THREAD_SAFE FUNCTION RECORD_IS_SCALAR_IF( THIS, IS_SCALAR, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  LOGICAL,              INTENT(OUT)   :: IS_SCALAR
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_IS_SCALAR_IF

PP_THREAD_SAFE FUNCTION RECORD_IS_RANGE_IF( THIS, IS_RANGE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  LOGICAL,              INTENT(OUT)   :: IS_RANGE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_IS_RANGE_IF

PP_THREAD_SAFE FUNCTION RECORD_IS_ARRAY_IF( THIS, IS_ARRAY, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  LOGICAL,              INTENT(OUT)   :: IS_ARRAY
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_IS_ARRAY_IF


PP_THREAD_SAFE FUNCTION RECORD_RESET_IF( THIS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  LOGICAL,              INTENT(OUT)   :: VALUES_IS_OK
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_RESET_IF




PP_THREAD_SAFE FUNCTION RECORD_SET_STRING_IF( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),     INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_SET_STRING_IF


PP_THREAD_SAFE FUNCTION RECORD_SET_BOOL_IF( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  LOGICAL,              INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_SET_BOOL_IF


PP_THREAD_SAFE FUNCTION RECORD_SET_INT64_IF( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_SET_INT64_IF


PP_THREAD_SAFE FUNCTION RECORD_SET_INT64_RANGE_IF( THIS, VALUE1, VALUE2, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VALUE1
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VALUE2
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_SET_INT64_RANGE_IF


PP_THREAD_SAFE FUNCTION RECORD_SET_INT64_ARRAY_IF( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A),               INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K), DIMENSION(:), INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_SET_INT64_ARRAY_IF


PP_THREAD_SAFE FUNCTION RECORD_SET_REAL64_IF( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  REAL(KIND=JPRD_K),    INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_SET_REAL64_IF


PP_THREAD_SAFE FUNCTION RECORD_SET_REAL64_ARRAY_IF( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A),            INTENT(INOUT) :: THIS
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_SET_REAL64_ARRAY_IF



PP_THREAD_SAFE FUNCTION RECORD_TO_STRING_IF( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),     INTENT(IN)    :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_TO_STRING_IF

PP_THREAD_SAFE FUNCTION RECORD_GET_STRING_IF( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),     INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_GET_STRING_IF


PP_THREAD_SAFE FUNCTION RECORD_GET_BOOL_IF( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  LOGICAL,              INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_GET_BOOL_IF


PP_THREAD_SAFE FUNCTION RECORD_GET_INT64_IF( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_GET_INT64_IF


PP_THREAD_SAFE FUNCTION RECORD_GET_INT64_RANGE_IF( THIS, VALUE1, VALUE2, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: VALUE1
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: VALUE2
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_GET_INT64_RANGE_IF


PP_THREAD_SAFE FUNCTION RECORD_GET_INT64_ARRAY_IF( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A),               INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K), DIMENSION(:), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_GET_INT64_ARRAY_IF


PP_THREAD_SAFE FUNCTION RECORD_GET_REAL64_IF( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  REAL(KIND=JPRD_K),    INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_GET_REAL64_IF


PP_THREAD_SAFE FUNCTION RECORD_GET_REAL64_ARRAY_IF( THIS, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A),            INTENT(INOUT) :: THIS
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_GET_REAL64_ARRAY_IF




PP_THREAD_SAFE FUNCTION RECORD_IS_EQUAL_TO_IF( THIS, OTHER, IS_EQUAL_TO, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A),          INTENT(INOUT) :: THIS
  CLASS(RECORD_BASE_A), POINTER, INTENT(IN)    :: OTHER
  LOGICAL,                       INTENT(OUT)   :: IS_EQUAL_TO
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_IS_EQUAL_TO_IF




PP_THREAD_SAFE FUNCTION RECORD_IS_LOWER_THAN_IF( THIS, OTHER, IS_LOWER_THAN, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A),          INTENT(INOUT) :: THIS
  CLASS(RECORD_BASE_A), POINTER, INTENT(IN)    :: OTHER
  LOGICAL,                       INTENT(OUT)   :: IS_LOWER_THAN
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_IS_LOWER_THAN_IF




PP_THREAD_SAFE FUNCTION RECORD_COPY_FROM_IF( THIS, OTHER, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A),          INTENT(INOUT) :: THIS
  CLASS(RECORD_BASE_A), POINTER, INTENT(IN)    :: OTHER
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_COPY_FROM_IF



PP_THREAD_SAFE FUNCTION RECORD_FREE_IF( THIS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Imported abstract class
  IMPORT :: RECORD_BASE_A

IMPLICIT NONE

  !> Dummy arguments
  CLASS(RECORD_BASE_A), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

END FUNCTION RECORD_FREE_IF



END INTERFACE

!> Whitelist of public symbols (Interfaces)
PUBLIC :: RECORD_BASE_A

END MODULE RECORD_BASE_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
