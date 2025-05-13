! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'api_general_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'API_GENERAL_UTILS_MOD'
MODULE API_GENERAL_UTILS_MOD

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!> Interface to the c utilities
INTERFACE CONVERT_TO_C_STRING
  MODULE PROCEDURE CONVERT_F_STRING_SCALAR_TO_C_STRING_SCALAR
  MODULE PROCEDURE CONVERT_INT8_SCALAR_TO_C_STRING_SCALAR
  MODULE PROCEDURE CONVERT_INT16_SCALAR_TO_C_STRING_SCALAR
  MODULE PROCEDURE CONVERT_INT32_SCALAR_TO_C_STRING_SCALAR
  MODULE PROCEDURE CONVERT_INT64_SCALAR_TO_C_STRING_SCALAR
  MODULE PROCEDURE CONVERT_REAL32_SCALAR_TO_C_STRING_SCALAR
  MODULE PROCEDURE CONVERT_REAL64_SCALAR_TO_C_STRING_SCALAR

  MODULE PROCEDURE CONVERT_F_STRING_ARRAY_TO_C_STRING_SCALAR
  MODULE PROCEDURE CONVERT_INT8_ARRAY_TO_C_STRING_SCALAR
  MODULE PROCEDURE CONVERT_INT16_ARRAY_TO_C_STRING_SCALAR
  MODULE PROCEDURE CONVERT_INT32_ARRAY_TO_C_STRING_SCALAR
  MODULE PROCEDURE CONVERT_INT64_ARRAY_TO_C_STRING_SCALAR
  MODULE PROCEDURE CONVERT_REAL32_ARRAY_TO_C_STRING_SCALAR
  MODULE PROCEDURE CONVERT_REAL64_ARRAY_TO_C_STRING_SCALAR
END INTERFACE

!> Whitelist of public symbols
PUBLIC :: CONVERT_TO_C_STRING
PUBLIC :: ALLOCATE_ITERATOR
PUBLIC :: DEALLOCATE_ITERATOR
PUBLIC :: GRIB_MESSAGE_TO_C_CODES_HANDLE
PUBLIC :: COPY_F_BUFFER_TO_C_BUFFER

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_INT8_SCALAR_TO_C_STRING_SCALAR'
PP_THREAD_SAFE FUNCTION CONVERT_INT8_SCALAR_TO_C_STRING_SCALAR( VALUE, C_STR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=C_INT8_T), TARGET, INTENT(IN)    :: VALUE
  TYPE(C_PTR),                    INTENT(OUT)   :: C_STR
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  INTERFACE
    FUNCTION CONVERT_INT8_TO_C_STRING_C( F_VAL, C_STR ) RESULT(RET) BIND(C, NAME='convert_int8_to_cstring')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN)  :: F_VAL
      TYPE(C_PTR),        INTENT(OUT) :: C_STR
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION CONVERT_INT8_TO_C_STRING_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  ! Fill the C string
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) CONVERT_INT8_TO_C_STRING_C(C_LOC(VALUE), C_STR)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate c string' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_INT8_SCALAR_TO_C_STRING_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_INT8_ARRAY_TO_C_STRING_SCALAR'
PP_THREAD_SAFE FUNCTION CONVERT_INT8_ARRAY_TO_C_STRING_SCALAR( VALUE, C_STR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=C_INT8_T), DIMENSION(:), TARGET, INTENT(IN)    :: VALUE
  TYPE(C_PTR),                                  INTENT(OUT)   :: C_STR
  TYPE(HOOKS_T),                                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=C_INT) :: N

  INTERFACE
    FUNCTION CONVERT_INT8_ARRAY_TO_C_STRING_C( F_VAL, N, C_STR ) RESULT(RET) BIND(C, NAME='convert_int8_array_to_cstring')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
    IMPLICIT NONE
      TYPE(C_PTR),         VALUE, INTENT(IN)  :: F_VAL
      INTEGER(KIND=C_INT), VALUE, INTENT(IN)  :: N
      TYPE(C_PTR),                INTENT(OUT) :: C_STR
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION CONVERT_INT8_ARRAY_TO_C_STRING_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  ! Fill the C string
  N = SIZE(VALUE)
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) CONVERT_INT8_ARRAY_TO_C_STRING_C(C_LOC(VALUE), N, C_STR)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate c string' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_INT8_ARRAY_TO_C_STRING_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_INT16_SCALAR_TO_C_STRING_SCALAR'
PP_THREAD_SAFE FUNCTION CONVERT_INT16_SCALAR_TO_C_STRING_SCALAR( VALUE, C_STR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT16_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=C_INT16_T), TARGET, INTENT(IN)    :: VALUE
  TYPE(C_PTR),                     INTENT(OUT)   :: C_STR
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  INTERFACE
    FUNCTION CONVERT_INT16_TO_C_STRING_C( F_VAL, C_STR ) RESULT(RET) BIND(C, NAME='convert_int16_to_cstring')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN)  :: F_VAL
      TYPE(C_PTR),        INTENT(OUT) :: C_STR
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION CONVERT_INT16_TO_C_STRING_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  ! Fill the C string
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) CONVERT_INT16_TO_C_STRING_C(C_LOC(VALUE), C_STR)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate c string' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_INT16_SCALAR_TO_C_STRING_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_INT16_ARRAY_TO_C_STRING_SCALAR'
PP_THREAD_SAFE FUNCTION CONVERT_INT16_ARRAY_TO_C_STRING_SCALAR( VALUE, C_STR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT16_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=C_INT16_T), DIMENSION(:), TARGET, INTENT(IN)    :: VALUE
  TYPE(C_PTR),                                  INTENT(OUT)   :: C_STR
  TYPE(HOOKS_T),                                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=C_INT) :: N

  INTERFACE
    FUNCTION CONVERT_INT16_ARRAY_TO_C_STRING_C( F_VAL, N, C_STR ) RESULT(RET) BIND(C, NAME='convert_int16_array_to_cstring')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT16_T
    IMPLICIT NONE
      TYPE(C_PTR),         VALUE, INTENT(IN)  :: F_VAL
      INTEGER(KIND=C_INT), VALUE, INTENT(IN)  :: N
      TYPE(C_PTR),                INTENT(OUT) :: C_STR
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION CONVERT_INT16_ARRAY_TO_C_STRING_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  ! Fill the C string
  N = SIZE(VALUE)
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) CONVERT_INT16_ARRAY_TO_C_STRING_C(C_LOC(VALUE), N, C_STR)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate c string' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_INT16_ARRAY_TO_C_STRING_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_INT32_SCALAR_TO_C_STRING_SCALAR'
PP_THREAD_SAFE FUNCTION CONVERT_INT32_SCALAR_TO_C_STRING_SCALAR( VALUE, C_STR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT32_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=C_INT32_T), TARGET, INTENT(IN)    :: VALUE
  TYPE(C_PTR),                     INTENT(OUT)   :: C_STR
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  INTERFACE
    FUNCTION CONVERT_INT32_TO_C_STRING_C( F_VAL, C_STR ) RESULT(RET) BIND(C, NAME='convert_int32_to_cstring')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN)  :: F_VAL
      TYPE(C_PTR),        INTENT(OUT) :: C_STR
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION CONVERT_INT32_TO_C_STRING_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  ! Fill the C string
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) CONVERT_INT32_TO_C_STRING_C(C_LOC(VALUE), C_STR)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate c string' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_INT32_SCALAR_TO_C_STRING_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_INT32_ARRAY_TO_C_STRING_SCALAR'
PP_THREAD_SAFE FUNCTION CONVERT_INT32_ARRAY_TO_C_STRING_SCALAR( VALUE, C_STR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT32_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=C_INT32_T), DIMENSION(:), TARGET, INTENT(IN)    :: VALUE
  TYPE(C_PTR),                                  INTENT(OUT)   :: C_STR
  TYPE(HOOKS_T),                                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=C_INT) :: N

  INTERFACE
    FUNCTION CONVERT_INT32_ARRAY_TO_C_STRING_C( F_VAL, N, C_STR ) RESULT(RET) BIND(C, NAME='convert_int32_array_to_cstring')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT32_T
    IMPLICIT NONE
      TYPE(C_PTR),         VALUE, INTENT(IN)  :: F_VAL
      INTEGER(KIND=C_INT), VALUE, INTENT(IN)  :: N
      TYPE(C_PTR),                INTENT(OUT) :: C_STR
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION CONVERT_INT32_ARRAY_TO_C_STRING_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  ! Fill the C string
  N = SIZE(VALUE)
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) CONVERT_INT32_ARRAY_TO_C_STRING_C(C_LOC(VALUE), N, C_STR)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate c string' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_INT32_ARRAY_TO_C_STRING_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_INT64_SCALAR_TO_C_STRING_SCALAR'
PP_THREAD_SAFE FUNCTION CONVERT_INT64_SCALAR_TO_C_STRING_SCALAR( VALUE, C_STR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT64_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=C_INT64_T), TARGET, INTENT(IN)    :: VALUE
  TYPE(C_PTR),                     INTENT(OUT)   :: C_STR
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  INTERFACE
    FUNCTION CONVERT_INT64_TO_C_STRING_C( F_VAL, C_STR ) RESULT(RET) BIND(C, NAME='convert_int64_to_cstring')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN)  :: F_VAL
      TYPE(C_PTR),        INTENT(OUT) :: C_STR
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION CONVERT_INT64_TO_C_STRING_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  ! Fill the C string
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) CONVERT_INT64_TO_C_STRING_C(C_LOC(VALUE), C_STR)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate c string' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_INT64_SCALAR_TO_C_STRING_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_INT64_ARRAY_TO_C_STRING_SCALAR'
PP_THREAD_SAFE FUNCTION CONVERT_INT64_ARRAY_TO_C_STRING_SCALAR( VALUE, C_STR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT64_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=C_INT64_T), DIMENSION(:), TARGET, INTENT(IN)    :: VALUE
  TYPE(C_PTR),                                   INTENT(OUT)   :: C_STR
  TYPE(HOOKS_T),                                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=C_INT) :: N

  INTERFACE
    FUNCTION CONVERT_INT64_ARRAY_TO_C_STRING_C( F_VAL, N, C_STR ) RESULT(RET) BIND(C, NAME='convert_int64_array_to_cstring')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT64_T
    IMPLICIT NONE
      TYPE(C_PTR),         VALUE, INTENT(IN)  :: F_VAL
      INTEGER(KIND=C_INT), VALUE, INTENT(IN)  :: N
      TYPE(C_PTR),                INTENT(OUT) :: C_STR
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION CONVERT_INT64_ARRAY_TO_C_STRING_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  ! Fill the C string
  N = SIZE(VALUE)
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) CONVERT_INT64_ARRAY_TO_C_STRING_C(C_LOC(VALUE), N, C_STR)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate c string' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_INT64_ARRAY_TO_C_STRING_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_REAL32_SCALAR_TO_C_STRING_SCALAR'
PP_THREAD_SAFE FUNCTION CONVERT_REAL32_SCALAR_TO_C_STRING_SCALAR( VALUE, C_STR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_FLOAT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  REAL(KIND=C_FLOAT), TARGET, INTENT(IN)    :: VALUE
  TYPE(C_PTR),                INTENT(OUT)   :: C_STR
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  INTERFACE
    FUNCTION CONVERT_REAL32_TO_C_STRING_C( F_VAL, C_STR ) RESULT(RET) BIND(C, NAME='convert_real32_to_cstring')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN)  :: F_VAL
      TYPE(C_PTR),        INTENT(OUT) :: C_STR
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION CONVERT_REAL32_TO_C_STRING_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  ! Fill the C string
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) CONVERT_REAL32_TO_C_STRING_C(C_LOC(VALUE), C_STR)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate c string' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_REAL32_SCALAR_TO_C_STRING_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_REAL32_ARRAY_TO_C_STRING_SCALAR'
PP_THREAD_SAFE FUNCTION CONVERT_REAL32_ARRAY_TO_C_STRING_SCALAR( VALUE, C_STR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_FLOAT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  REAL(KIND=C_FLOAT), DIMENSION(:), TARGET, INTENT(IN)    :: VALUE
  TYPE(C_PTR),                              INTENT(OUT)   :: C_STR
  TYPE(HOOKS_T),                            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=C_INT) :: N

  INTERFACE
    FUNCTION CONVERT_REAL32_ARRAY_TO_C_STRING_C( F_VAL, N, C_STR ) RESULT(RET) BIND(C, NAME='convert_real32_array_to_cstring')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
    IMPLICIT NONE
      TYPE(C_PTR),         VALUE, INTENT(IN)  :: F_VAL
      INTEGER(KIND=C_INT), VALUE, INTENT(IN)  :: N
      TYPE(C_PTR),                INTENT(OUT) :: C_STR
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION CONVERT_REAL32_ARRAY_TO_C_STRING_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  ! Fill the C string
  N = SIZE(VALUE)
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) CONVERT_REAL32_ARRAY_TO_C_STRING_C(C_LOC(VALUE), N, C_STR)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate c string' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_REAL32_ARRAY_TO_C_STRING_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_REAL64_SCALAR_TO_C_STRING_SCALAR'
PP_THREAD_SAFE FUNCTION CONVERT_REAL64_SCALAR_TO_C_STRING_SCALAR( VALUE, C_STR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  REAL(KIND=C_DOUBLE), TARGET, INTENT(IN)    :: VALUE
  TYPE(C_PTR),                 INTENT(OUT)   :: C_STR
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  INTERFACE
    FUNCTION CONVERT_REAL64_TO_C_STRING_C( F_VAL, C_STR ) RESULT(RET) BIND(C, NAME='convert_real64_to_cstring')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN)  :: F_VAL
      TYPE(C_PTR),        INTENT(OUT) :: C_STR
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION CONVERT_REAL64_TO_C_STRING_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  ! Fill the C string
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) CONVERT_REAL64_TO_C_STRING_C(C_LOC(VALUE), C_STR)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate c string' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_REAL64_SCALAR_TO_C_STRING_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_REAL64_ARRAY_TO_C_STRING_SCALAR'
PP_THREAD_SAFE FUNCTION CONVERT_REAL64_ARRAY_TO_C_STRING_SCALAR( VALUE, C_STR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  REAL(KIND=C_DOUBLE), DIMENSION(:), TARGET, INTENT(IN)    :: VALUE
  TYPE(C_PTR),                               INTENT(OUT)   :: C_STR
  TYPE(HOOKS_T),                             INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=C_INT) :: N

  INTERFACE
    FUNCTION CONVERT_REAL64_ARRAY_TO_C_STRING_C( F_VAL, N, C_STR ) RESULT(RET) BIND(C, NAME='convert_real64_array_to_cstring')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
    IMPLICIT NONE
      TYPE(C_PTR),         VALUE, INTENT(IN)  :: F_VAL
      INTEGER(KIND=C_INT), VALUE, INTENT(IN)  :: N
      TYPE(C_PTR),                INTENT(OUT) :: C_STR
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION CONVERT_REAL64_ARRAY_TO_C_STRING_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  ! Fill the C string
  N = SIZE(VALUE)
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) CONVERT_REAL64_ARRAY_TO_C_STRING_C(C_LOC(VALUE), N, C_STR)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate c string' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_REAL64_ARRAY_TO_C_STRING_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_F_STRING_SCALAR_TO_C_STRING_SCALAR'
PP_THREAD_SAFE FUNCTION CONVERT_F_STRING_SCALAR_TO_C_STRING_SCALAR( F_STR, C_STR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_PTR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_CHAR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: F_STR
  TYPE(C_PTR),          INTENT(OUT)   :: C_STR
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET
  INTEGER(KIND=JPIB_K) :: I

  !> Local variables
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(F_STR) + 1), TARGET :: TEMP

  INTERFACE
    FUNCTION CONVERT_FSTRING_TO_CSTRING_C(F_STR, C_STR) RESULT(RET) BIND(C, NAME='convert_fstring_to_cstring')
      USE, intrinsic :: ISO_C_BINDING, only : C_PTR
      USE, intrinsic :: ISO_C_BINDING, only : C_INT
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN)  :: F_STR
      TYPE(C_PTR),        INTENT(OUT) :: C_STR
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION CONVERT_FSTRING_TO_CSTRING_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  DO I=1,LEN_TRIM(F_STR)
    TEMP(I) = F_STR(I:I)
  END DO

  TEMP(SIZE(TEMP)) = C_NULL_CHAR

  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) CONVERT_FSTRING_TO_CSTRING_C( C_LOC(TEMP), C_STR)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate c string' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_F_STRING_SCALAR_TO_C_STRING_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_F_STRING_ARRAY_TO_C_STRING_SCALAR'
PP_THREAD_SAFE FUNCTION CONVERT_F_STRING_ARRAY_TO_C_STRING_SCALAR( F_STR, C_STR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_INT
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_PTR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_CHAR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*), DIMENSION(:), INTENT(IN)    :: F_STR
  TYPE(C_PTR),                    INTENT(OUT)   :: C_STR
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: FOUND
  INTEGER(KIND=C_INT), POINTER, DIMENSION(:) :: ARRAY_SIZE
  INTEGER(KIND=C_INT) :: N
  INTEGER(KIND=C_INT) :: M
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: K
  INTEGER(KIND=JPIB_K) :: SZ
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(:), POINTER :: TEMP
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  INTERFACE
    FUNCTION CONVERT_FSTRING_ARRAY_TO_CSTRING_C( VALUE, C_STRING ) RESULT(RET) BIND(C,NAME='convert_fstring_to_cstring')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN)  :: VALUE
      TYPE(C_PTR),        INTENT(OUT) :: C_STRING
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION CONVERT_FSTRING_ARRAY_TO_CSTRING_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FILL_CSTR=3_JPIB_K

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

  ! Count the characters
  SZ = SIZE(F_STR)
  DO I=1,SIZE(F_STR)
    SZ = SZ + LEN_TRIM(ADJUSTL(F_STR(I)))
  END DO

  ! Allocate the temporary array
  ALLOCATE( TEMP(SZ), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE )

  ! Fill the temporary array
  CNT = 0
  DO I = 1, SIZE(F_STR)
    FOUND = .FALSE.
    DO J = 1, LEN_TRIM(F_STR(I))
      IF ( F_STR(I)(J:J) .NE. ' ' ) THEN
        FOUND = .TRUE.
      ENDIF
      IF ( FOUND ) THEN
        CNT = CNT + 1_JPIB_K
        TEMP(CNT) = F_STR(I)(J:J)
      ENDIF
    ENDDO
    CNT = CNT + 1_JPIB_K
    TEMP(J) = C_NULL_CHAR
  ENDDO

  ! Fill the C string array
  N = SIZE(F_STR)
  M = SIZE(TEMP)
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FILL_CSTR) CONVERT_FSTRING_ARRAY_TO_CSTRING_C(C_LOC(TEMP), C_STR )

  ! Deallocate the temporary array
  DEALLOCATE( TEMP, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate the temporary array' )
      IF( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS)
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not deallocate the temporary array' )
      IF( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS)
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_FILL_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not fill the C string array' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_F_STRING_ARRAY_TO_C_STRING_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CONVERT_F_STRING_ARRAY_TO_C_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION CONVERT_F_STRING_ARRAY_TO_C_STRING_ARRAY( F_STR, C_STR, C_SZ, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_INT
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_PTR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_CHAR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*), DIMENSION(:), INTENT(IN)    :: F_STR
  TYPE(C_PTR),                    INTENT(OUT)   :: C_STR
  TYPE(C_PTR), VALUE,             INTENT(IN)    :: C_SZ
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: FOUND
  INTEGER(KIND=C_INT), POINTER, DIMENSION(:) :: ARRAY_SIZE
  INTEGER(KIND=C_INT) :: N
  INTEGER(KIND=C_INT) :: M
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: K
  INTEGER(KIND=JPIB_K) :: SZ
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(:), POINTER :: TEMP
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  INTERFACE
    FUNCTION FILLCSTR_ARRAY_C( VALUE, N, M, C_STRING ) RESULT(RET) BIND(C,NAME='convert_fstring_array_to_cstring_array')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR),         VALUE, INTENT(IN)  :: VALUE
      INTEGER(KIND=C_INT), VALUE, INTENT(IN)  :: N
      INTEGER(KIND=C_INT), VALUE, INTENT(IN)  :: M
      TYPE(C_PTR),                INTENT(OUT) :: C_STRING
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION FILLCSTR_ARRAY_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FILL_CSTR=3_JPIB_K

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

  ! Count the characters
  SZ = SIZE(F_STR)
  DO I=1,SIZE(F_STR)
    SZ = SZ + LEN_TRIM(ADJUSTL(F_STR(I)))
  END DO

  ! Allocate the temporary array
  ALLOCATE( TEMP(SZ), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE )

  ! Fill the temporary array
  CNT = 0
  DO I = 1, SIZE(F_STR)
    FOUND = .FALSE.
    DO J = 1, LEN_TRIM(F_STR(I))
      IF ( F_STR(I)(J:J) .NE. ' ' ) THEN
        FOUND = .TRUE.
      ENDIF
      IF ( FOUND ) THEN
        CNT = CNT + 1_JPIB_K
        TEMP(CNT) = F_STR(I)(J:J)
      ENDIF
    ENDDO
    CNT = CNT + 1_JPIB_K
    TEMP(J) = C_NULL_CHAR
  ENDDO

  ! Fill the C string array
  N = SIZE(F_STR)
  M = SIZE(TEMP)
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FILL_CSTR) FILLCSTR_ARRAY_C(C_LOC(TEMP), N, M, C_STR)

  ! Deallocate the temporary array
  DEALLOCATE( TEMP, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE )

  ! Fill the size
  CALL C_F_POINTER( C_SZ, ARRAY_SIZE, [1] )
  ARRAY_SIZE(1) = N

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate the temporary array' )
      IF( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS)
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not deallocate the temporary array' )
      IF( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS)
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_FILL_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not fill the C string array' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION CONVERT_F_STRING_ARRAY_TO_C_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ALLOCATE_ITERATOR'
PP_THREAD_SAFE FUNCTION ALLOCATE_ITERATOR( ITERATOR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_INT
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_PTR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_F_POINTER

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=C_INT), DIMENSION(:), POINTER, INTENT(INOUT) :: ITERATOR
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(C_PTR) :: C_ITERATOR

  INTERFACE
    FUNCTION ALLOCATE_ITERATOR_C(C_IT) RESULT(RET) BIND(C, NAME='allocate_iterator')
      USE, intrinsic :: ISO_C_BINDING, only : C_PTR
      USE, intrinsic :: ISO_C_BINDING, only : C_INT
    IMPLICIT NONE
      TYPE(C_PTR), INTENT(INOUT) :: C_IT
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION ALLOCATE_ITERATOR_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  ! Allocate the iterator
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) ALLOCATE_ITERATOR_C( C_ITERATOR )

  ! Associate the pointer
  CALL C_F_POINTER( C_ITERATOR, ITERATOR, [1] )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate the iterator' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION ALLOCATE_ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DEALLOCATE_ITERATOR'
PP_THREAD_SAFE FUNCTION DEALLOCATE_ITERATOR( ITERATOR, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_INT
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_PTR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=C_INT), DIMENSION(:), POINTER, INTENT(INOUT) :: ITERATOR
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  INTERFACE
    FUNCTION FREE_ITERATOR_C(C_IT) RESULT(RET) BIND(C, NAME='free_iterator')
      USE, intrinsic :: ISO_C_BINDING, only : C_PTR
      USE, intrinsic :: ISO_C_BINDING, only : C_INT
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: C_IT
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION FREE_ITERATOR_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ASSOCIATED=2_JPIB_K

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

  ! Check iterator status
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(ITERATOR), ERRFLAG_NOT_ASSOCIATED )

  ! Allocate the iterator
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) FREE_ITERATOR_C( C_LOC(ITERATOR) )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate the iterator' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION DEALLOCATE_ITERATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB_MESSAGE_TO_C_CODES_HANDLE'
PP_THREAD_SAFE FUNCTION GRIB_MESSAGE_TO_C_CODES_HANDLE( F_BUF, C_CODES_HANDLE_LOC, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_INT
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_PTR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_CHAR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=1), DIMENSION(:), TARGET, INTENT(IN)    :: F_BUF
  TYPE(C_PTR),                            INTENT(OUT)   :: C_CODES_HANDLE_LOC
  TYPE(HOOKS_T),                          INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=C_INT) :: N
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: SZ
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(:), POINTER :: TEMP
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  INTERFACE
    FUNCTION SET_CODES_HANDLE( VALUE, N, C_CODES_HANDLE_LOC ) RESULT(RET) BIND(C,NAME='set_codes_handle_c')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR),         VALUE, INTENT(IN)  :: VALUE
      INTEGER(KIND=C_INT), VALUE, INTENT(IN)  :: N
      TYPE(C_PTR),                INTENT(OUT) :: C_CODES_HANDLE_LOC
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION SET_CODES_HANDLE
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_SET_CODES_HANDLE=3_JPIB_K

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

  ! Count the characters
  SZ = SIZE(F_BUF)

  ! Allocate the temporary array
  ALLOCATE( TEMP(SZ), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE )

  ! Fill the temporary array
  DO I = 1, SIZE(F_BUF)
    TEMP(I) = F_BUF(I)
  ENDDO

  ! Fill the C string array
  N = SIZE(TEMP)
  PP_TRYCALL(ERRFLAG_UNABLE_SET_CODES_HANDLE) SET_CODES_HANDLE(C_LOC(TEMP), N, C_CODES_HANDLE_LOC)

  ! Deallocate the temporary array
  DEALLOCATE( TEMP, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate the temporary array' )
      IF( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS)
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not deallocate the temporary array' )
      IF( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STATUS)
      ENDIF
    CASE (ERRFLAG_UNABLE_SET_CODES_HANDLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not create an eccodes handle' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION GRIB_MESSAGE_TO_C_CODES_HANDLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COPY_F_BUFFER_TO_C_BUFFER'
PP_THREAD_SAFE FUNCTION COPY_F_BUFFER_TO_C_BUFFER( F_BUF, C_BUF, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_PTR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_INT
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_CHAR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, intrinsic :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(:), INTENT(IN)    :: F_BUF
  TYPE(C_PTR),                                 INTENT(OUT)   :: C_BUF
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET
  INTEGER(KIND=JPIB_K) :: I

  !> Local variables
  INTEGER(KIND=C_INT)  :: BUF_SIZE

  INTERFACE
    FUNCTION COPY_F_BUFFER_TO_C_BUFFER_C(F_BUF, BUF_SIZE, C_BUF) RESULT(RET) BIND(C, NAME='copy_f_buf_to_c_buf_c')
      USE, intrinsic :: ISO_C_BINDING, only : C_PTR
      USE, intrinsic :: ISO_C_BINDING, only : C_INT
    IMPLICIT NONE
      TYPE(C_PTR),         VALUE, INTENT(IN)  :: F_BUF
      INTEGER(KIND=C_INT), VALUE, INTENT(IN)  :: BUF_SIZE
      TYPE(C_PTR),                INTENT(OUT) :: C_BUF
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION COPY_F_BUFFER_TO_C_BUFFER_C
  END INTERFACE

  !> Errorflags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOC_CSTR=1_JPIB_K

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

  BUF_SIZE=INT(SIZE(F_BUF), KIND=C_INT)

  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOC_CSTR) COPY_F_BUFFER_TO_C_BUFFER_C( C_LOC(F_BUF), BUF_SIZE, C_BUF)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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

    CASE (ERRFLAG_UNABLE_TO_ALLOC_CSTR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Can not allocate c string' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point on error
  RETURN

END FUNCTION COPY_F_BUFFER_TO_C_BUFFER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE API_GENERAL_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
