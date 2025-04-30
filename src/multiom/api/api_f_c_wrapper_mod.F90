! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'api_f_c_wrapper_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'API_F_C_WRAPPER_MOD'
MODULE API_F_C_WRAPPER_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

  !> Symbols imported from intrinsic modules.
  USE ISO_C_BINDING, ONLY: C_INT8_T
  USE ISO_C_BINDING, ONLY: C_PTR
  USE ISO_C_BINDING, ONLY: C_LONG_LONG
  USE ISO_C_BINDING, ONLY: C_SIZE_T
  USE ISO_C_BINDING, ONLY: C_NULL_PTR

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!> Interoperable type meant to be used as a container for fortran data
TYPE, BIND(C) :: F_C_WRAPPER_T
  INTEGER(C_LONG_LONG) :: OBJECT_ID=0_C_LONG_LONG
  INTEGER(C_SIZE_T) :: BUF_SIZE=0_C_LONG_LONG
  INTEGER(C_SIZE_T) :: OBJ_SIZE=0_C_LONG_LONG
  INTEGER(C_SIZE_T) :: HASH=0_C_LONG_LONG
  TYPE(C_PTR) :: BYTES=C_NULL_PTR
END TYPE

!> Dummy wrapper Used to compute size
TYPE(F_C_WRAPPER_T) :: DUMMY_WRAPPER

!> Shared parameters
INTEGER(KIND=JPIB_K), PARAMETER :: F_C_GUARD_SIZE = 8_JPIB_K
INTEGER(KIND=C_INT8_T), PARAMETER, DIMENSION(F_C_GUARD_SIZE) :: F_C_FRONT_GUARD = [1,2,3,4,5,6,7,8]
INTEGER(KIND=C_INT8_T), PARAMETER, DIMENSION(F_C_GUARD_SIZE) :: F_C_BACK_GUARD = [8,7,6,5,4,3,2,1]

!> Whitelist of public symbols
PUBLIC :: F_C_WRAPPER_T
PUBLIC :: F_C_ALLOCATE_WRAPPER
PUBLIC :: F_C_EXTRACT_WRAPPER
PUBLIC :: F_C_FREE_WRAPPER
PUBLIC :: F_C_GET_INFO_WRAPPER

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'F_C_ALLOCATE_WRAPPER'
PP_THREAD_SAFE FUNCTION F_C_ALLOCATE_WRAPPER( WRAPPER, BUFFER, OBJECT_ID, DIM, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE ISO_C_BINDING, ONLY: C_PTR
  USE ISO_C_BINDING, ONLY: C_INT
  USE ISO_C_BINDING, ONLY: C_INT8_T
  USE ISO_C_BINDING, ONLY: C_LONG_LONG
  USE ISO_C_BINDING, ONLY: C_SIZE_T
  USE ISO_C_BINDING, ONLY: C_F_POINTER
  USE ISO_C_BINDING, ONLY: C_NULL_PTR

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

  !> Dummy arguments
  TYPE(F_C_WRAPPER_T),    POINTER, DIMENSION(:), INTENT(INOUT) :: WRAPPER
  INTEGER(KIND=C_INT8_T), POINTER, DIMENSION(:), INTENT(INOUT) :: BUFFER
  INTEGER(KIND=JPIB_K),                          INTENT(IN)    :: OBJECT_ID
  INTEGER(KIND=JPIB_K),                          INTENT(IN)    :: DIM
  TYPE(HOOKS_T),                                 INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT8_T), POINTER, DIMENSION(:) :: LOC_BUFFER
  INTEGER(KIND=C_SIZE_T) :: SZ
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  TYPE(C_PTR) :: MEMORY_WRAPPER

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARRAY_ALREADY_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_SIZE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_WRAPPER_SIZE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLA_C_ALLOCATE_WRAPPER=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_NOT_ASSOCIATED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLA_C_ALLOCATE_ARRAY=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARRAY_NOT_ASSOCIATED=8_JPIB_K

  ! Local explicit interfaces
  INTERFACE
    FUNCTION C_ALLOCATE_WRAPPER(CPTR, SZ) RESULT(RET) BIND(C,NAME='c_allocate_wrapper')
      USE ISO_C_BINDING, ONLY: C_PTR
      USE ISO_C_BINDING, ONLY: C_INT
      USE ISO_C_BINDING, ONLY: C_SIZE_T
    IMPLICIT NONE
      TYPE(C_PTR),              INTENT(INOUT) :: CPTR
      INTEGER(C_SIZE_T), VALUE, INTENT(IN)    :: SZ
      INTEGER(C_INT) :: RET
    END FUNCTION C_ALLOCATE_WRAPPER
    FUNCTION C_ALLOCATE_BYTES(CPTR, SZ) RESULT(RET) BIND(C,NAME='c_allocate_bytes')
      USE ISO_C_BINDING, ONLY: C_PTR
      USE ISO_C_BINDING, ONLY: C_INT
      USE ISO_C_BINDING, ONLY: C_SIZE_T
    IMPLICIT NONE
      TYPE(C_PTR),              INTENT(INOUT) :: CPTR
      INTEGER(C_SIZE_T), VALUE, INTENT(IN)    :: SZ
      INTEGER(C_INT) :: RET
    END FUNCTION C_ALLOCATE_BYTES
  END INTERFACE

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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(WRAPPER), ERRFLAG_WRAPPER_ALREADY_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(BUFFER), ERRFLAG_ARRAY_ALREADY_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( DIM.LT.1, ERRFLAG_WRONG_SIZE )

  ! Compute size of the wrapper
  SZ = INT( STORAGE_SIZE(DUMMY_WRAPPER), KIND=C_SIZE_T )
  PP_DEBUG_CRITICAL_COND_THROW( SZ.LT.1_C_SIZE_T, ERRFLAG_WRONG_WRAPPER_SIZE )

  ! Allocate wrapper (needs to be done using c routines because in this way it can be deallocated from c)
  PP_TRYCALL(ERRFLA_C_ALLOCATE_WRAPPER) C_ALLOCATE_WRAPPER( MEMORY_WRAPPER, SZ )

  ! Get the fortran pointer to the c-object
  CALL C_F_POINTER( MEMORY_WRAPPER, WRAPPER, [1] )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(WRAPPER), ERRFLAG_WRAPPER_NOT_ASSOCIATED )

  ! Init wrapper
  WRAPPER(1)%OBJECT_ID = 0_C_LONG_LONG
  WRAPPER(1)%BUF_SIZE = 0_C_SIZE_T
  WRAPPER(1)%OBJ_SIZE = 0_C_SIZE_T
  WRAPPER(1)%HASH = 0_C_SIZE_T
  WRAPPER(1)%BYTES = C_NULL_PTR

  ! Set object ID
  WRAPPER(1)%OBJECT_ID = INT(OBJECT_ID, KIND=C_LONG_LONG)

  ! Set size of the array
  WRAPPER(1)%OBJ_SIZE = INT(DIM, KIND=C_SIZE_T)
  WRAPPER(1)%BUF_SIZE = INT(DIM, KIND=C_SIZE_T) + INT( 2*F_C_GUARD_SIZE, KIND=C_SIZE_T)

  ! Allocate the array
  PP_TRYCALL(ERRFLA_C_ALLOCATE_ARRAY) C_ALLOCATE_BYTES( WRAPPER(1)%BYTES, WRAPPER(1)%BUF_SIZE )

  ! Get the fortran pointer to the c-object
  LOC_BUFFER => NULL()
  CALL C_F_POINTER( WRAPPER(1)%BYTES, LOC_BUFFER, [WRAPPER(1)%BUF_SIZE] )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(LOC_BUFFER), ERRFLAG_ARRAY_NOT_ASSOCIATED )

  ! Initialize the array
  LOC_BUFFER = 0_C_INT8_T

  ! Copy the front guard
  LO = 1
  HI = F_C_GUARD_SIZE
  LOC_BUFFER(LO:HI) = F_C_FRONT_GUARD(1:F_C_GUARD_SIZE)

  ! Copy the back guard
  LO = WRAPPER(1)%BUF_SIZE - F_C_GUARD_SIZE + 1
  HI = WRAPPER(1)%BUF_SIZE
  LOC_BUFFER(LO:HI) = F_C_BACK_GUARD(1:F_C_GUARD_SIZE)

  ! Extract the pointer tot he output array
  LO = F_C_GUARD_SIZE + 1
  HI = WRAPPER(1)%BUF_SIZE - F_C_GUARD_SIZE
  PP_DEBUG_CRITICAL_COND_THROW( DIM.NE.HI-LO+1, ERRFLAG_WRONG_SIZE )
  BUFFER(1:DIM) => LOC_BUFFER(LO:HI)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_WRAPPER_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper already associated' )
    CASE (ERRFLAG_ARRAY_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Array already associated' )
    CASE (ERRFLAG_WRONG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong buffer size' )
    CASE (ERRFLAG_WRONG_WRAPPER_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong wrapper size' )
    CASE (ERRFLA_C_ALLOCATE_WRAPPER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'C allocate wrapper failed' )
    CASE (ERRFLAG_WRAPPER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper not associated after allocation' )
    CASE (ERRFLA_C_ALLOCATE_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'C allocate array failed' )
    CASE (ERRFLAG_ARRAY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Array not associated after allocation' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION F_C_ALLOCATE_WRAPPER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'F_C_EXTRACT_WRAPPER'
PP_THREAD_SAFE FUNCTION F_C_EXTRACT_WRAPPER( WRAPPER, BUFFER, DIM, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE ISO_C_BINDING, ONLY: C_PTR
  USE ISO_C_BINDING, ONLY: C_LONG_LONG
  USE ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE ISO_C_BINDING, ONLY: C_NULL_PTR
  USE ISO_C_BINDING, ONLY: C_LOC
  USE ISO_C_BINDING, ONLY: C_F_POINTER

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

  !> Dummy arguments
  TYPE(F_C_WRAPPER_T),    POINTER, DIMENSION(:), INTENT(IN)    :: WRAPPER
  INTEGER(KIND=C_INT8_T), POINTER, DIMENSION(:), INTENT(INOUT) :: BUFFER
  INTEGER(KIND=JPIB_K),                          INTENT(OUT)   :: DIM
  TYPE(HOOKS_T),                                 INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(C_PTR) :: MEMORY_WRAPPER
  TYPE(C_PTR) :: TMP
  INTEGER(KIND=C_INT8_T), POINTER, DIMENSION(:) :: LOC_BUFFER
  INTEGER(KIND=C_INT8_T), POINTER, DIMENSION(:) :: LOC_FRONT_GUARD
  INTEGER(KIND=C_INT8_T), POINTER, DIMENSION(:) :: LOC_BACK_GUARD
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARRAY_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_FRONT_GUARD=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRNG_BACK_GUARD=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_SIZE=5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(WRAPPER), ERRFLAG_WRAPPER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(WRAPPER(1)%BYTES), ERRFLAG_ARRAY_NOT_ASSOCIATED )

! Get the fortran pointer to the c-object
  LOC_BUFFER => NULL()
  CALL C_F_POINTER( WRAPPER(1)%BYTES, LOC_BUFFER, [WRAPPER(1)%BUF_SIZE] )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(LOC_BUFFER), ERRFLAG_ARRAY_NOT_ASSOCIATED )

  ! Copy the front guard
  LOC_FRONT_GUARD => NULL()
  LO = 1
  HI = F_C_GUARD_SIZE
  LOC_FRONT_GUARD(LO:HI) => LOC_BUFFER(LO:HI)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALL(LOC_FRONT_GUARD.EQ.F_C_FRONT_GUARD), ERRFLAG_WRONG_FRONT_GUARD )

  ! Copy the back guard
  LOC_BACK_GUARD => NULL()
  LO = WRAPPER(1)%BUF_SIZE - F_C_GUARD_SIZE + 1
  HI = WRAPPER(1)%BUF_SIZE
  LOC_BACK_GUARD(LO:HI) => LOC_BUFFER(LO:HI)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALL(LOC_BACK_GUARD.EQ.F_C_BACK_GUARD), ERRFLAG_WRNG_BACK_GUARD )

  ! Extract the pointer tot he output array
  LOC_BUFFER => NULL()
  LO = F_C_GUARD_SIZE + 1
  HI = WRAPPER(1)%BUF_SIZE - F_C_GUARD_SIZE
  DIM = WRAPPER(1)%OBJ_SIZE
  PP_DEBUG_CRITICAL_COND_THROW( DIM.NE.HI-LO+1, ERRFLAG_WRONG_SIZE )
  BUFFER(1:DIM) => LOC_BUFFER(LO:HI)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)

    CASE (ERRFLAG_WRAPPER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper not associated' )

    CASE (ERRFLAG_ARRAY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Array not associated' )

    CASE (ERRFLAG_WRONG_FRONT_GUARD)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong front guard' )

    CASE (ERRFLAG_WRNG_BACK_GUARD)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong back guard' )

    CASE (ERRFLAG_WRONG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong buffer size' )

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

  ! Exit point (on error)
  RETURN

END FUNCTION F_C_EXTRACT_WRAPPER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'F_C_FREE_WRAPPER'
PP_THREAD_SAFE FUNCTION F_C_FREE_WRAPPER( WRAPPER, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE ISO_C_BINDING, ONLY: C_PTR
  USE ISO_C_BINDING, ONLY: C_LONG_LONG
  USE ISO_C_BINDING, ONLY: C_SIZE_T
  USE ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE ISO_C_BINDING, ONLY: C_NULL_PTR
  USE ISO_C_BINDING, ONLY: C_LOC

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

  !> Dummy arguments
  TYPE(F_C_WRAPPER_T), POINTER, DIMENSION(:), INTENT(INOUT) :: WRAPPER
  TYPE(HOOKS_T),                              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(C_PTR) :: MEMORY_WRAPPER
  TYPE(C_PTR) :: TMP

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ARRAY_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATE_WRAPPER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATE_BYTES=4_JPIB_K

  ! Local explicit interfaces
  INTERFACE
    FUNCTION C_FREE_BYTES(CPTR) RESULT(RET)  BIND(C,NAME='c_free_bytes')
      USE :: ISO_C_BINDING, ONLY: C_PTR
      USE :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR), INTENT(INOUT) :: CPTR
      INTEGER(C_INT) :: RET
    END FUNCTION C_FREE_BYTES
    FUNCTION C_FREE_WRAPPER(CPTR) RESULT(RET) BIND(C,NAME='c_free_wrapper')
      USE :: ISO_C_BINDING, ONLY: C_PTR
      USE :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR), INTENT(INOUT) :: CPTR
      INTEGER(C_INT) :: RET
    END FUNCTION C_FREE_WRAPPER
  END INTERFACE

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(WRAPPER), ERRFLAG_WRAPPER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(WRAPPER(1)%BYTES), ERRFLAG_ARRAY_NOT_ASSOCIATED )

  ! Free the array
  PP_TRYCALL(ERRFLAG_DEALLOCATE_BYTES) C_FREE_BYTES( WRAPPER(1)%BYTES )

  ! Free the wrapper fields
  WRAPPER(1)%OBJECT_ID = 0_C_LONG_LONG
  WRAPPER(1)%BUF_SIZE = 0_C_SIZE_T
  WRAPPER(1)%OBJ_SIZE = 0_C_SIZE_T
  WRAPPER(1)%HASH = 0_C_SIZE_T
  WRAPPER(1)%BYTES = C_NULL_PTR

  ! Free the wrapper
  TMP = C_LOC(WRAPPER)
  PP_TRYCALL(ERRFLAG_DEALLOCATE_WRAPPER) C_FREE_WRAPPER( TMP )
  NULLIFY(WRAPPER)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)

    CASE (ERRFLAG_WRAPPER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper not associated' )

    CASE (ERRFLAG_ARRAY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Array not associated' )

    CASE (ERRFLAG_DEALLOCATE_WRAPPER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'C free wrapper failed' )

    CASE (ERRFLAG_DEALLOCATE_BYTES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'C free bytes failed' )

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

  ! Exit point (on error)
  RETURN

END FUNCTION F_C_FREE_WRAPPER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'F_C_GET_INFO_WRAPPER'
PP_THREAD_SAFE FUNCTION F_C_GET_INFO_WRAPPER( WRAPPER, OBJ_ID, OBJ_SZ, BUF_SZ, HASH, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE ISO_C_BINDING, ONLY: C_SIZE_T
  USE ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE ISO_C_BINDING, ONLY: C_LOC

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

  !> Dummy arguments
  TYPE(C_PTR),          INTENT(IN)    :: WRAPPER
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: OBJ_ID
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: OBJ_SZ
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: BUF_SZ
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: HASH
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_SIZE_T) :: C_OBJ_ID
  INTEGER(KIND=C_SIZE_T) :: C_OBJ_SZ
  INTEGER(KIND=C_SIZE_T) :: C_BUF_SZ
  INTEGER(KIND=C_SIZE_T) :: C_HASH

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_INFO=2_JPIB_K

  ! Local explicit interfaces
  INTERFACE
    FUNCTION C_GET_INFO(CPTR, OBJ_ID, OBJ_SZ, BUF_SZ, HASH ) RESULT(RET)  BIND(C,NAME='c_wrapper_get_info')
      USE :: ISO_C_BINDING, ONLY: C_PTR
      USE :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: CPTR
      TYPE(C_PTR), VALUE, INTENT(IN) :: OBJ_ID
      TYPE(C_PTR), VALUE, INTENT(IN) :: OBJ_SZ
      TYPE(C_PTR), VALUE, INTENT(IN) :: BUF_SZ
      TYPE(C_PTR), VALUE, INTENT(IN) :: HASH
      INTEGER(C_INT) :: RET
    END FUNCTION C_GET_INFO
  END INTERFACE

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(WRAPPER), ERRFLAG_WRAPPER_NOT_ASSOCIATED )

  ! Init c variables
  C_OBJ_ID = 0_C_SIZE_T
  C_OBJ_SZ = 0_C_SIZE_T
  C_BUF_SZ = 0_C_SIZE_T
  C_HASH = 0_C_SIZE_T

  ! Init output variables
  OBJ_ID = 0_JPIB_K
  OBJ_SZ = 0_JPIB_K
  BUF_SZ = 0_JPIB_K
  HASH = 0_JPIB_K

  ! Get info from c subroutine
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_INFO) C_GET_INFO( WRAPPER, &
&        C_LOC(OBJ_ID), C_LOC(C_OBJ_SZ), C_LOC(C_BUF_SZ), C_LOC(C_HASH) )

  ! Cast info to fortran types
  OBJ_ID = INT(C_OBJ_ID, KIND=JPIB_K)
  OBJ_SZ = INT(C_OBJ_SZ, KIND=JPIB_K)
  BUF_SZ = INT(C_BUF_SZ, KIND=JPIB_K)
  HASH   = INT(C_HASH, KIND=JPIB_K)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_WRAPPER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper not associated' )
    CASE (ERRFLAG_UNABLE_TO_GET_INFO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get info' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION F_C_GET_INFO_WRAPPER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE API_F_C_WRAPPER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
