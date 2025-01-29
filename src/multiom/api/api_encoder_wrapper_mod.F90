! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'api_encoder_wrapper_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'API_ENCODER_WRAPPER_MOD'
MODULE API_ENCODER_WRAPPER_MOD

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

! Whitelist of public symbols (encoder management)
PUBLIC :: MULTIO_GRIB2_ENCODER_OPEN
PUBLIC :: MULTIO_GRIB2_ENCODER_CLOSE
! PUBLIC :: MULTIO_GRIB2_ENCODER_ENCODE64
! PUBLIC :: MULTIO_GRIB2_ENCODER_ENCODE32
PUBLIC :: MULTIO_GRIB2_ENCODER_ENCODE

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_ENCODER_OPEN'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_ENCODER_OPEN( OPTIONS, MULTIO_GRIB2 ) &
 BIND(C,NAME='multio_grib2_encoder_open') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: MAP_INT64_ENCODER_MOD,      ONLY: MAP_FUNCTION_INT64_ENCODER_IF
  USE :: MULTIOM_CACHED_ENCODER_MOD, ONLY: MULTIOM_CACHED_ENCODERS_T
  USE :: API_SHARED_DATA_MOD,        ONLY: SHARED_ENCODER_MAP
  USE :: API_SHARED_DATA_MOD,        ONLY: FREE_ENCODER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR), VALUE, INTENT(IN)    :: OPTIONS
  TYPE(C_PTR),        INTENT(INOUT) :: MULTIO_GRIB2

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  LOGICAL :: HAS_OPTIONS
  LOGICAL :: INITIALIZED
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:) :: F_MULTIO_GRIB2
  INTEGER(KIND=JPIB_K) :: SIZE
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:) :: MAX_ENCODER_HANDLE
  TYPE(MULTIOM_CACHED_ENCODERS_T), POINTER :: ENCODER
  PROCEDURE(MAP_FUNCTION_INT64_ENCODER_IF), POINTER :: ENCODER_DESTRUCTOR
  TYPE(HOOKS_T) :: HOOKS

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOC_FAILURE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ADD_ENCODER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_MAP=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SIZE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_MAXIMUM=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TOCHECK_INITIALIZATION_STATUS=7_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Initialization of the hooks
  CALL HOOKS%DEBUG_HOOK_%INIT( )

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( C_ASSOCIATED(MULTIO_GRIB2), ERRFLAG_ENCODER_ALREADY_ASSOCIATED )

  !> Check if the options are present
  HAS_OPTIONS = .NOT.C_ASSOCIATED( OPTIONS )

  !> Get the encoder handle from the c pointer
  ALLOCATE( F_MULTIO_GRIB2(2), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_ALLOC_FAILURE )

  !> Check if the encoder is already in the encoders map
  PP_TRYCALL(ERRFLAG_UNABLE_TOCHECK_INITIALIZATION_STATUS) SHARED_ENCODER_MAP%INITIALIZED( INITIALIZED, HOOKS )

  !> Conditionally initialized the encoders map
!$omp single
  IF ( .NOT. INITIALIZED ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_MAP) SHARED_ENCODER_MAP%INIT( HOOKS )
  ENDIF
!$omp end single

!$omp critical(API_ENCODER_MAP_INSERT)
  !> Get the size of the encoder map
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SIZE) SHARED_ENCODER_MAP%SIZE( SIZE, HOOKS )

  IF ( SIZE .EQ. 0_JPIB_K ) THEN
    F_MULTIO_GRIB2(1) = 1_C_LONG_LONG
    F_MULTIO_GRIB2(2) = 1_C_LONG_LONG
  ELSE
    !> Check if the encoder is already in the encoders map
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_MAXIMUM) SHARED_ENCODER_MAP%MAX( MAX_ENCODER_HANDLE, HOOKS )

    !> Increment the encoder handle
    F_MULTIO_GRIB2(1) = 1_C_LONG_LONG
    F_MULTIO_GRIB2(2) = MAX_ENCODER_HANDLE(2) + 1_C_LONG_LONG
  ENDIF

  !> Allocate the encoder
  ALLOCATE( ENCODER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_ALLOC_FAILURE )

  ENCODER_DESTRUCTOR => FREE_ENCODER
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ADD_ENCODER) SHARED_ENCODER_MAP%INSERT( F_MULTIO_GRIB2, ENCODER, ENCODER_DESTRUCTOR, HOOKS )

  !> Get the location of the encoder
  MULTIO_GRIB2 = C_LOC( F_MULTIO_GRIB2 )
!$omp end critical(API_ENCODER_MAP_INSERT)

  !> Be sure we don't have any memory leaks
  CALL HOOKS%DEBUG_HOOK_%FREE( )

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

    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ENCODER_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder already associated' )
    CASE (ERRFLAG_ALLOC_FAILURE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Allocation failure' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=ALLOC_STATUS)
      END IF
    CASE (ERRFLAG_UNABLE_TO_INIT_MAP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize the map' )
    CASE (ERRFLAG_UNABLE_TO_GET_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the size of the map' )
    CASE (ERRFLAG_UNABLE_TO_GET_MAXIMUM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the maximum of the map' )
    CASE (ERRFLAG_UNABLE_TO_ADD_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to add the encoder to the map' )
    CASE (ERRFLAG_UNABLE_TOCHECK_INITIALIZATION_STATUS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check the initialization status of the map' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Print the error stack
    ! NOTE: This is importent when c is calling this function. Is opens the error_unit
    WRITE(ERROR_UNIT,*) ' PRINT ERROR STACK FROM: "'//__FILE__//'":', __LINE__
    CALL HOOKS%DEBUG_HOOK_%PRINT_ERROR_STACK( ERROR_UNIT )

    ! Free the error stack
    CALL HOOKS%DEBUG_HOOK_%FREE( )

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  RETURN

END FUNCTION MULTIO_GRIB2_ENCODER_OPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_ENCODER_CLOSE'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_ENCODER_CLOSE( MULTIO_GRIB2 ) &
 BIND(C,NAME='multio_grib2_encoder_close') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: API_SHARED_DATA_MOD, ONLY: SHARED_ENCODER_MAP

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR), VALUE, INTENT(IN) :: MULTIO_GRIB2

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  TYPE(C_PTR), POINTER, DIMENSION(:) :: TMP
  LOGICAL :: INITIALIZED
  LOGICAL :: MAP_HAS_ENCODER
  LOGICAL :: ENCODER_REMOVED
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  INTEGER(KIND=JPIB_K) :: MAP_SIZE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:) :: F_MULTIO_GRIB2
  TYPE(HOOKS_T) :: HOOKS

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAP_NOT_INITIALIZED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_ENCODER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_REMOVE_ENCODER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAP_LIST=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_NOT_ASSOCIATED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOC_FAILURE=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_MAP=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SIZE=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_HANDLE=10_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Initialization of the hooks
  CALL HOOKS%DEBUG_HOOK_%INIT( )

!$omp critical(API_ENCODER_MAP_REMOVE)
  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(MULTIO_GRIB2), ERRFLAG_ENCODER_NOT_ASSOCIATED )

  !> Check if the encoder is already in the encoders map
  PP_TRYCALL(ERRFLAG_MAP_NOT_INITIALIZED) SHARED_ENCODER_MAP%INITIALIZED( INITIALIZED, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. INITIALIZED, ERRFLAG_MAP_NOT_INITIALIZED )

  !> Print the encoder map
  !> NOTE: To be uncommented for debugging the API
  ! PP_TRYCALL(ERRFLAG_MAP_LIST) SHARED_ENCODER_MAP%LIST( 6_JPIB_K, 'ENCODERS_MAP: ', HOOKS )
  TMP => NULL()
  CALL C_F_POINTER( MULTIO_GRIB2, TMP, [1] )

  !> Get th fortran handle from the c handle
  F_MULTIO_GRIB2 => NULL()
  CALL C_F_POINTER( TMP(1), F_MULTIO_GRIB2, [2] )

  !> Check the allocation status of the fortran handle
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(F_MULTIO_GRIB2), ERRFLAG_KEY_NOT_ASSOCIATED )

  !> Check that the handle is a proper encoder handle
  PP_DEBUG_CRITICAL_COND_THROW( F_MULTIO_GRIB2(1).NE.1, ERRFLAG_WRONG_HANDLE )

  !> Check if the handle is associated to an encoder
  PP_TRYCALL(ERRFLAG_MATCH_ENCODER) SHARED_ENCODER_MAP%HAS( F_MULTIO_GRIB2, MAP_HAS_ENCODER, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. MAP_HAS_ENCODER, ERRFLAG_MATCH_ENCODER )

  !> Remove the encoder from the map
  PP_TRYCALL(ERRFLAG_REMOVE_ENCODER) SHARED_ENCODER_MAP%REMOVE( F_MULTIO_GRIB2, ENCODER_REMOVED, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ENCODER_REMOVED, ERRFLAG_REMOVE_ENCODER )

  !> Print the encoder map
  !> NOTE: To be uncommented for debugging the API
  ! PP_TRYCALL(ERRFLAG_MAP_LIST) SHARED_ENCODER_MAP%LIST( 6_JPIB_K, 'ENCODERS_MAP: ', HOOKS )

  !> Get the map size
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SIZE) SHARED_ENCODER_MAP%SIZE( MAP_SIZE, HOOKS )

  !> To avoid the need of init/exit API, every time the map is empty we deallocate the map
  IF ( MAP_SIZE .EQ. 0_JPIB_K ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_MAP) SHARED_ENCODER_MAP%FREE( HOOKS )
  ENDIF

!$omp end critical(API_ENCODER_MAP_REMOVE)

  ! Reset the input pointer
  TMP(1) = C_NULL_PTR

  !> Be sure we don't have any memory leaks
  CALL HOOKS%DEBUG_HOOK_%FREE( )

  ! Exit point (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    CHARACTER(LEN=32) :: CTMP

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ENCODER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder not associated' )
    CASE (ERRFLAG_KEY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key not associated' )
    CASE (ERRFLAG_MAP_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Map not initialized' )
    CASE (ERRFLAG_MATCH_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to match the encoder' )
    CASE (ERRFLAG_REMOVE_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to remove the encoder' )
    CASE (ERRFLAG_MAP_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to list the map' )
    CASE (ERRFLAG_DEALLOC_FAILURE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key deallocation failure' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STAT)
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_FREE_MAP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free the map' )
    CASE (ERRFLAG_UNABLE_TO_GET_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the size of the map' )
    CASE (ERRFLAG_WRONG_HANDLE)
      CTMP = REPEAT(' ', 32)
      WRITE(CTMP, *, IOSTAT=DEALLOC_STAT) F_MULTIO_GRIB2(1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong handle (handle is not from an encoder)' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle is: '//TRIM(ADJUSTL(CTMP)) )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Print the error stack
    ! NOTE: This is importent when c is calling this function. Is opens the error_unit
    WRITE(ERROR_UNIT,*) ' PRINT ERROR STACK FROM: "'//__FILE__//'":', __LINE__
    CALL HOOKS%DEBUG_HOOK_%PRINT_ERROR_STACK( ERROR_UNIT )

    ! Free the error stack
    CALL HOOKS%DEBUG_HOOK_%FREE( )

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  RETURN

END FUNCTION MULTIO_GRIB2_ENCODER_CLOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



! #define PP_PROCEDURE_TYPE 'FUNCTION'
! #define PP_PROCEDURE_NAME 'MULTIO_GRIB2_ENCODER_ENCODE64'
! PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_ENCODER_ENCODE64( MULTIO_GRIB2, MARS_DICT, PAR_DICT, VALUES, DATA_LEN, GRIB_HANDLE ) &
!  BIND(C,NAME='multio_grib2_encoder_encode64') RESULT(RET)

!   !> Symbols imported from intrinsic modules.
!   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE
!   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
!   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
!   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG
!   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
!   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
!   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
!   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER

!   ! Symbols imported from other modules within the project.
!   USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
!   USE :: HOOKS_MOD,         ONLY: HOOKS_T

!   ! Symbols imported by the preprocessor for debugging purposes
!   PP_DEBUG_USE_VARS

!   ! Symbols imported by the preprocessor for logging purposes
!   PP_LOG_USE_VARS

!   ! Symbols imported by the preprocessor for tracing purposes
!   PP_TRACE_USE_VARS

! IMPLICIT NONE

!   !> Dummy arguments
!   TYPE(C_PTR),          VALUE, INTENT(IN) :: MULTIO_GRIB2
!   TYPE(C_PTR),          VALUE, INTENT(IN) :: MARS_DICT
!   TYPE(C_PTR),          VALUE, INTENT(IN) :: PAR_DICT
!   TYPE(C_PTR),          VALUE, INTENT(IN) :: VALUES
!   INTEGER(KIND=C_LONG), VALUE, INTENT(IN) :: DATA_LEN
!   TYPE(C_PTR),          VALUE, INTENT(IN) :: GRIB_HANDLE

!   !> Function result
!   INTEGER(KIND=C_INT) :: RET

!   !> Local variables
!   REAL(KIND=C_DOUBLE), DIMENSION(:), POINTER :: VALUES_ARRAY32
!   INTEGER(KIND=C_LONG_LONG), POINTER :: ENCODER_ID
!   INTEGER(KIND=C_LONG_LONG), POINTER :: MARS_ID
!   INTEGER(KIND=C_LONG_LONG), POINTER :: PAR_ID

!   !> Local error flags
!   INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_DATA_LEN=1_JPIB_K
!   INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_IS_NULL_POINTER=2_JPIB_K
!   INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_GRIB2_IS_NULL_POINTER=3_JPIB_K
!   INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_DICT_IS_NULL_POINTER=4_JPIB_K
!   INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PAR_DICT_IS_NULL_POINTER=5_JPIB_K
!   INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_HANDLE_IS_NULL_POINTER=6_JPIB_K

!   ! Local variables declared by the preprocessor for debugging purposes
!   PP_DEBUG_DECL_VARS

!   ! Local variables declared by the preprocessor for logging purposes
!   PP_LOG_DECL_VARS

!   ! Local variables declared by the preprocessor for tracing purposes
!   PP_TRACE_DECL_VARS

!   ! Trace begin of procedure
!   PP_TRACE_ENTER_PROCEDURE()

!   ! Initialization of good path return value
!   RET = 0_C_INT

!   !> Error handling
!   PP_DEBUG_CRITICAL_COND_THROW( DATA_LEN .LE. 0, ERRFLAG_WRONG_DATA_LEN )
!   PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(VALUES), ERRFLAG_VALUES_IS_NULL_POINTER )
!   PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(MULTIO_GRIB2), ERRFLAG_MULTIO_GRIB2_IS_NULL_POINTER )
!   PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(MARS_DICT), ERRFLAG_MARS_DICT_IS_NULL_POINTER )
!   PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(PAR_DICT), ERRFLAG_PAR_DICT_IS_NULL_POINTER )
!   PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(GRIB_HANDLE), ERRFLAG_GRIB_HANDLE_IS_NULL_POINTER )

!   !> Cast the VALUES pointer to a real array
!   CALL C_F_POINTER( VALUES, VALUES_ARRAY32, [DATA_LEN] )

!   !> Get encoder handle from the c pointer
!   CALL C_F_POINTER( MULTIO_GRIB2, ENCODER_ID )

!   !> Get mars dictionary handle from the c pointer
!   CALL C_F_POINTER( MARS_DICT, MARS_ID )

!   !> Parameterization handle from the c pointer
!   CALL C_F_POINTER( PAR_DICT, PAR_ID )

!   !>

!   ! Trace end of procedure (on success)
!   PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

!   ! Exit point (On success)
!   RETURN

! ! Error handler
! PP_ERROR_HANDLER

!   ! Initialization of bad path return value
!   PP_SET_ERR_FAILURE( RET )

!   ! TODO: Add error handling code here

!   RETURN

! END FUNCTION MULTIO_GRIB2_ENCODER_ENCODE64
! #undef PP_PROCEDURE_NAME
! #undef PP_PROCEDURE_TYPE


! #define PP_PROCEDURE_TYPE 'FUNCTION'
! #define PP_PROCEDURE_NAME 'MULTIO_GRIB2_ENCODER_ENCODE32'
! PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_ENCODER_ENCODE32( MULTIO_GRIB2, MARS_DICT, PAR_DICT, VALUES, DATA_LEN, GRIB_HANDLE ) &
!  BIND(C,NAME='multio_grib2_encoder_encode32') RESULT(RET)

!   !> Symbols imported from intrinsic modules.
!   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_FLOAT
!   USE, INTRINSIC :: ISO_C_BINDING, ONLY, INTRINSIC :: ISO_C_BINDING, ONLY, INTRINSIC :: ISO_C_BINDING
!   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
!   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
!   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
!   USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER

!   ! Symbols imported from other modules within the project.
!   USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
!   USE :: HOOKS_MOD,         ONLY: HOOKS_T

!   ! Symbols imported by the preprocessor for debugging purposes
!   PP_DEBUG_USE_VARS

!   ! Symbols imported by the preprocessor for logging purposes
!   PP_LOG_USE_VAR       S

!   ! Symbols impo       rted by the preprocessor for tracing purposes
!   PP_TRACE_USE_VARS

! IMPLICIT NONE

!   !> Dummy arguments
!   TYPE(C_PTR),          VALUE, INTENT(IN) :: MULTIO_GRIB2
!   TYPE(C_PTR),          VALUE, INTENT(IN) :: MARS_DICT
!   TYPE(C_PTR),          VALUE, INTENT(IN) :: PAR_DICT
!   TYPE(C_PTR),          VALUE, INTENT(IN) :: VALUES
!   INTEGER(KIND=C_LONG), VALUE, INTENT(IN) :: DATA_LEN
!   TYPE(C_PTR),          VALUE, INTENT(IN) :: GRIB_HANDLE

!   !> Function result
!   INTEGER(KIND=C_INT) :: RET

!   INTEGER(KIND=C_LONG_LONG), POINTER :: ENCODER_ID
!   INTEGER(KIND=C_LONG_LONG), POINTER :: MARS_ID
!   INTEGER(KIND=C_LONG_LONG), POINTER :: PAR_ID

!   !> Local error flags
!   INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_DATA_LEN=1_JPIB_K
!   INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_GRIB2_IS_NULL_POINTER=3_JPIB_K
!   INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_DICT_IS_NULL_POINTER=4_JPIB_K
!   INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PAR_DICT_IS_NULL_POINTER=5_JPIB_K
!   INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_HANDLE_IS_NULL_POINTER=6_JPIB_K

!   ! Local variables declared by the preprocessor for debugging purposes
!   PP_DEBUG_DECL_VARS

!   ! Local variables declared by the preprocessor for logging purposes
!   PP_LOG_DECL_VARS

!   ! Local variables declared by the preprocessor for tracing purposes
!   PP_TRACE_DECL_VARS

!   ! Trace begin of procedure
!   PP_TRACE_ENTER_PROCEDURE()

!   ! Initialization of good path return value
!   RET = 0_C_INT

!   !> Error handling
!   PP_DEBUG_CRITICAL_COND_THROW( DATA_LEN .LE. 0, ERRFLAG_WRONG_DATA_LEN )
!   PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(VALUES), ERRFLAG_VALUES_IS_NULL_POINTER )
!   PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(MULTIO_GRIB2), ERRFLAG_MULTIO_GRIB2_IS_NULL_POINTER )
!   PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(MARS_DICT), ERRFLAG_MARS_DICT_IS_NULL_POINTER )
!   PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(PAR_DICT), ERRFLAG_PAR_DICT_IS_NULL_POINTER )
!   PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(GRIB_HANDLE), ERRFLAG_GRIB_HANDLE_IS_NULL_POINTER )

!   !> Cast the VALUES pointer to a real array
!   CALL C_F_POINTER( VALUES, VALUES_ARRAY32, [DATA_LEN] )

!   !> Get encoder handle from the c pointer
!   CALL C_F_POINTER( MULTIO_GRIB2, ENCODER_ID )

!   !> Get mars dictionary handle from the c pointer
!   CALL C_F_POINTER( PAR_DICT, PAR_ID )

!   ! Trace end of procedure (on success)
!   PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

!   ! Exit point (On success)
!   RETURN

! ! Error handler
! PP_ERROR_HANDLER

!   ! Initialization of bad path return value
!   PP_SET_ERR_FAILURE( RET )

!   ! TODO: Add error handling code here

!   RETURN

! END FUNCTION MULTIO_GRIB2_ENCODER_ENCODE32
! #undef PP_PROCEDURE_NAME
! #undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_ENCODER_ENCODE'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_ENCODER_ENCODE( MULTIO_GRIB2, C_MARS_DICT, C_PAR_DICT, GRIB_HANDLE ) &
 BIND(C,NAME='multio_grib2_encoder_encode_f') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: API_SHARED_DATA_MOD,      ONLY: EXTRACT_MARS_DICTIONARY
  USE :: API_SHARED_DATA_MOD,      ONLY: EXTRACT_PAR_DICTIONARY
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: API_SHARED_DATA_MOD,      ONLY: MARS_DICT_TYPE_E
  USE :: API_SHARED_DATA_MOD,      ONLY: PAR_DICT_TYPE_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),          VALUE, INTENT(IN) :: MULTIO_GRIB2
  TYPE(C_PTR),          VALUE, INTENT(IN) :: C_MARS_DICT
  TYPE(C_PTR),          VALUE, INTENT(IN) :: C_PAR_DICT
  TYPE(C_PTR),          VALUE, INTENT(INOUT) :: WRITEBYTES
  TYPE(C_PTR),          VALUE, INTENT(INOUT) :: NUMBYTES

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:) :: F_MARS_DICT
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:) :: F_PAR_DICT
  TYPE(FORTRAN_MESSAGE_T),   POINTER :: MARS_DICT
  TYPE(PARAMETRIZATION_T),   POINTER :: PAR_DICT
  INTEGER(KIND=C_LONG_LONG), POINTER :: ENCODER_ID
  INTEGER(KIND=C_LONG_LONG), POINTER :: MARS_ID
  INTEGER(KIND=C_LONG_LONG), POINTER :: PAR_ID
  TYPE(HOOKS_T) :: HOOKS

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_DATA_LEN=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_MARS_DICTIONARY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_PAR_DICTIONARY=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_GRIB2_IS_NULL_POINTER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_DICT_IS_NULL_POINTER=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PAR_DICT_IS_NULL_POINTER=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITEBYTES_IS_NULL_POINTER=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NUMBYTES_IS_NULL_POINTER=8_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  RET = 0_C_INT

  ! Initialization of the hooks
  CALL HOOKS%DEBUG_HOOK_%INIT( )

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(MULTIO_GRIB2), ERRFLAG_MULTIO_GRIB2_IS_NULL_POINTER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(C_MARS_DICT), ERRFLAG_MARS_DICT_IS_NULL_POINTER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(C_PAR_DICT), ERRFLAG_PAR_DICT_IS_NULL_POINTER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(WRITEBYTES), ERRFLAG_WRITEBYTES_IS_NULL_POINTER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(NUMBYTES), ERRFLAG_NUMBYTES_IS_NULL_POINTER )

  !> Get encoder handle from the c pointer
  CALL C_F_POINTER( MULTIO_GRIB2, ENCODER_ID )

  !> Get mars dictionary handle from the c pointer
  F_MARS_DICT => NULL()
  CALL C_F_POINTER( C_MARS_DICT, F_MARS_DICT, [2] )
  
  !> Get parametrization dictionary handle from the c pointer
  F_PAR_DICT => NULL()
  CALL C_F_POINTER( C_PAR_DICT, F_PAR_DICT, [2] )


  PP_TRYCALL(ERRFLAG_EXTRACT_MARS_DICTIONARY) EXTRACT_MARS_DICTIONARY( F_MARS_DICT, MARS_DICT, HOOKS )
  
  PP_TRYCALL(ERRFLAG_EXTRACT_PAR_DICTIONARY) EXTRACT_PAR_DICTIONARY( F_PAR_DICT, PAR_DICT, HOOKS )

  !>

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

    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_WRONG_DATA_LEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong data len' )
    CASE (ERRFLAG_EXTRACT_MARS_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Extract mars dict' )
    CASE (ERRFLAG_EXTRACT_PAR_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Extract par dict' )
    CASE (ERRFLAG_MULTIO_GRIB2_IS_NULL_POINTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Multio grib2 is null pointer' )
    CASE (ERRFLAG_MARS_DICT_IS_NULL_POINTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mars dict is null' )
    CASE (ERRFLAG_PAR_DICT_IS_NULL_POINTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Par dict is null' )
    CASE (ERRFLAG_WRITEBYTES_IS_NULL_POINTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output location is null' )
    CASE (ERRFLAG_NUMBYTES_IS_NULL_POINTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output size location is null' )
    END SELECT

    ! Print the error stack
    ! NOTE: This is important when c is calling this function. Is opens the error_unit
    WRITE(ERROR_UNIT,*) ' PRINT ERROR STACK FROM: "'//__FILE__//'":', __LINE__
    CALL HOOKS%DEBUG_HOOK_%PRINT_ERROR_STACK( ERROR_UNIT )

    ! Free the error stack
    CALL HOOKS%DEBUG_HOOK_%FREE( )

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  RETURN

END FUNCTION MULTIO_GRIB2_ENCODER_ENCODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE API_ENCODER_WRAPPER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
