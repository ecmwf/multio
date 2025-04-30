! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'api_encoder_wrapper_mod-02.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'API_ENCODER_WRAPPER_MOD_02'
MODULE API_ENCODER_WRAPPER_MOD_02

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

! Whitelist of public symbols (encoder management)
PUBLIC :: MULTIO_GRIB2_ENCODER_OPEN
PUBLIC :: MULTIO_GRIB2_ENCODER_CLOSE
PUBLIC :: MULTIO_GRIB2_ENCODER_ENCODE

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_ENCODER_OPEN'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_ENCODER_OPEN( C_OPTIONS, C_ENCODER_MTG2, ENCODER_TYPE, LEN ) &
 BIND(C,NAME='multio_grib2_encoder_open_02') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT

  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,                  ONLY: JPIB_K
  USE :: HOOKS_MOD,                          ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD,                  ONLY: TOLOWER
  USE :: CACHED_MAPPED_ENCODER_WRAPPER_MOD,  ONLY: INIT_CACHED_MAPPED_ENCODER
  USE :: CACHED_MAPPED_ENCODER_WRAPPER_MOD,  ONLY: CACHED_MAPPED_ENCODER_NAME_E
  USE :: API_OPTIONS_DICTIONARY_UTILS_MOD,   ONLY: API_OPTIONS_T
  USE :: API_OPTIONS_DICTIONARY_WRAPPER_MOD, ONLY: GET_API_OPTIONS_DICTIONARY
  USE :: API_GENERAL_UTILS_MOD,              ONLY: COPY_CPTR_TO_F_STRING
  USE :: API_GENERAL_UTILS_MOD,              ONLY: DEREFERENCE_DOUBLE_C_POINTER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),         VALUE, INTENT(IN) :: C_OPTIONS
  TYPE(C_PTR),         VALUE, INTENT(IN) :: C_ENCODER_MTG2
  TYPE(C_PTR),         VALUE, INTENT(IN) :: ENCODER_TYPE
  INTEGER(KIND=C_INT), VALUE, INTENT(IN) :: LEN

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  TYPE(C_PTR), DIMENSION(:), POINTER :: TMP
  TYPE(API_OPTIONS_T) :: F_OPTIONS
  CHARACTER(LEN=64) :: ENCODER_TYPE_F_LOWERCASE
  TYPE(HOOKS_T) :: HOOKS

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEREFERENCE_DOUBLE_POINTER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_ALREADY_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_OPTIONS_INFO=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_ENCODER_TYPE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_ENCODER=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_NOT_ASSOCIATED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_ENCODER_ID=7_JPIB_K

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

!$omp critical(API_ENCODER_OPEN)

  !> Get the encoder pointer
  TMP => NULL()
  PP_TRYCALL(ERRFLAG_DEREFERENCE_DOUBLE_POINTER) DEREFERENCE_DOUBLE_C_POINTER( C_ENCODER_MTG2, 1_JPIB_K, TMP, HOOKS )

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( C_ASSOCIATED(TMP(1)), ERRFLAG_ENCODER_ALREADY_ASSOCIATED )

  !> Get the encoder options
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_OPTIONS_INFO) GET_API_OPTIONS_DICTIONARY( &
&       C_OPTIONS, F_OPTIONS, HOOKS )

  !> Get the type of the encoder to be created
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_ENCODER_TYPE) COPY_CPTR_TO_F_STRING( &
&        ENCODER_TYPE, LEN, ENCODER_TYPE_F_LOWERCASE, HOOKS, TOLOWER=.TRUE. )

  !> Depending on the encoder
  SELECT CASE (TRIM(ADJUSTL(ENCODER_TYPE_F_LOWERCASE)))

  CASE ( CACHED_MAPPED_ENCODER_NAME_E, 'default' )

    !> Create the encoder
    PP_TRYCALL(ERRFLAG_UNABLE_TO_EXTRACT_ENCODER) INIT_CACHED_MAPPED_ENCODER( &
&        TMP(1), F_OPTIONS, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(TMP(1)), ERRFLAG_ENCODER_NOT_ASSOCIATED )

  CASE DEFAULT

    !> Error handling
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_ENCODER_ID )

  END SELECT


!$omp end critical(API_ENCODER_OPEN)

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
    CASE (ERRFLAG_DEREFERENCE_DOUBLE_POINTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to dereference double pointer' )
    CASE (ERRFLAG_ENCODER_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder already associated' )
    CASE (ERRFLAG_UNABLE_TO_GET_OPTIONS_INFO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get options info' )
    CASE (ERRFLAG_UNABLE_TO_READ_ENCODER_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read encoder type' )
    CASE (ERRFLAG_UNABLE_TO_EXTRACT_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract encoder' )
    CASE (ERRFLAG_ENCODER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder not associated' )
    CASE (ERRFLAG_INVALID_ENCODER_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid encoder ID' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'encoder ID: '//TRIM(ADJUSTL(CACHED_MAPPED_ENCODER_NAME_E)) )

    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Print the error stack
    ! NOTE: This is important when c is calling this function.
    ! It opens the error_unit
    WRITE(ERROR_UNIT,*) ' PRINT ERROR STACK FROM: "'//__FILE__//'"', __LINE__
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
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_ENCODER_CLOSE( C_ENCODER_MTG2 ) &
 BIND(C,NAME='multio_grib2_encoder_close_02') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: CACHED_MAPPED_ENCODER_WRAPPER_MOD, ONLY: CACHED_MAPPED_ENCODER_IDX_E
  USE :: CACHED_MAPPED_ENCODER_WRAPPER_MOD, ONLY: FREE_CACHED_MAPPED_ENCODER
  USE :: API_F_C_WRAPPER_MOD,               ONLY: F_C_GET_INFO_WRAPPER
  USE :: API_GENERAL_UTILS_MOD,             ONLY: DEREFERENCE_DOUBLE_C_POINTER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR), VALUE, INTENT(IN) :: C_ENCODER_MTG2

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  TYPE(C_PTR), DIMENSION(:), POINTER :: TMP
  TYPE(HOOKS_T) :: HOOKS
  INTEGER(KIND=JPIB_K) :: OBJ_ID
  INTEGER(KIND=JPIB_K) :: OBJ_SIZE
  INTEGER(KIND=JPIB_K) :: BUF_SIZE
  INTEGER(KIND=JPIB_K) :: HASH

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEREFERENCE_DOUBLE_POINTER=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_INFO=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_FREE_ENCODER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_ENCODER_ID=5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(C_ENCODER_MTG2), ERRFLAG_ENCODER_NOT_ASSOCIATED )

  !> Get the encoder pointer
  TMP => NULL()
  PP_TRYCALL(ERRFLAG_DEREFERENCE_DOUBLE_POINTER) DEREFERENCE_DOUBLE_C_POINTER( C_ENCODER_MTG2, 1_JPIB_K, TMP, HOOKS )

  !> Extract info from the encoder
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_INFO) F_C_GET_INFO_WRAPPER( TMP(1), OBJ_ID, OBJ_SIZE, BUF_SIZE, HASH, HOOKS )

  !> Get th fortran handle from the c handle
  SELECT CASE ( OBJ_ID )
  CASE (CACHED_MAPPED_ENCODER_IDX_E)

    !> Free the encoder
    PP_TRYCALL(ERRFLAG_UNABLE_FREE_ENCODER) FREE_CACHED_MAPPED_ENCODER( TMP(1), HOOKS )
    TMP(1) = C_NULL_PTR

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_ENCODER_ID )

  END SELECT

!$omp end critical(API_ENCODER_MAP_REMOVE)

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
    CASE (ERRFLAG_DEREFERENCE_DOUBLE_POINTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to dereference double pointer' )
    CASE (ERRFLAG_UNABLE_TO_GET_INFO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get info' )
    CASE (ERRFLAG_UNABLE_FREE_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free encoder' )
    CASE (ERRFLAG_INVALID_ENCODER_ID)
      CTMP = REPEAT(' ', 32)
      WRITE(CTMP,*) OBJ_ID
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid encoder ID: '//TRIM(ADJUSTL(CTMP)) )
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


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB2_ENCODER_ENCODE'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB2_ENCODER_ENCODE( C_ENCODER_MTG2, &
& C_MARS_DICT, C_PAR_DICT, C_CODES_HANDLE_LOC, C_MESSAGE_LEN ) &
 BIND(C,NAME='multio_grib2_encoder_encode_f_02') RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER

  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,                      ONLY: JPIB_K
  USE :: HOOKS_MOD,                              ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD,                    ONLY: FORTRAN_MESSAGE_T
  USE :: MARS_DICTIONARY_WRAPPER_MOD,            ONLY: EXTRACT_MARS_DICTIONARY
  USE :: PARAMETRIZATION_MOD,                    ONLY: PARAMETRIZATION_T
  USE :: PARAMETRIZATION_DICTIONARY_WRAPPER_MOD, ONLY: EXTRACT_PARAMETRIZATION_DICTIONARY
  USE :: CACHED_MAPPED_ENCODER_WRAPPER_MOD,      ONLY: EXTRACT_CACHED_MAPPED_ENCODER
  USE :: API_F_C_WRAPPER_MOD,                    ONLY: F_C_GET_INFO_WRAPPER
  USE :: CACHED_MAPPED_ENCODER_WRAPPER_MOD,      ONLY: CACHED_MAPPED_ENCODER_IDX_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),  VALUE, INTENT(IN)    :: C_ENCODER_MTG2
  TYPE(C_PTR),  VALUE, INTENT(IN)    :: C_MARS_DICT
  TYPE(C_PTR),  VALUE, INTENT(IN)    :: C_PAR_DICT
  TYPE(C_PTR),         INTENT(INOUT) :: C_CODES_HANDLE_LOC
  INTEGER(KIND=C_INT), INTENT(INOUT) :: C_MESSAGE_LEN

  !> Function result
  INTEGER(KIND=C_INT) :: RET

  !> Local variables
  TYPE(HOOKS_T) :: HOOKS
  TYPE(FORTRAN_MESSAGE_T), POINTER :: F_MARS_DICT
  TYPE(PARAMETRIZATION_T), POINTER :: F_PAR_DICT
  TYPE(C_PTR), POINTER, DIMENSION(:) :: TMP
  INTEGER(KIND=JPIB_K) :: OBJ_ID
  INTEGER(KIND=JPIB_K) :: OBJ_SIZE
  INTEGER(KIND=JPIB_K) :: BUF_SIZE
  INTEGER(KIND=JPIB_K) :: HASH

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_MARS_DICTIONARY=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_PARAMETRIZATION_DICTIONARY=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_INFO=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_ENCODER_ID=5_JPIB_K

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

  ! Initialization of the hooks
  CALL HOOKS%DEBUG_HOOK_%INIT( )

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(C_ENCODER_MTG2), ERRFLAG_ENCODER_NOT_ASSOCIATED )

  !> Get the encoder pointer
  F_MARS_DICT => NULL()
  F_PAR_DICT => NULL()
  PP_TRYCALL(ERRFLAG_EXTRACT_MARS_DICTIONARY) EXTRACT_MARS_DICTIONARY( C_MARS_DICT, F_MARS_DICT, HOOKS )
  PP_TRYCALL(ERRFLAG_EXTRACT_MARS_DICTIONARY) EXTRACT_PARAMETRIZATION_DICTIONARY( C_PAR_DICT, F_PAR_DICT, HOOKS )

  !> Extract info from the encoder
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_INFO) F_C_GET_INFO_WRAPPER( C_ENCODER_MTG2, OBJ_ID, OBJ_SIZE, BUF_SIZE, HASH, HOOKS )


  !> Get th fortran handle from the c handle
  SELECT CASE ( OBJ_ID )
  CASE (CACHED_MAPPED_ENCODER_IDX_E)

    WRITE(*,*) 'ENCODER_MTG2'

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_ENCODER_ID )

  END SELECT


  !> Be sure we don't have any memory leaks
  CALL HOOKS%DEBUG_HOOK_%FREE( )

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

    CHARACTER(LEN=32) :: CTMP

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ENCODER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder not associated' )
    CASE (ERRFLAG_EXTRACT_MARS_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract MARS dictionary' )
    CASE (ERRFLAG_EXTRACT_PARAMETRIZATION_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract parametrization dictionary' )
    CASE (ERRFLAG_UNABLE_TO_GET_INFO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get info' )
    CASE (ERRFLAG_INVALID_ENCODER_ID)
      CTMP = REPEAT(' ', 32)
      WRITE(CTMP,*) OBJ_ID
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid encoder ID: '//TRIM(ADJUSTL(CTMP)) )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
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


END MODULE API_ENCODER_WRAPPER_MOD_02
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
