#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'api_shared_data_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'API_SHARED_DATA_MOD'
MODULE API_SHARED_DATA_MOD

  !> Symbols imported from other modules in the project
  USE :: MAP_INT64_MARS_DICT_MOD, ONLY: MAP_INT64_MARS_DICT_T
  USE :: MAP_INT64_PAR_DICT_MOD,  ONLY: MAP_INT64_PAR_DICT_T
  USE :: MAP_INT64_ENCODER_MOD,   ONLY: MAP_INT64_ENCODER_T

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

! In order to exchange data with c, we need to define the following maps
TYPE(MAP_INT64_MARS_DICT_T) :: SHARED_MARS_DICT_MAP
TYPE(MAP_INT64_PAR_DICT_T)  :: SHARED_PAR_DICT_MAP
TYPE(MAP_INT64_ENCODER_T)   :: SHARED_ENCODER_MAP

! Whitelist of public symbols (objects)
PUBLIC :: SHARED_MARS_DICT_MAP
PUBLIC :: SHARED_PAR_DICT_MAP
PUBLIC :: SHARED_ENCODER_MAP

! Whitelist of public symbols (subroutines)
PUBLIC :: FREE_ENCODER
PUBLIC :: FREE_PARAMETRIZATION
PUBLIC :: FREE_MARS_MESSAGE

PUBLIC :: EXTRACT_ENCODER
PUBLIC :: EXTRACT_MARS_DICTIONARY
PUBLIC :: EXTRACT_PAR_DICTIONARY

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FREE_ENCODER'
PP_THREAD_SAFE FUNCTION FREE_ENCODER( KEY, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: MULTIOM_CACHED_ENCODER_MOD, ONLY: MULTIOM_CACHED_ENCODERS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:), INTENT(INOUT) :: KEY
  TYPE(MULTIOM_CACHED_ENCODERS_T), POINTER,         INTENT(INOUT) :: VALUE
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_THE_ENCODER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=3_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(VALUE), ERRFLAG_ENCODER_NOT_ASSOCIATED )

  !> Free the encoder
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_THE_ENCODER) VALUE%FREE( HOOKS )

  !> Deallocate the key
  DEALLOCATE( KEY, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )


  !> Deallocate the encoder
  DEALLOCATE( VALUE, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )

  ! WRITE(6,*) " + Encoder deallocated"
  ! FLUSH(6)

  !> Paranoid move
  KEY => NULL()
  VALUE => NULL()

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'deallocation failure' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STAT)
      END IF
    CASE (ERRFLAG_ENCODER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'encoder not associated' )
    CASE (ERRFLAG_UNABLE_TO_FREE_THE_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free the encoder' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION FREE_ENCODER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FREE_PARAMETRIZATION'
PP_THREAD_SAFE FUNCTION FREE_PARAMETRIZATION( KEY, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:), INTENT(INOUT) :: KEY
  TYPE(PARAMETRIZATION_T), POINTER,                 INTENT(INOUT) :: VALUE
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_THE_DICTIONARY=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DICTIONARY_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=3_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(VALUE), ERRFLAG_DICTIONARY_NOT_ASSOCIATED )

  !> Free the dictionary
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_THE_DICTIONARY) VALUE%FREE( HOOKS )

  !> Deallocate the key
  DEALLOCATE( KEY, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )


  !> Deallocate the dictionary
  DEALLOCATE( VALUE, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )


  !> Paranoid move
  KEY => NULL()
  VALUE => NULL()

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'deallocation failure' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STAT)
      END IF
    CASE (ERRFLAG_DICTIONARY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'dictionary not associated' )
    CASE (ERRFLAG_UNABLE_TO_FREE_THE_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free the dictionary' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION FREE_PARAMETRIZATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FREE_MARS_MESSAGE'
PP_THREAD_SAFE FUNCTION FREE_MARS_MESSAGE( KEY, VALUE, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=C_LONG_LONG), POINTER, DIMENSION(:), INTENT(INOUT) :: KEY
  TYPE(FORTRAN_MESSAGE_T), POINTER,                 INTENT(INOUT) :: VALUE
  TYPE(HOOKS_T),                                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_THE_DICTIONARY=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DICTIONARY_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=3_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(VALUE), ERRFLAG_DICTIONARY_NOT_ASSOCIATED )

  !> Free the dictionary
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_THE_DICTIONARY) VALUE%FREE( HOOKS )

  !> Deallocate the key
  DEALLOCATE( KEY, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )


  !> Deallocate the dictionary
  DEALLOCATE( VALUE, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )


  !> Paranoid move
  KEY => NULL()
  VALUE => NULL()

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'deallocation failure' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STAT)
      END IF
    CASE (ERRFLAG_DICTIONARY_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'dictionary not associated' )
    CASE (ERRFLAG_UNABLE_TO_FREE_THE_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free the dictionary' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
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

END FUNCTION FREE_MARS_MESSAGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'EXTRACT_ENCODER'
PP_THREAD_SAFE FUNCTION EXTRACT_ENCODER( F_ENCODER_KEY, F_ENCODER, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_CACHED_ENCODER_MOD, ONLY: MULTIOM_CACHED_ENCODERS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=C_LONG_LONG), DIMENSION(2),  INTENT(IN)    :: F_ENCODER_KEY
  TYPE(MULTIOM_CACHED_ENCODERS_T), POINTER, INTENT(OUT)   :: F_ENCODER
  TYPE(HOOKS_T),                            INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: MAP_INITIALIZED
  LOGICAL :: MAP_HAS_ENCODER

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAP_NOT_INITIALIZED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_HANDLE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_ENCODER=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_ENCODER=6_JPIB_K


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

!$omp critical(API_ENCODER_MAP_GET)

  !> Check if the encoder is already in the encoders map
  PP_TRYCALL(ERRFLAG_MAP_NOT_INITIALIZED) SHARED_ENCODER_MAP%INITIALIZED( MAP_INITIALIZED, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. MAP_INITIALIZED, ERRFLAG_MAP_NOT_INITIALIZED )

  !> Check that the handle is a proper encoder handle
  PP_DEBUG_CRITICAL_COND_THROW( F_ENCODER_KEY(1).NE.1, ERRFLAG_WRONG_HANDLE )

  !> Check if the handle is associated to an encoder
  PP_TRYCALL(ERRFLAG_MATCH_ENCODER) SHARED_ENCODER_MAP%HAS( F_ENCODER_KEY, MAP_HAS_ENCODER, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. MAP_HAS_ENCODER, ERRFLAG_MATCH_ENCODER )

  !> Get the encoder from the map
  F_ENCODER => NULL()
  PP_TRYCALL(ERRFLAG_GET_ENCODER) SHARED_ENCODER_MAP%GET( F_ENCODER_KEY, F_ENCODER, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(F_ENCODER), ERRFLAG_GET_ENCODER )

!$omp end critical(API_ENCODER_MAP_GET)

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
    CASE(ERRFLAG_MAP_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The encoder map is not initialized' )
    CASE(ERRFLAG_WRONG_HANDLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The handle is not a proper encoder handle' )
    CASE(ERRFLAG_MATCH_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The handle is not associated to an encoder' )
    CASE(ERRFLAG_GET_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The encoder could not be retrieved' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  RETURN

END FUNCTION EXTRACT_ENCODER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'EXTRACT_MARS_DICTIONARY'
PP_THREAD_SAFE FUNCTION EXTRACT_MARS_DICTIONARY( F_MARS_KEY, F_MARS_DICTIONARY, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=C_LONG_LONG), DIMENSION(2), INTENT(IN)    :: F_MARS_KEY
  TYPE(FORTRAN_MESSAGE_T), POINTER,        INTENT(OUT)   :: F_MARS_DICTIONARY
  TYPE(HOOKS_T),                           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: MAP_INITIALIZED
  LOGICAL :: MAP_HAS_DICTIONARY

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAP_NOT_INITIALIZED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_HANDLE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_DICTIONARY=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_DICTIONARY=6_JPIB_K


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

!$omp critical(API_ENCODER_MAP_GET)
  !> Check if the dictionary is already in the dictionarys map
  PP_TRYCALL(ERRFLAG_MAP_NOT_INITIALIZED) SHARED_MARS_DICT_MAP%INITIALIZED( MAP_INITIALIZED, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. MAP_INITIALIZED, ERRFLAG_MAP_NOT_INITIALIZED )

  !> Check that the handle is a proper dictionary handle
  PP_DEBUG_CRITICAL_COND_THROW( F_MARS_KEY(1).NE.10, ERRFLAG_WRONG_HANDLE )

  !> Check if the handle is associated to an dictionary
  PP_TRYCALL(ERRFLAG_MATCH_DICTIONARY) SHARED_MARS_DICT_MAP%HAS( F_MARS_KEY, MAP_HAS_DICTIONARY, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. MAP_HAS_DICTIONARY, ERRFLAG_MATCH_DICTIONARY )

  !> Get the dictionary from the map
  F_MARS_DICTIONARY => NULL()
  PP_TRYCALL(ERRFLAG_GET_DICTIONARY) SHARED_MARS_DICT_MAP%GET( F_MARS_KEY, F_MARS_DICTIONARY, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(F_MARS_DICTIONARY), ERRFLAG_GET_DICTIONARY )

!$omp end critical(API_ENCODER_MAP_GET)

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
    CASE(ERRFLAG_MAP_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The dictionary map is not initialized' )
    CASE(ERRFLAG_WRONG_HANDLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The handle is not a proper dictionary handle' )
    CASE(ERRFLAG_MATCH_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The handle is not associated to an dictionary' )
    CASE(ERRFLAG_GET_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The dictionary could not be retrieved' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  RETURN

END FUNCTION EXTRACT_MARS_DICTIONARY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'EXTRACT_PAR_DICTIONARY'
PP_THREAD_SAFE FUNCTION EXTRACT_PAR_DICTIONARY( F_PAR_KEY, F_PAR_DICTIONARY, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=C_LONG_LONG), DIMENSION(2), INTENT(IN)    :: F_PAR_KEY
  TYPE(PARAMETRIZATION_T), POINTER,        INTENT(OUT)   :: F_PAR_DICTIONARY
  TYPE(HOOKS_T),                           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: MAP_INITIALIZED
  LOGICAL :: MAP_HAS_DICTIONARY

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAP_NOT_INITIALIZED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_HANDLE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_DICTIONARY=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GET_DICTIONARY=6_JPIB_K


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

!$omp critical(API_ENCODER_MAP_GET)
  !> Check if the dictionary is already in the dictionarys map
  PP_TRYCALL(ERRFLAG_MAP_NOT_INITIALIZED) SHARED_PAR_DICT_MAP%INITIALIZED( MAP_INITIALIZED, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. MAP_INITIALIZED, ERRFLAG_MAP_NOT_INITIALIZED )

  !> Check that the handle is a proper dictionary handle
  PP_DEBUG_CRITICAL_COND_THROW( F_PAR_KEY(1).NE.10, ERRFLAG_WRONG_HANDLE )

  !> Check if the handle is associated to an dictionary
  PP_TRYCALL(ERRFLAG_MATCH_DICTIONARY) SHARED_PAR_DICT_MAP%HAS( F_PAR_KEY, MAP_HAS_DICTIONARY, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. MAP_HAS_DICTIONARY, ERRFLAG_MATCH_DICTIONARY )

  !> Get the dictionary from the map
  F_PAR_DICTIONARY => NULL()
  PP_TRYCALL(ERRFLAG_GET_DICTIONARY) SHARED_PAR_DICT_MAP%GET( F_PAR_KEY, F_PAR_DICTIONARY, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(F_PAR_DICTIONARY), ERRFLAG_GET_DICTIONARY )

!$omp end critical(API_ENCODER_MAP_GET)

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
    CASE(ERRFLAG_MAP_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The dictionary map is not initialized' )
    CASE(ERRFLAG_WRONG_HANDLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The handle is not a proper dictionary handle' )
    CASE(ERRFLAG_MATCH_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The handle is not associated to an dictionary' )
    CASE(ERRFLAG_GET_DICTIONARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The dictionary could not be retrieved' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  RETURN

END FUNCTION EXTRACT_PAR_DICTIONARY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




END MODULE API_SHARED_DATA_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME