! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'raw_encoder_wrapper_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'RAW_ENCODER_WRAPPER_MOD'
MODULE RAW_ENCODER_WRAPPER_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_RAW_ENCODER_MOD, ONLY: MULTIOM_RAW_ENCODER_T

IMPLICIT NONE

  !> Default module visibnility
  PRIVATE

  !> Encoder index used used to identify the encoder wrapper
  INTEGER(KIND=JPIB_K), PARAMETER :: RAW_ENCODER_IDX_E = 1_JPIB_K
  CHARACTER(LEN=*), PARAMETER :: RAW_ENCODER_NAME_E = 'raw-encoder'

  !> Class used as a wrapper for the encoder
  TYPE :: RAW_ENCODER_CONTAINER_T
    TYPE(MULTIOM_RAW_ENCODER_T), POINTER :: ENCODER => NULL()
  END TYPE

  !> Whitelist of public symbols
  PUBLIC :: RAW_ENCODER_IDX_E
  PUBLIC :: RAW_ENCODER_NAME_E
  PUBLIC :: CREATE_RAW_ENCODER
  PUBLIC :: INIT_RAW_ENCODER
  PUBLIC :: EXTRACT_RAW_ENCODER
  PUBLIC :: FREE_RAW_ENCODER
  PUBLIC :: ENCODE_PREPARE_RAW_ENCODER
  PUBLIC :: ENCODE_RUNTIME_RAW_ENCODER

CONTAINS



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INIT_RAW_ENCODER'
PP_THREAD_SAFE FUNCTION INIT_RAW_ENCODER( WRAPPED_ENCODER, ENCODER_CFG, ENCODER_OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,                ONLY: JPIB_K
  USE :: HOOKS_MOD,                        ONLY: HOOKS_T
  USE :: API_OPTIONS_DICTIONARY_UTILS_MOD, ONLY: API_OPTIONS_T
  USE :: MULTIOM_RAW_ENCODER_MOD,          ONLY: MULTIOM_RAW_ENCODER_T
  USE :: API_F_C_WRAPPER_MOD,              ONLY: F_C_WRAPPER_T
  USE :: API_F_C_WRAPPER_MOD,              ONLY: F_C_ALLOCATE_WRAPPER
  USE :: YAML_CORE_UTILS_MOD,              ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_ENCODER_OPTIONS_MOD,         ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),                  INTENT(INOUT) :: WRAPPED_ENCODER
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: ENCODER_CFG
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: ENCODER_OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(MULTIOM_RAW_ENCODER_T), POINTER :: RAW_ENCODER

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_ENCODER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INITIALIZE_ENCODER=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INJECT_CHECKSUM=7_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( C_ASSOCIATED(WRAPPED_ENCODER), ERRFLAG_WRAPPER_ALREADY_ASSOCIATED )

  !> Initialize the encoder
  RAW_ENCODER => NULL()

  !> Create the encoder
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CREATE_ENCODER) CREATE_RAW_ENCODER( &
&       WRAPPED_ENCODER, RAW_ENCODER, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(RAW_ENCODER), ERRFLAG_ENCODER_NOT_ASSOCIATED )

  !> Initialize the encoder
   PP_TRYCALL(ERRFLAG_INITIALIZE_ENCODER) RAW_ENCODER%INIT(&
&        ENCODER_CFG, ENCODER_OPT, HOOKS )

  ! Inject checksum
  PP_TRYCALL(ERRFLAG_INJECT_CHECKSUM) INJECT_CHECKSUM_RAW_ENCODER( WRAPPED_ENCODER, HOOKS )

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
    CASE (ERRFLAG_WRAPPER_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder already associated at function entry' )
    CASE (ERRFLAG_ENCODER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder not associated after allocation' )
    CASE (ERRFLAG_UNABLE_TO_CREATE_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract encoder' )
    CASE (ERRFLAG_INITIALIZE_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to initialize encoder' )
    CASE (ERRFLAG_INJECT_CHECKSUM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inject checksum' )
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

END FUNCTION INIT_RAW_ENCODER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_PREPARE_RAW_ENCODER'
PP_THREAD_SAFE FUNCTION ENCODE_PREPARE_RAW_ENCODER( WRAPPED_ENCODER, &
&  F_MARS_DICT, F_PAR_DICT, GRIB_SAMPLE_ID, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIM_K
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD,        ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,        ONLY: PARAMETRIZATION_T
  USE :: MULTIOM_RAW_ENCODER_MOD,    ONLY: MULTIOM_RAW_ENCODER_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),             INTENT(IN)    :: WRAPPED_ENCODER
  TYPE(FORTRAN_MESSAGE_T), INTENT(IN)    :: F_MARS_DICT
  TYPE(PARAMETRIZATION_T), INTENT(IN)    :: F_PAR_DICT
  INTEGER(KIND=JPIM_K),    INTENT(INOUT) :: GRIB_SAMPLE_ID
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(MULTIOM_RAW_ENCODER_T), POINTER :: RAW_ENCODER

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_ENCODER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_THE_ENCODER=2_JPIB_K

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

  ! Extract the encoder
  PP_TRYCALL(ERRFLAG_EXTRACT_ENCODER) EXTRACT_RAW_ENCODER( WRAPPED_ENCODER, RAW_ENCODER, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(RAW_ENCODER), ERRFLAG_EXTRACT_ENCODER )

  ! Call the encoder
  PP_TRYCALL(ERRFLAG_CALL_THE_ENCODER) RAW_ENCODER%PREPARE(  F_MARS_DICT, F_PAR_DICT, GRIB_SAMPLE_ID, HOOKS )

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
    CASE (ERRFLAG_EXTRACT_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract encoder' )
    CASE (ERRFLAG_CALL_THE_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call the encoder' )
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

END FUNCTION ENCODE_PREPARE_RAW_ENCODER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ENCODE_RUNTIME_RAW_ENCODER'
PP_THREAD_SAFE FUNCTION ENCODE_RUNTIME_RAW_ENCODER( WRAPPED_ENCODER, &
&   F_MARS_DICT, F_PAR_DICT, GRIB_SAMPLE_ID, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIM_K
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD,        ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,        ONLY: PARAMETRIZATION_T
  USE :: MULTIOM_RAW_ENCODER_MOD,    ONLY: MULTIOM_RAW_ENCODER_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),             INTENT(IN)    :: WRAPPED_ENCODER
  TYPE(FORTRAN_MESSAGE_T), INTENT(IN)    :: F_MARS_DICT
  TYPE(PARAMETRIZATION_T), INTENT(IN)    :: F_PAR_DICT
  INTEGER(KIND=JPIM_K),    INTENT(INOUT) :: GRIB_SAMPLE_ID
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(MULTIOM_RAW_ENCODER_T), POINTER :: RAW_ENCODER

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EXTRACT_ENCODER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_THE_ENCODER=2_JPIB_K

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

  ! Extract the encoder
  PP_TRYCALL(ERRFLAG_EXTRACT_ENCODER) EXTRACT_RAW_ENCODER( WRAPPED_ENCODER, RAW_ENCODER, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(RAW_ENCODER), ERRFLAG_EXTRACT_ENCODER )

  ! Call the encoder
  PP_TRYCALL(ERRFLAG_CALL_THE_ENCODER) RAW_ENCODER%RUNTIME(  F_MARS_DICT, F_PAR_DICT, GRIB_SAMPLE_ID, HOOKS )

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
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_EXTRACT_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract encoder' )
    CASE (ERRFLAG_CALL_THE_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call the encoder' )
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

END FUNCTION ENCODE_RUNTIME_RAW_ENCODER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FREE_RAW_ENCODER'
PP_THREAD_SAFE FUNCTION FREE_RAW_ENCODER( WRAPPED_ENCODER, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPIB_K
  USE :: HOOKS_MOD,               ONLY: HOOKS_T
  USE :: MULTIOM_RAW_ENCODER_MOD, ONLY: MULTIOM_RAW_ENCODER_T
  USE :: API_F_C_WRAPPER_MOD,     ONLY: F_C_WRAPPER_T
  USE :: API_F_C_WRAPPER_MOD,     ONLY: F_C_EXTRACT_WRAPPER
  USE :: API_F_C_WRAPPER_MOD,     ONLY: F_C_FREE_WRAPPER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),   INTENT(INOUT) :: WRAPPED_ENCODER
  TYPE(HOOKS_T), INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(F_C_WRAPPER_T), POINTER, DIMENSION(:) :: WRAPPER
  INTEGER(KIND=C_INT8_T), POINTER, DIMENSION(:) :: BUFFER
  TYPE(MULTIOM_RAW_ENCODER_T), POINTER :: ENCODER
  TYPE(RAW_ENCODER_CONTAINER_T) :: ENCODER_CONTAINER
  INTEGER(KIND=JPIB_K) :: DIM
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local prameters
  INTEGER(KIND=JPIB_K), PARAMETER :: RAW_ENCODER_CONTAINER_BYTE_SIZE = &
&   STORAGE_SIZE(ENCODER_CONTAINER) / 8_JPIB_K

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_ENCODER=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_BUFFER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_BUFFER_NOT_ASSOCIATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_BUFFER_SIZE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_NOT_ASSOCIATED=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_ENCODER=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_ENCODER=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_WRAPPER=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_ENCODER=11_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(WRAPPED_ENCODER), ERRFLAG_WRAPPER_NOT_ASSOCIATED )

  ! Extract the wrapper
  WRAPPER => NULL()
  CALL C_F_POINTER( WRAPPED_ENCODER, WRAPPER, [1] )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(WRAPPER), ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER )

  ! Extract the buffer
  BUFFER => NULL()
  DIM = 0_JPIB_K
  PP_TRYCALL(ERRFLAG_UNABLE_TO_EXTRACT_BUFFER) F_C_EXTRACT_WRAPPER( &
&   WRAPPER, BUFFER, DIM, HOOKS )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(BUFFER), ERRFLAG_BUFFER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( DIM .NE. RAW_ENCODER_CONTAINER_BYTE_SIZE, ERRFLAG_WRONG_BUFFER_SIZE )

  ! Get the Container
  ENCODER_CONTAINER = TRANSFER( BUFFER, ENCODER_CONTAINER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(ENCODER_CONTAINER%ENCODER), ERRFLAG_ENCODER_NOT_ASSOCIATED )

  ! Associate the encoder pointer
  ENCODER => ENCODER_CONTAINER%ENCODER

  ! Print the encoder to be freed
  !! MIVAL: This is a debug print statement that should be enable to debug the c/fortran interoperability
  !! PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_ENCODER) ENCODER%PRINT( 6_JPIB_K, 0_JPIB_K, HOOKS )
  ! Free the encoder
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_ENCODER) ENCODER%FREE( HOOKS )

  ! Deallocate the encoder
  DEALLOCATE( ENCODER_CONTAINER%ENCODER, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0_JPIB_K, ERRFLAG_UNABLE_TO_DEALLOCATE_ENCODER )
  ENCODER_CONTAINER%ENCODER => NULL()
  ENCODER => NULL()

  ! Error handling
  BUFFER = 0_C_INT8_T

  ! Free the wrapper
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_WRAPPER) F_C_FREE_WRAPPER( &
&   WRAPPER, HOOKS )
  WRAPPER => NULL()
  BUFFER => NULL()

  ! Reset the c pointer
  WRAPPED_ENCODER = C_NULL_PTR

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
    CASE (ERRFLAG_UNABLE_TO_EXTRACT_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract encoder' )
    CASE (ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract wrapper' )
    CASE (ERRFLAG_UNABLE_TO_EXTRACT_BUFFER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract buffer' )
    CASE (ERRFLAG_BUFFER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Buffer not associated' )
    CASE (ERRFLAG_WRONG_BUFFER_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong buffer size' )
    CASE (ERRFLAG_ENCODER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder not associated' )
    CASE (ERRFLAG_UNABLE_TO_FREE_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free encoder' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate encoder' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=ALLOC_STAT )
      END IF
    CASE (ERRFLAG_UNABLE_TO_FREE_WRAPPER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free wrapper' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print encoder' )
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

END FUNCTION FREE_RAW_ENCODER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CREATE_RAW_ENCODER'
PP_THREAD_SAFE FUNCTION CREATE_RAW_ENCODER( WRAPPED_ENCODER, ENCODER, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: MULTIOM_RAW_ENCODER_MOD,    ONLY: MULTIOM_RAW_ENCODER_T
  USE :: API_F_C_WRAPPER_MOD,        ONLY: F_C_WRAPPER_T
  USE :: API_F_C_WRAPPER_MOD,        ONLY: F_C_ALLOCATE_WRAPPER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),                          INTENT(INOUT) :: WRAPPED_ENCODER
  TYPE(MULTIOM_RAW_ENCODER_T), POINTER, INTENT(INOUT) :: ENCODER
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(RAW_ENCODER_CONTAINER_T) :: ENCODER_CONTAINER
  TYPE(F_C_WRAPPER_T),    POINTER, DIMENSION(:) :: WRAPPER
  INTEGER(KIND=C_INT8_T), POINTER, DIMENSION(:) :: BUFFER
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local prameters
  INTEGER(KIND=JPIB_K), PARAMETER :: RAW_ENCODER_CONTAINER_BYTE_SIZE = &
&   STORAGE_SIZE(ENCODER_CONTAINER) / 8_JPIB_K

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_ALREADY_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_ENCODER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_WRAPPER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_NOT_ASSOCIATED_AFTER_ALLOCATION=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_BUFFER_NOT_ASSOCIATED_AFTER_ALLOCATION=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( C_ASSOCIATED(WRAPPED_ENCODER), ERRFLAG_WRAPPER_ALREADY_ASSOCIATED)
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(ENCODER), ERRFLAG_ENCODER_ALREADY_ASSOCIATED)

  ! Allocate the encoder
  ALLOCATE( ENCODER_CONTAINER%ENCODER, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0_JPIB_K, ERRFLAG_UNABLE_TO_ALLOCATE_ENCODER )

  ! Allocate the wrapper
  WRAPPER => NULL()
  BUFFER => NULL()
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE_WRAPPER) F_C_ALLOCATE_WRAPPER( &
&   WRAPPER, BUFFER, RAW_ENCODER_IDX_E, RAW_ENCODER_CONTAINER_BYTE_SIZE, HOOKS )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(WRAPPER), ERRFLAG_WRAPPER_NOT_ASSOCIATED_AFTER_ALLOCATION)
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(BUFFER), ERRFLAG_BUFFER_NOT_ASSOCIATED_AFTER_ALLOCATION)

  ! Wrap the encoder in order to be able to expose it to c
  BUFFER = TRANSFER(ENCODER_CONTAINER, BUFFER, RAW_ENCODER_CONTAINER_BYTE_SIZE )

  ! Set the return arguments
  ENCODER => ENCODER_CONTAINER%ENCODER
  WRAPPED_ENCODER = C_LOC(WRAPPER)

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
    CASE (ERRFLAG_WRAPPER_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper already associated' )
    CASE (ERRFLAG_ENCODER_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder already associated' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate encoder' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=ALLOC_STAT )
      END IF
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_WRAPPER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate wrapper' )
    CASE (ERRFLAG_WRAPPER_NOT_ASSOCIATED_AFTER_ALLOCATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper not associated after allocation' )
    CASE (ERRFLAG_BUFFER_NOT_ASSOCIATED_AFTER_ALLOCATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Buffer not associated after allocation' )
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

END FUNCTION CREATE_RAW_ENCODER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INJECT_CHECKSUM_RAW_ENCODER'
PP_THREAD_SAFE FUNCTION INJECT_CHECKSUM_RAW_ENCODER( WRAPPED_ENCODER, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,          ONLY: JPIB_K
  USE :: HOOKS_MOD,                  ONLY: HOOKS_T
  USE :: API_F_C_WRAPPER_MOD,        ONLY: F_C_WRAPPER_T
  USE :: API_F_C_WRAPPER_MOD,        ONLY: F_C_INJECT_CHECKSUM_WRAPPER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),   INTENT(INOUT) :: WRAPPED_ENCODER
  TYPE(HOOKS_T), INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(F_C_WRAPPER_T),    POINTER, DIMENSION(:) :: WRAPPER

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INJECT_CHECKOSUM=3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(WRAPPED_ENCODER), ERRFLAG_WRAPPER_NOT_ASSOCIATED )

  ! Extract the wrapper
  WRAPPER => NULL()
  CALL C_F_POINTER( WRAPPED_ENCODER, WRAPPER, [1] )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(WRAPPER), ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER )

  ! Extract the buffer
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INJECT_CHECKOSUM) F_C_INJECT_CHECKSUM_WRAPPER( WRAPPER, HOOKS )

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
    CASE (ERRFLAG_WRAPPER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper not associated' )
    CASE (ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract wrapper' )
    CASE (ERRFLAG_UNABLE_TO_INJECT_CHECKOSUM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inject checksum' )
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

END FUNCTION INJECT_CHECKSUM_RAW_ENCODER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'EXTRACT_RAW_ENCODER'
PP_THREAD_SAFE FUNCTION EXTRACT_RAW_ENCODER( WRAPPED_ENCODER, ENCODER, HOOKS ) RESULT(RET)

  !> Symbols imported from intrinsic modules.
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT8_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_F_POINTER

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPIB_K
  USE :: HOOKS_MOD,               ONLY: HOOKS_T
  USE :: MULTIOM_RAW_ENCODER_MOD, ONLY: MULTIOM_RAW_ENCODER_T
  USE :: API_F_C_WRAPPER_MOD,     ONLY: F_C_WRAPPER_T
  USE :: API_F_C_WRAPPER_MOD,     ONLY: F_C_EXTRACT_WRAPPER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(C_PTR),                          INTENT(IN)    :: WRAPPED_ENCODER
  TYPE(MULTIOM_RAW_ENCODER_T), POINTER, INTENT(INOUT) :: ENCODER
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(F_C_WRAPPER_T),    POINTER, DIMENSION(:) :: WRAPPER
  INTEGER(KIND=C_INT8_T), POINTER, DIMENSION(:) :: BUFFER
  TYPE(RAW_ENCODER_CONTAINER_T) :: ENCODER_CONTAINER
  INTEGER(KIND=JPIB_K) :: DIM

  !> Local prameters
  INTEGER(KIND=JPIB_K), PARAMETER :: RAW_ENCODER_CONTAINER_BYTE_SIZE = &
&   STORAGE_SIZE(ENCODER_CONTAINER) / 8_JPIB_K

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRAPPER_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_ALREADY_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_BUFFER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_BUFFER_NOT_ASSOCIATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_BUFFER_SIZE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_NOT_ASSOCIATED=7_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(WRAPPED_ENCODER), ERRFLAG_WRAPPER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(ENCODER), ERRFLAG_ENCODER_ALREADY_ASSOCIATED )

  ! Extract the wrapper
  WRAPPER => NULL()
  CALL C_F_POINTER( WRAPPED_ENCODER, WRAPPER, [1] )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(WRAPPER), ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER )

  ! Extract the buffer
  BUFFER => NULL()
  DIM = 0_JPIB_K
  PP_TRYCALL(ERRFLAG_UNABLE_TO_EXTRACT_BUFFER) F_C_EXTRACT_WRAPPER( &
&   WRAPPER, BUFFER, DIM, HOOKS )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(BUFFER), ERRFLAG_BUFFER_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( DIM .NE. RAW_ENCODER_CONTAINER_BYTE_SIZE, ERRFLAG_WRONG_BUFFER_SIZE )

  ! Get the Container
  ENCODER_CONTAINER = TRANSFER( BUFFER, ENCODER_CONTAINER )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(ENCODER_CONTAINER%ENCODER), ERRFLAG_ENCODER_NOT_ASSOCIATED )

  ! Associate the encoder pointer
  ENCODER => ENCODER_CONTAINER%ENCODER

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
    CASE (ERRFLAG_WRAPPER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrapper not associated' )
    CASE (ERRFLAG_ENCODER_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder already associated' )
    CASE (ERRFLAG_UNABLE_TO_EXTRACT_WRAPPER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract wrapper' )
    CASE (ERRFLAG_UNABLE_TO_EXTRACT_BUFFER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to extract buffer' )
    CASE (ERRFLAG_BUFFER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Buffer not associated' )
    CASE (ERRFLAG_WRONG_BUFFER_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong buffer size' )
    CASE (ERRFLAG_ENCODER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder not associated' )
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

END FUNCTION EXTRACT_RAW_ENCODER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE RAW_ENCODER_WRAPPER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME