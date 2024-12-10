
! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'cached_encoder_collection_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'CACHED_ENCODER_COLLECTION_MOD'
MODULE CACHED_ENCODER_COLLECTION_MOD

  ! Symbols imported from other modules within the project.
  USE :: CACHED_ENCODER_MOD,    ONLY: CACHED_ENCODER_T
  USE :: METADATA_BASE_MOD,     ONLY: METADATA_BASE_A
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!> @brief Datatype used to store a key-value pair
TYPE :: CACHED_ENCODER_COLLECTION_T

  !> Default visibility of the type
  PRIVATE

  !> Encoders that satisfy the rules
  TYPE(CACHED_ENCODER_T), DIMENSION(:), POINTER :: ENCODERS_ => NULL()

CONTAINS
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT           => CACHED_ENCODER_COLLECTION_INIT
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: IS_INITIALIZED => CACHED_ENCODER_COLLECTION_INITIALIZED
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: SIZE           => CACHED_ENCODER_COLLECTION_SIZE
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: BYTESIZE       => CACHED_ENCODER_COLLECTION_BYTESIZE
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: DUMP           => CACHED_ENCODER_COLLECTION_DUMP
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ENCODE         => CACHED_ENCODER_COLLECTION_ENCODE
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE           => CACHED_ENCODER_COLLECTION_FREE
END TYPE

!> Whitelist of public symbols (types)
PUBLIC :: CACHED_ENCODER_COLLECTION_T

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CACHED_ENCODER_COLLECTION_INIT'
PP_THREAD_SAFE FUNCTION CACHED_ENCODER_COLLECTION_INIT( THIS, MSG, PAR, &
& METADATA, ENCODERS, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: CACHED_ENCODER_MOD,       ONLY: CACHED_ENCODER_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CACHED_ENCODER_COLLECTION_T),            INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),                       INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),                       INTENT(IN)    :: PAR
  CLASS(METADATA_BASE_A), POINTER,               INTENT(IN)    :: METADATA
  TYPE(CACHED_ENCODER_T), DIMENSION(:), POINTER, INTENT(IN)    :: ENCODERS
  TYPE(GRIB_ENCODER_OPTIONS_T),                  INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODERS_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_ENCODER_NOT_ASSOCIATED=2_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%ENCODERS_), ERRFLAG_ENCODERS_ALREADY_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(ENCODERS), ERRFLAG_INPUT_ENCODER_NOT_ASSOCIATED )

  !> Associate the encoder
  THIS%ENCODERS_ => ENCODERS

  !> Allocate and preset the encoders
  WRITE(*,*) 'ALLOCATING/PRESET ENCODERS'

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
    CASE(ERRFLAG_ENCODERS_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoders already associated' )
    CASE(ERRFLAG_INPUT_ENCODER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input encoder not associated' )
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


END FUNCTION CACHED_ENCODER_COLLECTION_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CACHED_ENCODER_COLLECTION_INITIALIZED'
PP_THREAD_SAFE FUNCTION CACHED_ENCODER_COLLECTION_INITIALIZED( THIS, &
&    IS_INITIALIZED, HOOKS ) RESULT(RET)

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
  CLASS(CACHED_ENCODER_COLLECTION_T), INTENT(INOUT) :: THIS
  LOGICAL,                            INTENT(OUT)   :: IS_INITIALIZED
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODERS_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_ENCODER_NOT_ASSOCIATED=2_JPIB_K

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

  IS_INITIALIZED = ASSOCIATED(THIS%ENCODERS_)

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


END FUNCTION CACHED_ENCODER_COLLECTION_INITIALIZED
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CACHED_ENCODER_COLLECTION_SIZE'
PP_THREAD_SAFE FUNCTION CACHED_ENCODER_COLLECTION_SIZE( THIS, SZ, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CACHED_ENCODER_COLLECTION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),               INTENT(OUT)   :: SZ
  TYPE(GRIB_ENCODER_OPTIONS_T),       INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODERS_NOT_ASSOCIATED=1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%ENCODERS_), ERRFLAG_ENCODERS_NOT_ASSOCIATED )

  ! Get the size
  SZ = SIZE(THIS%ENCODERS_)

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
    CASE(ERRFLAG_ENCODERS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoders not associated' )
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


END FUNCTION CACHED_ENCODER_COLLECTION_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CACHED_ENCODER_COLLECTION_BYTESIZE'
PP_THREAD_SAFE FUNCTION CACHED_ENCODER_COLLECTION_BYTESIZE( THIS, MEMORY_BYTESIZE, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CACHED_ENCODER_COLLECTION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),               INTENT(OUT)   :: MEMORY_BYTESIZE
  TYPE(GRIB_ENCODER_OPTIONS_T),       INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: SZ

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODERS_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_NESTED_SIZE=2_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%ENCODERS_), ERRFLAG_ENCODERS_NOT_ASSOCIATED )

  ! Get the size
  MEMORY_BYTESIZE = 0_JPIB_K


  DO I = 1, SIZE(THIS%ENCODERS_)
    PP_TRYCALL(ERRFLAG_CALL_NESTED_SIZE) THIS%ENCODERS_(I)%BYTESIZE( SZ, OPT, HOOKS )
    MEMORY_BYTESIZE = MEMORY_BYTESIZE + SZ
  ENDDO

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
    CASE(ERRFLAG_ENCODERS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoders not associated' )
    CASE(ERRFLAG_CALL_NESTED_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in nested call' )
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


END FUNCTION CACHED_ENCODER_COLLECTION_BYTESIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CACHED_ENCODER_COLLECTION_DUMP'
PP_THREAD_SAFE FUNCTION CACHED_ENCODER_COLLECTION_DUMP( THIS, DUMP_PATH, CNT, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CACHED_ENCODER_COLLECTION_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                   INTENT(IN)    :: DUMP_PATH
  INTEGER(KIND=JPIB_K),               INTENT(INOUT) :: CNT
  TYPE(GRIB_ENCODER_OPTIONS_T),       INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODERS_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_NESTED_DUMP=2_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%ENCODERS_), ERRFLAG_ENCODERS_NOT_ASSOCIATED )

  ! Dump the encoders
  DO I = 1, SIZE(THIS%ENCODERS_)
    CNT = CNT + 1
    PP_TRYCALL(ERRFLAG_CALL_NESTED_DUMP) THIS%ENCODERS_(I)%DUMP( DUMP_PATH, CNT, OPT, HOOKS )
  ENDDO

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
    CASE(ERRFLAG_ENCODERS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoders not associated' )
    CASE(ERRFLAG_CALL_NESTED_DUMP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in nested call' )
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


END FUNCTION CACHED_ENCODER_COLLECTION_DUMP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CACHED_ENCODER_COLLECTION_ENCODE'
PP_THREAD_SAFE FUNCTION CACHED_ENCODER_COLLECTION_ENCODE( THIS, ID, &
&       MSG, PAR, TAG, NAME, METADATA, ENCODING_DONE, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: ENCODING_RULE_MOD,        ONLY: ENCODING_RULE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CACHED_ENCODER_COLLECTION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),               INTENT(IN)    :: ID
  TYPE(FORTRAN_MESSAGE_T),            INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),            INTENT(IN)    :: PAR
  CHARACTER(LEN=256),                 INTENT(OUT)   :: TAG
  CHARACTER(LEN=256),                 INTENT(OUT)   :: NAME
  CLASS(METADATA_BASE_A), POINTER,    INTENT(OUT)   :: METADATA
  LOGICAL,                            INTENT(OUT)   :: ENCODING_DONE
  TYPE(GRIB_ENCODER_OPTIONS_T),       INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODERS_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ID_LESS_THAN_ZERO=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ID_GREATER_THAN_SIZE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ENCODE=4_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%ENCODERS_), ERRFLAG_ENCODERS_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( ID .LE. 0, ERRFLAG_ID_LESS_THAN_ZERO )
  PP_DEBUG_CRITICAL_COND_THROW( ID .GT. SIZE(THIS%ENCODERS_), ERRFLAG_ID_GREATER_THAN_SIZE )

  ! Encode the message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ENCODE) THIS%ENCODERS_(ID)%ENCODE( &
& MSG, PAR, TAG, NAME, METADATA, ENCODING_DONE, OPT, HOOKS )

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
    CASE(ERRFLAG_ENCODERS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoders not associated' )
    CASE(ERRFLAG_ID_LESS_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'ID less than zero' )
    CASE(ERRFLAG_ID_GREATER_THAN_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'ID greater than size' )
    CASE(ERRFLAG_UNABLE_TO_ENCODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to encode' )
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

END FUNCTION CACHED_ENCODER_COLLECTION_ENCODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CACHED_ENCODER_COLLECTION_FREE'
PP_THREAD_SAFE FUNCTION CACHED_ENCODER_COLLECTION_FREE( THIS, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CACHED_ENCODER_COLLECTION_T), INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T),       INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODERS_NOT_ASSOCIATED=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_ENCODERS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_ENCODERS=2_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%ENCODERS_), ERRFLAG_ENCODERS_NOT_ASSOCIATED )

  ! Free all the encoders
  DO I = 1, SIZE(THIS%ENCODERS_)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_ENCODERS) THIS%ENCODERS_(I)%FREE( OPT, HOOKS )
  ENDDO

  ! Release memory
  DEALLOCATE( THIS%ENCODERS_, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_ENCODERS )

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
    CASE(ERRFLAG_ENCODERS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoders not associated' )
    CASE(ERRFLAG_UNABLE_TO_FREE_ENCODERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to free encoders' )
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE_ENCODERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate encoders' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STATUS)
      ENDIF
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


END FUNCTION CACHED_ENCODER_COLLECTION_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE CACHED_ENCODER_COLLECTION_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME