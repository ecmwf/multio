
! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'cached_encoder_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'CACHED_ENCODER_MOD'
MODULE CACHED_ENCODER_MOD

  ! Symbols imported from other modules within the project.
  USE :: METADATA_BASE_MOD,     ONLY: METADATA_BASE_A
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A
  USE :: TIME_UTILS_MOD,        ONLY: TIME_HISTORY_T
  USE :: SAMPLE_MOD,            ONLY: SAMPLE_T

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

TYPE :: CACHED_ENCODER_T

  !> Default visibility of the type
  PRIVATE

  !> Rule applied
  CHARACTER(LEN=256) :: TAG_=REPEAT(' ',256)
  CHARACTER(LEN=256) :: NAME_=REPEAT(' ',256)
  LOGICAL :: TO_BE_DEALLOCATED_=.FALSE.

  !> Sample pointer
  TYPE(SAMPLE_T), POINTER :: SAMPLE_ => NULL()

  !> Circular buffer used to store the time history
  TYPE(TIME_HISTORY_T) :: TIME_HISTORY_

  !> Metadata associated with the encoder
  CLASS(METADATA_BASE_A), POINTER :: METADATA_ => NULL()

  !> Pointer to the class
  CLASS(GRIB_SECTION_BASE_A), POINTER :: ENCODER_ => NULL()

CONTAINS
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT     =>  CACHED_ENCODER_INIT
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ENCODE   =>  CACHED_ENCODER_ENCODE
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: DUMP     =>  CACHED_ENCODER_DUMP
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: BYTESIZE =>  CACHED_ENCODER_BYTESIZE
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE     =>  CACHED_ENCODER_FREE
END TYPE

!> Whitelist of public symbols (types)
PUBLIC :: CACHED_ENCODER_T

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME ' CACHED_ENCODER_INIT'
PP_THREAD_SAFE FUNCTION  CACHED_ENCODER_INIT( THIS, MSG, PAR, TAG, NAME, &
&         METADATA, SAMPLE, ENCODER, TO_BE_DEALLOCATED, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_BASE_A
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: METADATA_FACTORY_MOD,     ONLY: MAKE_METADATA
  USE :: SAMPLE_MOD,               ONLY: SAMPLE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CACHED_ENCODER_T),             INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),             INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),             INTENT(IN)    :: PAR
  CHARACTER(LEN=*),                    INTENT(IN)    :: TAG
  CHARACTER(LEN=*),                    INTENT(IN)    :: NAME
  CLASS(METADATA_BASE_A), POINTER,     INTENT(IN)    :: METADATA
  TYPE(SAMPLE_T), POINTER,             INTENT(IN)    :: SAMPLE
  CLASS(GRIB_SECTION_BASE_A), POINTER, INTENT(IN)    :: ENCODER
  LOGICAL,                             INTENT(IN)    :: TO_BE_DEALLOCATED
  TYPE(GRIB_ENCODER_OPTIONS_T),        INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flag
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CLONE_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRESET=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INITIALIZE_TIME_HISTORY=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA_ALREADY_ALLOCATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_ALREADY_ALLOCATED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_IMPLEMENTED=7_JPIB_K

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

  ! Set the tag and name
  THIS%TAG_ = TAG
  THIS%NAME_ = NAME
  THIS%TO_BE_DEALLOCATED_ = TO_BE_DEALLOCATED

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%METADATA_), ERRFLAG_METADATA_ALREADY_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%ENCODER_),  ERRFLAG_ENCODER_ALREADY_ALLOCATED  )


  IF ( ASSOCIATED(THIS%SAMPLE_) ) THEN
    ! Call the Clone constructor of metadata
    ! Here we clone a metadata object passed from the rule.
    PP_TRYCALL(ERRFLAG_CLONE_METADATA) MAKE_METADATA( METADATA, THIS%METADATA_, TRIM(THIS%SAMPLE_%NAME_), HOOKS)
    ! PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_IMPLEMENTED )
  ELSE
    ! Call the Clone constructor of metadata
    ! Here we clone a metadata object passed as input.
    PP_TRYCALL(ERRFLAG_CLONE_METADATA) MAKE_METADATA( METADATA, THIS%METADATA_, HOOKS)
  ENDIF


  ! Associate the encoder
  THIS%SAMPLE_ => SAMPLE
  THIS%ENCODER_ => ENCODER

  ! Preconfigure the local metadata with all the memory related information
  PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE) THIS%ENCODER_%ALLOCATE( MSG, PAR, OPT, THIS%METADATA_, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRESET) THIS%ENCODER_%PRESET( MSG, PAR, OPT, THIS%METADATA_, HOOKS )

  ! Initialize the time history
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INITIALIZE_TIME_HISTORY) THIS%TIME_HISTORY_%INIT( OPT%TIME_HISTORY_CAPACITY, HOOKS)

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
    CASE(ERRFLAG_CLONE_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error cloning metadata' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating encoder' )
    CASE(ERRFLAG_UNABLE_TO_PRESET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error presetting encoder' )
    CASE(ERRFLAG_UNABLE_TO_INITIALIZE_TIME_HISTORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error initializing time history' )
    CASE(ERRFLAG_METADATA_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'metadata already allocated' )
    CASE(ERRFLAG_ENCODER_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'encoder already allocated' )
    CASE(ERRFLAG_NOT_IMPLEMENTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'not implemented' )
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


END FUNCTION  CACHED_ENCODER_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME ' CACHED_ENCODER_ENCODE'
PP_THREAD_SAFE FUNCTION  CACHED_ENCODER_ENCODE( THIS, MSG, PAR, TAG, NAME, METADATA, ENCODING_DONE, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: TIME_UTILS_MOD,           ONLY: CURR_TIME_T
  USE :: TIME_UTILS_MOD,           ONLY: COMPUTE_CURRENT_TIME
  USE :: METADATA_FACTORY_MOD,     ONLY: MAKE_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CACHED_ENCODER_T),         INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  CHARACTER(LEN=256),              INTENT(OUT)   :: TAG
  CHARACTER(LEN=256),              INTENT(OUT)   :: NAME
  CLASS(METADATA_BASE_A), POINTER, INTENT(OUT)   :: METADATA
  LOGICAL,                         INTENT(OUT)   :: ENCODING_DONE
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: TO_BE_ENCODED
  TYPE(CURR_TIME_T) :: CURR_TIME

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_NOT_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_METADATA_ALREADY_ALLOCATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COMPUTE_CURRENT_TIME=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_TO_BE_ENCODED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CLONE_METADATA=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_ENCODE=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_UPDATE_TIME_HISTORY=8_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%METADATA_), ERRFLAG_METADATA_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%ENCODER_),  ERRFLAG_ENCODER_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(METADATA), ERRFLAG_OUTPUT_METADATA_ALREADY_ALLOCATED )

  ! Return tag/name
  TAG = THIS%TAG_
  NAME = THIS%NAME_

  ! Compute current time
  PP_TRYCALL(ERRFLAG_UNABLE_TO_COMPUTE_CURRENT_TIME) COMPUTE_CURRENT_TIME( &
&       MSG, PAR, THIS%TIME_HISTORY_, CURR_TIME, OPT, HOOKS )

  ! Check if the field has to be encoded
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_TO_BE_ENCODED) THIS%ENCODER_%TO_BE_ENCODED( &
&    MSG, PAR, THIS%TIME_HISTORY_, CURR_TIME, OPT, TO_BE_ENCODED, HOOKS )

  ! If needed then encode the field
  IF ( TO_BE_ENCODED ) THEN

    ! Clone the metadata
    PP_TRYCALL(ERRFLAG_CLONE_METADATA) MAKE_METADATA( THIS%METADATA_, METADATA, HOOKS)

    ! Encode the field
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_ENCODE) THIS%ENCODER_%RUNTIME( &
&      MSG, PAR, THIS%TIME_HISTORY_, CURR_TIME, OPT, THIS%METADATA_, HOOKS )

    ! Update the time history
    PP_TRYCALL(ERRFLAG_UNABLE_TO_UPDATE_TIME_HISTORY) THIS%TIME_HISTORY_%ENQUEUE( &
&      CURR_TIME, HOOKS )

    ! Set the encoding done flag
    ENCODING_DONE = .TRUE.

  ELSE

    ! Set the encoding done flag
    ENCODING_DONE = .FALSE.

  ENDIF

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
    CASE(ERRFLAG_METADATA_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'metadata not allocated' )
    CASE(ERRFLAG_ENCODER_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'encoder not allocated' )
    CASE(ERRFLAG_OUTPUT_METADATA_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'output metadata already allocated' )
    CASE(ERRFLAG_UNABLE_TO_COMPUTE_CURRENT_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error computing current time' )
    CASE(ERRFLAG_UNABLE_TO_CALL_TO_BE_ENCODED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling to_be_encoded' )
    CASE(ERRFLAG_CLONE_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error cloning metadata' )
    CASE(ERRFLAG_UNABLE_TO_CALL_ENCODE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling encode' )
    CASE(ERRFLAG_UNABLE_TO_UPDATE_TIME_HISTORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error updating time history' )
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


END FUNCTION  CACHED_ENCODER_ENCODE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME ' CACHED_ENCODER_DUMP'
PP_THREAD_SAFE FUNCTION  CACHED_ENCODER_DUMP( THIS, DUMP_PATH, CNT, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CACHED_ENCODER_T),      INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),             INTENT(IN)    :: DUMP_PATH
  INTEGER(KIND=JPIB_K),         INTENT(INOUT) :: CNT
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

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

  ! TODO Dump the metadata!!!!

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


END FUNCTION  CACHED_ENCODER_DUMP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME ' CACHED_ENCODER_BYTESIZE'
PP_THREAD_SAFE FUNCTION  CACHED_ENCODER_BYTESIZE( THIS, MEMORY_BYTESIZE, OPT, HOOKS ) RESULT(RET)

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
  CLASS(CACHED_ENCODER_T),      INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(OUT)   :: MEMORY_BYTESIZE
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: TMP_MEMORY_BYTESIZE

  !> Local error flag
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COMPUTE_CIRCULAR_BUFFER_BYTESIZE=1_JPIB_K

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

  ! Initialize the memory bytesize
  MEMORY_BYTESIZE = 256 + 256 + 8 + 8

  ! Add the circular buffer byte size
  PP_TRYCALL(ERRFLAG_UNABLE_TO_COMPUTE_CIRCULAR_BUFFER_BYTESIZE) THIS%TIME_HISTORY_%BYTESIZE(TMP_MEMORY_BYTESIZE, HOOKS)
  MEMORY_BYTESIZE = MEMORY_BYTESIZE + TMP_MEMORY_BYTESIZE

  ! TODO: Add size of metadata
  ! PP_TRYCALL(ERRFLAG_UNABLE_TO_COMPUTE_METADATA_BYTESIZE) THIS%METADATA_%BYTESIZE(TMP_MEMORY_BYTESIZE, HOOKS)

  ! TODO: Add size of encoder (if needed)

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
    CASE(ERRFLAG_UNABLE_TO_COMPUTE_CIRCULAR_BUFFER_BYTESIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error unable to compute circular buffer byte size' )
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


END FUNCTION  CACHED_ENCODER_BYTESIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME ' CACHED_ENCODER_FREE'
PP_THREAD_SAFE FUNCTION  CACHED_ENCODER_FREE( THIS, OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: GRIB_ENCODER_FACTORY_MOD, ONLY: DESTROY_ENCODER
  USE :: METADATA_FACTORY_MOD,     ONLY: DESTROY_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CACHED_ENCODER_T),      INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flag
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_ENCODER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_METADATA=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FREE_TIME_HISTORY=3_JPIB_K

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

  !> If necessary deallocate the encoder
  IF ( THIS%TO_BE_DEALLOCATED_ .AND. ASSOCIATED(THIS%ENCODER_) ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_DESTROY_ENCODER) DESTROY_ENCODER( THIS%ENCODER_, OPT, HOOKS )
  ELSE
    THIS%ENCODER_ => NULL()
  ENDIF

  ! Deallocate the metadata
  IF ( ASSOCIATED(THIS%METADATA_) ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_DESTROY_METADATA) DESTROY_METADATA( THIS%METADATA_, HOOKS )
  ENDIF

  !> TODO: Free the time history
  PP_TRYCALL(ERRFLAG_UNABLE_TO_FREE_TIME_HISTORY) THIS%TIME_HISTORY_%FREE(HOOKS)

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
    CASE(ERRFLAG_UNABLE_TO_DESTROY_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error destroying encoder' )
    CASE(ERRFLAG_UNABLE_TO_DESTROY_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error destroying metadata' )
    CASE(ERRFLAG_UNABLE_TO_FREE_TIME_HISTORY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error freeing time history' )
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


END FUNCTION  CACHED_ENCODER_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE CACHED_ENCODER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME