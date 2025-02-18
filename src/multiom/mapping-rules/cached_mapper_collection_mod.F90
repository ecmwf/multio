
! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'cached_mapper_collection_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'CACHED_MAPPER_COLLECTION_MOD'
MODULE CACHED_MAPPER_COLLECTION_MOD

  ! Symbols imported from other modules within the project.
  USE :: CASHED_MAPPER_MOD, ONLY: CASHED_MAPPER_T

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

TYPE :: CACHED_MAPPER_COLLECTION_T

  !> Default visibility of the type
  PRIVATE

  !> Pointer to the class
  TYPE(CASHED_MAPPER_T), POINTER, DIMENSION(:) :: MAPPER_ => NULL()

CONTAINS

  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT => CACHED_MAPPER_COLLECTION_INIT
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: SIZE => CACHED_MAPPER_COLLECTION_SIZE
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: EVAL => CACHED_MAPPER_COLLECTION_EVAL
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => CACHED_MAPPER_COLLECTION_FREE
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRINT_RULES => CACHED_MAPPER_COLLECTION_PRINT_RULES

END TYPE

!> Whitelist of public symbols (types)
PUBLIC :: CACHED_MAPPER_COLLECTION_T

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME ' CACHED_MAPPER_COLLECTION_INIT'
PP_THREAD_SAFE FUNCTION  CACHED_MAPPER_COLLECTION_INIT( THIS, MAPPER, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPIB_K
  USE :: HOOKS_MOD,               ONLY: HOOKS_T
  USE :: CASHED_MAPPER_MOD,       ONLY: CASHED_MAPPER_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CACHED_MAPPER_COLLECTION_T),            INTENT(INOUT) :: THIS
  TYPE(CASHED_MAPPER_T), POINTER, DIMENSION(:), INTENT(IN)    :: MAPPER
  TYPE(HOOKS_T),                                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flag
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPER_NOT_ASSOCIATED=1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(MAPPER), ERRFLAG_MAPPER_NOT_ASSOCIATED )

  ! Associate the mapper
  THIS%MAPPER_ => MAPPER

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
    CASE(ERRFLAG_MAPPER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'input mapper array not associated' )
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


END FUNCTION  CACHED_MAPPER_COLLECTION_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME ' CACHED_MAPPER_COLLECTION_SIZE'
PP_THREAD_SAFE FUNCTION  CACHED_MAPPER_COLLECTION_SIZE( THIS, SZ, HOOKS ) RESULT(RET)

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
  CLASS(CACHED_MAPPER_COLLECTION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),              INTENT(OUT)   :: SZ
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flag
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPER_NOT_ASSOCIATED=1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%MAPPER_), ERRFLAG_MAPPER_NOT_ASSOCIATED )

  ! Associate the mapper
  SZ = SIZE(THIS%MAPPER_)

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
    CASE(ERRFLAG_MAPPER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'input mapper array not associated' )
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


END FUNCTION  CACHED_MAPPER_COLLECTION_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME ' CACHED_MAPPER_COLLECTION_PRINT_RULES'
PP_THREAD_SAFE FUNCTION  CACHED_MAPPER_COLLECTION_PRINT_RULES( THIS, UNIT, HOOKS ) RESULT(RET)

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
  CLASS(CACHED_MAPPER_COLLECTION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),              INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_EMAPPERS_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT=4_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%MAPPER_), ERRFLAG_EMAPPERS_NOT_ASSOCIATED )

  DO I = 1, SIZE(THIS%MAPPER_)
    ! Print the encoder
    PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT) THIS%MAPPER_(I)%PRINT_RULE( UNIT, HOOKS )
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
    CASE(ERRFLAG_EMAPPERS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Mappers not associated' )
    CASE(ERRFLAG_UNABLE_TO_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print' )
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


END FUNCTION  CACHED_MAPPER_COLLECTION_PRINT_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME ' CACHED_MAPPER_COLLECTION_EVAL'
PP_THREAD_SAFE FUNCTION  CACHED_MAPPER_COLLECTION_EVAL( THIS, IDX, &
&             IN_MSG, IN_PAR, OUT_MSG, OUT_PAR, TAG, NAME, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
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
  CLASS(CACHED_MAPPER_COLLECTION_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),              INTENT(IN)    :: IDX
  TYPE(FORTRAN_MESSAGE_T),           INTENT(IN)    :: IN_MSG
  TYPE(PARAMETRIZATION_T),           INTENT(IN)    :: IN_PAR
  TYPE(FORTRAN_MESSAGE_T),           INTENT(OUT)   :: OUT_MSG
  TYPE(PARAMETRIZATION_T),           INTENT(OUT)   :: OUT_PAR
  CHARACTER(LEN=256),                INTENT(OUT)   :: TAG
  CHARACTER(LEN=256),                INTENT(OUT)   :: NAME
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPER_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_INDEX=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_RULES_EVAL=3_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%MAPPER_), ERRFLAG_MAPPER_NOT_INITIALIZED )
  PP_DEBUG_CRITICAL_COND_THROW( IDX.LT.1 .OR. IDX.GT.SIZE(THIS%MAPPER_), ERRFLAG_INVALID_INDEX )

  ! Perform the mapping
  PP_TRYCALL(ERRFLAG_MAPPING_RULES_EVAL) THIS%MAPPER_(IDX)%EVAL( IN_MSG, IN_PAR, &
&   OUT_MSG, OUT_PAR, TAG, NAME, HOOKS )

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
    CASE(ERRFLAG_MAPPER_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'mapper not initialized' )
    CASE(ERRFLAG_INVALID_INDEX)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid index' )
    CASE(ERRFLAG_MAPPING_RULES_EVAL)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to evaluate the mapping rules' )
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


END FUNCTION  CACHED_MAPPER_COLLECTION_EVAL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME ' CACHED_MAPPER_COLLECTION_FREE'
PP_THREAD_SAFE FUNCTION  CACHED_MAPPER_COLLECTION_FREE( THIS, MAPPING_OPT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MAPPING_OPTIONS_MOD, ONLY: MAPPING_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(CACHED_MAPPER_COLLECTION_T), INTENT(INOUT) :: THIS
  TYPE(MAPPING_OPTIONS_T),           INTENT(IN)    :: MAPPING_OPT
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: DEALLOC_ERROR
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_CACHED_MAPPER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOC_ERROR=2_JPIB_K

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

  ! Reset the object
  IF ( ASSOCIATED(THIS%MAPPER_) ) THEN
    DO I = 1, SIZE(THIS%MAPPER_)
      PP_TRYCALL(ERRFLAG_FREE_CACHED_MAPPER) THIS%MAPPER_(I)%FREE( MAPPING_OPT, HOOKS )
    ENDDO
    DEALLOCATE( THIS%MAPPER_, STAT=DEALLOC_ERROR, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_ERROR.NE.0, ERRFLAG_FREE_CACHED_MAPPER )
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
    CASE(ERRFLAG_FREE_CACHED_MAPPER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free the cached mapper' )
    CASE(ERRFLAG_DEALLOC_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate the cached mapper' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: '//TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_ERROR )
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


END FUNCTION  CACHED_MAPPER_COLLECTION_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE CACHED_MAPPER_COLLECTION_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME