!> @file map_mod.F90
!>
!> @brief Module containing the implementation of a Red Black tree.
!>
!> Implementation of a Red Black tree.
!> Every routine in this file is deeply explained in the book:
!> "Introduction to Algorithms"
!> { Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, Clifford Stein}
!>
!> @todo improve error handling
!>
!> @author Mirco Valentini
!> @date   January 31, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'encoding_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ENCODING_UTILS_MOD'
MODULE ENCODING_UTILS_MOD

IMPLICIT NONE

!> @brief Default visibility of the module
PRIVATE

!> Whitelist of public symbols
PUBLIC :: MAKE_ENCODER_COLLECTION

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_ENCODER_COLLECTION'
PP_THREAD_SAFE FUNCTION MAKE_ENCODER_COLLECTION( MSG, PAR, &
        METADATA, ENCODING_RULES, ENCODER_OPTIONS, FILTER_OPTIONS, ENCODERS_COLLECTION, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,             ONLY: JPIB_K
  USE :: HOOKS_MOD,                     ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD,      ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FILTER_OPTIONS_MOD,            ONLY: FILTER_OPTIONS_T
  USE :: PARAMETRIZATION_MOD,           ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,           ONLY: FORTRAN_MESSAGE_T
  USE :: ENCODING_RULE_COLLECTION_MOD,  ONLY: ENCODING_RULE_COLLECTION_T
  USE :: CACHED_ENCODER_COLLECTION_MOD, ONLY: CACHED_ENCODER_COLLECTION_T
  USE :: METADATA_BASE_MOD,             ONLY: METADATA_BASE_A
  USE :: CACHED_ENCODER_MOD,            ONLY: CACHED_ENCODER_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FORTRAN_MESSAGE_T),           INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),           INTENT(IN)    :: PAR
  CLASS(METADATA_BASE_A), POINTER,   INTENT(IN)    :: METADATA
  TYPE(ENCODING_RULE_COLLECTION_T),  INTENT(IN)    :: ENCODING_RULES
  TYPE(FILTER_OPTIONS_T),            INTENT(IN)    :: FILTER_OPTIONS
  TYPE(GRIB_ENCODER_OPTIONS_T),      INTENT(IN)    :: ENCODER_OPTIONS
  TYPE(CACHED_ENCODER_COLLECTION_T), INTENT(OUT)   :: ENCODERS_COLLECTION
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_INITIALIZED
  TYPE(CACHED_ENCODER_T), DIMENSION(:), POINTER :: ENCODERS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_IS_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_RESET=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MATCH_RULE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INITIALIZE_ENCODERS=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODERS_NOT_ASSOCIATED=5_JPIB_K


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

  !> Reset the encoder collection if needed
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_IS_INITIALIZED) ENCODERS_COLLECTION%IS_INITIALIZED( IS_INITIALIZED, HOOKS )

  !> Reset the encoder if needed
  IF ( IS_INITIALIZED ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_RESET) ENCODERS_COLLECTION%FREE( ENCODER_OPTIONS, HOOKS )
  ENDIF

  !> Get the encoders
  ENCODERS => NULL()
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MATCH_RULE) ENCODING_RULES%MATCH( &
&  MSG, PAR, METADATA, ENCODERS, ENCODER_OPTIONS, HOOKS )

  !> Check if the encoders are not empty
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(ENCODERS), ERRFLAG_ENCODERS_NOT_ASSOCIATED )

  !> Encoder initialization
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INITIALIZE_ENCODERS) ENCODERS_COLLECTION%INIT( &
&  MSG, PAR, METADATA, ENCODERS, ENCODER_OPTIONS, HOOKS )
  ENCODERS => NULL()

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CALL_IS_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call ISINITIALIZED' )
    CASE (ERRFLAG_UNABLE_TO_CALL_RESET)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to call RESET' )
    CASE (ERRFLAG_UNABLE_TO_MATCH_RULE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to match rule' )
    CASE (ERRFLAG_UNABLE_TO_INITIALIZE_ENCODERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to initialize encoders' )
    CASE (ERRFLAG_ENCODERS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to match rule' )
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

END FUNCTION MAKE_ENCODER_COLLECTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE ENCODING_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME