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
#define PP_FILE_NAME 'mapping_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MAPPING_UTILS_MOD'
MODULE MAPPING_UTILS_MOD

IMPLICIT NONE

!> @brief Default visibility of the module
PRIVATE

!> Whitelist of public symbols
PUBLIC :: MAKE_MAPPERS_COLLECTION

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_MAPPERS_COLLECTION'
PP_THREAD_SAFE FUNCTION MAKE_MAPPERS_COLLECTION( MSG, PAR, &
        MAPPING_RULES, MAPPING_OPT, FILTER_OPT, MAPPERS_COLLECTION, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,            ONLY: JPIB_K
  USE :: HOOKS_MOD,                    ONLY: HOOKS_T
  USE :: MAPPING_OPTIONS_MOD,          ONLY: MAPPING_OPTIONS_T
  USE :: FILTER_OPTIONS_MOD,           ONLY: FILTER_OPTIONS_T
  USE :: PARAMETRIZATION_MOD,          ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,          ONLY: FORTRAN_MESSAGE_T
  USE :: MAPPING_RULES_COLLECTION_MOD, ONLY: MAPPING_RULES_COLLECTION_T
  USE :: CACHED_MAPPER_COLLECTION_MOD, ONLY: CACHED_MAPPER_COLLECTION_T
  USE :: CASHED_MAPPER_MOD,            ONLY: CASHED_MAPPER_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FORTRAN_MESSAGE_T),          INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),          INTENT(IN)    :: PAR
  TYPE(MAPPING_RULES_COLLECTION_T), INTENT(IN)    :: MAPPING_RULES
  TYPE(MAPPING_OPTIONS_T),          INTENT(IN)    :: MAPPING_OPT
  TYPE(FILTER_OPTIONS_T),           INTENT(IN)    :: FILTER_OPT
  TYPE(CACHED_MAPPER_COLLECTION_T), INTENT(OUT)   :: MAPPERS_COLLECTION
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_INITIALIZED
  TYPE(CASHED_MAPPER_T), POINTER, DIMENSION(:) :: MAPPING_INFO

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_MAPPING = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_COLLECTION_INIT = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAPPING_RULES_MATCH = 3_JPIB_K

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

  !> Apply filter to a message/parameterization
  MAPPING_INFO => NULL()
  PP_TRYCALL(ERRFLAG_MAPPING_RULES_MATCH) MAPPING_RULES%MATCH( MSG, PAR, FILTER_OPT, MAPPING_INFO, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(MAPPING_INFO), ERRFLAG_UNABLE_TO_CREATE_MAPPING )

  !> Check if we have any mapping info
  PP_TRYCALL(ERRFLAG_MAPPING_COLLECTION_INIT) MAPPERS_COLLECTION%INIT( MAPPING_INFO, HOOKS )
  MAPPING_INFO => NULL()

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
    CASE(ERRFLAG_UNABLE_TO_CREATE_MAPPING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to create mapping' )
    CASE(ERRFLAG_MAPPING_COLLECTION_INIT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'mapping collection initialization' )
    CASE(ERRFLAG_MAPPING_RULES_MATCH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'mapping rules match' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION MAKE_MAPPERS_COLLECTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE MAPPING_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME