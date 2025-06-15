!>
!> @file filter_level_mod.F90
!>
!> @brief Module containing definitions and procedures for level filters.
!>
!> This module defines the `FILTER_LEVELIST_T` type, along with its associated
!> procedures and helper functions that facilitate the creation, management, and
!> utilization of level filters within the system. Level filters allow for
!> complex filtering operations by combining multiple nested filters.
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'multiom_raw_encoder_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MULTIOM_RAW_ENCODER_MOD'
MODULE MULTIOM_RAW_ENCODER_MOD

  ! Symbols imported from other modules within the project.
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_BASE_A

IMPLICIT NONE

!> Default visibility of the module
PRIVATE

!> Type used to store a grib encoder
TYPE :: MULTIOM_RAW_ENCODER_T

  !> Default visibility of the type
  PRIVATE

  !> Options
  TYPE(GRIB_ENCODER_OPTIONS_T) :: ENCODER_OPT_

  !> Pointer to the class
  CLASS(GRIB_SECTION_BASE_A), POINTER :: ENCODER_ => NULL()

CONTAINS

  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: INIT     => MULTIOM_RAW_ENCODER_INIT

  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: RUNTIME  => MULTIOM_RAW_ENCODER_RUNTIME

  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: PRINT    => MULTIOM_RAW_ENCODER_PRINT

  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS :: FREE     => MULTIOM_RAW_ENCODER_FREE

END TYPE

!> Whitelist of public symbols
PUBLIC :: MULTIOM_RAW_ENCODER_T

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIOM_RAW_ENCODER_INIT'
PP_THREAD_SAFE FUNCTION MULTIOM_RAW_ENCODER_INIT( THIS, &
&  MSG, PAR, ENCODER_CFG, ENCODER_OPT, GRIB_SAMPLE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIM_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: GRIB_ENCODER_FACTORY_MOD, ONLY: MAKE_ENCODER
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  uSE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: GRIB_METADATA_MOD,        ONLY: GRIB_METADATA_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(MULTIOM_RAW_ENCODER_T), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: ENCODER_CFG
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: ENCODER_OPT
  INTEGER(KIND=JPIM_K),         INTENT(INOUT) :: GRIB_SAMPLE
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(GRIB_METADATA_T), TARGET :: METADATA
  CLASS(METADATA_BASE_A), POINTER :: P_METADATA
  INTEGER(KINd=JPIM_K) :: LOC_GRIB_SAMPLE

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_ALREADY_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_ENCODER = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BIND_METADATA = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PERFORM_ALLOCATE_STAGE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SAFE_LOAD1 = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PERFORM_PRESET_STAGE = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SAFE_LOAD2 = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_TAKE_HANDLE = 8_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%ENCODER_), ERRFLAG_ENCODER_ALREADY_ASSOCIATED )

  ! Copy options
  THIS%ENCODER_OPT_ = ENCODER_OPT
  LOC_GRIB_SAMPLE = GRIB_SAMPLE
  GRIB_SAMPLE = -1_JPIM_K

  !> Make the encoder (recursively go through the configuration an create all the nested objects)
  PP_LOG_DEVELOP_STR( ' * Make encoder' )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_ENCODER) MAKE_ENCODER( &
&   THIS%ENCODER_, ENCODER_CFG, THIS%ENCODER_OPT_, HOOKS )

  !> Bind the encoder to the sample
  PP_LOG_DEVELOP_STR( ' * Bind the encoder to the sample...' )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BIND_METADATA) METADATA%BIND_HANDLE( LOC_GRIB_SAMPLE, HOOKS )
  P_METADATA => METADATA

  ! Preconfigure the local metadata with all the memory related information
  PP_LOG_DEVELOP_STR( ' * Allocate the sample...' )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PERFORM_ALLOCATE_STAGE) THIS%ENCODER_%ALLOCATE( MSG, PAR, THIS%ENCODER_OPT_, P_METADATA, HOOKS )

  PP_LOG_DEVELOP_STR( ' * Safe reload of sample after allocation...' )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SAFE_LOAD1) METADATA%SAFE_LOAD( HOOKS )

  PP_LOG_DEVELOP_STR( ' * Preset the sample' )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PERFORM_PRESET_STAGE) THIS%ENCODER_%PRESET( MSG, PAR, THIS%ENCODER_OPT_, P_METADATA, HOOKS )

  PP_LOG_DEVELOP_STR( ' * Safe reload of sample after preset...' )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SAFE_LOAD2) METADATA%SAFE_LOAD( HOOKS )

  PP_LOG_DEVELOP_STR( ' * Take the sample from the metadata...' )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_TAKE_HANDLE) METADATA%GET_HANDLE( GRIB_SAMPLE, HOOKS, TAKE=.TRUE. )

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
    CASE (ERRFLAG_ENCODER_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder is already associated' )
    CASE (ERRFLAG_UNABLE_TO_MAKE_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to make the encoder' )
    CASE (ERRFLAG_UNABLE_TO_BIND_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to bind the encoder to the sample' )
    CASE (ERRFLAG_UNABLE_TO_PERFORM_ALLOCATE_STAGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate the encoder' )
    CASE (ERRFLAG_UNABLE_TO_SAFE_LOAD1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to safe load the sample after allocation' )
    CASE (ERRFLAG_UNABLE_TO_PERFORM_PRESET_STAGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to preset the sample' )
    CASE (ERRFLAG_UNABLE_TO_SAFE_LOAD2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to safe load the sample after preset' )
    CASE (ERRFLAG_UNABLE_TO_TAKE_HANDLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to take the sample from the metadata' )
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


END FUNCTION MULTIOM_RAW_ENCODER_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIOM_RAW_ENCODER_RUNTIME'
PP_THREAD_SAFE FUNCTION MULTIOM_RAW_ENCODER_RUNTIME( THIS, &
&    MSG, PAR, GRIB_SAMPLE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIM_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: GRIB_METADATA_MOD,   ONLY: GRIB_METADATA_T
  uSE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

  ! Dummy objects to be removed in the future
  USE :: TIME_UTILS_MOD, ONLY: TIME_HISTORY_T
  USE :: TIME_UTILS_MOD, ONLY: CURR_TIME_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(MULTIOM_RAW_ENCODER_T), INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  INTEGER(KIND=JPIM_K),         INTENT(IN)    :: GRIB_SAMPLE
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  TYPE(GRIB_METADATA_T), TARGET :: METADATA
  CLASS(METADATA_BASE_A), POINTER :: P_METADATA
  TYPE(TIME_HISTORY_T) :: TIME_HIST
  TYPE(CURR_TIME_T) :: CURR_TIME

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_NOT_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BIND_METADATA = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PERFORM_RUNTIME_STAGE = 3_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%ENCODER_), ERRFLAG_ENCODER_NOT_ASSOCIATED )

  !> Bind the sample to the metadata
  PP_LOG_DEVELOP_STR( ' * Bind metadata...' )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BIND_METADATA) METADATA%BIND_HANDLE( GRIB_SAMPLE, HOOKS )
  P_METADATA => METADATA

  !> Allocate phase of the sample
  PP_LOG_DEVELOP_STR( ' * Perform the runtime stage of the encoder...' )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PERFORM_RUNTIME_STAGE) THIS%ENCODER_%RUNTIME( &
&   MSG, PAR, TIME_HIST, CURR_TIME, THIS%ENCODER_OPT_, P_METADATA, HOOKS )

  !> N.B. No need to call metadata%free because the sample need to survive

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
    CASE (ERRFLAG_ENCODER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder is not associated' )
    CASE (ERRFLAG_UNABLE_TO_BIND_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to bind the metadata to the sample' )
    CASE (ERRFLAG_UNABLE_TO_PERFORM_RUNTIME_STAGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to perform the runtime stage of the encoder' )
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


END FUNCTION MULTIOM_RAW_ENCODER_RUNTIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME ' MULTIOM_RAW_ENCODER_PRINT'
PP_THREAD_SAFE FUNCTION MULTIOM_RAW_ENCODER_PRINT( THIS, UNIT, OFFSET, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,    ONLY: JPIB_K
  USE :: HOOKS_MOD,            ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(MULTIOM_RAW_ENCODER_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: OFFSET
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_NOT_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_ENCODER = 2_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%ENCODER_), ERRFLAG_ENCODER_NOT_ASSOCIATED )

  !> Print all the rules
  PP_TRYCALL( ERRFLAG_UNABLE_TO_PRINT_ENCODER ) THIS%ENCODER_%PRINT( &
&    UNIT, OFFSET, THIS%ENCODER_OPT_, HOOKS )

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
    CASE (ERRFLAG_ENCODER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder is not associated' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the encoder' )
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


END FUNCTION MULTIOM_RAW_ENCODER_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIOM_RAW_ENCODER_FREE'
PP_THREAD_SAFE FUNCTION MULTIOM_RAW_ENCODER_FREE( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_FACTORY_MOD, ONLY: DESTROY_ENCODER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(MULTIOM_RAW_ENCODER_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENCODER_NOT_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEESTROY_ENCODER = 2_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(THIS%ENCODER_), ERRFLAG_ENCODER_NOT_ASSOCIATED )

  !> Deallocate the encoder
  PP_LOG_DEVELOP_STR( ' * Destroy the encoder...' )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DEESTROY_ENCODER) DESTROY_ENCODER( THIS%ENCODER_, THIS%ENCODER_OPT_, HOOKS )

  !> Nullify the encoder pointer
  THIS%ENCODER_ => NULL()

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
    CASE (ERRFLAG_ENCODER_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Encoder is not associated' )
    CASE (ERRFLAG_UNABLE_TO_DEESTROY_ENCODER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to destroy the encoder' )
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


END FUNCTION MULTIOM_RAW_ENCODER_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'LOAD_SAMPLE'
PP_THREAD_SAFE FUNCTION LOAD_SAMPLE( MSG, GRIB_SAMPLE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,             ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,             ONLY: JPIM_K
  USE :: HOOKS_MOD,                     ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD,           ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(FORTRAN_MESSAGE_T),       INTENT(IN)    :: MSG
  INTEGER(KIND=JPIM_K),          INTENT(OUT)   :: GRIB_SAMPLE
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

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


END FUNCTION LOAD_SAMPLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE MULTIOM_RAW_ENCODER_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
