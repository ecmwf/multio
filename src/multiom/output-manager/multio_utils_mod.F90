!> @file
!>
!> @brief Module containing utilities for calling MultIO.
!>
!> This module provides utilities wrap the calls to MultIO
!>
!> @author Philipp Geier (ECMWF)
!> @author Mirco Valentini
!> @date   January 31, 2024
!>


! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'multio_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MULTIO_UTILS_MOD'
MODULE MULTIO_UTILS_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: MULTIO_NEW
PUBLIC :: MULTIO_DELETE
PUBLIC :: MULTIO_WRITE_BINARY_GRIB
PUBLIC :: MULTIO_WRITE_VALUES_DP
PUBLIC :: MULTIO_WRITE_VALUES_SP
PUBLIC :: MULTIO_SEND_PARAMETRISATION
PUBLIC :: MULTIO_FILL_MARS_METADATA
PUBLIC :: MULTIO_INJECT_PARAMETERS
PUBLIC :: MULTIO_FLUSH_FIRST_STEP
PUBLIC :: MULTIO_FLUSH
PUBLIC :: MULTIO_FLUSH_AND_TRIGGER_RESTART
PUBLIC :: MULTIO_FLUSH_LAST_STEP
PUBLIC :: MULTIO_FLUSH_END_OF_SIMULATION
PUBLIC :: MULTIO_NOTIFY_STEP

CONTAINS


!>
!> @brief initialise a new MultIO handle.
!>
!> This function is used initialise a new multio handle.
!>  - read environment variable [\"MULTIO_IFSIO_CONFIG_FILE\"|\"MULTIO_PLANS_FILE\"]
!>    where the name of the configuration file is defined
!>  - create a new multio_configuration object, used to wrap all the
!>    information needed to create a MultIO handle
!>  - set the failure handler function
!>  - create the MultIO handle
!>
!> @param [inout] mio_handle A MultIO handle
!>
!> @see MULTIO_DELETE
!> @see MULTIO_ERROR_HANDLER
!> @see MULTIO_GET_CONFIG_FILE_NAME
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_NEW'
PP_THREAD_SAFE FUNCTION MULTIO_NEW( MIO_HANDLE, CONFIG_FILE, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from external libraries.
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_CONFIGURATION
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_ERROR_STRING
  USE :: MULTIO_API,  ONLY: FAILURE_HANDLER_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE
  CHARACTER(LEN=*),    INTENT(IN)    :: CONFIG_FILE
  TYPE(HOOKS_T),       INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variabels
  TYPE(MULTIO_CONFIGURATION) :: MIOCC
  INTEGER(KIND=JPIM_K) :: ERR
  INTEGER(KIND=JPIM_K) :: STAT
  INTEGER(KIND=INT64)  :: ERR_CONTEXT
  PROCEDURE(FAILURE_HANDLER_T), POINTER :: PF

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_MIOCFG=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_FAILURE_HDL=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_MIOHDL=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_MIOCFG=4_JPIB_K

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

  ! Create the MultIO configuration object
  ERR = MIOCC%NEW( TRIM(ADJUSTL(CONFIG_FILE)) )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_CREATE_MIOCFG )

  ! Setup the failure handler
  PF => MULTIO_ERROR_HANDLER
  ERR = MIOCC%SET_FAILURE_HANDLER( PF, ERR_CONTEXT )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_SET_FAILURE_HDL )

  ! Create the MultIO handle
  ERR = MIO_HANDLE%NEW( MIOCC )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_CREATE_MIOHDL )

  ! Destroy the MultIO configuration object
  ERR = MIOCC%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_DESTROY_MIOCFG )

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
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CREATE_MIOCFG)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create MultIO config from file' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_SET_FAILURE_HDL)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create MultIO config' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_CREATE_MIOHDL)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create MultIO handle' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'config file: '//TRIM(ADJUSTL(CONFIG_FILE)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_DESTROY_MIOCFG)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to destroy MultIO configuration handle' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'config file: '//TRIM(ADJUSTL(CONFIG_FILE)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
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

END FUNCTION MULTIO_NEW
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief finalise a new MultIO handle.
!>
!> This function is used finalise a multio handle.
!>
!> @param [inout] mio_handle A MultIO handle
!>
!> @see MULTIO_NEW
!> @see MULTIO_ERROR_HANDLER
!> @see MULTIO_GET_CONFIG_FILE_NAME
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_DELETE'
PP_THREAD_SAFE FUNCTION MULTIO_DELETE( MIO_HANDLE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from external libraries.
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE
  TYPE(HOOKS_T),       INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR
  INTEGER(KIND=JPIM_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_MIOHDL=1_JPIB_K

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

  ! Delete the multio handle
  ERR = MIO_HANDLE%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_DESTROY_MIOHDL )

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
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_DESTROY_MIOHDL)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to finalise MultIO handle' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
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

END FUNCTION MULTIO_DELETE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief send a encoded buffer through multIO.
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_WRITE_BINARY_GRIB'
PP_THREAD_SAFE FUNCTION MULTIO_WRITE_BINARY_GRIB( MIO_HANDLE, BUFFER, HANDLE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: ISO_C_BINDING,     ONLY: C_CHAR
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: GET_GRIB_MESSAGE

  ! Symbols imported from external libraries.
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),                                      INTENT(INOUT) :: MIO_HANDLE
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: BUFFER
  INTEGER(KIND=JPIM_K),                                     INTENT(IN)    :: HANDLE
  TYPE(HOOKS_T),                                            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: DATA_LENGTH
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIM_K) :: ERR
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_GRIB_MESSAGE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_ENCODED_GRIB_THROUGH_MULTIO=2_JPIB_K


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

  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_GRIB_MESSAGE) GET_GRIB_MESSAGE( BUFFER, HANDLE, DATA_LENGTH, HOOKS )

  ! Write encoded message to multio
  ERR = MIO_HANDLE%WRITE_GRIB_ENCODED( BUFFER(1:DATA_LENGTH) )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_ENCODED_GRIB_THROUGH_MULTIO)


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
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR
    CHARACTER(LEN=4096) :: GRIB_ERROR
    CHARACTER(LEN=32) :: TMP

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)

    CASE (ERRFLAG_UNABLE_TO_GET_GRIB_MESSAGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the grib message to the buffer' )

    CASE (ERRFLAG_UNABLE_TO_WRITE_ENCODED_GRIB_THROUGH_MULTIO)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write encoded grib trough multio' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF

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

END FUNCTION MULTIO_WRITE_BINARY_GRIB
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send a encoded buffer through multIO.
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_WRITE_VALUES_DP'
PP_THREAD_SAFE FUNCTION MULTIO_WRITE_VALUES_DP( MIOH, MIOMD, VDP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from external libraries.
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),             INTENT(INOUT) :: MIOH
  TYPE(MULTIO_METADATA),           INTENT(INOUT) :: MIOMD
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(IN)    :: VDP
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR
  INTEGER(KIND=JPIM_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_SIZE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_VALUES=2_JPIB_K

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

  ! Write size
  ERR = MIOMD%SET( 'globalSize', SIZE(VDP) )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_SIZE)

  ! Write to multio
  ERR = MIOH%WRITE_FIELD( MIOMD, VDP )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_VALUES)

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
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_WRITE_SIZE)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write values size into multio metadata' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_WRITE_VALUES)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write values into multio handle' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
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

END FUNCTION MULTIO_WRITE_VALUES_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send a encoded buffer through multIO.
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_WRITE_VALUES_SP'
PP_THREAD_SAFE FUNCTION MULTIO_WRITE_VALUES_SP( MIOH, MIOMD, VSP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from external libraries.
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),             INTENT(INOUT) :: MIOH
  TYPE(MULTIO_METADATA),           INTENT(INOUT) :: MIOMD
  REAL(KIND=JPRM_K), DIMENSION(:), INTENT(IN)    :: VSP
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR
  INTEGER(KIND=JPIM_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_SIZE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_VALUES=2_JPIB_K

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

  ! Write size
  ERR = MIOMD%SET( 'globalSize', SIZE(VSP) )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_SIZE)

  ! Write to multio
  ERR = MIOH%WRITE_FIELD( MIOMD, VSP )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_VALUES)

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
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_WRITE_SIZE)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write values size into multio metadata' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_WRITE_VALUES)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write values into multio handle' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
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

END FUNCTION MULTIO_WRITE_VALUES_SP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send a encoded buffer through multIO.
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_SEND_PARAMETRISATION'
PP_THREAD_SAFE FUNCTION MULTIO_SEND_PARAMETRISATION( MODEL_PARAMS, MIOH, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),   INTENT(IN)    :: MODEL_PARAMS
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIOH
  TYPE(HOOKS_T),       INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR
  CHARACTER(LEN=128) :: KEY_NAME
  CHARACTER(LEN=16) :: CTMP
  CHARACTER(LEN=16) :: GG_NAME
  CHARACTER(LEN=16) :: SH_NAME
  INTEGER(KIND=JPIB_K)  :: WRITE_STAT
  INTEGER(KIND=JPIB_K)  :: ITMP

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_MIOMD=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DELETE_MIOMD=4_JPIB_K

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

  ! Create metadata
  ERR = MIOMD%NEW( MIOH )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_CREATE_MIOMD )

  !> Generate reduced gaussian grid
  CTMP=REPEAT(' ',16)
  GG_NAME=REPEAT(' ',16)
  WRITE(CTMP,'(I6)',IOSTAT=WRITE_STAT) MODEL_PARAMS%GEO_%ILATS/2_JPIB_K
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(GG_NAME, '(A,A)',IOSTAT=WRITE_STAT) 'O',TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(GG_NAME))//'-truncateDegrees'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), 1_JPIB_K )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(GG_NAME))//'-numberOfPointsAlongAMeridian'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), MODEL_PARAMS%GEO_%ILATS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(GG_NAME))//'-numberOfParallelsBetweenAPoleAndTheEquator'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), MODEL_PARAMS%GEO_%IDGNH )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(GG_NAME))//'-latitudeOfFirstGridPointInDegrees'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), 180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*MODEL_PARAMS%GEO_%ZNLAT )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(GG_NAME))//'-longitudeOfFirstGridPointInDegrees'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), 0.0_JPRD_K )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(GG_NAME))//'-latitudeOfLastGridPointInDegrees'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), 180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*MODEL_PARAMS%GEO_%ZSLAT )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(GG_NAME))//'-longitudeOfLastGridPointInDegrees'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), 360._JPRD_K-360._JPRD_K/REAL(MODEL_PARAMS%GEO_%ILONS,JPRD_K) )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(GG_NAME))//'-pl'
  ERR = MIOH%WRITE_PARAMETRIZATION( TRIM(ADJUSTL(KEY_NAME)), MODEL_PARAMS%GEO_%ILOENG(1:MODEL_PARAMS%GEO_%ILATS) )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  !> Generate regular gaussian grid
  CTMP=REPEAT(' ',16)
  GG_NAME=REPEAT(' ',16)
  WRITE(CTMP,'(I6)',IOSTAT=WRITE_STAT) MODEL_PARAMS%GEO_%IDGNH
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(GG_NAME, '(A,A)',IOSTAT=WRITE_STAT) 'F',TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(GG_NAME))//'-latitudeOfFirstGridPointInDegrees'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), 180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*MODEL_PARAMS%GEO_%ZNLAT )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(GG_NAME))//'-longitudeOfFirstGridPointInDegrees'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), 0.0_JPRD_K )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(GG_NAME))//'-latitudeOfLastGridPointInDegrees'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), 180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*MODEL_PARAMS%GEO_%ZSLAT )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(GG_NAME))//'-longitudeOfLastGridPointInDegrees'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), 360._JPRD_K-360._JPRD_K/REAL(MODEL_PARAMS%GEO_%ILONS,JPRD_K) )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(GG_NAME))//'-directionIncrementInDegrees'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), 360.0_JPRD_K/REAL(MODEL_PARAMS%GEO_%ILONS,JPRD_K) )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  !> Generate  sh name
  CTMP=REPEAT(' ',16)
  SH_NAME=REPEAT(' ',16)
  WRITE(CTMP,'(I16)',IOSTAT=WRITE_STAT) MODEL_PARAMS%GEO_%ISMAX
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(SH_NAME, '(A,A)',IOSTAT=WRITE_STAT) 'TCO',TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT.NE.0_JPIB_K, ERRFLAG_UNABLE_TO_WRITE )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(SH_NAME))//'-pentagonalResolutionParameterJ'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), MODEL_PARAMS%GEO_%ISMAX )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(SH_NAME))//'-pentagonalResolutionParameterK'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), MODEL_PARAMS%GEO_%ISMAX )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  KEY_NAME = REPEAT(' ', 128)
  KEY_NAME='geo-'//TRIM(ADJUSTL(SH_NAME))//'-pentagonalResolutionParameterM'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), MODEL_PARAMS%GEO_%ISMAX )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  IF ( MODEL_PARAMS%GEO_%NSTTYP .GE. 2_JPIB_K ) THEN

    !> Fill the general fields on the base data structure
    KEY_NAME = REPEAT(' ', 128)
    KEY_NAME='geo-'//TRIM(ADJUSTL(SH_NAME))//'-subType'
    ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)),'stretched_rotated_sh' )
    PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

    KEY_NAME = REPEAT(' ', 128)
    KEY_NAME='geo-'//TRIM(ADJUSTL(SH_NAME))//'-latitudeStretchInDegrees'
    ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), 180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*ASIN(REAL(MODEL_PARAMS%GEO_%RMUCEN,JPRD_K)) )
    PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

    KEY_NAME = REPEAT(' ', 128)
    KEY_NAME='geo-'//TRIM(ADJUSTL(SH_NAME))//'-longitudeStretchInDegrees'
    ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), 180._JPRD_K/(2.0_JPRD_K*ASIN(1.0_JPRD_K))*REAL(MODEL_PARAMS%GEO_%RLOCEN,JPRD_K) )
    PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

    KEY_NAME = REPEAT(' ', 128)
    KEY_NAME='geo-'//TRIM(ADJUSTL(SH_NAME))//'-stretchFactor'
    ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), MODEL_PARAMS%GEO_%RSTRET )
    PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )


  ELSEIF ( ABS(MODEL_PARAMS%GEO_%RSTRET-1.0_JPRD_K) .GE. 1.0E-14_JPRD_K ) THEN

    !> Fill the general fields on the base data structure
    KEY_NAME = REPEAT(' ', 128)
    KEY_NAME=TRIM(ADJUSTL(SH_NAME))//'-subType'
    ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), 'stretched_sh' )
    PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

    KEY_NAME = REPEAT(' ', 128)
    KEY_NAME='geo-'//TRIM(ADJUSTL(SH_NAME))//'-stretchFactor'
    ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), MODEL_PARAMS%GEO_%RSTRET )
    PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  ELSE

    !> Fill the general fields on the base data structure
    KEY_NAME = REPEAT(' ', 128)
    KEY_NAME = 'geo-'//TRIM(ADJUSTL(SH_NAME))//'-subType'
    ERR = MIOMD%SET( TRIM(ADJUSTL(KEY_NAME)), 'sh' )
    PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )
  ENDIF



  !> ensamble parametrization
  ERR = MIOMD%SET( 'misc-ensemble-typeOfEnsembleSimulation', 1_JPIB_K )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  ERR = MIOMD%SET( 'misc-ensemble-numberOfForecastsInEnsemble', MODEL_PARAMS%SIM_%NTOTENS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  ERR = MIOMD%SET( 'misc-ensemble-systemNumber', MODEL_PARAMS%SIM_%NSYSTEM )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  ERR = MIOMD%SET( 'misc-ensemble-methodNumber', MODEL_PARAMS%SIM_%NMETHOD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )


  !> Analysis parametrization
  ERR = MIOMD%SET( 'misc-analysis-lengthOfTimeWindow', MODEL_PARAMS%SIM_%NWINSIZE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  !> Time parametrization
  ERR = MIOMD%SET( 'misc-initialStep', MODEL_PARAMS%SIM_%NSTEPINI )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  ITMP = INT(MODEL_PARAMS%SIM_%TSTEP, KIND=JPIB_K)
  ERR = MIOMD%SET( 'misc-lengthOfTimeStepInSeconds', ITMP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  !> Level Parametrization
  ERR = MIOH%WRITE_PARAMETRIZATION( 'misc-pv', MODEL_PARAMS%GEO_%ZVERT(1:2*(MODEL_PARAMS%GEO_%IFLEV+1)) )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  !> Wave parametrization
  ERR = MIOH%WRITE_PARAMETRIZATION( 'misc-waveFrequencies', MODEL_PARAMS%WAM_%FR )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )

  ERR = MIOH%WRITE_PARAMETRIZATION( 'misc-waveDirections', MODEL_PARAMS%WAM_%TH )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )


  ERR = MIOH%WRITE_PARAMETRIZATION( MIOMD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION )


  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_DELETE_MIOMD )

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
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR
    INTEGER(KIND=JPIB_K) :: STAT

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CREATE_MIOMD)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create a new MultIO metadata object' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_WRITE_PARAMETRIZATION)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to send parametrization' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write to string' )
    CASE (ERRFLAG_UNABLE_TO_DELETE_MIOMD)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to delete MultIO metadata object' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
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

END FUNCTION MULTIO_SEND_PARAMETRISATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_FILL_MARS_METADATA'
PP_THREAD_SAFE FUNCTION MULTIO_FILL_MARS_METADATA( MARS, PAR, MIOMD, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPRD_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: ENUMERATORS_MOD,     ONLY: UNDEF_PARAM_E
  USE :: ENUMERATORS_MOD,     ONLY: ISTREAM2CSTREAM
  USE :: ENUMERATORS_MOD,     ONLY: ITYPE2CTYPE
  USE :: ENUMERATORS_MOD,     ONLY: ICLASS2CCLASS
  USE :: ENUMERATORS_MOD,     ONLY: IORIGIN2CORIGIN
  USE :: ENUMERATORS_MOD,     ONLY: IPACKING2CPACKING
  USE :: ENUMERATORS_MOD,     ONLY: ILEVTYPE2CLEVTYPE
  USE :: ENUMERATORS_MOD,     ONLY: IREPRES2CREPRES
  USE :: ENUMERATORS_MOD,     ONLY: ITYPE_OF_PROCESSED_DATA2CTYPE_OF_PROCESSED_DATA

  ! Symbols imported from other libraries
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FORTRAN_MESSAGE_T), INTENT(IN)    :: MARS
  TYPE(PARAMETRIZATION_T), INTENT(IN)    :: PAR
  TYPE(MULTIO_METADATA),   INTENT(INOUT) :: MIOMD
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=128) :: CTMP
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_ENUM2STRING=2_JPIB_K

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

  ! Handle "stream"
  IF ( MARS%STREAM .NE. UNDEF_PARAM_E ) THEN
    CTMP = REPEAT( ' ', LEN(CTMP))
    PP_TRYCALL(ERRFLAG_CONVERT_ENUM2STRING) ISTREAM2CSTREAM( MARS%STREAM, CTMP, HOOKS )
    ERR = MIOMD%SET( 'stream', TRIM(ADJUSTL(CTMP)) )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "type"
  IF ( MARS%TYPE .NE. UNDEF_PARAM_E ) THEN
    CTMP = REPEAT( ' ', LEN(CTMP))
    PP_TRYCALL(ERRFLAG_CONVERT_ENUM2STRING) ITYPE2CTYPE( MARS%TYPE, CTMP, HOOKS )
    ERR = MIOMD%SET( 'type', TRIM(ADJUSTL(CTMP)) )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "class"
  IF ( MARS%CLASS .NE. UNDEF_PARAM_E ) THEN
    CTMP = REPEAT( ' ', LEN(CTMP))
    PP_TRYCALL(ERRFLAG_CONVERT_ENUM2STRING) ICLASS2CCLASS( MARS%CLASS, CTMP, HOOKS )
    ERR = MIOMD%SET( 'class', TRIM(ADJUSTL(CTMP)) )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "expver"
  IF ( MARS%EXPVER .NE. REPEAT('*',4) ) THEN
    ERR = MIOMD%SET( 'expver', TRIM(ADJUSTL(MARS%EXPVER)) )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "origin"
  IF ( MARS%ORIGIN .NE. UNDEF_PARAM_E ) THEN
    CTMP = REPEAT( ' ', LEN(CTMP))
    PP_TRYCALL(ERRFLAG_CONVERT_ENUM2STRING) IORIGIN2CORIGIN( MARS%ORIGIN, CTMP, HOOKS )
    ERR = MIOMD%SET( 'origin', TRIM(ADJUSTL(CTMP)) )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "anoffset"
  IF ( MARS%ANOFFSET .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'anoffset', MARS%ANOFFSET )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "packing"
  IF ( MARS%PACKING .NE. UNDEF_PARAM_E ) THEN
    CTMP = REPEAT( ' ', LEN(CTMP))
    PP_TRYCALL(ERRFLAG_CONVERT_ENUM2STRING) IPACKING2CPACKING( MARS%PACKING, CTMP, HOOKS )
    ERR = MIOMD%SET( 'packing', TRIM(ADJUSTL(CTMP)) )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "number"
  IF ( MARS%NUMBER .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'number', MARS%NUMBER )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "ident"
  IF ( MARS%IDENT .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'ident', MARS%IDENT )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "instrument"
  IF ( MARS%INSTRUMENT .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'instrument', MARS%INSTRUMENT )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "channel"
  IF ( MARS%CHANNEL .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'channel', MARS%CHANNEL )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "chem"
  IF ( MARS%CHEM .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'chem', MARS%CHEM )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "param"
  IF ( MARS%PARAM .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'param', MARS%PARAM )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "levtype"
  IF ( MARS%LEVTYPE .NE. UNDEF_PARAM_E ) THEN
    CTMP = REPEAT( ' ', LEN(CTMP))
    PP_TRYCALL(ERRFLAG_CONVERT_ENUM2STRING) ILEVTYPE2CLEVTYPE( MARS%LEVTYPE, CTMP, HOOKS )
    ERR = MIOMD%SET( 'levtype', TRIM(ADJUSTL(CTMP)) )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "levelist"
  IF ( MARS%LEVELIST .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'levelist', MARS%LEVELIST )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "direction"
  IF ( MARS%DIRECTION .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'direction', MARS%DIRECTION )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "frequency"
  IF ( MARS%FREQUENCY .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'frequency', MARS%FREQUENCY )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "date"
  IF ( MARS%DATE .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'date', MARS%DATE )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "time"
  IF ( MARS%TIME .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'time', MARS%TIME )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "step"
  IF ( MARS%STEP .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'step', MARS%STEP )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "timeproc"
  IF ( MARS%TIMEPROC .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'timeproc', MARS%TIMEPROC )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "repres"
  IF ( MARS%REPRES .NE. UNDEF_PARAM_E ) THEN
    CTMP = REPEAT( ' ', LEN(CTMP))
    PP_TRYCALL(ERRFLAG_CONVERT_ENUM2STRING) IREPRES2CREPRES( MARS%REPRES, CTMP, HOOKS )
    ERR = MIOMD%SET( 'repres', TRIM(ADJUSTL(CTMP)) )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "grid"
  IF ( MARS%GRID .NE. REPEAT('*',8) ) THEN
    ERR = MIOMD%SET( 'grid', TRIM(ADJUSTL(MARS%GRID)) )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "truncation"
  IF ( MARS%TRUNCATION .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'truncation', MARS%TRUNCATION )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF


  ! ------------------------------------------------------------------------------------------------
  ! Handle "tableVersion"
  IF ( PAR%TABLES_VERSION .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'misc-tablesVersion', PAR%TABLES_VERSION )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "generatingProcessIdentifier"
  IF ( PAR%GENERATING_PROCESS_IDENTIFIER .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'misc-generatingProcessIdentifier', PAR%GENERATING_PROCESS_IDENTIFIER )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "typeOfProcessedData"
  IF ( PAR%TYPE_OF_PROCESSED_DATA .NE. UNDEF_PARAM_E ) THEN
    CTMP = REPEAT( ' ', LEN(CTMP))
    PP_TRYCALL(ERRFLAG_CONVERT_ENUM2STRING) ITYPE_OF_PROCESSED_DATA2CTYPE_OF_PROCESSED_DATA(PAR%TYPE_OF_PROCESSED_DATA, CTMP, HOOKS )
    ERR = MIOMD%SET( 'misc-typeOfProcessedData', TRIM(ADJUSTL(CTMP)) )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "systemNumber"
  IF ( PAR%ENSEMBLE%SYSTEM_NUMBER_ .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'misc-systemNumber', PAR%ENSEMBLE%SYSTEM_NUMBER_ )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "methodNumber"
  IF ( PAR%ENSEMBLE%METHOD_NUMBER_ .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'misc-methodNumber', PAR%ENSEMBLE%METHOD_NUMBER_ )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "typeOfEnsembleForecast"
  IF ( PAR%ENSEMBLE%TYPE_OF_ENSEMBLE_FORECAST_ .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'misc-typeOfEnsembleForecast', PAR%ENSEMBLE%TYPE_OF_ENSEMBLE_FORECAST_ )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "numberOfForecastsInEnsemble"
  IF ( PAR%ENSEMBLE%NUMBER_OF_FORECASTS_IN_ENSEMBLE_ .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'misc-numberOfForecastsInEnsemble', PAR%ENSEMBLE%NUMBER_OF_FORECASTS_IN_ENSEMBLE_ )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "lengthOfTimeWindow"
  IF ( PAR%ANALYSIS%LENGTH_OF_TIME_WINDOW_ .NE. UNDEF_PARAM_E ) THEN
    IF ( PAR%ANALYSIS%LENGTH_OF_TIME_WINDOW_ .GT. 0 ) THEN
      ERR = MIOMD%SET( 'misc-lengthOfTimeWindow', PAR%ANALYSIS%LENGTH_OF_TIME_WINDOW_ )
      PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
    ENDIF
  ENDIF

  ! Handle "itmin" period for wave
  IF ( PAR%WAVE%ITMIN .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'misc-itMin', PAR%WAVE%ITMIN )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "itmax" period for wave
  IF ( PAR%WAVE%ITMAX .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'misc-itMax', PAR%WAVE%ITMAX )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "satelliteSeries"
  IF ( PAR%SATELLITE%SATELLITE_SERIES .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'misc-satelliteSeries', PAR%SATELLITE%SATELLITE_SERIES )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "scaleFactorOfCentralVaweNumber"
  IF ( PAR%SATELLITE%SCALED_FACTOR_OF_CENTRAL_VAWENUMBER .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'misc-scaleFactorOfCentralVaweNumber', PAR%SATELLITE%SCALED_FACTOR_OF_CENTRAL_VAWENUMBER )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "scaledValueOfCentralWaveNumber"
  IF ( PAR%SATELLITE%SCALED_VALUE_OF_CENTRAL_VAWENUMBER .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'misc-scaledValueOfCentralWaveNumber', PAR%SATELLITE%SCALED_VALUE_OF_CENTRAL_VAWENUMBER )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "bitsPerValue"
  IF ( PAR%DATA_REPRESENTATION%BITS_PER_VALUE_ .NE. UNDEF_PARAM_E ) THEN
    ERR = MIOMD%SET( 'misc-bitsPerValue', PAR%DATA_REPRESENTATION%BITS_PER_VALUE_ )
    PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
  ENDIF

  ! Handle "laplacianScaleFactor"
  IF ( PAR%DATA_REPRESENTATION%LAPLACIAN_SCALE_FACTOR_ .NE. UNDEF_PARAM_E ) THEN
    IF ( PAR%DATA_REPRESENTATION%LAPLACIAN_SCALE_FACTOR_ .GT. 0 ) THEN
      ERR = MIOMD%SET( 'misc-laplacianScaleFactor', PAR%DATA_REPRESENTATION%LAPLACIAN_SCALE_FACTOR_ )
      PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
    ENDIF
  ENDIF

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
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR
    INTEGER(KIND=JPIB_K) :: STAT

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_CONVERT_ENUM2STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert enum to string' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_MULTIO_METADATA)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write multio metadata' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
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

END FUNCTION MULTIO_FILL_MARS_METADATA
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send a encoded buffer through multIO.
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_INJECT_PARAMETERS'
PP_THREAD_SAFE FUNCTION MULTIO_INJECT_PARAMETERS( MODEL_PARAMS, MIOMD, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),     INTENT(IN)    :: MODEL_PARAMS
  TYPE(MULTIO_METADATA), INTENT(INOUT) :: MIOMD
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
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

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


END FUNCTION MULTIO_INJECT_PARAMETERS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





!>
!> @brief send a flush to MultiIO using the provided handle to notify
!>        multIO that the simulation started.
!>
!> This function is used to send a flush message to the specified MultIO
!> handle. The flush message is used to notify MultIO that the simulation
!> started.
!>
!> @param [inout] mio_handle A MultIO handle
!>
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH_FIRST_STEP'
PP_THREAD_SAFE FUNCTION MULTIO_FLUSH_FIRST_STEP( MIO_HANDLE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from external libraries.
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING
  USE :: MULTIO_API, ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE
  TYPE(HOOKS_T),       INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR
  INTEGER(KIND=JPIM_K)  :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_MIOMD=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_METADATA=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FLUSH=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DELETE_MIOMD=4_JPIB_K

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

  ERR = MIOMD%NEW( MIO_HANDLE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_CREATE_MIOMD )

  ERR = MIOMD%SET( "flushKind", 0 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_SET_METADATA )

  ERR = MIOMD%SET( "step", -1 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_SET_METADATA )

  ERR = MIO_HANDLE%FLUSH( MIOMD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_FLUSH )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_DELETE_MIOMD )


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
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CREATE_MIOMD)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create a new MultIO metadata object' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_SET_METADATA)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set metadata' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_FLUSH)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call the flush' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_DELETE_MIOMD)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to delete MultIO metadata object' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
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

END FUNCTION MULTIO_FLUSH_FIRST_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





!>
!> @brief send a flush to MultiIO using the provided handle.
!>
!> This function is used to send a flush message to the specified MultIO
!> handle.
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH'
PP_THREAD_SAFE FUNCTION MULTIO_FLUSH( MIO_HANDLE, ISTEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from external libraries.
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING
  USE :: MULTIO_API, ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),  INTENT(INOUT) :: MIO_HANDLE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ISTEP
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR
  INTEGER(KIND=JPIM_K)  :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_MIOMD=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_METADATA=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FLUSH=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DELETE_MIOMD=4_JPIB_K

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

  ERR = MIOMD%NEW( MIO_HANDLE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_CREATE_MIOMD )

  ERR = MIOMD%SET( "flushKind", 1 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_SET_METADATA )

  ERR = MIOMD%SET( "step", ISTEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_SET_METADATA )

  ERR = MIO_HANDLE%FLUSH( MIOMD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_FLUSH )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_DELETE_MIOMD )


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
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CREATE_MIOMD)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create a new MultIO metadata object' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_SET_METADATA)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set metadata' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_FLUSH)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call the flush' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_DELETE_MIOMD)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to delete MultIO metadata object' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
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

END FUNCTION MULTIO_FLUSH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief send a flush to MultiIO using the provided handle.
!>
!> This function is used to send a flush message to the specified MultIO
!> handle. The flush contain also the request to dump a restart to all the
!> actions.
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH_AND_TRIGGER_RESTART'
PP_THREAD_SAFE FUNCTION MULTIO_FLUSH_AND_TRIGGER_RESTART( MIO_HANDLE, ISTEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from external libraries.
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING
  USE :: MULTIO_API, ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),  INTENT(INOUT) :: MIO_HANDLE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ISTEP
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR
  INTEGER(KIND=JPIM_K)  :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_MIOMD=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_METADATA=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FLUSH=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DELETE_MIOMD=4_JPIB_K

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

  ERR = MIOMD%NEW(MIO_HANDLE)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_CREATE_MIOMD )

  ERR = MIOMD%SET( "flushKind", 2 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_SET_METADATA )

  ERR = MIOMD%SET( "step",  ISTEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_SET_METADATA )

  ERR = MIO_HANDLE%FLUSH(MIOMD)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_FLUSH )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_DELETE_MIOMD )

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
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CREATE_MIOMD)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create a new MultIO metadata object' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_SET_METADATA)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set metadata' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_FLUSH)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call the flush' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_DELETE_MIOMD)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to delete MultIO metadata object' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
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

END FUNCTION MULTIO_FLUSH_AND_TRIGGER_RESTART
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send a flush to MultiIO using the provided handle.
!>
!> This function is used to send a flush message to the specified MultIO
!> handle. The flush contain also the information that this is the last
!> step of the simulation (used for example in statistics to dump the
!> last step).
!>
!> @param [inout] mio_handle A MultIO handle
!> @param [in]    istep      The current step to be notified
!>
!> @see MULTIO_FLUSH
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_END_OF_SIMULATION
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH_AND_TRIGGER_RESTART'
PP_THREAD_SAFE FUNCTION MULTIO_FLUSH_LAST_STEP(MIO_HANDLE, ISTEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from external libraries.
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING
  USE :: MULTIO_API, ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),  INTENT(INOUT) :: MIO_HANDLE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ISTEP
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR
  INTEGER(KIND=JPIM_K)  :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_MIOMD=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_METADATA=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FLUSH=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DELETE_MIOMD=4_JPIB_K

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

  ERR = MIOMD%NEW( MIO_HANDLE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_CREATE_MIOMD )

  ERR = MIOMD%SET( "flushKind", 3 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_SET_METADATA )

  ERR = MIOMD%SET( "step", ISTEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_SET_METADATA )

  ERR = MIO_HANDLE%FLUSH( MIOMD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_FLUSH )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_DELETE_MIOMD )


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
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CREATE_MIOMD)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create a new MultIO metadata object' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_SET_METADATA)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set metadata' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_FLUSH)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call the flush' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_DELETE_MIOMD)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to delete MultIO metadata object' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
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

END FUNCTION MULTIO_FLUSH_LAST_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send a flush to MultiIO using the provided handle.
!>
!> This function is used to send a flush message to the specified MultIO
!> handle. The flush contain also the information that this is the end
!> of the simulation (still not have a real utility, the idea is to use
!> this for deallocating all the data inside MultIO actions in order to
!> have more control over the destructor stage).
!>
!> @param [inout] mio_handle A MultIO handle
!>
!> @see MULTIO_FLUSH
!> @see MULTIO_FLUSH_AND_TRIGGER_RESTART
!> @see MULTIO_FLUSH_LAST_STEP
!> @see MULTIO_NOTIFY_STEP
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH_END_OF_SIMULATION'
PP_THREAD_SAFE FUNCTION MULTIO_FLUSH_END_OF_SIMULATION( MIO_HANDLE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from external libraries.
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING
  USE :: MULTIO_API, ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE
  TYPE(HOOKS_T),       INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR
  INTEGER(KIND=JPIM_K)  :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_MIOMD=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_METADATA=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FLUSH=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DELETE_MIOMD=4_JPIB_K

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


  ERR = MIOMD%NEW(MIO_HANDLE)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_CREATE_MIOMD )

  ERR = MIOMD%SET( "flushKind", 4 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_SET_METADATA )

  ERR = MIOMD%SET( "step", -1 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_SET_METADATA )

  ERR = MIO_HANDLE%FLUSH(MIOMD)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_FLUSH )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_DELETE_MIOMD )


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
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CREATE_MIOMD)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create a new MultIO metadata object' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_SET_METADATA)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set metadata' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_FLUSH)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call the flush' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_DELETE_MIOMD)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to delete MultIO metadata object' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
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

END FUNCTION MULTIO_FLUSH_END_OF_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send a notify to MultiIO the current step.
!>
!> @param [in] mio_handle A MultIO handle
!> @param [in] step       The current step
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_NOTIFY_STEP'
PP_THREAD_SAFE FUNCTION MULTIO_NOTIFY_STEP(MIO_HANDLE, STEP, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from external libraries.
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING
  USE :: MULTIO_API, ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),  INTENT(INOUT) :: MIO_HANDLE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR
  INTEGER(KIND=JPIM_K)  :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_MIOMD=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_METADATA=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_NOTIFY=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DELETE_MIOMD=4_JPIB_K

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

  ERR = MIOMD%NEW(MIO_HANDLE)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_CREATE_MIOMD )

  ERR = MIOMD%SET( "trigger", "step" )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_SET_METADATA )

  ERR = MIOMD%SET( "step", STEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_SET_METADATA )

  ERR = MIO_HANDLE%NOTIFY(MIOMD)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_NOTIFY )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  ERRFLAG_UNABLE_TO_DELETE_MIOMD )

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
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CREATE_MIOMD)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create a new MultIO metadata object' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_SET_METADATA)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set metadata' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_NOTIFY)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call the notify step' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_DELETE_MIOMD)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to delete MultIO metadata object' )
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multio error message: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR, STAT=STAT )
      ENDIF
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

END FUNCTION MULTIO_NOTIFY_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_ERROR_HANDLER'
SUBROUTINE MULTIO_ERROR_HANDLER( CONTEXT, ERR, INFO )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_FAILURE_INFO
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=INT64),       INTENT(INOUT) :: CONTEXT
  INTEGER,                   INTENT(IN)    :: ERR
  TYPE(MULTIO_FAILURE_INFO), INTENT(IN)    :: INFO

  ! Error handling variables
  CHARACTER(LEN=:), ALLOCATABLE :: STR
  CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

  MIO_ERR_STR = MULTIO_ERROR_STRING( ERR, INFO )
  WRITE(*,*) 'MULTIO_UTILS_MOD:MULTIO_ERROR_HANDLER - Error: '//TRIM(MIO_ERR_STR)

  ! Exit point on success
  RETURN

END SUBROUTINE MULTIO_ERROR_HANDLER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE MULTIO_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
