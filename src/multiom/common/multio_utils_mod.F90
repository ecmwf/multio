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
PUBLIC :: MULTIO_INJECT_PARAMETERS
PUBLIC :: MULTIO_FLUSH_FIRST_STEP
PUBLIC :: MULTIO_FLUSH
PUBLIC :: MULTIO_FLUSH_AND_TRIGGER_RESTART
PUBLIC :: MULTIO_FLUSH_LAST_STEP
PUBLIC :: MULTIO_FLUSH_END_OF_SIMULATION
PUBLIC :: MULTIO_NOTIFY_STEP
PUBLIC :: GET_GRIB_MESSAGE

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
!> @brief Extracts a grib message to a fortran array
!>
!> @param [inout] buffer  Allocatable byet array
!> @param [in]    handle  Codes fortran handle
!>
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GET_GRIB_MESSAGE'
PP_THREAD_SAFE FUNCTION GET_GRIB_MESSAGE( BUFFER, HANDLE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from external libraries.
  USE :: GRIB_API,   ONLY: GRIB_SUCCESS
  USE :: GRIB_API,   ONLY: GRIB_GET_MESSAGE_SIZE
  USE :: GRIB_API,   ONLY: GRIB_COPY_MESSAGE
  USE :: GRIB_API,   ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: BUFFER
  INTEGER(KIND=JPIM_K),                        INTENT(IN)    :: HANDLE
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: DATA_LENGTH
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIM_K) :: ERR
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_BUFFER_SIZE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_BUFFER_SIZE_MUST_BE_GREATER_THAN_ZERO=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_BUFFER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_BUFFER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COPY_MESSAGE_TO_BUFFER=5_JPIB_K


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

  ! Get encoded grib message size
  KRET = GRIB_SUCCESS
  CALL GRIB_GET_MESSAGE_SIZE( HANDLE, DATA_LENGTH, STATUS=KRET)
  PP_DEBUG_CRITICAL_COND_THROW(KRET.NE.GRIB_SUCCESS, ERRFLAG_UNABLE_TO_READ_BUFFER_SIZE)
  PP_DEBUG_CRITICAL_COND_THROW(DATA_LENGTH.LE.0, ERRFLAG_BUFFER_SIZE_MUST_BE_GREATER_THAN_ZERO)

  ! Maange buffer allocation
  IF (ALLOCATED(BUFFER) .AND. (SIZE(BUFFER) .LT. DATA_LENGTH)) THEN
    DEALLOCATE(BUFFER, STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_DEVELOP_COND_THROW(STAT.NE.0, ERRFLAG_UNABLE_TO_DEALLOCATE_BUFFER)
  ENDIF

  IF (.NOT.ALLOCATED(BUFFER)) THEN
    STAT = 0
    ALLOCATE(BUFFER(DATA_LENGTH), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_DEVELOP_COND_THROW(STAT.NE.0, ERRFLAG_UNABLE_TO_ALLOCATE_BUFFER)
  ENDIF

  ! Extract encoded message
  KRET = GRIB_SUCCESS
  CALL GRIB_COPY_MESSAGE(HANDLE, BUFFER, STATUS=KRET)
  PP_DEBUG_CRITICAL_COND_THROW(KRET.NE.GRIB_SUCCESS, ERRFLAG_UNABLE_TO_COPY_MESSAGE_TO_BUFFER)

  ! ! Write encoded message to multio
  ! ERR = MIO_HANDLE%WRITE_GRIB_ENCODED( BUFFER(1:DATA_LENGTH) )
  ! PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, ERRFLAG_UNABLE_TO_WRITE_ENCODED_GRIB_THROUGH_MULTIO)

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

    CASE (ERRFLAG_UNABLE_TO_READ_BUFFER_SIZE)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR)
      TMP = REPEAT(' ', 32)
      WRITE(TMP,*, IOSTAT=STAT) KRET
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read the buffer size' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'eccodes error code: '//TRIM(ADJUSTL(TMP)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'eccodes error message: '//TRIM(ADJUSTL(GRIB_ERROR)) )

    CASE (ERRFLAG_BUFFER_SIZE_MUST_BE_GREATER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Buffer size must be greater than 0' )

    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE_BUFFER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate buffer' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=STAT)
      ENDIF

    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_BUFFER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate buffer' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=STAT)
      ENDIF

    CASE (ERRFLAG_UNABLE_TO_COPY_MESSAGE_TO_BUFFER)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      TMP = REPEAT(' ', 32)
      WRITE(TMP,*, IOSTAT=STAT) KRET
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to copy the message to the buffer' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'eccodes error code: '//TRIM(ADJUSTL(TMP)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'eccodes error message: '//TRIM(ADJUSTL(GRIB_ERROR)) )

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

END FUNCTION GET_GRIB_MESSAGE
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
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

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
  TYPE(MULTIO_HANDLE),                         INTENT(INOUT) :: MIO_HANDLE
  CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: BUFFER
  INTEGER(KIND=JPIM_K),                        INTENT(IN)    :: HANDLE
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

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

  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_GRIB_MESSAGE) GET_GRIB_MESSAGE( BUFFER, HANDLE, HOOKS )

  ! Write encoded message to multio
  ! ERR = MIO_HANDLE%WRITE_GRIB_ENCODED( BUFFER )
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

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: ERR

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

  ! TODO: If needed here we can inject more parameters in the metadata of each message

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
