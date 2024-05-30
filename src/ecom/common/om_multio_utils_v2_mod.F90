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
#define PP_FILE_NAME 'om_multio_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'OM_MULTIO_UTILS_MOD'
MODULE OM_MULTIO_UTILS_MOD

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
PUBLIC :: MULTIO_FLUSH_BEGIN_OF_SIMULATION
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_NEW'
SUBROUTINE MULTIO_NEW( MIO_HANDLE, CONFIG_FILE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported from other modules within the project.
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_CONFIGURATION
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_ERROR_STRING
  USE :: MULTIO_API,  ONLY: FAILURE_HANDLER_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE
  CHARACTER(LEN=*),    INTENT(IN)    :: CONFIG_FILE

  ! Local variabels
  TYPE(MULTIO_CONFIGURATION) :: MIOCC
  INTEGER(KIND=JPIM_K) :: ERR
  INTEGER(KIND=INT64)  :: ERR_CONTEXT
  PROCEDURE(FAILURE_HANDLER_T), POINTER :: PF

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Create the MultIO configuration object
  ERR = MIOCC%NEW( TRIM(ADJUSTL(CONFIG_FILE)) )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ! Setup the failure handler
  PF => MULTIO_ERROR_HANDLER
  ERR = MIOCC%SET_FAILURE_HANDLER( PF, ERR_CONTEXT )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )

  ! Create the MultIO handle
  ERR = MIO_HANDLE%NEW( MIOCC )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  4 )

  ! Destroy the MultIO configuration object
  ERR = MIOCC%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  5 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create MultIO config from file: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (2)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create MultIO config: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (3)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create MultIO failure handler: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (4)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create MultIO handle: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (5)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to destroy MultIO handle: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_NEW
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_DELETE'
SUBROUTINE MULTIO_DELETE( MIO_HANDLE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Delete the multio handle
  ERR = MIO_HANDLE%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to finalise MultIO handle: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_DELETE
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_WRITE_BINARY_GRIB'
SUBROUTINE MULTIO_WRITE_BINARY_GRIB( MIO_HANDLE, BUFFER, HANDLE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported from other libraries
  USE :: GRIB_API,   ONLY: GRIB_SUCCESS
  USE :: GRIB_API,   ONLY: GRIB_GET_MESSAGE_SIZE
  USE :: GRIB_API,   ONLY: GRIB_COPY_MESSAGE
  USE :: GRIB_API,   ONLY: GRIB_GET_ERROR_STRING
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),                         INTENT(INOUT) :: MIO_HANDLE
  CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: BUFFER
  INTEGER(KIND=JPIM_K),                        INTENT(IN)    :: HANDLE

  ! Local variables
  INTEGER(KIND=JPIM_K) :: DATA_LENGTH
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIM_K) :: ERR
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  ! Get encoded grib message size
  KRET = GRIB_SUCCESS
  CALL GRIB_GET_MESSAGE_SIZE( HANDLE, DATA_LENGTH, STATUS=KRET)
  PP_DEBUG_CRITICAL_COND_THROW(KRET.NE.GRIB_SUCCESS, 1)
  PP_DEBUG_CRITICAL_COND_THROW(DATA_LENGTH.LE.0, 2)

  ! Maange buffer allocation
  IF (ALLOCATED(BUFFER) .AND. (SIZE(BUFFER) .LT. DATA_LENGTH)) THEN
    DEALLOCATE(BUFFER, STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_DEVELOP_COND_THROW(STAT.NE.0, 3)
  ENDIF

  IF (.NOT.ALLOCATED(BUFFER)) THEN
    STAT = 0
    ALLOCATE(BUFFER(DATA_LENGTH), STAT=STAT, ERRMSG=ERRMSG)
    PP_DEBUG_DEVELOP_COND_THROW(STAT.NE.0, 4)
  ENDIF

  ! Extract encoded message
  KRET = GRIB_SUCCESS
  CALL GRIB_COPY_MESSAGE(HANDLE, BUFFER, STATUS=KRET)
  PP_DEBUG_CRITICAL_COND_THROW(KRET.NE.GRIB_SUCCESS, 5)

  ! Write encoded message to multio
  ERR = MIO_HANDLE%WRITE_GRIB_ENCODED( BUFFER )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, 6)


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      CALL GRIB_GET_ERROR_STRING(KRET, GRIB_ERROR)
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to read the buffer size: ', KRET, GRIB_ERROR )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Buffer size must be greater than 0' )
    CASE (3)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate buffer: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate buffer' )
      ENDIF
    CASE (4)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate buffer: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate buffer' )
      ENDIF
    CASE (5)
      CALL GRIB_GET_ERROR_STRING(KRET, GRIB_ERROR)
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to copy the message to the buffer: ', KRET, GRIB_ERROR )
    CASE (6)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write encoded grib trough multio: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write encoded grib trough multio' )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_WRITE_BINARY_GRIB
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_WRITE_VALUES_DP'
SUBROUTINE MULTIO_WRITE_VALUES_DP( MIOH, MIOMD, VDP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPRD_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported from other libraries
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),             INTENT(INOUT) :: MIOH
  TYPE(MULTIO_METADATA),           INTENT(INOUT) :: MIOMD
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(IN)    :: VDP

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Write size
  ERR = MIOMD%SET( 'globalSize', SIZE(VDP) )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, 1)

  ! Write to multio
  ERR = MIOH%WRITE_FIELD( MIOMD, VDP )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, 2)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values size into multio metadata: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values size into multio metadata' )
      ENDIF
    CASE (2)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values into multio handle: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values into multio handle' )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_WRITE_VALUES_DP
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_WRITE_VALUES_SP'
SUBROUTINE MULTIO_WRITE_VALUES_SP( MIOH, MIOMD, VSP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPRM_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported from other libraries
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),             INTENT(INOUT) :: MIOH
  TYPE(MULTIO_METADATA),           INTENT(INOUT) :: MIOMD
  REAL(KIND=JPRM_K), DIMENSION(:), INTENT(IN)    :: VSP

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Write size
  ERR = MIOMD%SET( 'globalSize', SIZE(VSP) )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, 1)

  ! Write to multio
  ERR = MIOH%WRITE_FIELD( MIOMD, VSP )
  PP_DEBUG_CRITICAL_COND_THROW(ERR.NE.MULTIO_SUCCESS, 2)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values size into multio metadata: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values size into multio metadata' )
      ENDIF
    CASE (2)
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values into multio handle: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values into multio handle' )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_WRITE_VALUES_SP
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_INJECT_PARAMETERS'
SUBROUTINE MULTIO_INJECT_PARAMETERS( MODEL_PARAMS, MIOMD)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD, ONLY: JPRM_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported from other libraries
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),     INTENT(IN)    :: MODEL_PARAMS
  TYPE(MULTIO_METADATA), INTENT(INOUT) :: MIOMD

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! TODO: If needed here we can inject more parameters in the metadata of each message

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MULTIO_INJECT_PARAMETERS
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH_BEGIN_OF_SIMULATION'
SUBROUTINE MULTIO_FLUSH_BEGIN_OF_SIMULATION( MIO_HANDLE, MODEL_PARAMS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE
  TYPE(MODEL_PAR_T),   INTENT(IN)    :: MODEL_PARAMS

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Create a multio metadata object
  ERR = MIOMD%NEW( MIO_HANDLE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ! Set the flush kind
  ERR = MIOMD%SET( "flushKind", 0 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )

  ! Fill Simulation parameters
  CALL MULTIO_FILL_SIMPAR( MIOMD, MODEL_PARAMS%SIM_ )

  ! Fill geometry parameters
  CALL MULTIO_FILL_GEOPAR( MIOMD, MODEL_PARAMS%GEO_ )

  ! Fill atmosphere parameters
  CALL MULTIO_FILL_ATMPAR( MIOMD, MODEL_PARAMS%ATM_ )

  ! Fill wave parameters
  CALL MULTIO_FILL_WAMPAR( MIOMD, MODEL_PARAMS%WAM_ )

  ! Fill satellites parameters
  CALL MULTIO_FILL_SATPAR( MIOMD, MODEL_PARAMS%SAT_ )

  ! Flush the metadata
  ERR = MIO_HANDLE%FLUSH( MIOMD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )

  ! Delete the metadata object
  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create a new MultIO metadata object' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to call the flush' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to delete MultIO metadata object' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FLUSH_BEGIN_OF_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FILL_SIMPAR'
SUBROUTINE MULTIO_FILL_SIMPAR( MIOMD, SIM_PAR )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: SIM_PAR_T
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_METADATA), INTENT(INOUT) :: MIOMD
  TYPE(SIM_PAR_T),       INTENT(IN)    :: SIM_PAR

  ! Local variables
  CHARACTER(LEN=128)    :: KEY
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  ! Table version to be used for grib 2 encoding
  KEY='sim.grib2_latest_table_version'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%IGRIB2_TABLES_VERSION_LATEST )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 1  )

  ! Number of tasks in IO group
  KEY = 'sim.nprocsIO'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NPROC_IO )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 2 )

  ! yomgrib.F90 - grib coding descriptors
  ! -------------------------------------
  ! NCYCLE : cycle identifier
  KEY = 'sim.cycle'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NCYCLE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 3 )

  ! NLOCGRB - ECMWF LOCAL USAGE IDENTIFIER
  KEY = 'sim.nlocgrb'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NLOCGRB )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 4 )

  ! NBITSSHLNSP - Number of bits for GRIB encoding of LNSP (default=16)
  KEY = 'sim.nbitsSH'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NBITSSHLNSP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 5 )

  ! NBITSEXPR   - Number of bits for GRIB encoding of experimental
  ! parameters (default=-1 in which case multio is deciding)
  KEY = 'sim.nbitsEXPR'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NBITSEXPR )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 6 )

  ! CTYPE: type for use by FDB
  KEY = 'sim.type'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%CTYPE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 7 )

  ! CFCLASS: class for use by FDB
  KEY = 'sim.class'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%CFCLASS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 8 )

  ! NLEG - current VAREPS leg number (eg 1(2) for the T399(T255) part of a T399-T255 VAREPS)
  KEY = 'sim.nleg'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NLEG )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 9 )

  ! NTOTENS - TOTAL NUMBER OF FORCASTS IN ENSEMBLE
  KEY = 'sim.NTotEns'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NTOTENS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 10 )

  ! NENSFNB - ENSAMBLE FORECAST NUMBER
  KEY = 'sim.nensfnb'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NENSFNB )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 11 )

  ! NWINOFF - OFFSET OF ANALYSIS FROM END OF ASSIMILATION WINDOW [HOURS]
  KEY = 'sim.nwinoff'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NWINOFF )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 12 )

  ! NJDIAG  - SENSITIVITY DIAGNOSTIC NUMBER (1=>J1, 2=>J2, 3=>J3, 4=>J4)
  KEY = 'sim.njdiag'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NJDIAG )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 13 )

  ! NJDOMAI - SENSITIVITY DIAGNOSTIC MASK REGION (0=>global, 1=>europe, 2=>N.H., 3=>S.H.)
  KEY = 'sim.ndomai'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NJDOMAI )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 14 )

  ! ???????????????????
  KEY = 'sim.njiter'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NJITER )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 15 )

  ! NSTREAM - EXPLICIT STREAM NUMBER (OR ZERO FOR DEFAULT STREAMS TO BE USED)
  KEY = 'sim.nstream'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NSTREAM )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 16 )

  ! NSYSTEM - FOR USE IN SEASONAL STREAM (DIFFERENT OPERATIONAL SYSTEMS)
  KEY = 'sim.nsystem'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NSYSTEM )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 17 )

  ! NMETHOD - FOR USE IN SEASONAL STREAM (DIFFERENT ENSEMBLES)
  KEY = 'sim.nmethod'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NMETHOD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 18 )

  ! NREFERENCE - FOR USE IN HINDCAST STREAM
  KEY = 'sim.nreference'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NREFERENCE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 19 )

  ! NCONSENSUS - FOR MULTI_ANALYSIS STREAM (1 if consensus)
  KEY = 'sim.nconsensus'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NCONSENSUS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 20 )

  ! NDWD    - FOR MULTI_ANALYSIS STREAM (1 if DWD analysis used)
  KEY = 'sim.ndwd'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NDWD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 21 )

  ! NMFR    - FOR MULTI_ANALYSIS STREAM
  KEY = 'sim.nmfr'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NMFR )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 22 )

  ! NNCEP   - FOR MULTI_ANALYSIS STREAM
  KEY = 'sim.nncep'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NNCEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 233 )

  ! NUKM   - FOR MULTI_ANALYSIS STREAM
  KEY = 'sim.nukm'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NUKM )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 24 )

  ! yomrip0.F90 - time related quantities
  ! NINDAT : run initial date in the form AAAAMMDD
  KEY = 'sim.nindat'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NINDAT )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 25 )

  ! NSSSSS : initial time in seconds (e.g. for 12h, 43200)
  KEY = 'sim.nsssss'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NSSSSS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 26 )

  ! RTIMST : ABSOLUTE TIME OF THE MODEL AT START
  KEY = 'sim.rtimst'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%RTIMST )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 27 )

  ! yomct0.F90 - control variables for the job (Constant across the job)
  ! --------------------------------------------------------------------
  ! NCONF      : configuration of the job
  !                0- 99 : 3-D integration job
  !              100-199 : variational job
  !              200-299 : 2-D integration job
  !              300-349 : KALMAN filter
  !              350-399 : predictability model             (currently unused)
  !              400-499 : test of the adjoint
  !              500-599 : test of the tangent linear model
  !              600-699 : eigenvalue/vector solvers
  !              700-799 : optimal interpolation
  !              800-899 : sensitivity
  !              900-999 : miscellaneous other configurations.
  !                    1 : 3-D primitive equation model
  !                  131 : incremental 4-D VAR/3-D VAR
  !                  201 : shallow-water model
  !                  202 : vorticity equation model
  !                  302 : simplified extended Kalman filter (SEKF)
  !                  401 : test of adjoint with 3-D P.E. model
  !                  421 : test of adjoint with shallow-water model
  !                  422 : test of adjoint with vorticity equation model
  !                  501 : test of tangent linear with 3-D P.E. model
  !                  521 : test of tangent linear with shallow-water model
  !                  522 : test of tangent linear with vorticity equation model
  !                  601 : eigenvalue/vector solver for 3-D P.E. model
  !                  701 : optimal interpolation with CANARI
  !                  801 : sensitivity with 3-D P.E. model
  !                  901 : set up initial conditions (CPREP1)
  !                  923 : initialisation of climatologic files
  !                  931 : creation of an ARPEGE file containing the SST (INCLITC).
  !                  932 : interpolates the sea-ice concentration field from
  !                        satellite data to the ARPEGE grid (CSEAICE).
  !                  933 : interpolates SST from OSTIA and sea-ice concentration
  !                  from OSI SAF.
  KEY = 'sim.nconf'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NCONF )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 28 )

  ! NSTEPINI: Initial step in hours for the initial conditions
  !           at the beginning of 4D-Var trajectory (usually 3 hours).
  !           It is used to update the step while saving the FCs along
  !           the first trajectory.
  KEY = 'sim.nstepini'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NSTEPINI )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 29 )

  ! last postprocessing step present
  KEY = 'sim.lppsteps'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%LPPSTEPS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 30 )

  ! length of timestep in seconds
  KEY = 'sim.tstep'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%TSTEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 31 )

  ! LOBSC1  : .T. = term of observations included in configuration 1
  KEY = 'sim.lobsc1'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%LOBSC1 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 32 )

  ! CNMEXP     : name of the experiment
  !              An experiment is identified by its name (16 characters)
  !              and its cycle (typically same experiment but without a bug)
  KEY = 'sim.cnmexp'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%CNMEXP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 33 )

  ! yomvareps.F90 - control varibles for vareps
  ! -------------------------------------------
  ! LVAREPS          : .T. when running with variable resolution
  KEY = 'sim.lvareps'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%LVAREPS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 34 )

  ! NFCHO_TRUNC_INI  : forecast step used to define the ICs (ie NFCHO_TRUNC of previous VAREPS LEG)
  KEY = 'sim.nfcho_trunc_ini'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%NFCHO_TRUNC_INI )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 35 )

  ! ocean atmosphere coupling
  KEY = 'sim.ldmcc04'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIM_PAR%LDMCC04 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 36 )


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1:36)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set simulation parameter metadata "'//TRIM(ADJUSTL(KEY))//'"' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FILL_SIMPAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FILL_GEOPAR'
SUBROUTINE MULTIO_FILL_GEOPAR( MIOMD, GEO_PAR )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: GEO_PAR_T
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_METADATA), INTENT(INOUT) :: MIOMD
  TYPE(GEO_PAR_T),       INTENT(IN)    :: GEO_PAR

  ! Local variables
  CHARACTER(LEN=128)    :: KEY
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  ! pardim.F90 - parameters dimensions
  ! ----------------------------------
  ! JPMXLE : MAXIMUM NUMBER OF LEVELS
  KEY = 'geo.jpmxle'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), GEO_PAR%JPMXLE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 1 )


  ! JPMXGL : MAXIMUM NUMBER OF GAUSSIAN LATITUDES
  KEY = 'geo.jpmxgl'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), GEO_PAR%JPMXGL )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 2 )

  ! yomdim.F90 - Geometry dimensions
  ! --------------------------------
  ! NSMAX   : truncation order
  KEY = 'geo.ismax'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), GEO_PAR%ISMAX )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 3 )


  ! NDGLG  : number of rows of latitudes
  KEY = 'geo.ilats'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), GEO_PAR%ILATS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 4 )


  ! NDLON  : length of a row of latitude near equator
  KEY = 'geo.ilons'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), GEO_PAR%ILONS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 5 )


  ! dependant (ILATS+1)/2
  KEY = 'geo.idgnh'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), GEO_PAR%IDGNH )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 6 )

  ! yomleg.F90 - Legendre Polynomials
  !----------------------------------
  ! RLATIG(0) : theta of the zero (1) latitude
  KEY = 'geo.znlat'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), GEO_PAR%ZNLAT )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 7 )


  ! RLATIG(0) : theta of the last (NDGLG) latitude
  KEY = 'geo.zslat'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), GEO_PAR%ZSLAT )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 8 )

  ! yomvert.F90 - vertical coordinates handleing
  ! IFLEV : UBOUND(VBH) -> VBH(0:IFLEV) : B of the vertical coordinate
  KEY = 'geo.iflev'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), GEO_PAR%IFLEV )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 9 )

  ! ZVERT = [ VBH, VAH ]
  ! VBH : (0:NFLEVG) : B of the vertical coordinate
  ! VAH : (0:NFLEVG) ;  =VALH*VP00
  KEY = 'geo.zvert'
  ERR = MIOMD%SET_ARRAY( TRIM(ADJUSTL(KEY)), GEO_PAR%ZVERT )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 10 )

  ! yomgem.F90 - Geometry definition
  !----------------------------------

  !     NHTYP  : 0 = regular grid
  !            : 2 = number of points read on namelist namrgri
  KEY = 'geo.nhtyp'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), GEO_PAR%NHTYP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 11 )

  !     NSTTYP : 1 = POLE OF STRETCHING, POLE OF THE COLLOCATION GRID
  !                 AT THE NORTHERN POLE OF THE REAL EARTH.
  !              2 = THE POLE OF STRETCHING IS ANYWHERE ON THE REAL EARTH
  !             AND ON THE EQUATOR OF THE COLLOCATION GRID ON THE MERIDIAN PI.
  !                  THE EQUATOR OF THE COLLOCATION GRID IS TANGENT
  !             TO A PARALLEL OF THE EARTH.
  KEY = 'geo.nsttyp'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), GEO_PAR%NSTTYP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 12 )

  ! RMUCEN : MU OF THE POLE OF STRETCHING
  KEY = 'geo.rmucen'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), GEO_PAR%RMUCEN )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 13 )

  ! RLOCEN : LONGITUDE OF THE POLE OF STRETCHING
  KEY = 'geo.rlocen'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), GEO_PAR%RLOCEN )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 14 )

  ! RSTRET : STRETCHING FACTOR
  KEY = 'geo.rstret'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), GEO_PAR% RSTRET)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 15 )

  ! NLOENG(1:ILATS) : number of active points on a parallel (global)
  KEY = 'geo.iloeng'
  ERR = MIOMD%SET_ARRAY( TRIM(ADJUSTL(KEY)), GEO_PAR%ILOENG )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 16 )


  ! yomgrib.F90
  !----------------------------------
  KEY = 'geo.nsflevs_1'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIZE(GEO_PAR%NSFLEVS,1) )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 17 )


  KEY = 'geo.nsflevs_2'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SIZE(GEO_PAR%NSFLEVS,2) )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 18 )


  KEY = 'geo.nsflevs'
  ERR = MIOMD%SET_ARRAY( TRIM(ADJUSTL(KEY)), RESHAPE(GEO_PAR%NSFLEVS,[SIZE(GEO_PAR%NSFLEVS)]) )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 19 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1:19)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set geometry parameter metadata "'//TRIM(ADJUSTL(KEY))//'"' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FILL_GEOPAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FILL_ATMPAR'
SUBROUTINE MULTIO_FILL_ATMPAR( MIOMD, ATM_PAR )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: ATM_PAR_T
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_METADATA), INTENT(INOUT) :: MIOMD
  TYPE(ATM_PAR_T),       INTENT(IN)    :: ATM_PAR

  ! Local variables
  CHARACTER(LEN=128)    :: KEY
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FILL_ATMPAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FILL_WAMPAR'
SUBROUTINE MULTIO_FILL_WAMPAR( MIOMD, WAM_PAR )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: WAM_PAR_T
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_METADATA), INTENT(INOUT) :: MIOMD
  TYPE(WAM_PAR_T),       INTENT(IN)    :: WAM_PAR

  ! Local variables
  CHARACTER(LEN=128)    :: KEY
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  !
  ! yowfred.F90

  ! *FR* REAL FREQUENCIES IN HERTZ.  (1:NFRE_RED)
  KEY = 'wam.fr'
  ERR = MIOMD%SET_ARRAY( TRIM(ADJUSTL(KEY)), WAM_PAR%FR )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 1 )

  ! *TH* REAL DIRECTIONS IN RADIANS. (1:NANG)
  KEY = 'wam.th'
  ERR = MIOMD%SET_ARRAY( TRIM(ADJUSTL(KEY)), WAM_PAR%TH )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 2 )
  !
  ! yowcoup.F90
  !      *LWCOUSAMEGRID        TRUE when coupled and the atmospheric grid and the wave grid is the same
  KEY = 'wam.lwcousamegrid'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%LWCOUSAMEGRID )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 3 )
  !
  ! yowparam.F90
  ! *CLDOMAIN*  CHARACTER DEFINES THE DOMAIN OF THE MODEL (for the
  !                       FDB and for selection of some variables)
  KEY = 'wam.domain'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%CLDOMAIN )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 4 )

  ! *NGX*      INTEGER  NUMBER OF LONGITUDES IN GRID.
  KEY = 'wam.ngx'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NGX )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 5 )


  ! *NGY*      INTEGER  NUMBER OF LATITUDES IN GRID.
  KEY = 'wam.ngy'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NGY )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 6 )

  ! *NANG*     INTEGER  NUMBER OF ANGLES.
  KEY = 'wam.nang'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NANG )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 7 )

  ! *NFRE_RED* INTEGER  REDUCED NUMBER OF FREQUENCIES FOR THE PROPAGATION AND IO
  !                     BY DEFAULT = NFRE
  KEY = 'wam.nfre_red'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NFRE_RED )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 8 )

  !
  ! yowgribhd.F90
  !   IMDLGRBID_G           INTEGER   GLOBAL MODEL IDENTIFICATION FOR GRIB CODING
  !                                   IT CAN ALSO BE MODIFIED IN THE INPUT NAMELIST.
  KEY = 'wam.imdlgribid_g'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%IMDLGRBID_G )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 9 )

  !   IMDLGRBID_M           INTEGER   LAW MODEL IDENTIFICATION FOR GRIB CODING
  !                                   IT CAN ALSO BE MODIFIED IN THE INPUT NAMELIST.
  KEY = 'wam.imdlgribid_m'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%IMDLGRBID_M )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 10 )

  !   NDATE_TIME_WINDOW_END INTEGER ?
  KEY = 'wam.ndate_time_window_end'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NDATE_TIME_WINDOW_END )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 11 )

  !   NWINOFF               INTEGER ?
  KEY = 'wam.nwinoff'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NWINOFF )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 12)

  !   NGRIB_VERSION         INTEGER ?
  KEY = 'wam.ngrb_version'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NGRIB_VERSION )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 13 )

  !   NTENCODE              INTEGER   TOTAL NUMBER OF GRID POINTS FOR ENCODING
  KEY = 'wam.ntencode'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NTENCODE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 14 )

  !   NGRBRESI              INTEGER   NUMBER OF BITS USED TO ENCODE INTEGRATED
  !                                   PARAMETERS
  KEY = 'wam.ngrbresi'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NGRBRESI )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 15 )

  !   NGRBRESS              INTEGER   NUMBER OF BITS USED TO ENCODE SPECTRA
  KEY = 'wam.ngrbress'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NGRBRESS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 16 )

  !   PPMISS                REAL      ALL SPECTRAL VALUES LESS OR EQUAL PPMISS ARE
  !                                   REPLACED BY THE MISSING DATA INDICATOR
  KEY = 'wam.ppmiss'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%PPMISS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 17 )

  !   PPEPS                 REAL      SMALL NUMBER USED IN SPECTRAL PACKING OF 251
  KEY = 'wam.ppeps'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%PPEPS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 18 )

  !   PPREC                 REAL      REFERENCE VALUE FOR SPECTRAL PACKING OF 251
  KEY = 'wam.pprec'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%PPREC )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 19 )

  !   PPRESOL               REAL      MAXIMUN RESOLUTION POSSIBLE WHEN ENCODING
  !                                   SPECTRA (PARAMETER 251).
  KEY = 'wam.ppresol'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%PPRESOL )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 20 )

  !   PPMIN_RESET           REAL      CAN BE USED TO SET THE MINIMUM OF PPMIN
  !                                   IN WGRIBOUT TO A LOWER VALUE.
  KEY = 'wam.ppmind_reset'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%PPMIN_RESET )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 21 )

  !   HOPERI                CHARACTER GRIB ENCODING ACTION FOR INTEGRATED FIELDS.
  KEY = 'wam.hoperi'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%HOPERI )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 22 )

  !   HOPERS                CHARACTER GRIB ENCODING ACTION FOR SPECTRA.
  KEY = 'wam.hopers'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%HOPERS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 23 )

  !   LGRHDIFS              LOGICAL   IF TRUE THEN GRIB HEADER WILL USE INFORMATION
  !                                   AS PROVIDED BY THE IFS.
  KEY = 'wam.lgrhdifs'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%LGRHDIFS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 24 )

  !   LNEWLVTP              LOGICAL   IF TRUE THE NEW LEVTYPE DEFINITION WILL BE USED.
  KEY = 'wam.lnewlvtp'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%LNEWLVTP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 25 )

  !   LPADPOLES             LOGICAL   TRUE IF POLES ARE PADDED WHEN SAVIND TO GRIB.
  KEY = 'wam.lpadpoles'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%LPADPOLES )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 26 )

  !   LL_GRID_SIMPLE_MATRIX IF TRUE THEN THE 2D SPECTRA WILL USE THE LEGACY grid_simple_matrix
  !                         TO ENCODE THE 2D SPECTRA in GRIB1. THIS SHOULD BE PHASED OUT as soon as feasible!
  KEY = 'wam.ll_grid_simple_matrix'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%LL_GRID_SIMPLE_MATRIX )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 27 )

  !
  ! yowmap.F90
  !  *IRGG*    INTEGER   GRID CODE: 0 = REGULAR, 1 = IRREGULAR.
  KEY = 'wam.irgg'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%IRGG )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 28 )

  !  *IQGAUSS* INTEGER   =1 IF A QUASI GAUSSIAN GRID IS USED.
  !                      =0 OTHERWISE.
  KEY = 'wam.iqgauss'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%IQGAUSS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 29 )

  !  *AMOWEP*  REAL      MOST WESTERN LONGITUDE IN GRID (DEGREE).
  KEY = 'wam.amowep'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%AMOWEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 30 )

  !  *AMOSOP*  REAL      MOST SOUTHERN LATITUDE IN GRID (DEGREE).
  KEY = 'wam.amosop'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%AMOSOP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 31 )

  !  *AMOEAP*  REAL      MOST EASTERN LONGITUDE IN GRID (DEGREE).
  KEY = 'wam.amoeap'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%AMOEAP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 32 )

  !  *AMONOP*  REAL      MOST NORTHERN LATITUDE IN GRID (DEGREE).
  KEY = 'wam.amonop'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%AMONOP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 33 )

  !  *XDELLA*  REAL      GRID INCREMENT FOR LATITUDE (DEGREE).
  KEY = 'wam.xdella'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%XDELLA )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 34 )

  !  *XDELLO*  REAL      CONSTANT GRID INCREMENT FOR LONGITUDE
  KEY = 'wam.xdello'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%XDELLO )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 35 )

  !  *NLONRGG* INTEGER   NUMBER OF GRID POINTS PER LATITUDES (NOT THE PL ARRAY, NEED WORK). (1:NGY)
  KEY = 'wam.nlonrgg'
  ERR = MIOMD%SET_ARRAY( TRIM(ADJUSTL(KEY)), WAM_PAR%NLONRGG )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 36 )
 !
  ! yowpcons.F90
  !  *ZMISS* REAL MISSING DATA INDICATOR
  !               (SET IN CHIEF OR VIA THE IFS).
  KEY = 'wam.zmiss'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%ZMISS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 37 )

  ! yowstat.F90
  !  *NENSFNB*   INTEGER ENSEMBLE FORECAST NUMBER (DEFAULT=0)
  KEY = 'wam.nensfnb'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NENSFNB )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 38 )

  !  *NTOTENS*   INTEGER TOTAL ENSEMBLE FORECAST MEMBERS (DEFAULT=0)
  KEY = 'wam.ntotens'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NTOTENS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 39 )

  !  *NSYSNB*    INTEGER SYSTEM NUMBER TO BE USED FOR GRIBBING OF
  !                      SEASONAL DATA (DEFAULT=-1).
  KEY = 'wam.nsysnb'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NSYSNB )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 40 )

  !  *NMETNB*    INTEGER METHOD NUMBER TO BE USED FOR GRIBBING OF
  !                      SEASONAL DATA (DEFAULT=-1).
  KEY = 'wam.nmetnb'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NMETNB )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 41 )

  !  *ISTREAM*   INTEGER STREAM NUMBER WHEN CODING DATA IN GRID
  !                      IF SET TO 0 IT WILL NOT BE USED AND
  !                      INSTEAD MARSTYPE WILL BVE USED TO DETERMINE
  !                      THE STREAM.
  KEY = 'wam.istream'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%ISTREAM )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 42 )

  !  *NLOCGRB*   INTEGER LOCAL GRIB TABLE NUMBER.
  KEY = 'wam.nlocgrb'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NLOCGRB )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 43 )

  !  *NCONCENSUS INTEGER ONLY USED IN THE CONTEXT OF MULTI-ANALYSIS
  !                      ENSEMBLE FORECASTS. IT SPECIFIED WHETHER
  !                      ONE (NCONCENSUS=0) OR MORE ANALYSES ARE
  !                      USED IN THE INITIAL CONDITIONS.
  KEY = 'wam.nconsensus'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NCONSENSUS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 44 )

  !  *NDWD*      INTEGER ONLY USED IN THE CONTEXT OF MULTI-ANALYSIS
  !                      ENSEMBLE FORECASTS. IT SPECIFIED WHETHER
  !                      DWD IS USED IN THE INITIAL CONDITIONS.
  KEY = 'wam.ndwd'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NDWD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 45 )

  !  *NMFR*      INTEGER ONLY USED IN THE CONTEXT OF MULTI-ANALYSIS
  !                      ENSEMBLE FORECASTS. IT SPECIFIED WHETHER
  !                      METEO FRANCE IS USED IN THE INITIAL
  !                      CONDITIONS.
  KEY = 'wam.nmfr'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NMFR )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 46 )

  !  *NNCEP*     INTEGER ONLY USED IN THE CONTEXT OF MULTI-ANALYSIS
  !                      ENSEMBLE FORECASTS. IT SPECIFIED WHETHER
  !                      NCEP IS USED IN THE INITIAL CONDITIONS.
  KEY = 'wam.nncep'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NNCEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 47 )

  !  *NUKM*      INTEGER ONLY USED IN THE CONTEXT OF MULTI-ANALYSIS
  !                      ENSEMBLE FORECASTS. IT SPECIFIED WHETHER
  !                      THE MET OFFICE IS USED IN THE INITIAL
  !                      CONDITIONS.
  KEY = 'wam.nukm'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NUKM )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 48 )

  !  *IREFDATE*  INTEGER REFERENCE DATE FOR MONTHLY FORECAST
  !                      HINDCAST RUNS.
  KEY = 'wam.irefdate'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%IREFDATE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 49 )

  !  *MARSTYPE*  CHAR*2  CHARACTER STRING INDICATING THE CURRENT
  !                      STATUS OF THE MODEL.
  KEY = 'wam.marstype'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%MARSTYPE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 50 )

  !  *YCLASS*    CHAR*2  CHARACTER STRING INDICATING THE CLASS OF
  !                      THE CURRENT RUN.
  KEY = 'wam.yclass'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%YCLASS )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 51 )

  !  *YEXPVER*   CHAR*4  CHARACTER STRING INDICATING THE EXPERIMENT
  !                      VERSION NUMBER OF THE CURRENT RUN.
  KEY = 'wam.yexpver'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%YEXPVER )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 52 )

!     GRIB2 TABLE VERSION FOR WAVE SPECTRA
  KEY = 'wam.nspec2tab'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NSPEC2TAB )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 53 )

  !     GRIB2 TEMPLATE NUMBER FOR WAVE SPECTRA DETERMINISTIC
  KEY = 'wam.nspec2tmpd'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NSPEC2TMPD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 54 )

  !     GRIB2 TEMPLATE NUMBER FOR WAVE SPECTRA PROBABILISTIC
  KEY = 'wam.nspec2tmpp'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NSPEC2TMPP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 55 )

  !     GRIB2 TEMPLATE NUMBER FOR PARAMETER DEFINED OVER A WAVE PERIOD RANGE DETERMINISTIC
  KEY = 'wam.ntrg2tmpd'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NTRG2TMPD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 56 )

  !     GRIB2 TEMPLATE NUMBER FOR PARAMETER DEFINED OVER A WAVE PERIOD RANGE PROBABILISTIC
  KEY = 'wam.ntrg2tmpp'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%NTRG2TMPP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 57 )

  !          *ITMIN*   MINIMUM WAVE PERIOD FOR WHICH THE PARAMETER IS DEFINED (s)
  KEY = 'wam.itmin'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%ITMIN )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 58 )

  !          *ITMAX*   MAXIMUM WAVE PERIOD FOR WHICH THE PARAMETER IS DEFINED (s)
  KEY = 'wam.itmax'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), WAM_PAR%ITMAX )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 59 )


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1:59)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set wave parameter metadata "'//TRIM(ADJUSTL(KEY))//'"' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FILL_WAMPAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FILL_SATPAR'
SUBROUTINE MULTIO_FILL_SATPAR( MIOMD, SAT_PAR )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: SAT_PAR_T
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_METADATA), INTENT(INOUT) :: MIOMD
  TYPE(SAT_PAR_T),       INTENT(IN)    :: SAT_PAR

  ! Local variables
  CHARACTER(LEN=128)    :: KEY
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  KEY = 'sat.nsatsim'
  ERR = MIOMD%SET( TRIM(ADJUSTL(KEY)), SAT_PAR%NSATSIM )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 1 )

  KEY = 'sat.mseries'
  ERR = MIOMD%SET_ARRAY( TRIM(ADJUSTL(KEY)), SAT_PAR%MSERIES )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 2 )

  KEY = 'sat.msatid'
  ERR = MIOMD%SET_ARRAY( TRIM(ADJUSTL(KEY)), SAT_PAR%MSATID )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 3 )

  KEY = 'sat.minst'
  ERR = MIOMD%SET_ARRAY( TRIM(ADJUSTL(KEY)), SAT_PAR%MINST )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 4 )

  KEY = 'sat.mchan'
  ERR = MIOMD%SET_ARRAY( TRIM(ADJUSTL(KEY)), SAT_PAR%MCHAN )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 5 )

  KEY = 'sat.rcwn'
  ERR = MIOMD%SET_ARRAY( TRIM(ADJUSTL(KEY)), SAT_PAR%RCWN )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS, 6 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1:6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set satellite parameter metadata "'//TRIM(ADJUSTL(KEY))//'"' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FILL_SATPAR
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH'
SUBROUTINE MULTIO_FLUSH( MIO_HANDLE, ISTEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: ISTEP

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ERR = MIOMD%NEW( MIO_HANDLE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ERR = MIOMD%SET( "flushKind", 1 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )

  ERR = MIOMD%SET( "step", ISTEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )

  ERR = MIO_HANDLE%FLUSH( MIOMD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  4 )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  5 )


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create a new MultIO metadata object' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"flushKind\" in the MultIO metadata' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"step\" in the MultIO metadata' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to call the flush' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to delete MultIO metadata object' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FLUSH
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH_AND_TRIGGER_RESTART'
SUBROUTINE MULTIO_FLUSH_AND_TRIGGER_RESTART( MIO_HANDLE, ISTEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),  INTENT(INOUT) :: MIO_HANDLE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ISTEP

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ERR = MIOMD%NEW(MIO_HANDLE)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ERR = MIOMD%SET( "flushKind", 2 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )

  ERR = MIOMD%SET( "step",  ISTEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )

  ERR = MIO_HANDLE%FLUSH(MIOMD)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  4 )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  5 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create a new MultIO metadata object' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"flushKind\" in the MultIO metadata' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"step\" in the MultIO metadata' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to call the flush' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to delete MultIO metadata object' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FLUSH_AND_TRIGGER_RESTART
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH_AND_TRIGGER_RESTART'
SUBROUTINE MULTIO_FLUSH_LAST_STEP(MIO_HANDLE, ISTEP)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: ISTEP

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ERR = MIOMD%NEW( MIO_HANDLE )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ERR = MIOMD%SET( "flushKind", 3 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )

  ERR = MIOMD%SET( "step", ISTEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )

  ERR = MIO_HANDLE%FLUSH( MIOMD )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  4 )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  5 )


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create a new MultIO metadata object' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"flushKind\" in the MultIO metadata' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"step\" in the MultIO metadata' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to call the flush' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to delete MultIO metadata object' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FLUSH_LAST_STEP
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_FLUSH_END_OF_SIMULATION'
SUBROUTINE MULTIO_FLUSH_END_OF_SIMULATION(MIO_HANDLE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE), INTENT(INOUT) :: MIO_HANDLE

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  ERR = MIOMD%NEW(MIO_HANDLE)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ERR = MIOMD%SET( "flushKind", 4 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )

  ERR = MIOMD%SET( "step", -1 )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )

  ERR = MIO_HANDLE%FLUSH(MIOMD)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  4 )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  5 )


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create a new MultIO metadata object' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"flushKind\" in the MultIO metadata' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"step\" in the MultIO metadata' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to call the flush' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to delete MultIO metadata object' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_FLUSH_END_OF_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief send a notify to MultiIO the current step.
!>
!> @param [in] mio_handle A MultIO handle
!> @param [in] step       The current step
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_NOTIFY_STEP'
SUBROUTINE MULTIO_NOTIFY_STEP(MIO_HANDLE, STEP)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE
  USE :: MULTIO_API,  ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API,  ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_HANDLE),  INTENT(INOUT) :: MIO_HANDLE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP

  ! Local variables
  TYPE(MULTIO_METADATA) :: MIOMD
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ERR = MIOMD%NEW(MIO_HANDLE)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  1 )

  ERR = MIOMD%SET( "trigger", "step" )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  2 )

  ERR = MIOMD%SET( "step", STEP )
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  3 )

  ERR = MIO_HANDLE%NOTIFY(MIOMD)
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  4 )

  ERR = MIOMD%DELETE()
  PP_DEBUG_DEVELOP_COND_THROW( ERR.NE.MULTIO_SUCCESS,  5 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create a new MultIO metadata object' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"flushKind\" in the MultIO metadata' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set \"step\" in the MultIO metadata' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to call the flush' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to delete MultIO metadata object' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_NOTIFY_STEP
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

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=INT64),       INTENT(INOUT) :: CONTEXT
  INTEGER,                   INTENT(IN)    :: ERR
  TYPE(MULTIO_FAILURE_INFO), INTENT(IN)    :: INFO

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check error
  PP_DEBUG_CRITICAL_COND_THROW( ERR .NE. MULTIO_SUCCESS, 1 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      MIO_ERR_STR = MULTIO_ERROR_STRING( ERR, INFO )
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'MULTIO_UTILS_MOD:MULTIO_ERROR_HANDLER - Error: '//TRIM(MIO_ERR_STR) )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_ERROR_HANDLER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE OM_MULTIO_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
