!> @file
!>
!> @brief Definition of the `MULTIO_METADATA_T` derived type.
!>
!> The `MULTIO_METADATA_T` type is a derived type that extends the functionality of the
!> `METADATA_BASE_A` abstract interface to specifically handle MultIO metadata.
!> It serves as a wrapper for setting various types of MultIO-related values
!> such as strings, integers, and arrays.
!>
!> @note This type is intended to be used in applications dealing with MultIO data
!>       where metadata manipulation is a common requirement.
!>
!> @see metadata_base_mod.F90
!>
!> @author Mirco Valentini
!> @date   January 12, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'multio_metadata_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MULTIO_METADATA_MOD'
MODULE MULTIO_METADATA_MOD

  ! Symbols imported from other modules within the project.
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported from external libraries
  USE :: MULTIO_API,        ONLY: MULTIO_METADATA
  USE :: MULTIO_API,        ONLY: MULTIO_HANDLE

IMPLICIT NONE

! Default visibility
PRIVATE

!> @brief Definition of the `MULTIO_METADATA_T` derived type.
!>
!> The `MULTIO_METADATA_T` type is a derived type that extends the functionality of the
!> `METADATA_BASE_A` abstract interface to specifically handle GRIB metadata.
!> It serves as a wrapper for setting various types of GRIB-related values
!> such as strings, integers, and arrays.
!>
!> @see METADATA_BASE_A
TYPE, EXTENDS(METADATA_BASE_A) :: MULTIO_METADATA_T

  !> Visibility of the members
  PRIVATE

  !> @brief Track the initialization status of the object
  LOGICAL :: INITIALIZED_ = .FALSE.


  !> @brief Multio Handle pointer to be used to initialize multio metadata
  TYPE(MULTIO_HANDLE), POINTER :: MULTIO_HANDLE_=>NULL()

  !> @brief Multio Metadata
  TYPE(MULTIO_METADATA) :: MULTIO_METADATA_

CONTAINS

  !> @brief Initialisation status of the object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INITIALIZED => MULTIO_METADATA_INITIALIZED

  !> @brief Get multio metadata handle
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_MULTIO_METADATA => MULTIO_METADATA_GET_MULTIO_METADATA

  !> @brief Associate the multio handle to the multio metadata object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_MULTIO_HANDLE => MULTIO_METADATA_SET_MULTIO_HANDLE

  !> @brief Associate the multio handle to the multio metadata object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_MULTIO_HANDLE => MULTIO_METADATA_GET_MULTIO_HANDLE

  !> @brief Initializes the object with default values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_DEFAULT => MULTIO_METADATA_INIT_DEFAULT

  !> @brief Initializes the object from anotehr metadata object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_FROM_METADATA => MULTIO_METADATA_INIT_FROM_METADATA

  !> @brief Initializes the object from a handle.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_FROM_SAMPLE => MULTIO_METADATA_INIT_FROM_SAMPLE

  !> @brief Initializes the object from a sample.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_FROM_SAMPLE_NAME => MULTIO_METADATA_INIT_FROM_SAMPLE_NAME

  !> @brief Destroys the object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: DESTROY => MULTIO_METADATA_DESTROY

  !> @brief Sets a missing value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_MISSING => MULTIO_METADATA_SET_MISSING

  !> @brief Sets a string value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_STRING => MULTIO_METADATA_SET_STRING

  !> @brief Sets a boolean value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_BOOL   => MULTIO_METADATA_SET_BOOL

  !> @brief Sets an 8-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT8  => MULTIO_METADATA_SET_INT8

  !> @brief Sets a 16-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT16 => MULTIO_METADATA_SET_INT16

  !> @brief Sets a 32-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT32 => MULTIO_METADATA_SET_INT32

  !> @brief Sets a 64-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT64 => MULTIO_METADATA_SET_INT64

  !> @brief Sets a 32-bit real value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL32  => MULTIO_METADATA_SET_REAL32

  !> @brief Sets a 64-bit real value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL64  => MULTIO_METADATA_SET_REAL64

  !> @brief Sets an array of string values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_STRING_ARRAY => MULTIO_METADATA_SET_STRING_ARRAY

  !> @brief Sets an array of boolean values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_BOOL_ARRAY   => MULTIO_METADATA_SET_BOOL_ARRAY

  !> @brief Sets an array of 8-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT8_ARRAY  => MULTIO_METADATA_SET_INT8_ARRAY

  !> @brief Sets an array of 16-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT16_ARRAY => MULTIO_METADATA_SET_INT16_ARRAY

  !> @brief Sets an array of 32-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT32_ARRAY => MULTIO_METADATA_SET_INT32_ARRAY

  !> @brief Sets an array of 64-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT64_ARRAY => MULTIO_METADATA_SET_INT64_ARRAY

  !> @brief Sets an array of 32-bit real values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL32_ARRAY  => MULTIO_METADATA_SET_REAL32_ARRAY

  !> @brief Sets an array of 64-bit real values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL64_ARRAY  => MULTIO_METADATA_SET_REAL64_ARRAY

END TYPE


! Whitelist of public symbols
PUBLIC :: MULTIO_METADATA_T

CONTAINS


!>
!> @brief Retrive the grib handle wrapped by this object.
!>
!> This procedure return the grib handle
!>
!> @param [inout] this The object to be initialized.
!>
!> @result handle The grib handle wrapped by the object.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_INITIALIZED'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_INITIALIZED( THIS, EX, HOOKS ) RESULT(RET)

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

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(IN)    :: THIS
  LOGICAL,                  INTENT(OUT)   :: EX
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

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

  ! This procedure can be called only if the object is not initialized
  EX = THIS%INITIALIZED_

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION MULTIO_METADATA_INITIALIZED
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Retrive the grib handle wrapped by this object.
!>
!> This procedure return the grib handle
!>
!> @param [inout] this The object to be initialized.
!>
!> @result handle The grib handle wrapped by the object.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_GET_MULTIO_METADATA'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_GET_MULTIO_METADATA( THIS, HANDLE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other modules within the project.
  USE :: MULTIO_API, ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), TARGET, INTENT(IN)    :: THIS
  TYPE(MULTIO_METADATA), POINTER,   INTENT(OUT)   :: HANDLE
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flag
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K

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

  ! This procedure can be called only if the object is not initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! Read the sample and set the initialization flag to .true.
  HANDLE => THIS%MULTIO_METADATA_

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle must be initialized' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_GET_MULTIO_METADATA
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Used to initialise the multio handle needed to create the multio metadata object.
!>
!> This routine associateds the multio_handle to be used to initialise the
!> multIO metadata object.
!>
!> @param [inout] this The object to be initialized.
!> @param [in]    mioh Multio handle to be used to initialise the metadata
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_MULTIO_HANDLE'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_MULTIO_HANDLE( THIS, MIOH, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GRIB_METADATA_MOD, ONLY: GRIB_METADATA_T

  ! Symbols imported from other modules within the project.
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),    INTENT(INOUT) :: THIS
  TYPE(MULTIO_HANDLE), TARGET, INTENT(IN)    :: MIOH
  TYPE(HOOKS_T),               INTENT(INOUT) :: HOOKS

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

  ! Associate pointer to multio handle
  THIS%MULTIO_HANDLE_ => MIOH

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION MULTIO_METADATA_SET_MULTIO_HANDLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Used to initialise the multio handle needed to create the multio metadata object.
!>
!> This routine associateds the multio_handle to be used to initialise the
!> multIO metadata object.
!>
!> @param [inout] this The object to be initialized.
!> @param [in]    mioh Multio handle to be used to initialise the metadata
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_GET_MULTIO_HANDLE'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_GET_MULTIO_HANDLE( THIS, MIOH, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GRIB_METADATA_MOD, ONLY: GRIB_METADATA_T

  ! Symbols imported from other modules within the project.
  USE :: MULTIO_API, ONLY: MULTIO_HANDLE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),     INTENT(INOUT) :: THIS
  TYPE(MULTIO_HANDLE), POINTER, INTENT(OUT)   :: MIOH
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

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

  ! Associate pointer to multio handle
  MIOH => THIS%MULTIO_HANDLE_

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION MULTIO_METADATA_GET_MULTIO_HANDLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Initializes the object by copying metadata from another object.
!>
!> This routine initializes the object by copying metadata from another
!> metadata object.
!>
!> @attention This method is not yet implemented and serves as a placeholder.
!>
!> @note This functionality is currently not required in the existing workflow.
!>       If a user finds a need for this feature, they are encouraged to
!>       provide an implementation.
!>
!> @param [inout] this         The object to be initialized.
!> @param [in]    metadata     Metadata object to be used for data copy.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_INIT_FROM_METADATA'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_INIT_FROM_METADATA( THIS, METADATA, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GRIB_METADATA_MOD, ONLY: GRIB_METADATA_T

  ! Symbols imported from other libraries
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
  CLASS(MULTIO_METADATA_T),        INTENT(INOUT) :: THIS
  CLASS(METADATA_BASE_A), POINTER, INTENT(IN)    :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_INITIALIZED
  INTEGER(KIND=JPIM_K) :: IGRIB_HANDLE
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local error codes
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_ALREADY_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_NOT_ASSOCIATED=2_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_NEW=3_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_COPY=4_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_UNABLE_TO_COPY_TRACER=6_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_UNABLE_TO_CHECK_INITIALIZATION=7_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_UNSUPPORTED_METADATA=8_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_GRIB_TO_MD=9_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_HANLDE=10_JPIM_K

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

  ! Check members
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, ERRFLAG_ALREADY_INITIALIZED )
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.ASSOCIATED(THIS%MULTIO_HANDLE_), ERRFLAG_NOT_ASSOCIATED )

  ! Check the metadata object
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CHECK_INITIALIZATION) METADATA%INITIALIZED( IS_INITIALIZED, HOOKS )
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.IS_INITIALIZED, ERRFLAG_NOT_ASSOCIATED )

  ! This procedure has been defined only for some specific implementations
  SELECT TYPE( MD => METADATA )

  !-------------------------------------------------------------------------------------------------
  ! Initialize from grib metadata
  CLASS IS( MULTIO_METADATA_T )

    ! Read the sample and set the initialization flag to .true.
    ERR = THIS%MULTIO_METADATA_%NEW( THIS%MULTIO_HANDLE_ )
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.0, ERRFLAG_MULTIO_METADATA_NEW )

    ERR = THIS%MULTIO_METADATA_%COPY( MD%MULTIO_METADATA_ )
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.0, ERRFLAG_MULTIO_METADATA_COPY )

    ! Set the initialization flag to .true.
    THIS%INITIALIZED_ = .TRUE.

  !-------------------------------------------------------------------------------------------------
  ! Initialize from grib metadata
  CLASS IS( GRIB_METADATA_T )

    ! Read the sample and set the initialization flag to .true.
    ERR = THIS%MULTIO_METADATA_%NEW( THIS%MULTIO_HANDLE_ )
    PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.0, ERRFLAG_MULTIO_METADATA_NEW )

    ! Clone the MultIO metadata from grib metadata
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_HANLDE) MD%GET_HANDLE( IGRIB_HANDLE, HOOKS )
    PP_TRYCALL(ERRFLAG_GRIB_TO_MD) MULTIO_GRIB_TO_MD( THIS%MULTIO_METADATA_, IGRIB_HANDLE, HOOKS )
    THIS%INITIALIZED_ = .TRUE.

  !-------------------------------------------------------------------------------------------------
  ! Not implemented
  CLASS DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNSUPPORTED_METADATA )
  END SELECT


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
    CASE (ERRFLAG_ALREADY_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle already initialized' )
    CASE (ERRFLAG_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Try to clone an object which is not initialized' )
    CASE (ERRFLAG_UNABLE_TO_CHECK_INITIALIZATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check initialization status of the metadata object' )
    CASE (ERRFLAG_MULTIO_METADATA_NEW)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create new metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE (ERRFLAG_MULTIO_METADATA_COPY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create new metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_COPY_TRACER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to copy the encoding trace' )
    CASE (ERRFLAG_UNSUPPORTED_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unsupported metadata object' )
    CASE (ERRFLAG_GRIB_TO_MD)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert grib metadata to multio metadata' )
    CASE (ERRFLAG_UNABLE_TO_GET_HANLDE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the handle from the metadata object' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_INIT_FROM_METADATA
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Initializes the object from an eccodes sample.
!>
!> This procedure initializes the object by loading data from an eccodes sample.
!>
!> @attention the sample must be already loaded in memory
!>
!> @param [inout] this          The object to be initialized.
!> @param [in]    sample_name   The name of the sample file from which to load data.
!> @param [in]    sample_handle The sample already loaded
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_INIT_FROM_SAMPLE_NAME'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_INIT_FROM_SAMPLE_NAME( THIS, SAMPLE_NAME, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other modules within the project.
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
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: SAMPLE_NAME
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL              :: LOC_DISTRIBUTE
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local error codes
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_ALREADY_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_MULTIO_HANDLE_NOT_ASSOCIATED=2_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_NEW=3_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_STRING=4_JPIM_K

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

  ! This procedure can be called only if the object is not initialized
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, ERRFLAG_ALREADY_INITIALIZED )
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.ASSOCIATED(THIS%MULTIO_HANDLE_), ERRFLAG_MULTIO_HANDLE_NOT_ASSOCIATED )

  ! Read the sample and set the initialization flag to .true.
  ERR = THIS%MULTIO_METADATA_%NEW( THIS%MULTIO_HANDLE_ )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.0, ERRFLAG_MULTIO_METADATA_NEW )

  ERR = THIS%MULTIO_METADATA_%SET_STRING( 'eccodes_sample', SAMPLE_NAME )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.0, ERRFLAG_MULTIO_METADATA_SET_STRING )
  THIS%INITIALIZED_ = .TRUE.

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
    CASE (ERRFLAG_ALREADY_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle already initialized' )
    CASE (ERRFLAG_MULTIO_HANDLE_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Multio handle not associated' )
    CASE (ERRFLAG_MULTIO_METADATA_NEW)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to initialize MultIO metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE (ERRFLAG_MULTIO_METADATA_SET_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to set SAMPLE name to the metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )

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

END FUNCTION MULTIO_METADATA_INIT_FROM_SAMPLE_NAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Initializes the object from an eccodes sample.
!>
!> This procedure initializes the object by loading data from an eccodes sample.
!>
!> @attention the sample must be already loaded in memory
!>
!> @param [inout] this          The object to be initialized.
!> @param [in]    sample_name   The name of the sample file from which to load data.
!> @param [in]    sample_handle The sample already loaded
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_INIT_FROM_SAMPLE'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_INIT_FROM_SAMPLE( THIS, SAMPLE_NAME, SAMPLE_HANDLE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from external libraries
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
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: SAMPLE_NAME
  INTEGER(KIND=JPIM_K),     INTENT(IN)    :: SAMPLE_HANDLE
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL              :: LOC_DISTRIBUTE
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local error codes
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_ALREADY_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_MULTIO_HANDLE_NOT_ASSOCIATED=2_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_NEW=3_JPIM_K
  INTEGER(KIND=JPIM_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_STRING=4_JPIM_K

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

  ! This procedure can be called only if the object is not initialized
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, ERRFLAG_ALREADY_INITIALIZED )
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.ASSOCIATED(THIS%MULTIO_HANDLE_), ERRFLAG_MULTIO_HANDLE_NOT_ASSOCIATED )

  ! Read the sample and set the initialization flag to .true.
  ERR = THIS%MULTIO_METADATA_%NEW( THIS%MULTIO_HANDLE_ )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.0, ERRFLAG_MULTIO_METADATA_NEW )

  ERR = THIS%MULTIO_METADATA_%SET_STRING( 'eccodes_sample', SAMPLE_NAME )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.0, ERRFLAG_MULTIO_METADATA_SET_STRING )
  THIS%INITIALIZED_ = .TRUE.

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
    CASE (ERRFLAG_ALREADY_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle already initialized' )
    CASE (ERRFLAG_MULTIO_HANDLE_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Multio handle not associated' )
    CASE (ERRFLAG_MULTIO_METADATA_NEW)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to initialize multio metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE (ERRFLAG_MULTIO_METADATA_SET_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to set SAMPLE name to the metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )

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

END FUNCTION MULTIO_METADATA_INIT_FROM_SAMPLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Initializes the object with default values.
!>
!> This procedure initializes the object with default values.
!>
!> @param [inout] this         The object to be initialized.
!> @param [in]    optionalData Optional data needed for the initialization of the handle
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_INIT_DEFAULT'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_INIT_DEFAULT( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from external libraries
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
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_HANDLE_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_NEW=3_JPIB_K

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

  ! This procedure can be called only if the object is not initialized
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, ERRFLAG_ALREADY_INITIALIZED )
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.ASSOCIATED(THIS%MULTIO_HANDLE_), ERRFLAG_MULTIO_HANDLE_NOT_ASSOCIATED )

  ! Read the sample and set the initialization flag to .true.
  ERR = THIS%MULTIO_METADATA_%NEW( THIS%MULTIO_HANDLE_ )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.0, ERRFLAG_MULTIO_METADATA_NEW )
  THIS%INITIALIZED_ = .TRUE.

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
    CASE (ERRFLAG_ALREADY_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle already initialized' )
    CASE (ERRFLAG_MULTIO_HANDLE_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Multio handle not associated' )
    CASE (ERRFLAG_MULTIO_METADATA_NEW)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create new multIO metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_INIT_DEFAULT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Destroys the object.
!>
!> This procedure deallocates resources associated with the object.
!>
!> @param [inout] this The object to be destroyed.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_DESTROY'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_DESTROY( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
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
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DELETE_METADATA=2_JPIM_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! Set the value into the handle
  ERR = THIS%MULTIO_METADATA_%DELETE( )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.0, ERRFLAG_DELETE_METADATA )
  THIS%INITIALIZED_ = .FALSE.


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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_DELETE_METADATA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Delete the metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_DESTROY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets a string value.
!>
!> This procedure sets a string value associated with a specified key.
!>
!> @param [inout] this The object where the string value is to be set.
!> @param [in]    key  The key used to store the string value.
!> @param [in]    val  The string value to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_MISSING'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_MISSING( THIS, KEY, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! MultIO metadata are dynamic, so we do not need to set missing values,
  ! we can just ignore the call. If a key is not in the metadata it is considered missing.

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_MISSING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets a string value.
!>
!> This procedure sets a string value associated with a specified key.
!>
!> @param [inout] this The object where the string value is to be set.
!> @param [in]    key  The key used to store the string value.
!> @param [in]    val  The string value to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_STRING'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_STRING( THIS, KEY, VAL, HOOKS )RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
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
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  CHARACTER(LEN=*),         INTENT(IN)    :: VAL
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_STRING=2_JPIM_K


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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! Set the value into the handle
  ERR = THIS%MULTIO_METADATA_%SET_STRING( KEY, VAL )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS, ERRFLAG_MULTIO_METADATA_SET_STRING )

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets a boolean value.
!>
!> This procedure sets a boolean value associated with a specified key.
!>
!> @param [inout] this The object where the boolean value is to be set.
!> @param [in]    key  The key used to store the boolean value.
!> @param [in]    val  The boolean value to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_BOOL'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_BOOL( THIS, KEY, VAL, HOOKS )RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
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
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  LOGICAL,                  INTENT(IN)    :: VAL
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KVAL
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_BOOL=2_JPIM_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! Convert the logical to a long
  IF ( VAL ) THEN
    KVAL = 1_JPIM_K
  ELSE
    KVAL = 0_JPIM_K
  ENDIF

  ! Set the value into the handle
  ERR = THIS%MULTIO_METADATA_%SET( KEY, KVAL )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS, ERRFLAG_MULTIO_METADATA_SET_BOOL )

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_BOOL)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_BOOL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an 8-bit integer value.
!>
!> This procedure sets an 8-bit integer value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 8-bit integer value to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT8'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_INT8( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
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
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  INTEGER(KIND=INT8),       INTENT(IN)    :: VAL
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_INT8=2_JPIM_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! Set the value into the handle
  ERR = THIS%MULTIO_METADATA_%SET( KEY, VAL )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS, ERRFLAG_MULTIO_METADATA_SET_INT8 )

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_INT8)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_INT8
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an 16-bit integer value.
!>
!> This procedure sets an 16-bit integer value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 16-bit integer value to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT16'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_INT16( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
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
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  INTEGER(KIND=INT16),      INTENT(IN)    :: VAL
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_INT16=2_JPIM_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! Set the value into the handle
  ERR = THIS%MULTIO_METADATA_%SET( KEY, VAL )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS, ERRFLAG_MULTIO_METADATA_SET_INT16 )

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to set the value inside the metadata' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_INT16)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_INT16
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an 32-bit integer value.
!>
!> This procedure sets an 32-bit integer value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 32-bit integer value to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT32'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_INT32( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
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
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  INTEGER(KIND=INT32),      INTENT(IN)    :: VAL
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: ERR

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_INT32=2_JPIM_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! Set the value into the handle
  ERR = THIS%MULTIO_METADATA_%SET( KEY, VAL )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS, ERRFLAG_MULTIO_METADATA_SET_INT32 )

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_INT32)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_INT32
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an 64-bit integer value.
!>
!> This procedure sets an 64-bit integer value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 64-bit integer value to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT64'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_INT64( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
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
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  INTEGER(KIND=INT64),      INTENT(IN)    :: VAL
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_INT64=2_JPIM_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! Set the value into the handle
  ERR = THIS%MULTIO_METADATA_%SET( KEY, VAL )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS, ERRFLAG_MULTIO_METADATA_SET_INT64 )

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_INT64)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_INT64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets a 32-bit real value.
!>
!> This procedure sets a 32-bit real value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 32-bit real value to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_REAL32'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_REAL32( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
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
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  REAL(KIND=REAL32),        INTENT(IN)    :: VAL
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_REAL32=2_JPIM_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! Set the value into the handle
  ERR = THIS%MULTIO_METADATA_%SET( KEY, VAL )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS, ERRFLAG_MULTIO_METADATA_SET_REAL32 )

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_REAL32)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle already initialized' )
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

END FUNCTION MULTIO_METADATA_SET_REAL32
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets a 64-bit real value.
!>
!> This procedure sets a 64-bit real value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 64-bit real value to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_REAL64'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_REAL64( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
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
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  REAL(KIND=REAL64),        INTENT(IN)    :: VAL
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_REAL64=2_JPIM_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! Set the value into the handle
  ERR = THIS%MULTIO_METADATA_%SET( KEY, VAL )
  PP_DEBUG_CRITICAL_COND_THROW( ERR.NE.MULTIO_SUCCESS, ERRFLAG_MULTIO_METADATA_SET_REAL64 )

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_REAL64)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
      MIO_ERR_STR = MULTIO_ERROR_STRING(ERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'multIO error: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ENDIF
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )

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

END FUNCTION MULTIO_METADATA_SET_REAL64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an array of strings values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of strings values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of strings values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_STRING_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),       INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),               INTENT(IN)    :: KEY
  CHARACTER(LEN=*), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_STRING_ARRAY=2_JPIM_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_MULTIO_METADATA_SET_STRING_ARRAY )

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Impossible to write an array of  16-bit integer on a grib handle' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_STRING_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an array of logical values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of logical values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of logical values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_BOOL_ARRAY'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_BOOL_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

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

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  LOGICAL, DIMENSION(:),    INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_BOOL_ARRAY=2_JPIB_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_MULTIO_METADATA_SET_BOOL_ARRAY )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_BOOL_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Impossible to write an array of  16-bit integer on a grib handle' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_BOOL_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets an array of 8-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 8-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 8-bit integer values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT8_ARRAY'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_INT8_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

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

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),         INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                 INTENT(IN)    :: KEY
  INTEGER(KIND=INT8), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_INT8_ARRAY=2_JPIB_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_MULTIO_METADATA_SET_INT8_ARRAY )

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_INT8_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Impossible to write an array of  16-bit integer on a grib handle' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_INT8_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Sets an array of 16-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 16-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 16-bit integer values representing the metadata to be stored.
!>
!> @note: not available in grib_handle functionalities
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT16_ARRAY'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_INT16_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

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

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT16), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_INT16_ARRAY=2_JPIB_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_MULTIO_METADATA_SET_INT16_ARRAY )

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_INT16_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Impossible to write an array of  16-bit integer on a grib handle' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_INT16_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Sets an array of 32-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 32-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 32-bit integer values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT32_ARRAY'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_INT32_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

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

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT32), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_INT32_ARRAY=2_JPIB_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! Set value to the grib_handle
  WRITE(*,*) 'Write arrays into the metadata is still no supported'
  ! PP_DEBUG_CRITICAL_THROW(ERRFLAG_MULTIO_METADATA_SET_INT32_ARRAY)

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_INT32_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Not implemented' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_INT32_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Sets an array of 64-bit integer values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 64-bit integer values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 64-bit integer values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT64_ARRAY'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_INT64_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT64), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_INT64_ARRAY=2_JPIB_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! This datatype is not supported by grib API
  WRITE(*,*) 'Write arrays into the metadata is still no supported'
  ! PP_DEBUG_CRITICAL_THROW(ERRFLAG_MULTIO_METADATA_SET_INT64_ARRAY)

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_INT64_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Impossible to write an array of  16-bit integer on a grib handle' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_INT64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Sets an array of 32-bit real values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 32-bit real values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 32-bit real values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_REAL32_ARRAY'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_REAL32_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

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

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),        INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL32), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_REAL32_ARRAY=2_JPIB_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! Set value to the grib_handle
  WRITE(*,*) 'Write arrays into the metadata is still no supported'
  ! PP_DEBUG_CRITICAL_THROW(ERRFLAG_MULTIO_METADATA_SET_REAL32_ARRAY)

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_REAL32_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'nimplemented double array in metadata' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_REAL32_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Sets an array of 64-bit real values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 64-bit real values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 64-bit real values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_REAL64_ARRAY'
PP_THREAD_SAFE FUNCTION MULTIO_METADATA_SET_REAL64_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

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

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),        INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL64), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MULTIO_METADATA_SET_REAL64_ARRAY=2_JPIB_K

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

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! Set value to the grib_handle
  WRITE(*,*) 'Write arrays into the metadata is still no supported'
  ! PP_DEBUG_CRITICAL_THROW(ERRFLAG_MULTIO_METADATA_SET_REAL64_ARRAY)

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
    CASE (ERRFLAG_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_MULTIO_METADATA_SET_REAL64_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unimplemented double array in metadata' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_METADATA_SET_REAL64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB_TO_MD_SPECTRAL'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB_TO_MD_SPECTRAL( MIOMD, KGRIB_HANDLE, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
  USE :: ECCODES,    ONLY: CODES_GET
  USE :: ECCODES,    ONLY: CODES_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING
  USE :: ECCODES,    ONLY: CODES_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_METADATA), INTENT(INOUT) :: MIOMD
  INTEGER(KIND=JPIM_K),  INTENT(IN)    :: KGRIB_HANDLE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variabels
  INTEGER(KIND=INT64)  :: IVALUE
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIM_K) :: CERR

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


  CALL CODES_GET(KGRIB_HANDLE,'sphericalHarmonics',IVALUE,STATUS=KRET)
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 50 )
  CERR = MIOMD%SET('sphericalHarmonics',IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 1 )


  CALL CODES_GET(KGRIB_HANDLE,'complexPacking',IVALUE,STATUS=KRET)
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 51 )
  CERR = MIOMD%SET('complexPacking',IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 2 )


  CALL CODES_GET(KGRIB_HANDLE,'pentagonalResolutionParameterJ',IVALUE,STATUS=KRET)
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 52 )
  CERR = MIOMD%SET('pentagonalResolutionParameterJ',IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 3 )


  CALL CODES_GET(KGRIB_HANDLE,'pentagonalResolutionParameterK',IVALUE,STATUS=KRET)
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 53 )
  CERR = MIOMD%SET('pentagonalResolutionParameterK',IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 4 )


  CALL CODES_GET(KGRIB_HANDLE,'pentagonalResolutionParameterM',IVALUE,STATUS=KRET)
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 54 )
  CERR = MIOMD%SET('pentagonalResolutionParameterM',IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 5 )


  CALL CODES_GET(KGRIB_HANDLE,'subSetJ',IVALUE,STATUS=KRET)
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 55 )
  CERR = MIOMD%SET('subSetJ',IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 6 )


  CALL CODES_GET(KGRIB_HANDLE,'subSetK',IVALUE,STATUS=KRET)
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 56 )
  CERR = MIOMD%SET('subSetK',IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 7 )


  CALL CODES_GET(KGRIB_HANDLE,'subSetM',IVALUE,STATUS=KRET)
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 57 )
  CERR = MIOMD%SET('subSetM',IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 8 )


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
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (sphericalHarmonics): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (2)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (complexPacking): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (3)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (pentagonalResolutionParameterJ): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (4)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (pentagonalResolutionParameterK): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (5)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (pentagonalResolutionParameterM): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (6)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (subSetJ): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (7)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (subSetK): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (8)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (subSetM): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )


    CASE (50)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "sphericalHarmonics" from handle', KRET, GRIB_ERROR )
    CASE (51)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "complexPacking" from handle', KRET, GRIB_ERROR )
    CASE (52)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "pentagonalResolutionParameterJ" from handle', KRET, GRIB_ERROR )
    CASE (53)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "pentagonalResolutionParameterK" from handle', KRET, GRIB_ERROR )
    CASE (54)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "pentagonalResolutionParameterM" from handle', KRET, GRIB_ERROR )
    CASE (55)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "subSetJ" from handle', KRET, GRIB_ERROR )
    CASE (56)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "subSetK" from handle', KRET, GRIB_ERROR )
    CASE (57)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "subSetM" from handle', KRET, GRIB_ERROR )

    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_GRIB_TO_MD_SPECTRAL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB_TO_MD'
PP_THREAD_SAFE FUNCTION MULTIO_GRIB_TO_MD( MIOMD, KGRIB_HANDLE, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported from other libraries
  USE :: ECCODES,    ONLY: CODES_KEYS_ITERATOR_NEW
  USE :: ECCODES,    ONLY: CODES_KEYS_ITERATOR_NEXT
  USE :: ECCODES,    ONLY: CODES_KEYS_ITERATOR_GET_NAME
  USE :: ECCODES,    ONLY: CODES_GET
  USE :: ECCODES,    ONLY: CODES_IS_DEFINED
  USE :: ECCODES,    ONLY: CODES_KEYS_ITERATOR_DELETE
  USE :: ECCODES,    ONLY: CODES_GET_ERROR_STRING
  USE :: ECCODES,    ONLY: CODES_SUCCESS
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
  TYPE(MULTIO_METADATA), INTENT(INOUT) :: MIOMD
  INTEGER(KIND=JPIM_K),  INTENT(IN)    :: KGRIB_HANDLE
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIM_K) :: IRET
  INTEGER(KIND=JPIM_K) :: IHAS
  INTEGER(KIND=INT64)  :: PARAMID
  INTEGER(KIND=INT64)  :: LEVEL
  INTEGER(KIND=INT64)  :: DATADATE
  INTEGER(KIND=INT64)  :: DATATIME
  INTEGER(KIND=INT64)  :: STEPUNITS
  INTEGER(KIND=INT64)  :: STARTSTEP
  INTEGER(KIND=INT64)  :: ENDSTEP
  INTEGER(KIND=INT64)  :: TIMEINCREMENTUNIT
  INTEGER(KIND=INT64)  :: TIMEINCREMENT
  CHARACTER(LEN=256)   :: VALUE
  CHARACTER(LEN=256)   :: GRIDTYPE
  INTEGER(KIND=JPIM_K) :: CERR

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: UNABLE_TO_CALL_GRIB_TO_MDS=1001_JPIB_K

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

  ! Configure grib edition
  CALL CODES_GET( KGRIB_HANDLE, 'edition', VALUE, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 53 )
  CERR = MIOMD%SET( 'gribEdition', VALUE )
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 2 )


  IF (VALUE.EQ. '2') THEN
    CALL CODES_GET( KGRIB_HANDLE, 'date', DATADATE, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 54 )
    CERR = MIOMD%SET( 'date', DATADATE )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 3 )

    CALL CODES_GET( KGRIB_HANDLE, 'time', DATATIME, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 55 )
    CERR = MIOMD%SET( "time", DATATIME )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 4 )

    CALL CODES_GET( KGRIB_HANDLE, 'stepUnits', STEPUNITS, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 56 )
    CERR = MIOMD%SET( 'stepUnits', STEPUNITS )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 5 )

    CALL CODES_GET( KGRIB_HANDLE, 'startStep', STARTSTEP, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 57 )
    CERR = MIOMD%SET('startStep',STARTSTEP)
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 6 )

    CALL CODES_GET( KGRIB_HANDLE, 'endStep', ENDSTEP, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 58 )
    CERR = MIOMD%SET( 'endStep', ENDSTEP )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 7 )

    ! TODO: time increment should always be defined, no need for the check
    CALL CODES_IS_DEFINED( KGRIB_HANDLE, 'timeIncrement', IHAS, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 59 )
    IF (IHAS.NE. 0) THEN

      CALL CODES_GET( KGRIB_HANDLE, 'indicatorOfUnitForTimeIncrement', TIMEINCREMENTUNIT, STATUS=KRET )
      PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 60 )
      CERR = MIOMD%SET( 'indicatorOfUnitForTimeIncrement', TIMEINCREMENTUNIT )
      PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 8 )

      CALL CODES_GET( KGRIB_HANDLE, 'timeIncrement', TIMEINCREMENT, STATUS=KRET )
      PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 61 )
      CERR = MIOMD%SET( 'timeIncrement', TIMEINCREMENT )
      PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 9 )

    ENDIF
  ELSE
    CALL CODES_GET( KGRIB_HANDLE, 'stepUnits', STEPUNITS, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 62 )
    CERR = MIOMD%SET( 'stepUnits', STEPUNITS )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 10 )

    CALL CODES_GET( KGRIB_HANDLE, 'startStep', STARTSTEP, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 63 )
    CERR = MIOMD%SET( 'startStep', STARTSTEP )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 11 )

    CALL CODES_GET( KGRIB_HANDLE, 'endStep', ENDSTEP, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 64 )
    CERR = MIOMD%SET( 'endStep', ENDSTEP )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 12 )
  ENDIF

  CALL CODES_GET( KGRIB_HANDLE, 'paramId', PARAMID, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 65 )
  CERR = MIOMD%SET( 'paramId', PARAMID )
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 13 )

  CALL CODES_GET( KGRIB_HANDLE, 'gridType', GRIDTYPE, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 66 )
  CERR = MIOMD%SET( 'gridType', GRIDTYPE )
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 14 )

  CALL CODES_GET( KGRIB_HANDLE, 'levtype', VALUE, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 67 )
  CERR = MIOMD%SET( 'levtype', VALUE )
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 15 )

  CALL CODES_GET( KGRIB_HANDLE, 'level', LEVEL, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 68 )
  CERR = MIOMD%SET( 'level', LEVEL )
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 16 )

  CALL CODES_GET( KGRIB_HANDLE, 'class', VALUE, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 69 )
  CERR = MIOMD%SET( 'class', VALUE )
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 17 )

  CALL CODES_GET( KGRIB_HANDLE, 'stream', VALUE, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 70 )
  CERR = MIOMD%SET( 'stream', VALUE )
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 18 )

  CALL CODES_GET( KGRIB_HANDLE, 'type', VALUE, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 71 )
  CERR = MIOMD%SET( 'type', VALUE )
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 19 )

  CALL CODES_GET( KGRIB_HANDLE, 'expver', VALUE, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, 72 )
  CERR = MIOMD%SET( 'expver', VALUE )
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 20 )



  IF ( TRIM(ADJUSTL(GRIDTYPE)) .EQ. 'sh') THEN
    PP_TRYCALL(UNABLE_TO_CALL_GRIB_TO_MDS) MULTIO_GRIB_TO_MD_SPECTRAL( MIOMD, KGRIB_HANDLE, HOOKS )
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
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)

    CASE (2)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (gribEdition): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (3)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (startDate): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (4)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (startTime): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (5)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (stepUnits): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (6)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (startStep): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (7)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (endStep): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (8)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (indicatorOfUnitForTimeIncrement): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (9)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (timeIncrement): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (10)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (stepUnits): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (11)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (startStep): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (12)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (endStep): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (13)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (paramId): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (14)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (gridType): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (15)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (levtype): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (16)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (level): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (17)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (class): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (18)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (stream): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (19)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (type): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (20)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set MultIO metadata (expver): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )

    CASE (53)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "edition" from handle', KRET, GRIB_ERROR )
    CASE (54)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "dataDate" from handle ', KRET, GRIB_ERROR )
    CASE (55)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "dataTime" from handle ', KRET, GRIB_ERROR )
    CASE (56)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "stepUnits" from handle ', KRET, GRIB_ERROR )
    CASE (57)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "startStep" from handle ', KRET, GRIB_ERROR )
    CASE (58)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "endStep" from handle ', KRET, GRIB_ERROR )
    CASE (59)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "timeIncrement" from handle ', KRET, GRIB_ERROR )
    CASE (60)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "indicatorOfUnitForTimeIncrement" from handle ', KRET, GRIB_ERROR )
    CASE (61)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "timeIncrement" from handle ', KRET, GRIB_ERROR )
    CASE (62)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "stepUnits" from handle ', KRET, GRIB_ERROR )
    CASE (63)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "startStep" from handle ', KRET, GRIB_ERROR )
    CASE (64)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "endStep" from handle ', KRET, GRIB_ERROR )
    CASE (65)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "paramId" from handle ', KRET, GRIB_ERROR )
    CASE (66)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "gridType" from handle ', KRET, GRIB_ERROR )
    CASE (67)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "levtype" from handle ', KRET, GRIB_ERROR )
    CASE (68)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "level" from handle ', KRET, GRIB_ERROR )
    CASE (69)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "class" from handle ', KRET, GRIB_ERROR )
    CASE (70)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "stream" from handle ', KRET, GRIB_ERROR )
    CASE (71)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "type" from handle ', KRET, GRIB_ERROR )
    CASE (72)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL CODES_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to get grib "expver" from handle ', KRET, GRIB_ERROR )
    CASE (UNABLE_TO_CALL_GRIB_TO_MDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call MULTIO_GRIB_TO_MD_SPECTRAL' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unhandled error' )
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

END FUNCTION MULTIO_GRIB_TO_MD
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE MULTIO_METADATA_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME