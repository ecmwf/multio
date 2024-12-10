!> @file
!>
!> @brief Definition of the `LOG_METADATA_T` derived type.
!>
!> The `LOG_METADATA_T` type is a derived type that extends the functionality of the
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
#define PP_FILE_NAME 'log_metadata_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'LOG_METADATA_MOD'
MODULE LOG_METADATA_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

IMPLICIT NONE

! Default visibility
PRIVATE

!> @brief Definition of the `LOG_METADATA_T` derived type.
!>
!> The `LOG_METADATA_T` type is a derived type that extends the functionality of the
!> `METADATA_BASE_A` abstract interface to specifically handle GRIB metadata.
!> It serves as a wrapper for setting various types of GRIB-related values
!> such as strings, integers, and arrays.
!>
!> @see METADATA_BASE_A
TYPE, EXTENDS(METADATA_BASE_A) :: LOG_METADATA_T

  !> Visibility of the members
  PRIVATE

  !> @brief Track the initialization status of the object
  LOGICAL :: INITIALIZED_ = .FALSE.


  !> @brief Multio Handle pointer to be used to initialize multio metadata
  INTEGER(KIND=JPIB_K) :: UNIT_=-1_JPIB_K
  INTEGER(KIND=JPIB_K) :: N_=10_JPIB_K

CONTAINS

  !> @brief Initialisation status of the object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INITIALIZED => LOG_METADATA_INITIALIZED

  !> @brief Associate the multio handle to the multio metadata object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_LOG_HANDLE => LOG_METADATA_SET_LOG_HANDLE

  !> @brief Initializes the object with default values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_DEFAULT => LOG_METADATA_INIT_DEFAULT

  !> @brief Initializes the object from anotehr metadata object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_FROM_METADATA => LOG_METADATA_INIT_FROM_METADATA

  !> @brief Initializes the object from a handle.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_FROM_SAMPLE => LOG_METADATA_INIT_FROM_SAMPLE

  !> @brief Initializes the object from a sample.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_FROM_SAMPLE_NAME => LOG_METADATA_INIT_FROM_SAMPLE_NAME

  !> @brief Destroys the object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: DESTROY => LOG_METADATA_DESTROY

  !> @brief Sets a missing value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_MISSING => LOG_METADATA_SET_MISSING

  !> @brief Sets a string value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_STRING => LOG_METADATA_SET_STRING

  !> @brief Sets a boolean value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_BOOL   => LOG_METADATA_SET_BOOL

  !> @brief Sets an 8-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT8  => LOG_METADATA_SET_INT8

  !> @brief Sets a 16-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT16 => LOG_METADATA_SET_INT16

  !> @brief Sets a 32-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT32 => LOG_METADATA_SET_INT32

  !> @brief Sets a 64-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT64 => LOG_METADATA_SET_INT64

  !> @brief Sets a 32-bit real value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL32  => LOG_METADATA_SET_REAL32

  !> @brief Sets a 64-bit real value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL64  => LOG_METADATA_SET_REAL64

  !> @brief Sets an array of string values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_STRING_ARRAY => LOG_METADATA_SET_STRING_ARRAY

  !> @brief Sets an array of boolean values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_BOOL_ARRAY   => LOG_METADATA_SET_BOOL_ARRAY

  !> @brief Sets an array of 8-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT8_ARRAY  => LOG_METADATA_SET_INT8_ARRAY

  !> @brief Sets an array of 16-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT16_ARRAY => LOG_METADATA_SET_INT16_ARRAY

  !> @brief Sets an array of 32-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT32_ARRAY => LOG_METADATA_SET_INT32_ARRAY

  !> @brief Sets an array of 64-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT64_ARRAY => LOG_METADATA_SET_INT64_ARRAY

  !> @brief Sets an array of 32-bit real values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL32_ARRAY  => LOG_METADATA_SET_REAL32_ARRAY

  !> @brief Sets an array of 64-bit real values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL64_ARRAY  => LOG_METADATA_SET_REAL64_ARRAY

END TYPE


! Whitelist of public symbols
PUBLIC :: LOG_METADATA_T

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
#define PP_PROCEDURE_NAME 'LOG_METADATA_INITIALIZED'
PP_THREAD_SAFE FUNCTION LOG_METADATA_INITIALIZED( THIS, EX, HOOKS ) RESULT(RET)

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
  CLASS(LOG_METADATA_T), INTENT(IN)    :: THIS
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

END FUNCTION LOG_METADATA_INITIALIZED
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Used to initialise the multio handle needed to create the multio metadata object.
!>
!> This routine associateds the log_handle to be used to initialise the
!> multIO metadata object.
!>
!> @param [inout] this The object to be initialized.
!> @param [in]    mioh Multio handle to be used to initialise the metadata
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_LOG_HANDLE'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_LOG_HANDLE( THIS, UNIT, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GRIB_METADATA_MOD, ONLY: GRIB_METADATA_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: UNIT
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

  ! Associate pointer to multio handle
  THIS%UNIT_ = UNIT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION LOG_METADATA_SET_LOG_HANDLE
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_INIT_FROM_METADATA'
PP_THREAD_SAFE FUNCTION LOG_METADATA_INIT_FROM_METADATA( THIS, METADATA, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GRIB_METADATA_MOD, ONLY: GRIB_METADATA_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(LOG_METADATA_T),           INTENT(INOUT) :: THIS
  CLASS(METADATA_BASE_A), POINTER, INTENT(IN)    :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

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

  WRITE(*,*) 'LOG_METADATA_INIT_FROM_METADATA'

  ! Set the initialization flag to .true.
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

END FUNCTION LOG_METADATA_INIT_FROM_METADATA
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_INIT_FROM_SAMPLE_NAME'
PP_THREAD_SAFE FUNCTION LOG_METADATA_INIT_FROM_SAMPLE_NAME( THIS, SAMPLE_NAME, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: SAMPLE_NAME
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

  WRITE(*,*) 'LOG_METADATA_INIT_FROM_SAMPLE_NAME: '//TRIM(ADJUSTL(SAMPLE_NAME))//'.tmpl'

  ! Set the initialization flag to .true.
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

END FUNCTION LOG_METADATA_INIT_FROM_SAMPLE_NAME
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_INIT_FROM_SAMPLE'
PP_THREAD_SAFE FUNCTION LOG_METADATA_INIT_FROM_SAMPLE( THIS, SAMPLE_NAME, SAMPLE_HANDLE, HOOKS ) RESULT(RET)

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
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(IN)    :: SAMPLE_NAME
  INTEGER(KIND=JPIM_K),  INTENT(IN)    :: SAMPLE_HANDLE
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

  ! Set the initialization flag to .true.
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

END FUNCTION LOG_METADATA_INIT_FROM_SAMPLE
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_INIT_DEFAULT'
PP_THREAD_SAFE FUNCTION LOG_METADATA_INIT_DEFAULT( THIS, HOOKS ) RESULT(RET)

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
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: ERR

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

  ! Set the initialization flag to .true.
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

END FUNCTION LOG_METADATA_INIT_DEFAULT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Destroys the object.
!>
!> This procedure deallocates resources associated with the object.
!>
!> @param [inout] this The object to be destroyed.
!>
#define PP_PROCEDURE_TYPE 'PP_THREAD_SAFE FUNCTION'
#define PP_PROCEDURE_NAME 'LOG_METADATA_DESTROY'
PP_THREAD_SAFE FUNCTION LOG_METADATA_DESTROY( THIS, HOOKS ) RESULT(RET)

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
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
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

END FUNCTION LOG_METADATA_DESTROY
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_MISSING'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_MISSING( THIS, KEY, HOOKS ) RESULT(RET)

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
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(IN)    :: KEY
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: IOSTAT

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

  ! Log the missing value
  WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) PP_PROCEDURE_NAME//': "'//TRIM(ADJUSTL(KEY))//'"'

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
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION LOG_METADATA_SET_MISSING
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_STRING'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_STRING( THIS, KEY, VAL, HOOKS )RESULT(RET)

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
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(IN)    :: KEY
  CHARACTER(LEN=*),      INTENT(IN)    :: VAL
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: IOSTAT

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_STRING=2_JPIM_K

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
  WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) PP_PROCEDURE_NAME//': "'//TRIM(ADJUSTL(KEY))//'" = '//TRIM(ADJUSTL(VAL))
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_STRING )

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
    CASE (ERRFLAG_LOG_METADATA_SET_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
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

END FUNCTION LOG_METADATA_SET_STRING
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_BOOL'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_BOOL( THIS, KEY, VAL, HOOKS )RESULT(RET)

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
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  LOGICAL,                  INTENT(IN)    :: VAL
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=7) :: KVAL
  INTEGER(KIND=JPIM_K)  :: IOSTAT

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_STRING=2_JPIM_K

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
  KVAL = REPEAT(' ',7)
  IF ( VAL ) THEN
    KVAL = '.TRUE.'
  ELSE
    KVAL = '.FALSE.'
  ENDIF

  ! Set the value into the handle
  WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) PP_PROCEDURE_NAME//': "'//TRIM(ADJUSTL(KEY))//'" = '//TRIM(ADJUSTL(KVAL))
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_STRING )

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
    CASE (ERRFLAG_LOG_METADATA_SET_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
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

END FUNCTION LOG_METADATA_SET_BOOL
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_INT8'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_INT8( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

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
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(IN)    :: KEY
  INTEGER(KIND=INT8),    INTENT(IN)    :: VAL
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: IOSTAT
  CHARACTER(LEN=32) :: CTMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_INT8=2_JPIM_K

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
  CTMP=REPEAT(' ',32)
  WRITE(CTMP,*,IOSTAT=IOSTAT) VAL
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_INT8 )
  WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) PP_PROCEDURE_NAME//': "'//TRIM(ADJUSTL(KEY))//'" = '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_INT8 )

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
    CASE (ERRFLAG_LOG_METADATA_SET_INT8)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
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

END FUNCTION LOG_METADATA_SET_INT8
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_INT16'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_INT16( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

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
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  INTEGER(KIND=INT16),      INTENT(IN)    :: VAL
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: IOSTAT
  CHARACTER(LEN=32) :: CTMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_INT16=2_JPIM_K

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
  CTMP=REPEAT(' ',32)
  WRITE(CTMP,*,IOSTAT=IOSTAT) VAL
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_INT16 )
  WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) PP_PROCEDURE_NAME//': "'//TRIM(ADJUSTL(KEY))//'" = '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_INT16 )

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Failed to set the value inside the metadata' )
    CASE (ERRFLAG_LOG_METADATA_SET_INT16)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
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

END FUNCTION LOG_METADATA_SET_INT16
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_INT32'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_INT32( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

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
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(IN)    :: KEY
  INTEGER(KIND=INT32),   INTENT(IN)    :: VAL
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: IOSTAT
  CHARACTER(LEN=32) :: CTMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_INT32=2_JPIM_K

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
  CTMP=REPEAT(' ',32)
  WRITE(CTMP,*,IOSTAT=IOSTAT) VAL
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_INT32 )
  WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) PP_PROCEDURE_NAME//': "'//TRIM(ADJUSTL(KEY))//'" = '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_INT32 )

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
    CASE (ERRFLAG_LOG_METADATA_SET_INT32)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
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

END FUNCTION LOG_METADATA_SET_INT32
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_INT64'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_INT64( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(IN)    :: KEY
  INTEGER(KIND=INT64),   INTENT(IN)    :: VAL
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: IOSTAT
  CHARACTER(LEN=32) :: CTMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_INT64=2_JPIM_K

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
  CTMP=REPEAT(' ',32)
  WRITE(CTMP,*,IOSTAT=IOSTAT) VAL
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_INT64 )
  WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) PP_PROCEDURE_NAME//': "'//TRIM(ADJUSTL(KEY))//'" = '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_INT64 )

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
    CASE (ERRFLAG_LOG_METADATA_SET_INT64)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
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

END FUNCTION LOG_METADATA_SET_INT64
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_REAL32'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_REAL32( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

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
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(IN)    :: KEY
  REAL(KIND=REAL32),     INTENT(IN)    :: VAL
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: IOSTAT
  CHARACTER(LEN=32) :: CTMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_REAL32=2_JPIM_K

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
  CTMP=REPEAT(' ',32)
  WRITE(CTMP,*,IOSTAT=IOSTAT) VAL
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_REAL32 )
  WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) PP_PROCEDURE_NAME//': "'//TRIM(ADJUSTL(KEY))//'" = '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_REAL32 )

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
    CASE (ERRFLAG_LOG_METADATA_SET_REAL32)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle already initialized' )
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

END FUNCTION LOG_METADATA_SET_REAL32
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_REAL64'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_REAL64( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

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
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(IN)    :: KEY
  REAL(KIND=REAL64),     INTENT(IN)    :: VAL
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: IOSTAT
  CHARACTER(LEN=32) :: CTMP

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_REAL64=2_JPIM_K

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
  CTMP=REPEAT(' ',32)
  WRITE(CTMP,*,IOSTAT=IOSTAT) VAL
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_REAL64 )
  WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) PP_PROCEDURE_NAME//': "'//TRIM(ADJUSTL(KEY))//'" = '//TRIM(ADJUSTL(CTMP))
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_REAL64 )

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
    CASE (ERRFLAG_LOG_METADATA_SET_REAL64)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write into multio metadata' )
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

END FUNCTION LOG_METADATA_SET_REAL64
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_STRING_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

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
  CLASS(LOG_METADATA_T),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),               INTENT(IN)    :: KEY
  CHARACTER(LEN=*), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K)  :: IOSTAT
  INTEGER(KIND=JPIM_K)  :: I

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIM_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_STRING=2_JPIM_K

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
  WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) PP_PROCEDURE_NAME//': "'//TRIM(ADJUSTL(KEY))//'" = [ ...'
  IF ( SIZE(VALUES) .GT. 1 ) THEN
    DO I = 1, SIZE(VALUES)-1
      IF ( MOD(I-1,THIS%N_) .NE. 0 ) THEN
        WRITE(THIS%UNIT_,'(A)',ADVANCE='NO',IOSTAT=IOSTAT) '"'//TRIM(ADJUSTL(VALUES(I)))//'", '
        PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_STRING )
      ELSE
        WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) '"'//TRIM(ADJUSTL(VALUES(I)))//'", ...'
        PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_STRING )
      ENDIF
    ENDDO
  ENDIF
  WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) '"'//TRIM(ADJUSTL(VALUES(I)))//'" ]'
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_STRING )

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
    CASE (ERRFLAG_LOG_METADATA_SET_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Impossible to write an array of  16-bit integer on a grib handle' )
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

END FUNCTION LOG_METADATA_SET_STRING_ARRAY
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_BOOL_ARRAY'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_BOOL_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

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
  CLASS(LOG_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),      INTENT(IN)    :: KEY
  LOGICAL, DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),         INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: IOSTAT
  CHARACTER(LEN=32) :: CTMP
  INTEGER(KIND=JPIB_K)  :: I

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_BOOL=2_JPIB_K

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

  ! This datatype is not supported by grib API
  WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) PP_PROCEDURE_NAME//': "'//TRIM(ADJUSTL(KEY))//'" = [ ...'
  IF ( SIZE(VALUES) .GT. 1 ) THEN
    DO I = 1, SIZE(VALUES)-1
      CTMP = REPEAT(' ',32)
      IF ( VALUES(I) ) THEN
        CTMP = '.TRUE.'
      ELSE
        CTMP = '.FALSE.'
      ENDIF
      IF ( MOD(I-1,THIS%N_) .NE. 0 ) THEN
        WRITE(THIS%UNIT_,'(A)',ADVANCE='NO',IOSTAT=IOSTAT) '"'//TRIM(ADJUSTL(CTMP))//'", '
        PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_BOOL )
      ELSE
        WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) '"'//TRIM(ADJUSTL(CTMP))//'", ...'
        PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_BOOL )
      ENDIF
    ENDDO
  ENDIF
  CTMP = REPEAT(' ',32)
  IF ( VALUES(I) ) THEN
    CTMP = '.TRUE.'
  ELSE
    CTMP = '.FALSE.'
  ENDIF
  WRITE(THIS%UNIT_,*,IOSTAT=IOSTAT) '"'//TRIM(ADJUSTL(CTMP))//'" ]'
  PP_DEBUG_CRITICAL_COND_THROW( IOSTAT.NE.0, ERRFLAG_LOG_METADATA_SET_BOOL )

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
    CASE (ERRFLAG_LOG_METADATA_SET_BOOL)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Impossible to write an array of  16-bit integer on a grib handle' )
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

END FUNCTION LOG_METADATA_SET_BOOL_ARRAY
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_INT8_ARRAY'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_INT8_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

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
  CLASS(LOG_METADATA_T),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                 INTENT(IN)    :: KEY
  INTEGER(KIND=INT8), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_STRING=2_JPIB_K

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
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_LOG_METADATA_SET_STRING )

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
    CASE (ERRFLAG_LOG_METADATA_SET_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Impossible to write an array of  16-bit integer on a grib handle' )
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

END FUNCTION LOG_METADATA_SET_INT8_ARRAY
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_INT16_ARRAY'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_INT16_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

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
  CLASS(LOG_METADATA_T),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT16), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_STRING=2_JPIB_K

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
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_LOG_METADATA_SET_STRING )

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
    CASE (ERRFLAG_LOG_METADATA_SET_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Impossible to write an array of  16-bit integer on a grib handle' )
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

END FUNCTION LOG_METADATA_SET_INT16_ARRAY
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_INT32_ARRAY'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_INT32_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

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
  CLASS(LOG_METADATA_T),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT32), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_STRING=2_JPIB_K

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
  ! PP_DEBUG_CRITICAL_THROW(ERRFLAG_LOG_METADATA_SET_STRING)

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
    CASE (2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Not implemented' )
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

END FUNCTION LOG_METADATA_SET_INT32_ARRAY
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_INT64_ARRAY'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_INT64_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

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
  CLASS(LOG_METADATA_T),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT64), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_STRING=2_JPIB_K

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
  ! PP_DEBUG_CRITICAL_THROW(ERRFLAG_LOG_METADATA_SET_STRING)

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
    CASE (ERRFLAG_LOG_METADATA_SET_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Impossible to write an array of  16-bit integer on a grib handle' )
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

END FUNCTION LOG_METADATA_SET_INT64_ARRAY
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_REAL32_ARRAY'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_REAL32_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

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
  CLASS(LOG_METADATA_T),        INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL32), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_STRING=2_JPIB_K

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
  ! PP_DEBUG_CRITICAL_THROW(ERRFLAG_LOG_METADATA_SET_STRING)

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
    CASE (ERRFLAG_LOG_METADATA_SET_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'nimplemented double array in metadata' )
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

END FUNCTION LOG_METADATA_SET_REAL32_ARRAY
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
#define PP_PROCEDURE_NAME 'LOG_METADATA_SET_REAL64_ARRAY'
PP_THREAD_SAFE FUNCTION LOG_METADATA_SET_REAL64_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

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
  CLASS(LOG_METADATA_T),        INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL64), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOG_METADATA_SET_STRING=2_JPIB_K

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
  ! PP_DEBUG_CRITICAL_THROW(ERRFLAG_LOG_METADATA_SET_STRING)

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
    CASE (ERRFLAG_LOG_METADATA_SET_STRING)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unimplemented double array in metadata' )
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

END FUNCTION LOG_METADATA_SET_REAL64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE LOG_METADATA_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME