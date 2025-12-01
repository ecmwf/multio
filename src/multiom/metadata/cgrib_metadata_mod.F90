!> @file
!>
!> @brief Definition of the `CGRIB_METADATA_T` derived type.
!>
!> The `CGRIB_METADATA_T` type is a derived type that extends the functionality of the
!> `METADATA_BASE_A` abstract interface to specifically handle GRIB metadata.
!> It serves as a wrapper for setting various types of GRIB-related values
!> such as strings, integers, and arrays.
!>
!> @note This type is intended to be used in applications dealing with GRIB data
!>       where metadata manipulation is a common requirement.
!>
!> @tod use `grib_api` instead of `grib_api_interface` to break the depency
!> from ifs
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
#define PP_FILE_NAME 'CGRIB_METADATA_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'CGRIB_METADATA_MOD'
MODULE CGRIB_METADATA_MOD

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

IMPLICIT NONE

! Default visibility
PRIVATE

! Set grib constants
INTEGER(KIND=C_INT), PARAMETER :: GRIB_SUCCESS = 0_C_INT
INTEGER(KIND=JPIB_K), PARAMETER :: ERRMSG_LEN = 4096_JPIB_K


!> @brief Definition of the `CGRIB_METADATA_T` derived type.
!>
!> The `CGRIB_METADATA_T` type is a derived type that extends the functionality of the
!> `METADATA_BASE_A` abstract interface to specifically handle GRIB metadata.
!> It serves as a wrapper for setting various types of GRIB-related values
!> such as strings, integers, and arrays.
!>
!> @see METADATA_BASE_A
TYPE, EXTENDS(METADATA_BASE_A) :: CGRIB_METADATA_T

  !> Visibility of the members
  PRIVATE

  !> @brief Track the initialization status of the object
  LOGICAL :: INITIALIZED_ = .FALSE.

  !> @brief Grib handle
  TYPE(C_PTR) :: CGRIB_HANDLE_ = C_NULL_PTR

CONTAINS

  !> @brief Initialisation status of the object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INITIALIZED => CGRIB_METADATA_INITIALIZED

  !> @brief Retrieve the wrapped grib handle.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_HANDLE => CGRIB_METADATA_GET_HANDLE

  !> @brief Bind a grib handle to this object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: BIND_HANDLE => CGRIB_METADATA_BIND_HANDLE
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: UNBIND_HANDLE => CGRIB_METADATA_UNBIND_HANDLE
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: TABLES_VERSION_LATEST => CGRIB_METADATA_GET_TABLES_VERSION_LATEST

  !> @brief Initializes the object with default values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_DEFAULT => CGRIB_METADATA_INIT_DEFAULT

  !> @brief Initializes the object from anotehr metadata object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_FROM_METADATA => CGRIB_METADATA_INIT_FROM_METADATA

  !> @brief Initializes the object from a file.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_FROM_SAMPLE => CGRIB_METADATA_INIT_FROM_SAMPLE

  !> @brief Initializes the object from the sample name.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_FROM_SAMPLE_NAME => CGRIB_METADATA_INIT_FROM_SAMPLE_NAME

  !> @brief Destroys the object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: DESTROY => CGRIB_METADATA_DESTROY

  !> @brief Sets a string value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_MISSING => CGRIB_METADATA_SET_MISSING

  !> @brief Sets a string value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_STRING => CGRIB_METADATA_SET_STRING

  !> @brief Sets a boolean value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_BOOL   => CGRIB_METADATA_SET_BOOL

  !> @brief Sets an 8-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT8  => CGRIB_METADATA_SET_INT8

  !> @brief Sets a 16-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT16 => CGRIB_METADATA_SET_INT16

  !> @brief Sets a 32-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT32 => CGRIB_METADATA_SET_INT32

  !> @brief Sets a 64-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT64 => CGRIB_METADATA_SET_INT64

  !> @brief Sets a 32-bit real value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL32  => CGRIB_METADATA_SET_REAL32

  !> @brief Sets a 64-bit real value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL64  => CGRIB_METADATA_SET_REAL64

  !> @brief Sets an array of string values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_STRING_ARRAY => CGRIB_METADATA_SET_STRING_ARRAY

  !> @brief Sets an array of boolean values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_BOOL_ARRAY   => CGRIB_METADATA_SET_BOOL_ARRAY

  !> @brief Sets an array of 8-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT8_ARRAY  => CGRIB_METADATA_SET_INT8_ARRAY

  !> @brief Sets an array of 16-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT16_ARRAY => CGRIB_METADATA_SET_INT16_ARRAY

  !> @brief Sets an array of 32-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT32_ARRAY => CGRIB_METADATA_SET_INT32_ARRAY

  !> @brief Sets an array of 64-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT64_ARRAY => CGRIB_METADATA_SET_INT64_ARRAY

  !> @brief Sets an array of 32-bit real values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL32_ARRAY  => CGRIB_METADATA_SET_REAL32_ARRAY

  !> @brief Sets an array of 64-bit real values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL64_ARRAY  => CGRIB_METADATA_SET_REAL64_ARRAY

  !> @brief Dump the sample to disk (for debugging and checking purposes)
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: DUMP_SAMPLE  => CGRIB_METADATA_DUMP_SAMPLE

  !> @brief Get the size in bytes of the sample
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SAMPLE_SIZE  => CGRIB_METADATA_SAMPLE_SIZE

  !> @brief Safe load a grib sample
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SAFE_LOAD  => CGRIB_METADATA_SAFE_LOAD


END TYPE


! Whitelist of public symbols
PUBLIC :: CGRIB_METADATA_T

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
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_INITIALIZED'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_INITIALIZED( THIS, EX, HOOKS ) RESULT(RET)

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
  CLASS(CGRIB_METADATA_T), INTENT(IN)    :: THIS
  LOGICAL,                INTENT(OUT)   :: EX
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

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

END FUNCTION CGRIB_METADATA_INITIALIZED
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
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_GET_HANDLE'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_GET_HANDLE( THIS, HANDLE, HOOKS, TAKE ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  TYPE(C_PTR),             INTENT(OUT)   :: HANDLE
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS
  LOGICAL, OPTIONAL,       INTENT(IN)    :: TAKE

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: LOC_TAKE

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

  ! Initialize the handle to null pointer
  HANDLE = C_NULL_PTR

  ! Handle optional arguments
  IF ( PRESENT(TAKE) ) THEN
    LOC_TAKE = TAKE
  ELSE
    LOC_TAKE = .FALSE.
  ENDIF

  ! This procedure can be called only if the object is not initialized
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_NOT_INITIALIZED )

  ! Read the sample and set the initialization flag to .true.
  HANDLE = THIS%CGRIB_HANDLE_

  ! If TAKE is .TRUE., set the handle to -1
  IF ( LOC_TAKE ) THEN
    THIS%CGRIB_HANDLE_ = C_NULL_PTR
    THIS%INITIALIZED_ = .FALSE.
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
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  RETURN

END FUNCTION CGRIB_METADATA_GET_HANDLE
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
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_BIND_HANDLE'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_BIND_HANDLE( THIS, HANDLE, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  TYPE(C_PTR),             INTENT(IN)    :: HANDLE
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flag
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_INITIALIZED=1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( THIS%INITIALIZED_, ERRFLAG_ALREADY_INITIALIZED )

  ! Read the sample and set the initialization flag to .true.
  THIS%CGRIB_HANDLE_ = HANDLE
  THIS%INITIALIZED_ = .TRUE.

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ALREADY_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle is already initialized' )
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

END FUNCTION CGRIB_METADATA_BIND_HANDLE
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
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_UNBIND_HANDLE'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_UNBIND_HANDLE( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flag
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_UNINITIALIZED=1_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_ALREADY_UNINITIALIZED )

  ! Read the sample and set the initialization flag to .true.
  THIS%CGRIB_HANDLE_ = C_NULL_PTR
  THIS%INITIALIZED_ = .FALSE.

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_ALREADY_UNINITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle is already uninitialized' )
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

END FUNCTION CGRIB_METADATA_UNBIND_HANDLE
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_INIT_FROM_METADATA'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_INIT_FROM_METADATA( THIS, METADATA, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T),         INTENT(INOUT) :: THIS
  CLASS(METADATA_BASE_A), POINTER, INTENT(IN)    :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_INITIALIZED
  INTEGER(KIND=JPIM_K) :: KRET
  TYPE(C_PTR) :: CGRIB_HANDLE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_IMPLEMENTED=2_JPIB_K

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

  ! Check dummy arguments
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! On purpose not implemented this module should be used only for c++ interfacing,
  ! hence the metadata is not initialized from a sample name
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_IMPLEMENTED )

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
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle already initialized' )
    CASE (ERRFLAG_NOT_IMPLEMENTED)
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

END FUNCTION CGRIB_METADATA_INIT_FROM_METADATA
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_INIT_FROM_SAMPLE_NAME'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_INIT_FROM_SAMPLE_NAME( THIS, SAMPLE_NAME, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_ASSOCIATED

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),        INTENT(IN)    :: SAMPLE_NAME
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_IMPLEMENTED=2_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! On purpose not implemented this module should be used only for c++ interfacing,
  ! hence the metadata is not initialized from a sample name
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_IMPLEMENTED )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle already initialized' )
    CASE (ERRFLAG_NOT_IMPLEMENTED)
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

END FUNCTION CGRIB_METADATA_INIT_FROM_SAMPLE_NAME
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_INIT_FROM_SAMPLE'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_INIT_FROM_SAMPLE( THIS, SAMPLE_NAME, SAMPLE_HANDLE, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
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
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),        INTENT(IN)    :: SAMPLE_NAME
  INTEGER(KIND=JPIM_K),    INTENT(IN)    :: SAMPLE_HANDLE
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET


  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_IMPLEMENTED=2_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! Logging the sample name
  PP_LOG_DEVELOP_STR( ' + Init from sample' )

  ! Read the sample ad if it's necessary distribute it
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_IMPLEMENTED )
  THIS%INITIALIZED_ = .TRUE.

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle already initialized' )
    CASE (ERRFLAG_NOT_IMPLEMENTED)
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

END FUNCTION CGRIB_METADATA_INIT_FROM_SAMPLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Initializes the object with default values.
!>
!> This procedure initializes the object with default values.
!>
!> @param [inout] this         The object to be initialized.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_INIT_DEFAULT'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_INIT_DEFAULT( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1

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
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! Default initialization has an invalid grib_handle and initialization flag set to .false.
  THIS%CGRIB_HANDLE_ = C_NULL_PTR
  THIS%INITIALIZED_ = .FALSE.

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle already initialized' )
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

END FUNCTION CGRIB_METADATA_INIT_DEFAULT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Destroys the object.
!>
!> This procedure deallocates resources associated with the object.
!>
!> @param [inout] this The object to be destroyed.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_DESTROY'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_DESTROY( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_PTR

  ! Symbols imported from other modules within the project.
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
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT) :: KRET

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_HANDLE_DELETE( HANDLE ) &
&    RESULT(RET) BIND(C, NAME='grib_handle_delete')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: HANDLE
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_HANDLE_DELETE
  END INTERFACE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DELETE_HANDLE=2_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! Set the value into the handle
  KRET = C_GRIB_HANDLE_DELETE( THIS%CGRIB_HANDLE_ )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_DELETE_HANDLE )

  ! Reset object internal status
  THIS%CGRIB_HANDLE_ = C_NULL_PTR
  THIS%INITIALIZED_ = .FALSE.

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
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_DELETE_HANDLE)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to release grib handle.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: "'//TRIM(ADJUSTL(ERRMSG))//'"' )
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

END FUNCTION CGRIB_METADATA_DESTROY
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_MISSING'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_MISSING( THIS, KEY, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIM_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT) :: KRET
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_SIZE_T) :: C_LENGTH
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(KEY)+1), TARGET :: C_KEY
  INTEGER(KIND=C_LONG) :: C_VALUE

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_SET_MISSING( HANDLE, KEY) &
&    RESULT(RET) BIND(C, NAME='grib_set_missing')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: HANDLE
      TYPE(C_PTR), VALUE, INTENT(IN) :: KEY
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_SET_MISSING
  END INTERFACE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_CSTRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_MISSING_FAILED=3_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! Copy filename to character array
  DO I = 1, LEN_TRIM(KEY)
    C_KEY(I) = KEY(I:I)
  ENDDO
  C_KEY(LEN_TRIM(KEY)+1) = C_NULL_CHAR

  ! Call the c-api function to set the real64 array
  KRET = C_GRIB_SET_MISSING( THIS%CGRIB_HANDLE_, C_LOC(C_KEY) )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_SET_MISSING_FAILED )

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
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_MISSING_FAILED)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set missing value.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'key: '//TRIM(ADJUSTL(KEY)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: "'//TRIM(ADJUSTL(ERRMSG))//'"' )
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

END FUNCTION CGRIB_METADATA_SET_MISSING
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_STRING'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_STRING( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T

  ! Symbols imported from other modules within the project.
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
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),        INTENT(IN)    :: KEY
  CHARACTER(LEN=*),        INTENT(IN)    :: VAL
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT) :: KRET
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_SIZE_T), TARGET :: C_LENGTH
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(KEY)+1), TARGET :: C_KEY
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(VAL)+1), TARGET :: C_VALUE

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_SET_STRING( HANDLE, KEY, VALUES, LENGTH ) &
&    RESULT(RET) BIND(C, NAME='grib_set_string')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: HANDLE
      TYPE(C_PTR), VALUE, INTENT(IN) :: KEY
      TYPE(C_PTR), VALUE, INTENT(IN) :: VALUES
      TYPE(C_PTR), VALUE, INTENT(IN) :: LENGTH
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_SET_STRING
  END INTERFACE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_CSTRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=3_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! Copy filename to character array
  DO I = 1, LEN_TRIM(KEY)
    C_KEY(I) = KEY(I:I)
  ENDDO
  C_KEY(LEN_TRIM(KEY)+1) = C_NULL_CHAR

  ! Prepare input for c-api
  C_LENGTH = INT(LEN_TRIM(VAL), KIND=C_SIZE_T)
  DO I = 1, C_LENGTH
    C_VALUE(I) = VAL(I:I)
  ENDDO
  C_VALUE(C_LENGTH+1) = C_NULL_CHAR

  ! Call the c-api function to set the real64 array
  KRET = C_GRIB_SET_STRING( THIS%CGRIB_HANDLE_, C_LOC(C_KEY), C_LOC(C_VALUE), C_LOC(C_LENGTH) )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_SET_FAILED )

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
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set string value.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key: "'//TRIM(ADJUSTL(KEY))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value: '//TRIM(ADJUSTL(VAL)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: "'//TRIM(ADJUSTL(ERRMSG))//'"' )
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

END FUNCTION CGRIB_METADATA_SET_STRING
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_BOOL'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_BOOL( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG

  ! Symbols imported from other modules within the project.
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
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  LOGICAL,                INTENT(IN)    :: VAL
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT) :: KRET
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_SIZE_T) :: C_LENGTH
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(KEY)+1), TARGET :: C_KEY
  INTEGER(C_LONG) :: C_VALUE

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_SET_LONG( HANDLE, KEY, VALUES ) &
&    RESULT(RET) BIND(C, NAME='grib_set_long')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: HANDLE
      TYPE(C_PTR), VALUE, INTENT(IN) :: KEY
      INTEGER(C_LONG), VALUE, INTENT(IN) :: VALUES
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_SET_LONG
  END INTERFACE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_CSTRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=3_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! Convert bool to int64
  IF ( VAL ) THEN
    C_VALUE = 1_C_LONG
  ELSE
    C_VALUE = 0_C_LONG
  ENDIF

  ! Set the value into the grib handle
  DO I = 1, LEN_TRIM(KEY)
    C_KEY(I) = KEY(I:I)
  ENDDO
  C_KEY(LEN_TRIM(KEY)+1) = C_NULL_CHAR

  ! Call the c-api function to set the real64 array
  KRET = C_GRIB_SET_LONG( THIS%CGRIB_HANDLE_, C_LOC(C_KEY), C_VALUE )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_SET_FAILED )

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
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG
    CHARACTER(LEN=32)   :: CTMP
    INTEGER(KIND=JPIB_K) :: WRITE_STAT

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      CTMP = REPEAT(' ', 32)
      WRITE(CTMP, '(L4)', IOSTAT=WRITE_STAT) VAL
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set bool value.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key: "'//TRIM(ADJUSTL(KEY))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value: '//TRIM(ADJUSTL(CTMP)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: "'//TRIM(ADJUSTL(ERRMSG))//'"' )
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

END FUNCTION CGRIB_METADATA_SET_BOOL
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_INT8'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_INT8( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

  ! Symbols imported from other modules within the project.
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT8),     INTENT(IN)    :: VAL
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=2_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_GRIB_SET_FAILED )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
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

END FUNCTION CGRIB_METADATA_SET_INT8
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_INT16'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_INT16( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

  ! Symbols imported from other modules within the project.
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT16),    INTENT(IN)    :: VAL
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=2_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_GRIB_SET_FAILED )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
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

END FUNCTION CGRIB_METADATA_SET_INT16
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_INT32'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_INT32( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG

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
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT32),    INTENT(IN)    :: VAL
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT) :: KRET
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_SIZE_T) :: C_LENGTH
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(KEY)+1), TARGET :: C_KEY
  INTEGER(C_LONG) :: C_VALUE

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_SET_LONG( HANDLE, KEY, VALUES ) &
&    RESULT(RET) BIND(C, NAME='grib_set_long')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: HANDLE
      TYPE(C_PTR), VALUE, INTENT(IN) :: KEY
      INTEGER(C_LONG), VALUE, INTENT(IN) :: VALUES
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_SET_LONG
  END INTERFACE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_CSTRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=3_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! Copy filename to character array
  DO I = 1, LEN_TRIM(KEY)
    C_KEY(I) = KEY(I:I)
  ENDDO
  C_KEY(LEN_TRIM(KEY)+1) = C_NULL_CHAR

  ! Prepare input for c-api
  C_VALUE = INT(VAL, KIND=C_LONG)

  ! Call the c-api function to set the real64 array
  KRET = C_GRIB_SET_LONG( THIS%CGRIB_HANDLE_, C_LOC(C_KEY), C_VALUE )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_SET_FAILED )

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
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG
    CHARACTER(LEN=32)   :: CTMP
    INTEGER(KIND=JPIB_K) :: WRITE_STAT

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      CTMP = REPEAT(' ', 32)
      WRITE(CTMP, '(I32)', IOSTAT=WRITE_STAT) VAL
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set int32 value.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key: "'//TRIM(ADJUSTL(KEY))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value: '//TRIM(ADJUSTL(CTMP)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: "'//TRIM(ADJUSTL(ERRMSG))//'"' )
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

END FUNCTION CGRIB_METADATA_SET_INT32
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_INT64'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_INT64( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG

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
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),        INTENT(IN)    :: KEY
  INTEGER(KIND=INT64),     INTENT(IN)    :: VAL
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT) :: KRET
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_SIZE_T) :: C_LENGTH
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(KEY)+1), TARGET :: C_KEY
  INTEGER(KIND=C_LONG) :: C_VALUE

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_SET_LONG( HANDLE, KEY, VALUES ) &
&    RESULT(RET) BIND(C, NAME='grib_set_long')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: HANDLE
      TYPE(C_PTR), VALUE, INTENT(IN) :: KEY
      INTEGER(C_LONG), VALUE, INTENT(IN) :: VALUES
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_SET_LONG
  END INTERFACE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_CSTRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=3_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! Copy filename to character array
  DO I = 1, LEN_TRIM(KEY)
    C_KEY(I) = KEY(I:I)
  ENDDO
  C_KEY(LEN_TRIM(KEY)+1) = C_NULL_CHAR

  ! Prepare input for c-api
  C_VALUE = INT(VAL, KIND=C_LONG)

  ! Call the c-api function to set the real64 array
  KRET = C_GRIB_SET_LONG( THIS%CGRIB_HANDLE_, C_LOC(C_KEY), C_VALUE )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_SET_FAILED )

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
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG
    CHARACTER(LEN=32)   :: CTMP
    INTEGER(KIND=JPIB_K) :: WRITE_STAT

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      CTMP = REPEAT(' ', 32)
      WRITE(CTMP, '(I32)', IOSTAT=WRITE_STAT) VAL
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set int64 value.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key: "'//TRIM(ADJUSTL(KEY))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value: '//TRIM(ADJUSTL(CTMP)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: "'//TRIM(ADJUSTL(ERRMSG))//'"' )
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

END FUNCTION CGRIB_METADATA_SET_INT64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_GET_TABLES_VERSION_LATEST'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_GET_TABLES_VERSION_LATEST( THIS, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG

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
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=INT64),     INTENT(OUT)   :: VAL
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=*), PARAMETER :: KEY = 'tablesVersionLatest'
  INTEGER(KIND=C_INT) :: KRET
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_SIZE_T) :: C_LENGTH
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(KEY)+1), TARGET :: C_KEY
  INTEGER(KIND=C_LONG), TARGET :: C_VALUE

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_GET_LONG( HANDLE, KEY, VALUES ) &
&    RESULT(RET) BIND(C, NAME='grib_get_long')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: HANDLE
      TYPE(C_PTR), VALUE, INTENT(IN) :: KEY
      TYPE(C_PTR), VALUE, INTENT(IN) :: VALUES
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_GET_LONG
  END INTERFACE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_CSTRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=3_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! Copy filename to character array
  DO I = 1, LEN_TRIM(KEY)
    C_KEY(I) = KEY(I:I)
  ENDDO
  C_KEY(LEN_TRIM(KEY)+1) = C_NULL_CHAR

  ! Prepare input for c-api
  C_VALUE = INT(0, KIND=C_LONG)

  ! Call the c-api function to set the real64 array
  KRET = C_GRIB_GET_LONG( THIS%CGRIB_HANDLE_, C_LOC(C_KEY), C_LOC(C_VALUE) )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_SET_FAILED )

  ! Set the return value
  VAL = INT(C_VALUE, KIND=INT64)

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
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG
    CHARACTER(LEN=32)   :: CTMP
    INTEGER(KIND=JPIB_K) :: WRITE_STAT

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      CTMP = REPEAT(' ', 32)
      WRITE(CTMP, '(I32)', IOSTAT=WRITE_STAT) VAL
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get int64 value.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key: "'//TRIM(ADJUSTL(KEY))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value: '//TRIM(ADJUSTL(CTMP)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: "'//TRIM(ADJUSTL(ERRMSG))//'"' )
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

END FUNCTION CGRIB_METADATA_GET_TABLES_VERSION_LATEST
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_REAL32'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_REAL32( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE

  ! Symbols imported from other modules within the project.
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
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),        INTENT(IN)    :: KEY
  REAL(KIND=REAL32),       INTENT(IN)    :: VAL
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT) :: KRET
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_SIZE_T) :: C_LENGTH
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(KEY)+1), TARGET :: C_KEY
  REAL(KIND=C_DOUBLE) :: C_VALUE

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_SET_DOUBLE( HANDLE, KEY, VALUES ) &
&    RESULT(RET) BIND(C, NAME='grib_set_double')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE
    IMPLICIT NONE
      TYPE(C_PTR),   VALUE, INTENT(IN) :: HANDLE
      TYPE(C_PTR),   VALUE, INTENT(IN) :: KEY
      REAL(C_DOUBLE), VALUE, INTENT(IN) :: VALUES
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_SET_DOUBLE
  END INTERFACE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_CSTRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=3_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )


  ! Copy filename to character array
  DO I = 1, LEN_TRIM(KEY)
    C_KEY(I) = KEY(I:I)
  ENDDO
  C_KEY(LEN_TRIM(KEY)+1) = C_NULL_CHAR

  ! Prepare input for c-api
  C_VALUE = REAL(VAL, KIND=C_DOUBLE)

  ! Call the c-api function to set the real64 array
  KRET = C_GRIB_SET_DOUBLE( THIS%CGRIB_HANDLE_, C_LOC(C_KEY), C_VALUE )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_SET_FAILED )

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
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG
    CHARACTER(LEN=32)   :: CTMP
    INTEGER(KIND=JPIB_K) :: WRITE_STAT

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      CTMP = REPEAT(' ', 32)
      WRITE(CTMP, '(F11.4)', IOSTAT=WRITE_STAT) VAL
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set real32 value.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'key: "'//TRIM(ADJUSTL(KEY))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value: '//TRIM(ADJUSTL(CTMP)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: "'//TRIM(ADJUSTL(ERRMSG))//'"' )
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

END FUNCTION CGRIB_METADATA_SET_REAL32
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_REAL64'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_REAL64( THIS, KEY, VAL, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE

  ! Symbols imported from other modules within the project.
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
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),        INTENT(IN)    :: KEY
  REAL(KIND=REAL64),       INTENT(IN)    :: VAL
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT) :: KRET
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_SIZE_T) :: C_LENGTH
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(KEY)+1), TARGET :: C_KEY
  REAL(KIND=C_DOUBLE) :: C_VALUE

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_SET_REAL8( HANDLE, KEY, VALUES ) &
&    RESULT(RET) BIND(C, NAME='grib_set_double')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: HANDLE
      TYPE(C_PTR), VALUE, INTENT(IN) :: KEY
      REAL(C_DOUBLE), VALUE, INTENT(IN) :: VALUES
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_SET_REAL8
  END INTERFACE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_CSTRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=3_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )


  ! Copy filename to character array
  DO I = 1, LEN_TRIM(KEY)
    C_KEY(I) = KEY(I:I)
  ENDDO
  C_KEY(LEN_TRIM(KEY)+1) = C_NULL_CHAR

  ! Prepare input for c-api
  C_VALUE = REAL(VAL, KIND=C_DOUBLE)

  ! Call the c-api function to set the real64 array
  KRET = C_GRIB_SET_REAL8( THIS%CGRIB_HANDLE_, C_LOC(C_KEY), C_VALUE )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_SET_FAILED )

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

    ! Error message variables
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG
    CHARACTER(LEN=32)   :: CTMP
    INTEGER(KIND=JPIB_K) :: WRITE_STAT

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      CTMP = REPEAT(' ', 32)
      WRITE(CTMP, '(F11.4)', IOSTAT=WRITE_STAT) VAL
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set real64 value.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'key: "'//TRIM(ADJUSTL(KEY))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value: '//TRIM(ADJUSTL(CTMP)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: "'//TRIM(ADJUSTL(ERRMSG))//'"' )
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

END FUNCTION CGRIB_METADATA_SET_REAL64
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_STRING_ARRAY'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_STRING_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T),         INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),               INTENT(IN)    :: KEY
  CHARACTER(LEN=*), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=2_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_GRIB_SET_FAILED )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
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

END FUNCTION CGRIB_METADATA_SET_STRING_ARRAY
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_BOOL_ARRAY'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_BOOL_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T),  INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),        INTENT(IN)    :: KEY
  LOGICAL, DIMENSION(:),   INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=2_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_GRIB_SET_FAILED )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
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

END FUNCTION CGRIB_METADATA_SET_BOOL_ARRAY
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_INT8_ARRAY'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_INT8_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

  ! Symbols imported from other modules within the project.
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T),           INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                 INTENT(IN)    :: KEY
  INTEGER(KIND=INT8), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=2_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_GRIB_SET_FAILED )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
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

END FUNCTION CGRIB_METADATA_SET_INT8_ARRAY
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_INT16_ARRAY'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_INT16_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

  ! Symbols imported from other modules within the project.
  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT16), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=2_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( ERRFLAG_GRIB_SET_FAILED )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
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

END FUNCTION CGRIB_METADATA_SET_INT16_ARRAY
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_INT32_ARRAY'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_INT32_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T

  ! Symbols imported from other modules within the project.
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
  CLASS(CGRIB_METADATA_T),           INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT32), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_SIZE_T) :: C_LENGTH
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(KEY)+1), TARGET :: C_KEY
  INTEGER(KIND=C_INT), DIMENSION(SIZE(VALUES,1)), TARGET :: C_VALUES

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_SET_INT_ARRAY( HANDLE, KEY, VALUES, LENGTH ) &
&    RESULT(RET) BIND(C, NAME='grib_set_long_array')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: HANDLE
      TYPE(C_PTR), VALUE, INTENT(IN) :: KEY
      TYPE(C_PTR), VALUE, INTENT(IN) :: VALUES
      INTEGER(KIND=C_SIZE_T), VALUE, INTENT(IN) :: LENGTH
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_SET_INT_ARRAY
  END INTERFACE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_CSTRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=3_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! Copy filename to character array
  DO I = 1, LEN_TRIM(KEY)
    C_KEY(I) = KEY(I:I)
  ENDDO
  C_KEY(LEN_TRIM(KEY)+1) = C_NULL_CHAR

  ! Prepare input for c-api
  C_LENGTH = SIZE(VALUES,1)

  DO I = 1, C_LENGTH
    C_VALUES(I) = INT(VALUES(I), KIND=C_INT)
  END DO

  ! Call the c-api function to set the real64 array
  KRET = C_GRIB_SET_INT_ARRAY( THIS%CGRIB_HANDLE_, C_LOC(C_KEY), C_LOC(C_VALUES), C_LENGTH )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_SET_FAILED )

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

    ! Error message variables
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set int32 array value.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'key: "'//TRIM(ADJUSTL(KEY))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: "'//TRIM(ADJUSTL(ERRMSG))//'"' )
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

END FUNCTION CGRIB_METADATA_SET_INT32_ARRAY
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_INT64_ARRAY'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_INT64_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG

  ! Symbols imported from other modules within the project.
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
  CLASS(CGRIB_METADATA_T),           INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT64), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_SIZE_T) :: C_LENGTH
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(KEY)+1), TARGET :: C_KEY
  INTEGER(KIND=C_LONG), DIMENSION(SIZE(VALUES,1)), TARGET :: C_VALUES

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_SET_LONG_ARRAY( HANDLE, KEY, VALUES, LENGTH ) &
&    RESULT(RET) BIND(C, NAME='grib_set_long_array')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: HANDLE
      TYPE(C_PTR), VALUE, INTENT(IN) :: KEY
      TYPE(C_PTR), VALUE, INTENT(IN) :: VALUES
      INTEGER(KIND=C_SIZE_T), VALUE, INTENT(IN) :: LENGTH
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_SET_LONG_ARRAY
  END INTERFACE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_CSTRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=3_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! Copy filename to character array
  DO I = 1, LEN_TRIM(KEY)
    C_KEY(I) = KEY(I:I)
  ENDDO
  C_KEY(LEN_TRIM(KEY)+1) = C_NULL_CHAR

  ! Prepare input for c-api
  C_LENGTH = SIZE(VALUES,1)

  DO I = 1, C_LENGTH
    C_VALUES(I) = INT(VALUES(I), KIND=C_LONG)
  END DO

  ! Call the c-api function to set the real64 array
  KRET = C_GRIB_SET_LONG_ARRAY( THIS%CGRIB_HANDLE_, C_LOC(C_KEY), C_LOC(C_VALUES), C_LENGTH )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_SET_FAILED )

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

    ! Error message variables
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set int64 array value.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'key: "'//TRIM(ADJUSTL(KEY))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: "'//TRIM(ADJUSTL(ERRMSG))//'"' )
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

END FUNCTION CGRIB_METADATA_SET_INT64_ARRAY
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_REAL32_ARRAY'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_REAL32_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_FLOAT

  ! Symbols imported from other modules within the project.
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
  CLASS(CGRIB_METADATA_T),         INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL32), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_SIZE_T) :: C_LENGTH
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(KEY)+1), TARGET :: C_KEY
  REAL(KIND=C_FLOAT), DIMENSION(SIZE(VALUES,1)), TARGET :: C_VALUES

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_SET_REAL4_ARRAY( HANDLE, KEY, VALUES, LENGTH ) &
&    RESULT(RET) BIND(C, NAME='grib_set_float_array')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: HANDLE
      TYPE(C_PTR), VALUE, INTENT(IN) :: KEY
      TYPE(C_PTR), VALUE, INTENT(IN) :: VALUES
      INTEGER(KIND=C_SIZE_T), VALUE, INTENT(IN) :: LENGTH
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_SET_REAL4_ARRAY
  END INTERFACE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_CSTRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=3_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! Copy filename to character array
  DO I = 1, LEN_TRIM(KEY)
    C_KEY(I) = KEY(I:I)
  ENDDO
  C_KEY(LEN_TRIM(KEY)+1) = C_NULL_CHAR

  ! Prepare input for c-api
  C_LENGTH = SIZE(VALUES,1)

  DO I = 1, C_LENGTH
    C_VALUES(I) = REAL(VALUES(I), KIND=C_FLOAT)
  END DO

  ! Call the c-api function to set the real64 array
  KRET = C_GRIB_SET_REAL4_ARRAY( THIS%CGRIB_HANDLE_, C_LOC(C_KEY), C_LOC(C_VALUES), C_LENGTH )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_SET_FAILED )

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

    ! Error message variables
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set real32 array value.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'key: "'//TRIM(ADJUSTL(KEY))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
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

END FUNCTION CGRIB_METADATA_SET_REAL32_ARRAY
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
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SET_REAL64_ARRAY'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SET_REAL64_ARRAY( THIS, KEY, VALUES, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE

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
  CLASS(CGRIB_METADATA_T),         INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL64), DIMENSION(:), INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT) :: KRET
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_SIZE_T) :: C_LENGTH
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(KEY)+1), TARGET :: C_KEY
  REAL(KIND=C_DOUBLE), DIMENSION(SIZE(VALUES,1)), TARGET :: C_VALUES

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_SET_REAL8_ARRAY( HANDLE, KEY, VALUES, LENGTH ) &
&    RESULT(RET) BIND(C, NAME='grib_set_double_array')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: HANDLE
      TYPE(C_PTR), VALUE, INTENT(IN) :: KEY
      TYPE(C_PTR), VALUE, INTENT(IN) :: VALUES
      INTEGER(KIND=C_SIZE_T), VALUE, INTENT(IN) :: LENGTH
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_SET_REAL8_ARRAY
  END INTERFACE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_THIS_NOT_INITIALIZED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_TO_CSTRING=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GRIB_SET_FAILED=3_JPIB_K

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
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, ERRFLAG_THIS_NOT_INITIALIZED )

  ! Copy filename to character array
  DO I = 1, LEN_TRIM(KEY)
    C_KEY(I) = KEY(I:I)
  ENDDO
  C_KEY(LEN_TRIM(KEY)+1) = C_NULL_CHAR

  ! Prepare input for c-api
  C_LENGTH = SIZE(VALUES,1)

  DO I = 1, C_LENGTH
    C_VALUES(I) = REAL(VALUES(I), KIND=C_DOUBLE)
  END DO

  ! Call the c-api function to set the real64 array
  KRET = C_GRIB_SET_REAL8_ARRAY( THIS%CGRIB_HANDLE_, C_LOC(C_KEY), C_LOC(C_VALUES), C_LENGTH )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, ERRFLAG_GRIB_SET_FAILED )

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

    ! Error message variables
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_THIS_NOT_INITIALIZED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Handle not initialized' )
    CASE (ERRFLAG_GRIB_SET_FAILED)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to set real64 array value.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'key: "'//TRIM(ADJUSTL(KEY))//'"' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: "'//TRIM(ADJUSTL(ERRMSG))//'"' )
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

END FUNCTION CGRIB_METADATA_SET_REAL64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Write the sample to disk (for debugging purposes).
!>
!> This procedure allows the user to write the metadata to disk.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    name   The name to be given to the file.
!> @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_DUMP_SAMPLE'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_DUMP_SAMPLE( THIS, NAME, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_SIZE_T

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIM_K
  USE :: HOOKS_MOD,             ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),        INTENT(IN)    :: NAME
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT) :: KRET
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(LEN(NAME)+1), TARGET :: C_NAME
  CHARACTER(LEN=1,KIND=C_CHAR), DIMENSION(2), TARGET :: C_MODE
  INTEGER(KIND=JPIB_K) :: I

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_WRITE_MESSAGE( HANDLE, NAME, MODE ) &
&    RESULT(RET) BIND(C, NAME='grib_write_message')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: HANDLE
      TYPE(C_PTR), VALUE, INTENT(IN) :: NAME
      TYPE(C_PTR), VALUE, INTENT(IN) :: MODE
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_WRITE_MESSAGE
  END INTERFACE

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TO_STRING=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_MESSAGE=1_JPIB_K

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

  ! Copy filename to character array
  DO I = 1, LEN_TRIM(NAME)
    C_NAME(I) = NAME(I:I)
  ENDDO
  C_NAME(LEN_TRIM(NAME)+1) = C_NULL_CHAR

  C_MODE(1) = 'a'
  C_MODE(2) = C_NULL_CHAR

  ! Write message
  KRET = C_GRIB_WRITE_MESSAGE( THIS%CGRIB_HANDLE_, C_LOC(C_NAME), C_LOC(C_MODE) )
  PP_DEBUG_CRITICAL_COND_THROW(KRET.NE.GRIB_SUCCESS, ERRFLAG_WRITE_MESSAGE)

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

    ! Error message variables
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_WRITE_MESSAGE)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write the file.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
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

END FUNCTION CGRIB_METADATA_DUMP_SAMPLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Get the size in bytes of the sample.
!>
!> This procedure allows the user to inquire the size in bytes of the sample.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [out]   size   Size of the sample in bytes.
!> @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SAMPLE_SIZE'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SAMPLE_SIZE( THIS, SIZE, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_SIZE_T
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT

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
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),    INTENT(OUT)   :: SIZE
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_SIZE_T) :: DATA_LENGTH
  INTEGER(KIND=C_INT) :: KRET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SIZE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_LENGTH=2_JPIB_K

  ! Local interfaces
  INTERFACE
    FUNCTION C_GET_MESSAGE_SIZE( HANDLE, SIZE ) &
&    RESULT(RET) BIND(C, NAME='grib_get_message_size')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
    IMPLICIT NONE
      TYPE(C_PTR),            INTENT(IN) :: HANDLE
      INTEGER(KIND=C_SIZE_T), INTENT(OUT) :: SIZE
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GET_MESSAGE_SIZE
  END INTERFACE

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
  KRET = C_GET_MESSAGE_SIZE( THIS%CGRIB_HANDLE_, DATA_LENGTH )
  PP_DEBUG_CRITICAL_COND_THROW(KRET.NE.GRIB_SUCCESS, ERRFLAG_UNABLE_TO_GET_SIZE)
  PP_DEBUG_CRITICAL_COND_THROW(DATA_LENGTH.LE.0, ERRFLAG_INVALID_LENGTH)

  ! Cast the size
  SIZE = INT(DATA_LENGTH, KIND=JPIB_K)

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

    ! Error message variables
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_GET_SIZE)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get the size of the sample.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
    CASE (ERRFLAG_INVALID_LENGTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid length of the sample.' )
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

END FUNCTION CGRIB_METADATA_SAMPLE_SIZE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CGRIB_METADATA_SAFE_LOAD'
PP_THREAD_SAFE FUNCTION CGRIB_METADATA_SAFE_LOAD( THIS, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_ASSOCIATED

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
  CLASS(CGRIB_METADATA_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),          INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT) :: KRET
  TYPE(C_PTR) :: TMPHANDLE

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CLONE_SAMPLE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_RELEASE_SAMPLE=2_JPIB_K

  ! Local interfaces
  INTERFACE
    FUNCTION C_GRIB_CLONE( HANDLE ) &
&     RESULT(CLONE_HANDLE) BIND(C, NAME='grib_handle_clone')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
      TYPE(C_PTR), INTENT(IN) :: HANDLE
      TYPE(C_PTR) :: CLONE_HANDLE
    END FUNCTION C_GRIB_CLONE

    FUNCTION C_GRIB_RELEASE( HANDLE ) &
&    RESULT(RET) BIND(C, NAME='grib_handle_delete')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: HANDLE
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_GRIB_RELEASE
  END INTERFACE

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
  TMPHANDLE = THIS%CGRIB_HANDLE_
  THIS%CGRIB_HANDLE_ = C_NULL_PTR

  ! Clone the handle
  THIS%CGRIB_HANDLE_ = C_GRIB_CLONE( TMPHANDLE )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.C_ASSOCIATED(THIS%CGRIB_HANDLE_), ERRFLAG_UNABLE_TO_CLONE_SAMPLE )

  ! Release the old handle
  KRET = C_GRIB_RELEASE( TMPHANDLE )
  PP_DEBUG_CRITICAL_COND_THROW(KRET.NE.GRIB_SUCCESS, ERRFLAG_UNABLE_TO_RELEASE_SAMPLE)

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

    ! Error message variables
    CHARACTER(LEN=ERRMSG_LEN) :: ERRMSG

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CLONE_SAMPLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to clone the sample.' )
    CASE (ERRFLAG_UNABLE_TO_RELEASE_SAMPLE)
      CALL CGRIB_GET_ERROR_MESSAGE( KRET, ERRMSG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to release the sample.' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
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

END FUNCTION CGRIB_METADATA_SAFE_LOAD
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE






#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'CGRIB_GET_ERROR_MESSAGE'
PP_THREAD_SAFE SUBROUTINE CGRIB_GET_ERROR_MESSAGE( ERR_CODE, ERRMSG )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_NULL_CHAR
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_PTR
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_NULL_PTR
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_SIZE_T
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_F_POINTER
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_ASSOCIATED

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=C_INT),       INTENT(IN)    :: ERR_CODE
  CHARACTER(LEN=ERRMSG_LEN), INTENT(OUT)   :: ERRMSG

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=C_INT) :: KRET
  INTEGER(KIND=C_SIZE_T) :: C_LENGTH
  INTEGER(KIND=JPIB_K) :: LENGTH
  INTEGER(KIND=JPIB_K) :: I
  CHARACTER(LEN=32) :: CTMP
  CHARACTER(KIND=C_CHAR,LEN=1), DIMENSION(:), POINTER :: C_ERRMSG
  TYPE(C_PTR) :: C_ERRMSG_PTR

  ! Local interfaces
  INTERFACE
    FUNCTION C_GET_ERROR_CODE( ERR_CODE ) &
&    RESULT(RET) BIND(C, NAME='grib_get_error_message')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
      INTEGER(KIND=C_INT), VALUE, INTENT(IN) :: ERR_CODE
      TYPE(C_PTR) :: RET
    END FUNCTION C_GET_ERROR_CODE
    FUNCTION C_STRLEN( STR ) &
&    RESULT(LEN) BIND(C, NAME='strlen')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: STR
      INTEGER(KIND=C_SIZE_T) :: LEN
    END FUNCTION C_STRLEN
    FUNCTION C_FREE( PTR ) &
&    RESULT(RET) BIND(C, NAME='free')
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: PTR
      INTEGER(KIND=C_INT) :: RET
    END FUNCTION C_FREE
  END INTERFACE

  ! Initialize the error message
  ERRMSG = REPEAT(' ', ERRMSG_LEN)
  CTMP = REPEAT(' ', 32)
  WRITE( CTMP, '(A)' ) ERR_CODE

  ! Copy filename to character array
  C_ERRMSG_PTR = C_NULL_PTR
  C_ERRMSG_PTR = C_GET_ERROR_CODE( ERR_CODE )
  IF ( .NOT.C_ASSOCIATED(C_ERRMSG_PTR) ) THEN
    ERRMSG = 'Unknown error code: '//TRIM(ADJUSTL(CTMP))
  ELSE
    C_LENGTH = C_STRLEN( C_ERRMSG_PTR )
    LENGTH = MIN( ERRMSG_LEN, INT(C_LENGTH, KIND=JPIB_K))
    IF ( LENGTH .GT. 0 ) THEN
      C_ERRMSG => NULL()
      CALL C_F_POINTER( C_ERRMSG_PTR, C_ERRMSG, [C_LENGTH] )
      IF ( ASSOCIATED(C_ERRMSG) ) THEN
        IF ( SIZE(C_ERRMSG) .GE. LENGTH ) THEN
          DO I = 1, LENGTH
            ERRMSG(I:I) = C_ERRMSG(I)
          ENDDO
        ELSE
          ERRMSG = 'Unknown error code: '//TRIM(ADJUSTL(CTMP))
        ENDIF
      ELSE
        ERRMSG = 'Unknown error code: '//TRIM(ADJUSTL(CTMP))
      ENDIF
    ELSE
      ERRMSG = 'Unknown error code: '//TRIM(ADJUSTL(CTMP))
    ENDIF
    C_ERRMSG => NULL()
  ENDIF

  ! Exit point (On success)
  RETURN

END SUBROUTINE CGRIB_GET_ERROR_MESSAGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE CGRIB_METADATA_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
