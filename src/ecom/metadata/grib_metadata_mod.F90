!> @file
!>
!> @brief Definition of the `GRIB_METADATA_T` derived type.
!>
!> The `GRIB_METADATA_T` type is a derived type that extends the functionality of the
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
#define PP_FILE_NAME 'grib_metadata_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB_METADATA_MOD'
MODULE GRIB_METADATA_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,  ONLY: JPIM_K
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

IMPLICIT NONE

! Default visibility
PRIVATE

!> @brief Definition of the `GRIB_METADATA_T` derived type.
!>
!> The `GRIB_METADATA_T` type is a derived type that extends the functionality of the
!> `METADATA_BASE_A` abstract interface to specifically handle GRIB metadata.
!> It serves as a wrapper for setting various types of GRIB-related values
!> such as strings, integers, and arrays.
!>
!> @see METADATA_BASE_A
TYPE, EXTENDS(METADATA_BASE_A) :: GRIB_METADATA_T

  !> Visibility of the members
  PRIVATE

  !> @brief Track the initialization status of the object
  LOGICAL :: INITIALIZED_ = .FALSE.

  !> @brief Grib handle
  INTEGER(KIND=JPIM_K) :: IGRIB_HANDLE_ = -99_JPIM_K

CONTAINS

  !> @brief Initialisation status of the object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INITIALIZED => GRIB_METADATA_INITIALIZED

  !> @brief Retrieve the wrapped grib handle.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: GET_HANDLE => GRIB_METADATA_GET_HANDLE

  !> @brief Initializes the object with default values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_DEFAULT => GRIB_METADATA_INIT_DEFAULT

  !> @brief Initializes the object from anotehr metadata object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_FROM_METADATA => GRIB_METADATA_INIT_FROM_METADATA

  !> @brief Initializes the object from a file.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_FROM_SAMPLE => GRIB_METADATA_INIT_FROM_SAMPLE

  !> @brief Initializes the object from the sample name.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: INIT_FROM_SAMPLE_NAME => GRIB_METADATA_INIT_FROM_SAMPLE_NAME

  !> @brief Destroys the object.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: DESTROY => GRIB_METADATA_DESTROY

  !> @brief Sets a string value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_STRING => GRIB_METADATA_SET_STRING

  !> @brief Sets a boolean value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_BOOL   => GRIB_METADATA_SET_BOOL

  !> @brief Sets an 8-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT8  => GRIB_METADATA_SET_INT8

  !> @brief Sets a 16-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT16 => GRIB_METADATA_SET_INT16

  !> @brief Sets a 32-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT32 => GRIB_METADATA_SET_INT32

  !> @brief Sets a 64-bit integer value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT64 => GRIB_METADATA_SET_INT64

  !> @brief Sets a 32-bit real value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL32  => GRIB_METADATA_SET_REAL32

  !> @brief Sets a 64-bit real value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL64  => GRIB_METADATA_SET_REAL64

  !> @brief Sets a 128-bit real value.
  ! PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL128 => GRIB_METADATA_SET_REAL128

  !> @brief Sets an array of string values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_STRING_ARRAY => GRIB_METADATA_SET_STRING_ARRAY

  !> @brief Sets an array of boolean values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_BOOL_ARRAY   => GRIB_METADATA_SET_BOOL_ARRAY

  !> @brief Sets an array of 8-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT8_ARRAY  => GRIB_METADATA_SET_INT8_ARRAY

  !> @brief Sets an array of 16-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT16_ARRAY => GRIB_METADATA_SET_INT16_ARRAY

  !> @brief Sets an array of 32-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT32_ARRAY => GRIB_METADATA_SET_INT32_ARRAY

  !> @brief Sets an array of 64-bit integer values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_INT64_ARRAY => GRIB_METADATA_SET_INT64_ARRAY

  !> @brief Sets an array of 32-bit real values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL32_ARRAY  => GRIB_METADATA_SET_REAL32_ARRAY

  !> @brief Sets an array of 64-bit real values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL64_ARRAY  => GRIB_METADATA_SET_REAL64_ARRAY

  !> @brief Sets an array of 128-bit real values.
  ! PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL128_ARRAY => GRIB_METADATA_SET_REAL128_ARRAY

END TYPE


! Whitelist of public symbols
PUBLIC :: GRIB_METADATA_T

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
#define PP_PROCEDURE_NAME 'GRIB_METADATA_INITIALIZED'
FUNCTION GRIB_METADATA_INITIALIZED( THIS ) RESULT(EX)

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T), INTENT(IN) :: THIS

  ! Function result
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is not initialized
  EX = THIS%INITIALIZED_

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION GRIB_METADATA_INITIALIZED
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
#define PP_PROCEDURE_NAME 'GRIB_METADATA_GET_HANDLE'
FUNCTION GRIB_METADATA_GET_HANDLE( THIS ) RESULT(HANDLE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T), INTENT(IN) :: THIS

  ! Function result
  INTEGER(KIND=JPIM_K) :: HANDLE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is not initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Read the sample and set the initialization flag to .true.
  HANDLE = THIS%IGRIB_HANDLE_

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle must be initialized' )
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

END FUNCTION GRIB_METADATA_GET_HANDLE
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_INIT_FROM_METADATA'
SUBROUTINE GRIB_METADATA_INIT_FROM_METADATA( THIS, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: GRIB_API,    ONLY: GRIB_CLONE
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T),          INTENT(INOUT) :: THIS
  CLASS(METADATA_BASE_A), POINTER, INTENT(IN)    :: METADATA

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=JPIM_K) :: IGRIB_HANDLE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check dummy arguments
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, 1 )
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.METADATA%INITIALIZED(), 2 )

  ! This procedure has been defined only for some specific implementations
  SELECT TYPE( MD => METADATA )

  !---------------------------------------------------------------------
  ! Initialize from grib metadata
  CLASS IS( GRIB_METADATA_T )

    ! Extract the metadata from the grib handle
    IGRIB_HANDLE = MD%GET_HANDLE()

    ! Clone the grib handle
    CALL GRIB_CLONE( IGRIB_HANDLE, THIS%IGRIB_HANDLE_, STATUS=KRET )
    PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 3 )

    ! Set the initialization flag to .true.
    THIS%INITIALIZED_ = .TRUE.

  !---------------------------------------------------------------------
  ! Not implemented
  CLASS DEFAULT
    PP_DEBUG_CRITICAL_THROW( 4 )
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle already initialized' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Try to clone an object which is not initialized' )
    CASE (3)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to set real64 value.', KRET, GRIB_ERROR )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Initialization not implemented from the input type' )
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

END SUBROUTINE GRIB_METADATA_INIT_FROM_METADATA
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_INIT_FROM_SAMPLE_NAME'
SUBROUTINE GRIB_METADATA_INIT_FROM_SAMPLE_NAME( THIS, SAMPLE_NAME )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: GRIB_API,    ONLY: GRIB_NEW_FROM_SAMPLES
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: SAMPLE_NAME

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is not initialized
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, 1 )

  ! Read the sample ad if it's necessary distribute it
  CALL GRIB_NEW_FROM_SAMPLES( THIS%IGRIB_HANDLE_, TRIM(ADJUSTL(SAMPLE_NAME)), STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )
  THIS%INITIALIZED_ = .TRUE.

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle already initialized' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to load the sample.', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIB_METADATA_INIT_FROM_SAMPLE_NAME
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_INIT_FROM_SAMPLE'
SUBROUTINE GRIB_METADATA_INIT_FROM_SAMPLE( THIS, SAMPLE_NAME, SAMPLE_HANDLE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: GRIB_API,    ONLY: GRIB_CLONE
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: SAMPLE_NAME
  INTEGER(KIND=JPIM_K),   INTENT(IN)    :: SAMPLE_HANDLE

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is not initialized
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, 1 )

  ! Read the sample ad if it's necessary distribute it
  CALL GRIB_CLONE( SAMPLE_HANDLE, THIS%IGRIB_HANDLE_, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )
  THIS%INITIALIZED_ = .TRUE.

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle already initialized' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to clone the sample.', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIB_METADATA_INIT_FROM_SAMPLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Initializes the object with default values.
!>
!> This procedure initializes the object with default values.
!>
!> @param [inout] this         The object to be initialized.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_INIT_DEFAULT'
SUBROUTINE GRIB_METADATA_INIT_DEFAULT( THIS )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T), INTENT(INOUT) :: THIS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is not initialized
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, 1 )

  ! Default initialization has an invalid grib_handle and initialization flag set to .false.
  THIS%IGRIB_HANDLE_ = -99
  THIS%INITIALIZED_ = .FALSE.

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle already initialized' )
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

END SUBROUTINE GRIB_METADATA_INIT_DEFAULT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Destroys the object.
!>
!> This procedure deallocates resources associated with the object.
!>
!> @param [inout] this The object to be destroyed.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_DESTROY'
SUBROUTINE GRIB_METADATA_DESTROY( THIS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: GRIB_API,    ONLY: GRIB_RELEASE
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T), INTENT(INOUT) :: THIS

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set the value into the handle
  CALL GRIB_RELEASE( THIS%IGRIB_HANDLE_, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

  ! Reset object internal status
  THIS%IGRIB_HANDLE_ = -99
  THIS%INITIALIZED_ = .FALSE.

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to release grib handle.', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIB_METADATA_DESTROY
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_STRING'
SUBROUTINE GRIB_METADATA_SET_STRING( THIS, KEY, VAL )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: GRIB_API,    ONLY: GRIB_SET_STRING
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  CHARACTER(LEN=*),       INTENT(IN)    :: VAL

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set the value into the handle
  CALL GRIB_SET_STRING( THIS%IGRIB_HANDLE_, KEY, VAL, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to set string value.', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIB_METADATA_SET_STRING
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_BOOL'
SUBROUTINE GRIB_METADATA_SET_BOOL( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: GRIB_API,    ONLY: GRIB_SET_LONG
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  LOGICAL,                INTENT(IN)    :: VAL

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET
  INTEGER(KIND=INT64)  :: TMP

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Convert bool to int64
  IF ( VAL ) THEN
    TMP = 1_INT64
  ELSE
    TMP = 0_INT64
  ENDIF

  ! Set the value into the grib handle
  CALL GRIB_SET_LONG( THIS%IGRIB_HANDLE_, KEY, TMP, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to set bool value.', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIB_METADATA_SET_BOOL
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_INT8'
SUBROUTINE GRIB_METADATA_SET_INT8( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT8),     INTENT(IN)    :: VAL

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Impossible to write an array of  16-bit integer on a grib handle' )
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

END SUBROUTINE GRIB_METADATA_SET_INT8
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_INT16'
SUBROUTINE GRIB_METADATA_SET_INT16( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT16),    INTENT(IN)    :: VAL

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Impossible to write an array of  16-bit integer on a grib handle' )
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

END SUBROUTINE GRIB_METADATA_SET_INT16
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_INT32'
SUBROUTINE GRIB_METADATA_SET_INT32( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: GRIB_API,    ONLY: GRIB_SET_INT
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT32),    INTENT(IN)    :: VAL

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set the value into the grib handle
  CALL GRIB_SET_INT( THIS%IGRIB_HANDLE_, KEY, VAL, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to set int32 value.', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIB_METADATA_SET_INT32
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_INT64'
SUBROUTINE GRIB_METADATA_SET_INT64( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: GRIB_API,    ONLY: GRIB_SET_LONG
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  INTEGER(KIND=INT64),    INTENT(IN)    :: VAL

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set the value into the grib handle
  CALL GRIB_SET_LONG( THIS%IGRIB_HANDLE_, KEY, VAL, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to set int64 value.', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIB_METADATA_SET_INT64
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_REAL32'
SUBROUTINE GRIB_METADATA_SET_REAL32( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: GRIB_API,    ONLY: GRIB_SET_REAL4
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  REAL(KIND=REAL32),      INTENT(IN)    :: VAL

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set the value into the grib handle
  CALL GRIB_SET_REAL4( THIS%IGRIB_HANDLE_, KEY, VAL, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to set real64 value.', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIB_METADATA_SET_REAL32
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_REAL64'
SUBROUTINE GRIB_METADATA_SET_REAL64( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: GRIB_API,    ONLY: GRIB_SET_REAL8
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),       INTENT(IN)    :: KEY
  REAL(KIND=REAL64),      INTENT(IN)    :: VAL

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set the value into the grib handle
  CALL GRIB_SET_REAL8( THIS%IGRIB_HANDLE_, KEY, VAL, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to set real64 value.', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIB_METADATA_SET_REAL64
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets a 128-bit real value.
!>
!> This procedure sets a 128-bit real value associated with a specified key.
!>
!> @param [inout] this The object where the integer value is to be set.
!> @param [in]    key  The key used to store the integer value.
!> @param [in]    val  The 128-bit real value to be stored.
!>
! #define PP_PROCEDURE_TYPE 'SUBROUTINE'
! #define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_REAL128'
! SUBROUTINE GRIB_METADATA_SET_REAL128( THIS, KEY, VAL )
!
!   ! Symbolds imported from intrinsic modules
!   USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
!
!   ! Symbols imported by the preprocessor for debugging purposes
!   PP_DEBUG_USE_VARS
!
!   ! Symbols imported by the preprocessor for tracing purposes
!   PP_TRACE_USE_VARS
!
! IMPLICIT NONE
!
!   ! Dummy arguments
!   CLASS(GRIB_METADATA_T), INTENT(INOUT) :: THIS
!   CHARACTER(LEN=*),       INTENT(IN)    :: KEY
!   REAL(KIND=REAL128),     INTENT(IN)    :: VAL
!
!   ! Local variables declared by the preprocessor for debugging purposes
!   PP_DEBUG_DECL_VARS
!
!   ! Local variables declared by the preprocessor for tracing purposes
!   PP_TRACE_DECL_VARS
!
!   ! Trace begin of procedure
!   PP_TRACE_ENTER_PROCEDURE()
!
!   ! This procedure can be called only if the object is initialized
!   PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )
!
!   ! This datatype is not supported by grib API
!   PP_DEBUG_CRITICAL_THROW( 2 )
!
!   ! Trace end of procedure (on success)
!   PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()
!
!   ! Exit point on success
!   RETURN
!
! ! Error handler
! PP_ERROR_HANDLER
!
!   ErrorHandler: BLOCK
!
!     ! Error handling variables
!     CHARACTER(LEN=:), ALLOCATABLE :: STR
!
!     ! HAndle different errors
!     SELECT CASE(ERRIDX)
!     CASE (1)
!       PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
!     CASE (2)
!       PP_DEBUG_CREATE_ERROR_MSG( STR, 'Impossible to write an array of  16-bit integer on a grib handle' )
!     CASE DEFAULT
!       PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
!     END SELECT
!
!     ! Trace end of procedure (on error)
!     PP_TRACE_EXIT_PROCEDURE_ON_ERROR()
!
!     ! Write the error message and stop the program
!     PP_DEBUG_ABORT( STR )
!
!   END BLOCK ErrorHandler
!
!   ! Exit point on error
!   RETURN
!
! END SUBROUTINE GRIB_METADATA_SET_REAL128
! #undef PP_PROCEDURE_NAME
! #undef PP_PROCEDURE_TYPE


!> @brief Sets an array of strings values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of strings values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of strings values representing the metadata to be stored.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_STRING_ARRAY'
SUBROUTINE GRIB_METADATA_SET_STRING_ARRAY( THIS, KEY, VALUES )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T),         INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),               INTENT(IN)    :: KEY
  CHARACTER(LEN=*), DIMENSION(:), INTENT(IN)    :: VALUES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Impossible to write an array of  16-bit integer on a grib handle' )
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

END SUBROUTINE GRIB_METADATA_SET_STRING_ARRAY
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_BOOL_ARRAY'
SUBROUTINE GRIB_METADATA_SET_BOOL_ARRAY( THIS, KEY, VALUES )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T),  INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),        INTENT(IN)    :: KEY
  LOGICAL, DIMENSION(:),   INTENT(IN)    :: VALUES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Impossible to write an array of  16-bit integer on a grib handle' )
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

END SUBROUTINE GRIB_METADATA_SET_BOOL_ARRAY
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_INT8_ARRAY'
SUBROUTINE GRIB_METADATA_SET_INT8_ARRAY( THIS, KEY, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T),           INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                 INTENT(IN)    :: KEY
  INTEGER(KIND=INT8), DIMENSION(:), INTENT(IN)    :: VALUES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Impossible to write an array of  16-bit integer on a grib handle' )
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

END SUBROUTINE GRIB_METADATA_SET_INT8_ARRAY
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_INT16_ARRAY'
SUBROUTINE GRIB_METADATA_SET_INT16_ARRAY( THIS, KEY, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT16), DIMENSION(:), INTENT(IN)    :: VALUES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! This datatype is not supported by grib API
  PP_DEBUG_CRITICAL_THROW( 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Impossible to write an array of  16-bit integer on a grib handle' )
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

END SUBROUTINE GRIB_METADATA_SET_INT16_ARRAY
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_INT32_ARRAY'
SUBROUTINE GRIB_METADATA_SET_INT32_ARRAY( THIS, KEY, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: GRIB_API,    ONLY: GRIB_SET_INT_ARRAY
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT32), DIMENSION(:), INTENT(IN)    :: VALUES

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set the value into the grib handle
  CALL GRIB_SET_INT_ARRAY( THIS%IGRIB_HANDLE_, KEY, VALUES, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to set int32 array value.', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIB_METADATA_SET_INT32_ARRAY
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_INT64_ARRAY'
SUBROUTINE GRIB_METADATA_SET_INT64_ARRAY( THIS, KEY, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: GRIB_API,    ONLY: GRIB_SET_LONG_ARRAY
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T),            INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT64), DIMENSION(:), INTENT(IN)    :: VALUES

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set the value into the grib handle
  CALL GRIB_SET_LONG_ARRAY( THIS%IGRIB_HANDLE_, KEY, VALUES, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to set int64 array value.', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIB_METADATA_SET_INT64_ARRAY
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_REAL32_ARRAY'
SUBROUTINE GRIB_METADATA_SET_REAL32_ARRAY( THIS, KEY, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: GRIB_API,    ONLY: GRIB_SET_REAL4_ARRAY
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL32), DIMENSION(:), INTENT(IN)    :: VALUES

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set the value into the grib handle
  CALL GRIB_SET_REAL4_ARRAY( THIS%IGRIB_HANDLE_, KEY, VALUES, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to set real64 array value.', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIB_METADATA_SET_REAL32_ARRAY
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_REAL64_ARRAY'
SUBROUTINE GRIB_METADATA_SET_REAL64_ARRAY( THIS, KEY, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: GRIB_API,    ONLY: GRIB_SET_REAL8_ARRAY
  USE :: GRIB_API,    ONLY: GRIB_SUCCESS
  USE :: GRIB_API,    ONLY: GRIB_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_METADATA_T),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL64), DIMENSION(:), INTENT(IN)    :: VALUES

  ! Local variables
  INTEGER(KIND=JPIM_K) :: KRET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set the value into the grib handle
  CALL GRIB_SET_REAL8_ARRAY( THIS%IGRIB_HANDLE_, KEY, VALUES, STATUS=KRET )
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.GRIB_SUCCESS, 2 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=4096) :: GRIB_ERROR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
    CASE (2)
      GRIB_ERROR = REPEAT(' ', 4096)
      CALL GRIB_GET_ERROR_STRING( KRET, GRIB_ERROR )
      PP_DEBUG_CREATE_ERROR_MSG_GRIB( STR, 'Unable to set real64 array value.', KRET, GRIB_ERROR )
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

END SUBROUTINE GRIB_METADATA_SET_REAL64_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Sets an array of 128-bit real values as metadata.
!>
!> This procedure allows the user to store metadata in the object by associating
!> a key with an array of 128-bit real values.
!>
!> @param [inout] this   The object where metadata is to be set.
!> @param [in]    key    The key used to store the metadata.
!> @param [in]    values An array of 128-bit real values representing the metadata to be stored.
!>
!#define PP_PROCEDURE_TYPE 'SUBROUTINE'
!#define PP_PROCEDURE_NAME 'GRIB_METADATA_SET_REAL128_ARRAY'
!SUBROUTINE GRIB_METADATA_SET_REAL128_ARRAY( THIS, KEY, VALUES )
!
!  ! Symbolds imported from intrinsic modules
!  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
!
!  ! Symbols imported by the preprocessor for debugging purposes
!  PP_DEBUG_USE_VARS
!
!  ! Symbols imported by the preprocessor for tracing purposes
!  PP_TRACE_USE_VARS
!
!IMPLICIT NONE
!
!  ! Dummy arguments
!  CLASS(GRIB_METADATA_T),           INTENT(INOUT) :: THIS
!  CHARACTER(LEN=*),                 INTENT(IN)    :: KEY
!  REAL(KIND=REAL128), DIMENSION(:), INTENT(IN)    :: VALUES
!
!  ! Local variables declared by the preprocessor for debugging purposes
!  PP_DEBUG_DECL_VARS
!
!  ! Local variables declared by the preprocessor for tracing purposes
!  PP_TRACE_DECL_VARS
!
!  ! Trace begin of procedure
!  PP_TRACE_ENTER_PROCEDURE()
!
!  ! This procedure can be called only if the object is initialized
!  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )
!
!  ! This datatype is not supported by grib API
!  PP_DEBUG_CRITICAL_THROW( 2 )
!
!  ! Trace end of procedure (on success)
!  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()
!
!  ! Exit point on success
!  RETURN
!
!! Error handler
!PP_ERROR_HANDLER
!
!  ErrorHandler: BLOCK
!
!    ! Error handling variables
!    CHARACTER(LEN=:), ALLOCATABLE :: STR
!
!    ! HAndle different errors
!    SELECT CASE(ERRIDX)
!    CASE (1)
!      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )
!    CASE (2)
!      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Impossible to write an array of  16-bit integer on a grib handle' )
!    CASE DEFAULT
!      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
!    END SELECT
!
!    ! Trace end of procedure (on error)
!    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()
!
!    ! Write the error message and stop the program
!    PP_DEBUG_ABORT( STR )
!
!  END BLOCK ErrorHandler
!
!  ! Exit point on error
!  RETURN
!
!END SUBROUTINE GRIB_METADATA_SET_REAL128_ARRAY
!#undef PP_PROCEDURE_NAME
!#undef PP_PROCEDURE_TYPE


END MODULE GRIB_METADATA_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME