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

  !> @brief Sets a 128-bit real value.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL128 => MULTIO_METADATA_SET_REAL128

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

  !> @brief Sets an array of 128-bit real values.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS :: SET_REAL128_ARRAY => MULTIO_METADATA_SET_REAL128_ARRAY

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
FUNCTION MULTIO_METADATA_INITIALIZED( THIS ) RESULT(EX)

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(IN) :: THIS

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
FUNCTION MULTIO_METADATA_GET_MULTIO_METADATA( THIS ) RESULT(HANDLE)

  ! Symbols imported from other modules within the project.
  USE :: MULTIO_API, ONLY: MULTIO_METADATA

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), TARGET, INTENT(IN) :: THIS

  ! Function result
  TYPE(MULTIO_METADATA), POINTER :: HANDLE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is not initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Read the sample and set the initialization flag to .true.
  HANDLE => THIS%MULTIO_METADATA_

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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_MULTIO_HANDLE'
SUBROUTINE MULTIO_METADATA_SET_MULTIO_HANDLE( THIS, MIOH )

  ! Symbols imported from other modules within the project.
  USE :: GRIB_METADATA_MOD, ONLY: GRIB_METADATA_T
  USE :: MULTIO_API,        ONLY: MULTIO_HANDLE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),    INTENT(INOUT) :: THIS
  TYPE(MULTIO_HANDLE), TARGET, INTENT(IN)    :: MIOH

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Associate pointer to multio handle
  THIS%MULTIO_HANDLE_ => MIOH

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MULTIO_METADATA_SET_MULTIO_HANDLE
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_INIT_FROM_METADATA'
SUBROUTINE MULTIO_METADATA_INIT_FROM_METADATA( THIS, METADATA )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIM_K
  USE :: GRIB_METADATA_MOD, ONLY: GRIB_METADATA_T

  ! Symbols imported from other libraries
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),          INTENT(INOUT) :: THIS
  CLASS(METADATA_BASE_A),   POINTER, INTENT(IN)    :: METADATA

  ! Local variables
  INTEGER(KIND=JPIM_K) :: IGRIB_HANDLE
  INTEGER(KIND=C_INT)  :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check dummy arguments
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, 1 )
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.METADATA%INITIALIZED(), 2 )
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.ASSOCIATED(THIS%MULTIO_HANDLE_), 3 )

  ! This procedure has been defined only for some specific implementations
  SELECT TYPE( MD => METADATA )

  !-------------------------------------------------------------------------------------------------
  ! Initialize from grib metadata
  CLASS IS( MULTIO_METADATA_T )

    ! Read the sample and set the initialization flag to .true.
    CERR = THIS%MULTIO_METADATA_%NEW( THIS%MULTIO_HANDLE_ )
    PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 4 )

    CERR = THIS%MULTIO_METADATA_%COPY( MD%MULTIO_METADATA_ )
    PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 5 )

    ! Copy the tracer
    CALL THIS%TRACER_%COPY( MD%TRACER_ )

    ! Set the initialization flag to .true.
    THIS%INITIALIZED_ = .TRUE.

  !-------------------------------------------------------------------------------------------------
  ! Initialize from grib metadata
  CLASS IS( GRIB_METADATA_T )

    ! Read the sample and set the initialization flag to .true.
    CERR = THIS%MULTIO_METADATA_%NEW( THIS%MULTIO_HANDLE_ )
    PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 4 )

    ! Copy the tracer
    CALL THIS%TRACER_%COPY( MD%TRACER_ )

    ! Clone the MultIO metadata from grib metadata
    CALL MULTIO_GRIB_TO_MD( THIS%MULTIO_METADATA_, MD%GET_HANDLE() )
    THIS%INITIALIZED_ = .TRUE.

  !-------------------------------------------------------------------------------------------------
  ! Not implemented
  CLASS DEFAULT
    PP_DEBUG_CRITICAL_THROW( 5 )
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
    CHARACTER(LEN=:), ALLOCATABLE :: MIO_ERR_STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle already initialized' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Try to clone an object which is not initialized' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Multio handle not associated' )
    CASE (4)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create new metadata: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create new metadata' )
      ENDIF
    CASE (5)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      IF ( ALLOCATED( MIO_ERR_STR ) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to clone multIO metadata: '//TRIM(ADJUSTL(MIO_ERR_STR)) )
        DEALLOCATE( MIO_ERR_STR )
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to clone multIO metadata' )
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

END SUBROUTINE MULTIO_METADATA_INIT_FROM_METADATA
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_INIT_FROM_SAMPLE_NAME'
SUBROUTINE MULTIO_METADATA_INIT_FROM_SAMPLE_NAME( THIS, SAMPLE_NAME )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT

  ! Symbols imported from other modules within the project.
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: SAMPLE_NAME

  ! Local variables
  LOGICAL             :: LOC_DISTRIBUTE
  INTEGER(KIND=C_INT) :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is not initialized
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, 1 )
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.ASSOCIATED(THIS%MULTIO_HANDLE_), 2 )


  ! Read the sample and set the initialization flag to .true.
  CERR = THIS%MULTIO_METADATA_%NEW( THIS%MULTIO_HANDLE_ )
  PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 3 )
  CERR = THIS%MULTIO_METADATA_%SET_STRING( 'eccodes_sample', SAMPLE_NAME )
  PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 4 )
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle already initialized' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unexpected optional data' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Multio handle not associated' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Failed to initialize MultIO metadata' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Failed to set SAMPLE name to the metadata' )

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

END SUBROUTINE MULTIO_METADATA_INIT_FROM_SAMPLE_NAME
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_INIT_FROM_SAMPLE'
SUBROUTINE MULTIO_METADATA_INIT_FROM_SAMPLE( THIS, SAMPLE_NAME, SAMPLE_HANDLE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: SAMPLE_NAME
  INTEGER(KIND=JPIM_K),     INTENT(IN)    :: SAMPLE_HANDLE

  ! Local variables
  LOGICAL             :: LOC_DISTRIBUTE
  INTEGER(KIND=C_INT) :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is not initialized
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, 1 )
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.ASSOCIATED(THIS%MULTIO_HANDLE_), 2 )


  ! Read the sample and set the initialization flag to .true.
  CERR = THIS%MULTIO_METADATA_%NEW( THIS%MULTIO_HANDLE_ )
  PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 3 )
  CERR = THIS%MULTIO_METADATA_%SET_STRING( 'eccodes_sample', SAMPLE_NAME )
  PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 4 )
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle already initialized' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unexpected optional data' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Multio handle not associated' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Failed to initialize MultIO metadata' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Failed to set SAMPLE name to the metadata' )

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

END SUBROUTINE MULTIO_METADATA_INIT_FROM_SAMPLE
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_INIT_DEFAULT'
SUBROUTINE MULTIO_METADATA_INIT_DEFAULT( THIS )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT

  ! Symbols imported from other modules within the project.
  USE :: MULTIO_API,  ONLY: MULTIO_HANDLE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS

  ! Local variables
  INTEGER(KIND=C_INT) :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is not initialized
  PP_DEBUG_DEVELOP_COND_THROW( THIS%INITIALIZED_, 1 )
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.ASSOCIATED(THIS%MULTIO_HANDLE_), 2 )

  ! Read the sample and set the initialization flag to .true.
  CERR = THIS%MULTIO_METADATA_%NEW( THIS%MULTIO_HANDLE_ )
  PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 3 )
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle already initialized' )

    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Multio handle not associated' )

    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Failed to initialize multio metadata' )

    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unrecognize optional data' )

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

END SUBROUTINE MULTIO_METADATA_INIT_DEFAULT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Destroys the object.
!>
!> This procedure deallocates resources associated with the object.
!>
!> @param [inout] this The object to be destroyed.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_DESTROY'
SUBROUTINE MULTIO_METADATA_DESTROY( THIS )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS

  ! Local variables
  INTEGER(KIND=C_INT) :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set the value into the handle
  CERR = THIS%MULTIO_METADATA_%DELETE( )
  PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 2 )
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle not initialized' )

    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Delete the metadata' )

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

END SUBROUTINE MULTIO_METADATA_DESTROY
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_STRING'
SUBROUTINE MULTIO_METADATA_SET_STRING( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  CHARACTER(LEN=*),         INTENT(IN)    :: VAL

  ! Local variables
  INTEGER(KIND=C_INT) :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set the value into the handle
  CERR = THIS%MULTIO_METADATA_%SET_STRING( KEY, VAL )
  PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set a string in the metadata' )

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

END SUBROUTINE MULTIO_METADATA_SET_STRING
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_BOOL'
SUBROUTINE MULTIO_METADATA_SET_BOOL( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  LOGICAL,                  INTENT(IN)    :: VAL

  ! Local variables
  INTEGER(KIND=C_LONG) :: KVAL
  INTEGER(KIND=C_INT)  :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Convert the logical to a long
  IF ( VAL ) THEN
    KVAL = 1_C_LONG
  ELSE
    KVAL = 0_C_LONG
  ENDIF

  ! Set the value into the handle
  CERR = THIS%MULTIO_METADATA_%SET_LONG( KEY, KVAL )
  PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Failed to set the value inside the metadata' )

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

END SUBROUTINE MULTIO_METADATA_SET_BOOL
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT8'
SUBROUTINE MULTIO_METADATA_SET_INT8( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_LONG
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  INTEGER(KIND=INT8),       INTENT(IN)    :: VAL

  ! Local variables
  INTEGER(KIND=C_LONG) :: KVAL
  INTEGER(KIND=C_INT)  :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! This datatype is not supported by grib API
  KVAL = INT(VAL,C_LONG)

  ! Set the value into the handle
  CERR = THIS%MULTIO_METADATA_%SET_LONG( KEY, KVAL )
  PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Failed to set the value inside the metadata' )

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

END SUBROUTINE MULTIO_METADATA_SET_INT8
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT16'
SUBROUTINE MULTIO_METADATA_SET_INT16( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_LONG
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  INTEGER(KIND=INT16),      INTENT(IN)    :: VAL

  ! Local variables
  INTEGER(KIND=C_LONG) :: KVAL
  INTEGER(KIND=C_INT)  :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! This datatype is not supported by grib API
  KVAL = INT(VAL,C_LONG)

  ! Set the value into the handle
  CERR = THIS%MULTIO_METADATA_%SET_LONG( KEY, KVAL )
  PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Failed to set the value inside the metadata' )

    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Failed to set the value inside the metadata' )

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

END SUBROUTINE MULTIO_METADATA_SET_INT16
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT32'
SUBROUTINE MULTIO_METADATA_SET_INT32( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_LONG
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  INTEGER(KIND=INT32),      INTENT(IN)    :: VAL

  ! Local variables
  INTEGER(KIND=C_LONG) :: KVAL
  INTEGER(KIND=C_INT)  :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! This datatype is not supported by grib API
  KVAL = INT(VAL,C_LONG)

  ! Set the value into the handle
  CERR = THIS%MULTIO_METADATA_%SET_LONG( KEY, KVAL )
  PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Failed to set the value inside the metadata' )

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

END SUBROUTINE MULTIO_METADATA_SET_INT32
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT64'
SUBROUTINE MULTIO_METADATA_SET_INT64( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_LONG
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  INTEGER(KIND=INT64),      INTENT(IN)    :: VAL

  ! Local variables
  INTEGER(KIND=C_LONG) :: KVAL
  INTEGER(KIND=C_INT)  :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! This datatype is not supported by grib API
  KVAL = INT(VAL,C_LONG)

  ! Set the value into the handle
  CERR = THIS%MULTIO_METADATA_%SET_LONG( KEY, KVAL )
  PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Failed to set the value inside the metadata' )

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

END SUBROUTINE MULTIO_METADATA_SET_INT64
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_REAL32'
SUBROUTINE MULTIO_METADATA_SET_REAL32( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_FLOAT
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  REAL(KIND=REAL32),        INTENT(IN)    :: VAL

  ! Local variables
  REAL(KIND=C_FLOAT)  :: ZVAL
  INTEGER(KIND=C_INT) :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Cast the input value to the correct datatype
  ZVAL = REAL(VAL, C_FLOAT)

  ! Set the value into the handle
  CERR = THIS%MULTIO_METADATA_%SET_FLOAT( KEY, ZVAL )
  PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Failed to set the value inside the metadata' )

    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Handle already initialized' )

    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_METADATA_SET_REAL32
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_REAL64'
SUBROUTINE MULTIO_METADATA_SET_REAL64( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_DOUBLE
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  REAL(KIND=REAL64),        INTENT(IN)    :: VAL

  ! Local variables
  REAL(KIND=C_DOUBLE) :: ZVAL
  INTEGER(KIND=C_INT) :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Cast the input value to the correct datatype
  ZVAL = REAL(VAL, C_DOUBLE)

  ! Set the value into the handle
  CERR = THIS%MULTIO_METADATA_%SET_DOUBLE( KEY, ZVAL )
  PP_DEBUG_CRITICAL_COND_THROW( CERR.NE.0, 2 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Failed to set the value inside the metadata' )

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

END SUBROUTINE MULTIO_METADATA_SET_REAL64
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_REAL128'
SUBROUTINE MULTIO_METADATA_SET_REAL128( THIS, KEY, VAL )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS
IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  REAL(KIND=REAL128),       INTENT(IN)    :: VAL

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

END SUBROUTINE MULTIO_METADATA_SET_REAL128
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_STRING_ARRAY'
SUBROUTINE MULTIO_METADATA_SET_STRING_ARRAY( THIS, KEY, VALUES )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),       INTENT(INOUT) :: THIS
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

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END SUBROUTINE MULTIO_METADATA_SET_STRING_ARRAY
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_BOOL_ARRAY'
SUBROUTINE MULTIO_METADATA_SET_BOOL_ARRAY( THIS, KEY, VALUES )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),         INTENT(IN)    :: KEY
  LOGICAL, DIMENSION(:),    INTENT(IN)    :: VALUES

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

END SUBROUTINE MULTIO_METADATA_SET_BOOL_ARRAY
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT8_ARRAY'
SUBROUTINE MULTIO_METADATA_SET_INT8_ARRAY( THIS, KEY, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),         INTENT(INOUT) :: THIS
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

END SUBROUTINE MULTIO_METADATA_SET_INT8_ARRAY
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT16_ARRAY'
SUBROUTINE MULTIO_METADATA_SET_INT16_ARRAY( THIS, KEY, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT16

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),          INTENT(INOUT) :: THIS
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

END SUBROUTINE MULTIO_METADATA_SET_INT16_ARRAY
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT32_ARRAY'
SUBROUTINE MULTIO_METADATA_SET_INT32_ARRAY( THIS, KEY, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS


IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT32), DIMENSION(:), INTENT(IN)    :: VALUES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set value to the grib_handle
  WRITE(*,*) 'Write arrays into the metadata is still no supported'
  ! PP_DEBUG_CRITICAL_THROW(2)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Not implemented' )

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

END SUBROUTINE MULTIO_METADATA_SET_INT32_ARRAY
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_INT64_ARRAY'
SUBROUTINE MULTIO_METADATA_SET_INT64_ARRAY( THIS, KEY, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),          INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                  INTENT(IN)    :: KEY
  INTEGER(KIND=INT64), DIMENSION(:), INTENT(IN)    :: VALUES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! This datatype is not supported by grib API
  WRITE(*,*) 'Write arrays into the metadata is still no supported'
  ! PP_DEBUG_CRITICAL_THROW(2)

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

END SUBROUTINE MULTIO_METADATA_SET_INT64_ARRAY
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_REAL32_ARRAY'
SUBROUTINE MULTIO_METADATA_SET_REAL32_ARRAY( THIS, KEY, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),        INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL32), DIMENSION(:), INTENT(IN)    :: VALUES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set value to the grib_handle
  WRITE(*,*) 'Write arrays into the metadata is still no supported'
  ! PP_DEBUG_CRITICAL_THROW(2)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'nimplemented double array in metadata' )

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

END SUBROUTINE MULTIO_METADATA_SET_REAL32_ARRAY
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
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_REAL64_ARRAY'
SUBROUTINE MULTIO_METADATA_SET_REAL64_ARRAY( THIS, KEY, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),        INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                INTENT(IN)    :: KEY
  REAL(KIND=REAL64), DIMENSION(:), INTENT(IN)    :: VALUES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! This procedure can be called only if the object is initialized
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.THIS%INITIALIZED_, 1 )

  ! Set value to the grib_handle
  WRITE(*,*) 'Write arrays into the metadata is still no supported'
  ! PP_DEBUG_CRITICAL_THROW(2)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unimplemented double array in metadata' )

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

END SUBROUTINE MULTIO_METADATA_SET_REAL64_ARRAY
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
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_METADATA_SET_REAL128_ARRAY'
SUBROUTINE MULTIO_METADATA_SET_REAL128_ARRAY( THIS, KEY, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(MULTIO_METADATA_T),         INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),                 INTENT(IN)    :: KEY
  REAL(KIND=REAL128), DIMENSION(:), INTENT(IN)    :: VALUES

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

END SUBROUTINE MULTIO_METADATA_SET_REAL128_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB_TO_MD_SPECTRAL'
SUBROUTINE MULTIO_GRIB_TO_MD_SPECTRAL( MIOMD, KGRIB_HANDLE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported from other libraries
  USE :: ECCODES,    ONLY: CODES_GET
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_METADATA), INTENT(IN)  :: MIOMD
  INTEGER(KIND=JPIM_K),  INTENT(IN)  :: KGRIB_HANDLE

  ! Local variabels
  INTEGER(KIND=INT64)  :: IVALUE
  INTEGER(KIND=C_INT)  :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  CALL CODES_GET(KGRIB_HANDLE,"sphericalHarmonics",IVALUE)
  CERR = MIOMD%SET_LONG("sphericalHarmonics",IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 1 )


  CALL CODES_GET(KGRIB_HANDLE,"complexPacking",IVALUE)
  CERR = MIOMD%SET_LONG("complexPacking",IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 2 )


  CALL CODES_GET(KGRIB_HANDLE,"pentagonalResolutionParameterJ",IVALUE)
  CERR = MIOMD%SET_LONG("pentagonalResolutionParameterJ",IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 3 )


  CALL CODES_GET(KGRIB_HANDLE,"pentagonalResolutionParameterK",IVALUE)
  CERR = MIOMD%SET_LONG("pentagonalResolutionParameterK",IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 4 )


  CALL CODES_GET(KGRIB_HANDLE,"pentagonalResolutionParameterM",IVALUE)
  CERR = MIOMD%SET_LONG("pentagonalResolutionParameterM",IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 5 )


  CALL CODES_GET(KGRIB_HANDLE,"subSetJ",IVALUE)
  CERR = MIOMD%SET_LONG("subSetJ",IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 6 )


  CALL CODES_GET(KGRIB_HANDLE,"subSetK",IVALUE)
  CERR = MIOMD%SET_LONG("subSetK",IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 7 )


  CALL CODES_GET(KGRIB_HANDLE,"subSetM",IVALUE)
  CERR = MIOMD%SET_LONG("subSetM",IVALUE)
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 8 )


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
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (sphericalHarmonics): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (2)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (complexPacking): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (3)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (pentagonalResolutionParameterJ): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (4)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (pentagonalResolutionParameterK): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (5)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (pentagonalResolutionParameterM): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (6)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (subSetJ): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (7)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (subSetK): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (8)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (subSetM): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
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

END SUBROUTINE MULTIO_GRIB_TO_MD_SPECTRAL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MULTIO_GRIB_TO_MD'
SUBROUTINE MULTIO_GRIB_TO_MD( MIOMD, KGRIB_HANDLE )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K

  ! Symbols imported from other libraries
  USE :: ECCODES,    ONLY: CODES_KEYS_ITERATOR_NEW
  USE :: ECCODES,    ONLY: CODES_KEYS_ITERATOR_NEXT
  USE :: ECCODES,    ONLY: CODES_KEYS_ITERATOR_GET_NAME
  USE :: ECCODES,    ONLY: CODES_GET
  USE :: ECCODES,    ONLY: CODES_IS_DEFINED
  USE :: ECCODES,    ONLY: CODES_KEYS_ITERATOR_DELETE
  USE :: ECCODES,    ONLY: CODES_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_METADATA
  USE :: MULTIO_API, ONLY: MULTIO_SUCCESS
  USE :: MULTIO_API, ONLY: MULTIO_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MULTIO_METADATA), INTENT(IN) :: MIOMD
  INTEGER(KIND=JPIM_K),  INTENT(IN) :: KGRIB_HANDLE

  ! Local variables
  CHARACTER(LEN=20)    :: NAME_SPACE
  INTEGER(KIND=JPIM_K) :: KITER
  INTEGER(KIND=JPIM_K) :: IRET
  INTEGER(KIND=JPIM_K) :: IHAS
  INTEGER(KIND=INT64)  :: DATADATE
  INTEGER(KIND=INT64)  :: DATATIME
  INTEGER(KIND=INT64)  :: STEPUNITS
  INTEGER(KIND=INT64)  :: STARTSTEP
  INTEGER(KIND=INT64)  :: ENDSTEP
  INTEGER(KIND=INT64)  :: TIMEINCREMENTUNIT
  INTEGER(KIND=INT64)  :: TIMEINCREMENT
  CHARACTER(LEN=256)   :: KEY
  CHARACTER(LEN=256)   :: VALUE
  INTEGER(KIND=C_INT)  :: CERR

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Set the namespace to be used to lookup keywords ('ls', 'mars')
  NAME_SPACE='mars'

  ! Create a new iterator
  CALL CODES_KEYS_ITERATOR_NEW( KGRIB_HANDLE, KITER, NAME_SPACE )

  ! Loop over the keywords
  MarsKeywords: DO

    ! Read the next key
    CALL CODES_KEYS_ITERATOR_NEXT( KITER, IRET )

    ! Exit condition (No more keywords)
    IF ( IRET .NE. CODES_SUCCESS ) THEN
      EXIT MarsKeywords
    ENDIF

    ! Get key name
    CALL CODES_KEYS_ITERATOR_GET_NAME( KITER, KEY )

    ! Get value and set to multio metadata
    CALL CODES_GET( KGRIB_HANDLE, TRIM(KEY), VALUE )
    CERR = MIOMD%SET_STRING( KEY, VALUE )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 1 )

  END DO MarsKeywords

  ! Configure grib edition
  CALL CODES_GET( KGRIB_HANDLE, "edition", VALUE )
  CERR = MIOMD%SET_STRING( "gribEdition", VALUE )
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 2 )


  IF (VALUE.EQ. "2") THEN
    CALL CODES_GET( KGRIB_HANDLE, "dataDate", DATADATE )
    CERR = MIOMD%SET_LONG( "startDate", DATADATE )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 3 )

    CALL CODES_GET( KGRIB_HANDLE, "dataTime", DATATIME )
    CERR = MIOMD%SET_LONG( "startTime", DATATIME )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 4 )

    CALL CODES_GET( KGRIB_HANDLE, "stepUnits", STEPUNITS )
    CERR = MIOMD%SET_LONG( "stepUnits", STEPUNITS )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 5 )

    CALL CODES_GET( KGRIB_HANDLE, "startStep", STARTSTEP )
    CERR = MIOMD%SET_LONG("startStep",STARTSTEP)
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 6 )

    CALL CODES_GET( KGRIB_HANDLE, "endStep", ENDSTEP )
    CERR = MIOMD%SET_LONG( "endStep", ENDSTEP )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 7 )

    CALL CODES_IS_DEFINED( KGRIB_HANDLE, "timeIncrement", IHAS )
    IF (IHAS.NE. 0) THEN

      CALL CODES_GET( KGRIB_HANDLE, "indicatorOfUnitForTimeIncrement", TIMEINCREMENTUNIT )
      CERR = MIOMD%SET_LONG( "indicatorOfUnitForTimeIncrement", TIMEINCREMENTUNIT )
      PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 8 )

      CALL CODES_GET( KGRIB_HANDLE, "timeIncrement", TIMEINCREMENT )
      CERR = MIOMD%SET_LONG( "timeIncrement", TIMEINCREMENT )
      PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 9 )

    ENDIF
  ELSE
    CALL CODES_GET( KGRIB_HANDLE, "stepUnits", STEPUNITS )
    CERR = MIOMD%SET_LONG( "stepUnits", STEPUNITS )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 10 )

    CALL CODES_GET( KGRIB_HANDLE, "startStep", STARTSTEP )
    CERR = MIOMD%SET_LONG( "startStep", STARTSTEP )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 11 )

    CALL CODES_GET( KGRIB_HANDLE, "endStep", ENDSTEP )
    CERR = MIOMD%SET_LONG( "endStep", ENDSTEP )
    PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 12 )
  ENDIF

  CALL CODES_GET( KGRIB_HANDLE, "paramId", VALUE )
  CERR = MIOMD%SET_STRING( "paramId", VALUE )
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 13 )

  CALL CODES_GET( KGRIB_HANDLE, "gridType", VALUE )
  CERR = MIOMD%SET_STRING( "gridType", VALUE )
  PP_DEBUG_DEVELOP_COND_THROW( CERR.NE.MULTIO_SUCCESS, 14 )

  IF ( VALUE.EQ. "sh") THEN
    CALL MULTIO_GRIB_TO_MD_SPECTRAL( MIOMD, KGRIB_HANDLE )
  ENDIF

  ! Delete the iterator
  CALL CODES_KEYS_ITERATOR_DELETE( KITER )

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
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata ('//TRIM(KEY)//'): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (2)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (gribEdition): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (3)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (startDate): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (4)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (startTime): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (5)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (stepUnits): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (6)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (startStep): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (7)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (endStep): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (8)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (indicatorOfUnitForTimeIncrement): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (9)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (timeIncrement): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (10)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (stepUnits): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (11)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (startStep): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (12)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (endStep): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (13)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (paramId): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
      IF ( ALLOCATED( MIO_ERR_STR ) ) DEALLOCATE( MIO_ERR_STR )
    CASE (14)
      MIO_ERR_STR = MULTIO_ERROR_STRING(CERR)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to set MultIO metadata (gridType): '//TRIM(ADJUSTL(MIO_ERR_STR)) )
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

END SUBROUTINE MULTIO_GRIB_TO_MD
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE MULTIO_METADATA_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME