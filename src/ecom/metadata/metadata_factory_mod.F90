!> @file
!>
!> @brief Factory for metadata objects.
!>
!> This module serves as a factory for metadata objects, supporting currently
!> two types: ['grib', 'multIO'].
!>
!> Additionally, the factory includes utilities to generate and allocate arrays
!> of metadata, not limited to scalar instances.
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
MODULE METADATA_FACTORY_MOD

  ! Symbols imported from other modules within the project.
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

IMPLICIT NONE

! Default visibility of the module
PRIVATE

! Use polymorphism to reduce the public interfaces
INTERFACE MAKE_METADATA
  MODULE PROCEDURE MAKE_METADATA_SCALAR
  MODULE PROCEDURE MAKE_METADATA_ARRAY
  MODULE PROCEDURE MAKE_METADATA_SCALAR_1ARG
  MODULE PROCEDURE MAKE_METADATA_ARRAY_1ARG
END INTERFACE

INTERFACE DESTROY_METADATA
  MODULE PROCEDURE DESTROY_METADATA_SCALAR
  MODULE PROCEDURE DESTROY_METADATA_ARRAY
END INTERFACE

! Whitelist of public symbols
PUBLIC :: METADATA_BASE_A
PUBLIC :: MAKE_METADATA
PUBLIC :: DESTROY_METADATA

CONTAINS


!>
!> @brief Instantiates a new scalar metadata object.
!>
!> @param [in]    type Type of the metadata to be instantiated.
!> @param [inout] md   The newly created array of metadata objects.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAKE_METADATA_SCALAR'
SUBROUTINE MAKE_METADATA_SCALAR( TYPE, MD )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,         ONLY: JPIB_K
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A
  USE :: GRIB_METADATA_MOD,   ONLY: GRIB_METADATA_T
  USE :: MULTIO_METADATA_MOD, ONLY: MULTIO_METADATA_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                INTENT(IN)    :: TYPE
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: MD

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  !> @todo: prepare strings before comparing
  SELECT CASE ( TYPE )

  ! --------------------------------------------------------------------
  ! Multio metadata
  CASE ( 'MULTIO' )
    PP_DEBUG_CRITICAL_THROW( 2 )

  ! --------------------------------------------------------------------
  ! Grib metadata
  CASE ( 'GRIB' )
    ALLOCATE( GRIB_METADATA_T::MD, STAT=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )

  ! --------------------------------------------------------------------
  ! Fallback
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( 100 )

  END SELECT

  ! Check error message
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(ERRMSG), 101 )

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
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'MultIO handle required for allocating a multIO object' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating grib metadata: '//TRIM(ADJUSTL(ERRMSG)) )
    CASE (100)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown metadata type to allocate' )
    CASE (101)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error message allocated at the end of the function' )
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

END SUBROUTINE MAKE_METADATA_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Instantiates a new array of metadata objects.
!>
!> @param [in]    type Type of the metadata to be instantiated.
!> @param [in]    size Size of the metadata array to be instantiated.
!> @param [inout] md   The newly created array of metadata objects.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAKE_METADATA_ARRAY'
SUBROUTINE MAKE_METADATA_ARRAY( TYPE, SIZE, MD)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,         ONLY: JPIB_K
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A
  USE :: GRIB_METADATA_MOD,   ONLY: GRIB_METADATA_T
  USE :: MULTIO_METADATA_MOD, ONLY: MULTIO_METADATA_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN) :: TYPE
  INTEGER(KIND=JPIB_K), INTENT(IN) :: SIZE

  ! Function result
  CLASS(METADATA_BASE_A), DIMENSION(:), POINTER :: MD

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Dummy arguments check
  PP_DEBUG_DEVELOP_COND_THROW( SIZE.LE.0, 1 )

  !> @todo: prepare strings before comparing
  SELECT CASE ( TYPE )

  CASE ( 'MULTIO' )
    PP_DEBUG_CRITICAL_THROW( 2 )

  ! ------------------------------------------------------------------------------------------------
  ! Grib metadata
  CASE ( 'GRIB' )
    ALLOCATE( GRIB_METADATA_T::MD(SIZE), STAT=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 3 )

  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( 100 )
  END SELECT

  ! Check error message
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(ERRMSG), 101 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Size must be greater than 0' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'MultIO handle required for allocating a multIO object' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating grib metadata: '//TRIM(ADJUSTL(ERRMSG)) )
    CASE (100)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Invalid object to allocate: '//TRIM(ADJUSTL(TYPE)) )
    CASE (101)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error message allocated at the end of the function' )
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

END SUBROUTINE MAKE_METADATA_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Instantiates a new scalar metadata object.
!>
!> @param [in]    type Type of the metadata to be instantiated.
!> @param [inout] md   The newly created array of metadata objects.
!> @param [in]    mioh Multio handle to be used to initialise the metadata
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAKE_METADATA_SCALAR_1ARG'
SUBROUTINE MAKE_METADATA_SCALAR_1ARG( TYPE, MD, MIOH )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,         ONLY: JPIB_K
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A
  USE :: GRIB_METADATA_MOD,   ONLY: GRIB_METADATA_T
  USE :: MULTIO_METADATA_MOD, ONLY: MULTIO_METADATA_T
  USE :: MULTIO_API,          ONLY: MULTIO_HANDLE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                INTENT(IN)    :: TYPE
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: MD
  TYPE(MULTIO_HANDLE),             INTENT(IN)    :: MIOH

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  !> @todo: prepare strings before comparing
  SELECT CASE ( TYPE )

  ! --------------------------------------------------------------------
  ! Multio metadata
  CASE ( 'MULTIO' )
    ALLOCATE( MULTIO_METADATA_T::MD, STAT=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
    SELECT TYPE ( A => MD )
    CLASS IS ( MULTIO_METADATA_T )
        CALL A%SET_MULTIO_HANDLE( MIOH )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( 4 )
    END SELECT

  ! ------------------------------------------------------------------------------------------------
  ! Grib metadata
  CASE ( 'GRIB' )
    PP_DEBUG_CRITICAL_THROW( 3 )

  ! --------------------------------------------------------------------
  ! Fallback
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( 100 )

  END SELECT

  ! Check error message
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(ERRMSG), 101 )

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
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating multio metadata: '//TRIM(ADJUSTL(ERRMSG)) )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'grib metadata does not need multio handle' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'ambiguous alocated object' )
    CASE (100)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown metadata type to allocate' )
    CASE (101)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error message allocated at the end of the function' )
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

END SUBROUTINE MAKE_METADATA_SCALAR_1ARG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Instantiates a new array of metadata objects.
!>
!> @param [in]    type Type of the metadata to be instantiated.
!> @param [in]    size Size of the metadata array to be instantiated.
!> @param [inout] md   The newly created array of metadata objects.
!> @param [in]    mioh Multio handle to be used to initialise the metadata
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MAKE_METADATA_ARRAY_1ARG'
SUBROUTINE MAKE_METADATA_ARRAY_1ARG( TYPE, SIZE, MD, MIOH )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,         ONLY: JPIB_K
  USE :: METADATA_BASE_MOD,   ONLY: METADATA_BASE_A
  USE :: GRIB_METADATA_MOD,   ONLY: GRIB_METADATA_T
  USE :: MULTIO_METADATA_MOD, ONLY: MULTIO_METADATA_T
  USE :: MULTIO_API,          ONLY: MULTIO_HANDLE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                              INTENT(IN)    :: TYPE
  INTEGER(KIND=JPIB_K),                          INTENT(IN)    :: SIZE
  CLASS(METADATA_BASE_A), DIMENSION(:), POINTER, INTENT(INOUT) :: MD
  TYPE(MULTIO_HANDLE),                           INTENT(IN)    :: MIOH

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Dummy arguments check
  PP_DEBUG_DEVELOP_COND_THROW( SIZE.LE.0, 1 )

  !> @todo: prepare strings before comparing
  SELECT CASE ( TYPE )

  CASE ( 'MULTIO' )
    ALLOCATE( MULTIO_METADATA_T::MD(SIZE), STAT=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
    SELECT TYPE ( A => MD )
    CLASS IS ( MULTIO_METADATA_T )
      DO I = 1, SIZE
        CALL A(I)%SET_MULTIO_HANDLE( MIOH )
      ENDDO
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( 4 )
    END SELECT

  ! ------------------------------------------------------------------------------------------------
  ! Grib metadata
  CASE ( 'GRIB' )
    PP_DEBUG_CRITICAL_THROW( 3 )

  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( 100 )
  END SELECT

  ! Check error message
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(ERRMSG), 101 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Size must be greater than 0' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating multIO metadata: '//TRIM(ADJUSTL(ERRMSG)) )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'grib metadata does not need multio handle' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'ambiguous alocated object' )
    CASE (100)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Invalid object to allocate: '//TRIM(ADJUSTL(TYPE)) )
    CASE (101)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error message allocated at the end of the function' )
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

END SUBROUTINE MAKE_METADATA_ARRAY_1ARG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Deallocate a scalar metadata object.
!>
!> @param [in] md The scalar object to be deallocated
!>
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DESTROY_METADATA_SCALAR'
SUBROUTINE DESTROY_METADATA_SCALAR( MD )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), POINTER , INTENT(INOUT):: MD

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check dummy arguments
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.ASSOCIATED(MD), 1 )

  ! Deallocate the metadata
  DEALLOCATE( MD )
  NULLIFY( MD )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Metadata must be allocated' )
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

END SUBROUTINE DESTROY_METADATA_SCALAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Deallocate a array metadata object.
!>
!> @param [in] md The array object to be deallocated
!>
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DESTROY_METADATA_ARRAY'
SUBROUTINE DESTROY_METADATA_ARRAY( MDA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(METADATA_BASE_A), DIMENSION(:), POINTER, INTENT(INOUT) :: MDA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check dummy arguments
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.ASSOCIATED(MDA), 1 )

  ! Deallocate the metadata
  DEALLOCATE( MDA )
  NULLIFY( MDA )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Metadata must be allocated' )
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

END SUBROUTINE DESTROY_METADATA_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE METADATA_FACTORY_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
