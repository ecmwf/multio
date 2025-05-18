!>
!> @file grib2_section1_reference_time_factory_mod.F90
!>
!> @brief Module containing the factory function for creating or initializing GRIB2 Param Configurator objects.
!>
!> The `GRIB2_SECTION1_REFERENCE_TIME_FACTORY_MOD` provides a factory function that creates or initializes
!> instances of GRIB2 Param Configurator objects. The function relies on various data structures and
!> types defined within the model's core and data types modules, as well as a YAML configuration
!> for initializing the section's parameters. Debugging, logging, and tracing features are enabled
!> via preprocessor directives to allow additional output when needed.
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [TYPE] GRIB2_REFERENCE_TIME_CONFIGURATOR_000_MOD::GRIB2_REFERENCE_TIME_CONFIGURATOR_000_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @author Mirco Valentini
!> @date   August, 2024
!>

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib2_section1_reference_time_factory_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SECTION1_REFERENCE_TIME_FACTORY_MOD'
MODULE GRIB2_SECTION1_REFERENCE_TIME_FACTORY_MOD

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!>
!> Key to be used to recover the reference_time_configurator number from the YAML configuration.
CHARACTER(LEN=*), PARAMETER :: REFERENCE_TIME_CONFIGURATOR_KEY='type'


!>
!> Generic interface to the make encoder function.
INTERFACE MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR
  MODULE PROCEDURE MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR_CFG
  MODULE PROCEDURE MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR_LAZY
END INTERFACE

!>
!> Public symbols (dataTypes)
PUBLIC :: MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR
PUBLIC :: DESTROY_GRIB2_REFERENCE_TIME_CONFIGURATOR

CONTAINS

!>
!> @brief Factory function for creating or initializing GRIB2 Param Configurator objects.
!>
!> This function acts as a factory for creating or initializing a GRIB2 Param Configurator object
!> based on the provided parameters. It assigns the proper type (`GRIB2_REFERENCE_TIME_CONFIGURATOR_000_T`)
!> to the `GRIB2_REFERENCE_TIME_CONFIGURATOR` object and configures it using the provided model parameters,
!> ID, and YAML configuration. If verbose mode is enabled, additional debug information
!> is output during the process.
!>
!> @param [inout] GRIB2_REFERENCE_TIME_CONFIGURATOR The GRIB2 Param Configurator object that will be created or initialized.
!>                              It must be a pointer of type `GRIB_SECTION_BASE_A`.
!> @param [in] ID Integer identifier for the GRIB2 Param Configurator object.
!> @param [in] CFG YAML configuration object used to configure the GRIB2 Param Configurator object.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Section that can be constructed with this factory
!>   - `GRIB2_REFERENCE_TIME_CONFIGURATOR_000_T`
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [TYPE] GRIB2_REFERENCE_TIME_CONFIGURATOR_000_MOD::GRIB2_REFERENCE_TIME_CONFIGURATOR_000_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_REFERENCE_TIME_CONFIGURATOR_000_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR_CFG'
PP_THREAD_SAFE FUNCTION MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR_CFG( GRIB2_REFERENCE_TIME_CONFIGURATOR, SEC4ID, &
&               CFG, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,                         ONLY: JPIB_K
  USE :: GRIB_SECTION_BASE_MOD,                     ONLY: GRIB_SECTION_BASE_A
  USE :: GRIB2_SECTION1_REFERENCE_TIME_DEFAULT_MOD, ONLY: GRIB2_SECTION1_REFERENCE_TIME_DEFAULT_T
  USE :: GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_MOD,  ONLY: GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T
  USE :: GRIB_SECTION_BASE_MOD,                     ONLY: GRIB_SECTION_FACTORY_T
  USE :: YAML_CORE_UTILS_MOD,                       ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_ENCODER_OPTIONS_MOD,                  ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: HOOKS_MOD,                                 ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_SECTION_BASE_A), POINTER, INTENT(INOUT) :: GRIB2_REFERENCE_TIME_CONFIGURATOR
  INTEGER(KINd=JPIB_K),                INTENT(IN)    :: SEC4ID
  TYPE(YAML_CONFIGURATION_T),          INTENT(IN)    :: CFG
  TYPE(GRIB_ENCODER_OPTIONS_T),        INTENT(IN)    :: OPT
  TYPE(GRIB_SECTION_FACTORY_T),        INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ID
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

   ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_SECTION_0=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INITIALIZATION_ERROR=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SECTION_TYPE=4_JPIB_K

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

  !> Read the encoder type from the configuration
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SECTION_TYPE) READ_GRIB2_REFERENCE_TIME_CONFIGURATOR_TYPE_FROM_CFG( CFG, SEC4ID, ID, HOOKS )

  ! Initialize the section
  SELECT CASE( ID )

  CASE( 0 )

    ALLOCATE( GRIB2_SECTION1_REFERENCE_TIME_DEFAULT_T::GRIB2_REFERENCE_TIME_CONFIGURATOR, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE( 1 )

    ALLOCATE( GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T::GRIB2_REFERENCE_TIME_CONFIGURATOR, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_SECTION_0 )

  END SELECT

  !> Initialization of the section
  PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  GRIB2_REFERENCE_TIME_CONFIGURATOR%INIT( CFG, OPT, FACTORIES, HOOKS )

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
    CHARACTER(LEN=32) :: TMP

    ! Initialize error frame
    PP_DEBUG_PUSH_FRAME()

    TMP = REPEAT(' ', 32)
    WRITE(TMP,'(I32)')  ID

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_READ_SECTION_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get reference_time_configurator ID' )
    CASE (ERRFLAG_UNKNOWN_SECTION_0)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown reference_time_configurator number: '//TRIM(ADJUSTL(TMP)) )
    CASE (ERRFLAG_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating reference_time_configurator number: '//TRIM(ADJUSTL(TMP)) )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_INITIALIZATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error initializing reference_time_configurator number: '//TRIM(ADJUSTL(TMP)) )
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

END FUNCTION MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




!>
!> @brief Factory function for creating or initializing GRIB2 Section 3 objects.
!>
!> This function acts as a factory for creating or initializing a GRIB2 Section 3 object
!> based on the provided parameters. It assigns the proper type (`GRIB2_SECTION3_000_T`)
!> to the `GRIB2_SECTION3` object and configures it using the provided model parameters,
!> ID, and YAML configuration. If verbose mode is enabled, additional debug information
!> is output during the process.
!>
!> @param [inout] GRIB2_SECTION3 The GRIB2 Section 3 object that will be created or initialized.
!>                              It must be a pointer of type `GRIB_SECTION_BASE_A`.
!> @param [in]    MSG All the mars keywords needed to describe the field `FORTRAN_MESSAGE_T`.
!> @param [in]    PAR All information outside mars keywords needed to describe the field `PARAMETRIZATION_T`.
!> @param [in]    OPT The encoder options structure of type `ENCODER_OPTIONS_T`.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Section that can be constructed with this factory
!>   - `GRIB2_SECTION3_040_T`
!>   - `GRIB2_SECTION3_050_T`
!>   - `GRIB2_SECTION3_101_T`
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [TYPE] GRIB2_SECTION0_040_MOD::GRIB2_SECTION0_040_T
!>   - @dependency [TYPE] GRIB2_SECTION0_050_MOD::GRIB2_SECTION0_050_T
!>   - @dependency [TYPE] GRIB2_SECTION0_101_MOD::GRIB2_SECTION0_101_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see GRIB2_SECTION3_040_T
!> @see GRIB2_SECTION3_050_T
!> @see GRIB2_SECTION3_101_T
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR_LAZY'
PP_THREAD_SAFE FUNCTION MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR_LAZY( GRIB2_REFERENCE_TIME_CONFIGURATOR, SEC4ID, &
&               MSG, PAR, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,                         ONLY: JPIB_K
  USE :: GRIB_SECTION_BASE_MOD,                     ONLY: GRIB_SECTION_BASE_A
  USE :: GRIB2_SECTION1_REFERENCE_TIME_DEFAULT_MOD, ONLY: GRIB2_SECTION1_REFERENCE_TIME_DEFAULT_T
  USE :: GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_MOD,  ONLY: GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T
  USE :: GRIB_SECTION_BASE_MOD,                     ONLY: GRIB_SECTION_FACTORY_T
  USE :: FORTRAN_MESSAGE_MOD,                       ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,                       ONLY: PARAMETRIZATION_T
  USE :: GRIB_ENCODER_OPTIONS_MOD,                  ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: HOOKS_MOD,                                 ONLY: HOOKS_T

#if defined( PP_HAS_GET_GRIB2_REFERENCE_TIME_CONFIGURATOR_ID_FROM_MESSAGE )
  !> Symbols imported from the mapping module
  USE :: MARS2GRIB_MAPPING_MOD, ONLY: GET_GRIB2_REFERENCE_TIME_CONFIGURATOR_ID_FROM_MESSAGE
#endif

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_SECTION_BASE_A), POINTER, INTENT(INOUT) :: GRIB2_REFERENCE_TIME_CONFIGURATOR
  INTEGER(KINd=JPIB_K),                INTENT(IN)    :: SEC4ID
  TYPE(FORTRAN_MESSAGE_T),             INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),             INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),        INTENT(IN)    :: OPT
  TYPE(GRIB_SECTION_FACTORY_T),        INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ID
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

   ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SECTION_TYPE=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_SECTION_3=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INITIALIZATION_ERROR=3_JPIB_K

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

#if defined( PP_HAS_GET_GRIB2_REFERENCE_TIME_CONFIGURATOR_ID_FROM_MESSAGE )

  !> Get the section0 ID from the message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SECTION_TYPE) GET_GRIB2_REFERENCE_TIME_CONFIGURATOR_ID_FROM_MESSAGE( MSG, PAR, SEC4ID, OPT, ID, HOOKS )

  ! Initialize the section
  SELECT CASE( ID )

  CASE( 0 )

    ALLOCATE( GRIB2_SECTION1_REFERENCE_TIME_DEFAULT_T::GRIB2_REFERENCE_TIME_CONFIGURATOR, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE( 1 )

    ALLOCATE( GRIB2_SECTION1_REFERENCE_TIME_CUSTOM_T::GRIB2_REFERENCE_TIME_CONFIGURATOR, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_SECTION_0 )

  END SELECT

  !> Initialization of the section
  PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  GRIB2_REFERENCE_TIME_CONFIGURATOR%INIT( MSG, PAR, OPT, FACTORIES, HOOKS )

#endif

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
    CHARACTER(LEN=32) :: TMP

    ! Initialize error frame
    PP_DEBUG_PUSH_FRAME()

    TMP = REPEAT(' ', 32)
    WRITE(TMP,'(I32)')  ID

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_READ_SECTION_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get section3 ID' )
    CASE (ERRFLAG_UNKNOWN_SECTION_3)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown section3 number: '//TRIM(ADJUSTL(TMP)) )
    CASE (ERRFLAG_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating section3 number: '//TRIM(ADJUSTL(TMP)) )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_INITIALIZATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error initializing section3 number: '//TRIM(ADJUSTL(TMP)) )
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

END FUNCTION MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR_LAZY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Destroys a GRIB2 Param Configurator structure.
!>
!> This function takes a GRIB2 Param Configurator object (`GRIB2_REFERENCE_TIME_CONFIGURATOR`) and performs
!> the necessary cleanup, freeing any associated resources. It also supports a
!> `VERBOSE` mode for detailed output during the destruction process.
!>
!> @section interface
!> @param[in,out] GRIB2_REFERENCE_TIME_CONFIGURATOR The GRIB2 Param Configurator object to be destroyed.
!>                              The structure is modified in place.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DESTROY_GRIB2_REFERENCE_TIME_CONFIGURATOR'
PP_THREAD_SAFE FUNCTION DESTROY_GRIB2_REFERENCE_TIME_CONFIGURATOR( GRIB2_REFERENCE_TIME_CONFIGURATOR, OPT, DESTRUCTORS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_BASE_A
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_DESTRUCTOR_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(GRIB_SECTION_BASE_A), POINTER, INTENT(INOUT) :: GRIB2_REFERENCE_TIME_CONFIGURATOR
  TYPE(GRIB_ENCODER_OPTIONS_T),        INTENT(IN)    :: OPT
  TYPE(GRIB_SECTION_DESTRUCTOR_T),     INTENT(IN)    :: DESTRUCTORS
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_ERROR=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ASSOCIATED=1_JPIB_K

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

  ! Check if the section is associated
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(GRIB2_REFERENCE_TIME_CONFIGURATOR), ERRFLAG_NOT_ASSOCIATED )

  !> Free the section
  PP_TRYCALL(ERRFLAG_FREE_ERROR)  GRIB2_REFERENCE_TIME_CONFIGURATOR%FREE( OPT, DESTRUCTORS, HOOKS )

  ! Initialize the section
  DEALLOCATE( GRIB2_REFERENCE_TIME_CONFIGURATOR, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )

  ! Nullify the pointer after deallocation
  GRIB2_REFERENCE_TIME_CONFIGURATOR => NULL()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
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
    CASE (ERRFLAG_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'reference_time_configurator to destroy is not associated' )
    CASE (ERRFLAG_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating reference_time_configurator number' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_FREE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error destructing reference_time_configurator' )
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

END FUNCTION DESTROY_GRIB2_REFERENCE_TIME_CONFIGURATOR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Read from the YAML configuration the type of the GRIB2 Param Configurator object.
!>
!> @section interface
!> @param [in]     CFG             YAML configuration object used to configure the GRIB2 Param Configurator object.
!> @param [out]    REFERENCE_TIME_CONFIGURATOR_TYPE   Identifier of the reference_time_configurator type read from the configuration.
!> @param [in,out] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection module dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see MAKE_GRIB2_REFERENCE_TIME_CONFIGURATOR
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_GRIB2_REFERENCE_TIME_CONFIGURATOR_TYPE_FROM_CFG'
PP_THREAD_SAFE FUNCTION READ_GRIB2_REFERENCE_TIME_CONFIGURATOR_TYPE_FROM_CFG( CFG, &
&  SEC4ID, REFERENCE_TIME_CONFIGURATOR_TYPE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_READ_STRING
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_IS_INTEGER
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_TO_INTEGER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(YAML_CONFIGURATION_T), INTENT(IN)    :: CFG
  INTEGER(KINd=JPIB_K),       INTENT(IN)    :: SEC4ID
  INTEGER(KIND=JPIB_K),       INTENT(OUT)   :: REFERENCE_TIME_CONFIGURATOR_TYPE
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=:), ALLOCATABLE :: CREFERENCE_TIME_CONFIGURATOR_TYPE
  LOGICAL :: HAS_REFERENCE_TIME_CONFIGURATOR
  LOGICAL :: REFERENCE_TIME_CONFIGURATOR_IS_INTEGER

   ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED_AFTER_READ=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_NOT_PRESENT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_KEY_IS_NOT_INTEGER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_KEY=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_ERROR=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_IS_INTEGER=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_TO_INTEGER=8_JPIB_K


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

  ! Initialization
  REFERENCE_TIME_CONFIGURATOR_TYPE = -1_JPIB_K

  !> Check if configuration has the REFERENCE_TIME_CONFIGURATOR key
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_KEY) YAML_CONFIGURATION_HAS_KEY( CFG, REFERENCE_TIME_CONFIGURATOR_KEY, HAS_REFERENCE_TIME_CONFIGURATOR, HOOKS )

  !> Read the REFERENCE_TIME_CONFIGURATOR template number
  IF ( HAS_REFERENCE_TIME_CONFIGURATOR ) THEN
    PP_TRYCALL(ERRFLAG_READ_ERROR) YAML_READ_STRING( CFG, REFERENCE_TIME_CONFIGURATOR_KEY, CREFERENCE_TIME_CONFIGURATOR_TYPE, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(CREFERENCE_TIME_CONFIGURATOR_TYPE), ERRFLAG_NOT_ALLOCATED_AFTER_READ )
    PP_TRYCALL( ERRFLAG_STRING_IS_INTEGER ) STRING_IS_INTEGER( CREFERENCE_TIME_CONFIGURATOR_TYPE, REFERENCE_TIME_CONFIGURATOR_IS_INTEGER, HOOKS )
    IF ( REFERENCE_TIME_CONFIGURATOR_IS_INTEGER ) THEN
       PP_TRYCALL( ERRFLAG_STRING_TO_INTEGER ) STRING_TO_INTEGER( CREFERENCE_TIME_CONFIGURATOR_TYPE, REFERENCE_TIME_CONFIGURATOR_TYPE, HOOKS )
    ELSE
      SELECT CASE (CREFERENCE_TIME_CONFIGURATOR_TYPE)
      CASE ( 'default' )
        REFERENCE_TIME_CONFIGURATOR_TYPE = 0_JPIB_K
      CASE ( 'custom' )
        REFERENCE_TIME_CONFIGURATOR_TYPE = 1_JPIB_K
      CASE DEFAULT
        PP_DEBUG_CRITICAL_THROW( ERRFLAG_KEY_IS_NOT_INTEGER )
      END SELECT
    ENDIF
    DEALLOCATE(CREFERENCE_TIME_CONFIGURATOR_TYPE, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
  ELSE
    ! TODO
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_KEY_NOT_PRESENT )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
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
    CASE (ERRFLAG_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'REFERENCE_TIME_CONFIGURATOR ctype not allocated after reading' )
    CASE (ERRFLAG_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating REFERENCE_TIME_CONFIGURATOR number' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_KEY_NOT_PRESENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key is not present' )
    CASE (ERRFLAG_KEY_IS_NOT_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Key is not integer' )

    CASE (ERRFLAG_UNABLE_TO_READ_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error calling check key' )
    CASE (ERRFLAG_READ_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading key' )
    CASE (ERRFLAG_STRING_IS_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in checking if string is an integer' )
    CASE (ERRFLAG_STRING_TO_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in converting string to integer' )
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

END FUNCTION READ_GRIB2_REFERENCE_TIME_CONFIGURATOR_TYPE_FROM_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE GRIB2_SECTION1_REFERENCE_TIME_FACTORY_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
