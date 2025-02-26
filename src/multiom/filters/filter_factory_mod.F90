!>
!> @file filter_factory_mod.F90
!>
!> @brief Module containing the factory function for creating or initializing encoder objects.
!>
!> The `FILTER_FACTORY_MOD` provides a factory function that creates or initializes
!> instances of encoder objects. The function relies on various data structures and
!> types defined within the model's core and data types modules, as well as a YAML configuration
!> for initializing the section's parameters. Debugging, logging, and tracing features are enabled
!> via preprocessor directives to allow additional output when needed.
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [TYPE] FILTER_000_MOD::FILTER_000_T
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


#define PP_FILE_NAME 'filter_factory_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'FILTER_FACTORY_MOD'
MODULE FILTER_FACTORY_MOD

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE
!>
!> Public symbols (dataTypes)
PUBLIC :: MAKE_FILTER
PUBLIC :: DESTROY_FILTER

CONTAINS

!>
!> @brief Factory function for creating or initializing encoder objects.
!>
!> This function acts as a factory for creating or initializing a encoder object
!> based on the provided parameters. It assigns the proper type (`FILTER_000_T`)
!> to the `FILTER` object and configures it using the provided model parameters,
!> ID, and YAML configuration. If verbose mode is enabled, additional debug information
!> is output during the process.
!>
!> @param [inout] FILTER The encoder object that will be created or initialized.
!>                              It must be a pointer of type `GRIB_SECTION_BASE_A`.
!> @param [in] ID Integer identifier for the encoder object.
!> @param [in] PARAMS The model parameters structure of type `MODEL_PAR_T`.
!> @param [in] CFG YAML configuration object used to configure the encoder object.
!> @param [in] OPT The generic options to be used to initialize the filter.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Section that can be constructed with this factory
!>   - `FILTER_000_T`
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [TYPE] FILTER_000_MOD::FILTER_000_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see FILTER_000_T
!>
#define PP_PROCEDURE_TYPE 'RESURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_FILTER'
RECURSIVE FUNCTION MAKE_FILTER( FILTER, CTYPE, CFG, OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,              ONLY: JPIB_K
  USE :: FILTER_BASE_MOD,                ONLY: FILTER_BASE_A
  USE :: FILTER_OPTIONS_MOD,             ONLY: FILTER_OPTIONS_T
  USE :: FILTER_BASE_MOD,                ONLY: FILTER_CONTAINER_T
  USE :: FILTER_COMPOSED_MOD,            ONLY: FILTER_COMPOSED_T
  USE :: FILTER_PARAM_MOD,               ONLY: FILTER_PARAM_T
  USE :: FILTER_LEVELIST_MOD,            ONLY: FILTER_LEVELIST_T
  USE :: FILTER_CHEM_MOD,                ONLY: FILTER_CHEM_T
  USE :: FILTER_ANOFFSET_MOD,            ONLY: FILTER_ANOFFSET_T
  USE :: FILTER_WAVELENGTH_MOD,          ONLY: FILTER_WAVELENGTH_T
  USE :: FILTER_NUMBER_MOD,              ONLY: FILTER_NUMBER_T
  USE :: FILTER_STEP_MOD,                ONLY: FILTER_STEP_T
  USE :: FILTER_FREQUENCY_MOD,           ONLY: FILTER_FREQUENCY_T
  USE :: FILTER_DIRECTION_MOD,           ONLY: FILTER_DIRECTION_T
  USE :: FILTER_LEVTYPE_MOD,             ONLY: FILTER_LEVTYPE_T
  USE :: FILTER_CLASS_MOD,               ONLY: FILTER_CLASS_T
  USE :: FILTER_TYPE_MOD,                ONLY: FILTER_TYPE_T
  USE :: FILTER_STREAM_MOD,              ONLY: FILTER_STREAM_T
  USE :: FILTER_MODEL_MOD,               ONLY: FILTER_MODEL_T
  USE :: FILTER_REPRES_MOD,              ONLY: FILTER_REPRES_T
  USE :: FILTER_PACKING_MOD,             ONLY: FILTER_PACKING_T
  USE :: FILTER_IS_CHEMICAL_MOD,         ONLY: FILTER_IS_CHEMICAL_T
  USE :: FILTER_IS_AEROSOL_MOD,          ONLY: FILTER_IS_AEROSOL_T
  USE :: FILTER_IS_ENSEMBLE_MOD,         ONLY: FILTER_IS_ENSEMBLE_T
  USE :: FILTER_IS_CHEMICAL_OPTICAL_MOD, ONLY: FILTER_IS_CHEMICAL_OPTICAL_T
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_CONFIGURATIONS_T
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_GET_SUBCONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_GET_CONFIGURATIONS_SIZE
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_GET_CONFIGURATION_BY_ID
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_READ_STRING
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,            ONLY: YAML_DELETE_CONFIGURATIONS
  USE :: HOOKS_MOD,                      ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(FILTER_BASE_A), POINTER, INTENT(INOUT) :: FILTER
  CHARACTER(LEN=*),              INTENT(IN)    :: CTYPE
  TYPE(YAML_CONFIGURATION_T),    INTENT(IN)    :: CFG
  TYPE(FILTER_OPTIONS_T),        INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: N_FILTERS
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=:), ALLOCATABLE :: NESTED_CTYPE
  TYPE(FILTER_CONTAINER_T), DIMENSION(:), POINTER :: NESTED_FILTERS
  LOGICAL :: HAS_FILTERS
  LOGICAL :: HAS_TYPE
  TYPE(YAML_CONFIGURATION_T)  :: NESTED_FILTER_CONFIGURATION
  TYPE(YAML_CONFIGURATIONS_T) :: NESTED_FILTERS_CONFIGURATION

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_FILTER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INITIALIZATION_ERROR=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECTIONS_UNDEFINED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_NUMBER_OF_SECTIONS=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_FILTER_TYPE=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_NESTED_FILTERS=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_NESTED_FILTERS=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG_BY_ID=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_TYPE_CFG=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UDEFINED_TYPE=14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_TYPE_ID=15_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ=16_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE=17_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILTER_TYPE_DEALLOCATION_ERROR=18_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILTERS_TYPE_DEALLOCATION_ERROR=19_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_SIZE_OF_NESTED_FILTERS=20_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NESTED_FILTERS_NOT_ASSOCIATED=21_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_NESTED_FILTER=22_JPIB_K


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

  ! Initialize the section
  SELECT CASE( TRIM(ADJUSTL(CTYPE)) )

  CASE( 'composed' )

    ALLOCATE( FILTER_COMPOSED_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

    !> Read the encoder configuration
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'filters', HAS_FILTERS, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_FILTERS, ERRFLAG_SECTIONS_UNDEFINED )

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATIONS( CFG, 'filters', NESTED_FILTERS_CONFIGURATION, HOOKS )

    !> Get the sections size
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE) YAML_GET_CONFIGURATIONS_SIZE( NESTED_FILTERS_CONFIGURATION, N_FILTERS, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( N_FILTERS .LE. 0, ERRFLAG_WRONG_NUMBER_OF_SECTIONS )

    !> Allocate the nested filters
    SELECT TYPE(F => FILTER)
    CLASS IS (FILTER_COMPOSED_T)

      !> Allocate Filters
      PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE_NESTED_FILTERS) F%ALLOCATE_NESTED_FILTERS( N_FILTERS, HOOKS )

      !> Get Filters
      NESTED_FILTERS => NULL()
      PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NESTED_FILTERS) F%GET_NESTED_FILTERS( NESTED_FILTERS, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(NESTED_FILTERS), ERRFLAG_NESTED_FILTERS_NOT_ASSOCIATED )
      PP_DEBUG_CRITICAL_COND_THROW( SIZE(NESTED_FILTERS).NE.N_FILTERS, ERRFLAG_WRONG_SIZE_OF_NESTED_FILTERS)

    CLASS DEFAULT

      PP_DEBUG_CRITICAL_THROW( ERRFLAG_WRONG_FILTER_TYPE )

    END SELECT

    !> Loop over sections
    DO I = 1 , N_FILTERS

      !> Get section configuration by ID
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG_BY_ID) YAML_GET_CONFIGURATION_BY_ID( NESTED_FILTERS_CONFIGURATION, I, NESTED_FILTER_CONFIGURATION, HOOKS )

      !> Check if configuration has the section keyword
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_TYPE_CFG) YAML_CONFIGURATION_HAS_KEY( NESTED_FILTER_CONFIGURATION, 'type', HAS_TYPE, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_TYPE, ERRFLAG_UDEFINED_TYPE )

      !> Get the section ID
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_TYPE_ID) YAML_READ_STRING( NESTED_FILTER_CONFIGURATION, 'type', NESTED_CTYPE, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(NESTED_CTYPE), ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ )

      !> Create the nested filter
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_NESTED_FILTER) MAKE_FILTER( NESTED_FILTERS(I)%FILTER_, NESTED_CTYPE, NESTED_FILTER_CONFIGURATION, OPT, HOOKS )

      DEALLOCATE(NESTED_CTYPE, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE )

      !> Deallocate section configuration
      PP_TRYCALL( ERRFLAG_FILTER_TYPE_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( NESTED_FILTER_CONFIGURATION, HOOKS )

    ENDDO

    !> Deallocate sections
    PP_TRYCALL( ERRFLAG_FILTERS_TYPE_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATIONS( NESTED_FILTERS_CONFIGURATION, HOOKS )

  CASE( 'param' )

    ALLOCATE( FILTER_PARAM_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'levelist' )

    ALLOCATE( FILTER_LEVELIST_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'number' )

    ALLOCATE( FILTER_NUMBER_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'chem' )

    ALLOCATE( FILTER_CHEM_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'anoffset' )

    ALLOCATE( FILTER_ANOFFSET_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'wavelength' )

    ALLOCATE( FILTER_WAVELENGTH_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'step' )

    ALLOCATE( FILTER_STEP_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'frequency' )

    ALLOCATE( FILTER_FREQUENCY_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'direction' )

    ALLOCATE( FILTER_DIRECTION_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'levtype' )

    ALLOCATE( FILTER_LEVTYPE_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'class' )

    ALLOCATE( FILTER_CLASS_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'type' )

    ALLOCATE( FILTER_TYPE_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'stream' )

    ALLOCATE( FILTER_STREAM_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'repres' )

    ALLOCATE( FILTER_REPRES_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'packing' )

    ALLOCATE( FILTER_PACKING_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'model' )

    ALLOCATE( FILTER_MODEL_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'is-chemical' )

    ALLOCATE( FILTER_IS_CHEMICAL_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'is-ensemble' )

    ALLOCATE( FILTER_IS_ENSEMBLE_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'is-aerosol' )

    ALLOCATE( FILTER_IS_AEROSOL_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE( 'is-chemical-optical' )

    ALLOCATE( FILTER_IS_CHEMICAL_OPTICAL_T::FILTER, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FILTER%INIT( CFG, OPT, HOOKS )

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_FILTER )

  END SELECT

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
    CASE (ERRFLAG_UNKNOWN_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown filter type number: "'//TRIM(ADJUSTL(CTYPE))//'"' )
    CASE (ERRFLAG_ALLOCATION_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating filter number: "'//TRIM(ADJUSTL(CTYPE))//'"' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating filter number: "'//TRIM(ADJUSTL(CTYPE))//'" : "'//TRIM(ADJUSTL(ERRMSG))//'"' )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_INITIALIZATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error initializing filter: "'//TRIM(ADJUSTL(CTYPE))//'"' )
    CASE (ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read filter configuration' )
    CASE (ERRFLAG_SECTIONS_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'sections undefined' )
    CASE (ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfigurations' )
    CASE (ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get subconfigurations size' )
    CASE (ERRFLAG_WRONG_NUMBER_OF_SECTIONS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong number of sections' )
    CASE (ERRFLAG_WRONG_FILTER_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong filter type' )
    CASE (ERRFLAG_UNABLE_TO_ALLOCATE_NESTED_FILTERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate nested filters' )
    CASE (ERRFLAG_UNABLE_TO_GET_NESTED_FILTERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get nested filters' )
    CASE (ERRFLAG_WRONG_SIZE_OF_NESTED_FILTERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong size of nested filters' )
    CASE (ERRFLAG_UNABLE_TO_READ_CFG_BY_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read filter configuration by ID' )
    CASE (ERRFLAG_UNABLE_TO_READ_TYPE_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read type configuration' )
    CASE (ERRFLAG_UDEFINED_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'undefined type' )
    CASE (ERRFLAG_UNABLE_TO_READ_NESTED_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read nested filter: "'//TRIM(ADJUSTL(CTYPE))//'"' )
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

END FUNCTION MAKE_FILTER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Destroys a encoder structure.
!>
!> This function takes a encoder object (`FILTER`) and performs
!> the necessary cleanup, freeing any associated resources. It also supports a
!> `VERBOSE` mode for detailed output during the destruction process.
!>
!> @section interface
!> @param[in,out] FILTER The encoder object to be destroyed.
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
!> @see MAKE_FILTER
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DESTROY_FILTER'
RECURSIVE FUNCTION DESTROY_FILTER( FILTER, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: FILTER_BASE_MOD,     ONLY: FILTER_BASE_A
  USE :: FILTER_BASE_MOD,     ONLY: FILTER_CONTAINER_T
  USE :: FILTER_COMPOSED_MOD, ONLY: FILTER_COMPOSED_T
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(FILTER_BASE_A), POINTER, INTENT(INOUT) :: FILTER
  TYPE(HOOKS_T),                 INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  TYPE(FILTER_CONTAINER_T), DIMENSION(:), POINTER :: NESTED_FILTERS

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NESTED_DESTRUCTOR_ERROR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_ERROR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ASSOCIATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_NESTED_FILTERS=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NESTED_FILTERS_NOT_ASSOCIATED=6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(FILTER), ERRFLAG_NOT_ASSOCIATED )

  ! Deallocate filters
  SELECT TYPE(F => FILTER)
  CLASS IS (FILTER_COMPOSED_T)

    !> Get Filters
    NESTED_FILTERS => NULL()
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NESTED_FILTERS) F%GET_NESTED_FILTERS( NESTED_FILTERS, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(NESTED_FILTERS), ERRFLAG_NESTED_FILTERS_NOT_ASSOCIATED )

    DO I = 1 , SIZE(NESTED_FILTERS)
      PP_TRYCALL(ERRFLAG_NESTED_DESTRUCTOR_ERROR) DESTROY_FILTER( NESTED_FILTERS(I)%FILTER_, HOOKS )
    ENDDO

  END SELECT

  ! Deeallocate filter
  PP_TRYCALL(ERRFLAG_FREE_ERROR)  FILTER%FREE( HOOKS )
  DEALLOCATE( FILTER, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )

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
    CASE (ERRFLAG_NESTED_DESTRUCTOR_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error destructing nested filter' )
    CASE (ERRFLAG_FREE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error destructing filter' )
    CASE (ERRFLAG_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'filter to destroy is not associated' )
    CASE (ERRFLAG_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating filter' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating filter: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_UNABLE_TO_GET_NESTED_FILTERS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get nested filters' )
    CASE (ERRFLAG_NESTED_FILTERS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'nested filters are not associated' )
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

END FUNCTION DESTROY_FILTER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE FILTER_FACTORY_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
