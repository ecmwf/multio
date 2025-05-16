!>
!> @file G2S4_049_mod.F90
!>
!> @brief Module for managing GRIB2 Section 4 operations.
!>
!> The `G2S4_049_MOD` module contains procedures to initialize, allocate,
!> preset, run, and clean up the resources associated with GRIB2 Section 4 objects.
!> This module provides thread-safe operations and includes extensive use of debugging,
!> logging, and tracing capabilities, making it robust for production and testing.
!>
!> The key operations covered by this module include:
!>   - Initialization of GRIB2 Section 4 objects.
!>   - Allocation of resources.
!>   - Presetting internal parameters.
!>   - Managing runtime operations based on input parameters.
!>   - Cleaning up and deallocating resources after use.
!>
!> @section interface
!>
!> The module exports the following procedures:
!>   - @see G2S4_049_INIT
!>   - @see G2S4_049_ALLOCATE
!>   - @see G2S4_049_PRESET
!>   - @see G2S4_049_RUNTIME
!>   - @see G2S4_049_TO_BE_ENCODED
!>   - @see G2S4_049_FREE
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>   - @dependency [TYPE] OM_CORE_MOD::TIME_HISTORY_T
!>   - @dependency [TYPE] OM_CORE_MOD::CURR_TIME_T
!>
!> @subsection special dependencies
!>
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


#define PP_FILE_NAME 'grib2_section4_049_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SECTION4_049_MOD'
MODULE GRIB2_SECTION4_049_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

!> Name of the object (to be used in the register)
CHARACTER(LEN=*), PARAMETER :: G2S4_049_NAME = 'g2s4::049'

!>
!> @brief Type definition for GRIB2 Section 4 handler.
!>
!> The `G2S4_049_T` type extends the base class `GRIB_SECTION_BASE_A` and
!> provides concrete implementations of initialization, allocation, preset, runtime,
!> encoding checks, and cleanup operations for GRIB2 Section 4 objects.
!>
!> This type ensures that the required resources are properly managed through thread-safe,
!> non-overridable methods, providing robustness in both multi-threaded and single-threaded
!> environments.
!>
TYPE, EXTENDS(GRIB_SECTION_BASE_A) :: G2S4_049_T

  !> Default symbols visibility
  PRIVATE

  !> Integer section number
  INTEGER(KIND=JPIB_K) :: TEMPLATE_NUMBER_ = 41_JPIB_K

  !> Name of the sub sections
  CHARACTER(LEN=64) :: MODEL_SUBSECTION_NAME_ = REPEAT( ' ', 64 )
  CHARACTER(LEN=64) :: TIME_SUBSECTION_NAME_ = REPEAT( ' ', 64 )
  CHARACTER(LEN=64) :: LEVEL_SUBSECTION_NAME_ = REPEAT( ' ', 64 )
  CHARACTER(LEN=64) :: PARAMID_SUBSECTION_NAME_ = REPEAT( ' ', 64 )
  CHARACTER(LEN=64) :: ENSEMBLE_SUBSECTION_NAME_ = REPEAT( ' ', 64 )

  !> Type definition for GRIB2 Section 4 handler.
  CLASS(GRIB_SECTION_BASE_A), POINTER :: MODEL_ => NULL()
  CLASS(GRIB_SECTION_BASE_A), POINTER :: TIME_ => NULL()
  CLASS(GRIB_SECTION_BASE_A), POINTER :: LEVEL_ => NULL()
  CLASS(GRIB_SECTION_BASE_A), POINTER :: CHEM_ => NULL()
  CLASS(GRIB_SECTION_BASE_A), POINTER :: PARAMID_ => NULL()
  CLASS(GRIB_SECTION_BASE_A), POINTER :: ENSEMBLE_ => NULL()

CONTAINS

  !>
  !> @brief Initializes the GRIB2 Section 4 object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The procedure starts from a yaml configuration file to construct the
  !> GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT_CFG => G2S4_049_INIT_CFG

  !>
  !> @brief Initializes the GRIB2 Section 4 object.
  !>
  !> This procedure sets up the necessary parameters and prepares the
  !> object for use.
  !> The preocedure starts from a message and fro the parameters to construct
  !> the GRIB2 encoder.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: INIT_LAZY => G2S4_049_INIT_LAZY

  !>
  !> @brief Allocates resources for the GRIB2 Section 4 object.
  !>
  !> This procedure allocates memory and other necessary resources for
  !> the object based on provided parameters.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: ALLOCATE => G2S4_049_ALLOCATE

  !>
  !> @brief Presets the parameters of the GRIB2 Section 4 object.
  !>
  !> This procedure configures the internal parameters of the object
  !> before runtime execution.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRESET => G2S4_049_PRESET

  !>
  !> @brief Manages the runtime execution of GRIB2 Section 4 operations.
  !>
  !> This procedure handles operations and computations during runtime,
  !> making use of time and metadata information.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: RUNTIME => G2S4_049_RUNTIME

  !>
  !> @brief Determines if the GRIB2 Section 4 object needs to be encoded.
  !>
  !> This procedure checks whether the object should be encoded based
  !> on the provided parameters and internal state.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: TO_BE_ENCODED => G2S4_049_TO_BE_ENCODED

  !>
  !> @brief Frees resources allocated for the GRIB2 Section 4 object.
  !>
  !> This procedure deallocates resources and performs cleanup after
  !> the object has been used.
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: FREE => G2S4_049_FREE

  !>
  !> @brief Print informations related to the section
  !>
  !> This procedure print informatin about the section and eventually call
  !> the print method of the nested sub-sections
  !>
  PROCEDURE, PUBLIC, PASS, NON_OVERRIDABLE :: PRINT => G2S4_049_PRINT



  !>
  !> @brief Build the time configurator object from yaml configuration
  !>
  !> This procedure allocates the proper time configurator
  !>
  PROCEDURE, PRIVATE, PASS :: BUILD_TIME_CONFIGURATOR_CFG => G2S4_049_BUILD_TIME_HANDLER_CFG


  !>
  !> @brief Build the model configurator object from yaml configuration
  !>
  !> This procedure allocates the proper model configurator
  !>
  PROCEDURE, PRIVATE, PASS :: BUILD_MODEL_CONFIGURATOR_CFG => G2S4_049_BUILD_MODEL_HANDLER_CFG

  !>
  !> @brief Build the param configurator object from yaml configuration
  !>
  !> This procedure allocates the proper param configurator
  !>
  PROCEDURE, PRIVATE, PASS :: BUILD_PARAM_CONFIGURATOR_CFG => G2S4_049_BUILD_PARAM_HANDLER_CFG

  !>
  !> @brief Build the level configurator object from yaml configuration
  !>
  !> This procedure allocates the proper level configurator
  !>
  PROCEDURE, PRIVATE, PASS :: BUILD_LEVEL_CONFIGURATOR_CFG => G2S4_049_BUILD_LEVEL_HANDLER_CFG

  !>
  !> @brief Build the ensemble configurator object from yaml configuration
  !>
  !> This procedure allocates the proper ensemble configurator
  !>
  PROCEDURE, PRIVATE, PASS :: BUILD_ENSEMBLE_CONFIGURATOR_CFG => G2S4_049_BUILD_ENSEMBLE_HANDLER_CFG

  PROCEDURE, PRIVATE, PASS :: BUILD_CHEM_CONFIGURATOR_CFG => GRIB2_SECTION4_049_BUILD_CHEM_HANDLER


  !>
  !> @brief Build the param configurator object from rules
  !>
  !> This procedure allocates the proper param configurator
  !>
  PROCEDURE, PRIVATE, PASS :: BUILD_PARAM_CONFIGURATOR_LAZY => G2S4_049_BUILD_PARAM_HANDLER_LAZY

  !>
  !> @brief Build the level configurator object from rules
  !>
  !> This procedure allocates the proper level configurator
  !>
  PROCEDURE, PRIVATE, PASS :: BUILD_LEVEL_CONFIGURATOR_LAZY => G2S4_049_BUILD_LEVEL_HANDLER_LAZY

  !>
  !> @brief Build the time configurator object from rules
  !>
  !> This procedure allocates the proper time configurator
  !>
  PROCEDURE, PRIVATE, PASS :: BUILD_TIME_CONFIGURATOR_LAZY => G2S4_049_BUILD_TIME_HANDLER_LAZY

  !>
  !> @brief Build the model configurator object from rules
  !>
  !> This procedure allocates the proper model configurator
  !>
  PROCEDURE, PRIVATE, PASS :: BUILD_MODEL_CONFIGURATOR_LAZY => G2S4_049_BUILD_MODEL_HANDLER_LAZY

  !>
  !> @brief Build the ensemble configurator object from rules
  !>
  !> This procedure allocates the proper ensemble configurator
  !>
  PROCEDURE, PRIVATE, PASS :: BUILD_ENSEMBLE_CONFIGURATOR_LAZY => G2S4_049_BUILD_ENSEMBLE_HANDLER_LAZY

END TYPE


!>
!> Public symbols (procedures)
PUBLIC :: G2S4_049_MAKE
PUBLIC :: G2S4_049_REGISTER
PUBLIC :: G2S4_049_DESTROY

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_REGISTER'
PP_THREAD_SAFE FUNCTION G2S4_049_REGISTER( IDX ) BIND(C, NAME='g2s4_049_register') RESULT(RET)

  !> Symbols imported from intrinsics modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,         ONLY: JPIB_K
  USE :: GRIB_SECTION_BASE_MOD,     ONLY: GRIB_SECTION_BASE_A
  USE :: GRIB_SECTION_BASE_MOD,     ONLY: SECTION_ALLOCATOR_IF
  USE :: GRIB_SECTION_BASE_MOD,     ONLY: SECTION_DEALLOCATOR_IF
  USE :: GRIB_ENCODER_REGISTER_MOD, ONLY: GRIB_SECTION_REGISTER_T
  USE :: HOOKS_MOD,                 ONLY: HOOKS_T

  USE :: GRIB_ENCODER_TUNNELING_MOD, ONLY: GET_TUNNELING_INFO
  USE :: GRIB_ENCODER_TUNNELING_MOD, ONLY: GET_SECTIONS_REGISTER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=INT64), INTENT(INOUT) :: IDX

  !> Function result
  INTEGER(KIND=INT64) :: RET

  !> Local variables
  TYPE(GRIB_SECTION_REGISTER_T), POINTER :: REG
  TYPE(HOOKS_T), POINTER :: HOOKS
  INTEGER(KIND=JPIB_K) :: LOC_IDX
  PROCEDURE(SECTION_ALLOCATOR_IF), POINTER   :: FA
  PROCEDURE(SECTION_DEALLOCATOR_IF), POINTER :: FD

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_REGISTER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SECTION_REGISTER=2_JPIB_K

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

!$omp single
  ! Obtain the section register and the hooks
  LOC_IDX = INT(IDX,KIND=JPIB_K)

  ! Get the hooks data structure
  HOOKS => NULL()
  CALL GET_TUNNELING_INFO( LOC_IDX, HOOKS )

  ! Only if hooks is associated then proceed
  IF ( ASSOCIATED(HOOKS) ) THEN

    ! Get the section register
     REG => NULL()
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SECTION_REGISTER) GET_SECTIONS_REGISTER( REG, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(REG), ERRFLAG_UNABLE_TO_GET_SECTION_REGISTER)

    ! Associate the allocator
    FA => G2S4_049_MAKE
    FD => G2S4_049_DESTROY

    ! Register the section (since the register is shared, constructors needs to be registered only once)
    PP_TRYCALL(ERRFLAG_UNABLE_TO_REGISTER) REG%REGISTER( G2S4_049_NAME, FA, FD, HOOKS )

  ENDIF
!omp end single

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
    CASE ( ERRFLAG_UNABLE_TO_REGISTER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to register section: '//TRIM(ADJUSTL(G2S4_049_NAME)) )
    CASE ( ERRFLAG_UNABLE_TO_GET_SECTION_REGISTER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get section register' )
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

END FUNCTION G2S4_049_REGISTER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_MAKE'
PP_THREAD_SAFE FUNCTION G2S4_049_MAKE( GRIB_SECTION, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A), POINTER, INTENT(INOUT) :: GRIB_SECTION
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=2_JPIB_K

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

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(GRIB_SECTION), ERRFLAG_ALREADY_ASSOCIATED )

  ! Allocate concrete class
!$omp critical(G2S4_ALLOCATE)
  ALLOCATE( G2S4_049_T::GRIB_SECTION, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )
!$omp end critical(G2S4_ALLOCATE)

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

    ! Handle different e
    SELECT CASE(ERRIDX)
    CASE ( ERRFLAG_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'already associated' )
    CASE ( ERRFLAG_ALLOCATION_ERROR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'allocation error' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=ALLOC_STATUS)
      END IF
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

END FUNCTION G2S4_049_MAKE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_DESTROY'
PP_THREAD_SAFE FUNCTION G2S4_049_DESTROY( GRIB_SECTION, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(GRIB_SECTION_BASE_A), POINTER, INTENT(INOUT) :: GRIB_SECTION
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR=2_JPIB_K

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

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(GRIB_SECTION), ERRFLAG_ALREADY_ASSOCIATED )

  ! Allocate concrete class

!$omp critical(G2S4_ALLOCATE)
  DEALLOCATE( GRIB_SECTION, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )
!$omp end critical(G2S4_ALLOCATE)

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
    CASE ( ERRFLAG_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'not associated' )
    CASE ( ERRFLAG_DEALLOCATION_ERROR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'deallocation error' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error message: ' // TRIM(ERRMSG) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STATUS)
      END IF
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

END FUNCTION G2S4_049_DESTROY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Initializes GRIB2 Section 4 for a given object using the provided parameters.
!>
!> This function initializes a GRIB2 Section 4 object (`THIS`) using the provided model parameters (`PARAMS`)
!> and configuration data (`CFG`). The process can be run in verbose mode if specified. The function
!> is thread-safe and returns an error code indicating the success or failure of the operation.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `G2S4_049_T` representing the GRIB section being initialized.
!>   @param [in]    CFG   The YAML configuration object of type `YAML_CONFIGURATION_T`.
!>   @param [in]    OPT   The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] HOOKS A structure of type `HOOKS_T` that contains hooks for initialization.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>
!> @susection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see G2S4_049_INIT
!> @see G2S4_049_ALLOCATE
!> @see G2S4_049_PRESET
!> @see G2S4_049_RUNTIME
!> @see G2S4_049_TO_BE_ENCODED
!> @see G2S4_049_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_INIT_CFG'
PP_THREAD_SAFE FUNCTION G2S4_049_INIT_CFG( THIS, &
&               CFG, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_TIME_HANDLER=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_PARAM_HANDLER=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_LEVEL_HANDLER=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_ENSEMBLE_HANDLER=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_CHEM_HANDLER=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INIT_MODEL_HANDLER=6_JPIB_K

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

  ! Initialise the section
  THIS%TEMPLATE_NUMBER_ = 41_JPIB_K
  THIS%TYPE_ = 'SECTION'
  THIS%SUBTYPE_ = 'PRODUCT_DEFINITION_SECTION'
  THIS%KIND_   = '4.41'

  ! Initialize tha time handler
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_TIME_HANDLER)  THIS%BUILD_TIME_CONFIGURATOR_CFG( CFG, OPT, FACTORIES, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_PARAM_HANDLER) THIS%BUILD_PARAM_CONFIGURATOR_CFG( CFG, OPT, FACTORIES, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_LEVEL_HANDLER) THIS%BUILD_LEVEL_CONFIGURATOR_CFG( CFG, OPT, FACTORIES, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_ENSEMBLE_HANDLER) THIS%BUILD_ENSEMBLE_CONFIGURATOR_CFG( CFG, OPT, FACTORIES, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_CHEM_HANDLER)  THIS%BUILD_CHEM_CONFIGURATOR_CFG( CFG, OPT, FACTORIES, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_INIT_MODEL_HANDLER)  THIS%BUILD_MODEL_CONFIGURATOR_CFG( CFG, OPT, FACTORIES, HOOKS )

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
    CASE ( ERRFLAG_UNABLE_TO_INIT_TIME_HANDLER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to initialize time handler' )
    CASE ( ERRFLAG_UNABLE_TO_INIT_MODEL_HANDLER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to initialize model handler' )
    CASE ( ERRFLAG_UNABLE_TO_INIT_PARAM_HANDLER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to init param handler' )
    CASE ( ERRFLAG_UNABLE_TO_INIT_LEVEL_HANDLER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to initialize level handler' )
    CASE ( ERRFLAG_UNABLE_TO_INIT_ENSEMBLE_HANDLER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to initialize ensemble handler' )
    CASE ( ERRFLAG_UNABLE_TO_INIT_CHEM_HANDLER )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to initialize chem handler' )
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

END FUNCTION G2S4_049_INIT_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Initializes GRIB2 Section 4 for a given object using the provided parameters.
!>
!> This function initializes a GRIB2 Section 4 object (`THIS`) using the provided model parameters (`PARAMS`)
!> and configuration data (`CFG`). The process can be run in verbose mode if specified. The function
!> is thread-safe and returns an error code indicating the success or failure of the operation.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `G2S4_049_T` representing the GRIB section being initialized.
!>   @param [in]    MSG   All the mars keywords needed to describe the field `FORTRAN_MESSAGE_T`.
!>   @param [in]    PAR   All information outside mars keywords needed to describe the field `PARAMETRIZATION_T`.
!>   @param [in]    OPT   The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] HOOKS A structure of type `HOOKS_T` that contains hooks for initialization.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>
!> @susection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see G2S4_049_INIT
!> @see G2S4_049_ALLOCATE
!> @see G2S4_049_PRESET
!> @see G2S4_049_RUNTIME
!> @see G2S4_049_TO_BE_ENCODED
!> @see G2S4_049_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_INIT_LAZY'
PP_THREAD_SAFE FUNCTION G2S4_049_INIT_LAZY( THIS, &
&               MSG, PAR, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,                        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,                 ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,                      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,                      ONLY: PARAMETRIZATION_T
  USE :: GRIB_SECTION_BASE_MOD,                    ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_TIME_CONFIGURATOR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_PARAM_CONFIGURATOR=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_LEVEL_CONFIGURATOR=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_ENSEMBLE_CONFIGURATOR=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_CHEM_CONFIGURATOR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_BUILD_MODEL_CONFIGURATOR=6_JPIB_K

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

  ! Initialise the section
  THIS%TEMPLATE_NUMBER_ = 41_JPIB_K
  THIS%TYPE_ = 'SECTION'
  THIS%SUBTYPE_ = 'PRODUCT_DEFINITION_SECTION'
  THIS%KIND_   = '4.41'

  ! Build the param configurator
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_TIME_CONFIGURATOR) FACTORIES%MAKE_GRIB2_POINT_IN_TIME_CONFIGURATOR_LAZY( &
&  THIS%TIME_, THIS%TEMPLATE_NUMBER_, MSG, PAR, OPT, FACTORIES, HOOKS )

  ! Build the param configurator
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_PARAM_CONFIGURATOR) FACTORIES%MAKE_GRIB2_PARAM_CONFIGURATOR_LAZY( &
&  THIS%PARAMID_, THIS%TEMPLATE_NUMBER_, MSG, PAR, OPT, FACTORIES, HOOKS )

  ! Build the level configurator
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_LEVEL_CONFIGURATOR) FACTORIES%MAKE_GRIB2_LEVEL_CONFIGURATOR_LAZY( &
&  THIS%LEVEL_, THIS%TEMPLATE_NUMBER_, MSG, PAR, OPT, FACTORIES, HOOKS )

  ! Build the level configurator
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_ENSEMBLE_CONFIGURATOR) FACTORIES%MAKE_GRIB2_ENSEMBLE_CONFIGURATOR_LAZY( &
&  THIS%LEVEL_, THIS%TEMPLATE_NUMBER_, MSG, PAR, OPT, FACTORIES, HOOKS )

  ! Build the level configurator
  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_CHEM_CONFIGURATOR) FACTORIES%MAKE_GRIB2_CHEM_CONFIGURATOR_LAZY( &
&  THIS%CHEM_, THIS%TEMPLATE_NUMBER_, MSG, PAR, OPT, FACTORIES, HOOKS )


  PP_TRYCALL(ERRFLAG_UNABLE_TO_BUILD_MODEL_CONFIGURATOR) FACTORIES%MAKE_GRIB2_MODEL_CONFIGURATOR_LAZY( &
&  THIS%MODEL_, THIS%TEMPLATE_NUMBER_, MSG, PAR, OPT, FACTORIES, HOOKS )

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
    CASE ( ERRFLAG_UNABLE_TO_BUILD_TIME_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to initialize time handler' )
    CASE ( ERRFLAG_UNABLE_TO_BUILD_PARAM_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build param configurator' )
    CASE ( ERRFLAG_UNABLE_TO_BUILD_LEVEL_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build level configurator' )
    CASE ( ERRFLAG_UNABLE_TO_BUILD_ENSEMBLE_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build ensemble configurator' )
    CASE ( ERRFLAG_UNABLE_TO_BUILD_CHEM_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build chem configurator' )
    CASE ( ERRFLAG_UNABLE_TO_BUILD_MODEL_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build model configurator' )
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

END FUNCTION G2S4_049_INIT_LAZY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Allocates resources for GRIB2 Section 4 using the provided parameters.
!>
!> This function allocates resources for a GRIB2 Section 4 object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the allocation process.
!>
!> @section interface
!>   @param [in]    THIS     An object of type `G2S4_049_T` representing the GRIB section to allocate resources for.
!>   @param [in]    MSG      All the mars keywords needed to describe the field `FORTRAN_MESSAGE_T`.
!>   @param [in]    PAR      All information outside mars keywords needed to describe the field `PARAMETRIZATION_T`.
!>   @param [in]    OPT      The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] METADATA A pointer to the metadata object of type `METADATA_BASE_A` used during allocation.
!>   @param [inout] HOOKS    A structure of type `HOOKS_T` that contains hooks for the allocation process.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see G2S4_049_ALLOCATE
!> @see G2S4_049_INIT
!> @see G2S4_049_PRESET
!> @see G2S4_049_RUNTIME
!> @see G2S4_049_TO_BE_ENCODED
!> @see G2S4_049_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_ALLOCATE'
PP_THREAD_SAFE FUNCTION G2S4_049_ALLOCATE( THIS, &
&  MSG, PAR, OPT,  METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),               INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_TIME=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_LEVEL=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_PARAMID=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_ENSEMBLE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_CHEM=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_MODEL=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED=13_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA, ERRFLAG_METADATA )

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(METADATA), ERRFLAG_METADATA )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%TIME_), ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%LEVEL_), ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%CHEM_), ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%PARAMID_), ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%ENSEMBLE_), ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%MODEL_), ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED )

  ! Enable section 4
  PP_METADATA_SET( METADATA, ERRFLAG_METADATA, 'productDefinitionTemplateNumber', THIS%TEMPLATE_NUMBER_ )

  ! Allocate time, level and paramId subcomponents of the section
  PP_TRYCALL(ERRFLAG_ALLOCATE_TIME) THIS%TIME_%ALLOCATE( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_ALLOCATE_LEVEL) THIS%LEVEL_%ALLOCATE( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_ALLOCATE_CHEM) THIS%CHEM_%ALLOCATE( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_ALLOCATE_PARAMID) THIS%PARAMID_%ALLOCATE( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_ALLOCATE_ENSEMBLE) THIS%ENSEMBLE_%ALLOCATE( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_ALLOCATE_MODEL) THIS%MODEL_%ALLOCATE( MSG, PAR, OPT, METADATA, HOOKS )

  ! Allocate time, level and paramId subcomponents of the section

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA, ERRFLAG_METADATA )
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
    CASE ( ERRFLAG_ALLOCATE_TIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate time' )
    CASE ( ERRFLAG_ALLOCATE_LEVEL )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate level' )
    CASE ( ERRFLAG_ALLOCATE_CHEM )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate chemistry' )
    CASE ( ERRFLAG_ALLOCATE_MODEL )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate model' )
    CASE ( ERRFLAG_ALLOCATE_PARAMID )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate paramId' )
    CASE ( ERRFLAG_ALLOCATE_ENSEMBLE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate ensemble' )
    CASE ( ERRFLAG_METADATA )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error using metadata' )
    CASE ( ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'time configurator not associated' )
    CASE ( ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'level configurator not associated' )
    CASE ( ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'paramId configurator not associated' )
    CASE ( ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'chemistry configurator not associated' )
    CASE ( ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'model configurator not associated' )
    CASE ( ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'ensemble configurator not associated' )
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

END FUNCTION G2S4_049_ALLOCATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Presets GRIB2 Section 4 using the provided parameters and message data.
!>
!> This function presets a GRIB2 Section 4 object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), and metadata (`METADATA`). The process can be run in verbose mode if specified.
!> The function is thread-safe and returns an error code indicating the success or failure of the preset operation.
!>
!> @section interface
!>   @param [in]    THIS     An object of type `G2S4_049_T` representing the GRIB section to be preset.
!>   @param [in]    MSG      The message object of type `FORTRAN_MESSAGE_T` used to handle preset-related messaging.
!>   @param [in]    PAR      The parametrization structure of type `PARAMETRIZATION_T` used for the preset operation.
!>   @param [in]    OPT      The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] METADATA A pointer to the metadata object of type `METADATA_BASE_A` used for presetting the section.
!>   @param [inout] HOOKS    A structure of type `HOOKS_T` that contains hooks for the preset operation.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see G2S4_049_PRESET
!> @see G2S4_049_ALLOCATE
!> @see G2S4_049_INIT
!> @see G2S4_049_RUNTIME
!> @see G2S4_049_TO_BE_ENCODED
!> @see G2S4_049_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_PRESET'
PP_THREAD_SAFE FUNCTION G2S4_049_PRESET( THIS, &
&  MSG, PAR, OPT,  METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),               INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PRESET_TIME=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PRESET_LEVEL=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PRESET_PARAMID=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PRESET_ENSEMBLE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PRESET_CHEM=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PRESET_MODEL=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED=13_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA, ERRFLAG_METADATA )


  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(METADATA), ERRFLAG_METADATA )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%LEVEL_), ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%PARAMID_), ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%CHEM_), ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%ENSEMBLE_), ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%TIME_), ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%MODEL_), ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED )

  ! Allocate time, level and paramId subcomponents of the section
  PP_TRYCALL(ERRFLAG_PRESET_TIME) THIS%TIME_%PRESET( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_PRESET_LEVEL) THIS%LEVEL_%PRESET( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_PRESET_CHEM) THIS%CHEM_%PRESET( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_PRESET_MODEL) THIS%MODEL_%PRESET( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_PRESET_PARAMID) THIS%PARAMID_%PRESET( MSG, PAR, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_PRESET_ENSEMBLE) THIS%ENSEMBLE_%PRESET( MSG, PAR, OPT, METADATA, HOOKS )

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA, ERRFLAG_METADATA )
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
    CASE ( ERRFLAG_PRESET_TIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to preset time' )
    CASE ( ERRFLAG_PRESET_LEVEL )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to preset level' )
    CASE ( ERRFLAG_PRESET_CHEM )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to preset chemistry' )
    CASE ( ERRFLAG_PRESET_MODEL )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to preset model' )
    CASE ( ERRFLAG_PRESET_PARAMID )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to preset paramId' )
    CASE ( ERRFLAG_PRESET_ENSEMBLE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to preset ensemble' )
    CASE ( ERRFLAG_METADATA )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error using metadata' )
    CASE ( ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'time configurator not associated' )
    CASE ( ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'level configurator not associated' )
    CASE ( ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'chemistry configurator not associated' )
    CASE ( ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'model configurator not associated' )
    CASE ( ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'paramId configurator not associated' )
    CASE ( ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'ensemble configurator not associated' )
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

END FUNCTION G2S4_049_PRESET
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Executes runtime processing for GRIB2 Section 4 using provided parameters, message data, and time history.
!>
!> This function performs runtime operations for a GRIB2 Section 4 object (`THIS`) using the provided model parameters (`PARAMS`),
!> message structure (`MSG`), current time (`CURR_TIME`), time history (`TIME_HISTORY`), and metadata (`METADATA`).
!> The process can be run in verbose mode if specified. The function is thread-safe and returns an error code indicating
!> the success or failure of the runtime operation.
!>
!> @section interface
!>   @param [in]    THIS      An object of type `G2S4_049_T` representing the GRIB section for runtime execution.
!>   @param [in]    MSG       The message object of type `FORTRAN_MESSAGE_T` used to handle preset-related messaging.
!>   @param [in]    PAR       The parametrization structure of type `PARAMETRIZATION_T` used for the preset operation.
!>   @param [in]    TIME_HIST The time history object of type `TIME_HISTORY_T` providing historical time data.
!>   @param [in]    CURR_TIME The current time object of type `CURR_TIME_T` for the runtime phase.
!>   @param [in]    OPT       The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] METADATA  A pointer to the metadata object of type `METADATA_BASE_A` used during runtime.
!>   @param [inout] HOOKS     A structure of type `HOOKS_T` that contains hooks for runtime operations.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] METADATA_BASE_MOD::METADATA_BASE_A
!>   - @dependency [TYPE] OM_CORE_MOD::TIME_HISTORY_T
!>   - @dependency [TYPE] OM_CORE_MOD::CURR_TIME_T
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see G2S4_049_RUNTIME
!> @see G2S4_049_ALLOCATE
!> @see G2S4_049_INIT
!> @see G2S4_049_PRESET
!> @see G2S4_049_TO_BE_ENCODED
!> @see G2S4_049_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_RUNTIME'
PP_THREAD_SAFE FUNCTION G2S4_049_RUNTIME( THIS, &
&  MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: TIME_UTILS_MOD,           ONLY: TIME_HISTORY_T
  USE :: TIME_UTILS_MOD,           ONLY: CURR_TIME_T
  USE :: METADATA_BASE_MOD,        ONLY: METADATA_BASE_A
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),               INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),         INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),         INTENT(IN)    :: PAR
  TYPE(TIME_HISTORY_T),            INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),               INTENT(IN)    :: CURR_TIME
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_METADATA=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RUNTIME_TIME=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RUNTIME_LEVEL=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RUNTIME_CHEM=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RUNTIME_PARAMID=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RUNTIME_ENSEMBLE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RUNTIME_MODEL=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED=13_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA, ERRFLAG_METADATA )

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(METADATA), ERRFLAG_METADATA )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%LEVEL_), ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%CHEM_), ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%MODEL_), ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%PARAMID_), ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%ENSEMBLE_), ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%TIME_), ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED )

  ! Allocate time, level and paramId subcomponents of the section
  PP_TRYCALL(ERRFLAG_RUNTIME_TIME) THIS%TIME_%RUNTIME( MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_RUNTIME_LEVEL) THIS%LEVEL_%RUNTIME( MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_RUNTIME_CHEM) THIS%CHEM_%RUNTIME( MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_RUNTIME_MODEL) THIS%MODEL_%RUNTIME( MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_RUNTIME_PARAMID) THIS%PARAMID_%RUNTIME( MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )
  PP_TRYCALL(ERRFLAG_RUNTIME_ENSEMBLE) THIS%ENSEMBLE_%RUNTIME( MSG, PAR, TIME_HIST, CURR_TIME, OPT, METADATA, HOOKS )


  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA, ERRFLAG_METADATA )
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
    CASE ( ERRFLAG_RUNTIME_TIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to runtime time' )
    CASE ( ERRFLAG_RUNTIME_LEVEL )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to runtime level' )
    CASE ( ERRFLAG_RUNTIME_CHEM )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to runtime chemistry' )
    CASE ( ERRFLAG_RUNTIME_MODEL )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to runtime model' )
    CASE ( ERRFLAG_RUNTIME_PARAMID )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to runtime paramId' )
    CASE ( ERRFLAG_RUNTIME_ENSEMBLE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to runtime ensemble' )
    CASE ( ERRFLAG_METADATA )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error using metadata' )
    CASE ( ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'time configurator not associated' )
    CASE ( ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'level configurator not associated' )
    CASE ( ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'chemistry configurator not associated' )
    CASE ( ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'model configurator not associated' )
    CASE ( ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'paramId configurator not associated' )
    CASE ( ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'ensemble configurator not associated' )
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

END FUNCTION G2S4_049_RUNTIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Prepares GRIB2 Section 4 for encoding based on provided parameters, message data, and time history.
!>
!> This function determines whether GRIB2 Section 4 (`THIS`) is ready to be encoded. It processes the provided model parameters
!> (`PARAMS`), message structure (`MSG`), current time (`CURR_TIME`), time history (`TIME_HISTORY`), and updates the
!> `TO_BE_ENCODED` flag accordingly. The function is thread-safe and returns an error code indicating the success or failure
!> of the operation. The process can also be run in verbose mode if specified.
!>
!> @section interface
!>   @param [inout] THIS          An object of type `G2S4_049_T` representing the GRIB section being checked.
!>   @param [in]    MSG           The message object of type `FORTRAN_MESSAGE_T` used to handle preset-related messaging.
!>   @param [in]    PAR           The parametrization structure of type `PARAMETRIZATION_T` used for the preset operation.
!>   @param [in]    TIME_HIST     The time history object of type `TIME_HISTORY_T` providing historical time data.
!>   @param [in]    CURR_TIME     The current time object of type `CURR_TIME_T` for time-based encoding decisions.
!>   @param [in]    OPT           The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [out]   TO_BE_ENCODED Logical flag indicating if the GRIB section should be encoded.
!>   @param [inout] HOOKS         A structure of type `HOOKS_T` that contains hooks for managing encoding-related operations.
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MESSAGE_T
!>   - @dependency [TYPE] OM_CORE_MOD::TIME_HISTORY_T
!>   - @dependency [TYPE] OM_CORE_MOD::CURR_TIME_T
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see G2S4_049_TO_BE_ENCODED
!> @see G2S4_049_INIT
!> @see G2S4_049_ALLOCATE
!> @see G2S4_049_PRESET
!> @see G2S4_049_RUNTIME
!> @see G2S4_049_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_TO_BE_ENCODED'
PP_THREAD_SAFE FUNCTION G2S4_049_TO_BE_ENCODED( THIS, &
&  MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: TIME_UTILS_MOD,           ONLY: TIME_HISTORY_T
  USE :: TIME_UTILS_MOD,           ONLY: CURR_TIME_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(TIME_HISTORY_T),         INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),            INTENT(IN)    :: CURR_TIME
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  LOGICAL,                      INTENT(OUT)   :: TO_BE_ENCODED
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL, DIMENSION(6) :: TO_BE_ENCODED_SUBS

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TO_BE_ENCODED_TIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TO_BE_ENCODED_LEVEL=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TO_BE_ENCODED_CHEM=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TO_BE_ENCODED_MODEL=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TO_BE_ENCODED_PARAMID=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TO_BE_ENCODED_ENSEMBLE=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED=12_JPIB_K

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

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%LEVEL_), ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%CHEM_), ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%MODEL_), ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%PARAMID_), ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%ENSEMBLE_), ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%TIME_), ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED )

  ! Check if the field needs to be encoded, time, level and paramId subcomponents
  PP_TRYCALL(ERRFLAG_TO_BE_ENCODED_TIME) THIS%TIME_%TO_BE_ENCODED( MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED_SUBS(1), HOOKS )
  PP_TRYCALL(ERRFLAG_TO_BE_ENCODED_LEVEL) THIS%LEVEL_%TO_BE_ENCODED( MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED_SUBS(2), HOOKS )
  PP_TRYCALL(ERRFLAG_TO_BE_ENCODED_CHEM) THIS%CHEM_%TO_BE_ENCODED( MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED_SUBS(3), HOOKS )
  PP_TRYCALL(ERRFLAG_TO_BE_ENCODED_MODEL) THIS%MODEL_%TO_BE_ENCODED( MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED_SUBS(4), HOOKS )
  PP_TRYCALL(ERRFLAG_TO_BE_ENCODED_PARAMID) THIS%PARAMID_%TO_BE_ENCODED( MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED_SUBS(5), HOOKS )
  PP_TRYCALL(ERRFLAG_TO_BE_ENCODED_ENSEMBLE) THIS%ENSEMBLE_%TO_BE_ENCODED( MSG, PAR, TIME_HIST, CURR_TIME, OPT, TO_BE_ENCODED_SUBS(6), HOOKS )

  ! Check if all subcomponents are ready to be encoded
  TO_BE_ENCODED = ALL( TO_BE_ENCODED_SUBS )

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
    CASE ( ERRFLAG_TO_BE_ENCODED_TIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to check if time is ready to be encoded' )
    CASE ( ERRFLAG_TO_BE_ENCODED_MODEL )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to check if model is ready to be encoded' )
    CASE ( ERRFLAG_TO_BE_ENCODED_LEVEL )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to check if level is ready to be encoded' )
    CASE ( ERRFLAG_TO_BE_ENCODED_CHEM )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to check if chem is ready to be encoded' )
    CASE ( ERRFLAG_TO_BE_ENCODED_PARAMID )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to check if paramId is ready to be encoded' )
    CASE ( ERRFLAG_TO_BE_ENCODED_ENSEMBLE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to check if ensemble is ready to be encoded' )
    CASE ( ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'time configurator not associated' )
    CASE ( ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'model configurator not associated' )
    CASE ( ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'level configurator not associated' )
    CASE ( ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'chemistry configurator not associated' )
    CASE ( ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'paramId configurator not associated' )
    CASE ( ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'ensemble configurator not associated' )
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

END FUNCTION G2S4_049_TO_BE_ENCODED
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Frees resources associated with GRIB2 Section 4 object.
!>
!> This function deallocates and cleans up resources associated with the GRIB2 Section 4 object (`THIS`).
!> The process can be run in verbose mode for additional output. The function is thread-safe and returns an
!> error code indicating the success or failure of the operation.
!>
!> @section interface
!>   @param [inout] THIS  An object of type `G2S4_049_T` representing the GRIB section to be freed.
!>   @param [in]    OPT   The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!> @section dependencies
!>
!> @subsection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see G2S4_049_INIT
!> @see G2S4_049_ALLOCATE
!> @see G2S4_049_PRESET
!> @see G2S4_049_RUNTIME
!> @see G2S4_049_TO_BE_ENCODED
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_FREE'
PP_THREAD_SAFE FUNCTION G2S4_049_FREE( THIS, OPT, DESTRUCTORS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_DESTRUCTOR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),               INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T),    INTENT(IN)    :: OPT
  TYPE(GRIB_SECTION_DESTRUCTOR_T), INTENT(IN)    :: DESTRUCTORS
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DESTROY_INSTANT=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DESTROY_PARAM=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DESTROY_MODEL=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DESTROY_LEVEL=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DESTROY_CHEM=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DESTROY_ENSEMBLE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED=12_JPIB_K

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

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%LEVEL_), ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%CHEM_), ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%PARAMID_), ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%ENSEMBLE_), ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%TIME_), ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%MODEL_), ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED )

  ! Free all the memory
  PP_TRYCALL(ERRFLAG_DESTROY_INSTANT)  DESTRUCTORS%DESTROY_GRIB2_POINT_IN_TIME_CONFIGURATOR( THIS%TIME_, OPT, DESTRUCTORS, HOOKS )
  PP_TRYCALL(ERRFLAG_DESTROY_LEVEL)    DESTRUCTORS%DESTROY_GRIB2_LEVEL_CONFIGURATOR(    THIS%LEVEL_,    OPT, DESTRUCTORS, HOOKS )
  PP_TRYCALL(ERRFLAG_DESTROY_CHEM)     DESTRUCTORS%DESTROY_GRIB2_CHEM_CONFIGURATOR( THIS%CHEM_, OPT, DESTRUCTORS, HOOKS )
  PP_TRYCALL(ERRFLAG_DESTROY_PARAM)    DESTRUCTORS%DESTROY_GRIB2_PARAM_CONFIGURATOR(    THIS%PARAMID_,  OPT, DESTRUCTORS, HOOKS )
  PP_TRYCALL(ERRFLAG_DESTROY_ENSEMBLE) DESTRUCTORS%DESTROY_GRIB2_ENSEMBLE_CONFIGURATOR( THIS%ENSEMBLE_, OPT, DESTRUCTORS, HOOKS )
  PP_TRYCALL(ERRFLAG_DESTROY_MODEL)    DESTRUCTORS%DESTROY_GRIB2_MODEL_CONFIGURATOR( THIS%MODEL_, OPT, DESTRUCTORS, HOOKS )

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
    CASE ( ERRFLAG_DESTROY_INSTANT )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free instant' )
    CASE ( ERRFLAG_DESTROY_PARAM )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free paramId' )
    CASE ( ERRFLAG_DESTROY_MODEL )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free model' )
    CASE ( ERRFLAG_DESTROY_LEVEL )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free level' )
    CASE ( ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'chem configurator not associated' )
    CASE ( ERRFLAG_DESTROY_ENSEMBLE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free ensemble' )
    CASE ( ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'time configurator not associated' )
    CASE ( ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'level configurator not associated' )
    CASE ( ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'paramId configurator not associated' )
    CASE ( ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'ensemble configurator not associated' )
    CASE ( ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'model configurator not associated' )
    CASE ( ERRFLAG_DESTROY_CHEM )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to free chemistry' )
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

END FUNCTION G2S4_049_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE






!>
!> @brief Print informations related to the grib section
!>
!> @section interface
!>   @param [inout] THIS   An object of type `G2S4_049_T` representing the GRIB section to be freed.
!>   @param [in]    UNIT   The unit number to print the information.
!>   @param [in]    OFFSET The offset to print the information.
!>   @param [in]    OPT    The encoder options structure of type `ENCODER_OPTIONS_T`.
!>   @param [inout] HOOKS  Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure:
!>         - `0`: Success
!>         - `1`: Failure
!>
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] DATAKINDS_DEF_MOD::JPIB_K
!>   - @dependency [TYPE] PARAMETRIZATION_MOD::PARAMETRIZATION_T
!>   - @dependency [TYPE] FORTRAN_MESSAGE_MOD::FORTRAN_MESSAGE_T
!>   - @dependency [TYPE] HOOKS_MOD::HOOKS_T
!>
!> @section special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>
!> @section intrinsic dependencies
!>   None.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_PRINT'
PP_THREAD_SAFE FUNCTION G2S4_049_PRINT( THIS, &
& UNIT, OFFSET, OPT, HOOKS, SEPARATOR ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: OFFSET
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=*), OPTIONAL,   INTENT(IN)    :: SEPARATOR

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STAT

   ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_ERROR=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_PRINT_TIME=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_PRINT_LEVEL=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_PRINT_ENSEMBLE=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_PRINT_PARAMID=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_PRINT_CHEM=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_PRINT_MODEL=12_JPIB_K

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

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%LEVEL_), ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%ENSEMBLE_), ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%PARAMID_), ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(THIS%CHEM_), ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED )

  ! Write the section information
  IF ( OFFSET .LE. 0 ) THEN
    WRITE(UNIT,'(A,A,A,A,A,A,A)', IOSTAT=WRITE_STAT) 'GRIB_SECTION: ', &
&     TRIM(ADJUSTL(THIS%TYPE_)), '::', TRIM(ADJUSTL(THIS%SUBTYPE_)), &
&     '(', TRIM(ADJUSTL(THIS%KIND_)), '){'
    PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
  ELSE
    WRITE(UNIT,'(A,A,A,A,A,A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET), &
&     'GRIB_SECTION: ', TRIM(ADJUSTL(THIS%TYPE_)), '::', TRIM(ADJUSTL(THIS%SUBTYPE_)), &
&     '(', TRIM(ADJUSTL(THIS%KIND_)), '){'
    PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
  ENDIF

  ! Print of nested sections
  PP_TRYCALL(ERRFLAG_CALL_PRINT_MODEL) THIS%MODEL_%PRINT( UNIT, OFFSET+2, OPT, HOOKS, ', ...' )
  PP_TRYCALL(ERRFLAG_CALL_PRINT_TIME) THIS%TIME_%PRINT( UNIT, OFFSET+2, OPT, HOOKS, ', ...' )
  PP_TRYCALL(ERRFLAG_CALL_PRINT_LEVEL) THIS%LEVEL_%PRINT( UNIT, OFFSET+2, OPT, HOOKS, ', ...' )
  PP_TRYCALL(ERRFLAG_CALL_PRINT_ENSEMBLE) THIS%ENSEMBLE_%PRINT( UNIT, OFFSET+2, OPT, HOOKS, ', ...' )
  PP_TRYCALL(ERRFLAG_CALL_PRINT_CHEM) THIS%CHEM_%PRINT( UNIT, OFFSET+2, OPT, HOOKS, ', ...' )
  PP_TRYCALL(ERRFLAG_CALL_PRINT_PARAMID) THIS%PARAMID_%PRINT( UNIT, OFFSET+2, OPT, HOOKS )

  ! Close section
  IF ( PRESENT(SEPARATOR) ) THEN
    IF ( OFFSET .LE. 0 ) THEN
      WRITE(UNIT,'(A,A)', IOSTAT=WRITE_STAT) '}', TRIM(ADJUSTL(SEPARATOR))
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
    ELSE
      WRITE(UNIT,'(A,A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET), '}', TRIM(ADJUSTL(SEPARATOR))
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
    ENDIF
  ELSE
    IF ( OFFSET .LE. 0 ) THEN
      WRITE(UNIT,'(A)', IOSTAT=WRITE_STAT) '}'
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
    ELSE
      WRITE(UNIT,'(A,A)', IOSTAT=WRITE_STAT) REPEAT(' ',OFFSET), '}'
      PP_DEBUG_CRITICAL_COND_THROW(WRITE_STAT.NE.0, ERRFLAG_WRITE_ERROR )
    ENDIF
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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE ( ERRFLAG_TIME_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'time configurator not associated' )
    CASE ( ERRFLAG_MODEL_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'model configurator not associated' )
    CASE ( ERRFLAG_LEVEL_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'level configurator not associated' )
    CASE ( ERRFLAG_CHEM_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'chemistry configurator not associated' )
    CASE ( ERRFLAG_PARAMID_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'paramId configurator not associated' )
    CASE ( ERRFLAG_ENSEMBLE_CONFIGURATOR_NOT_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'ensemble configurator not associated' )
    CASE (ERRFLAG_WRITE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error writing to given unit' )
    CASE (ERRFLAG_CALL_PRINT_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling print time' )
    CASE (ERRFLAG_CALL_PRINT_MODEL)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling print model' )
    CASE (ERRFLAG_CALL_PRINT_LEVEL)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling print level' )
    CASE (ERRFLAG_CALL_PRINT_ENSEMBLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling print ensemble' )
    CASE (ERRFLAG_CALL_PRINT_PARAMID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling print paramId' )
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

END FUNCTION G2S4_049_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



!>
!> @brief Builds the param handler for GRIB2 section 4, template 049.
!>
!> This function constructs the param handler for GRIB2 section 4, template 049,
!> based on the provided configuration (`CFG`), options (`OPT`).
!> It modifies the `THIS` structure accordingly and returns an error code if the operation fails.
!> The function is thread-safe and uses preprocessor directives for debugging, logging, and tracing.
!>
!> @section interface
!>
!> @param [inout] THIS GRIB2 section 4 structure that is modified by this procedure.
!> @param [in] OPT GRIB encoder options used in the building process.
!> @param [in] CFG YAML configuration object containing relevant settings.
!> @param [inout] HOOKS Hooks object used for additional operations and callbacks during execution.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>
!> @susection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see G2S4_049_INIT
!> @see G2S4_049_ALLOCATE
!> @see G2S4_049_PRESET
!> @see G2S4_049_RUNPARAM
!> @see G2S4_049_TO_BE_ENCODED
!> @see G2S4_049_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_BUILD_PARAM_HANDLER_CFG'
PP_THREAD_SAFE FUNCTION G2S4_049_BUILD_PARAM_HANDLER_CFG( THIS, &
&               CFG, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_DELETE_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_PARAM_CONFIGURATOR
  TYPE(YAML_CONFIGURATION_T) :: PARAM_CONFIGURATOR_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: PARAM_CONFIGURATOR_NAME='param-configurator'

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARAM_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARAM_CONFIGURATOR_CFG_NOT_PRESENT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_PARAM_CONFIGURATOR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=6_JPIB_K

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


  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%PARAMID_), ERRFLAG_PARAM_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Check if a configuration for the indicator-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, PARAM_CONFIGURATOR_NAME, HAS_PARAM_CONFIGURATOR, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_PARAM_CONFIGURATOR, ERRFLAG_PARAM_CONFIGURATOR_CFG_NOT_PRESENT )

  !> Read the subconfiguration for the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, PARAM_CONFIGURATOR_NAME, PARAM_CONFIGURATOR_CFG, HOOKS )

  !> Get the name of the param-configurator section
  !! TODO

  !> Build the key to be used for the param configurator section
  !! TODO

  !> Build the param configurator object
  !! TODO

  !> Initialize the param configurator object
  !! TODO

  !> Make the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_PARAM_CONFIGURATOR) FACTORIES%MAKE_GRIB2_PARAM_CONFIGURATOR_CFG( &
&    THIS%PARAMID_, THIS%TEMPLATE_NUMBER_, PARAM_CONFIGURATOR_CFG, OPT, FACTORIES, HOOKS )

  !> Deallocate the indicator-section configuration
  PP_TRYCALL( ERRFLAG_UNABLE_TO_DEALLOCATE ) YAML_DELETE_CONFIGURATION( PARAM_CONFIGURATOR_CFG, HOOKS )

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
    CASE ( ERRFLAG_PARAM_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'param configurator already associated' )
    CASE ( ERRFLAG_UNABLE_TO_READ_CFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
    CASE ( ERRFLAG_PARAM_CONFIGURATOR_CFG_NOT_PRESENT )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'param configurator configuration not present' )
    CASE ( ERRFLAG_UNABLE_TO_READ_SUBCFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfiguration' )
    CASE ( ERRFLAG_UNABLE_TO_MAKE_PARAM_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make param configurator' )
    CASE ( ERRFLAG_UNABLE_TO_DEALLOCATE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate param configurator object' )
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

END FUNCTION G2S4_049_BUILD_PARAM_HANDLER_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_BUILD_PARAM_HANDLER_LAZY'
PP_THREAD_SAFE FUNCTION G2S4_049_BUILD_PARAM_HANDLER_LAZY( THIS, &
&               MSG, PAR, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,           ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,    ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,         ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,         ONLY: PARAMETRIZATION_T
  USE :: GRIB_SECTION_BASE_MOD,       ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                   ONLY: HOOKS_T
  USE :: GRIB_ENCODER_LAZY_RULES_MOD, ONLY: PARAM_CONFIGURATOR_NAME

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARAM_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_BUILD_PARAM_CONFUGIRATOR_LAZY=2_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%PARAMID_), ERRFLAG_PARAM_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Construct the name of the param configurator form message and parameter
  PP_TRYCALL(ERRFLAG_BUILD_PARAM_CONFUGIRATOR_LAZY) PARAM_CONFIGURATOR_NAME( MSG, PAR, THIS%PARAMID_SUBSECTION_NAME_, OPT, HOOKS )

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
    CASE ( ERRFLAG_PARAM_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'param configurator already associated' )
    CASE ( ERRFLAG_BUILD_PARAM_CONFUGIRATOR_LAZY )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build param configurator' )
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

END FUNCTION G2S4_049_BUILD_PARAM_HANDLER_LAZY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Builds the level handler for GRIB2 section 4, template 049.
!>
!> This function constructs the level handler for GRIB2 section 4, template 049,
!> based on the provided configuration (`CFG`), options (`OPT`).
!> It modifies the `THIS` structure accordingly and returns an error code if the operation fails.
!> The function is thread-safe and uses preprocessor directives for debugging, logging, and tracing.
!>
!> @section interface
!>
!> @level [inout] THIS GRIB2 section 4 structure that is modified by this procedure.
!> @level [in] OPT GRIB encoder options used in the building process.
!> @level [in] CFG YAML configuration object containing relevant settings.
!> @level [inout] HOOKS Hooks object used for additional operations and callbacks during execution.
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>
!> @susection special dependencies
!>
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see G2S4_049_INIT
!> @see G2S4_049_ALLOCATE
!> @see G2S4_049_PRESET
!> @see G2S4_049_RUNLEVEL
!> @see G2S4_049_TO_BE_ENCODED
!> @see G2S4_049_FREE
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_BUILD_LEVEL_HANDLER_CFG'
PP_THREAD_SAFE FUNCTION G2S4_049_BUILD_LEVEL_HANDLER_CFG( THIS, &
&               CFG, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_DELETE_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_LEVEL_CONFIGURATOR
  TYPE(YAML_CONFIGURATION_T) :: LEVEL_CONFIGURATOR_CFG

  !> Local leveleters
  CHARACTER(LEN=*), PARAMETER :: LEVEL_CONFIGURATOR_NAME='level-configurator'

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LEVEL_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LEVEL_CONFIGURATOR_CFG_NOT_PRESENT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_LEVEL_CONFIGURATOR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=6_JPIB_K

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


  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%LEVEL_), ERRFLAG_LEVEL_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Check if a configuration for the indicator-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, LEVEL_CONFIGURATOR_NAME, HAS_LEVEL_CONFIGURATOR, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_LEVEL_CONFIGURATOR, ERRFLAG_LEVEL_CONFIGURATOR_CFG_NOT_PRESENT )

  !> Read the subconfiguration for the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, LEVEL_CONFIGURATOR_NAME, LEVEL_CONFIGURATOR_CFG, HOOKS )

  !> Make the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_LEVEL_CONFIGURATOR) FACTORIES%MAKE_GRIB2_LEVEL_CONFIGURATOR_CFG( &
&    THIS%LEVEL_, THIS%TEMPLATE_NUMBER_, LEVEL_CONFIGURATOR_CFG, OPT, FACTORIES, HOOKS )

  !> Deallocate the indicator-section configuration
  PP_TRYCALL( ERRFLAG_UNABLE_TO_DEALLOCATE ) YAML_DELETE_CONFIGURATION( LEVEL_CONFIGURATOR_CFG, HOOKS )

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
    CASE ( ERRFLAG_LEVEL_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'level configurator already associated' )
    CASE ( ERRFLAG_UNABLE_TO_READ_CFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
    CASE ( ERRFLAG_LEVEL_CONFIGURATOR_CFG_NOT_PRESENT )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'level configurator configuration not present' )
    CASE ( ERRFLAG_UNABLE_TO_READ_SUBCFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfiguration' )
    CASE ( ERRFLAG_UNABLE_TO_MAKE_LEVEL_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make level configurator' )
    CASE ( ERRFLAG_UNABLE_TO_DEALLOCATE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate level configurator object' )
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

END FUNCTION G2S4_049_BUILD_LEVEL_HANDLER_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_BUILD_LEVEL_HANDLER_LAZY'
PP_THREAD_SAFE FUNCTION G2S4_049_BUILD_LEVEL_HANDLER_LAZY( THIS, &
&               MSG, PAR, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,           ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,    ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,         ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,         ONLY: PARAMETRIZATION_T
  USE :: GRIB_SECTION_BASE_MOD,       ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                   ONLY: HOOKS_T
  USE :: GRIB_ENCODER_LAZY_RULES_MOD, ONLY: LEVEL_CONFIGURATOR_NAME

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LEVEL_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_BUILD_LEVEL_CONFUGIRATOR_LAZY=2_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%LEVEL_), ERRFLAG_LEVEL_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Construct the name of the param configurator form message and parameter
  PP_TRYCALL(ERRFLAG_BUILD_LEVEL_CONFUGIRATOR_LAZY) LEVEL_CONFIGURATOR_NAME( MSG, PAR, THIS%LEVEL_SUBSECTION_NAME_, OPT, HOOKS )

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
    CASE ( ERRFLAG_LEVEL_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'level configurator already associated' )
    CASE ( ERRFLAG_BUILD_LEVEL_CONFUGIRATOR_LAZY )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build level configurator' )
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

END FUNCTION G2S4_049_BUILD_LEVEL_HANDLER_LAZY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_BUILD_ENSEMBLE_HANDLER_CFG'
PP_THREAD_SAFE FUNCTION G2S4_049_BUILD_ENSEMBLE_HANDLER_CFG( THIS, &
&               CFG, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_DELETE_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_ENSEMBLE_CONFIGURATOR
  TYPE(YAML_CONFIGURATION_T) :: ENSEMBLE_CONFIGURATOR_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: ENSEMBLE_CONFIGURATOR_NAME='ensemble-configurator'

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENSEMBLE_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENSEMBLE_CONFIGURATOR_CFG_NOT_PRESENT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_ENSEMBLE_CONFIGURATOR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=6_JPIB_K

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


  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%ENSEMBLE_), ERRFLAG_ENSEMBLE_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Check if a configuration for the indicator-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, ENSEMBLE_CONFIGURATOR_NAME, HAS_ENSEMBLE_CONFIGURATOR, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_ENSEMBLE_CONFIGURATOR, ERRFLAG_ENSEMBLE_CONFIGURATOR_CFG_NOT_PRESENT )

  !> Read the subconfiguration for the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, ENSEMBLE_CONFIGURATOR_NAME, ENSEMBLE_CONFIGURATOR_CFG, HOOKS )

  !> Make the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_ENSEMBLE_CONFIGURATOR) FACTORIES%MAKE_GRIB2_ENSEMBLE_CONFIGURATOR_CFG( &
&    THIS%ENSEMBLE_, THIS%TEMPLATE_NUMBER_, ENSEMBLE_CONFIGURATOR_CFG, OPT, FACTORIES, HOOKS )

  !> Deallocate the indicator-section configuration
  PP_TRYCALL( ERRFLAG_UNABLE_TO_DEALLOCATE ) YAML_DELETE_CONFIGURATION( ENSEMBLE_CONFIGURATOR_CFG, HOOKS )

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
    CASE ( ERRFLAG_ENSEMBLE_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'ensemble configurator already associated' )
    CASE ( ERRFLAG_UNABLE_TO_READ_CFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
    CASE ( ERRFLAG_ENSEMBLE_CONFIGURATOR_CFG_NOT_PRESENT )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'ensemble configurator configuration not present' )
    CASE ( ERRFLAG_UNABLE_TO_READ_SUBCFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfiguration' )
    CASE ( ERRFLAG_UNABLE_TO_MAKE_ENSEMBLE_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make ensemble configurator' )
    CASE ( ERRFLAG_UNABLE_TO_DEALLOCATE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate ensemble configurator object' )
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

END FUNCTION G2S4_049_BUILD_ENSEMBLE_HANDLER_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_BUILD_ENSEMBLE_HANDLER_LAZY'
PP_THREAD_SAFE FUNCTION G2S4_049_BUILD_ENSEMBLE_HANDLER_LAZY( THIS, &
&               MSG, PAR, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,           ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,    ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,         ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,         ONLY: PARAMETRIZATION_T
  USE :: GRIB_SECTION_BASE_MOD,       ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                   ONLY: HOOKS_T
  USE :: GRIB_ENCODER_LAZY_RULES_MOD, ONLY: ENSEMBLE_CONFIGURATOR_NAME

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ENSEMBLE_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_BUILD_ENSEMBLE_CONFUGIRATOR_LAZY=2_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%LEVEL_), ERRFLAG_ENSEMBLE_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Construct the name of the param configurator form message and parameter
  PP_TRYCALL(ERRFLAG_BUILD_ENSEMBLE_CONFUGIRATOR_LAZY) ENSEMBLE_CONFIGURATOR_NAME( MSG, PAR, THIS%ENSEMBLE_SUBSECTION_NAME_, OPT, HOOKS )

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
    CASE ( ERRFLAG_ENSEMBLE_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'ensemble configurator already associated' )
    CASE ( ERRFLAG_BUILD_ENSEMBLE_CONFUGIRATOR_LAZY )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build ensemble configurator' )
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

END FUNCTION G2S4_049_BUILD_ENSEMBLE_HANDLER_LAZY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GRIB2_SECTION4_049_BUILD_CHEM_HANDLER'
PP_THREAD_SAFE FUNCTION GRIB2_SECTION4_049_BUILD_CHEM_HANDLER( THIS, &
&               CFG, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_DELETE_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_CHEM_CONFIGURATOR
  TYPE(YAML_CONFIGURATION_T) :: CHEM_CONFIGURATOR_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: CHEM_CONFIGURATOR_NAME='chemistry-configurator'

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHEM_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHEM_CONFIGURATOR_CFG_NOT_PRESENT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_CHEM_CONFIGURATOR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=6_JPIB_K

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


  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%CHEM_), ERRFLAG_CHEM_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Check if a configuration for the indicator-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, CHEM_CONFIGURATOR_NAME, HAS_CHEM_CONFIGURATOR, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_CHEM_CONFIGURATOR, ERRFLAG_CHEM_CONFIGURATOR_CFG_NOT_PRESENT )

  !> Read the subconfiguration for the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, CHEM_CONFIGURATOR_NAME, CHEM_CONFIGURATOR_CFG, HOOKS )

  !> Make the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_CHEM_CONFIGURATOR) FACTORIES%MAKE_GRIB2_CHEM_CONFIGURATOR_CFG( &
&    THIS%CHEM_, THIS%TEMPLATE_NUMBER_, CHEM_CONFIGURATOR_CFG, OPT, FACTORIES, HOOKS )

  !> Deallocate the indicator-section configuration
  PP_TRYCALL( ERRFLAG_UNABLE_TO_DEALLOCATE ) YAML_DELETE_CONFIGURATION( CHEM_CONFIGURATOR_CFG, HOOKS )

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
    CASE ( ERRFLAG_CHEM_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'chemistry configurator already associated' )
    CASE ( ERRFLAG_UNABLE_TO_READ_CFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
    CASE ( ERRFLAG_CHEM_CONFIGURATOR_CFG_NOT_PRESENT )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'chemistry configurator configuration not present' )
    CASE ( ERRFLAG_UNABLE_TO_READ_SUBCFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfiguration' )
    CASE ( ERRFLAG_UNABLE_TO_MAKE_CHEM_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make chem configurator' )
    CASE ( ERRFLAG_UNABLE_TO_DEALLOCATE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate chem configurator object' )
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

END FUNCTION GRIB2_SECTION4_049_BUILD_CHEM_HANDLER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_BUILD_TIME_HANDLER_CFG'
PP_THREAD_SAFE FUNCTION G2S4_049_BUILD_TIME_HANDLER_CFG( THIS, &
&               CFG, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_DELETE_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_TIME_CONFIGURATOR
  TYPE(YAML_CONFIGURATION_T) :: TIME_CONFIGURATOR_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: TIME_CONFIGURATOR_NAME='point-in-time-configurator'

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIME_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIME_CONFIGURATOR_CFG_NOT_PRESENT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_TIME_CONFIGURATOR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=6_JPIB_K

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


  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%TIME_), ERRFLAG_TIME_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Check if a configuration for the indicator-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, TIME_CONFIGURATOR_NAME, HAS_TIME_CONFIGURATOR, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_TIME_CONFIGURATOR, ERRFLAG_TIME_CONFIGURATOR_CFG_NOT_PRESENT )

  !> Read the subconfiguration for the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, TIME_CONFIGURATOR_NAME, TIME_CONFIGURATOR_CFG, HOOKS )

  !> Make the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_TIME_CONFIGURATOR) FACTORIES%MAKE_GRIB2_POINT_IN_TIME_CONFIGURATOR_CFG( &
&    THIS%TIME_, THIS%TEMPLATE_NUMBER_, TIME_CONFIGURATOR_CFG, OPT, FACTORIES, HOOKS )

  !> Deallocate the indicator-section configuration
  PP_TRYCALL( ERRFLAG_UNABLE_TO_DEALLOCATE ) YAML_DELETE_CONFIGURATION( TIME_CONFIGURATOR_CFG, HOOKS )

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
    CASE ( ERRFLAG_TIME_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'time configurator already associated' )
    CASE ( ERRFLAG_UNABLE_TO_READ_CFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
    CASE ( ERRFLAG_TIME_CONFIGURATOR_CFG_NOT_PRESENT )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'time configurator configuration not present' )
    CASE ( ERRFLAG_UNABLE_TO_READ_SUBCFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfiguration' )
    CASE ( ERRFLAG_UNABLE_TO_MAKE_TIME_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make time configurator' )
    CASE ( ERRFLAG_UNABLE_TO_DEALLOCATE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate time configurator object' )
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

END FUNCTION G2S4_049_BUILD_TIME_HANDLER_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_BUILD_TIME_HANDLER_LAZY'
PP_THREAD_SAFE FUNCTION G2S4_049_BUILD_TIME_HANDLER_LAZY( THIS, &
&               MSG, PAR, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,           ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,    ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,         ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,         ONLY: PARAMETRIZATION_T
  USE :: GRIB_SECTION_BASE_MOD,       ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                   ONLY: HOOKS_T
  USE :: GRIB_ENCODER_LAZY_RULES_MOD, ONLY: TIME_CONFIGURATOR_NAME

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIME_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_BUILD_TIME_CONFUGIRATOR_LAZY=2_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%LEVEL_), ERRFLAG_TIME_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Construct the name of the param configurator form message and parameter
  PP_TRYCALL(ERRFLAG_BUILD_TIME_CONFUGIRATOR_LAZY) TIME_CONFIGURATOR_NAME( MSG, PAR, THIS%TIME_SUBSECTION_NAME_, OPT, HOOKS )

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
    CASE ( ERRFLAG_TIME_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'time configurator already associated' )
    CASE ( ERRFLAG_BUILD_TIME_CONFUGIRATOR_LAZY )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build time configurator' )
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

END FUNCTION G2S4_049_BUILD_TIME_HANDLER_LAZY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_BUILD_MODEL_HANDLER_CFG'
PP_THREAD_SAFE FUNCTION G2S4_049_BUILD_MODEL_HANDLER_CFG( THIS, &
&               CFG, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_DELETE_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_MODEL_CONFIGURATOR
  TYPE(YAML_CONFIGURATION_T) :: MODEL_CONFIGURATOR_CFG

  !> Local parameters
  CHARACTER(LEN=*), PARAMETER :: MODEL_CONFIGURATOR_NAME='model-configurator'

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODEL_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODEL_CONFIGURATOR_CFG_NOT_PRESENT=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_MAKE_MODEL_CONFIGURATOR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=6_JPIB_K

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


  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%MODEL_), ERRFLAG_MODEL_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Check if a configuration for the indicator-section is available
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, MODEL_CONFIGURATOR_NAME, HAS_MODEL_CONFIGURATOR, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_MODEL_CONFIGURATOR, ERRFLAG_MODEL_CONFIGURATOR_CFG_NOT_PRESENT )

  !> Read the subconfiguration for the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, MODEL_CONFIGURATOR_NAME, MODEL_CONFIGURATOR_CFG, HOOKS )

  !> Make the indicator-section
  PP_TRYCALL(ERRFLAG_UNABLE_TO_MAKE_MODEL_CONFIGURATOR) FACTORIES%MAKE_GRIB2_MODEL_CONFIGURATOR_CFG( &
&    THIS%MODEL_, THIS%TEMPLATE_NUMBER_, MODEL_CONFIGURATOR_CFG, OPT, FACTORIES, HOOKS )

  !> Deallocate the indicator-section configuration
  PP_TRYCALL( ERRFLAG_UNABLE_TO_DEALLOCATE ) YAML_DELETE_CONFIGURATION( MODEL_CONFIGURATOR_CFG, HOOKS )

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
    CASE ( ERRFLAG_MODEL_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'model configurator already associated' )
    CASE ( ERRFLAG_UNABLE_TO_READ_CFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
    CASE ( ERRFLAG_MODEL_CONFIGURATOR_CFG_NOT_PRESENT )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'model configurator configuration not present' )
    CASE ( ERRFLAG_UNABLE_TO_READ_SUBCFG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfiguration' )
    CASE ( ERRFLAG_UNABLE_TO_MAKE_MODEL_CONFIGURATOR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to make model configurator' )
    CASE ( ERRFLAG_UNABLE_TO_DEALLOCATE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate model configurator object' )
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

END FUNCTION G2S4_049_BUILD_MODEL_HANDLER_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'G2S4_049_BUILD_MODEL_HANDLER_LAZY'
PP_THREAD_SAFE FUNCTION G2S4_049_BUILD_MODEL_HANDLER_LAZY( THIS, &
&               MSG, PAR, OPT, FACTORIES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,           ONLY: JPIB_K
  USE :: GRIB_ENCODER_OPTIONS_MOD,    ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FORTRAN_MESSAGE_MOD,         ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,         ONLY: PARAMETRIZATION_T
  USE :: GRIB_SECTION_BASE_MOD,       ONLY: GRIB_SECTION_FACTORY_T
  USE :: HOOKS_MOD,                   ONLY: HOOKS_T
  USE :: GRIB_ENCODER_LAZY_RULES_MOD, ONLY: MODEL_CONFIGURATOR_NAME

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(G2S4_049_T),            INTENT(INOUT) :: THIS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(IN)    :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T), INTENT(IN)    :: OPT
  TYPE(GRIB_SECTION_FACTORY_T), INTENT(IN)    :: FACTORIES
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MODEL_CONFIGURATOR_ALREADY_ASSOCIATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_BUILD_MODEL_CONFUGIRATOR_LAZY=2_JPIB_K

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

  !> Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(THIS%LEVEL_), ERRFLAG_MODEL_CONFIGURATOR_ALREADY_ASSOCIATED )

  !> Construct the name of the param configurator form message and parameter
  PP_TRYCALL(ERRFLAG_BUILD_MODEL_CONFUGIRATOR_LAZY) MODEL_CONFIGURATOR_NAME( MSG, PAR, THIS%MODEL_SUBSECTION_NAME_, OPT, HOOKS )

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
    CASE ( ERRFLAG_MODEL_CONFIGURATOR_ALREADY_ASSOCIATED )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'model configurator already associated' )
    CASE ( ERRFLAG_BUILD_MODEL_CONFUGIRATOR_LAZY )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to build model configurator' )
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

END FUNCTION G2S4_049_BUILD_MODEL_HANDLER_LAZY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB2_SECTION4_049_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
