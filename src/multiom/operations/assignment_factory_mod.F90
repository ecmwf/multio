!>
!> @file assignment_factory_mod.F90
!>
!> @brief Module containing the factory function for creating or initializing encoder objects.
!>
!> The `ASSIGNMENT_FACTORY_MOD` provides a factory function that creates or initializes
!> instances of encoder objects. The function relies on various data structures and
!> types defined within the model's core and data types modules, as well as a YAML configuration
!> for initializing the section's parameters. Debugging, logging, and tracing features are enabled
!> via preprocessor directives to allow additional output when needed.
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [TYPE] ASSIGNMENT_000_MOD::ASSIGNMENT_000_T
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


#define PP_FILE_NAME 'assignment_factory_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ASSIGNMENT_FACTORY_MOD'
MODULE ASSIGNMENT_FACTORY_MOD

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE
!>
!> Public symbols (dataTypes)
PUBLIC :: MAKE_ASSIGNMENT
PUBLIC :: DESTROY_ASSIGNMENT

CONTAINS

!>
!> @brief Factory function for creating or initializing encoder objects.
!>
!> This function acts as a factory for creating or initializing a encoder object
!> based on the provided parameters. It assigns the proper type (`ASSIGNMENT_000_T`)
!> to the `ASSIGNMENT` object and configures it using the provided model parameters,
!> ID, and YAML configuration. If verbose mode is enabled, additional debug information
!> is output during the process.
!>
!> @param [inout] ASSIGNMENT The encoder object that will be created or initialized.
!>                              It must be a pointer of type `GRIB_SECTION_BASE_A`.
!> @param [in] ID Integer identifier for the encoder object.
!> @param [in] PARAMS The model parameters structure of type `MODEL_PAR_T`.
!> @param [in] CFG YAML configuration object used to configure the encoder object.
!> @param [in] OPT The generic options to be used to initialize the assignment.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return Integer error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Section that can be constructed with this factory
!>   - `ASSIGNMENT_000_T`
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [TYPE] ASSIGNMENT_000_MOD::ASSIGNMENT_000_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see ASSIGNMENT_000_T
!>
#define PP_PROCEDURE_TYPE 'RESURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_ASSIGNMENT'
RECURSIVE FUNCTION MAKE_ASSIGNMENT( ASSIGNMENT, CTYPE, CFG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_GET_SUBCONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_GET_CONFIGURATIONS_SIZE
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_GET_CONFIGURATION_BY_ID
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_READ_STRING
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_DELETE_CONFIGURATIONS
  USE :: ASSIGNMENT_BASE_MOD,      ONLY: ASSIGNMENT_BASE_A
  USE :: ASSIGNMENT_BASE_MOD,      ONLY: ASSIGNMENT_CONTAINER_T
  USE :: ASSIGNMENT_COMPOSED_MOD,  ONLY: ASSIGNMENT_COMPOSED_T
  USE :: ASSIGNMENT_COPY_MOD,      ONLY: ASSIGNMENT_COPY_T
  USE :: ASSIGNMENT_MSG_INT_MOD,   ONLY: ASSIGNMENT_MSG_INT_T
  USE :: ASSIGNMENT_PAR_INT_MOD,   ONLY: ASSIGNMENT_PAR_INT_T
  USE :: ASSIGNMENT_MSG_FLOAT_MOD, ONLY: ASSIGNMENT_MSG_FLOAT_T
  USE :: ASSIGNMENT_PAR_FLOAT_MOD, ONLY: ASSIGNMENT_PAR_FLOAT_T
  USE :: ASSIGNMENT_MSG_STRING_MOD, ONLY: ASSIGNMENT_MSG_STRING_T
  USE :: ASSIGNMENT_PAR_STRING_MOD, ONLY: ASSIGNMENT_PAR_STRING_T



  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ASSIGNMENT_BASE_A), POINTER, INTENT(INOUT) :: ASSIGNMENT
  CHARACTER(LEN=*),                  INTENT(IN)    :: CTYPE
  TYPE(YAML_CONFIGURATION_T),        INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: N_ASSIGNMENTS
  INTEGER(KIND=JPIB_K) :: I
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=:), ALLOCATABLE :: NESTED_CTYPE
  LOGICAL :: HAS_ASSIGNMENTS
  LOGICAL :: HAS_TYPE
  TYPE(YAML_CONFIGURATIONS_T) :: NESTED_ASSIGNMENTS_CFG
  TYPE(YAML_CONFIGURATION_T) :: NESTED_ASSIGNMENT_CFG
  TYPE(ASSIGNMENT_CONTAINER_T), DIMENSION(:), POINTER :: NESTED_ASSIGNMENTS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INITIALIZATION_ERROR = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_ASSIGNMENT = 4_JPIB_K

  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NESTED_ASSIGNMENTS_NOT_FOUND = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_NUMBER_OF_ASSIGNMENTS = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_NESTED_ASSIGNMENTS = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_NESTED_ASSIGNMENTS = 10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NESTED_ASSIGNMENTS_NOT_ASSOCIATED = 11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_SIZE_OF_NESTED_ASSIGNMENTS = 12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_ASSIGNMENT_TYPE = 13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG_BY_ID = 14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_TYPE_CFG = 15_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UDEFINED_TYPE = 16_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_TYPE_ID = 17_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ = 18_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_NESTED_FILTER = 19_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE = 20_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ASSIGNMENT_TYPE_DEALLOCATION_ERROR = 21_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ASSIGNMENTS_TYPE_DEALLOCATION_ERROR = 22_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG = 23_JPIB_K


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

  !> Check initial status
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(ASSIGNMENT), ERRFLAG_ALREADY_ASSOCIATED )

  ! Initialize the section
  SELECT CASE( TRIM(ADJUSTL(CTYPE)) )

  CASE ( 'composed' )


    ALLOCATE( ASSIGNMENT_COMPOSED_T::ASSIGNMENT, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  ASSIGNMENT%INIT( CFG, HOOKS )

    !> Read the encoder configuration
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'assignments', HAS_ASSIGNMENTS, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_ASSIGNMENTS, ERRFLAG_NESTED_ASSIGNMENTS_NOT_FOUND )

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATIONS( CFG, 'assignments', NESTED_ASSIGNMENTS_CFG, HOOKS )

    !> Get the sections size
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE) YAML_GET_CONFIGURATIONS_SIZE( NESTED_ASSIGNMENTS_CFG, N_ASSIGNMENTS, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( N_ASSIGNMENTS .LE. 0, ERRFLAG_WRONG_NUMBER_OF_ASSIGNMENTS )

    !> Allocate the nested filters
    SELECT TYPE(A => ASSIGNMENT)
    CLASS IS (ASSIGNMENT_COMPOSED_T)

      !> Allocate Filters
      PP_TRYCALL(ERRFLAG_UNABLE_TO_ALLOCATE_NESTED_ASSIGNMENTS) A%ALLOCATE_NESTED_ASSIGNMENTS( N_ASSIGNMENTS, HOOKS )

      !> Get Filters
      NESTED_ASSIGNMENTS => NULL()
      PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NESTED_ASSIGNMENTS) A%GET_NESTED_ASSIGNMENTS( NESTED_ASSIGNMENTS, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(NESTED_ASSIGNMENTS), ERRFLAG_NESTED_ASSIGNMENTS_NOT_ASSOCIATED )
      PP_DEBUG_CRITICAL_COND_THROW( SIZE(NESTED_ASSIGNMENTS).NE.N_ASSIGNMENTS, ERRFLAG_WRONG_SIZE_OF_NESTED_ASSIGNMENTS)

    CLASS DEFAULT

      PP_DEBUG_CRITICAL_THROW( ERRFLAG_WRONG_ASSIGNMENT_TYPE )

    END SELECT

    !> Loop over sections
    DO I = 1 , N_ASSIGNMENTS

      !> Get section configuration by ID
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG_BY_ID) YAML_GET_CONFIGURATION_BY_ID( NESTED_ASSIGNMENTS_CFG, I, NESTED_ASSIGNMENT_CFG, HOOKS )

      !> Check if configuration has the section keyword
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_TYPE_CFG) YAML_CONFIGURATION_HAS_KEY( NESTED_ASSIGNMENT_CFG, 'type', HAS_TYPE, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_TYPE, ERRFLAG_UDEFINED_TYPE )

      !> Get the section ID
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_TYPE_ID) YAML_READ_STRING( NESTED_ASSIGNMENT_CFG, 'type', NESTED_CTYPE, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(NESTED_CTYPE), ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ )

      !> Create the nested filter
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_NESTED_FILTER) MAKE_ASSIGNMENT( NESTED_ASSIGNMENTS(I)%ASSIGNMENT_, NESTED_CTYPE, NESTED_ASSIGNMENT_CFG, HOOKS )

      DEALLOCATE(NESTED_CTYPE, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE )

      !> Deallocate section configuration
      PP_TRYCALL( ERRFLAG_ASSIGNMENT_TYPE_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( NESTED_ASSIGNMENT_CFG, HOOKS )

    ENDDO

    !> Deallocate sections
    PP_TRYCALL( ERRFLAG_ASSIGNMENTS_TYPE_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATIONS( NESTED_ASSIGNMENTS_CFG, HOOKS )

  CASE( 'copy' )


    ALLOCATE( ASSIGNMENT_COPY_T::ASSIGNMENT, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  ASSIGNMENT%INIT( CFG, HOOKS )


  CASE( 'message-int' )


    ALLOCATE( ASSIGNMENT_MSG_INT_T::ASSIGNMENT, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  ASSIGNMENT%INIT( CFG, HOOKS )


  CASE( 'parametrization-int' )


    ALLOCATE( ASSIGNMENT_PAR_INT_T::ASSIGNMENT, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  ASSIGNMENT%INIT( CFG, HOOKS )


  CASE( 'message-float' )


    ALLOCATE( ASSIGNMENT_MSG_FLOAT_T::ASSIGNMENT, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  ASSIGNMENT%INIT( CFG, HOOKS )


  CASE( 'parametrization-float' )


    ALLOCATE( ASSIGNMENT_PAR_FLOAT_T::ASSIGNMENT, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  ASSIGNMENT%INIT( CFG, HOOKS )


  CASE( 'message-string' )


    ALLOCATE( ASSIGNMENT_MSG_STRING_T::ASSIGNMENT, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  ASSIGNMENT%INIT( CFG, HOOKS )


  CASE( 'parametrization-string' )


    ALLOCATE( ASSIGNMENT_PAR_STRING_T::ASSIGNMENT, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  ASSIGNMENT%INIT( CFG, HOOKS )

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_ASSIGNMENT )

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
    CASE(ERRFLAG_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'section already associated' )
    CASE(ERRFLAG_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'allocation error' )
    CASE(ERRFLAG_INITIALIZATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'initialization error' )
    CASE(ERRFLAG_UNKNOWN_ASSIGNMENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown assignment type: '//TRIM(ADJUSTL(CTYPE)) )
    CASE(ERRFLAG_NESTED_ASSIGNMENTS_NOT_FOUND)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'nested assignments not found' )
    CASE(ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read subconfigurations' )
    CASE(ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get subconfigurations size' )
    CASE(ERRFLAG_WRONG_NUMBER_OF_ASSIGNMENTS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong number of assignments' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE_NESTED_ASSIGNMENTS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to allocate nested assignments' )
    CASE(ERRFLAG_UNABLE_TO_GET_NESTED_ASSIGNMENTS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get nested assignments' )
    CASE(ERRFLAG_NESTED_ASSIGNMENTS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'nested assignments not associated' )
    CASE(ERRFLAG_WRONG_SIZE_OF_NESTED_ASSIGNMENTS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong size of nested assignments' )
    CASE(ERRFLAG_WRONG_ASSIGNMENT_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong assignment type' )
    CASE(ERRFLAG_UNABLE_TO_READ_CFG_BY_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration by ID' )
    CASE(ERRFLAG_UNABLE_TO_READ_TYPE_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read type configuration' )
    CASE(ERRFLAG_UDEFINED_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'undefined type' )
    CASE(ERRFLAG_UNABLE_TO_READ_TYPE_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read type ID' )
    CASE(ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'type not allocated after read' )
    CASE(ERRFLAG_UNABLE_TO_READ_NESTED_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read nested filter' )
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to deallocate type' )
    CASE(ERRFLAG_ASSIGNMENT_TYPE_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'assignment type deallocation error' )
    CASE(ERRFLAG_ASSIGNMENTS_TYPE_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'assignments type deallocation error' )
    CASE(ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read configuration' )
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

END FUNCTION MAKE_ASSIGNMENT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Destroys a encoder structure.
!>
!> This function takes a encoder object (`ASSIGNMENT`) and performs
!> the necessary cleanup, freeing any associated resources. It also supports a
!> `VERBOSE` mode for detailed output during the destruction process.
!>
!> @section interface
!> @param[in,out] ASSIGNMENT The encoder object to be destroyed.
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
!> @see MAKE_ASSIGNMENT
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DESTROY_ASSIGNMENT'
RECURSIVE FUNCTION DESTROY_ASSIGNMENT( ASSIGNMENT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,       ONLY: JPIB_K
  USE :: HOOKS_MOD,               ONLY: HOOKS_T
  USE :: ASSIGNMENT_BASE_MOD,     ONLY: ASSIGNMENT_BASE_A
  USE :: ASSIGNMENT_BASE_MOD,     ONLY: ASSIGNMENT_CONTAINER_T
  USE :: ASSIGNMENT_COMPOSED_MOD, ONLY: ASSIGNMENT_COMPOSED_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(ASSIGNMENT_BASE_A), POINTER, INTENT(INOUT) :: ASSIGNMENT
  TYPE(HOOKS_T),                     INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  TYPE(ASSIGNMENT_CONTAINER_T), DIMENSION(:), POINTER :: NESTED_ASSIGNMENTS


  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_ERROR = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_NESTED_ASSIGNMENTS = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NESTED_ASSIGNMENTS = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NESTED_DESTRUCTOR_ERROR = 6_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(ASSIGNMENT), ERRFLAG_NOT_ASSOCIATED )

  SELECT TYPE(A => ASSIGNMENT)
  CLASS IS (ASSIGNMENT_COMPOSED_T)

    !> Get Assignments
    NESTED_ASSIGNMENTS => NULL()
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NESTED_ASSIGNMENTS) A%GET_NESTED_ASSIGNMENTS( NESTED_ASSIGNMENTS, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(NESTED_ASSIGNMENTS), ERRFLAG_NESTED_ASSIGNMENTS )

    DO I = 1 , SIZE(NESTED_ASSIGNMENTS)
      PP_TRYCALL(ERRFLAG_NESTED_DESTRUCTOR_ERROR) DESTROY_ASSIGNMENT( NESTED_ASSIGNMENTS(I)%ASSIGNMENT_, HOOKS )
    ENDDO

  END SELECT

  ! Deallocate assignments
  PP_TRYCALL(ERRFLAG_FREE_ERROR)  ASSIGNMENT%FREE( HOOKS )
  DEALLOCATE( ASSIGNMENT, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
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
    CASE(ERRFLAG_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'section not associated' )
    CASE(ERRFLAG_FREE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'free error' )
    CASE(ERRFLAG_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'deallocation error' )
    CASE(ERRFLAG_UNABLE_TO_GET_NESTED_ASSIGNMENTS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get nested assignments' )
    CASE(ERRFLAG_NESTED_ASSIGNMENTS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'nested assignments' )
    CASE(ERRFLAG_NESTED_DESTRUCTOR_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'nested destructor error' )
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

END FUNCTION DESTROY_ASSIGNMENT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE ASSIGNMENT_FACTORY_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
