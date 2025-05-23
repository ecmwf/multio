!>
!> @file floatop_factory_mod.F90
!>
!> @brief Module containing the factory function for creating or initializing encoder objects.
!>
!> The `FLOATOP_FACTORY_MOD` provides a factory function that creates or initializes
!> instances of encoder objects. The function relies on various data structures and
!> types defined within the model's core and data types modules, as well as a YAML configuration
!> for initializing the section's parameters. Debugging, logging, and tracing features are enabled
!> via preprocessor directives to allow additional output when needed.
!>
!> @section local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [TYPE] FLOATOP_000_MOD::FLOATOP_000_T
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


#define PP_FILE_NAME 'floatop_factory_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'FLOATOP_FACTORY_MOD'
MODULE FLOATOP_FACTORY_MOD

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE
!>
!> Public symbols (dataTypes)
PUBLIC :: MAKE_FLOATOP
PUBLIC :: DESTROY_FLOATOP

CONTAINS

!>
!> @brief Factory function for creating or initializing encoder objects.
!>
!> This function acts as a factory for creating or initializing a encoder object
!> based on the provided parameters. It assigns the proper type (`FLOATOP_000_T`)
!> to the `FLOATOP` object and configures it using the provided model parameters,
!> ID, and YAML configuration. If verbose mode is enabled, additional debug information
!> is output during the process.
!>
!> @param [inout] FLOATOP The encoder object that will be created or initialized.
!>                              It must be a pointer of type `GRIB_SECTION_BASE_A`.
!> @param [in] ID INTEGER identifier for the encoder object.
!> @param [in] PARAMS The model parameters structure of type `MODEL_PAR_T`.
!> @param [in] CFG YAML configuration object used to configure the encoder object.
!> @param [in] OPT The generic options to be used to initialize the floatop.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return INTEGER error code (`RET`) indicating success or failure of the operation.
!>         Possible values:
!>           - `0`: Success
!>           - `1`: Failure
!>
!> @section Section that can be constructed with this factory
!>   - `FLOATOP_000_T`
!>
!> @section Dependencies of this function:
!>
!> @subsection local dependencies
!>   - @dependency [PARAMETER] OM_CORE_MOD::JPIB_K
!>   - @dependency [TYPE] FLOATOP_000_MOD::FLOATOP_000_T
!>   - @dependency [TYPE] YAML_CORE_UTILS_MOD::YAML_CONFIGURATION_T
!>   - @dependency [TYPE] OM_DATA_TYPES_MOD::MODEL_PAR_T
!>
!> @subsection special dependencies
!>   - @dependency [*] PP_DEBUG_USE_VARS::*
!>   - @dependency [*] PP_LOG_USE_VARS::*
!>   - @dependency [*] PP_TRACE_USE_VARS::*
!>
!> @see FLOATOP_000_T
!>
#define PP_PROCEDURE_TYPE 'RESURSIVE FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_FLOATOP'
RECURSIVE FUNCTION MAKE_FLOATOP( FLOATOP, CTYPE, CFG, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,           ONLY: JPIB_K
  USE :: FLOATOP_BASE_MOD,            ONLY: FLOATOP_BASE_A
  USE :: FLOATOP_BASE_MOD,            ONLY: FLOATOP_CONTAINER_T
  USE :: FLOATOP_FUNCTION_CALL_MOD,   ONLY: FLOATOP_FUNCTION_CALL_T
  USE :: FLOATOP_BINARY_OP_MOD,       ONLY: FLOATOP_BINARY_OP_T
  USE :: FLOATOP_UNARY_OP_MOD,        ONLY: FLOATOP_UNARY_OP_T
  USE :: FLOATOP_CONSTANT_MOD,        ONLY: FLOATOP_CONSTANT_T
  USE :: FLOATOP_MESSAGE_MOD,         ONLY: FLOATOP_MESSAGE_T
  USE :: FLOATOP_PARAMETRIZATION_MOD, ONLY: FLOATOP_PARAMETRIZATION_T
  USE :: YAML_CORE_UTILS_MOD,         ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD,         ONLY: YAML_CONFIGURATIONS_T
  USE :: YAML_CORE_UTILS_MOD,         ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,         ONLY: YAML_GET_SUBCONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD,         ONLY: YAML_GET_SUBCONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,         ONLY: YAML_GET_CONFIGURATIONS_SIZE
  USE :: YAML_CORE_UTILS_MOD,         ONLY: YAML_GET_CONFIGURATION_BY_ID
  USE :: YAML_CORE_UTILS_MOD,         ONLY: YAML_READ_STRING
  USE :: YAML_CORE_UTILS_MOD,         ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,         ONLY: YAML_DELETE_CONFIGURATIONS
  USE :: HOOKS_MOD,                   ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(FLOATOP_BASE_A), POINTER, INTENT(INOUT) :: FLOATOP
  CHARACTER(LEN=*),                INTENT(IN)    :: CTYPE
  TYPE(YAML_CONFIGURATION_T),      INTENT(IN)    :: CFG
  TYPE(HOOKS_T),                   INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: N_FLOATOPS
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=:), ALLOCATABLE :: NESTED_CTYPE
  LOGICAL :: HAS_FLOATOPS
  LOGICAL :: HAS_TYPE
  CLASS(FLOATOP_BASE_A), POINTER :: TMP_OP
  TYPE(YAML_CONFIGURATION_T)  :: NESTED_FLOATOP_CONFIGURATION
  TYPE(YAML_CONFIGURATIONS_T) :: NESTED_FLOATOPS_CONFIGURATION
  TYPE(FLOATOP_CONTAINER_T), POINTER, DIMENSION(:) :: OPERANDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INITIALIZATION_ERROR = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERAND_UNDEFINED = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_TYPE_CFG = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UDEFINED_TYPE = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_TYPE_ID = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_NESTED_FLOATOP = 10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE = 11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_FLOATOP_TYPE = 12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FLOATOP_TYPE_DEALLOCATION_ERROR = 13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_NUMBER_OF_OPERATIONS = 14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE = 15_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_NESTED_FLOATOPS = 16_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG_BY_ID = 17_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FLOATOPS_TYPE_DEALLOCATION_ERROR = 18_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_FLOATOP = 19_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERANDS_NOT_ASSOCIATED = 20_JPIB_K



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


  CASE( 'message' )


    ALLOCATE( FLOATOP_MESSAGE_T::FLOATOP, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FLOATOP%INIT( CFG, HOOKS )


  CASE( 'parametrization' )


    ALLOCATE( FLOATOP_PARAMETRIZATION_T::FLOATOP, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FLOATOP%INIT( CFG, HOOKS )

  CASE( 'constant' )


    ALLOCATE( FLOATOP_CONSTANT_T::FLOATOP, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FLOATOP%INIT( CFG, HOOKS )

  CASE( 'unary-op' )


    ALLOCATE( FLOATOP_UNARY_OP_T::FLOATOP, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FLOATOP%INIT( CFG, HOOKS )

    !> Read the encoder configuration
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'operand', HAS_FLOATOPS, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_FLOATOPS, ERRFLAG_OPERAND_UNDEFINED )

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATION( CFG, 'operand', NESTED_FLOATOP_CONFIGURATION, HOOKS )

    !> Check if configuration has the section keyword
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_TYPE_CFG) YAML_CONFIGURATION_HAS_KEY( NESTED_FLOATOP_CONFIGURATION, 'type', HAS_TYPE, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_TYPE, ERRFLAG_UDEFINED_TYPE )

    !> Get the section ID
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_TYPE_ID) YAML_READ_STRING( NESTED_FLOATOP_CONFIGURATION, 'type', NESTED_CTYPE, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(NESTED_CTYPE), ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ )

    !> Create the nested floatop
    TMP_OP => NULL()
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_NESTED_FLOATOP) MAKE_FLOATOP( TMP_OP, NESTED_CTYPE, NESTED_FLOATOP_CONFIGURATION, HOOKS )

    DEALLOCATE(NESTED_CTYPE, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE )

    SELECT TYPE(F => FLOATOP)
    CLASS IS (FLOATOP_UNARY_OP_T)

      !> Get FLOATOPs
      PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NESTED_FLOATOPS) F%SET( TMP_OP, HOOKS )

    CLASS DEFAULT

      PP_DEBUG_CRITICAL_THROW( ERRFLAG_WRONG_FLOATOP_TYPE )

    END SELECT

    !> Deallocate section configuration
    PP_TRYCALL( ERRFLAG_FLOATOP_TYPE_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( NESTED_FLOATOP_CONFIGURATION, HOOKS )

  CASE( 'binary-op' )

    ALLOCATE( FLOATOP_BINARY_OP_T::FLOATOP, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FLOATOP%INIT( CFG, HOOKS )

    !> Read the encoder configuration
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'operands', HAS_FLOATOPS, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_FLOATOPS, ERRFLAG_OPERAND_UNDEFINED )

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATIONS( CFG, 'operands', NESTED_FLOATOPS_CONFIGURATION, HOOKS )

    !> Get the sections size
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE) YAML_GET_CONFIGURATIONS_SIZE( NESTED_FLOATOPS_CONFIGURATION, N_FLOATOPS, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( N_FLOATOPS .NE. 2, ERRFLAG_WRONG_NUMBER_OF_OPERATIONS )

    !> Loop over sections
    DO I = 1 , N_FLOATOPS

      !> Get section configuration by ID
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG_BY_ID) YAML_GET_CONFIGURATION_BY_ID( NESTED_FLOATOPS_CONFIGURATION, I, NESTED_FLOATOP_CONFIGURATION, HOOKS )

      !> Check if configuration has the section keyword
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_TYPE_CFG) YAML_CONFIGURATION_HAS_KEY( NESTED_FLOATOP_CONFIGURATION, 'type', HAS_TYPE, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_TYPE, ERRFLAG_UDEFINED_TYPE )

      !> Get the section ID
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_TYPE_ID) YAML_READ_STRING( NESTED_FLOATOP_CONFIGURATION, 'type', NESTED_CTYPE, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(NESTED_CTYPE), ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ )

      !> Create the nested floatop
      TMP_OP => NULL()
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_NESTED_FLOATOP) MAKE_FLOATOP( TMP_OP, NESTED_CTYPE, NESTED_FLOATOP_CONFIGURATION, HOOKS )

      DEALLOCATE(NESTED_CTYPE, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE )

      SELECT TYPE(F => FLOATOP)
      CLASS IS (FLOATOP_BINARY_OP_T)

        !> Get FLOATOPs
        PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NESTED_FLOATOPS) F%SET( I, TMP_OP, HOOKS )

      CLASS DEFAULT

        PP_DEBUG_CRITICAL_THROW( ERRFLAG_WRONG_FLOATOP_TYPE )

      END SELECT

      !> Deallocate section configuration
      PP_TRYCALL( ERRFLAG_FLOATOP_TYPE_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( NESTED_FLOATOP_CONFIGURATION, HOOKS )

    ENDDO

    !> Deallocate sections
    PP_TRYCALL( ERRFLAG_FLOATOPS_TYPE_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATIONS( NESTED_FLOATOPS_CONFIGURATION, HOOKS )

  CASE ( 'function-call' )

    ALLOCATE( FLOATOP_FUNCTION_CALL_T::FLOATOP, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

    !> Initialization of the section
    PP_TRYCALL(ERRFLAG_INITIALIZATION_ERROR)  FLOATOP%INIT( CFG, HOOKS )

    !> Get the array of operands
    SELECT TYPE(F => FLOATOP)
    CLASS IS (FLOATOP_FUNCTION_CALL_T)

      !> Get FLOATOPs
      OPERANDS => NULL()
      PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NESTED_FLOATOPS) F%GET( OPERANDS, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(OPERANDS), ERRFLAG_OPERANDS_NOT_ASSOCIATED )

    CLASS DEFAULT

      PP_DEBUG_CRITICAL_THROW( ERRFLAG_WRONG_FLOATOP_TYPE )

    END SELECT

    !> Read the encoder configuration
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CFG, 'operands', HAS_FLOATOPS, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_FLOATOPS, ERRFLAG_OPERAND_UNDEFINED )

    !> Read all the subconfigurations
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_GET_SUBCONFIGURATIONS( CFG, 'operands', NESTED_FLOATOPS_CONFIGURATION, HOOKS )

    !> Get the sections size
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE) YAML_GET_CONFIGURATIONS_SIZE( NESTED_FLOATOPS_CONFIGURATION, N_FLOATOPS, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( N_FLOATOPS .NE. SIZE(OPERANDS), ERRFLAG_WRONG_NUMBER_OF_OPERATIONS )


    !> Loop over sections
    DO I = 1 , N_FLOATOPS

      !> Get section configuration by ID
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG_BY_ID) YAML_GET_CONFIGURATION_BY_ID( NESTED_FLOATOPS_CONFIGURATION, I, NESTED_FLOATOP_CONFIGURATION, HOOKS )

      !> Check if configuration has the section keyword
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_TYPE_CFG) YAML_CONFIGURATION_HAS_KEY( NESTED_FLOATOP_CONFIGURATION, 'type', HAS_TYPE, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_TYPE, ERRFLAG_UDEFINED_TYPE )

      !> Get the section ID
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_TYPE_ID) YAML_READ_STRING( NESTED_FLOATOP_CONFIGURATION, 'type', NESTED_CTYPE, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(NESTED_CTYPE), ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ )

      !> Create the nested floatop
      TMP_OP => OPERANDS(I)%OPERATION_
      PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_NESTED_FLOATOP) MAKE_FLOATOP( TMP_OP, NESTED_CTYPE, NESTED_FLOATOP_CONFIGURATION, HOOKS )

      !> Deallocate section configuration
      PP_TRYCALL( ERRFLAG_FLOATOP_TYPE_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( NESTED_FLOATOP_CONFIGURATION, HOOKS )

    ENDDO

    !> Deallocate sections
    PP_TRYCALL( ERRFLAG_FLOATOPS_TYPE_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATIONS( NESTED_FLOATOPS_CONFIGURATION, HOOKS )

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_FLOATOP )

  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit pofloat (On success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ! Initialization of bad path return value
  PP_SET_ERR_FAILURE( RET )

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    CHARACTER(LEN=64) :: TMPSTR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE  (ERRFLAG_ALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in allocation' )
    CASE  (ERRFLAG_INITIALIZATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in initialization' )
    CASE  (ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read configuration' )
    CASE  (ERRFLAG_OPERAND_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operand undefined' )
    CASE  (ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read subconfiguration' )
    CASE  (ERRFLAG_UNABLE_TO_READ_TYPE_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read type configuration' )
    CASE  (ERRFLAG_UDEFINED_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Undefined type' )
    CASE  (ERRFLAG_UNABLE_TO_READ_TYPE_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read type ID' )
    CASE  (ERRFLAG_TYPE_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Type not allocated after read' )
    CASE  (ERRFLAG_UNABLE_TO_READ_NESTED_FLOATOP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read nested floatop' )
    CASE  (ERRFLAG_UNABLE_TO_DEALLOCATE_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate type' )
    CASE  (ERRFLAG_WRONG_FLOATOP_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong floatop type' )
    CASE  (ERRFLAG_FLOATOP_TYPE_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in floatop type deallocation' )
    CASE  (ERRFLAG_WRONG_NUMBER_OF_OPERATIONS)
      TMPSTR = REPEAT(' ',64)
      WRITE(TMPSTR,*) N_FLOATOPS
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong number of operations: '//TRIM(ADJUSTL(TMPSTR)) )
    CASE  (ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get subconfiguration size' )
    CASE  (ERRFLAG_UNABLE_TO_GET_NESTED_FLOATOPS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get nested floatops' )
    CASE  (ERRFLAG_UNABLE_TO_READ_CFG_BY_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read configuration by ID' )
    CASE  (ERRFLAG_FLOATOPS_TYPE_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in floatops type deallocation' )
    CASE  (ERRFLAG_UNKNOWN_FLOATOP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown floatop' )
    CASE  (ERRFLAG_OPERANDS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operands not associated' )
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

  ! Exit pofloat (on error)
  RETURN

END FUNCTION MAKE_FLOATOP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Destroys a encoder structure.
!>
!> This function takes a encoder object (`FLOATOP`) and performs
!> the necessary cleanup, freeing any associated resources. It also supports a
!> `VERBOSE` mode for detailed output during the destruction process.
!>
!> @section floaterface
!> @param[in,out] FLOATOP The encoder object to be destroyed.
!>                              The structure is modified in place.
!> @param [inout] HOOKS Utilities to be used for logging, debugging, tracing and option handling
!>
!> @return INTEGER error code (`RET`) indicating success or failure of the operation.
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
!> @see MAKE_FLOATOP
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DESTROY_FLOATOP'
RECURSIVE FUNCTION DESTROY_FLOATOP( FLOATOP, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,         ONLY: JPIB_K
  USE :: FLOATOP_BASE_MOD,          ONLY: FLOATOP_BASE_A
  USE :: FLOATOP_BASE_MOD,          ONLY: FLOATOP_CONTAINER_T
  USE :: FLOATOP_FUNCTION_CALL_MOD, ONLY: FLOATOP_FUNCTION_CALL_T
  USE :: FLOATOP_BINARY_OP_MOD,     ONLY: FLOATOP_BINARY_OP_T
  USE :: FLOATOP_UNARY_OP_MOD,      ONLY: FLOATOP_UNARY_OP_T
  USE :: FLOATOP_CONSTANT_MOD,      ONLY: FLOATOP_CONSTANT_T
  USE :: HOOKS_MOD,                 ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(FLOATOP_BASE_A), POINTER, INTENT(INOUT) :: FLOATOP
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CLASS(FLOATOP_BASE_A), POINTER :: NESTED_FLOATOP
  TYPE(FLOATOP_CONTAINER_T), POINTER, DIMENSION(:) :: OPERANDS

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ASSOCIATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_NESTED_FLOATOPS = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NESTED_FLOATOPS_NOT_ASSOCIATED = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NESTED_DESTRUCTOR_ERROR = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_ERROR = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OPERANDS_NOT_ASSOCIATED = 7_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(FLOATOP), ERRFLAG_NOT_ASSOCIATED )

  ! Deallocate floatops
  SELECT TYPE(F => FLOATOP)

  CLASS IS (FLOATOP_FUNCTION_CALL_T)

    OPERANDS => NULL()
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NESTED_FLOATOPS) F%GET( OPERANDS, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(OPERANDS), ERRFLAG_OPERANDS_NOT_ASSOCIATED )

    DO I = 1 , SIZE(OPERANDS)
      NESTED_FLOATOP => OPERANDS(I)%OPERATION_
      PP_TRYCALL(ERRFLAG_NESTED_DESTRUCTOR_ERROR) DESTROY_FLOATOP( NESTED_FLOATOP, HOOKS )
    ENDDO
    OPERANDS => NULL()
    PP_TRYCALL(ERRFLAG_FREE_ERROR) FLOATOP%FREE( HOOKS )


    DEALLOCATE( FLOATOP, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )

  CLASS IS (FLOATOP_BINARY_OP_T)

    NESTED_FLOATOP => NULL()
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NESTED_FLOATOPS) F%GET( 1_JPIB_K, NESTED_FLOATOP, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(NESTED_FLOATOP), ERRFLAG_NESTED_FLOATOPS_NOT_ASSOCIATED )
    PP_TRYCALL(ERRFLAG_NESTED_DESTRUCTOR_ERROR) DESTROY_FLOATOP( NESTED_FLOATOP, HOOKS )

    NESTED_FLOATOP => NULL()
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NESTED_FLOATOPS) F%GET( 2_JPIB_K, NESTED_FLOATOP, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(NESTED_FLOATOP), ERRFLAG_NESTED_FLOATOPS_NOT_ASSOCIATED )
    PP_TRYCALL(ERRFLAG_NESTED_DESTRUCTOR_ERROR) DESTROY_FLOATOP( NESTED_FLOATOP, HOOKS )

    NESTED_FLOATOP => NULL()
    PP_TRYCALL(ERRFLAG_FREE_ERROR) FLOATOP%FREE( HOOKS )
    DEALLOCATE( FLOATOP, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )

  CLASS IS (FLOATOP_UNARY_OP_T)

    NESTED_FLOATOP => NULL()
    PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NESTED_FLOATOPS) F%GET( NESTED_FLOATOP, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(NESTED_FLOATOP), ERRFLAG_NESTED_FLOATOPS_NOT_ASSOCIATED )
    PP_TRYCALL(ERRFLAG_NESTED_DESTRUCTOR_ERROR) DESTROY_FLOATOP( NESTED_FLOATOP, HOOKS )

    NESTED_FLOATOP => NULL()
    PP_TRYCALL(ERRFLAG_FREE_ERROR) FLOATOP%FREE( HOOKS )
    DEALLOCATE( FLOATOP, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )

  CLASS DEFAULT

    PP_TRYCALL(ERRFLAG_FREE_ERROR)  FLOATOP%FREE( HOOKS )
    DEALLOCATE( FLOATOP, STAT=DEALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STATUS.NE.0, ERRFLAG_DEALLOCATION_ERROR )

  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit pofloat (on success)
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
    CASE  (ERRFLAG_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The section is not associated' )
    CASE  (ERRFLAG_UNABLE_TO_GET_NESTED_FLOATOPS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to get nested floatops' )
    CASE  (ERRFLAG_NESTED_FLOATOPS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Nested floatops not associated' )
    CASE  (ERRFLAG_NESTED_DESTRUCTOR_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in nested destructor' )
    CASE  (ERRFLAG_FREE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in free' )
    CASE  (ERRFLAG_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in deallocation' )
    CASE (ERRFLAG_OPERANDS_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Operands not associated' )
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

  ! Exit pofloat (on error)
  RETURN

END FUNCTION DESTROY_FLOATOP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE FLOATOP_FACTORY_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
