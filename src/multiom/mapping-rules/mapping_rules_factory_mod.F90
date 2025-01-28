
! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'mapping_rules_factory_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MAPPING_RULES_FACTORY_MOD'
MODULE MAPPING_RULES_FACTORY_MOD
IMPLICIT NONE

PRIVATE

PUBLIC :: MAKE_MAPPING_RULES
PUBLIC :: DESTROY_MAPPING_RULES

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_MAPPING_RULES'
PP_THREAD_SAFE FUNCTION MAKE_MAPPING_RULES( RULES, &
&   RULES_CONFIGURATION, FILTER_OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,     ONLY: JPIB_K
  USE :: MAPPING_RULE_MOD,      ONLY: MAPPING_RULE_CONTAINER_T
  USE :: GRIB_SECTION_BASE_MOD, ONLY: GRIB_SECTION_BASE_A
  USE :: GRIB2_ENCODER_MOD,     ONLY: GRIB2_ENCODER_T
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_CONFIGURATIONS_T
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_DELETE_CONFIGURATION
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_GET_SUBCONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_GET_CONFIGURATIONS_SIZE
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_GET_CONFIGURATION_BY_ID
  USE :: YAML_CORE_UTILS_MOD,   ONLY: YAML_DELETE_CONFIGURATIONS
  USE :: FILTER_OPTIONS_MOD,    ONLY: FILTER_OPTIONS_T
  USE :: HOOKS_MOD,             ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAPPING_RULE_CONTAINER_T), POINTER, DIMENSION(:), INTENT(INOUT) :: RULES
  TYPE(YAML_CONFIGURATIONS_T),                           INTENT(IN)    :: RULES_CONFIGURATION
  TYPE(FILTER_OPTIONS_T),                                INTENT(IN)    :: FILTER_OPT
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: HAS_RULE
  LOGICAL :: HAS_FILE
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: N_RULES
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  TYPE(YAML_CONFIGURATION_T) :: RULE_CONFIGURATION

  ! Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_FILE_DOES_NOT_EXIST=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_FILE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECTIONS_UNDEFINED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SUBCFG=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_NUMBER_OF_RULES=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_RULES=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_CFG_BY_ID=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONSTRUCT_THE_RULE=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULE_DEALLOCATION_ERROR=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DELETE_CONFIGURATIONS=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_THE_RULE=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HAS_KEY=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_RULE_FROM_CFG=14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_RULE_FROM_FILE=15_JPIB_K

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

  !> Get the sections size
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE) YAML_GET_CONFIGURATIONS_SIZE( RULES_CONFIGURATION, N_RULES, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( N_RULES .LE. 0, ERRFLAG_WRONG_NUMBER_OF_RULES )

  ALLOCATE(RULES(N_RULES), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_RULES )

  !> Loop over sections
  DO I = 1, N_RULES

    !> Get section configuration by ID
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG_BY_ID) YAML_GET_CONFIGURATION_BY_ID( RULES_CONFIGURATION, I, RULE_CONFIGURATION, HOOKS )

    !> Create the rule object
    PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( RULE_CONFIGURATION, "rule", HAS_RULE, HOOKS )
    PP_TRYCALL(ERRFLAG_HAS_KEY) YAML_CONFIGURATION_HAS_KEY( RULE_CONFIGURATION, "file", HAS_FILE, HOOKS )

    IF ( HAS_RULE .AND. .NOT.HAS_FILE ) THEN

      !> Read rule from configureation
      PP_TRYCALL(ERRFLAG_READ_RULE_FROM_CFG) MAKE_RULE_FROM_CFG( RULES(I), I, RULE_CONFIGURATION, FILTER_OPT, HOOKS )

      !> Deallocate section configuration
      PP_TRYCALL(ERRFLAG_RULE_DEALLOCATION_ERROR) YAML_DELETE_CONFIGURATION( RULE_CONFIGURATION, HOOKS )

    ELSE IF ( HAS_FILE .AND. .NOT. HAS_RULE) THEN

      !> Read rule from file
      PP_TRYCALL(ERRFLAG_READ_RULE_FROM_FILE) MAKE_RULE_FROM_FILE( RULES(I), I, RULE_CONFIGURATION, FILTER_OPT, HOOKS )

      !> Deallocate section configuration
      PP_TRYCALL( ERRFLAG_RULE_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( RULE_CONFIGURATION, HOOKS )

    ELSE

      !> Deallocate section configuration
      PP_TRYCALL( ERRFLAG_RULE_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( RULE_CONFIGURATION, HOOKS )
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_SECTIONS_UNDEFINED )
    END IF

  ENDDO

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

    ! Initialize error frame
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_CFG_FILE_DOES_NOT_EXIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The configuration file does not exist' )
    CASE(ERRFLAG_CFG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error opening the configuration file' )
    CASE(ERRFLAG_UNABLE_TO_READ_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading the configuration file' )
    CASE(ERRFLAG_SECTIONS_UNDEFINED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The sections are not defined' )
    CASE(ERRFLAG_UNABLE_TO_READ_SUBCFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading the subconfigurations' )
    CASE(ERRFLAG_UNABLE_TO_GET_SUBCFG_SIZE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error getting the subconfigurations size' )
    CASE(ERRFLAG_WRONG_NUMBER_OF_RULES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The number of rules is wrong' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE_RULES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error allocating the rules' )
    CASE(ERRFLAG_UNABLE_TO_READ_CFG_BY_ID)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading the configuration by ID' )
    CASE(ERRFLAG_UNABLE_TO_CONSTRUCT_THE_RULE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error constructing the rule' )
    CASE(ERRFLAG_RULE_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error deallocating the rule' )
    CASE(ERRFLAG_DELETE_CONFIGURATIONS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error deleting the configuration object' )
    CASE(ERRFLAG_UNABLE_TO_PRINT_THE_RULE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error printing the rule' )
    CASE(ERRFLAG_HAS_KEY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error checking the key' )
    CASE(ERRFLAG_READ_RULE_FROM_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading the rule from configuration' )
    CASE(ERRFLAG_READ_RULE_FROM_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading the rule from file' )
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

END FUNCTION MAKE_MAPPING_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_RULE_FROM_CFG'
RECURSIVE FUNCTION MAKE_RULE_FROM_CFG( RULE, ID, CFG, FILTER_OPT, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: MAPPING_RULE_MOD,         ONLY: MAPPING_RULE_CONTAINER_T
  USE :: FILTER_OPTIONS_MOD,       ONLY: FILTER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAPPING_RULE_CONTAINER_T), INTENT(INOUT) :: RULE
  INTEGER(KIND=JPIB_K),           INTENT(IN)    :: ID
  TYPE(YAML_CONFIGURATION_T),     INTENT(IN)    :: CFG
  TYPE(FILTER_OPTIONS_T),         INTENT(IN)    :: FILTER_OPT
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ASSOCIATED=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_RULES=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONSTRUCT_THE_RULE=2_JPIB_K


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

  !> Check if the rule is associated
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(RULE%RULE_), ERRFLAG_ALREADY_ASSOCIATED )

  !> Create the rule object
  ALLOCATE(RULE%RULE_, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_RULES )

  !> Read the rule
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONSTRUCT_THE_RULE) RULE%RULE_%INIT( ID, CFG, FILTER_OPT, HOOKS )

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
    CASE(ERRFLAG_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'rule is already associated' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE_RULES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating rule' )
    CASE(ERRFLAG_UNABLE_TO_CONSTRUCT_THE_RULE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error constructing rule' )
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

END FUNCTION MAKE_RULE_FROM_CFG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MAKE_RULE_FROM_FILE'
RECURSIVE FUNCTION MAKE_RULE_FROM_FILE( RULE, ID, CFG, FILTER_OPT,HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: MAPPING_RULE_MOD,         ONLY: MAPPING_RULE_CONTAINER_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FILTER_OPTIONS_MOD,       ONLY: FILTER_OPTIONS_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_READ_STRING_WITH_ENV_EXPANSION
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_NEW_CONFIGURATION_FROM_FILE
  USE :: YAML_CORE_UTILS_MOD,      ONLY: YAML_DELETE_CONFIGURATION
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MAPPING_RULE_CONTAINER_T),       INTENT(INOUT) :: RULE
  INTEGER(KIND=JPIB_K),         INTENT(IN)    :: ID
  TYPE(YAML_CONFIGURATION_T),   INTENT(IN)    :: CFG
  TYPE(FILTER_OPTIONS_T),       INTENT(IN)    :: FILTER_OPT
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: FEXIST
  TYPE(YAML_CONFIGURATION_T) :: LOC_CFG
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  CHARACTER(LEN=:), ALLOCATABLE :: FNAME

  ! Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ASSOCIATED=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE_RULES=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONSTRUCT_THE_RULE=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_FILE_DOES_NOT_EXIST=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED_AFTER_READ=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_FILE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULE_DEALLOCATION_ERROR=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_ERROR=7_JPIB_K


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

  !> Check if the rule is associated
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(RULE%RULE_), ERRFLAG_ALREADY_ASSOCIATED )

  !> Read the name of the file
  PP_TRYCALL(ERRFLAG_READ_ERROR) YAML_READ_STRING_WITH_ENV_EXPANSION( CFG, 'file', FNAME, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(FNAME), ERRFLAG_NOT_ALLOCATED_AFTER_READ )

  !> TODO: Expand environment variables

  !> Read the configuration from the file
  INQUIRE( FILE=FNAME, EXIST=FEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. FEXIST, ERRFLAG_CFG_FILE_DOES_NOT_EXIST )

  !> Read the configuration from the file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_FILE) YAML_NEW_CONFIGURATION_FROM_FILE( TRIM(ADJUSTL(FNAME)), LOC_CFG, HOOKS )

  !> Create the rule object
  ALLOCATE(RULE%RULE_, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE_RULES )

  !> Read the rule
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONSTRUCT_THE_RULE) RULE%RULE_%INIT( ID, LOC_CFG, FILTER_OPT, HOOKS )

  !> Free local configuration
  PP_TRYCALL(ERRFLAG_RULE_DEALLOCATION_ERROR) YAML_DELETE_CONFIGURATION( LOC_CFG, HOOKS )

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
    CASE(ERRFLAG_ALREADY_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'rule is already associated' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE_RULES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating rule' )
    CASE(ERRFLAG_UNABLE_TO_CONSTRUCT_THE_RULE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error constructing rule' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'file name: '//TRIM(ADJUSTL(FNAME)) )
    CASE(ERRFLAG_CFG_FILE_DOES_NOT_EXIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The configuration file does not exist' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'file name: '//TRIM(ADJUSTL(FNAME)) )
    CASE(ERRFLAG_NOT_ALLOCATED_AFTER_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error reading the file name' )
    CASE(ERRFLAG_UNABLE_TO_READ_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error reading the file' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'file name: '//TRIM(ADJUSTL(FNAME)) )
    CASE(ERRFLAG_RULE_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating rule' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'file name: '//TRIM(ADJUSTL(FNAME)) )
    CASE(ERRFLAG_READ_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error reading the file' )
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

END FUNCTION MAKE_RULE_FROM_FILE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DESTROY_MAPPING_RULES'
RECURSIVE FUNCTION DESTROY_MAPPING_RULES( RULES, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: MAPPING_RULE_MOD,    ONLY: MAPPING_RULE_CONTAINER_T
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
  TYPE(MAPPING_RULE_CONTAINER_T), POINTER, DIMENSION(:), INTENT(INOUT) :: RULES
  TYPE(HOOKS_T),                                         INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ASSOCIATED=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULE_FREE_ERROR=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULE_DEALLOCATION_ERROR=2_JPIB_K


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

  !> Check if the rules are associated
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(RULES), ERRFLAG_NOT_ASSOCIATED )


  !> Free the rules
  DO I = 1, SIZE(RULES)

    !> Check if the rule is associated
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(RULES(I)%RULE_), ERRFLAG_NOT_ASSOCIATED )

    !> Destroy the rule object
    PP_TRYCALL(ERRFLAG_RULE_FREE_ERROR) RULES(I)%RULE_%FREE( HOOKS )

    !> Deallocate nested rule
    DEALLOCATE(RULES(I)%RULE_, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_RULE_DEALLOCATION_ERROR )
    NULLIFY(RULES(I)%RULE_)

  ENDDO

  !> Deallocate rules
  DEALLOCATE(RULES, STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS .NE. 0, ERRFLAG_RULE_DEALLOCATION_ERROR )
  NULLIFY(RULES)


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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'rules are not associated' )
    CASE(ERRFLAG_RULE_FREE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error freeing the rule' )
    CASE(ERRFLAG_RULE_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating the rule' )
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

END FUNCTION DESTROY_MAPPING_RULES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE MAPPING_RULES_FACTORY_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME