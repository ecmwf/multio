! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'multiom_list_cfg_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MULTIOM_LIST_CFG_MOD'
MODULE MULTIOM_LIST_CFG_MOD

  ! Symbols imported from the main library of the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: CMDARG_STRLEN

IMPLICIT NONE

! Default visibility
PRIVATE

! Local enumerators
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_VERBOSE_E = 1_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_DUMP_PATH_E = 2_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_USE_VALUES_E = 3_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_OUTPUT_MANAGER_E = 4_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_CONFIGURATION_FILE_E = 5_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_SEED_E = 6_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_READ_BIG_ENDIAN_E = 7_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_PROCESSORS_E = 8_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_PARAM_E = 9_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_REPRES_E = 10_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_LEVTYPE_E = 11_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_LEVEL_E = 12_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_FREQUENCY_E = 13_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_DIRCTION_E = 14_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_FILTER_FILE_E = 15_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: ARG_LAST_E = 15_JPIB_K

! Calls used to handle command line arguments
TYPE :: COMMAND_LINE_ARGS_T

  ! General behaviour
  LOGICAL :: VERBOSE=.FALSE.
  LOGICAL :: BIG_ENDIAN_READ=.FALSE.
  LOGICAL :: USE_VALUES=.FALSE.
  INTEGER(KIND=JPIB_K) :: SEED=0_JPIB_K

  ! Main configuration switches
  CHARACTER(LEN=CMDARG_STRLEN) :: OUTPUT_MANAGER_TYPE=REPEAT(' ',CMDARG_STRLEN)
  CHARACTER(LEN=CMDARG_STRLEN) :: INPUT_DIR=REPEAT(' ',CMDARG_STRLEN)
  CHARACTER(LEN=CMDARG_STRLEN) :: YAML_CONFIGURATION=REPEAT(' ',CMDARG_STRLEN)
  CHARACTER(LEN=CMDARG_STRLEN) :: FILTERS_FILE=REPEAT(' ',CMDARG_STRLEN)


  ! Which data to be loaded
  LOGICAL :: MATCH_PROCESSORS=.TRUE.
  INTEGER(JPIB_K), DIMENSION(:), ALLOCATABLE :: PROCESSORS


  ! Filters to be applied to the fields to be processed
  LOGICAL :: MATCH_PARAM=.TRUE.
  INTEGER(JPIB_K), DIMENSION(:), ALLOCATABLE :: PARAM

  LOGICAL :: MATCH_LEVTYPE=.TRUE.
  INTEGER(JPIB_K), DIMENSION(:), ALLOCATABLE :: LEVTYPE

  LOGICAL :: MATCH_REPRES=.TRUE.
  INTEGER(JPIB_K), DIMENSION(:), ALLOCATABLE :: REPRES

  LOGICAL :: MATCH_STEP=.TRUE.
  INTEGER(JPIB_K), DIMENSION(:), ALLOCATABLE :: STEP

  LOGICAL :: MATCH_LEVEL=.TRUE.
  INTEGER(JPIB_K), DIMENSION(:), ALLOCATABLE :: LEVEL

  LOGICAL :: MATCH_FREQUENCY=.TRUE.
  INTEGER(JPIB_K), DIMENSION(:), ALLOCATABLE :: FREQUENCY

  LOGICAL :: MATCH_DIRECTION=.TRUE.
  INTEGER(JPIB_K), DIMENSION(:), ALLOCATABLE :: DIRECTION

CONTAINS


  PROCEDURE, PUBLIC,  PASS, NON_OVERRIDABLE :: INIT => INIT_COMMAND_LINE_ARGS
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: INIT_DEFAULT => DEFAULT_COMMAND_LINE_ARGS
  PROCEDURE, PUBLIC,  PASS, NON_OVERRIDABLE :: GET_CONFIGURATION_FILE => COMMAND_LINE_ARGS_GET_CONFIGURATION_FILE
  PROCEDURE, PUBLIC,  PASS, NON_OVERRIDABLE :: GET_VERBOSE => COMMAND_LINE_ARGS_GET_VERBOSE
  PROCEDURE, PUBLIC,  PASS, NON_OVERRIDABLE :: GET_TYPE => COMMAND_LINE_ARGS_GET_TYPE
  PROCEDURE, PUBLIC,  PASS, NON_OVERRIDABLE :: GET_PROCESSOR_LIST => COMMAND_LINE_ARGS_GET_PROCESSOR_LIST
  PROCEDURE, PUBLIC,  PASS, NON_OVERRIDABLE :: GET_DUMP_PATH => COMMAND_LINE_ARGS_GET_DUMP_PATH
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: MATCH_ATM => COMMAND_LINE_ARGS_MATCH_ATM
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: MATCH_WAM => COMMAND_LINE_ARGS_MATCH_WAM
  GENERIC, PUBLIC :: MATCH => MATCH_ATM, MATCH_WAM
  PROCEDURE, PUBLIC,  PASS, NON_OVERRIDABLE :: FREE => FREE_COMMAND_LINE_ARGS


  ! Convert command line arguments to internal arguments
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: CARG2IARG_SHORT => CMDARG_CARG2IARG_SHORT
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: CARG2IARG_LONG => CMDARG_CARG2IARG_LONG
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: CHECK_ARGS_CONSISTENCY => CMDARG_CHECK_ARGS_CONSISTENCY


  ! Private methods to read command arguments
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: PARSE_VERBOSE => CMDARG_PARSE_VERBOSE
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: PARSE_DUMP_PATH => CMDARG_PARSE_DUMP_PATH
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: PARSE_USE_VALUES => CMDARG_PARSE_USE_VALUES
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: PARSE_OUTPUT_MANAGER_TYPE => CMDARG_PARSE_OUTPUT_MANAGER_TYPE
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: PARSE_CONFIGURATION_FILE => CMDARG_PARSE_CONFIGURATION_FILE
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: PARSE_PROCESSORS_LIST => CMDARG_PARSE_PROCESSORS_LIST
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: PARSE_PARAM => CMDARG_PARSE_PARAM
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: PARSE_REPRES => CMDARG_PARSE_REPRES
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: PARSE_LEVTYPE => CMDARG_PARSE_LEVTYPE
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: PARSE_LEVEL => CMDARG_PARSE_LEVEL
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: PARSE_STEP => CMDARG_PARSE_STEP
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: PARSE_FREQUENCY => CMDARG_PARSE_FREQUENCY
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: PARSE_DIRECTION => CMDARG_PARSE_DIRECTION
  PROCEDURE, PRIVATE, PASS, NON_OVERRIDABLE :: PARSE_FILTER_FILE => CMDARG_PARSE_FILTER_FILE


END TYPE

! Whitelist of public symbols
PUBLIC :: COMMAND_LINE_ARGS_T

CONTAINS



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INIT_COMMAND_LINE_ARGS'
PP_THREAD_SAFE FUNCTION INIT_COMMAND_LINE_ARGS( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: COMMAND_ARGUMENTS_TOKENIZER_T
  USE :: MULTIOM_API, ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  TYPE(COMMAND_ARGUMENTS_TOKENIZER_T) :: CMDARGS
  INTEGER(KIND=JPIB_K) :: ARGID
  INTEGER(KIND=JPIB_K) :: ARGSCOUNT
  CHARACTER(LEN=CMDARG_STRLEN) :: ARG_NAME
  INTEGER(KIND=JPIB_K) :: MODIFIER_COUNT
  INTEGER(KIND=JPIB_K) :: ARG_ENUM
  INTEGER(KIND=JPIB_K), DIMENSION(ARG_LAST_E) :: ARGS_CHECKER
  LOGICAL :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER :: VALUES


  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INIT_CMDARGS = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SIZE_CMDARGS = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NORMALISE_MODIFIER = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_MODIFIER = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHECK_ARGS_CONSISTENCY = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_ARGUMENT = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_VERBOSE = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_DUMP_PATH = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_USE_VALUES = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_OUTPUT_MANAGER_TYPE = 10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_CONFIGURATION_FILE = 11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_PROCESSORS = 14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_PARAM = 15_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_REPRES = 16_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_LEVTYPE = 17_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_LEVEL = 18_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_STEP = 19_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_FREQUENCY = 20_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_DIRECTION = 21_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_FILTER_FILE = 22_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_CMDARGS = 23_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INIT_DEFAULT_CMDARGS = 24_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PRINT_CMDARGS = 25_JPIB_K


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

  ! Initialize to the defaults
  PP_TRYCALL(ERRFLAG_INIT_DEFAULT_CMDARGS) THIS%INIT_DEFAULT( HOOKS )

  ! Initialize the command arguments tokenizer
  PP_TRYCALL(ERRFLAG_INIT_CMDARGS) CMDARGS%INIT( HOOKS )

  ! Print tokenized command line arguments
  PP_TRYCALL(ERRFLAG_PRINT_CMDARGS) CMDARGS%PRINT( 6_JPIB_K, HOOKS )

  ! Consume the command line arguments
  PP_TRYCALL(ERRFLAG_SIZE_CMDARGS) CMDARGS%SIZE( ARGSCOUNT, HOOKS )

  ! Initialize the args checker
  ARGS_CHECKER = 0_JPIB_K

  ! Consume the command line arguments
  DO ARGID = 1, ARGSCOUNT

    ! Initialize the values
    MODIFIER_COUNT = -1_JPIB_K
    ARG_NAME = REPEAT(' ',CMDARG_STRLEN)
    MATCH = .TRUE.
    VALUES => NULL()

    ! Get the name of the flag and the modifier
    PP_TRYCALL(ERRFLAG_SIZE_CMDARGS) CMDARGS%GET_MODIFIER( ARGID, MODIFIER_COUNT, HOOKS )
    PP_TRYCALL(ERRFLAG_SIZE_CMDARGS) CMDARGS%GET_NAME( ARGID, ARG_NAME, HOOKS )
    PP_TRYCALL(ERRFLAG_SIZE_CMDARGS) CMDARGS%GET_MATCH( ARGID, MATCH, HOOKS )
    PP_TRYCALL(ERRFLAG_SIZE_CMDARGS) CMDARGS%GET_VALUES( ARGID, VALUES, HOOKS )

    ! Normalize arguments name. Modifier cound is the number of '-' in the name of the argument
    ! This part of the code is used to parse the names and convert them to enum values
    SELECT CASE(MODIFIER_COUNT)
    CASE (1)
      PP_TRYCALL(ERRFLAG_NORMALISE_MODIFIER) THIS%CARG2IARG_SHORT( ARG_NAME, ARG_ENUM, HOOKS )
    CASE (2)
      PP_TRYCALL(ERRFLAG_NORMALISE_MODIFIER) THIS%CARG2IARG_LONG( ARG_NAME, ARG_ENUM, HOOKS )
    CASE DEFAULT
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_MODIFIER )
    END SELECT

    ! Update arguments checker
    ARGS_CHECKER(ARG_ENUM) = ARGS_CHECKER(ARG_ENUM) + 1_JPIB_K

    ! Check for consistency of the input flags (some flags can not be used together)
    PP_TRYCALL(ERRFLAG_CHECK_ARGS_CONSISTENCY) THIS%CHECK_ARGS_CONSISTENCY( ARGS_CHECKER, HOOKS )


    !
    ! Parse the command line arguments
    SELECT CASE(ARG_ENUM)

    ! Management of the general behaviour
    CASE ( ARG_VERBOSE_E )
      PP_TRYCALL(ERRFLAG_PARSE_VERBOSE) THIS%PARSE_VERBOSE( MATCH, VALUES, HOOKS )

    CASE ( ARG_DUMP_PATH_E )
      PP_TRYCALL(ERRFLAG_PARSE_DUMP_PATH) THIS%PARSE_DUMP_PATH( MATCH, VALUES, HOOKS )

    CASE ( ARG_USE_VALUES_E )
      PP_TRYCALL(ERRFLAG_PARSE_USE_VALUES) THIS%PARSE_USE_VALUES( MATCH, VALUES, HOOKS )

    CASE ( ARG_OUTPUT_MANAGER_E )
      PP_TRYCALL(ERRFLAG_PARSE_OUTPUT_MANAGER_TYPE) THIS%PARSE_OUTPUT_MANAGER_TYPE( MATCH, VALUES, HOOKS )

    CASE ( ARG_CONFIGURATION_FILE_E )
      PP_TRYCALL(ERRFLAG_PARSE_CONFIGURATION_FILE) THIS%PARSE_CONFIGURATION_FILE( MATCH, VALUES, HOOKS )

    CASE ( ARG_PROCESSORS_E )
      PP_TRYCALL(ERRFLAG_PARSE_PROCESSORS) THIS%PARSE_PROCESSORS_LIST( MATCH, VALUES, HOOKS )

    ! Filters (from command line)
    CASE ( ARG_PARAM_E )
      PP_TRYCALL(ERRFLAG_PARSE_PARAM) THIS%PARSE_PARAM( MATCH, VALUES, HOOKS )

    CASE ( ARG_REPRES_E )
      PP_TRYCALL(ERRFLAG_PARSE_REPRES) THIS%PARSE_REPRES( MATCH, VALUES, HOOKS )

    CASE ( ARG_LEVTYPE_E )
      PP_TRYCALL(ERRFLAG_PARSE_LEVTYPE) THIS%PARSE_LEVTYPE( MATCH, VALUES, HOOKS )

    CASE ( ARG_LEVEL_E )
      PP_TRYCALL(ERRFLAG_PARSE_LEVEL) THIS%PARSE_LEVEL( MATCH, VALUES, HOOKS )

    CASE ( ARG_FREQUENCY_E )
      PP_TRYCALL(ERRFLAG_PARSE_FREQUENCY) THIS%PARSE_FREQUENCY( MATCH, VALUES, HOOKS )

    CASE ( ARG_DIRCTION_E )
      PP_TRYCALL(ERRFLAG_PARSE_DIRECTION) THIS%PARSE_DIRECTION( MATCH, VALUES, HOOKS )

    ! Filters (file)
    CASE ( ARG_FILTER_FILE_E )
      PP_TRYCALL(ERRFLAG_PARSE_FILTER_FILE) THIS%PARSE_FILTER_FILE( MATCH, VALUES, HOOKS )

    CASE DEFAULT
        PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_ARGUMENT )

    END SELECT

  END DO

  ! Initialize the command arguments tokenizer
  PP_TRYCALL(ERRFLAG_FREE_CMDARGS) CMDARGS%FREE( HOOKS )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_INIT_CMDARGS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error initializing the command arguments tokenizer' )
    CASE (ERRFLAG_SIZE_CMDARGS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error getting the size of the command arguments' )
    CASE (ERRFLAG_NORMALISE_MODIFIER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error normalising the modifier' )
    CASE (ERRFLAG_INVALID_MODIFIER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid modifier' )
    CASE (ERRFLAG_CHECK_ARGS_CONSISTENCY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error checking the consistency of the command line arguments' )
    CASE (ERRFLAG_UNKNOWN_ARGUMENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown argument' )
    CASE (ERRFLAG_PARSE_VERBOSE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing the verbose flag' )
    CASE (ERRFLAG_PARSE_DUMP_PATH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing the dump path flag' )
    CASE (ERRFLAG_PARSE_USE_VALUES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing the use values flag' )
    CASE (ERRFLAG_PARSE_OUTPUT_MANAGER_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing the output manager type flag' )
    CASE (ERRFLAG_PARSE_CONFIGURATION_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing the configuration file flag' )
    CASE (ERRFLAG_PARSE_PROCESSORS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing the processors flag' )
    CASE (ERRFLAG_PARSE_PARAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing the param flag' )
    CASE (ERRFLAG_PARSE_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing the repres flag' )
    CASE (ERRFLAG_PARSE_LEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing the levtype flag' )
    CASE (ERRFLAG_PARSE_LEVEL)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing the level flag' )
    CASE (ERRFLAG_PARSE_STEP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing the step flag' )
    CASE (ERRFLAG_PARSE_FREQUENCY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing the frequency flag' )
    CASE (ERRFLAG_PARSE_DIRECTION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing the direction flag' )
    CASE (ERRFLAG_PARSE_FILTER_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing the filter file flag' )
    CASE (ERRFLAG_FREE_CMDARGS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error freeing the command arguments tokenizer' )
    CASE (ERRFLAG_INIT_DEFAULT_CMDARGS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error initializing the default command line arguments' )
    CASE (ERRFLAG_PRINT_CMDARGS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error printing command line arguments' )
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

END FUNCTION INIT_COMMAND_LINE_ARGS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FREE_COMMAND_LINE_ARGS'
PP_THREAD_SAFE FUNCTION FREE_COMMAND_LINE_ARGS( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: COMMAND_ARGUMENTS_TOKENIZER_T
  USE :: MULTIOM_API, ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

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

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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

END FUNCTION FREE_COMMAND_LINE_ARGS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_LINE_ARGS_GET_CONFIGURATION_FILE'
PP_THREAD_SAFE FUNCTION COMMAND_LINE_ARGS_GET_CONFIGURATION_FILE( THIS, CONFIGURATION_FILE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: COMMAND_ARGUMENTS_TOKENIZER_T
  USE :: MULTIOM_API, ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),           INTENT(OUT)   :: CONFIGURATION_FILE
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_LEN = 1_JPIB_K

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

  ! Wrong length
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(THIS%YAML_CONFIGURATION).NE.LEN(CONFIGURATION_FILE), ERRFLAG_WRONG_LEN )

  ! Copy the configuration file
  CONFIGURATION_FILE = REPEAT( ' ', LEN(CONFIGURATION_FILE) )
  CONFIGURATION_FILE = TRIM(ADJUSTL(THIS%YAML_CONFIGURATION))


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_WRONG_LEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong length of the configuration file' )
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

END FUNCTION COMMAND_LINE_ARGS_GET_CONFIGURATION_FILE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_LINE_ARGS_GET_DUMP_PATH'
PP_THREAD_SAFE FUNCTION COMMAND_LINE_ARGS_GET_DUMP_PATH( THIS, DUMP_PATH, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: COMMAND_ARGUMENTS_TOKENIZER_T
  USE :: MULTIOM_API, ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),           INTENT(OUT)   :: DUMP_PATH
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_LEN = 1_JPIB_K

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

  ! Wrong length
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(THIS%INPUT_DIR).NE.LEN(DUMP_PATH), ERRFLAG_WRONG_LEN )

  ! Copy the configuration file
  DUMP_PATH = REPEAT( ' ', LEN(DUMP_PATH) )
  DUMP_PATH = TRIM(ADJUSTL(THIS%INPUT_DIR))


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_WRONG_LEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong length of the configuration file' )
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

END FUNCTION COMMAND_LINE_ARGS_GET_DUMP_PATH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_LINE_ARGS_GET_TYPE'
PP_THREAD_SAFE FUNCTION COMMAND_LINE_ARGS_GET_TYPE( THIS, TYPE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: COMMAND_ARGUMENTS_TOKENIZER_T
  USE :: MULTIOM_API, ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: THIS
  CHARACTER(LEN=*),           INTENT(OUT)   :: TYPE
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_LEN = 1_JPIB_K

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

  ! Wrong length
  PP_DEBUG_CRITICAL_COND_THROW( LEN_TRIM(THIS%OUTPUT_MANAGER_TYPE).NE.LEN(TYPE), ERRFLAG_WRONG_LEN )

  ! Copy the configuration file
  TYPE = REPEAT( ' ', LEN(TYPE) )
  TYPE = TRIM(ADJUSTL(THIS%OUTPUT_MANAGER_TYPE))


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_WRONG_LEN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Wrong length of the configuration file' )
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

END FUNCTION COMMAND_LINE_ARGS_GET_TYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_LINE_ARGS_GET_VERBOSE'
PP_THREAD_SAFE FUNCTION COMMAND_LINE_ARGS_GET_VERBOSE( THIS, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: COMMAND_ARGUMENTS_TOKENIZER_T
  USE :: MULTIOM_API, ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: THIS
  LOGICAL,                    INTENT(OUT)   :: VERBOSE
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

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

  ! Copy the configuration file
  VERBOSE = THIS%VERBOSE

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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

END FUNCTION COMMAND_LINE_ARGS_GET_VERBOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_LINE_ARGS_MATCH_ATM'
PP_THREAD_SAFE FUNCTION COMMAND_LINE_ARGS_MATCH_ATM( THIS, PARAM, REPRES, PREFIX, LEVEL, STEP, MATCH, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: COMMAND_ARGUMENTS_TOKENIZER_T
  USE :: MULTIOM_API, ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: PARAM
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: REPRES
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: PREFIX
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: LEVEL
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: STEP
  LOGICAL,                    INTENT(OUT)   :: MATCH
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

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

  ! Copy the configuration file
  MATCH = .TRUE.

  ! Match param
  IF ( ALLOCATED(THIS%PARAM) ) THEN
    IF ( THIS%MATCH_PARAM ) THEN
      MATCH = MATCH .AND. ANY( THIS%PARAM .EQ. PARAM )
    ELSE
      MATCH = MATCH .AND. .NOT.ANY( THIS%PARAM .EQ. PARAM )
    ENDIF
  ENDIF

  ! Match repres
  IF ( ALLOCATED(THIS%REPRES) ) THEN
    IF ( THIS%MATCH_REPRES ) THEN
      MATCH = MATCH .AND. ANY( THIS%REPRES .EQ. REPRES )
    ELSE
      MATCH = MATCH .AND. .NOT.ANY( THIS%REPRES .EQ. REPRES )
    ENDIF
  ENDIF

  ! Match levtype
  IF ( ALLOCATED(THIS%LEVTYPE) ) THEN
    IF ( THIS%MATCH_LEVTYPE ) THEN
      MATCH = MATCH .AND. ANY( THIS%LEVTYPE .EQ. PREFIX )
    ELSE
      MATCH = MATCH .AND. .NOT.ANY( THIS%LEVTYPE .EQ. PREFIX )
    ENDIF
  ENDIF

  ! Match level
  IF ( ALLOCATED(THIS%LEVEL) ) THEN
    IF ( THIS%MATCH_LEVEL ) THEN
      MATCH = MATCH .AND. ANY( THIS%LEVEL .EQ. LEVEL )
    ELSE
      MATCH = MATCH .AND. .NOT.ANY( THIS%LEVEL .EQ. LEVEL )
    ENDIF
  ENDIF

  ! Match step
  IF ( ALLOCATED(THIS%STEP) ) THEN
    IF ( THIS%MATCH_STEP ) THEN
      MATCH = MATCH .AND. ANY( THIS%STEP .EQ. STEP )
    ELSE
      MATCH = MATCH .AND. .NOT.ANY( THIS%STEP .EQ. STEP )
    ENDIF
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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

END FUNCTION COMMAND_LINE_ARGS_MATCH_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_LINE_ARGS_MATCH_WAM'
PP_THREAD_SAFE FUNCTION COMMAND_LINE_ARGS_MATCH_WAM( THIS, PARAM, REPRES, PREFIX, FREQUENCY, DIRECTION, STEP, MATCH, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: COMMAND_ARGUMENTS_TOKENIZER_T
  USE :: MULTIOM_API, ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: PARAM
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: REPRES
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: PREFIX
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: FREQUENCY
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: DIRECTION
  INTEGER(KIND=JPIB_K),       INTENT(IN)    :: STEP
  LOGICAL,                    INTENT(OUT)   :: MATCH
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

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

  ! Copy the configuration file
  MATCH = .TRUE.

  ! Match param
  IF ( ALLOCATED(THIS%PARAM) ) THEN
    IF ( THIS%MATCH_PARAM ) THEN
      MATCH = MATCH .AND. ANY( THIS%PARAM .EQ. PARAM )
    ELSE
      MATCH = MATCH .AND. .NOT.ANY( THIS%PARAM .EQ. PARAM )
    ENDIF
  ENDIF

  ! Match repres
  IF ( ALLOCATED(THIS%REPRES) ) THEN
    IF ( THIS%MATCH_REPRES ) THEN
      MATCH = MATCH .AND. ANY( THIS%REPRES .EQ. REPRES )
    ELSE
      MATCH = MATCH .AND. .NOT.ANY( THIS%REPRES .EQ. REPRES )
    ENDIF
  ENDIF

  ! Match levtype
  IF ( ALLOCATED(THIS%LEVTYPE) ) THEN
    IF ( THIS%MATCH_LEVTYPE ) THEN
      MATCH = MATCH .AND. ANY( THIS%LEVTYPE .EQ. PREFIX )
    ELSE
      MATCH = MATCH .AND. .NOT.ANY( THIS%LEVTYPE .EQ. PREFIX )
    ENDIF
  ENDIF

  ! Match frequency
  IF ( ALLOCATED(THIS%FREQUENCY) ) THEN
    IF ( THIS%MATCH_FREQUENCY ) THEN
      MATCH = MATCH .AND. ANY( THIS%FREQUENCY .EQ. FREQUENCY )
    ELSE
      MATCH = MATCH .AND. .NOT.ANY( THIS%FREQUENCY .EQ. FREQUENCY )
    ENDIF
  ENDIF

  ! Match direction
  IF ( ALLOCATED(THIS%DIRECTION) ) THEN
    IF ( THIS%MATCH_DIRECTION ) THEN
      MATCH = MATCH .AND. ANY( THIS%DIRECTION .EQ. DIRECTION )
    ELSE
      MATCH = MATCH .AND. .NOT.ANY( THIS%DIRECTION .EQ. DIRECTION )
    ENDIF
  ENDIF

  ! Match step
  IF ( ALLOCATED(THIS%STEP) ) THEN
    IF ( THIS%MATCH_STEP ) THEN
      MATCH = MATCH .AND. ANY( THIS%STEP .EQ. STEP )
    ELSE
      MATCH = MATCH .AND. .NOT.ANY( THIS%STEP .EQ. STEP )
    ENDIF
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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

END FUNCTION COMMAND_LINE_ARGS_MATCH_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMMAND_LINE_ARGS_GET_PROCESSOR_LIST'
PP_THREAD_SAFE FUNCTION COMMAND_LINE_ARGS_GET_PROCESSOR_LIST( THIS, PROCESSOR_LIST, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: COMMAND_ARGUMENTS_TOKENIZER_T
  USE :: MULTIOM_API, ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                      INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: PROCESSOR_LIST
  TYPE(HOOKS_T),                                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALREADY_ALLOCATED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATE_PROCESSOR_LIST =2_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(THIS%PROCESSORS), ERRFLAG_ALREADY_ALLOCATED )

  ! Check if the processor list is allocated
  IF ( ALLOCATED(THIS%PROCESSORS) ) THEN
    ALLOCATE( PROCESSOR_LIST(SIZE(THIS%PROCESSORS)), STAT=STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ALLOCATE_PROCESSOR_LIST )
    DO I = 1, SIZE(THIS%PROCESSORS)
      PROCESSOR_LIST(I) = THIS%PROCESSORS(I)
    END DO
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Processor list already allocated' )
    CASE (ERRFLAG_ALLOCATE_PROCESSOR_LIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error allocating the processor list' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(  ERRMSG, STAT=STAT )
      ENDIF
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

END FUNCTION COMMAND_LINE_ARGS_GET_PROCESSOR_LIST
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DEFAULT_COMMAND_LINE_ARGS'
PP_THREAD_SAFE FUNCTION DEFAULT_COMMAND_LINE_ARGS( THIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: MULTIOM_API, ONLY: COMMAND_ARGUMENTS_TOKENIZER_T
  USE :: MULTIOM_API, ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: THIS
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

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

  ! Default initialization
  THIS%VERBOSE = .FALSE.
  THIS%INPUT_DIR = './'
  THIS%YAML_CONFIGURATION = './output-manager-cfg.yaml'
  THIS%OUTPUT_MANAGER_TYPE = 'GRIB-HEADER-TO-MULTIO'

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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

END FUNCTION DEFAULT_COMMAND_LINE_ARGS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_CARG2IARG_SHORT'
PP_THREAD_SAFE FUNCTION CMDARG_CARG2IARG_SHORT( THIS, CARG, IARG, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: MULTIOM_API,       ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),   INTENT(INOUT) :: THIS
  CHARACTER(LEN=CMDARG_STRLEN), INTENT(IN)    :: CARG
  INTEGER(KIND=JPIB_K),         INTENT(OUT)   :: IARG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_ARGUMENT = 1_JPIB_K

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

  ! select the proper command line argument
  SELECT CASE(TRIM(ADJUSTL(CARG)))

  CASE ( 'v' )
    IARG = ARG_VERBOSE_E

  CASE ( 'i' )
    IARG = ARG_DUMP_PATH_E

  CASE ( 'u' )
    IARG = ARG_USE_VALUES_E

  CASE ( 't' )
    IARG = ARG_OUTPUT_MANAGER_E

  CASE ( 'y' )
    IARG = ARG_CONFIGURATION_FILE_E

  CASE ( 'c' )
    IARG = ARG_PROCESSORS_E

  CASE ( 'p' )
    IARG = ARG_PARAM_E

  CASE ( 'r' )
    IARG = ARG_REPRES_E

  CASE ( 'm' )
    IARG = ARG_LEVTYPE_E

  CASE ( 'l' )
    IARG = ARG_LEVEL_E

  CASE ( 'f' )
    IARG = ARG_FREQUENCY_E

  CASE ( 'd' )
    IARG = ARG_DIRCTION_E

  CASE ( 'q' )
    IARG = ARG_FILTER_FILE_E

  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_ARGUMENT )

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
    CASE (ERRFLAG_UNKNOWN_ARGUMENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown command line argument argument' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'flag: '//TRIM(ADJUSTL(CARG)) )
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

END FUNCTION CMDARG_CARG2IARG_SHORT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_CARG2IARG_LONG'
PP_THREAD_SAFE FUNCTION CMDARG_CARG2IARG_LONG( THIS, CARG, IARG, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: MULTIOM_API,       ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),   INTENT(INOUT) :: THIS
  CHARACTER(LEN=CMDARG_STRLEN), INTENT(IN)    :: CARG
  INTEGER(KIND=JPIB_K),         INTENT(OUT)   :: IARG
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_ARGUMENT = 1_JPIB_K

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



  ! select the proper command line argument
  SELECT CASE(TRIM(ADJUSTL(CARG)))

  CASE ( 'verbose' )
    IARG = ARG_VERBOSE_E

  CASE ( 'input-path' )
    IARG = ARG_DUMP_PATH_E

  CASE ( 'use-values' )
    IARG = ARG_USE_VALUES_E

  CASE ( 'output-managet-type', 'type' )
    IARG = ARG_OUTPUT_MANAGER_E

  CASE ( 'output-manager-yaml', 'yaml', 'configuration-file' )
    IARG = ARG_CONFIGURATION_FILE_E

  CASE ( 'processors-list' )
    IARG = ARG_PROCESSORS_E

  CASE ( 'param' )
    IARG = ARG_PARAM_E

  CASE ( 'repres' )
    IARG = ARG_REPRES_E

  CASE ( 'levtype' )
    IARG = ARG_LEVTYPE_E

  CASE ( 'level' )
    IARG = ARG_LEVEL_E

  CASE ( 'frequency' )
    IARG = ARG_FREQUENCY_E

  CASE ( 'direction' )
    IARG = ARG_DIRCTION_E

  CASE ( 'filter-file' )
    IARG = ARG_FILTER_FILE_E

  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_ARGUMENT )

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
    CASE (ERRFLAG_UNKNOWN_ARGUMENT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown command line argument argument' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'flag: '//TRIM(ADJUSTL(CARG)) )
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

END FUNCTION CMDARG_CARG2IARG_LONG
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_CHECK_ARGS_CONSISTENCY'
PP_THREAD_SAFE FUNCTION CMDARG_CHECK_ARGS_CONSISTENCY( THIS, ARGS_CHACKER, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                  INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K), DIMENSION(ARG_LAST_E), INTENT(IN)    :: ARGS_CHACKER
  TYPE(HOOKS_T),                               INTENT(INOUT) :: HOOKS

  !> Function result
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

END FUNCTION CMDARG_CHECK_ARGS_CONSISTENCY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_PARSE_VERBOSE'
PP_THREAD_SAFE FUNCTION CMDARG_PARSE_VERBOSE( THIS, MATCH, VALUES, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_API,         ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                          INTENT(INOUT) :: THIS
  LOGICAL,                                             INTENT(IN)    :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_NOT_EXPECTED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_NOT_EXPECTED = 2_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. MATCH, ERRFLAG_MATCH_NOT_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(VALUES), ERRFLAG_VALUES_NOT_EXPECTED )

  ! Parse the verbosity flag
  WRITE(*,*) 'Parsing the verbosity flag: T'

  ! Parse the verbosity flag
  THIS%VERBOSE = .TRUE.

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
    CASE (ERRFLAG_MATCH_NOT_EXPECTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Match not expected' )
    CASE (ERRFLAG_VALUES_NOT_EXPECTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Values not expected' )
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

END FUNCTION CMDARG_PARSE_VERBOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_PARSE_DUMP_PATH'
PP_THREAD_SAFE FUNCTION CMDARG_PARSE_DUMP_PATH( THIS, MATCH, VALUES, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_API,         ONLY: CMDARG_STRLEN
  USE :: MULTIOM_API,         ONLY: REPLACE_ENVVAR_IN_STRING
  USE :: MULTIOM_API,         ONLY: IS_DIRECTORY

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                          INTENT(INOUT) :: THIS
  LOGICAL,                                             INTENT(IN)    :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: EXISTS
  CHARACTER(LEN=CMDARG_STRLEN) :: DUMP_PATH

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_NOT_EXPECTED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_EXPECTED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUES_COUNT = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CHECK_DUMP_PATH = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_DUMP_PATH = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXPAND_ENVVAR = 6_JPIB_K

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

  ! Parse the dump path flag
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. MATCH, ERRFLAG_MATCH_NOT_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(VALUES), ERRFLAG_VALUES_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES) .NE. 1, ERRFLAG_INVALID_VALUES_COUNT )

  ! Copy input value to local variable
  DUMP_PATH = REPEAT( ' ', CMDARG_STRLEN )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_EXPAND_ENVVAR) REPLACE_ENVVAR_IN_STRING( VALUES(1), DUMP_PATH, HOOKS )

  ! Set the dump path
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CHECK_DUMP_PATH) IS_DIRECTORY( TRIM(ADJUSTL(DUMP_PATH)), EXISTS, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. EXISTS, ERRFLAG_INVALID_DUMP_PATH )

  ! Set the dump path
  THIS%INPUT_DIR = REPEAT( ' ', CMDARG_STRLEN )
  THIS%INPUT_DIR = TRIM(ADJUSTL(DUMP_PATH))


  ! Parse the verbosity flag
  WRITE(*,*) 'Parsing the dump path: "'//TRIM(ADJUSTL(DUMP_PATH))//'"'

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

    ! Debug variables
    CHARACTER(LEN=32) :: TMP
    INTEGER(KIND=JPIB_K) :: STATUS

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_MATCH_NOT_EXPECTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Match not expected' )
    CASE (ERRFLAG_VALUES_EXPECTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Values expected' )
    CASE (ERRFLAG_INVALID_VALUES_COUNT)
      TMP = REPEAT(' ',32)
      WRITE(TMP, '(I32)', IOSTAT=STATUS) SIZE(VALUES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid values count' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'expected: 1' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'got: '//TRIM(ADJUSTL(TMP)) )
    CASE (ERRFLAG_UNABLE_TO_CHECK_DUMP_PATH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check dump path' )
    CASE (ERRFLAG_INVALID_DUMP_PATH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Dump path does not exist' )
    CASE (ERRFLAG_UNABLE_TO_EXPAND_ENVVAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to expand environment variable' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'variable: '//TRIM(ADJUSTL(VALUES(1))) )
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

END FUNCTION CMDARG_PARSE_DUMP_PATH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_PARSE_USE_VALUES'
PP_THREAD_SAFE FUNCTION CMDARG_PARSE_USE_VALUES( THIS, MATCH, VALUES, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_API,         ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                          INTENT(INOUT) :: THIS
  LOGICAL,                                             INTENT(IN)    :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_NOT_EXPECTED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_NOT_EXPECTED = 2_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. MATCH, ERRFLAG_MATCH_NOT_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(VALUES), ERRFLAG_VALUES_NOT_EXPECTED )

  ! Parse the use values flag
  WRITE(*,*) 'Parsing the use values flag: T'

  THIS%USE_VALUES = .TRUE.

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
    CASE (ERRFLAG_MATCH_NOT_EXPECTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Match not expected' )
    CASE (ERRFLAG_VALUES_NOT_EXPECTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Values not expected' )
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

END FUNCTION CMDARG_PARSE_USE_VALUES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_PARSE_OUTPUT_MANAGER_TYPE'
PP_THREAD_SAFE FUNCTION CMDARG_PARSE_OUTPUT_MANAGER_TYPE( THIS, MATCH, VALUES, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_API,         ONLY: CMDARG_STRLEN
  USE :: MULTIOM_API,         ONLY: REPLACE_ENVVAR_IN_STRING
  USE :: MULTIOM_API,         ONLY: IS_VALID_OUTPUT_MANAGER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                          INTENT(INOUT) :: THIS
  LOGICAL,                                             INTENT(IN)    :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=CMDARG_STRLEN) :: OUTPUT_MANAGER_TYPE
  LOGICAL :: IS_VALID

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_NOT_EXPECTED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_EXPECTED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_OUTPUT_MANAGER_TYPE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXPAND_ENVVAR = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CHECK_OUTPUT_MANAGER_TYPE = 5_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. MATCH, ERRFLAG_MATCH_NOT_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(VALUES), ERRFLAG_VALUES_EXPECTED )

  ! Copy input value to local variable
  OUTPUT_MANAGER_TYPE = REPEAT( ' ', CMDARG_STRLEN )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_EXPAND_ENVVAR) REPLACE_ENVVAR_IN_STRING( VALUES(1), OUTPUT_MANAGER_TYPE, HOOKS )

  ! Check the output manager type
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CHECK_OUTPUT_MANAGER_TYPE) IS_VALID_OUTPUT_MANAGER( TRIM(ADJUSTL(OUTPUT_MANAGER_TYPE)), IS_VALID, HOOKS )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. IS_VALID, ERRFLAG_INVALID_OUTPUT_MANAGER_TYPE )

  ! Set the output manager type
  THIS%OUTPUT_MANAGER_TYPE = REPEAT( ' ', CMDARG_STRLEN )
  THIS%OUTPUT_MANAGER_TYPE = TRIM(ADJUSTL(OUTPUT_MANAGER_TYPE))

  ! Parse the use values flag
  WRITE(*,*) 'Parsing output manager type: "'//TRIM(ADJUSTL(OUTPUT_MANAGER_TYPE))//'"'

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
    CASE (ERRFLAG_MATCH_NOT_EXPECTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Match not expected' )
    CASE (ERRFLAG_VALUES_EXPECTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Values expected' )
    CASE (ERRFLAG_INVALID_OUTPUT_MANAGER_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid output manager type' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'type: '//TRIM(ADJUSTL(OUTPUT_MANAGER_TYPE)) )
    CASE (ERRFLAG_UNABLE_TO_EXPAND_ENVVAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to expand environment variable' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'variable: '//TRIM(ADJUSTL(VALUES(1))) )
    CASE (ERRFLAG_UNABLE_TO_CHECK_OUTPUT_MANAGER_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check output manager type' )
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

END FUNCTION CMDARG_PARSE_OUTPUT_MANAGER_TYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_PARSE_CONFIGURATION_FILE'
PP_THREAD_SAFE FUNCTION CMDARG_PARSE_CONFIGURATION_FILE( THIS, MATCH, VALUES, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_API,         ONLY: CMDARG_STRLEN
  USE :: MULTIOM_API,         ONLY: REPLACE_ENVVAR_IN_STRING


  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                          INTENT(INOUT) :: THIS
  LOGICAL,                                             INTENT(IN)    :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: EXISTS
  INTEGER(KIND=JPIB_K) :: STATUS
  CHARACTER(LEN=CMDARG_STRLEN) :: CONFIGURATION_FILE

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MATCH_NOT_EXPECTED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_EXPECTED = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUES_COUNT = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CONFIGURATION_FILE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXPAND_ENVVAR = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CHECK_CONFIGURATION_FILE = 6_JPIB_K

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

  ! Parse the dump path flag
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. MATCH, ERRFLAG_MATCH_NOT_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(VALUES), ERRFLAG_VALUES_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES) .NE. 1, ERRFLAG_INVALID_VALUES_COUNT )

  ! Copy input value to local variable
  CONFIGURATION_FILE = REPEAT( ' ', CMDARG_STRLEN )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_EXPAND_ENVVAR) REPLACE_ENVVAR_IN_STRING( VALUES(1), CONFIGURATION_FILE, HOOKS )

  ! Set the dump path
  INQUIRE( FILE=TRIM(ADJUSTL(CONFIGURATION_FILE)), EXIST=EXISTS, IOSTAT=STATUS )
  PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_CHECK_CONFIGURATION_FILE )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. EXISTS, ERRFLAG_INVALID_CONFIGURATION_FILE )

  ! Set the dump path
  THIS%YAML_CONFIGURATION = REPEAT( ' ', CMDARG_STRLEN )
  THIS%YAML_CONFIGURATION = TRIM(ADJUSTL(CONFIGURATION_FILE))


  ! Parse the verbosity flag
  WRITE(*,*) 'Parsing the yaml configuration file: "'//TRIM(ADJUSTL(CONFIGURATION_FILE))//'"'

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
    CASE (ERRFLAG_MATCH_NOT_EXPECTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Match not expected' )
    CASE (ERRFLAG_VALUES_EXPECTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Values expected' )
    CASE (ERRFLAG_INVALID_VALUES_COUNT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid values count' )
    CASE (ERRFLAG_INVALID_CONFIGURATION_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid configuration file' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'file: '//TRIM(ADJUSTL(CONFIGURATION_FILE)) )
    CASE (ERRFLAG_UNABLE_TO_EXPAND_ENVVAR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to expand environment variable' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'variable: '//TRIM(ADJUSTL(VALUES(1))) )
    CASE (ERRFLAG_UNABLE_TO_CHECK_CONFIGURATION_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check configuration file' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'file: '//TRIM(ADJUSTL(CONFIGURATION_FILE)) )
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

END FUNCTION CMDARG_PARSE_CONFIGURATION_FILE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_PARSE_PROCESSORS_LIST'
PP_THREAD_SAFE FUNCTION CMDARG_PARSE_PROCESSORS_LIST( THIS, MATCH, VALUES, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_API,         ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                          INTENT(INOUT) :: THIS
  LOGICAL,                                             INTENT(IN)    :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_EXPECTED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUES_COUNT = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_INTEGER_ARRAY = 3_JPIB_K

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

  ! Parse the dump path flag
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(VALUES), ERRFLAG_VALUES_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES) .LT. 1, ERRFLAG_INVALID_VALUES_COUNT )

  ! Read integer array with ranges
  PP_TRYCALL(ERRFLAG_READ_INTEGER_ARRAY) READ_INTEGER_ARRAY_WITH_RANGES( VALUES, THIS%PROCESSORS, HOOKS )

  ! Parse the verbosity flag
  WRITE(*,*) 'Parsing the processors list: ', THIS%PROCESSORS

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

END FUNCTION CMDARG_PARSE_PROCESSORS_LIST
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_PARSE_PARAM'
PP_THREAD_SAFE FUNCTION CMDARG_PARSE_PARAM( THIS, MATCH, VALUES, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_API,         ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                          INTENT(INOUT) :: THIS
  LOGICAL,                                             INTENT(IN)    :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_EXPECTED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUES_COUNT = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_INTEGER_ARRAY = 3_JPIB_K

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

  ! Parse the dump path flag
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(VALUES), ERRFLAG_VALUES_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES) .LT. 1, ERRFLAG_INVALID_VALUES_COUNT )

  ! Read integer array with ranges
  THIS%MATCH_PARAM = .TRUE.
  PP_TRYCALL(ERRFLAG_READ_INTEGER_ARRAY) READ_INTEGER_ARRAY_WITH_RANGES( VALUES, THIS%PARAM, HOOKS )

  ! Parse the verbosity flag
  WRITE(*,*) 'Parsing the param list: ', THIS%PARAM



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

END FUNCTION CMDARG_PARSE_PARAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_PARSE_REPRES'
PP_THREAD_SAFE FUNCTION CMDARG_PARSE_REPRES( THIS, MATCH, VALUES, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_API,         ONLY: CMDARG_STRLEN
  USE :: MULTIOM_API,         ONLY: CREPRES2IREPRES
  USE :: MULTIOM_API,         ONLY: FUN_C2I_IF

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                          INTENT(INOUT) :: THIS
  LOGICAL,                                             INTENT(IN)    :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  PROCEDURE(FUN_C2I_IF), POINTER :: FILTER

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_EXPECTED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUES_COUNT = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_INTEGER_ARRAY = 3_JPIB_K

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

  ! Parse the dump path flag
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(VALUES), ERRFLAG_VALUES_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES) .LT. 1, ERRFLAG_INVALID_VALUES_COUNT )

  ! Read integer array with ranges
  THIS%MATCH_REPRES = .TRUE.
  FILTER => CREPRES2IREPRES
  PP_TRYCALL(ERRFLAG_READ_INTEGER_ARRAY) READ_INTEGER_ARRAY_WITH_FILTER( VALUES, THIS%REPRES, FILTER, HOOKS )

  ! Parse the verbosity flag
  WRITE(*,*) 'Parsing the repres list: ', THIS%REPRES

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
    CASE (ERRFLAG_VALUES_EXPECTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Values expected' )
    CASE (ERRFLAG_INVALID_VALUES_COUNT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid values count' )
    CASE (ERRFLAG_READ_INTEGER_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading integer array' )
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

END FUNCTION CMDARG_PARSE_REPRES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_PARSE_LEVTYPE'
PP_THREAD_SAFE FUNCTION CMDARG_PARSE_LEVTYPE( THIS, MATCH, VALUES, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_API,         ONLY: CMDARG_STRLEN
  USE :: MULTIOM_API,         ONLY: CPREFIX2IPREFIX
  USE :: MULTIOM_API,         ONLY: FUN_C2I_IF

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                          INTENT(INOUT) :: THIS
  LOGICAL,                                             INTENT(IN)    :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  PROCEDURE(FUN_C2I_IF), POINTER :: FILTER

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_EXPECTED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUES_COUNT = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_INTEGER_ARRAY = 3_JPIB_K

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

  ! Parse the dump path flag
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(VALUES), ERRFLAG_VALUES_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES) .LT. 1, ERRFLAG_INVALID_VALUES_COUNT )

  ! Read integer array with ranges
  THIS%MATCH_REPRES = .TRUE.
  FILTER => CPREFIX2IPREFIX
  PP_TRYCALL(ERRFLAG_READ_INTEGER_ARRAY) READ_INTEGER_ARRAY_WITH_FILTER( VALUES, THIS%LEVTYPE, FILTER, HOOKS )

  ! Parse the verbosity flag
  WRITE(*,*) 'Parsing the levtype list: ', THIS%LEVTYPE



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

END FUNCTION CMDARG_PARSE_LEVTYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_PARSE_LEVEL'
PP_THREAD_SAFE FUNCTION CMDARG_PARSE_LEVEL( THIS, MATCH, VALUES, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_API,         ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                          INTENT(INOUT) :: THIS
  LOGICAL,                                             INTENT(IN)    :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_EXPECTED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUES_COUNT = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_INTEGER_ARRAY = 3_JPIB_K

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

  ! Parse the dump path flag
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(VALUES), ERRFLAG_VALUES_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES) .LT. 1, ERRFLAG_INVALID_VALUES_COUNT )

  ! Read integer array with ranges
  THIS%MATCH_LEVEL = MATCH
  PP_TRYCALL(ERRFLAG_READ_INTEGER_ARRAY) READ_INTEGER_ARRAY_WITH_RANGES( VALUES, THIS%LEVEL, HOOKS )

  ! Parse the verbosity flag
  WRITE(*,*) 'Parsing the level list: ', THIS%LEVEL



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

END FUNCTION CMDARG_PARSE_LEVEL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_PARSE_STEP'
PP_THREAD_SAFE FUNCTION CMDARG_PARSE_STEP( THIS, MATCH, VALUES, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_API,         ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                          INTENT(INOUT) :: THIS
  LOGICAL,                                             INTENT(IN)    :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_EXPECTED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUES_COUNT = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_INTEGER_ARRAY = 3_JPIB_K

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

  ! Parse the dump path flag
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(VALUES), ERRFLAG_VALUES_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES) .LT. 1, ERRFLAG_INVALID_VALUES_COUNT )

  ! Read integer array with ranges
  THIS%MATCH_STEP = MATCH
  PP_TRYCALL(ERRFLAG_READ_INTEGER_ARRAY) READ_INTEGER_ARRAY_WITH_RANGES( VALUES, THIS%STEP, HOOKS )

  ! Parse the verbosity flag
  WRITE(*,*) 'Parsing the step list: ', THIS%STEP



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

END FUNCTION CMDARG_PARSE_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_PARSE_FREQUENCY'
PP_THREAD_SAFE FUNCTION CMDARG_PARSE_FREQUENCY( THIS, MATCH, VALUES, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_API,         ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                          INTENT(INOUT) :: THIS
  LOGICAL,                                             INTENT(IN)    :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_EXPECTED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUES_COUNT = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_INTEGER_ARRAY = 3_JPIB_K

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

  ! Parse the dump path flag
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(VALUES), ERRFLAG_VALUES_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES) .LT. 1, ERRFLAG_INVALID_VALUES_COUNT )

  ! Read integer array with ranges
  THIS%MATCH_FREQUENCY = MATCH
  PP_TRYCALL(ERRFLAG_READ_INTEGER_ARRAY) READ_INTEGER_ARRAY_WITH_RANGES( VALUES, THIS%FREQUENCY, HOOKS )

  ! Parse the verbosity flag
  WRITE(*,*) 'Parsing the frequency list: ', THIS%FREQUENCY



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

END FUNCTION CMDARG_PARSE_FREQUENCY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_PARSE_DIRECTION'
PP_THREAD_SAFE FUNCTION CMDARG_PARSE_DIRECTION( THIS, MATCH, VALUES, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_API,         ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                          INTENT(INOUT) :: THIS
  LOGICAL,                                             INTENT(IN)    :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUES_EXPECTED = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_VALUES_COUNT = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_READ_INTEGER_ARRAY = 3_JPIB_K

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

  ! Parse the dump path flag
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. ASSOCIATED(VALUES), ERRFLAG_VALUES_EXPECTED )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(VALUES) .LT. 1, ERRFLAG_INVALID_VALUES_COUNT )

  ! Read integer array with ranges
  THIS%MATCH_DIRECTION = MATCH
  PP_TRYCALL(ERRFLAG_READ_INTEGER_ARRAY) READ_INTEGER_ARRAY_WITH_RANGES( VALUES, THIS%DIRECTION, HOOKS )

  ! Parse the verbosity flag
  WRITE(*,*) 'Parsing the direction list: ', THIS%DIRECTION



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

END FUNCTION CMDARG_PARSE_DIRECTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMDARG_PARSE_FILTER_FILE'
PP_THREAD_SAFE FUNCTION CMDARG_PARSE_FILTER_FILE( THIS, MATCH, VALUES, HOOKS )  RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: MULTIOM_API,         ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CLASS(COMMAND_LINE_ARGS_T),                          INTENT(INOUT) :: THIS
  LOGICAL,                                             INTENT(IN)    :: MATCH
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: VALUES
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  !> Function result
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

END FUNCTION CMDARG_PARSE_FILTER_FILE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_INTEGER_ARRAY_WITH_RANGES'
PP_THREAD_SAFE FUNCTION READ_INTEGER_ARRAY_WITH_RANGES( ATMP, VALUE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: MULTIOM_API,       ONLY: CMDARG_STRLEN

  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_IS_INTEGER
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_IS_INTEGER_RANGE
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_IS_INTEGER_RANGE_BY
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_TO_INTEGER
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_TO_INTEGER_RANGE
  USE :: CONFIGURATION_UTILS_MOD, ONLY: STRING_TO_INTEGER_RANGE_BY

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=CMDARG_STRLEN), POINTER, DIMENSION(:), INTENT(IN)    :: ATMP
  INTEGER(KIND=JPIB_K), ALLOCATABLE,  DIMENSION(:),    INTENT(OUT)   :: VALUE
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  LOGICAL :: IS_INTEGER
  LOGICAL :: IS_INTEGER_RANGE
  LOGICAL :: IS_INTEGER_RANGE_BY
  INTEGER(KIND=JPIB_K) :: LO
  INTEGER(KIND=JPIB_K) :: HI
  INTEGER(KIND=JPIB_K) :: BY
  INTEGER(KIND=JPIB_K) :: VALUE_SIZE
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG


  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_ARRAY_NOT_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_ARRAY_SIZE_LT_1=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_1=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_2=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_3=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_4=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STRING_IN_ARRAY_5=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_OUT_OF_BOUNDS=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUS_SIZE_LT_1=14_JPIB_K

  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_READ_STRING_ARRAY=15_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_IS_INTEGER=16_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE=17_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE_BY=18_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_TO_INTEGER=19_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE=20_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE_BY=21_JPIB_K


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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(ATMP), ERRFLAG_INPUT_NOT_ALLOCATED )

  ! Read the paramId as a string array
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(ATMP).LT.1, ERRFLAG_STRING_ARRAY_SIZE_LT_1 )

  ! Compute the size of the paramId array
  VALUE_SIZE = 0_JPIB_K
  ParamIdFIlterSizeLoop: DO I = 1, SIZE(ATMP)

    ! Check if the value is an integer
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER) STRING_IS_INTEGER( ATMP(I), IS_INTEGER, HOOKS )
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE) STRING_IS_INTEGER_RANGE( ATMP(I), IS_INTEGER_RANGE, HOOKS )
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE_BY) STRING_IS_INTEGER_RANGE_BY( ATMP(I), IS_INTEGER_RANGE_BY, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INTEGER .AND. .NOT.IS_INTEGER_RANGE .AND. .NOT.IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_1 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_2 )

    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE, ERRFLAG_INVALID_STRING_IN_ARRAY_3 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_4 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER_RANGE .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_5 )

    ! Update the total size of the paramId array
    IF ( IS_INTEGER ) THEN
      VALUE_SIZE = VALUE_SIZE + 1
    ELSE IF ( IS_INTEGER_RANGE ) THEN
      PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE) STRING_TO_INTEGER_RANGE( ATMP(I), LO, HI, HOOKS )
      VALUE_SIZE = VALUE_SIZE + HI - LO + 1
    ELSE IF ( IS_INTEGER_RANGE_BY ) THEN
      PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE_BY) STRING_TO_INTEGER_RANGE_BY( ATMP(I), LO, HI, BY, HOOKS )
      VALUE_SIZE = VALUE_SIZE + (HI - LO + 1)/BY
    ENDIF

  ENDDO ParamIdFIlterSizeLoop
  PP_DEBUG_CRITICAL_COND_THROW( VALUE_SIZE.LT.1, ERRFLAG_VALUS_SIZE_LT_1 )

  ! Allocate the paramId array
  ALLOCATE( VALUE(VALUE_SIZE), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  ! Fill the paramId array
  J = 0_JPIB_K
  ParamIdFIlterFillLoop: DO I = 1, SIZE(ATMP)

    ! Check if the value is an integer
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER) STRING_IS_INTEGER( ATMP(I), IS_INTEGER, HOOKS )
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE) STRING_IS_INTEGER_RANGE( ATMP(I), IS_INTEGER_RANGE, HOOKS )
    PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE_BY) STRING_IS_INTEGER_RANGE_BY( ATMP(I), IS_INTEGER_RANGE_BY, HOOKS )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.IS_INTEGER .AND. .NOT.IS_INTEGER_RANGE .AND. .NOT.IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_1 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_2 )

    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE, ERRFLAG_INVALID_STRING_IN_ARRAY_3 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_4 )
    PP_DEBUG_CRITICAL_COND_THROW( IS_INTEGER_RANGE .AND. IS_INTEGER_RANGE_BY, ERRFLAG_INVALID_STRING_IN_ARRAY_5 )

    ! Update the total size of the paramId array
    IF ( IS_INTEGER ) THEN

      ! Fill the paramId array when the item is an integer (e.g., 1)
      J = J + 1
      PP_DEBUG_CRITICAL_COND_THROW( J.GT.SIZE(VALUE), ERRFLAG_VALUE_OUT_OF_BOUNDS )
      PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_TO_INTEGER) STRING_TO_INTEGER( ATMP(I), VALUE(J), HOOKS )

    ELSE IF ( IS_INTEGER_RANGE ) THEN

      ! Fill the paramId array when the item is an integer range (e.g., 1:10)
      PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE) STRING_TO_INTEGER_RANGE( ATMP(I), LO, HI, HOOKS )
      ParamIdFIlterFillRangeLoop: DO
        IF ( LO .LE. HI ) THEN
          J = J + 1
          PP_DEBUG_CRITICAL_COND_THROW( J.GT.SIZE(VALUE), ERRFLAG_VALUE_OUT_OF_BOUNDS )
          VALUE(J) = LO
          LO = LO + 1
        ELSE
          EXIT ParamIdFIlterFillRangeLoop
        ENDIF
      ENDDO ParamIdFIlterFillRangeLoop

    ELSE IF ( IS_INTEGER_RANGE_BY ) THEN

      ! Fill the paramId array when the item is an integer range by (e.g., 1:10:2)
      PP_TRYCALL(ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE_BY) STRING_TO_INTEGER_RANGE_BY( ATMP(I), LO, HI, BY, HOOKS )
      ParamIdFIlterFillRangeByLoop: DO
        IF ( LO .LE. HI ) THEN
          J = J + 1
          PP_DEBUG_CRITICAL_COND_THROW( J.GT.SIZE(VALUE), ERRFLAG_VALUE_OUT_OF_BOUNDS )
          VALUE(J) = LO
          LO = LO + BY
        ELSE
          EXIT ParamIdFIlterFillRangeByLoop
        ENDIF
      ENDDO ParamIdFIlterFillRangeByLoop

    ENDIF

  ENDDO ParamIdFIlterFillLoop

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE(ERRFLAG_INPUT_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input array not allocated' )
    CASE(ERRFLAG_STRING_ARRAY_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array not allocated after read' )
    CASE(ERRFLAG_STRING_ARRAY_SIZE_LT_1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array is empty' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string no matches (no integer, no integer range, no integer range by)' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_2)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string multiple matches ( integer, integer range, integer range by)' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_3)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string multiple matches (integer, integer range)' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_4)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string multiple matches (integer, integer range by)' )
    CASE(ERRFLAG_INVALID_STRING_IN_ARRAY_5)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error parsing string multiple matches (integer range, integer range by)' )
    CASE(ERRFLAG_ALLOCATION_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating VALUES' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating VALUES: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE(ERRFLAG_VALUE_OUT_OF_BOUNDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Out of bounds while setting integer value to the output array' )
    CASE(ERRFLAG_DEALLOCATION_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating ATMP' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating ATMP: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_VALUS_SIZE_LT_1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Value size is less than 1 after parsing all the elements' )
    CASE (ERRFLAG_ERROR_CALL_READ_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling YAML_READ_STRING_ARRAY' )
    CASE (ERRFLAG_ERROR_CALL_STRING_IS_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_IS_INTEGER' )
    CASE (ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_IS_INTEGER_RANGE' )
    CASE (ERRFLAG_ERROR_CALL_STRING_IS_INTEGER_RANGE_BY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_IS_INTEGER_RANGE_BY' )
    CASE (ERRFLAG_ERROR_CALL_STRING_TO_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_TO_INTEGER' )
    CASE (ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_TO_INTEGER_RANGE' )
    CASE (ERRFLAG_ERROR_CALL_STRING_TO_INTEGER_RANGE_BY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling STRING_TO_INTEGER_RANGE_BY' )
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
  ! Exit point on error
  RETURN

END FUNCTION READ_INTEGER_ARRAY_WITH_RANGES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_INTEGER_ARRAY_WITH_FILTER'
PP_THREAD_SAFE FUNCTION READ_INTEGER_ARRAY_WITH_FILTER( ATMP, VALUE, FILTER, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: MULTIOM_API,       ONLY: FUN_C2I_IF
  USE :: MULTIOM_API,       ONLY: CMDARG_STRLEN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=CMDARG_STRLEN), DIMENSION(:), POINTER, INTENT(IN)    :: ATMP
  INTEGER(KIND=JPIB_K), ALLOCATABLE,  DIMENSION(:),    INTENT(OUT)   :: VALUE
  PROCEDURE(FUN_C2I_IF), POINTER,                      INTENT(IN)    :: FILTER
  TYPE(HOOKS_T),                                       INTENT(INOUT) :: HOOKS

  ! Function return value
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: VALUE_SIZE
  INTEGER(KIND=JPIB_K) :: ALLOC_STATUS
  INTEGER(KIND=JPIB_K) :: DEALLOC_STATUS
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_NOT_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_VALUE_ALREADY_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_ARRAY_NOT_ALLOCATED=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_STRING_ARRAY_SIZE_LT_1=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ALLOCATION_ERROR=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CALL_FILTER=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DEALLOCATION_ERROR=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CALL_READ_STRING_ARRAY=8_JPIB_K

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
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(ATMP), ERRFLAG_CFG_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(VALUE),          ERRFLAG_VALUE_ALREADY_ALLOCATED )

  ! Read the paramId as a string array
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(ATMP).LT.1, ERRFLAG_STRING_ARRAY_SIZE_LT_1 )

  ! Allocate the paramId array
  VALUE_SIZE = SIZE(ATMP)
  ALLOCATE( VALUE(VALUE_SIZE), STAT=ALLOC_STATUS, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STATUS.NE.0, ERRFLAG_ALLOCATION_ERROR )

  ! Compute the size of the paramId array
  ParamIdFIlterSizeLoop: DO I = 1, VALUE_SIZE

    ! Filter the value
    PP_TRYCALL(ERRFLAG_CALL_FILTER) FILTER(ATMP(I), VALUE(I), HOOKS )

  ENDDO ParamIdFIlterSizeLoop

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE (ERRFLAG_CFG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'YAML configuration not allocated' )
    CASE (ERRFLAG_VALUE_ALREADY_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'VALUE already allocated' )
    CASE (ERRFLAG_STRING_ARRAY_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array not allocated after read' )
    CASE (ERRFLAG_STRING_ARRAY_SIZE_LT_1)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'String array is empty' )
    CASE (ERRFLAG_ALLOCATION_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating VALUES' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error allocating VALUES: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_CALL_FILTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling filter' )
    CASE (ERRFLAG_DEALLOCATION_ERROR)
      IF ( .NOT.ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating ATMP' )
      ELSE
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'error deallocating ATMP: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ENDIF
    CASE (ERRFLAG_ERROR_CALL_READ_STRING_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'error calling YAML_READ_STRING_ARRAY' )
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

  ! Exit point on error
  RETURN

END FUNCTION READ_INTEGER_ARRAY_WITH_FILTER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE MULTIOM_LIST_CFG_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME