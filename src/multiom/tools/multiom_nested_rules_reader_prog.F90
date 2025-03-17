! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

#define PP_FILE_NAME 'multiom_nested_rules_reader_prog.F90'
#define PP_SECTION_TYPE 'PROGRAM'
#define PP_SECTION_NAME 'MULTIOM_NESTED_RULES_READER_PROG'
#define PP_PROCEDURE_TYPE 'PROGRAM'
#define PP_PROCEDURE_NAME 'MULTIOM_NESTED_RULES_READER_PROG'
PROGRAM MULTIOM_NESTED_RULES_READER_PROG

  ! Symbolds imported from intrinsic modules
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T
  USE :: ENCODING_RULES_TREE_LOADER_MOD, ONLY: ENCODING_RULES_TREE_LOADER_T
  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T
  USE :: FILTER_OPTIONS_MOD,       ONLY: FILTER_OPTIONS_T

  ! Symbols imported from other libraries
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_NEW_CONFIGURATION_FROM_FILE
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION

IMPLICIT NONE

  ! Local variables
  TYPE(HOOKS_T) :: HOOKS
  TYPE(YAML_CONFIGURATION_T) :: MAIN_CFG
  TYPE(ENCODING_RULES_TREE_LOADER_T) :: ENCODING_RULES_TREE_LOADER
  CHARACTER(LEN=128) :: YAMLFNAME
  TYPE(GRIB_ENCODER_OPTIONS_T) :: ENCODER_OPT
  TYPE(FILTER_OPTIONS_T) :: FILTER_OPT

  ! Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INIT_CMDARGS = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FREE_CMDARGS = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PRINT_CMDARGS = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN_CFG_FILE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULES_LOADER = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULES_PRINT = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_RULES_FREE = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DESTROY_CFG = 8_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialize the hooks
  CALL HOOKS%DEBUG_HOOK_%INIT( )

  ! Initialize local variables
  YAMLFNAME = REPEAT( ' ', LEN(YAMLFNAME))
  YAMLFNAME = 'encoding-rules-nested.yaml'

  ! Open the filename
  PP_TRYCALL(ERRFLAG_UNABLE_TO_OPEN_CFG_FILE) YAML_NEW_CONFIGURATION_FROM_FILE( TRIM(YAMLFNAME), MAIN_CFG, HOOKS )

  ! Initialize the rules loader
  PP_TRYCALL(ERRFLAG_RULES_LOADER) ENCODING_RULES_TREE_LOADER%INIT( MAIN_CFG, FILTER_OPT, ENCODER_OPT, HOOKS )

  ! Initialize the rules loader
  PP_TRYCALL(ERRFLAG_RULES_PRINT) ENCODING_RULES_TREE_LOADER%PRINT( 6_JPIB_K, 0_JPIB_K, ENCODER_OPT, HOOKS )


  ! Initialize the rules loader
  PP_TRYCALL(ERRFLAG_RULES_FREE) ENCODING_RULES_TREE_LOADER%FREE( ENCODER_OPT, HOOKS )


  ! Deallocate the dump-output-manager object
  PP_TRYCALL(ERRFLAG_UNABLE_TO_DESTROY_CFG) YAML_DELETE_CONFIGURATION( MAIN_CFG, HOOKS )

  !> Be sure we don't have any memory leaks
  CALL HOOKS%DEBUG_HOOK_%FREE( )

  STOP

! Error handler
PP_ERROR_HANDLER

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_INIT_CMDARGS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error initializing the command arguments tokenizer' )
    CASE (ERRFLAG_FREE_CMDARGS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error freeing the command arguments tokenizer' )
    CASE (ERRFLAG_PRINT_CMDARGS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error printing command line arguments' )
    CASE (ERRFLAG_UNABLE_TO_OPEN_CFG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error opening the configuration file' )
    CASE (ERRFLAG_RULES_LOADER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error initializing the rules loader' )
    CASE (ERRFLAG_RULES_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error printing the rules' )
    CASE (ERRFLAG_RULES_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error initializing the rules free' )
    CASE (ERRFLAG_UNABLE_TO_DESTROY_CFG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error destroying the configuration object' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Print the error stack
    CALL HOOKS%DEBUG_HOOK_%PRINT_ERROR_STACK( 6_JPIB_K )

    ! Free the error stack
    CALL HOOKS%DEBUG_HOOK_%FREE( )

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  STOP 1

END PROGRAM MULTIOM_NESTED_RULES_READER_PROG
#undef PP_FILE_NAME
#undef PP_SECTION_TYPE
#undef PP_SECTION_NAME
#undef PP_PROCEDURE_TYPE
#undef PP_PROCEDURE_NAME