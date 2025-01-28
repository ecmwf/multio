! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

#define PP_FILE_NAME 'test_read05_prog.F90'
#define PP_SECTION_TYPE 'PROGRAM'
#define PP_SECTION_NAME 'TEST_READ05_PROG'
#define PP_PROCEDURE_TYPE 'PROGRAM'
#define PP_PROCEDURE_NAME 'MAIN'
PROGRAM TEST_READ05_PROG

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K


  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATIONS_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_NEW_CONFIGURATION_FROM_FILE
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_HAS_KEY
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_GET_SUBCONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATIONS
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION

  USE :: HOOKS_MOD, ONLY: HOOKS_T
  USE :: INTOP_FACTORY_MOD, ONLY: MAKE_INTOP
  USE :: INTOP_FACTORY_MOD, ONLY: DESTROY_INTOP
  USE :: INTOP_BASE_MOD,    ONLY: INTOP_BASE_A

  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Test encodr object
  TYPE(HOOKS_T) :: HOOKS
  TYPE(YAML_CONFIGURATION_T) :: CONFIG
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: UNIT
  INTEGER(KIND=JPIB_K) :: OFFSET
  INTEGER(KIND=JPIB_K) :: RET
  INTEGER(KIND=JPIB_K) :: LENGTH
  INTEGER(KIND=JPIB_K) :: RES
  LOGICAL :: HAS_INTOP
  TYPE(YAML_CONFIGURATION_T) :: INTOP_CONFIGURATION
  CLASS(INTOP_BASE_A), POINTER :: INTOP

  TYPE(FORTRAN_MESSAGE_T) :: MSG
  TYPE(PARAMETRIZATION_T) :: PAR

  LOGICAL :: FEXIST


  CHARACTER(LEN=256) :: FNAME

  ! Error codes

  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_FILE = 0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CFG_FILE_DOES_NOT_EXIST = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MAKE_INTOP = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INTOP_DEALLOCATION_ERROR = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INTOP_PRINT_ERROR = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INTOP_EVAL_ERROR = 5_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the first command-line argument (index starts from 1)
  FNAME = REPEAT(' ', 256)
  IF ( COMMAND_ARGUMENT_COUNT() .EQ. 1 ) THEN
    ! Get the first command-line argument (index starts from 1)
    CALL GET_COMMAND_ARGUMENT(1, FNAME, LENGTH, RET )
    IF (RET .NE. 0) THEN
      FNAME = 'config-expression.yaml'
    END IF
  ELSE
    FNAME = 'config-expression.yaml'
  END IF

  ! Inquire file existence
  INQUIRE( FILE=TRIM(ADJUSTL(FNAME)), EXIST=FEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. FEXIST, ERRFLAG_CFG_FILE )

  !> Set the unit and offset
  UNIT = 6
  OFFSET = 1
  CALL HOOKS%DEBUG_HOOK_%INIT( )


  MSG%PARAM=220221
  MSG%LEVTYPE=6
  MSG%LEVELIST=137

  !> Open the configuration file
  PP_TRYCALL(ERRFLAG_CFG_FILE_DOES_NOT_EXIST) YAML_NEW_CONFIGURATION_FROM_FILE( TRIM(ADJUSTL(FNAME)), CONFIG, HOOKS )

  !> Read the encoder configuration
  ! PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_CFG) YAML_CONFIGURATION_HAS_KEY( CONFIG, 'type', HAS_INTOP, HOOKS )
  ! PP_DEBUG_CRITICAL_COND_THROW( .NOT. HAS_INTOP, ERRFLAG_SECTIONS_UNDEFINED )

  !> Read all the subconfigurations
  ! PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SUBCFG) YAML_READ_STRING( CONFIG, 'type', INTOP_CONFIGURATION, HOOKS )

  !> Deallocate section configuration
  PP_TRYCALL(ERRFLAG_MAKE_INTOP) MAKE_INTOP( INTOP, 'binary-op', CONFIG, HOOKS )

  !> Destroy the configuration object
  ! PP_TRYCALL(ERRFLAG_DELETE_CONFIGURATIONS) YAML_DELETE_CONFIGURATIONS( INTOP_CONFIGURATION, HOOKS )

  !> Deallocate section configuration
  PP_TRYCALL( ERRFLAG_INTOP_DEALLOCATION_ERROR ) YAML_DELETE_CONFIGURATION( CONFIG, HOOKS )

  !> Print all the intop
  ! DO I = 1, SIZE(INTOP)
  !   PP_TRYCALL( ERRFLAG_RULE_PRINT_ERROR ) INTOP(I)%RULE_%PRINT( UNIT, OFFSET, HOOKS )
  ! END DO

  ! Deallocate all the intop
  ! PP_TRYCALL( ERRFLAG_INTOP_DEALLOCATION_ERROR ) DESTROY_INTOP( INTOP, HOOKS )

  !> Write the intop to the file
  PP_TRYCALL( ERRFLAG_INTOP_PRINT_ERROR ) INTOP%PRINT( 6_JPIB_K, 0_JPIB_K, HOOKS )
  PP_TRYCALL( ERRFLAG_INTOP_EVAL_ERROR ) INTOP%EVAL( MSG, PAR, RES, HOOKS )
  WRITE(*,*) 'RES: ', RES

  !> Be sure we don't have any memory leaks
  CALL HOOKS%DEBUG_HOOK_%FREE( )

  !> Exit point (on success)
  STOP 0

! Error handler
PP_ERROR_HANDLER

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_CFG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open the configuration file' )
    CASE (ERRFLAG_CFG_FILE_DOES_NOT_EXIST)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'The configuration file does not exist' )
    CASE (ERRFLAG_MAKE_INTOP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to make the intop' )
    CASE (ERRFLAG_INTOP_DEALLOCATION_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate the intop' )
    CASE (ERRFLAG_INTOP_PRINT_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print the intop' )
    CASE (ERRFLAG_INTOP_EVAL_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to evaluate the intop' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Print the error stack
    CALL HOOKS%DEBUG_HOOK_%PRINT_ERROR_STACK( 6_JPIB_K )

    ! Free the error stack
    CALL HOOKS%DEBUG_HOOK_%FREE( )

    ! Write the error message and stop the program
    PP_DEBUG_ABORT()

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  STOP 1

END PROGRAM TEST_READ05_PROG
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME