! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

#define PP_FILE_NAME 'test_read01_prog.F90'
#define PP_SECTION_TYPE 'PROGRAM'
#define PP_SECTION_NAME 'TEST_READ01_PROG'
#define PP_PROCEDURE_TYPE 'PROGRAM'
#define PP_PROCEDURE_NAME 'MAIN'
PROGRAM TEST_READ01_PROG

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

  USE :: GRIB_SECTION_BASE_MOD,    ONLY: GRIB_SECTION_BASE_A
  USE :: GRIB_ENCODER_FACTORY_MOD, ONLY: MAKE_ENCODER
  USE :: GRIB_ENCODER_FACTORY_MOD, ONLY: DESTROY_ENCODER

  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_CONFIGURATION_T
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_NEW_CONFIGURATION_FROM_FILE
  USE :: YAML_CORE_UTILS_MOD, ONLY: YAML_DELETE_CONFIGURATION

  USE :: GRIB_ENCODER_OPTIONS_MOD, ONLY: GRIB_ENCODER_OPTIONS_T

  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T

  USE :: HOOKS_MOD, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

IMPLICIT NONE

  !> Test encodr object
  CLASS(GRIB_SECTION_BASE_A), POINTER :: ENCODER => NULL()
  TYPE(HOOKS_T) :: HOOKS
  TYPE(YAML_CONFIGURATION_T) :: CONFIG
  INTEGER(KIND=JPIB_K) :: UNIT
  INTEGER(KIND=JPIB_K) :: OFFSET
  INTEGER(KIND=JPIB_K) :: RET
  INTEGER(KIND=JPIB_K) :: LENGTH
  INTEGER(KIND=JPIB_K) :: ENCODER_TYPE
  LOGICAL :: FEXIST

  TYPE(PARAMETRIZATION_T) :: PAR
  TYPE(GRIB_ENCODER_OPTIONS_T) :: OPT
  CHARACTER(LEN=256) :: FNAME

  ! Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_CFG_FILE_DOES_NOT_EXIST = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_CFG_FILE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_MAKE_ENCODER = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_PRINT_ENCODER = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_DESTROY_ENCODER = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERR_FLAG_DELETE_CONFIGURATIONS = 7_JPIB_K


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS



  ! Get the first command-line argument (index starts from 1)
  FNAME = REPEAT(' ', 256)
  IF ( COMMAND_ARGUMENT_COUNT() .EQ. 1 ) THEN
    CALL GET_COMMAND_ARGUMENT(1, FNAME, LENGTH, RET )
    IF (RET .NE. 0) THEN
      FNAME = 'config.yaml'
    END IF
  ELSE
    FNAME = 'config.yaml'
  END IF

  ! Inquire file existence
  INQUIRE( FILE=TRIM(ADJUSTL(FNAME)), EXIST=FEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT. FEXIST, ERR_FLAG_CFG_FILE )

  !> Set the unit and offset
  UNIT = 6
  OFFSET = 1
  CALL HOOKS%DEBUG_HOOK_%INIT( )

  !> Open the configuration file
  PP_TRYCALL(ERR_FLAG_CFG_FILE) YAML_NEW_CONFIGURATION_FROM_FILE( TRIM(ADJUSTL(FNAME)), CONFIG, HOOKS )

  !> Create the encoder object
  PP_TRYCALL(ERR_FLAG_MAKE_ENCODER) MAKE_ENCODER( ENCODER, CONFIG, OPT, HOOKS )

  !> Print the encoder object
   PP_TRYCALL(ERR_FLAG_PRINT_ENCODER) ENCODER%PRINT( UNIT, OFFSET, OPT, HOOKS )

  !> Destroy the encoder object
  PP_TRYCALL(ERR_FLAG_DESTROY_ENCODER) DESTROY_ENCODER( ENCODER, OPT, HOOKS )

  !> Destroy the configuration object
  PP_TRYCALL(ERR_FLAG_DELETE_CONFIGURATIONS) YAML_DELETE_CONFIGURATION( CONFIG, HOOKS )

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
      CASE(ERR_FLAG_CFG_FILE_DOES_NOT_EXIST)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'The configuration file does not exist' )
      CASE(ERR_FLAG_CFG_FILE)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error opening the configuration file' )
      CASE(ERR_FLAG_MAKE_ENCODER)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error creating the encoder object' )
      CASE(ERR_FLAG_PRINT_ENCODER)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error printing the encoder object' )
      CASE(ERR_FLAG_DESTROY_ENCODER)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error destroying the encoder object' )
      CASE(ERR_FLAG_DELETE_CONFIGURATIONS)
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error deleting the configuration object' )
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

END PROGRAM TEST_READ01_PROG