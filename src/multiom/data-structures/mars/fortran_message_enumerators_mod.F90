! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'fortran_message_enumerators_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'FORTRAN_MESSAGE_ENUMERATORS_MOD'
MODULE FORTRAN_MESSAGE_ENUMERATORS_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

!> Default visibiliity of the module
PRIVATE

  !> Intger enumerators
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_STREAM_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_TYPE_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_CLASS_E=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_ORIGIN_E=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_ANOFFSET_E=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_NUMBER_E=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_IDENT_E=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_INSTRUMENT_E=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_CHANNEL_E=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_PARAM_TYPE_E=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_CHEM_E=11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_PARAM_E=12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_LEVTYPE_E=13_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_LEVELIST_E=14_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_DIRECTION_E=15_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_FREQUENCY_E=16_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_MODEL_E=17_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_REPRES_E=18_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_DATE_E=19_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_TIME_E=20_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_STEP_E=21_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGINTFLD_PACKING_E=22_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_MSGINTFLDS=22_JPIB_K

  ! String enumerators
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGSTRFLD_EXPVER_E=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: MSGSTRFLD_GRID_E=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: N_MSGSTRFLDS=2_JPIB_K

  ! Float enumerators
  INTEGER(KIND=JPIB_K), PARAMETER :: N_MSGFLOATFLDS=0_JPIB_K


  !> Whitelist of public symbols (parameters)

  ! Integer enumerators
  PUBLIC :: MSGINTFLD_STREAM_E
  PUBLIC :: MSGINTFLD_TYPE_E
  PUBLIC :: MSGINTFLD_CLASS_E
  PUBLIC :: MSGINTFLD_ORIGIN_E
  PUBLIC :: MSGINTFLD_ANOFFSET_E
  PUBLIC :: MSGINTFLD_NUMBER_E
  PUBLIC :: MSGINTFLD_IDENT_E
  PUBLIC :: MSGINTFLD_INSTRUMENT_E
  PUBLIC :: MSGINTFLD_CHANNEL_E
  PUBLIC :: MSGINTFLD_PARAM_TYPE_E
  PUBLIC :: MSGINTFLD_CHEM_E
  PUBLIC :: MSGINTFLD_PARAM_E
  PUBLIC :: MSGINTFLD_LEVTYPE_E
  PUBLIC :: MSGINTFLD_LEVELIST_E
  PUBLIC :: MSGINTFLD_DIRECTION_E
  PUBLIC :: MSGINTFLD_FREQUENCY_E
  PUBLIC :: MSGINTFLD_MODEL_E
  PUBLIC :: MSGINTFLD_REPRES_E
  PUBLIC :: MSGINTFLD_DATE_E
  PUBLIC :: MSGINTFLD_TIME_E
  PUBLIC :: MSGINTFLD_STEP_E
  PUBLIC :: MSGINTFLD_PACKING_E
  PUBLIC :: N_MSGINTFLDS

  ! String enumerators
  PUBLIC :: MSGSTRFLD_EXPVER_E
  PUBLIC :: MSGSTRFLD_GRID_E
  PUBLIC :: N_MSGSTRFLDS

  ! Float enumerators
  PUBLIC :: N_MSGFLOATFLDS


  !> Whitelist of public symbols (procedures)
  PUBLIC :: IMSGINTFLDS2CMSGINTFLDS
  PUBLIC :: CMSGINTFLDS2IMSGINTFLDS
  PUBLIC :: IMSGFLOATFLDS2CMSGFLOATFLDS
  PUBLIC :: CMSGFLOATFLDS2IMSGFLOATFLDS
  PUBLIC :: IMSGSTRINGFLDS2CMSGSTRINGFLDS
  PUBLIC :: CMSGSTRINGFLDS2IMSGSTRINGFLDS

  PUBLIC :: HAS_CMSGFLOATFLDS
  PUBLIC :: HAS_CMSGINTFLDS
  PUBLIC :: HAS_CMSGSTRINGFLDS
  PUBLIC :: HAS_IMSGFLOATFLDS
  PUBLIC :: HAS_IMSGINTFLDS
  PUBLIC :: HAS_IMSGSTRINGFLDS

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IMSGINTFLDS2CMSGINTFLDS'
PP_THREAD_SAFE FUNCTION IMSGINTFLDS2CMSGINTFLDS( IMSGINTFLDS, CMSGINTFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMSGINTFLDS
  CHARACTER(LEN=16),    INTENT(OUT)   :: CMSGINTFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_MSGINTFLD_UNARY=1_JPIB_K

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

  !> Initialization of the output variable
  CMSGINTFLDS = REPEAT(' ', 16)

  !> Select the prefix
  SELECT CASE ( IMSGINTFLDS )
  CASE (MSGINTFLD_STREAM_E)
    CMSGINTFLDS = 'stream'
  CASE (MSGINTFLD_TYPE_E)
    CMSGINTFLDS = 'type'
  CASE (MSGINTFLD_CLASS_E)
    CMSGINTFLDS = 'class'
  CASE (MSGINTFLD_ORIGIN_E)
    CMSGINTFLDS = 'origin'
  CASE (MSGINTFLD_ANOFFSET_E)
    CMSGINTFLDS = 'anoffset'
  CASE (MSGINTFLD_NUMBER_E)
    CMSGINTFLDS = 'number'
  CASE (MSGINTFLD_IDENT_E)
    CMSGINTFLDS = 'ident'
  CASE (MSGINTFLD_INSTRUMENT_E)
    CMSGINTFLDS = 'instrument'
  CASE (MSGINTFLD_CHANNEL_E)
    CMSGINTFLDS = 'channel'
  CASE (MSGINTFLD_PARAM_TYPE_E)
    CMSGINTFLDS = 'param-type'
  CASE (MSGINTFLD_CHEM_E)
    CMSGINTFLDS = 'chem'
  CASE (MSGINTFLD_PARAM_E)
    CMSGINTFLDS = 'param'
  CASE (MSGINTFLD_LEVTYPE_E)
    CMSGINTFLDS = 'levtype'
  CASE (MSGINTFLD_LEVELIST_E)
    CMSGINTFLDS = 'levelist'
  CASE (MSGINTFLD_DIRECTION_E)
    CMSGINTFLDS = 'direction'
  CASE (MSGINTFLD_FREQUENCY_E)
    CMSGINTFLDS = 'frequency'
  CASE (MSGINTFLD_MODEL_E)
    CMSGINTFLDS = 'model'
  CASE (MSGINTFLD_REPRES_E)
    CMSGINTFLDS = 'repres'
  CASE (MSGINTFLD_DATE_E)
    CMSGINTFLDS = 'date'
  CASE (MSGINTFLD_TIME_E)
    CMSGINTFLDS = 'time'
  CASE (MSGINTFLD_STEP_E)
    CMSGINTFLDS = 'step'
  CASE (MSGINTFLD_PACKING_E)
    CMSGINTFLDS = 'packing'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_MSGINTFLD_UNARY )
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
    CHARACTER(LEN=16) :: TMPSTR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNKNOWN_MSGINTFLD_UNARY)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IMSGINTFLDS
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown iintop_unary: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IMSGINTFLDS2CMSGINTFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMSGINTFLDS2IMSGINTFLDS'
PP_THREAD_SAFE FUNCTION CMSGINTFLDS2IMSGINTFLDS( CMSGINTFLDS, IMSGINTFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: CMSGINTFLDS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IMSGINTFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CMSGINTFLDS)) :: LOC_CMSGINTFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_MSGINTFLD_UNARY=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=2_JPIB_K

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

  !> Initialization of the output variable
  IMSGINTFLDS = UNDEF_PARAM_E

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CMSGINTFLDS, LOC_CMSGINTFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CMSGINTFLDS)) )
  CASE( 'stream' )
    IMSGINTFLDS = MSGINTFLD_STREAM_E
  CASE( 'type' )
    IMSGINTFLDS = MSGINTFLD_TYPE_E
  CASE( 'class' )
    IMSGINTFLDS = MSGINTFLD_CLASS_E
  CASE( 'origin' )
    IMSGINTFLDS = MSGINTFLD_ORIGIN_E
  CASE( 'anoffset' )
    IMSGINTFLDS = MSGINTFLD_ANOFFSET_E
  CASE( 'number' )
    IMSGINTFLDS = MSGINTFLD_NUMBER_E
  CASE( 'ident' )
    IMSGINTFLDS = MSGINTFLD_IDENT_E
  CASE( 'instrument' )
    IMSGINTFLDS = MSGINTFLD_INSTRUMENT_E
  CASE( 'channel' )
    IMSGINTFLDS = MSGINTFLD_CHANNEL_E
  CASE( 'param-type' )
    IMSGINTFLDS = MSGINTFLD_PARAM_TYPE_E
  CASE( 'chem' )
    IMSGINTFLDS = MSGINTFLD_CHEM_E
  CASE( 'param' )
    IMSGINTFLDS = MSGINTFLD_PARAM_E
  CASE( 'levtype' )
    IMSGINTFLDS = MSGINTFLD_LEVTYPE_E
  CASE( 'levelist' )
    IMSGINTFLDS = MSGINTFLD_LEVELIST_E
  CASE( 'direction' )
    IMSGINTFLDS = MSGINTFLD_DIRECTION_E
  CASE( 'frequency' )
    IMSGINTFLDS = MSGINTFLD_FREQUENCY_E
  CASE( 'model' )
    IMSGINTFLDS = MSGINTFLD_MODEL_E
  CASE( 'repres' )
    IMSGINTFLDS = MSGINTFLD_REPRES_E
  CASE( 'date' )
    IMSGINTFLDS = MSGINTFLD_DATE_E
  CASE( 'time' )
    IMSGINTFLDS = MSGINTFLD_TIME_E
  CASE( 'step' )
    IMSGINTFLDS = MSGINTFLD_STEP_E
  CASE( 'packing' )
    IMSGINTFLDS = MSGINTFLD_PACKING_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_MSGINTFLD_UNARY )
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
    CASE (ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
    CASE (ERRFLAG_UNKNOWN_MSGINTFLD_UNARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cintop_unary: '//TRIM(ADJUSTL(CMSGINTFLDS)) )
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

END FUNCTION CMSGINTFLDS2IMSGINTFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HAS_CMSGINTFLDS'
PP_THREAD_SAFE FUNCTION HAS_CMSGINTFLDS( CMSGINTFLDS, HAS_FIELD, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*), INTENT(IN)    :: CMSGINTFLDS
  LOGICAL,          INTENT(OUT)   :: HAS_FIELD
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CMSGINTFLDS)) :: LOC_CMSGINTFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=1_JPIB_K

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

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CMSGINTFLDS, LOC_CMSGINTFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CMSGINTFLDS)) )
  CASE( 'stream' )
    HAS_FIELD = .TRUE.
  CASE( 'type' )
    HAS_FIELD = .TRUE.
  CASE( 'class' )
    HAS_FIELD = .TRUE.
  CASE( 'origin' )
    HAS_FIELD = .TRUE.
  CASE( 'anoffset' )
    HAS_FIELD = .TRUE.
  CASE( 'number' )
    HAS_FIELD = .TRUE.
  CASE( 'ident' )
    HAS_FIELD = .TRUE.
  CASE( 'instrument' )
    HAS_FIELD = .TRUE.
  CASE( 'channel' )
    HAS_FIELD = .TRUE.
  CASE( 'param-type' )
    HAS_FIELD = .TRUE.
  CASE( 'chem' )
    HAS_FIELD = .TRUE.
  CASE( 'param' )
    HAS_FIELD = .TRUE.
  CASE( 'levtype' )
    HAS_FIELD = .TRUE.
  CASE( 'levelist' )
    HAS_FIELD = .TRUE.
  CASE( 'direction' )
    HAS_FIELD = .TRUE.
  CASE( 'frequency' )
    HAS_FIELD = .TRUE.
  CASE( 'model' )
    HAS_FIELD = .TRUE.
  CASE( 'repres' )
    HAS_FIELD = .TRUE.
  CASE( 'date' )
    HAS_FIELD = .TRUE.
  CASE( 'time' )
    HAS_FIELD = .TRUE.
  CASE( 'step' )
    HAS_FIELD = .TRUE.
  CASE( 'packing' )
    HAS_FIELD = .TRUE.
  CASE DEFAULT
    HAS_FIELD = .FALSE.
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
    CASE (ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
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

END FUNCTION HAS_CMSGINTFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HAS_IMSGINTFLDS'
PP_THREAD_SAFE FUNCTION HAS_IMSGINTFLDS( IMSGINTFLDS, HAS_FIELD, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMSGINTFLDS
  LOGICAL,              INTENT(OUT)   :: HAS_FIELD
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

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

  !> Select the prefix
  SELECT CASE ( IMSGINTFLDS )
  CASE (MSGINTFLD_STREAM_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_TYPE_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_CLASS_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_ORIGIN_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_ANOFFSET_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_NUMBER_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_IDENT_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_INSTRUMENT_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_CHANNEL_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_PARAM_TYPE_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_CHEM_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_PARAM_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_LEVTYPE_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_LEVELIST_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_DIRECTION_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_FREQUENCY_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_MODEL_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_REPRES_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_DATE_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_TIME_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_STEP_E)
    HAS_FIELD = .TRUE.
  CASE (MSGINTFLD_PACKING_E)
    HAS_FIELD = .TRUE.
  CASE DEFAULT
    HAS_FIELD = .FALSE.
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION HAS_IMSGINTFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMSGFLOATFLDS2IMSGFLOATFLDS'
PP_THREAD_SAFE FUNCTION CMSGFLOATFLDS2IMSGFLOATFLDS( CMSGFLOATFLDS, IMSGFLOATFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: CMSGFLOATFLDS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IMSGFLOATFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CMSGFLOATFLDS)) :: LOC_CMSGFLOATFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_MSGFLOATFLD_UNARY=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=2_JPIB_K

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

  !> Initialization of the output variable
  IMSGFLOATFLDS = UNDEF_PARAM_E

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CMSGFLOATFLDS, LOC_CMSGFLOATFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CMSGFLOATFLDS)) )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_MSGFLOATFLD_UNARY )
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
    CASE (ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
    CASE (ERRFLAG_UNKNOWN_MSGFLOATFLD_UNARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cfloatop_unary: '//TRIM(ADJUSTL(CMSGFLOATFLDS)) )
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

END FUNCTION CMSGFLOATFLDS2IMSGFLOATFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IMSGFLOATFLDS2CMSGFLOATFLDS'
PP_THREAD_SAFE FUNCTION IMSGFLOATFLDS2CMSGFLOATFLDS( IMSGFLOATFLDS, CMSGFLOATFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMSGFLOATFLDS
  CHARACTER(LEN=16),    INTENT(OUT)   :: CMSGFLOATFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_MSGFLOATFLD_UNARY=1_JPIB_K

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

  !> Initialization of the output variable
  CMSGFLOATFLDS = REPEAT(' ', 16)

  !> Select the prefix
  SELECT CASE ( IMSGFLOATFLDS )
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_MSGFLOATFLD_UNARY )
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
    CHARACTER(LEN=16) :: TMPSTR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNKNOWN_MSGFLOATFLD_UNARY)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IMSGFLOATFLDS
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown iintop_unary: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IMSGFLOATFLDS2CMSGFLOATFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HAS_CMSGFLOATFLDS'
PP_THREAD_SAFE FUNCTION HAS_CMSGFLOATFLDS( CMSGFLOATFLDS, HAS_FIELD, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*), INTENT(IN)    :: CMSGFLOATFLDS
  LOGICAL,          INTENT(OUT)   :: HAS_FIELD
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CMSGFLOATFLDS)) :: LOC_CMSGFLOATFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=2_JPIB_K

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

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CMSGFLOATFLDS, LOC_CMSGFLOATFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CMSGFLOATFLDS)) )
  CASE DEFAULT
    HAS_FIELD = .FALSE.
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
    CASE (ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
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

END FUNCTION HAS_CMSGFLOATFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HAS_IMSGFLOATFLDS'
PP_THREAD_SAFE FUNCTION HAS_IMSGFLOATFLDS( IMSGFLOATFLDS, HAS_FIELD, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMSGFLOATFLDS
  LOGICAL,              INTENT(OUT)   :: HAS_FIELD
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

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

  !> Select the prefix
  SELECT CASE ( IMSGFLOATFLDS )
  CASE DEFAULT
    HAS_FIELD = .FALSE.
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION HAS_IMSGFLOATFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'CMSGSTRINGFLDS2IMSGSTRINGFLDS'
PP_THREAD_SAFE FUNCTION CMSGSTRINGFLDS2IMSGSTRINGFLDS( CMSGSTRINGFLDS, IMSGSTRINGFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: CMSGSTRINGFLDS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IMSGSTRINGFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CMSGSTRINGFLDS)) :: LOC_CMSGSTRINGFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_MSGSTRINGFLD_UNARY=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=2_JPIB_K

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

  !> Initialization of the output variable
  IMSGSTRINGFLDS = UNDEF_PARAM_E

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CMSGSTRINGFLDS, LOC_CMSGSTRINGFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CMSGSTRINGFLDS)) )
  CASE ('expver')
    IMSGSTRINGFLDS = MSGSTRFLD_EXPVER_E
  CASE ('grid')
    IMSGSTRINGFLDS = MSGSTRFLD_GRID_E
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_MSGSTRINGFLD_UNARY )
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
    CASE (ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
    CASE (ERRFLAG_UNKNOWN_MSGSTRINGFLD_UNARY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown cstringop_unary: '//TRIM(ADJUSTL(CMSGSTRINGFLDS)) )
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

END FUNCTION CMSGSTRINGFLDS2IMSGSTRINGFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IMSGSTRINGFLDS2CMSGSTRINGFLDS'
PP_THREAD_SAFE FUNCTION IMSGSTRINGFLDS2CMSGSTRINGFLDS( IMSGSTRINGFLDS, CMSGSTRINGFLDS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMSGSTRINGFLDS
  CHARACTER(LEN=16),    INTENT(OUT)   :: CMSGSTRINGFLDS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_MSGSTRINGFLD_UNARY=1_JPIB_K

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

  !> Initialization of the output variable
  CMSGSTRINGFLDS = REPEAT(' ', 16)

  !> Select the prefix
  SELECT CASE ( IMSGSTRINGFLDS )
  CASE (MSGSTRFLD_GRID_E)
    CMSGSTRINGFLDS = 'grid'
  CASE (MSGSTRFLD_EXPVER_E)
    CMSGSTRINGFLDS = 'expver'
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_MSGSTRINGFLD_UNARY )
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
    CHARACTER(LEN=16) :: TMPSTR

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNKNOWN_MSGSTRINGFLD_UNARY)
      TMPSTR = REPEAT(' ', 16)
      WRITE(TMPSTR,*) IMSGSTRINGFLDS
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown iintop_unary: '//TRIM(ADJUSTL(TMPSTR)) )
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

END FUNCTION IMSGSTRINGFLDS2CMSGSTRINGFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HAS_CMSGSTRINGFLDS'
PP_THREAD_SAFE FUNCTION HAS_CMSGSTRINGFLDS( CMSGSTRINGFLDS, HAS_FIELD, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*), INTENT(IN)    :: CMSGSTRINGFLDS
  LOGICAL,          INTENT(OUT)   :: HAS_FIELD
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=LEN_TRIM(CMSGSTRINGFLDS)) :: LOC_CMSGSTRINGFLDS

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LC=1_JPIB_K

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

  !> Convert prefix to lowercase
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LC) TOLOWER( CMSGSTRINGFLDS, LOC_CMSGSTRINGFLDS, HOOKS )

  !> Select the prefix
  SELECT CASE ( TRIM(ADJUSTL(LOC_CMSGSTRINGFLDS)) )
  CASE ('expver')
    HAS_FIELD = .TRUE.
  CASE ('grid')
    HAS_FIELD = .TRUE.
  CASE DEFAULT
    HAS_FIELD = .FALSE.
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
    CASE (ERRFLAG_UNABLE_TO_CONVERT_LC)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert to lowercase' )
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

END FUNCTION HAS_CMSGSTRINGFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HAS_IMSGSTRINGFLDS'
PP_THREAD_SAFE FUNCTION HAS_IMSGSTRINGFLDS( IMSGSTRINGFLDS, HAS_FIELD, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: GENERAL_UTILS_MOD, ONLY: TOLOWER
  USE :: ENUMERATORS_MOD,   ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMSGSTRINGFLDS
  LOGICAL,              INTENT(OUT)   :: HAS_FIELD
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

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

  !> Select the prefix
  SELECT CASE ( IMSGSTRINGFLDS )
  CASE (MSGSTRFLD_GRID_E)
    HAS_FIELD = .TRUE.
  CASE (MSGSTRFLD_EXPVER_E)
    HAS_FIELD = .TRUE.
  CASE DEFAULT
    HAS_FIELD = .FALSE.
  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (On success)
  RETURN

END FUNCTION HAS_IMSGSTRINGFLDS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE FORTRAN_MESSAGE_ENUMERATORS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
