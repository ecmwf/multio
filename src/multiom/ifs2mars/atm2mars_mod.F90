!> @file om_message_mod.F90
!>
!> @brief Definition of a message for the output manager
!>
!> This module defines the message used by the output manager
!> and some utitlities to handle these messages
!>
!> @author Mirco Valentini
!> @date February 10, 2024

! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


! Definition of the module
#define PP_FILE_NAME 'atm2mars_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'ATM2MARS_MOD'
MODULE ATM2MARS_MOD
IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: ATM2MARS_SET_ANALYSIS
PUBLIC :: ATM2MARS_SET_ENSEMBLE
! PUBLIC :: ATM2MARS_SET_GEOMETRY
PUBLIC :: ATM2MARS_SET_LEVEL
! PUBLIC :: ATM2MARS_SET_WAVE
! PUBLIC :: ATM2MARS_SET_SATELLITE
! PUBLIC :: ATM2MARS_SET_SRFLEV

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_IS_ANALYSIS'
PP_THREAD_SAFE FUNCTION ATM2MARS_IS_ANALYSIS( IFS_MSG, IFS_PAR, IS_ANALYSIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,       ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T), INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),  INTENT(IN)    :: IFS_PAR
  LOGICAL,            INTENT(OUT)   :: IS_ANALYSIS
  TYPE(HOOKS_T),      INTENT(INOUT) :: HOOKS

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

  ! TODO: this check is very naive, it should be replaced with a more complex one
  IF ( IFS_PAR%SIM_%NLOCGRB .EQ. 36 ) THEN
    IS_ANALYSIS = .TRUE.
  ELSE
    IS_ANALYSIS = .FALSE.
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION ATM2MARS_IS_ANALYSIS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_IS_ENSEMBLE'
PP_THREAD_SAFE FUNCTION ATM2MARS_IS_ENSEMBLE( IFS_MSG, IFS_PAR, IS_ENSEMBLE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,       ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T
  USE :: ENUMERATORS_MOD,   ONLY: CTYPE2ITYPE
  USE :: ENUMERATORS_MOD,   ONLY: CCLASS2ICLASS
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_CF_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_PF_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_CV_E
  USE :: ENUMERATORS_MOD,   ONLY: STREAM_ENDA_E
  USE :: ENUMERATORS_MOD,   ONLY: STREAM_ELDA_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T), INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),  INTENT(IN)    :: IFS_PAR
  LOGICAL,            INTENT(OUT)   :: IS_ENSEMBLE
  TYPE(HOOKS_T),      INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ITYPE
  INTEGER(KIND=JPIB_K) :: ICLASS
  INTEGER(KIND=JPIB_K) :: ISTREAM
  LOGICAL, DIMENSION(5) :: CONDITIONS

  ! Error Flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TYPE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_CLASS=2_JPIB_K

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

  ! Set stream
  ISTREAM = IFS_PAR%SIM_%NSTREAM

  ! Set the message type
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TYPE) CTYPE2ITYPE( IFS_PAR%SIM_%CTYPE, ITYPE, HOOKS )

  ! Set the message class
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_CLASS) CCLASS2ICLASS( IFS_PAR%SIM_%CFCLASS, ICLASS, HOOKS )

  ! Set ensemble number
  CONDITIONS(1) = (ITYPE.EQ.TYPE_CF_E)
  CONDITIONS(2) = (ITYPE.EQ.TYPE_PF_E)
  CONDITIONS(3) = (ITYPE.EQ.TYPE_CV_E)
  CONDITIONS(4) = (ISTREAM.EQ.STREAM_ENDA_E)
  CONDITIONS(5) = (ISTREAM.EQ.STREAM_ELDA_E)

  ! TODO: this check is very naive, it should be replaced with a more complex one
  IF ( ANY(CONDITIONS) ) THEN
    IS_ENSEMBLE = .TRUE.
  ELSE
    IS_ENSEMBLE = .FALSE.
  ENDIF

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_CONVERT_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert the type of the message' )
    CASE (ERRFLAG_UNABLE_TO_CONVERT_CLASS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert the class of the message' )
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

  ! Exit point on error
  RETURN

END FUNCTION ATM2MARS_IS_ENSEMBLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_SET_ANALYSIS'
PP_THREAD_SAFE FUNCTION ATM2MARS_SET_ANALYSIS( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: ENUMERATORS_MOD,     ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T),      INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),       INTENT(IN)    :: IFS_PAR
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_ANALYSIS

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IS_ANALYSIS=1_JPIB_K

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

  ! TODO: this check is very naive, it should be replaced with a more complex one
  PP_TRYCALL(ERRFLAG_IS_ANALYSIS) ATM2MARS_IS_ANALYSIS( IFS_MSG, IFS_PAR, IS_ANALYSIS, HOOKS )

  ! If it is analysis configure mars and context
  IF ( IS_ANALYSIS ) THEN
!    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_MARS) MSG%SET( 'anoffset', IFS_PAR%SIM_%NWINOFF )
!    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_PARAMETRIZATION) PAR%SET( 'widthOfAnalysisWindow', IFS_PAR%SIM_%NWINSIZE )
!     MSG%ANOFFSET = IFS_PAR%SIM_%NWINOFF
!     PAR%ANALYSIS%LENGTH_OF_TIME_WINDOW_ = UNDEF_PARAM_E
!   ELSE
!     MSG%ANOFFSET = UNDEF_PARAM_E
!     PAR%ANALYSIS%LENGTH_OF_TIME_WINDOW_ = UNDEF_PARAM_E
  ENDIF

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_IS_ANALYSIS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in calling ATM2MARS_IS_ANALYSIS' )
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

  ! Exit point on error
  RETURN

END FUNCTION ATM2MARS_SET_ANALYSIS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_SET_ENSEMBLE'
PP_THREAD_SAFE FUNCTION ATM2MARS_SET_ENSEMBLE( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: ENUMERATORS_MOD,     ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T),      INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),       INTENT(IN)    :: IFS_PAR
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: IS_ENSEMBLE

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IS_ENSEMBLE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_MARS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_PARAMETRIZATION=3_JPIB_K

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

  ! TODO: this check is very naive, it should be replaced with a more complex one
  PP_TRYCALL(ERRFLAG_IS_ENSEMBLE) ATM2MARS_IS_ENSEMBLE( IFS_MSG, IFS_PAR, IS_ENSEMBLE, HOOKS )

  ! If it is analysis configure mars and context
  IF ( IS_ENSEMBLE ) THEN
!    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_MARS) MSG%SET( 'number', IFS_PAR%SIM_%NENSFNB )
!    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_PARAMETRIZATION) PAR%SET( 'typeOfEnsembleForecast', IFS_PAR%SIM_%NENSFNB )
!    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_PARAMETRIZATION) PAR%SET( 'numberOfForecastsInEnsemble', IFS_PAR%SIM_%NENSFNB )
!    MSG%NUMBER = IFS_PAR%SIM_%NENSFNB
!    PAR%ENSEMBLE%TYPE_OF_ENSEMBLE_FORECAST_ = 1_JPIB_K
!    PAR%ENSEMBLE%NUMBER_OF_FORECASTS_IN_ENSEMBLE_ = IFS_PAR%SIM_%NTOTENS
!  ELSE
!    MSG%NUMBER = UNDEF_PARAM_E
!    PAR%ENSEMBLE%TYPE_OF_ENSEMBLE_FORECAST_ = UNDEF_PARAM_E
!    PAR%ENSEMBLE%NUMBER_OF_FORECASTS_IN_ENSEMBLE_ = UNDEF_PARAM_E
  ENDIF

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_IS_ENSEMBLE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in calling ATM2MARS_IS_ENSEMBLE' )
    CASE(ERRFLAG_UNABLE_TO_SET_MARS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in setting the number of ensemble' )
    CASE(ERRFLAG_UNABLE_TO_SET_PARAMETRIZATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in setting the parametrization' )
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

  ! Exit point on error
  RETURN

END FUNCTION ATM2MARS_SET_ENSEMBLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_NEEDS_PV_ARRAY'
PP_THREAD_SAFE FUNCTION ATM2MARS_NEEDS_PV_ARRAY( IFS_MSG, IFS_PAR, NEEDS_PV_ARRAY, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: ENUMERATORS_MOD,     ONLY: PREFIX_MODEL_LEVEL_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T), INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),  INTENT(IN)    :: IFS_PAR
  LOGICAL,            INTENT(OUT)   :: NEEDS_PV_ARRAY
  TYPE(HOOKS_T),      INTENT(INOUT) :: HOOKS

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

  ! At the moment we need the PV array only for the model levels
  IF ( IFS_MSG%IPREF_ .EQ. PREFIX_MODEL_LEVEL_E ) THEN
    NEEDS_PV_ARRAY = .TRUE.
  ELSE
    NEEDS_PV_ARRAY = .FALSE.
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION ATM2MARS_NEEDS_PV_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_INFER_GG_REPRES_FROM_IFS'
PP_THREAD_SAFE FUNCTION ATM2MARS_INFER_GG_REPRES_FROM_IFS( IFS_MSG, IFS_PAR, NAME, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T), INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),  INTENT(IN)    :: IFS_PAR
  CHARACTER(LEN=8),   INTENT(OUT)   :: NAME
  TYPE(HOOKS_T),      INTENT(INOUT) :: HOOKS

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

  ! TODO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION ATM2MARS_INFER_GG_REPRES_FROM_IFS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_INFER_SH_REPRES_FROM_IFS'
PP_THREAD_SAFE FUNCTION ATM2MARS_INFER_SH_REPRES_FROM_IFS( IFS_MSG, IFS_PAR, NAME, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T), INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),  INTENT(IN)    :: IFS_PAR
  CHARACTER(LEN=8),   INTENT(OUT)   :: NAME
  TYPE(HOOKS_T),      INTENT(INOUT) :: HOOKS

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

  ! TODO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION ATM2MARS_INFER_SH_REPRES_FROM_IFS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_GET_GG_REPRES_DEFINITION'
PP_THREAD_SAFE FUNCTION ATM2MARS_GET_GG_REPRES_DEFINITION( IFS_MSG, IFS_PAR, NAME, GRID_DEFINITION, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: REDUCED_GG_MAP_MOD,  ONLY: REDUCED_GG_GEOMETRY_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T),                   INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),                    INTENT(IN)    :: IFS_PAR
  CHARACTER(LEN=8),                     INTENT(IN)    :: NAME
  TYPE(REDUCED_GG_GEOMETRY_T), POINTER, INTENT(OUT)   :: GRID_DEFINITION
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

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

  ! TODO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION ATM2MARS_GET_GG_REPRES_DEFINITION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_GET_SH_REPRES_DEFINITION'
PP_THREAD_SAFE FUNCTION ATM2MARS_GET_SH_REPRES_DEFINITION( IFS_MSG, IFS_PAR, NAME, SPHERICAL_HARMONICS_DEFINITIONS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,           ONLY: JPIB_K
  USE :: HOOKS_MOD,                   ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,                 ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,                 ONLY: MODEL_PAR_T
  USE :: SPHERICAL_HARMONICS_MAP_MOD, ONLY: SPHERICAL_HARMONICS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T),                   INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),                    INTENT(IN)    :: IFS_PAR
  CHARACTER(LEN=8),                     INTENT(IN)    :: NAME
  TYPE(SPHERICAL_HARMONICS_T), POINTER, INTENT(OUT)   :: SPHERICAL_HARMONICS_DEFINITIONS
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

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

  ! TODO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION ATM2MARS_GET_SH_REPRES_DEFINITION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_SET_LEVEL'
PP_THREAD_SAFE FUNCTION ATM2MARS_SET_LEVEL( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: ENUMERATORS_MOD,     ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T),        INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T), TARGET, INTENT(IN)    :: IFS_PAR
  TYPE(FORTRAN_MESSAGE_T),   INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T),   INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: NEEDS_PV_ARRAY
  LOGICAL :: TO_BE_DEALLOCATED

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NEEDS_PV_ARRAY=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_PARAMETRIZATION=2_JPIB_K

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

  ! At the moment the PV array is only needed for the model levels
  PP_TRYCALL(ERRFLAG_NEEDS_PV_ARRAY) ATM2MARS_NEEDS_PV_ARRAY( IFS_MSG, IFS_PAR, NEEDS_PV_ARRAY, HOOKS )

  ! If PV array is needed, then associate it
  IF ( NEEDS_PV_ARRAY ) THEN
    TO_BE_DEALLOCATED = .FALSE.
!    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_PARAMETRIZATION) PAR%SET( 'pv', IFS_PAR%GEO_%ZVERT(1:2*(IFS_PAR%GEO_%IFLEV+1)), TO_BE_DEALLOCATED )
!    PAR%LEVELS%TO_BE_DEALLOCATED = .FALSE.
!    PAR%LEVELS%PV => IFS_PAR%GEO_%ZVERT(1:2*(IFS_PAR%GEO_%IFLEV+1))
!  ELSE
!    PAR%LEVELS%TO_BE_DEALLOCATED = .FALSE.
!    PAR%LEVELS%PV => NULL()
  ENDIF

  ! Set level and levtype
  MSG%LEVELIST = IFS_MSG%ILEVG_

  ! NOTE(S): prefix is not strictly levtype, in particular there are 3 big exceptions:
  ! 1) Prefix contains the wv_int and wv_spec that are not present in the levtype
  !    definition, but this function apply only to atmospehre
  ! 2) All soil fields are emitted from ifs as surfaces, but I deal with them in the mapping
  ! 3) hhl and hpl levtypes are not present in the prefix but I deal with them in the mapping
  MSG%LEVTYPE = IFS_MSG%IPREF_

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_NEEDS_PV_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in calling ATM2MARS_NEEDS_PV_ARRAY' )
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

  ! Exit point on error
  RETURN

END FUNCTION ATM2MARS_SET_LEVEL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_SET_GEOMETRY'
PP_THREAD_SAFE FUNCTION ATM2MARS_SET_GEOMETRY( IFS_MSG, IFS_PAR, REPRESENTATIONS, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: GEOMETRY_PAR_MOD,    ONLY: GEOMETRY_PAR_T
  USE :: IFS_MSG_MOD,         ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: ENUMERATORS_MOD,     ONLY: REPRES_LATLONG_E
  USE :: ENUMERATORS_MOD,     ONLY: REPRES_GAUSSIANGRID_E
  USE :: ENUMERATORS_MOD,     ONLY: REPRES_SPHERICALHARMONICS_E

  USE :: REGULAR_LL_MAP_MOD,          ONLY: REGULAR_LL_GEOMETRY_T
  USE :: REDUCED_GG_MAP_MOD,          ONLY: REDUCED_GG_GEOMETRY_T
  USE :: SPHERICAL_HARMONICS_MAP_MOD, ONLY: SPHERICAL_HARMONICS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T),           INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),            INTENT(IN)    :: IFS_PAR
  TYPE(GEOMETRY_PAR_T), TARGET, INTENT(IN)    :: REPRESENTATIONS
  TYPE(FORTRAN_MESSAGE_T),      INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T),      INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),                INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: LMATCH
  TYPE(REGULAR_LL_GEOMETRY_T), POINTER :: LL_REPRES_DESCRIPTION
  TYPE(REDUCED_GG_GEOMETRY_T), POINTER :: GG_REPRES_DESCRIPTION
  TYPE(SPHERICAL_HARMONICS_T), POINTER :: SH_REPRES_DESCRIPTION

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_IMPLEMENTED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_REPRES=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INFER_GG_REPRES_FROM_IFS=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHECK_GG_REPRES=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GENERATE_GG_REPRES=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PUSH_GG_REPRES=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INFER_SH_REPRES_FROM_IFS=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CHECK_SH_REPRES=8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_GENERATE_SH_REPRES=9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PUSH_SH_REPRES=10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_REPRES_NOT_ASSOCIATED=11_JPIB_K

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

  ! At the moment we need the PV array only for the model levels
  SELECT CASE ( IFS_MSG%IREPRES_ )

  ! ================================================================================================
  CASE (REPRES_LATLONG_E)

    ! Unexpected regular latlon from IFS
    LL_REPRES_DESCRIPTION => NULL()
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_NOT_IMPLEMENTED )

  ! ================================================================================================
  CASE (REPRES_GAUSSIANGRID_E)

    GG_REPRES_DESCRIPTION => NULL()
    MSG%REPRES = REPRES_GAUSSIANGRID_E

    PAR%GEOMETRY%LL_TO_BE_DEALLOCATED = .FALSE.
    PAR%GEOMETRY%GG_TO_BE_DEALLOCATED = .FALSE.
    PAR%GEOMETRY%SH_TO_BE_DEALLOCATED = .FALSE.

    PAR%GEOMETRY%LL => NULL()
    PAR%GEOMETRY%GG => REPRESENTATIONS%GG
    PAR%GEOMETRY%SH => NULL()

    ! Infer grid (type) from if parameters
    PP_TRYCALL(ERRFLAG_INFER_GG_REPRES_FROM_IFS) ATM2MARS_INFER_GG_REPRES_FROM_IFS( IFS_MSG, IFS_PAR, MSG%GRID, HOOKS )

    ! Check if the grid definition is already in the parameters
    PP_TRYCALL(ERRFLAG_CHECK_GG_REPRES) PAR%GEOMETRY%GG%MATCH( MSG%GRID, LMATCH, HOOKS )

    ! If the grid is not already in the parameters, then set it
    IF ( .NOT. LMATCH ) THEN
      PP_TRYCALL(ERRFLAG_GENERATE_GG_REPRES) ATM2MARS_GET_GG_REPRES_DEFINITION( IFS_MSG, IFS_PAR, MSG%GRID, GG_REPRES_DESCRIPTION, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(GG_REPRES_DESCRIPTION), ERRFLAG_REPRES_NOT_ASSOCIATED )
      PP_TRYCALL(ERRFLAG_PUSH_GG_REPRES) PAR%GEOMETRY%GG%PUSH( MSG%GRID, GG_REPRES_DESCRIPTION, HOOKS )
    ENDIF

  ! ================================================================================================
  CASE (REPRES_SPHERICALHARMONICS_E)

    SH_REPRES_DESCRIPTION => NULL()
    MSG%REPRES = REPRES_SPHERICALHARMONICS_E

    PAR%GEOMETRY%LL_TO_BE_DEALLOCATED = .FALSE.
    PAR%GEOMETRY%GG_TO_BE_DEALLOCATED = .FALSE.
    PAR%GEOMETRY%SH_TO_BE_DEALLOCATED = .FALSE.

    PAR%GEOMETRY%LL => NULL()
    PAR%GEOMETRY%GG => NULL()
    PAR%GEOMETRY%SH => REPRESENTATIONS%SH

    ! Infer grid (type) from if parameters
    PP_TRYCALL(ERRFLAG_INFER_SH_REPRES_FROM_IFS) ATM2MARS_INFER_SH_REPRES_FROM_IFS( IFS_MSG, IFS_PAR, MSG%GRID, HOOKS )

    ! Check if the grid definition is already in the parameters
    PP_TRYCALL(ERRFLAG_CHECK_SH_REPRES) PAR%GEOMETRY%SH%MATCH( MSG%GRID, LMATCH, HOOKS )

    ! If the grid is not already in the parameters, then set it
    IF ( .NOT. LMATCH ) THEN
      PP_TRYCALL(ERRFLAG_GENERATE_SH_REPRES) ATM2MARS_GET_SH_REPRES_DEFINITION( IFS_MSG, IFS_PAR, MSG%GRID, SH_REPRES_DESCRIPTION, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.ASSOCIATED(SH_REPRES_DESCRIPTION), ERRFLAG_REPRES_NOT_ASSOCIATED )
      PP_TRYCALL(ERRFLAG_PUSH_SH_REPRES) PAR%GEOMETRY%SH%PUSH( MSG%GRID, SH_REPRES_DESCRIPTION, HOOKS )
    ENDIF

  ! ================================================================================================
  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_REPRES )
  END SELECT

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
    CASE(ERRFLAG_NOT_IMPLEMENTED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unexpected regular latlon from IFS' )
    CASE(ERRFLAG_UNKNOWN_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown representation' )
    CASE(ERRFLAG_INFER_GG_REPRES_FROM_IFS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in calling ATM2MARS_INFER_GG_REPRES_FROM_IFS' )
    CASE(ERRFLAG_CHECK_GG_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in checking GG representation' )
    CASE(ERRFLAG_GENERATE_GG_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in generating GG representation' )
    CASE(ERRFLAG_PUSH_GG_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in pushing GG representation' )
    CASE(ERRFLAG_INFER_SH_REPRES_FROM_IFS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in calling ATM2MARS_INFER_SH_REPRES_FROM_IFS' )
    CASE(ERRFLAG_CHECK_SH_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in checking SH representation' )
    CASE(ERRFLAG_GENERATE_SH_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in generating SH representation' )
    CASE(ERRFLAG_PUSH_SH_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in pushing SH representation' )
    CASE(ERRFLAG_REPRES_NOT_ASSOCIATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Representation not associated' )
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

END FUNCTION ATM2MARS_SET_GEOMETRY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE ATM2MARS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
