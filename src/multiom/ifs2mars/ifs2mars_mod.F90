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
#define PP_FILE_NAME 'ifs2mars_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'IFS2MARS_MOD'
MODULE IFS2MARS_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

INTERFACE IFS2MARS_SET_IDENTIFICATION
MODULE PROCEDURE IFS2MARS_SET_IDENTIFICATION_ATM
MODULE PROCEDURE IFS2MARS_SET_IDENTIFICATION_WAM
END INTERFACE

! Whitelist of public symbols
PUBLIC :: IFS2MARS_SET_STREAM       ! STREAM
PUBLIC :: IFS2MARS_SET_TYPE         ! TYPE
PUBLIC :: IFS2MARS_SET_CLASS        ! CLASS
PUBLIC :: IFS2MARS_SET_EXPVER       ! EXPVER
PUBLIC :: IFS2MARS_SET_ANALYSIS     ! ANOFFSET
PUBLIC :: IFS2MARS_SET_ENSEMBLE     ! NUMBER
PUBLIC :: ATM2MARS_SET_LEVTYPE      ! LEVTYPE
PUBLIC :: ATM2MARS_SET_LEVELIST     ! LEVELIST
PUBLIC :: IFS2MARS_SET_PACKING      ! PACKING
PUBLIC :: IFS2MARS_SET_ORIGIN       ! ORIGIN
PUBLIC :: ATM2MARS_SET_SATELLITE    ! IDENT/INSTRUMENT/CHANNEL
PUBLIC :: ATM2MARS_SET_PARAM        ! PARAM/PARAM_TYPE/CHEM/WAVELENGTH
PUBLIC :: WAM2MARS_SET_PARAM        ! PARAM/PARAM_TYPE
PUBLIC :: IFS2MARS_SET_DATETIME     ! DATE/TIME/STEP/TIMEPROC
PUBLIC :: IFS2MARS_SET_GEOMETRY     ! GRID/REPRES
PUBLIC :: WAM2MARS_SET_DIRFREQ      ! DIRECTION/FREQUENCY
PUBLIC :: IFS2MARS_SET_IDENTIFICATION ! PAR::GENERATING_PROCESS_IDENTIFIER

CONTAINS
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WAM2MARS_IS_WAVE_SPECTRA'
PP_THREAD_SAFE FUNCTION WAM2MARS_IS_WAVE_SPECTRA( IFS_MSG, IFS_PAR, IS_WAVE_SPECTRA, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,       ONLY: OM_WAM_MSG_T
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T

  USE :: GRIB_CODES_MOD,    ONLY: NGRBCRRFL
  USE :: GRIB_CODES_MOD,    ONLY: NGRBCDRFL
  USE :: GRIB_CODES_MOD,    ONLY: NGRBCSBT
  USE :: GRIB_CODES_MOD,    ONLY: NGRBCLBT

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_WAM_MSG_T), INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),  INTENT(IN)    :: IFS_PAR
  LOGICAL,            INTENT(OUT)   :: IS_WAVE_SPECTRA
  TYPE(HOOKS_T),      INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL, DIMENSION(4) :: CONDITIONS

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

  ! Conditions to match the satellite message
  CONDITIONS(1) = (IFS_MSG%PARAM_ID_ .EQ. 140250)
  CONDITIONS(2) = (IFS_MSG%PARAM_ID_ .EQ. 140251)

  ! If any of the conditions match the it is a satellite message
  IF ( ANY(CONDITIONS) ) THEN
    IS_WAVE_SPECTRA = .TRUE.
  ELSE
    IS_WAVE_SPECTRA = .FALSE.
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION WAM2MARS_IS_WAVE_SPECTRA
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_IS_SATELLITE'
PP_THREAD_SAFE FUNCTION ATM2MARS_IS_SATELLITE( IFS_MSG, IFS_PAR, IS_SATELLITE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,       ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T

  USE :: GRIB_CODES_MOD,    ONLY: NGRBCRRFL
  USE :: GRIB_CODES_MOD,    ONLY: NGRBCDRFL
  USE :: GRIB_CODES_MOD,    ONLY: NGRBCSBT
  USE :: GRIB_CODES_MOD,    ONLY: NGRBCLBT

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
  LOGICAL,            INTENT(OUT)   :: IS_SATELLITE
  TYPE(HOOKS_T),      INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL, DIMENSION(4) :: CONDITIONS

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

  ! Conditions to match the satellite message
  CONDITIONS(1) = (IFS_MSG%PARAM_ID_ .EQ. NGRBCRRFL)
  CONDITIONS(2) = (IFS_MSG%PARAM_ID_ .EQ. NGRBCDRFL)
  CONDITIONS(3) = (IFS_MSG%PARAM_ID_ .EQ. NGRBCSBT)
  CONDITIONS(4) = (IFS_MSG%PARAM_ID_ .EQ. NGRBCLBT)

  ! If any of the conditions match the it is a satellite message
  IF ( ANY(CONDITIONS) ) THEN
    IS_SATELLITE = .TRUE.
  ELSE
    IS_SATELLITE = .FALSE.
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION ATM2MARS_IS_SATELLITE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_IS_ANALYSIS'
PP_THREAD_SAFE FUNCTION IFS2MARS_IS_ANALYSIS( IFS_MSG, IFS_PAR, IS_ANALYSIS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,       ONLY: OM_BASE_MSG_A
  USE :: IFS_PAR_MOD,       ONLY: MODEL_PAR_T
  USE :: ENUMERATORS_MOD,   ONLY: CTYPE2ITYPE
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_AN_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_IA_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_OI_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_3V_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_4V_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_EA_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_4I_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_PA_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_OR_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_TPA_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_GA_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_GAI_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_AI_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_AF_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_AB_E
  USE :: ENUMERATORS_MOD,   ONLY: TYPE_OAI_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(OM_BASE_MSG_A), INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),    INTENT(IN)    :: IFS_PAR
  LOGICAL,              INTENT(OUT)   :: IS_ANALYSIS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ITYPE
  LOGICAL, DIMENSION(16) :: CONDITIONS

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TYPE=1_JPIB_K

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

  ! Get the enum for type
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TYPE) CTYPE2ITYPE( IFS_PAR%SIM_%CTYPE, ITYPE, HOOKS )

  ! Initialize conditions
  CONDITIONS = .FALSE.

  !> List all the conditions
  CONDITIONS(1)  = (ITYPE .EQ. TYPE_AN_E )
  CONDITIONS(2)  = (ITYPE .EQ. TYPE_IA_E )
  CONDITIONS(3)  = (ITYPE .EQ. TYPE_OI_E )
  CONDITIONS(4)  = (ITYPE .EQ. TYPE_3V_E )
  CONDITIONS(5)  = (ITYPE .EQ. TYPE_4V_E )
  CONDITIONS(6)  = (ITYPE .EQ. TYPE_EA_E )
  CONDITIONS(7)  = (ITYPE .EQ. TYPE_4I_E )
  CONDITIONS(8)  = (ITYPE .EQ. TYPE_PA_E )
  CONDITIONS(9)  = (ITYPE .EQ. TYPE_OR_E )
  CONDITIONS(10) = (ITYPE .EQ. TYPE_TPA_E )
  CONDITIONS(11) = (ITYPE .EQ. TYPE_GA_E )
  CONDITIONS(12) = (ITYPE .EQ. TYPE_GAI_E )
  CONDITIONS(13) = (ITYPE .EQ. TYPE_AI_E )
  CONDITIONS(14) = (ITYPE .EQ. TYPE_AF_E )
  CONDITIONS(15) = (ITYPE .EQ. TYPE_AB_E )
  CONDITIONS(16) = (ITYPE .EQ. TYPE_OAI_E )

  ! TODO: this check is very naive, it should be replaced with a more complex one
  ! IF ( IFS_PAR%SIM_%NLOCGRB .EQ. 36 ) THEN ! -> Trivial check
  IF ( ANY(CONDITIONS) ) THEN
    IS_ANALYSIS = .TRUE.
  ELSE
    IS_ANALYSIS = .FALSE.
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

END FUNCTION IFS2MARS_IS_ANALYSIS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_IS_ENSEMBLE'
PP_THREAD_SAFE FUNCTION IFS2MARS_IS_ENSEMBLE( IFS_MSG, IFS_PAR, IS_ENSEMBLE, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,       ONLY: OM_BASE_MSG_A
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
  CLASS(OM_BASE_MSG_A), INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),    INTENT(IN)    :: IFS_PAR
  LOGICAL,              INTENT(OUT)   :: IS_ENSEMBLE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

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
  ! PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_CLASS) CCLASS2ICLASS( IFS_PAR%SIM_%CFCLASS, ICLASS, HOOKS )

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert the "type" of the message' )
    CASE (ERRFLAG_UNABLE_TO_CONVERT_CLASS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to convert the "class" of the message' )
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

END FUNCTION IFS2MARS_IS_ENSEMBLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_SET_TYPE'
PP_THREAD_SAFE FUNCTION IFS2MARS_SET_TYPE( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_BASE_MSG_A
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: ENUMERATORS_MOD,     ONLY: CTYPE2ITYPE

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(OM_BASE_MSG_A),    INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),       INTENT(IN)    :: IFS_PAR
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_TYPE=1_JPIB_K

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

  ! Set the message type
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_TYPE) CTYPE2ITYPE( IFS_PAR%SIM_%CTYPE, MSG%TYPE, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_CONVERT_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error unable to convert "type" to enum' )
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

END FUNCTION IFS2MARS_SET_TYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_SET_PACKING'
PP_THREAD_SAFE FUNCTION IFS2MARS_SET_PACKING( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_BASE_MSG_A
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: ENUMERATORS_MOD,     ONLY: PACKING_GRIB_CCSDS_E
  USE :: ENUMERATORS_MOD,     ONLY: PACKING_GRIB_COMPLEX_E
  USE :: ENUMERATORS_MOD,     ONLY: REPRES_GAUSSIANGRID_E
  USE :: ENUMERATORS_MOD,     ONLY: REPRES_SPHERICALHARMONICS_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(OM_BASE_MSG_A),    INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),       INTENT(IN)    :: IFS_PAR
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_REPRES=1_JPIB_K

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

  ! Set the message type
  SELECT CASE (IFS_MSG%IREPRES_)
  CASE ( REPRES_GAUSSIANGRID_E )
    MSG%PACKING = PACKING_GRIB_CCSDS_E
    PAR%DATA_REPRESENTATION%BITS_PER_VALUE_ = 16_JPIB_K
  CASE ( REPRES_SPHERICALHARMONICS_E )
    MSG%PACKING = PACKING_GRIB_COMPLEX_E
    PAR%DATA_REPRESENTATION%BITS_PER_VALUE_ = 16_JPIB_K
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_UNKNOWN_REPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown representation id' )
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

END FUNCTION IFS2MARS_SET_PACKING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_SET_ORIGIN'
PP_THREAD_SAFE FUNCTION IFS2MARS_SET_ORIGIN( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_BASE_MSG_A
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(OM_BASE_MSG_A),    INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),       INTENT(IN)    :: IFS_PAR
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

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

  ! Set the centre
  MSG%ORIGIN = 98_JPIB_K

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

END FUNCTION IFS2MARS_SET_ORIGIN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_SET_LEVTYPE'
PP_THREAD_SAFE FUNCTION ATM2MARS_SET_LEVTYPE( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: ENUMERATORS_MOD,     ONLY: IPREFIX2ILEVTYPE

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
  INTEGER(KIND=JPIB_K) :: IPREFIX

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_LEVTYPE=1_JPIB_K

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

  ! Set the message type
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_LEVTYPE) IPREFIX2ILEVTYPE( IFS_MSG%IPREF_, IFS_MSG%PARAM_ID_, IFS_MSG%ILEVG_, IFS_MSG%IREPRES_, MSG%LEVTYPE, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_CONVERT_LEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error unable to convert "levtype" to enum' )
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

END FUNCTION ATM2MARS_SET_LEVTYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_SET_CLASS'
PP_THREAD_SAFE FUNCTION IFS2MARS_SET_CLASS( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_BASE_MSG_A
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: ENUMERATORS_MOD,     ONLY: CCLASS2ICLASS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(OM_BASE_MSG_A),    INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),       INTENT(IN)    :: IFS_PAR
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_CLASS=1_JPIB_K

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

  ! Set the message type
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_CLASS) CCLASS2ICLASS( IFS_PAR%SIM_%CFCLASS, MSG%CLASS, HOOKS )

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
    CASE(ERRFLAG_UNABLE_TO_CONVERT_CLASS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error unable to convert "class" to enum' )
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

END FUNCTION IFS2MARS_SET_CLASS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_SET_STREAM'
PP_THREAD_SAFE FUNCTION IFS2MARS_SET_STREAM( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_BASE_MSG_A
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(OM_BASE_MSG_A),    INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),       INTENT(IN)    :: IFS_PAR
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

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

  ! Set stream
  MSG%STREAM = IFS_PAR%SIM_%NSTREAM

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION IFS2MARS_SET_STREAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_SET_EXPVER'
PP_THREAD_SAFE FUNCTION IFS2MARS_SET_EXPVER( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_BASE_MSG_A
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(OM_BASE_MSG_A),    INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),       INTENT(IN)    :: IFS_PAR
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

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

  ! Set stream
  MSG%EXPVER = IFS_PAR%SIM_%CNMEXP(1:4)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION IFS2MARS_SET_EXPVER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_SET_SATELLITE'
PP_THREAD_SAFE FUNCTION ATM2MARS_SET_SATELLITE( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T

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
  LOGICAL :: IS_SATELLITE

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IS_SATELLITE=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS1=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS2=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS3=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS4=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS5=7_JPIB_K

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
  PP_TRYCALL(ERRFLAG_IS_SATELLITE) ATM2MARS_IS_SATELLITE( IFS_MSG, IFS_PAR, IS_SATELLITE, HOOKS )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(IFS_PAR%SAT_%MSATID), ERRFLAG_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(IFS_PAR%SAT_%MINST), ERRFLAG_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(IFS_PAR%SAT_%MCHAN), ERRFLAG_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(IFS_PAR%SAT_%MSERIES), ERRFLAG_NOT_ALLOCATED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(IFS_PAR%SAT_%RCWN), ERRFLAG_NOT_ALLOCATED )

  PP_DEBUG_CRITICAL_COND_THROW( SIZE(IFS_PAR%SAT_%MSATID).LT.IFS_MSG%ILEVG_, ERRFLAG_OUT_OF_BOUNDS1 )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(IFS_PAR%SAT_%MINST).LT.IFS_MSG%ILEVG_, ERRFLAG_OUT_OF_BOUNDS2 )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(IFS_PAR%SAT_%MCHAN).LT.IFS_MSG%ILEVG_, ERRFLAG_OUT_OF_BOUNDS3 )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(IFS_PAR%SAT_%MSERIES).LT.IFS_MSG%ILEVG_, ERRFLAG_OUT_OF_BOUNDS4 )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(IFS_PAR%SAT_%RCWN).LT.IFS_MSG%ILEVG_, ERRFLAG_OUT_OF_BOUNDS5 )

  ! If it is analysis configure mars and context
  IF ( IS_SATELLITE ) THEN
    MSG%IDENT      = IFS_PAR%SAT_%MSATID(IFS_MSG%ILEVG_)
    MSG%INSTRUMENT = IFS_PAR%SAT_%MINST(IFS_MSG%ILEVG_)
    MSG%CHANNEL    = IFS_PAR%SAT_%MCHAN(IFS_MSG%ILEVG_)
    PAR%SATELLITE%SATELLITE_SERIES = IFS_PAR%SAT_%MSERIES(IFS_MSG%ILEVG_)
    PAR%SATELLITE%SCALED_FACTOR_OF_CENTRAL_VAWENUMBER = 0_JPIB_K
    PAR%SATELLITE%SCALED_VALUE_OF_CENTRAL_VAWENUMBER = 100_JPIB_K*NINT(IFS_PAR%SAT_%RCWN(IFS_MSG%ILEVG_))
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

    ! Local error variables
    CHARACTER(LEN=32) :: CTMP1
    CHARACTER(LEN=32) :: CTMP2

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_IS_SATELLITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in calling IFS2MARS_IS_SATELLITE' )
    CASE(ERRFLAG_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error satellite information not allocated' )
    CASE(ERRFLAG_OUT_OF_BOUNDS1)
      CTMP1 = REPEAT( ' ', 32 )
      CTMP2 = REPEAT( ' ', 32 )
      WRITE( CTMP1, '(I32)' ) SIZE(IFS_PAR%SAT_%MSATID)
      WRITE( CTMP2, '(I32)' ) IFS_MSG%ILEVG_
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in accessing satellites descriptors out of bounds memory' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Size of MSATID: ' // TRIM(ADJUSTL(CTMP1)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Index: ' // TRIM(ADJUSTL(CTMP2)) )
    CASE(ERRFLAG_OUT_OF_BOUNDS2)
      CTMP1 = REPEAT( ' ', 32 )
      CTMP2 = REPEAT( ' ', 32 )
      WRITE( CTMP1, '(I32)' ) SIZE(IFS_PAR%SAT_%MINST)
      WRITE( CTMP2, '(I32)' ) IFS_MSG%ILEVG_
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in accessing satellites descriptors out of bounds memory' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Size of MINST: ' // TRIM(ADJUSTL(CTMP1)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Index: ' // TRIM(ADJUSTL(CTMP2)) )
    CASE(ERRFLAG_OUT_OF_BOUNDS3)
      CTMP1 = REPEAT( ' ', 32 )
      CTMP2 = REPEAT( ' ', 32 )
      WRITE( CTMP1, '(I32)' ) SIZE(IFS_PAR%SAT_%MCHAN)
      WRITE( CTMP2, '(I32)' ) IFS_MSG%ILEVG_
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in accessing satellites descriptors out of bounds memory' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Size of MCHAN: ' // TRIM(ADJUSTL(CTMP1)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Index: ' // TRIM(ADJUSTL(CTMP2)) )
    CASE(ERRFLAG_OUT_OF_BOUNDS4)
      CTMP1 = REPEAT( ' ', 32 )
      CTMP2 = REPEAT( ' ', 32 )
      WRITE( CTMP1, '(I32)' ) SIZE(IFS_PAR%SAT_%MSERIES)
      WRITE( CTMP2, '(I32)' ) IFS_MSG%ILEVG_
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in accessing satellites descriptors out of bounds memory' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Size of MSERIES: ' // TRIM(ADJUSTL(CTMP1)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Index: ' // TRIM(ADJUSTL(CTMP2)) )
    CASE(ERRFLAG_OUT_OF_BOUNDS5)
      CTMP1 = REPEAT( ' ', 32 )
      CTMP2 = REPEAT( ' ', 32 )
      WRITE( CTMP1, '(I32)' ) SIZE(IFS_PAR%SAT_%RCWN)
      WRITE( CTMP2, '(I32)' ) IFS_MSG%ILEVG_
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in accessing satellites descriptors out of bounds memory' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Size of RCWN: ' // TRIM(ADJUSTL(CTMP1)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Index: ' // TRIM(ADJUSTL(CTMP2)) )
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

END FUNCTION ATM2MARS_SET_SATELLITE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_SET_ANALYSIS'
PP_THREAD_SAFE FUNCTION IFS2MARS_SET_ANALYSIS( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_BASE_MSG_A
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
  CLASS(OM_BASE_MSG_A),    INTENT(IN)    :: IFS_MSG
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
  PP_TRYCALL(ERRFLAG_IS_ANALYSIS) IFS2MARS_IS_ANALYSIS( IFS_MSG, IFS_PAR, IS_ANALYSIS, HOOKS )

  ! If it is analysis configure mars and context
  IF ( IS_ANALYSIS ) THEN
!    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_MARS) MSG%SET( 'anoffset', IFS_PAR%SIM_%NWINOFF )
!    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_PARAMETRIZATION) PAR%SET( 'widthOfAnalysisWindow', IFS_PAR%SIM_%NWINSIZE )
    PAR%ANALYSIS%LENGTH_OF_TIME_WINDOW_ = IFS_PAR%SIM_%NWINSIZE
    MSG%ANOFFSET = IFS_PAR%SIM_%NWINOFF
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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in calling IFS2MARS_IS_ANALYSIS' )
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

END FUNCTION IFS2MARS_SET_ANALYSIS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_SET_ENSEMBLE'
PP_THREAD_SAFE FUNCTION IFS2MARS_SET_ENSEMBLE( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_BASE_MSG_A
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
  CLASS(OM_BASE_MSG_A),    INTENT(IN)    :: IFS_MSG
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
  PP_TRYCALL(ERRFLAG_IS_ENSEMBLE) IFS2MARS_IS_ENSEMBLE( IFS_MSG, IFS_PAR, IS_ENSEMBLE, HOOKS )

  ! If it is analysis configure mars and context
  IF ( IS_ENSEMBLE ) THEN
!    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_MARS) MSG%SET( 'number', IFS_PAR%SIM_%NENSFNB )
!    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_PARAMETRIZATION) PAR%SET( 'typeOfEnsembleForecast', IFS_PAR%SIM_%NENSFNB )
!    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_PARAMETRIZATION) PAR%SET( 'numberOfForecastsInEnsemble', IFS_PAR%SIM_%NENSFNB )

    MSG%NUMBER = IFS_PAR%SIM_%NENSFNB
    PAR%ENSEMBLE%TYPE_OF_ENSEMBLE_FORECAST_ = 1_JPIB_K
    PAR%ENSEMBLE%NUMBER_OF_FORECASTS_IN_ENSEMBLE_ = IFS_PAR%SIM_%NTOTENS

    ! For Seasonal forecast data we need to set the system number and method number
    PAR%ENSEMBLE%SYSTEM_NUMBER_  = IFS_PAR%SIM_%NSYSTEM
    PAR%ENSEMBLE%METHOD_NUMBER_ = IFS_PAR%SIM_%NMETHOD
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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in calling IFS2MARS_IS_ENSEMBLE' )
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

END FUNCTION IFS2MARS_SET_ENSEMBLE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_SET_IDENTIFICATION_ATM'
PP_THREAD_SAFE FUNCTION IFS2MARS_SET_IDENTIFICATION_ATM( ATM_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

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
  CLASS(OM_ATM_MSG_T),     INTENT(IN)    :: ATM_MSG
  TYPE(MODEL_PAR_T),       INTENT(IN)    :: IFS_PAR
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

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

  PAR%GENERATING_PROCESS_IDENTIFIER = IFS_PAR%SIM_%NCYCLE

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

END FUNCTION IFS2MARS_SET_IDENTIFICATION_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_SET_IDENTIFICATION_WAM'
PP_THREAD_SAFE FUNCTION IFS2MARS_SET_IDENTIFICATION_WAM( WAM_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_WAM_MSG_T
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
  CLASS(OM_WAM_MSG_T),     INTENT(IN)    :: WAM_MSG
  TYPE(MODEL_PAR_T),       INTENT(IN)    :: IFS_PAR
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

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

  IF (  IFS_PAR%WAM_%CLDOMAIN == 'g' ) THEN
    PAR%GENERATING_PROCESS_IDENTIFIER = IFS_PAR%WAM_%IMDLGRBID_G
  ELSE
    PAR%GENERATING_PROCESS_IDENTIFIER = IFS_PAR%WAM_%IMDLGRBID_M
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

END FUNCTION IFS2MARS_SET_IDENTIFICATION_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_SET_PARAM'
PP_THREAD_SAFE FUNCTION ATM2MARS_SET_PARAM( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_ATM_MSG_T
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: ENUMERATORS_MOD,     ONLY: PARAMTYPE_CHEMICAL_E
  USE :: ENUMERATORS_MOD,     ONLY: PARAMTYPE_OPTICAL_E
  USE :: ENUMERATORS_MOD,     ONLY: PARAMTYPE_CHEMICAL_OPTICAL_E
  USE :: ENUMERATORS_MOD,     ONLY: PARAMTYPE_BASE_E

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

  ! Extract paramtype
  IF ( IFS_MSG%PARAM_ID_ .GE. 400000000 .AND. IFS_MSG%PARAM_ID_ .LT. 500000000 ) THEN
    ! MSG%PARAM_TYPE = PARAMTYPE_CHEMICAL_OPTICAL_E
    MSG%PARAM = IFS_MSG%PARAM_ID_/1000000*1000
    MSG%CHEM  = MOD( IFS_MSG%PARAM_ID_/1000, 1000 )
    ! TODO: need to understand how to handle wavelength ranges
    ! WAVELENGTH_ID = MOD( IFS_MSG%PARAM_ID_, 1000 )
  ELSEIF ( IFS_MSG%PARAM_ID_ .GE. 400000 .AND. IFS_MSG%PARAM_ID_ .LT. 500000 ) THEN
    ! MSG%PARAM_TYPE = PARAMTYPE_CHEMICAL_E
    MSG%PARAM = (IFS_MSG%PARAM_ID_/1000)*1000
    MSG%CHEM  = MOD( IFS_MSG%PARAM_ID_, 1000 )
  ELSE
    ! MSG%PARAM_TYPE = PARAMTYPE_BASE_E
    MSG%PARAM = IFS_MSG%PARAM_ID_
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

END FUNCTION ATM2MARS_SET_PARAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WAM2MARS_SET_PARAM'
PP_THREAD_SAFE FUNCTION WAM2MARS_SET_PARAM( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_WAM_MSG_T
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: ENUMERATORS_MOD,     ONLY: PARAMTYPE_WAVE_SPECTRA_E
  USE :: ENUMERATORS_MOD,     ONLY: PARAMTYPE_BASE_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_WAM_MSG_T),      INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),       INTENT(IN)    :: IFS_PAR
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL, DIMENSION(2) :: CONDITIONS

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

  ! Checks
  CONDITIONS(1) = IFS_MSG%PARAM_ID_ .EQ. 140250
  CONDITIONS(2) = IFS_MSG%PARAM_ID_ .EQ. 140251
  ! TODO : more conditions to be added

  ! Extract paramtype
  IF ( ANY(CONDITIONS) ) THEN
    ! MSG%PARAM_TYPE = PARAMTYPE_WAVE_SPECTRA_E
    MSG%PARAM = IFS_MSG%PARAM_ID_
    ! TODO: need to understand how to handle wavelength ranges
    ! WAVELENGTH_ID = MOD( IFS_MSG%PARAM_ID_, 1000 )
  ELSE
    ! MSG%PARAM_TYPE = PARAMTYPE_BASE_E
    MSG%PARAM = IFS_MSG%PARAM_ID_
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

END FUNCTION WAM2MARS_SET_PARAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_SET_DATETIME'
PP_THREAD_SAFE FUNCTION IFS2MARS_SET_DATETIME( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_BASE_MSG_A
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: DATETIME_UTILS_MOD,  ONLY: UNPACK_YYYYMMDD
  USE :: DATETIME_UTILS_MOD,  ONLY: SEC2HH_MM_SS
  USE :: DATETIME_UTILS_MOD,  ONLY: PACK_YYYYMMDD
  USE :: DATETIME_UTILS_MOD,  ONLY: PACK_HHMMSS
  USE :: DATETIME_UTILS_MOD,  ONLY: DATE_SUB_DAYS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(OM_BASE_MSG_A),    INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),       INTENT(IN)    :: IFS_PAR
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL, DIMENSION(2) :: CONDITION1
  LOGICAL, DIMENSION(2) :: CONDITION2

  INTEGER(KIND=JPIB_K) :: DYYYY
  INTEGER(KIND=JPIB_K) :: DMM
  INTEGER(KIND=JPIB_K) :: DDD
  INTEGER(KIND=JPIB_K) :: THH
  INTEGER(KIND=JPIB_K) :: TMM
  INTEGER(KIND=JPIB_K) :: TSS
  INTEGER(KIND=JPIB_K) :: DYYYY1
  INTEGER(KIND=JPIB_K) :: DMM1
  INTEGER(KIND=JPIB_K) :: DDD1
  INTEGER(KIND=JPIB_K) :: THH1
  INTEGER(KIND=JPIB_K) :: TMM1
  INTEGER(KIND=JPIB_K) :: TSS1
  INTEGER(KIND=JPIB_K) :: IFCDA_INI
  INTEGER(KIND=JPIB_K) :: IFCHO_RES
  LOGICAL :: IS_ANALYSIS
  INTEGER(KIND=JPIB_K) :: DATE
  INTEGER(KIND=JPIB_K) :: TIME
  INTEGER(KIND=JPIB_K) :: STEP
  INTEGER(KIND=JPIB_K) :: TIMESTEP_IN_SECONDS

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_UNPACK_DATETIME=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVER_SECONDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PACK_DATE=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PACK_TIME=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SUB_DAYS=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CHECK_TYPE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SET_ANALYSIS=7_JPIB_K

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

  ! Extract date/time components
  PP_TRYCALL(ERRFLAG_UNABLE_TO_UNPACK_DATETIME) UNPACK_YYYYMMDD( IFS_PAR%SIM_%NINDAT, DYYYY, DMM, DDD, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVER_SECONDS) SEC2HH_MM_SS( IFS_PAR%SIM_%NSSSSS, THH, TMM, TSS, HOOKS )

  ! Initialization of the modified date/time
  ! NOTE: Apparently minutes and seconds are cut away in the grib encoding,
  !       not sure it is the correct way to proceed
  DYYYY1 = DYYYY
  DMM1   = DMM
  DDD1   = DDD
  THH1   = THH
  TMM1   = 0
  TSS1   = 0

  ! First special case
  CONDITION1(1) = (IFS_PAR%SIM_%CTYPE .EQ. 'fc') ! 'type' is forecast (gribCode=9)
  CONDITION1(2) = (IFS_PAR%SIM_%LOBSC1)          ! .T. = term of observations included in configuration 1

  ! Second special case
  CONDITION2(1) = (IFS_PAR%SIM_%LVAREPS)     ! .T. when running with variable resolution
  CONDITION2(2) = (IFS_PAR%SIM_%NLEG .GE. 2) ! current VAREPS leg number (eg 1(2) for the T399(T255) part of a T399-T255 VAREPS)

  ! TODO: Need to handle the analysis case and in that case add the offset to the date/time

  ! If needed modify the time
  IF ( ALL(CONDITION1) ) THEN
    ! NOTE: This code works because NSTEPINI is supposed to be less than 24
    ! NSTEPINI: Initial step in hours for the initial conditions
    !           at the beginning of 4D-Var trajectory (usually 3 hours).
    !           It is used to update the step while saving the FCs along
    !           the first trajectory.
    THH1 = THH - IFS_PAR%SIM_%NSTEPINI
    IF ( THH1 .LT. 0 ) THEN
      THH1 = THH1 + 24
      ! TODO: Replace custom function with Julian date provided in eccodes
      PP_TRYCALL(ERRFLAG_UNABLE_TO_SUB_DAYS) DATE_SUB_DAYS( DYYYY, DMM, DDD, INT(-1,JPIB_K), DYYYY1, DMM1, DDD1, HOOKS )
    ENDIF

  ELSEIF ( ALL(CONDITION2) ) THEN

    ! NFCHO_TRUNC_INI: forecast step used to define the ICs (ie NFCHO_TRUNC of previous VAREPS LEG)
    IFCDA_INI = IFS_PAR%SIM_%NFCHO_TRUNC_INI/24
    IFCHO_RES = MOD(IFS_PAR%SIM_%NFCHO_TRUNC_INI, 24)
    THH1 = THH - IFCHO_RES
    TMM1 = 0
    TSS1 = 0
    IF ( THH1 .LT. 0 ) THEN
      THH1 = THH1 + 24
      IFCDA_INI = IFCDA_INI + 1
    ENDIF
    ! TODO: Replace custom function with Julian date provided in eccodes
    PP_TRYCALL(ERRFLAG_UNABLE_TO_SUB_DAYS) DATE_SUB_DAYS( DYYYY, DMM, DDD, -IFCDA_INI, DYYYY1, DMM1, DDD1, HOOKS )

  ENDIF

  ! Output date and time
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PACK_DATE) PACK_YYYYMMDD( DYYYY1, DMM1, DDD1, DATE, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PACK_TIME) PACK_HHMMSS( THH1, TMM1, TSS1, TIME, HOOKS )

  ! Set the step
  STEP = IFS_MSG%ISTEP_
  TIMESTEP_IN_SECONDS = INT( IFS_PAR%SIM_%TSTEP, KIND=JPIB_K )

  ! TODO: If we do not set "timeproc" we rely on the default in the rules
  ! MSG%TIMEPROC = 'instant'

  ! Check if the simulation is analysis
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CHECK_TYPE) IFS2MARS_IS_ANALYSIS( IFS_MSG, IFS_PAR, IS_ANALYSIS, HOOKS )

  ! If it is analysis DATE/TIME/STEP should be set to the analysis time
  IF ( IS_ANALYSIS ) THEN
    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_ANALYSIS) IFS2MARS_SET_ANALYSIS_TIME( &
&     DATE, TIME, STEP, TIMESTEP_IN_SECONDS, MSG, PAR, HOOKS )
  ELSE
    MSG%DATE = DATE
    MSG%TIME = TIME
    MSG%STEP = STEP
    PAR%TIME%LENGTH_OF_TIME_STEP_IN_SECONDS_ = TIMESTEP_IN_SECONDS
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
    CASE(ERRFLAG_UNABLE_TO_UNPACK_DATETIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in unpacking date/time' )
    CASE(ERRFLAG_UNABLE_TO_CONVER_SECONDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in converting seconds to HH:MM:SS' )
    CASE(ERRFLAG_UNABLE_TO_PACK_DATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in packing date' )
    CASE(ERRFLAG_UNABLE_TO_PACK_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in packing time' )
    CASE(ERRFLAG_UNABLE_TO_SUB_DAYS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in subtracting days' )
    CASE(ERRFLAG_UNABLE_TO_CHECK_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in checking the type of simulation' )
    CASE(ERRFLAG_UNABLE_TO_SET_ANALYSIS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in setting the analysis time' )
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

END FUNCTION IFS2MARS_SET_DATETIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_SET_ANALYSIS_TIME'
PP_THREAD_SAFE FUNCTION IFS2MARS_SET_ANALYSIS_TIME( DATE, TIME, STEP, &
&  TIMESTEP_IN_SECONDS, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_BASE_MSG_A
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: DATETIME_UTILS_MOD,  ONLY: SEC2DD_SS
  USE :: DATETIME_UTILS_MOD,  ONLY: SEC2HH_MM_SS
  USE :: DATETIME_UTILS_MOD,  ONLY: HH_MM_SS2SEC
  USE :: DATETIME_UTILS_MOD,  ONLY: UNPACK_HHMMSS
  USE :: DATETIME_UTILS_MOD,  ONLY: PACK_HHMMSS
  USE :: DATETIME_UTILS_MOD,  ONLY: DATE_SUM_DAYS
  USE :: DATETIME_UTILS_MOD,  ONLY: UNPACK_YYYYMMDD
  USE :: DATETIME_UTILS_MOD,  ONLY: PACK_YYYYMMDD

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),    INTENT(IN)    :: DATE
  INTEGER(KIND=JPIB_K),    INTENT(IN)    :: TIME
  INTEGER(KIND=JPIB_K),    INTENT(IN)    :: STEP
  INTEGER(KIND=JPIB_K),    INTENT(IN)    :: TIMESTEP_IN_SECONDS
  TYPE(FORTRAN_MESSAGE_T), INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T), INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variabels
  INTEGER(KIND=JPIB_K) :: IN_YYYY
  INTEGER(KIND=JPIB_K) :: IN_MN
  INTEGER(KIND=JPIB_K) :: IN_DD
  INTEGER(KIND=JPIB_K) :: DELTA_DD
  INTEGER(KIND=JPIB_K) :: OUT_YYYY
  INTEGER(KIND=JPIB_K) :: OUT_MN
  INTEGER(KIND=JPIB_K) :: OUT_DD
  INTEGER(KIND=JPIB_K) :: HH
  INTEGER(KIND=JPIB_K) :: MS
  INTEGER(KIND=JPIB_K) :: SS
  INTEGER(KIND=JPIB_K) :: SEC
  INTEGER(KIND=JPIB_K) :: TOTAL_SEC

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_EXTRACT_HH_MM_SS=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_SECONDS=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_SECONDS_TO_DAYS=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_SECONDS_TO_HH_MM_SS=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PACK_TIME=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: UNABLE_TO_UNPACK_DATE=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_SUM_DAYS=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PACK_DATE=8_JPIB_K

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

  ! TODO: To be verified, THis logic uses gregorian calendar (Functions copied from IFS, but probably julian calendar need to be used)
  PP_TRYCALL(ERRFLAG_UNABLE_TO_EXTRACT_HH_MM_SS) UNPACK_HHMMSS( TIME, HH, MS, SS, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_SECONDS) HH_MM_SS2SEC( HH, MS, SS, SEC, HOOKS )

  TOTAL_SEC = SEC + STEP*TIMESTEP_IN_SECONDS

  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_SECONDS_TO_DAYS) SEC2DD_SS( TOTAL_SEC, DELTA_DD, SEC, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CONVERT_SECONDS_TO_HH_MM_SS) SEC2HH_MM_SS( SEC, HH, MS, SS, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PACK_TIME) PACK_HHMMSS( HH, MS, SS, MSG%TIME, HOOKS )

  PP_TRYCALL(UNABLE_TO_UNPACK_DATE) UNPACK_YYYYMMDD( DATE, IN_YYYY, IN_MN, IN_DD, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_SUM_DAYS) DATE_SUM_DAYS( IN_YYYY, IN_MN, IN_DD, DELTA_DD, OUT_YYYY, OUT_MN, OUT_DD, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PACK_DATE) PACK_YYYYMMDD( OUT_YYYY, OUT_MN, OUT_DD, MSG%DATE, HOOKS )

  MSG%STEP = 0_JPIB_K

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
    CASE(ERRFLAG_UNABLE_TO_EXTRACT_HH_MM_SS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in extracting HH:MM:SS' )
    CASE(ERRFLAG_UNABLE_TO_CONVERT_SECONDS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in converting seconds' )
    CASE(ERRFLAG_UNABLE_TO_CONVERT_SECONDS_TO_DAYS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in converting seconds to days' )
    CASE(ERRFLAG_UNABLE_TO_CONVERT_SECONDS_TO_HH_MM_SS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in converting seconds to HH:MM:SS' )
    CASE(ERRFLAG_UNABLE_TO_PACK_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in packing time' )
    CASE(UNABLE_TO_UNPACK_DATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in unpacking date' )
    CASE(ERRFLAG_UNABLE_TO_SUM_DAYS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in summing days' )
    CASE(ERRFLAG_UNABLE_TO_PACK_DATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in packing date' )
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

END FUNCTION IFS2MARS_SET_ANALYSIS_TIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_NEEDS_PV_ARRAY'
PP_THREAD_SAFE FUNCTION IFS2MARS_NEEDS_PV_ARRAY( IFS_MSG, IFS_PAR, NEEDS_PV_ARRAY, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_BASE_MSG_A
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
  CLASS(OM_BASE_MSG_A), INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),    INTENT(IN)    :: IFS_PAR
  LOGICAL,              INTENT(OUT)   :: NEEDS_PV_ARRAY
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

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

END FUNCTION IFS2MARS_NEEDS_PV_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ATM2MARS_SET_LEVELIST'
PP_THREAD_SAFE FUNCTION ATM2MARS_SET_LEVELIST( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_BASE_MSG_A
  USE :: IFS_MSG_MOD,         ONLY: OM_ATM_MSG_T
  USE :: IFS_MSG_MOD,         ONLY: OM_WAM_MSG_T
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
  CLASS(OM_BASE_MSG_A),      INTENT(IN)    :: IFS_MSG
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
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_CLASS=3_JPIB_K

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
  PP_TRYCALL(ERRFLAG_NEEDS_PV_ARRAY) IFS2MARS_NEEDS_PV_ARRAY( IFS_MSG, IFS_PAR, NEEDS_PV_ARRAY, HOOKS )

  ! If PV array is needed, then associate it
  IF ( NEEDS_PV_ARRAY ) THEN
    TO_BE_DEALLOCATED = .FALSE.
!    PP_TRYCALL(ERRFLAG_UNABLE_TO_SET_PARAMETRIZATION) PAR%SET( 'pv', IFS_PAR%GEO_%ZVERT(1:2*(IFS_PAR%GEO_%IFLEV+1)), TO_BE_DEALLOCATED )
!    PAR%LEVELS%TO_BE_DEALLOCATED = .FALSE.
    PAR%LEVELS%PV => IFS_PAR%GEO_%ZVERT(1:2*(IFS_PAR%GEO_%IFLEV+1))
!  ELSE
!    PAR%LEVELS%TO_BE_DEALLOCATED = .FALSE.
!    PAR%LEVELS%PV => NULL()
  ENDIF

  ! Set levelist
  SELECT TYPE(A  => IFS_MSG)
  CLASS IS (OM_ATM_MSG_T)
    MSG%LEVELIST = A%ILEVG_
  CLASS IS (OM_WAM_MSG_T)
    MSG%LEVELIST = 0_JPIB_K
  CLASS DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_CLASS )
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_NEEDS_PV_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in calling IFS2MARS_NEEDS_PV_ARRAY' )
    ! CASE(ERRFLAG_UNABLE_TO_SET_PARAMETRIZATION)
    !   PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in setting the parametrization' )
    CASE(ERRFLAG_UNKNOWN_CLASS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown class' )
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

END FUNCTION ATM2MARS_SET_LEVELIST
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IFS2MARS_SET_GEOMETRY'
PP_THREAD_SAFE FUNCTION IFS2MARS_SET_GEOMETRY( IFS_MSG, IFS_PAR, REPRES_NAME, &
&  REPRESENTATION, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: GEOMETRY_PAR_MOD,    ONLY: GEOMETRY_PAR_T
  USE :: IFS_MSG_MOD,         ONLY: OM_BASE_MSG_A
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  ! USE :: ENUMERATORS_MOD,     ONLY: REPRES_LATLONG_E
  USE :: ENUMERATORS_MOD,     ONLY: REPRES_GAUSSIANGRID_E
  USE :: ENUMERATORS_MOD,     ONLY: REPRES_SPHERICALHARMONICS_E
  USE :: REPRESENTATIONS_MOD, ONLY: REPRES_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(OM_BASE_MSG_A),     INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T),        INTENT(IN)    :: IFS_PAR
  CHARACTER(LEN=*),         INTENT(IN)    :: REPRES_NAME
  CLASS(REPRES_A), POINTER, INTENT(IN)    :: REPRESENTATION
  TYPE(FORTRAN_MESSAGE_T),  INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T),  INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),            INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNSUPPORTED_REPRESENTATION=1_JPIB_K

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

  IF ( IFS_MSG%IREPRES_ .EQ. REPRES_GAUSSIANGRID_E ) THEN
    MSG%REPRES = REPRES_GAUSSIANGRID_E
    MSG%GRID = TRIM(ADJUSTL(REPRES_NAME))
    PAR%GEOMETRY%TO_BE_DEALLOCATED = .FALSE.
    PAR%GEOMETRY%REPRES => REPRESENTATION
  ELSEIF ( IFS_MSG%IREPRES_ .EQ. REPRES_SPHERICALHARMONICS_E ) THEN
    MSG%REPRES = REPRES_SPHERICALHARMONICS_E
    MSG%TRUNCATION = IFS_PAR%GEO_%ISMAX
    PAR%GEOMETRY%TO_BE_DEALLOCATED = .FALSE.
    PAR%GEOMETRY%REPRES => REPRESENTATION
  ELSE
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNSUPPORTED_REPRESENTATION )
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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_UNSUPPORTED_REPRESENTATION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unsupported representation' )
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

END FUNCTION IFS2MARS_SET_GEOMETRY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WAM2MARS_SET_DIRFREQ'
PP_THREAD_SAFE FUNCTION WAM2MARS_SET_DIRFREQ( IFS_MSG, IFS_PAR, MSG, PAR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: IFS_MSG_MOD,         ONLY: OM_WAM_MSG_T
  USE :: IFS_PAR_MOD,         ONLY: MODEL_PAR_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD, ONLY: PARAMETRIZATION_T
  USE :: ENUMERATORS_MOD,     ONLY: LEVTYPE_SFC_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_WAM_MSG_T),         INTENT(IN)    :: IFS_MSG
  TYPE(MODEL_PAR_T), TARGET,  INTENT(IN)    :: IFS_PAR
  TYPE(FORTRAN_MESSAGE_T),    INTENT(INOUT) :: MSG
  TYPE(PARAMETRIZATION_T),    INTENT(INOUT) :: PAR
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL IS_WAVE_SPECTRA

  ! Error handling variables
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IS_SPECTRA=0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_TH_ALLOCATED=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_NOT_FR_ALLOCATED=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS_FRLB=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS_FRUB=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS_THLB=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUT_OF_BOUNDS_THUB=6_JPIB_K

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

  ! TODO: if it is wave spectra then we need to set the frequency and direction,
  !       otherwise need to set level and levetype
  PP_TRYCALL(ERRFLAG_IS_SPECTRA) WAM2MARS_IS_WAVE_SPECTRA( IFS_MSG, IFS_PAR, IS_WAVE_SPECTRA, HOOKS )

  IF ( IS_WAVE_SPECTRA ) THEN

    ! Error handling
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(IFS_PAR%WAM_%TH), ERRFLAG_NOT_TH_ALLOCATED )
    PP_DEBUG_CRITICAL_COND_THROW( .NOT.ALLOCATED(IFS_PAR%WAM_%FR), ERRFLAG_NOT_FR_ALLOCATED )
    PP_DEBUG_CRITICAL_COND_THROW( IFS_MSG%IANGLE .LT. 1_JPIB_K, ERRFLAG_OUT_OF_BOUNDS_THLB )
    PP_DEBUG_CRITICAL_COND_THROW( IFS_MSG%IANGLE .GT. SIZE(IFS_PAR%WAM_%TH), ERRFLAG_OUT_OF_BOUNDS_THUB )
    PP_DEBUG_CRITICAL_COND_THROW( IFS_MSG%IFREQ .LT. 1_JPIB_K, ERRFLAG_OUT_OF_BOUNDS_FRLB )
    PP_DEBUG_CRITICAL_COND_THROW( IFS_MSG%IFREQ .GT. SIZE(IFS_PAR%WAM_%FR), ERRFLAG_OUT_OF_BOUNDS_FRUB )

    ! Set the direction of the frequency
    MSG%DIRECTION = IFS_MSG%IANGLE
    MSG%FREQUENCY = IFS_MSG%IFREQ
    PAR%WAVE%TO_BE_DEALLOCATED = .FALSE.
    PAR%WAVE%DIRS_ => IFS_PAR%WAM_%TH
    PAR%WAVE%FREQ_ => IFS_PAR%WAM_%FR

  ELSE

    MSG%LEVTYPE  = LEVTYPE_SFC_E

    ! TODO Not sure if levelist has to be set here (for surfaces is always 0)
    MSG%LEVELIST = 0_JPIB_K

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

    ! Local debug variables
    INTEGER(KIND=JPIB_K) :: DUMMYSTAT
    CHARACTER(LEN=32) :: GOT
    CHARACTER(LEN=32) :: EXPECTED

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE(ERRFLAG_IS_SPECTRA)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error in checking for wave spectra' )
    CASE(ERRFLAG_NOT_TH_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Direction not allocated' )
    CASE(ERRFLAG_NOT_FR_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Frequency not allocated' )
    CASE(ERRFLAG_OUT_OF_BOUNDS_FRLB)
      GOT=REPEAT(' ',32)
      WRITE(GOT, '(I32)', IOSTAT=DUMMYSTAT) IFS_MSG%IFREQ
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Frequency lower bound out of bounds' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Got: '//TRIM(ADJUSTL(GOT)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Expected: bigger than 0' )
    CASE(ERRFLAG_OUT_OF_BOUNDS_FRUB)
      GOT=REPEAT(' ',32)
      EXPECTED=REPEAT(' ',32)
      WRITE(GOT, '(I32)', IOSTAT=DUMMYSTAT) IFS_MSG%IFREQ
      WRITE(EXPECTED, '(I32)', IOSTAT=DUMMYSTAT) SIZE(IFS_PAR%WAM_%FR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Frequency upper bound out of bounds' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Expected: lower or equal to -> '//TRIM(ADJUSTL(EXPECTED)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Got: '//TRIM(ADJUSTL(GOT)) )
    CASE(ERRFLAG_OUT_OF_BOUNDS_THLB)
      GOT=REPEAT(' ',32)
      WRITE(GOT, '(I32)', IOSTAT=DUMMYSTAT) IFS_MSG%IANGLE
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Direction lower bound out of bounds' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Got: '//TRIM(ADJUSTL(GOT)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Expected: bigger than 0' )
    CASE(ERRFLAG_OUT_OF_BOUNDS_THUB)
      GOT=REPEAT(' ',32)
      EXPECTED=REPEAT(' ',32)
      WRITE(GOT, '(I32)', IOSTAT=DUMMYSTAT) IFS_MSG%IANGLE
      WRITE(EXPECTED, '(I32)', IOSTAT=DUMMYSTAT) SIZE(IFS_PAR%WAM_%TH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Direction upper bound out of bounds' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Expected: lower or equal to -> '//TRIM(ADJUSTL(EXPECTED)) )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Got: '//TRIM(ADJUSTL(GOT)) )
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

END FUNCTION WAM2MARS_SET_DIRFREQ
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE IFS2MARS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
