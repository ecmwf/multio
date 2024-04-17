#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'mars_preset_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MARS_PRESET_UTILS_MOD'
MODULE MARS_PRESET_UTILS_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: MARS_PRESET_INIT
PUBLIC :: MARS_PRESET_FREE
PUBLIC :: MARS_PRESET_GRIB2_WAM_I
PUBLIC :: MARS_PRESET_GRIB2_WAM_S
PUBLIC :: MARS_LSSI_PRESET_GRIBX_ATM
PUBLIC :: MARS_PRESET_GRIBX_ATM

CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MARS_PRESET_INIT'
SUBROUTINE MARS_PRESET_INIT( CFG, MODEL_PARAMS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,  ONLY: MODEL_PAR_T

  ! Symbols imported from other libraries
  USE :: FCKIT_CONFIGURATION_MODULE, ONLY: FCKIT_CONFIGURATION

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(FCKIT_CONFIGURATION), INTENT(IN) :: CFG
  TYPE(MODEL_PAR_T), TARGET, INTENT(IN) :: MODEL_PARAMS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MARS_PRESET_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MARS_PRESET_FREE'
SUBROUTINE MARS_PRESET_FREE( )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE MARS_PRESET_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MARS_PRESET_GRIB2_WAM_I'
FUNCTION MARS_PRESET_GRIB2_WAM_I( MODEL_PARAMS, METADATA ) RESULT(CDTYPE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: OM_CORE_MOD,       ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,       ONLY: MODEL_PAR_T
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Function Result
  CHARACTER(LEN=16) :: CDTYPE

  ! Local variables
  CHARACTER(LEN=96)     :: CLWORD
  INTEGER(KIND=JPIB_K)  :: ICENTRE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA,  'discipline', 10 )

  ! Set the default parameters
  PP_METADATA_SET( METADATA,  'setLocalDefinition', 1 )
  PP_METADATA_SET( METADATA,  'localDefinitionNumber', MODEL_PARAMS%WAM_%NLOCGRB )
  PP_METADATA_SET( METADATA,  'stream', MODEL_PARAMS%WAM_%ISTREAM )
  PP_METADATA_SET( METADATA,  'type', TRIM(MODEL_PARAMS%SIM_%CTYPE) )
  CDTYPE = TRIM(MODEL_PARAMS%SIM_%CTYPE)
  PP_METADATA_SET( METADATA,  'class', TRIM(MODEL_PARAMS%SIM_%CFCLASS))
  PP_METADATA_SET( METADATA,  'experimentVersionNumber', MODEL_PARAMS%SIM_%CNMEXP(1:4))

  ! Handle special local definitions
  CALL MARS_PRESET_LOCAL_SECTION( MODEL_PARAMS, METADATA )

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION MARS_PRESET_GRIB2_WAM_I
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MARS_PRESET_GRIB2_WAM_S'
FUNCTION MARS_PRESET_GRIB2_WAM_S( MODEL_PARAMS, METADATA ) RESULT(CLTYPE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: OM_CORE_MOD,       ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,       ONLY: MODEL_PAR_T
  USE :: METADATA_BASE_MOD, ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Function Result
  CHARACTER(LEN=16) :: CLTYPE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Enable local section
  PP_METADATA_SET( METADATA,  'setLocalDefinition', 1)

  ! Specify the type of local definition to be used
  PP_METADATA_SET( METADATA,  'localDefinitionNumber', 1)

  ! Experiment version
  PP_METADATA_SET( METADATA,  'experimentVersionNumber', MODEL_PARAMS%WAM_%YEXPVER )

  ! Class
  PP_METADATA_SET( METADATA,  'class', MODEL_PARAMS%WAM_%YCLASS )

  ! Type 2 = Analysis?????
  CLTYPE = MODEL_PARAMS%WAM_%MARSTYPE
  PP_METADATA_SET( METADATA,  'type', TRIM(CLTYPE) )

  ! Stream
  PP_DEBUG_CRITICAL_COND_THROW( MODEL_PARAMS%WAM_%ISTREAM.LE.0, 1 )
  PP_METADATA_SET( METADATA,  'stream', MODEL_PARAMS%WAM_%ISTREAM )

  ! Handle the ensamble case
  IF ( MODEL_PARAMS%WAM_%NTOTENS .GT. 0 ) THEN

    ! Ensamble forecast number
    PP_METADATA_SET( METADATA,  'perturbationNumber', MODEL_PARAMS%WAM_%NENSFNB )

    ! Total ensamble forecast number
    PP_METADATA_SET( METADATA,  'numberOfForecastsInEnsemble', MODEL_PARAMS%WAM_%NTOTENS )
  ENDIF

  ! 1082	wasf	Wave seasonal forecast
  ! 1095	wamf	Wave monthly forecast
  ! 1203	mnfw	Wave real-time
  ! 1204	mfhw	Monthly forecast hindcasts wave
  IF ( (MODEL_PARAMS%WAM_%ISTREAM .EQ. 1082) .OR.  &
&      (MODEL_PARAMS%WAM_%ISTREAM .EQ. 1095) .OR.  &
&      (MODEL_PARAMS%WAM_%ISTREAM .EQ. 1203) .OR.  &
&      (MODEL_PARAMS%WAM_%ISTREAM .EQ. 1204) ) THEN
    PP_METADATA_SET( METADATA,  'grib2LocalSectionNumber', 30 )
    ! SYSTEM NUMBER
    PP_METADATA_SET( METADATA,  'systemNumber', MODEL_PARAMS%WAM_%NSYSNB )
    ! METHOD NUMBER
    PP_METADATA_SET( METADATA,  'methodNumber', MODEL_PARAMS%WAM_%NMETNB )
  ENDIF

  ! 1204	mfhw	Monthly forecast hindcasts wave
  ! 1078	ewho	Ensemble forecast wave hindcast overlap
  ! 1079	enwh	Ensemble forecast wave hindcasts
  ! 1084	ewhc	Wave ensemble forecast hindcast (obsolete)
  ! 1085	wvhc	Wave hindcast
  IF ( (MODEL_PARAMS%WAM_%ISTREAM .EQ. 1204) .OR.  &
&      (MODEL_PARAMS%WAM_%ISTREAM .EQ. 1078) .OR.  &
&      (MODEL_PARAMS%WAM_%ISTREAM .EQ. 1079) .OR.  &
&      (MODEL_PARAMS%WAM_%ISTREAM .EQ. 1084) .OR.  &
&      (MODEL_PARAMS%WAM_%ISTREAM .EQ. 1085) ) THEN

    PP_METADATA_SET( METADATA,  'grib2LocalSectionNumber', 30 )
    PP_METADATA_SET( METADATA,  'referenceDate', MODEL_PARAMS%WAM_%IREFDATE )
    PP_METADATA_SET( METADATA,  'climateDateFrom', 0 )
    PP_METADATA_SET( METADATA,  'climateDateTo', 0 )
    PP_METADATA_SET( METADATA,  'legBaseDate', 0 )
    PP_METADATA_SET( METADATA,  'legBaseTime', 0 )
    PP_METADATA_SET( METADATA,  'legNumber', 0 )
    PP_METADATA_SET( METADATA,  'oceanAtmosphereCoupling', 0 )

  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Stream must be greater than 0' )
    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )
    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END FUNCTION MARS_PRESET_GRIB2_WAM_S
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Sets MARS metadata (section 2 in GRIB2) for simulated satellite images.
!>
!> This function sets the MARS metadata of a simulation into the provided metadata object
!> using information retrieved from the specified data structure (YDIOS).
!>
!> @param [inout] METADATA Metadata object where the MARS metadata will be set.
!> @param [in]    YDIOS    Data structure used to retrieve the information necessary for setting the metadata.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MARS_LSSI_PRESET_GRIBX_ATM'
FUNCTION MARS_LSSI_PRESET_GRIBX_ATM( MODEL_PARAMS, METADATA ) RESULT(CDTYPE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A
  USE :: DATETIME_UTILS_MOD, ONLY: DATE_SUB_DAYS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Function Result
  CHARACTER(LEN=16) :: CDTYPE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ASSOCIATE ( YPI                           => MODEL_PARAMS%SIM_, &
&             ECMWF_LOCAL_DEFINITION_NUMBER => MODEL_PARAMS%SIM_%NLOCGRB )

  SELECT CASE (ECMWF_LOCAL_DEFINITION_NUMBER)

  CASE (1)
    ! 1  -> MARS labeling or ensemble forecast data
    ! 24 -> Satellite image simulation
    ! ---------------------------------------------
    ! Set the default parameters
    PP_METADATA_SET( METADATA,  'localDefinitionNumber', 24 )
    PP_METADATA_SET( METADATA,  'stream',                YPI%NSTREAM )
    PP_METADATA_SET( METADATA,  'class',                 YPI%CFCLASS )


  CASE (36)
    ! 36  -> MARS labelling for long window 4Dvar system
    ! 192 -> Multiple ECMWF local definitions
    ! --------------------------------------------------
    PP_METADATA_SET( METADATA,  'localDefinitionNumber',     192 )
    PP_METADATA_SET( METADATA,  'stream',                    YPI%NSTREAM )
    PP_METADATA_SET( METADATA,  'class',                     YPI%CFCLASS )
    PP_METADATA_SET( METADATA,  'subLocalDefinitionNumber1', 24 )
    PP_METADATA_SET( METADATA,  'subLocalDefinitionNumber2', 36 )
    PP_METADATA_SET( METADATA,  'anoffset',                  YPI%NWINOFF )

  CASE DEFAULT

    ! Here I am assuming I covered all the cases. If this is not the case
    ! then need to raise an error
    PP_DEBUG_CRITICAL_THROW( 1 )

  END SELECT


  ! Section 2 common configuration for Simulated Satellite data
  PP_METADATA_SET( METADATA,  'type', 'ssd' ) ! Simulated satellite data
  CDTYPE = REPEAT(' ',16)
  CDTYPE = 'ssd'
  PP_METADATA_SET( METADATA,  'productDefinitionTemplateNumber',   32 )
  PP_METADATA_SET( METADATA,  'numberOfContributingSpectralBands', 1 )
  PP_METADATA_SET( METADATA,  'scaleFactorOfCentralWaveNumber',    0 )

  END ASSOCIATE

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled case in switch' )

    CASE DEFAULT
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unhandled error' )

    END SELECT

    ! Trace end of procedure (on error)
    PP_TRACE_EXIT_PROCEDURE_ON_ERROR()

    ! Write the error message and stop the program
    PP_DEBUG_ABORT( STR )

  END BLOCK ErrorHandler

  ! Exit point on error
  RETURN

END FUNCTION MARS_LSSI_PRESET_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!> @brief Sets MARS metadata (section 2 in GRIB2) for generic fields.
!>
!> This function sets the MARS metadata of a simulation into the provided metadata object
!> using information retrieved from the specified data structure (YDIOS).
!>
!> @param [inout] METADATA Metadata object where the MARS metadata will be set.
!> @param [in]    YDIOS    Data structure used to retrieve the information necessary for setting the metadata.
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MARS_PRESET_GRIBX_ATM'
FUNCTION MARS_PRESET_GRIBX_ATM( MODEL_PARAMS, METADATA ) RESULT(CDTYPE)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A
  USE :: DATETIME_UTILS_MOD, ONLY: SEC2HHMM

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Function Result
  CHARACTER(LEN=16) :: CDTYPE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Set the default parameters
  PP_METADATA_SET( METADATA,  'setLocalDefinition', 1 )
  PP_METADATA_SET( METADATA,  'localDefinitionNumber',MODEL_PARAMS%SIM_%NLOCGRB )
  PP_METADATA_SET( METADATA,  'stream',MODEL_PARAMS%SIM_%NSTREAM)
  PP_METADATA_SET( METADATA,  'type',MODEL_PARAMS%SIM_%CTYPE)
  CDTYPE = MODEL_PARAMS%SIM_%CTYPE
  PP_METADATA_SET( METADATA,  'class', MODEL_PARAMS%SIM_%CFCLASS)
  PP_METADATA_SET( METADATA,  'experimentVersionNumber', MODEL_PARAMS%SIM_%CNMEXP(1:4))

  ! Handle special local definitions
  CALL MARS_PRESET_LOCAL_SECTION( MODEL_PARAMS, METADATA )

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION MARS_PRESET_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




!> @brief Sets MARS metadata (section 2 in GRIB2) for generic fields.
!>
!> This function sets the MARS metadata of a simulation into the provided metadata object
!> using information retrieved from the specified data structure (YDIOS).
!>
!> @param [inout] METADATA Metadata object where the MARS metadata will be set.
!> @param [in]    YDIOS    Data structure used to retrieve the information necessary for setting the metadata.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'MARS_PRESET_LOCAL_SECTION'
SUBROUTINE MARS_PRESET_LOCAL_SECTION( MODEL_PARAMS, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A
  USE :: DATETIME_UTILS_MOD, ONLY: SEC2HHMM

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Function Result
  CHARACTER(LEN=16) :: CDTYPE

  ! Local variables
  LOGICAL, DIMENSION(5) :: CONDITIONS
  CHARACTER(LEN=96)     :: CLWORD
  INTEGER(KIND=JPIB_K)  :: ICENTRE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Initialization
  CLWORD = REPEAT(' ', 96)
  ICENTRE = 0
  CONDITIONS = .FALSE.
  CDTYPE = REPEAT(' ',16)

  ASSOCIATE ( YPI                           => MODEL_PARAMS%SIM_, &
&             ECMWF_LOCAL_DEFINITION_NUMBER => MODEL_PARAMS%SIM_%NLOCGRB )

    ! Sepcial cases handling
    CONDITIONS(1) = (YPI%CTYPE .EQ. 'cf')    ! Control forecast (gribCode=10)
    CONDITIONS(2) = (YPI%CTYPE .EQ. 'pf')    ! Perturbed forecast (gribCode=11)
    CONDITIONS(3) = (YPI%CTYPE .EQ. 'cv')    ! Calibration/Validation forecast (gribCode=65)
    CONDITIONS(4) = (YPI%NSTREAM .EQ. 1030)  ! 'enda' -> Ensemble data assimilation
    CONDITIONS(5) = (YPI%NSTREAM .EQ. 1249)  ! 'elda' -> Ensemble Long window Data Assimilation


    ! Handling the special cases related to the 'type' and 'stream'
    IF( ANY(CONDITIONS) ) THEN
      PP_METADATA_SET( METADATA,  'eps',1)
      PP_METADATA_SET( METADATA,  'perturbationNumber',YPI%NENSFNB)
      PP_METADATA_SET( METADATA,  'numberOfForecastsInEnsemble',YPI%NTOTENS)
    ELSE
      PP_METADATA_SET( METADATA,  'eps',0)
    ENDIF

    ! Handling the special cases related to the local definition number
    ! https://codes.ecmwf.int/grib/format/grib1/local/
    SELECT CASE( ECMWF_LOCAL_DEFINITION_NUMBER )

    CASE ( 7 )

      ! 7  -> Sensitivity data
      ! -----------------------
      ! NCONF is a control variable for the job and defines job configuration.
      ! When 800 <= NCONF < 900, the job is a sensitivity analysis.
      IF ( ABS(YPI%NCONF)/100 .EQ. 8 ) THEN
        ! sg -> Sensitivity gradient (gribCode=50)
        PP_METADATA_SET( METADATA,  'type','sg')
        CDTYPE = REPEAT(' ',16)
        CDTYPE = 'sg'
      ELSE
        ! sf -> Sensitivity forecast (gribCode=52)
        PP_METADATA_SET( METADATA,  'type','sf')
        CDTYPE = REPEAT(' ',16)
        CDTYPE = 'sf'
      ENDIF
      PP_METADATA_SET( METADATA,  'iterationNumber',YPI%NJITER)
      PP_METADATA_SET( METADATA,  'sensitiveAreaDomain',YPI%NJDOMAI)
      PP_METADATA_SET( METADATA,  'diagnosticNumber',YPI%NJDIAG)

    CASE ( 9 )

      ! 9  -> Singular vectors and ensemble perturbations
      ! --------------------------------------------------
      ! sv -> Singular vector (gribCode=62)
      PP_METADATA_SET( METADATA,  'type','sv')
      CDTYPE = REPEAT(' ',16)
      CDTYPE = 'sv'
      ! TODO: Comment copied from the original code
      ! NB The rest set in SULCZ or NALAN1 ????????

    CASE ( 15 )

      ! 15 -> Seasonal forecast data
      ! -----------------------------
      PP_METADATA_SET( METADATA,  'eps',1)
      PP_METADATA_SET( METADATA,  'perturbationNumber',YPI%NENSFNB)
      PP_METADATA_SET( METADATA,  'numberOfForecastsInEnsemble',YPI%NTOTENS)
      PP_METADATA_SET( METADATA,  'systemNumber',YPI%NSYSTEM)
      PP_METADATA_SET( METADATA,  'methodNumber',YPI%NMETHOD)

    CASE ( 18 )

      ! Multi-analysis ensemble data
      ! ----------------------------
      PP_METADATA_SET( METADATA,  'modelIdentifier','ECMF')
      IF(YPI%NCONSENSUS == 0) THEN
        !   Data from one centre
        IF(YPI%NDWD == 1) THEN
          PP_METADATA_SET( METADATA,  'dataOrigin',78)
        ELSEIF(YPI%NMFR == 1) THEN
          PP_METADATA_SET( METADATA,  'dataOrigin',85)
        ELSEIF(YPI%NNCEP == 1) THEN
          PP_METADATA_SET( METADATA,  'dataOrigin',7)
        ELSEIF(YPI%NUKM == 1) THEN
          PP_METADATA_SET( METADATA,  'dataOrigin',74)
        ENDIF
      ELSE
        ! Consensus analysis (always includes EC)
        PP_METADATA_SET( METADATA,  'dataOrigin',255)
        ICENTRE=1
        CLWORD='ECMF'
        IF(YPI%NDWD == 1) THEN
          ICENTRE=ICENTRE+1
          CLWORD=CLWORD//'EDZW'
        ENDIF
        IF(YPI%NMFR == 1) THEN
          ICENTRE=ICENTRE+1
          CLWORD=CLWORD//'LFPW'
        ENDIF
        IF(YPI%NNCEP == 1) THEN
          ICENTRE=ICENTRE+1
          CLWORD=CLWORD//'KWBC'
        ENDIF
        IF(YPI%NUKM == 1) THEN
          ICENTRE=ICENTRE+1
          CLWORD=CLWORD//'EGRR'
        ENDIF
        PP_METADATA_SET( METADATA,  'consensusCount',ICENTRE)
        PP_METADATA_SET( METADATA,  'ccccIdentifiers',CLWORD(1:4*ICENTRE))
      ENDIF

    CASE ( 23 )

      ! 23 -> Coupled atmospheric, wave and ocean means (with hindcast support)
      ! ------------------------------------------------------------------------
      PP_METADATA_SET( METADATA,  'systemNumber', YPI%NSYSTEM)
      PP_METADATA_SET( METADATA,  'methodNumber', YPI%NMETHOD)
      PP_METADATA_SET( METADATA,  'referenceDate',YPI%NREFERENCE)

    CASE ( 26 )

      ! 26 -> MARS labelling or ensemble forecast data (with hindcast support)
      ! -----------------------------------------------------------------------
      PP_METADATA_SET( METADATA,  'referenceDate', YPI%NREFERENCE)

    CASE ( 27, 30 )

      ! 27 -> Forecasting Systems with Variable Resolution (Obsolete)
      ! 30 -> Forecasting Systems with Variable Resolution
      ! --------------------------------------------------------------

      ! Date and time of current leg for VAREPS
      PP_METADATA_SET( METADATA,  'legBaseDate',   YPI%NINDAT)
      PP_METADATA_SET( METADATA,  'legBaseTime',   SEC2HHMM(YPI%NSSSSS) )
      PP_METADATA_SET( METADATA,  'legNumber',     YPI%NLEG)
      PP_METADATA_SET( METADATA,  'referenceDate', YPI%NREFERENCE)
      ! Coupling parameters
      IF ( YPI%LDMCC04 ) THEN
          PP_METADATA_SET( METADATA,  'oceanAtmosphereCoupling',2)
      ENDIF

    CASE ( 36 )

      ! Long window
      ! -----------
      PP_METADATA_SET( METADATA,  'anoffset',YPI%NWINOFF)


    END SELECT

  END ASSOCIATE

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE MARS_PRESET_LOCAL_SECTION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE MARS_PRESET_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
