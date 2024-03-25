#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'time_assumptions_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'TIME_ASSUMPTIONS_MOD'
MODULE TIME_ASSUMPTIONS_MOD
IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: COMPUTE_TIMING_ZERO
PUBLIC :: COMPUTE_TIMING
PUBLIC :: TYPE_OF_STATISTICAL_PROCESS_TO_STRING
PUBLIC :: TYPE_OF_TIME_RANGE_TO_STRING
PUBLIC :: TIME_ASSUMPTIONS_INIT
PUBLIC :: TIME_ASSUMPTIONS_FREE
CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TIME_ASSUMPTIONS_INIT'
SUBROUTINE TIME_ASSUMPTIONS_INIT( CFG, MODEL_PARAMS )

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

END SUBROUTINE TIME_ASSUMPTIONS_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TIME_ASSUMPTIONS_FREE'
SUBROUTINE TIME_ASSUMPTIONS_FREE( )

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

END SUBROUTINE TIME_ASSUMPTIONS_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



FUNCTION TYPE_OF_STATISTICAL_PROCESS_TO_STRING( TYPE_OF_STATISTICAL_PROCESS ) RESULT(S)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MIN_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MAX_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MODE_E

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: TYPE_OF_STATISTICAL_PROCESS

  ! Function result
  CHARACTER(LEN=16) :: S

  S = REPEAT(' ',16)
  SELECT CASE(TYPE_OF_STATISTICAL_PROCESS)
  CASE (TYPE_OF_STATISTICAL_PROCESS_INSTANT_E)
    S = 'INSTANT'
  CASE (TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E)
    S = 'AVERAGE'
  CASE (TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E)
    S = 'ACCUMULATION'
  CASE (TYPE_OF_STATISTICAL_PROCESS_MIN_E)
    S = 'MIN'
  CASE (TYPE_OF_STATISTICAL_PROCESS_MAX_E)
    S = 'MAX'
  CASE (TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E)
    S = 'SEVERITY'
  CASE (TYPE_OF_STATISTICAL_PROCESS_MODE_E)
    S = 'MODE'
  CASE DEFAULT
    S = 'UNKNOWN'
  END SELECT

  ! Exit point
  RETURN

END FUNCTION TYPE_OF_STATISTICAL_PROCESS_TO_STRING

FUNCTION TYPE_OF_TIME_RANGE_TO_STRING( TYPE_OF_TIME_RANGE ) RESULT(S)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_STEP0_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_LASTPP_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FIXED_SIZE_E

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: TYPE_OF_TIME_RANGE

  ! Function result
  CHARACTER(LEN=16) :: S

  S = REPEAT(' ',16)
  SELECT CASE(TYPE_OF_TIME_RANGE)
  CASE (TYPE_OF_TIME_RANGE_INSTANT_E)
    S = 'SINGLE_POINT'
  CASE (TYPE_OF_TIME_RANGE_FROM_STEP0_E)
    S = 'FROM_STEP_0'
  CASE (TYPE_OF_TIME_RANGE_FROM_LASTPP_E)
    S = 'FROM_LAST_PP'
  CASE (TYPE_OF_TIME_RANGE_FIXED_SIZE_E)
    S = 'FIXED_SIZE'
  CASE DEFAULT
    S = 'UNKNOWN'
  END SELECT

  ! Exit point
  RETURN

END FUNCTION TYPE_OF_TIME_RANGE_TO_STRING


! Some rules I used to be able to use a single data structure:
! TYPE_OF_STATISTICAL_PROCESS     = -1  is used to represent an Instant field (with LENGTH_OF_TIMERANGE=0)
! TYPE_OF_TIME_RANGE              =  0 Instant
! TYPE_OF_TIME_RANGE              =  1 from 0 to current time
! TYPE_OF_TIME_RANGE              =  2 from previous post-processing step to current postprocessing step
! TYPE_OF_TIME_RANGE              =  3 predefined length (defined by length of timerange)
! LENGTH_OF_TIME_RANGE_IN_SECONDS = -1  is used to represent a statistics that starts from the beginning of the simulation
! LENGTH_OF_TIME_RANGE_IN_SECONDS = -2  is used to represent a statistics that starts from the last postprocessing step
! Every value set to -99 means that it has been not set => error
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'COMPUTE_TIMING_ZERO'
SUBROUTINE COMPUTE_TIMING_ZERO( YDMODEL_PARAMETERS, KDPARAM_ID, &
& KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER, KDTYPE_OF_STATISTICAL_PROCESSING, &
& KDTYPE_OF_TIMERANGE, KDLENGTH_OF_TIMERANGE_IN_SECONDS, LDVALID )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MIN_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MAX_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MODE_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_STEP0_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_LASTPP_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FIXED_SIZE_E
  USE :: GENERAL_ASSUMPTIONS_MOD, ONLY: IS_ENSAMBLE_SIMULATION

  ! Grib codes
  USE :: GRIB_CODES_MOD, ONLY: NGRBLSP
  USE :: GRIB_CODES_MOD, ONLY: NGRBSF
  USE :: GRIB_CODES_MOD, ONLY: NGRBSSRD
  USE :: GRIB_CODES_MOD, ONLY: NGRBSTRD
  USE :: GRIB_CODES_MOD, ONLY: NGRBSSR
  USE :: GRIB_CODES_MOD, ONLY: NGRBSTR
  USE :: GRIB_CODES_MOD, ONLY: NGRBTTR
  USE :: GRIB_CODES_MOD, ONLY: NGRBRO
  USE :: GRIB_CODES_MOD, ONLY: NGRBTP
  USE :: GRIB_CODES_MOD, ONLY: NGRBFZRA
  USE :: GRIB_CODES_MOD, ONLY: NGRBTSR
  USE :: GRIB_CODES_MOD, ONLY: NGRBDSRP
  USE :: GRIB_CODES_MOD, ONLY: NGRBLSPF
  USE :: GRIB_CODES_MOD, ONLY: NGRBUVB
  USE :: GRIB_CODES_MOD, ONLY: NGRBPAR
  USE :: GRIB_CODES_MOD, ONLY: NGRBBLD
  USE :: GRIB_CODES_MOD, ONLY: NGRBSLHF
  USE :: GRIB_CODES_MOD, ONLY: NGRBNSSS
  USE :: GRIB_CODES_MOD, ONLY: NGRBEWSS
  USE :: GRIB_CODES_MOD, ONLY: NGRBSUND
  USE :: GRIB_CODES_MOD, ONLY: NGRBMGWS
  USE :: GRIB_CODES_MOD, ONLY: NGRBGWD
  USE :: GRIB_CODES_MOD, ONLY: NGRBTSRC
  USE :: GRIB_CODES_MOD, ONLY: NGRBSSRC
  USE :: GRIB_CODES_MOD, ONLY: NGRBOCLZ
  USE :: GRIB_CODES_MOD, ONLY: NGRBE
  USE :: GRIB_CODES_MOD, ONLY: NGRBSMLT
  USE :: GRIB_CODES_MOD, ONLY: NGRBSRO
  USE :: GRIB_CODES_MOD, ONLY: NGRBSSRO
  USE :: GRIB_CODES_MOD, ONLY: NGRB10FG
  USE :: GRIB_CODES_MOD, ONLY: NGRBMX2T
  USE :: GRIB_CODES_MOD, ONLY: NGRBLITOTA6
  USE :: GRIB_CODES_MOD, ONLY: NGRBMX2T3
  USE :: GRIB_CODES_MOD, ONLY: NGRBMN2T3
  USE :: GRIB_CODES_MOD, ONLY: NGRB10FG3
  USE :: GRIB_CODES_MOD, ONLY: NGRBMXTPR3
  USE :: GRIB_CODES_MOD, ONLY: NGRBMNTPR3
  USE :: GRIB_CODES_MOD, ONLY: NGRBMX2T6
  USE :: GRIB_CODES_MOD, ONLY: NGRBMN2T6
  USE :: GRIB_CODES_MOD, ONLY: NGRB10FG6
  USE :: GRIB_CODES_MOD, ONLY: NGRBMXTPR6
  USE :: GRIB_CODES_MOD, ONLY: NGRBMNTPR6
  USE :: GRIB_CODES_MOD, ONLY: NGRBLITOTA1
  USE :: GRIB_CODES_MOD, ONLY: NGRBLICGA1
  USE :: GRIB_CODES_MOD, ONLY: NGRBLITOTA3
  USE :: GRIB_CODES_MOD, ONLY: NGRBLICGA3
  USE :: GRIB_CODES_MOD, ONLY: NGRBLITOTA6
  USE :: GRIB_CODES_MOD, ONLY: NGRBLICGA6
  USE :: GRIB_CODES_MOD, ONLY: NGRBMXCAP6
  USE :: GRIB_CODES_MOD, ONLY: NGRBMXCAPS6
  USE :: GRIB_CODES_MOD, ONLY: NGRBPTYPEMODE1
  USE :: GRIB_CODES_MOD, ONLY: NGRBPTYPEMODE3
  USE :: GRIB_CODES_MOD, ONLY: NGRBPTYPESEVR1
  USE :: GRIB_CODES_MOD, ONLY: NGRBPTYPEMODE6
  USE :: GRIB_CODES_MOD, ONLY: NGRBPTYPESEVR3
  USE :: GRIB_CODES_MOD, ONLY: NGRBPTYPESEVR6

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),    INTENT(IN)  :: YDMODEL_PARAMETERS
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: KDPARAM_ID
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: KDTYPE_OF_STATISTICAL_PROCESSING
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: KDTYPE_OF_TIMERANGE
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: KDLENGTH_OF_TIMERANGE_IN_SECONDS
  LOGICAL,              INTENT(OUT) :: LDVALID

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Depending on parmId set the "Time Characteristics" for each field
  SELECT CASE (KDPARAM_ID)

  ! NGRBMX2T3  - 228026 - Maximum temperature at 2 m since last 3 hours
  ! NGRBMN2T3  - 228027 - Minimum temperature at 2 m since last 3 hours
  ! NGRB10FG3  - 228028 - Wind gust at 10 metres since last 3 hours
  ! NGRBMXTPR3 - 228222 - Max precip rate in last 3 hours
  ! NGRBMNTPR3 - 228223 - Min precip rate in last 3 hours
  CASE(NGRBMX2T3, NGRBMN2T3, NGRB10FG3, NGRBMXTPR3, NGRBMNTPR3)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ! Instant
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_INSTANT_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 0
    LDVALID = .FALSE.


  ! NGRBMX2T6  - 121    - Maximum temperature at 2 m since last 6 hours
  ! NGRBMN2T6  - 122    - Minimum temperature at 2 m since last 6 hours
  ! NGRB10FG6  - 123    - Wind gust at 10 metres since last 6 hours
  ! NGRBMXTPR6 - 228224 - Max precip rate in last 6 hours
  ! NGRBMNTPR6 - 228225 - Min precip rate in last 6 hours
  CASE(NGRBMX2T6, NGRBMN2T6, NGRB10FG6, NGRBMXTPR6, NGRBMNTPR6)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ! Instant
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_INSTANT_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 0
    LDVALID = .FALSE.


  ! NGRBLITOTA1 - 228051 - 1h averaged total lightning flash density
  ! NGRBLICGA1  - 228053 - 1h averaged cloud-to-ground lightning flash density
  CASE(NGRBLITOTA1, NGRBLICGA1)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ! Instant
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_INSTANT_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 0
    LDVALID = .FALSE.


  ! NGRBLITOTA3 - 228057 - 3h averaged total lightning flash density
  ! NGRBLICGA3  - 228059 - 3h averaged cloud-to-ground lightning flash density
  CASE(NGRBLITOTA3, NGRBLICGA3)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ! Instant
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_INSTANT_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 0
    LDVALID = .FALSE.


  ! NGRBLITOTA6 - 228058 - 6h averaged total lightning flash density
  ! NGRBLICGA6  - 228060 - 6h averaged cloud-to-ground lightning flash density
  CASE(NGRBLITOTA6, NGRBLICGA6)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ! Instant
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_INSTANT_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 0
    LDVALID = .FALSE.


  ! NGRBMXCAP6  - 228035 - maximum CAPE in the last 6 hours
  ! NGRBMXCAPS6 - 228036 - maximum CAPES in the last 6 hours
  CASE(NGRBMXCAP6, NGRBMXCAPS6)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ! Instant
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_INSTANT_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 0
    LDVALID = .FALSE.


  ! NGRBPTYPEMODE1 - 260320 - Precipitation type (most frequent) in the last 1 hour
  CASE(NGRBPTYPEMODE1)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ! Instant
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_INSTANT_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 0
    LDVALID = .FALSE.


  ! NGRBPTYPEMODE3 - 260321 - Precipitation type (most frequent) in the last 3 hours
  CASE(NGRBPTYPEMODE3)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ! Instant
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_INSTANT_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 0
    LDVALID = .FALSE.


  ! NGRBPTYPEMODE6 - 260339 Precipitation type (most frequent) in the last 6 hours
  CASE(NGRBPTYPEMODE6)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ! Instant
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_INSTANT_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 0
    LDVALID = .FALSE.
  CASE(NGRBPTYPESEVR1)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ! Instant
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_INSTANT_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 0
    LDVALID = .FALSE.


  ! NGRBPTYPESEVR1 - 260318 Precipitation type (most severe) in the last 1 hour
  CASE(NGRBPTYPESEVR3)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ! Instant
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_INSTANT_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 0
    LDVALID = .FALSE.


  ! NGRBPTYPESEVR3 - 260319 Precipitation type (most severe) in the last 3 hours
  CASE(NGRBPTYPESEVR6)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ! Instant
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_INSTANT_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 0
    LDVALID = .FALSE.


  ! NGRBLSP  - 142 Large scale precipitation
  ! NGRBSF   - 144 Snow fall
  ! NGRBSSRD - 169 Surface solar radiation downwards
  ! NGRBSTRD - 175 Surface thermal radiation downwards
  ! NGRBSSR  - 176 Surface solar radiation
  ! NGRBSTR  - 177 Surface thermal radiation
  ! NGRBTTR  - 179 Top thermal radiation
  ! NGRBRO   - 205 Runoff
  ! NGRBTP   - 228 Total precipitation
  ! NGRBFZRA - 228216 Freezing rain accumulation
  ! NGRBTSR  - 178 Top solar radiation
  ! NGRBDSRP -  47 Direct solar radiation incident on a plane perpendicular to the Sun's direction
  ! NGRBLSPF -  50 large scale precipitation fraction
  ! NGRBUVB  -  57 surface UV-B radiation
  ! NGRBPAR  -  58 surface PARadiation
  ! NGRBBLD  - 145 Boundary layer dissipation
  ! NGRBSLHF - 147 Surface latent heat flux
  ! NGRBNSSS - 181 V-stress
  ! NGRBEWSS - 180 U-stress
  ! NGRBSUND - 189 Sunshine duration
  ! NGRBMGWS - 196 Meridional component of gravity wave stress
  ! NGRBGWD  - 197 Gravity wave dissipation
  ! NGRBTSRC - 208 Top solar radiation clear sky
  ! NGRBSSRC - 210 Surface solar radiation clear sky
  ! NGRBOCLZ - 213 layer depth
  ! NGRBE    - 182 Evaporation
  ! NGRBSMLT -  45 Snow melt
  ! NGRBSRO   -  8 Surface Runoff
  ! NGRBSSRO  -  9 Sub-Surface Runoff
  CASE(NGRBLSP,NGRBSF,NGRBSSRD,NGRBSTRD,NGRBSSR,NGRBSTR,NGRBTTR,NGRBRO,NGRBTP,NGRBFZRA, &
       & NGRBTSR,NGRBDSRP,NGRBLSPF,NGRBUVB,NGRBPAR,NGRBBLD,NGRBSLHF,NGRBNSSS,NGRBEWSS, &
       & NGRBSUND,NGRBMGWS,NGRBGWD,NGRBTSRC,NGRBSSRC,NGRBOCLZ,NGRBE,NGRBSMLT,NGRBSRO,NGRBSSRO)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 11 - Individual ensemble forecast, control and perturbed,
      !      at a horizontal level or in a horizontal layer, in a
      !      continuous or non-continuous interval
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 11
    ELSE
      ! 8 - Average, accumulation, extreme values or other statistically
      !     processed values at a horizontal level or in a horizontal
      !     layer in a continuous or non-continuous time interval
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 8
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E ! Accumulation
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FROM_STEP0_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = -1
    LDVALID = .TRUE.


  ! NGRBPTYPESEVR6 - 260338 Precipitation type (most severe) in the last 6 hours
  CASE (222001:222256, 223001:223256) ! DGOV-353, has to be 'accum' also for SEC=0
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 11 - Individual ensemble forecast, control and perturbed,
      !      at a horizontal level or in a horizontal layer, in a
      !      continuous or non-continuous interval
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 11
    ELSE
      ! 8 - Average, accumulation, extreme values or other statistically
      !     processed values at a horizontal level or in a horizontal
      !     layer in a continuous or non-continuous time interval
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 8
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E ! Accumulation
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FROM_STEP0_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = -1
    LDVALID = .TRUE.

  ! NGRB10FG -  49 gust at 10 m level since previous post-processing
  ! NGRBMX2T - 201 Maximum temperature at 2m since last post-processing
  CASE(NGRB10FG,NGRBMX2T)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 11 - Individual ensemble forecast, control and perturbed,
      !      at a horizontal level or in a horizontal layer, in a
      !      continuous or non-continuous interval
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 11
    ELSE
      ! 8 - Average, accumulation, extreme values or other statistically
      !     processed values at a horizontal level or in a horizontal
      !     layer in a continuous or non-continuous time interval
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 8
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_MAX_E ! Max
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FROM_LASTPP_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = -2
    LDVALID = .FALSE.

  CASE DEFAULT
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ! Instant
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_INSTANT_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 0
    LDVALID = .TRUE.

  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE COMPUTE_TIMING_ZERO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'COMPUTE_TIMING'
SUBROUTINE COMPUTE_TIMING( YDMODEL_PARAMETERS, KDPARAM_ID, &
& KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER, KDTYPE_OF_STATISTICAL_PROCESSING, &
& KDTYPE_OF_TIMERANGE, KDLENGTH_OF_TIMERANGE_IN_SECONDS )
  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MIN_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MAX_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESS_MODE_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_STEP0_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_LASTPP_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FIXED_SIZE_E
  USE :: GENERAL_ASSUMPTIONS_MOD, ONLY: IS_ENSAMBLE_SIMULATION

  ! Grib codes
  USE :: GRIB_CODES_MOD, ONLY: NGRBLSP
  USE :: GRIB_CODES_MOD, ONLY: NGRBSF
  USE :: GRIB_CODES_MOD, ONLY: NGRBSSRD
  USE :: GRIB_CODES_MOD, ONLY: NGRBSTRD
  USE :: GRIB_CODES_MOD, ONLY: NGRBSSR
  USE :: GRIB_CODES_MOD, ONLY: NGRBSTR
  USE :: GRIB_CODES_MOD, ONLY: NGRBTTR
  USE :: GRIB_CODES_MOD, ONLY: NGRBRO
  USE :: GRIB_CODES_MOD, ONLY: NGRBTP
  USE :: GRIB_CODES_MOD, ONLY: NGRBFZRA
  USE :: GRIB_CODES_MOD, ONLY: NGRBTSR
  USE :: GRIB_CODES_MOD, ONLY: NGRBDSRP
  USE :: GRIB_CODES_MOD, ONLY: NGRBLSPF
  USE :: GRIB_CODES_MOD, ONLY: NGRBUVB
  USE :: GRIB_CODES_MOD, ONLY: NGRBPAR
  USE :: GRIB_CODES_MOD, ONLY: NGRBBLD
  USE :: GRIB_CODES_MOD, ONLY: NGRBSLHF
  USE :: GRIB_CODES_MOD, ONLY: NGRBNSSS
  USE :: GRIB_CODES_MOD, ONLY: NGRBEWSS
  USE :: GRIB_CODES_MOD, ONLY: NGRBSUND
  USE :: GRIB_CODES_MOD, ONLY: NGRBMGWS
  USE :: GRIB_CODES_MOD, ONLY: NGRBGWD
  USE :: GRIB_CODES_MOD, ONLY: NGRBTSRC
  USE :: GRIB_CODES_MOD, ONLY: NGRBSSRC
  USE :: GRIB_CODES_MOD, ONLY: NGRBOCLZ
  USE :: GRIB_CODES_MOD, ONLY: NGRBE
  USE :: GRIB_CODES_MOD, ONLY: NGRBSMLT
  USE :: GRIB_CODES_MOD, ONLY: NGRBSRO
  USE :: GRIB_CODES_MOD, ONLY: NGRBSSRO
  USE :: GRIB_CODES_MOD, ONLY: NGRB10FG
  USE :: GRIB_CODES_MOD, ONLY: NGRBMX2T
  USE :: GRIB_CODES_MOD, ONLY: NGRBLITOTA6
  USE :: GRIB_CODES_MOD, ONLY: NGRBMX2T3
  USE :: GRIB_CODES_MOD, ONLY: NGRBMN2T3
  USE :: GRIB_CODES_MOD, ONLY: NGRB10FG3
  USE :: GRIB_CODES_MOD, ONLY: NGRBMXTPR3
  USE :: GRIB_CODES_MOD, ONLY: NGRBMNTPR3
  USE :: GRIB_CODES_MOD, ONLY: NGRBMX2T6
  USE :: GRIB_CODES_MOD, ONLY: NGRBMN2T6
  USE :: GRIB_CODES_MOD, ONLY: NGRB10FG6
  USE :: GRIB_CODES_MOD, ONLY: NGRBMXTPR6
  USE :: GRIB_CODES_MOD, ONLY: NGRBMNTPR6
  USE :: GRIB_CODES_MOD, ONLY: NGRBLITOTA1
  USE :: GRIB_CODES_MOD, ONLY: NGRBLICGA1
  USE :: GRIB_CODES_MOD, ONLY: NGRBLITOTA3
  USE :: GRIB_CODES_MOD, ONLY: NGRBLICGA3
  USE :: GRIB_CODES_MOD, ONLY: NGRBLITOTA6
  USE :: GRIB_CODES_MOD, ONLY: NGRBLICGA6
  USE :: GRIB_CODES_MOD, ONLY: NGRBMXCAP6
  USE :: GRIB_CODES_MOD, ONLY: NGRBMXCAPS6
  USE :: GRIB_CODES_MOD, ONLY: NGRBPTYPEMODE1
  USE :: GRIB_CODES_MOD, ONLY: NGRBPTYPEMODE3
  USE :: GRIB_CODES_MOD, ONLY: NGRBPTYPESEVR1
  USE :: GRIB_CODES_MOD, ONLY: NGRBPTYPEMODE6
  USE :: GRIB_CODES_MOD, ONLY: NGRBPTYPESEVR3
  USE :: GRIB_CODES_MOD, ONLY: NGRBPTYPESEVR6

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),    INTENT(IN)  :: YDMODEL_PARAMETERS
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: KDPARAM_ID
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: KDTYPE_OF_STATISTICAL_PROCESSING
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: KDTYPE_OF_TIMERANGE
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: KDLENGTH_OF_TIMERANGE_IN_SECONDS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Depending on parmId set the "Time Characteristics" for each field
  SELECT CASE (KDPARAM_ID)

  ! NGRBMX2T3  - 228026 - Maximum temperature at 2 m since last 3 hours
  ! NGRBMN2T3  - 228027 - Minimum temperature at 2 m since last 3 hours
  ! NGRB10FG3  - 228028 - Wind gust at 10 metres since last 3 hours
  ! NGRBMXTPR3 - 228222 - Max precip rate in last 3 hours
  ! NGRBMNTPR3 - 228223 - Min precip rate in last 3 hours
  CASE(NGRBMX2T3, NGRBMN2T3, NGRB10FG3, NGRBMXTPR3, NGRBMNTPR3)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_MAX_E ! Max
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FIXED_SIZE_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 3*3600


  ! NGRBMX2T6  - 121    - Maximum temperature at 2 m since last 6 hours
  ! NGRBMN2T6  - 122    - Minimum temperature at 2 m since last 6 hours
  ! NGRB10FG6  - 123    - Wind gust at 10 metres since last 6 hours
  ! NGRBMXTPR6 - 228224 - Max precip rate in last 6 hours
  ! NGRBMNTPR6 - 228225 - Min precip rate in last 6 hours
  CASE(NGRBMX2T6, NGRBMN2T6, NGRB10FG6, NGRBMXTPR6, NGRBMNTPR6)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_MAX_E ! Max
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FIXED_SIZE_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 6*3600


  ! NGRBLITOTA1 - 228051 - 1h averaged total lightning flash density
  ! NGRBLICGA1  - 228053 - 1h averaged cloud-to-ground lightning flash density
  CASE(NGRBLITOTA1, NGRBLICGA1)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E ! Average
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FIXED_SIZE_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 1*3600


  ! NGRBLITOTA3 - 228057 - 3h averaged total lightning flash density
  ! NGRBLICGA3  - 228059 - 3h averaged cloud-to-ground lightning flash density
  CASE(NGRBLITOTA3, NGRBLICGA3)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E ! Average
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FIXED_SIZE_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 3*3600


  ! NGRBLITOTA6 - 228058 - 6h averaged total lightning flash density
  ! NGRBLICGA6  - 228060 - 6h averaged cloud-to-ground lightning flash density
  CASE(NGRBLITOTA6, NGRBLICGA6)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_AVERAGE_E ! Average
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FIXED_SIZE_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 6*3600


  ! NGRBMXCAP6  - 228035 - maximum CAPE in the last 6 hours
  ! NGRBMXCAPS6 - 228036 - maximum CAPES in the last 6 hours
  CASE(NGRBMXCAP6, NGRBMXCAPS6)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_MAX_E ! Max
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FIXED_SIZE_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 6*3600


  ! NGRBPTYPEMODE1 - 260320 - Precipitation type (most frequent) in the last 1 hour
  CASE(NGRBPTYPEMODE1)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_MODE_E ! Mode
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FIXED_SIZE_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 1*3600


  ! NGRBPTYPEMODE3 - 260321 - Precipitation type (most frequent) in the last 3 hours
  CASE(NGRBPTYPEMODE3)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_MODE_E ! Mode
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FIXED_SIZE_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 3*3600


  ! NGRBPTYPEMODE6 - 260339 Precipitation type (most frequent) in the last 6 hours
  CASE(NGRBPTYPEMODE6)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_MODE_E ! Mode
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FIXED_SIZE_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 6*3600


  CASE(NGRBPTYPESEVR1)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E ! Severity
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FIXED_SIZE_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 1*3600


  ! NGRBPTYPESEVR1 - 260318 Precipitation type (most severe) in the last 1 hour
  CASE(NGRBPTYPESEVR3)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E ! Severity
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FIXED_SIZE_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 3*3600


  ! NGRBPTYPESEVR3 - 260319 Precipitation type (most severe) in the last 3 hours
  CASE(NGRBPTYPESEVR6)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_SEVERITY_E ! Severity
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FIXED_SIZE_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 6*3600


  ! NGRBLSP  - 142 Large scale precipitation
  ! NGRBSF   - 144 Snow fall
  ! NGRBSSRD - 169 Surface solar radiation downwards
  ! NGRBSTRD - 175 Surface thermal radiation downwards
  ! NGRBSSR  - 176 Surface solar radiation
  ! NGRBSTR  - 177 Surface thermal radiation
  ! NGRBTTR  - 179 Top thermal radiation
  ! NGRBRO   - 205 Runoff
  ! NGRBTP   - 228 Total precipitation
  ! NGRBFZRA - 228216 Freezing rain accumulation
  ! NGRBTSR  - 178 Top solar radiation
  ! NGRBDSRP -  47 Direct solar radiation incident on a plane perpendicular to the Sun's direction
  ! NGRBLSPF -  50 large scale precipitation fraction
  ! NGRBUVB  -  57 surface UV-B radiation
  ! NGRBPAR  -  58 surface PARadiation
  ! NGRBBLD  - 145 Boundary layer dissipation
  ! NGRBSLHF - 147 Surface latent heat flux
  ! NGRBNSSS - 181 V-stress
  ! NGRBEWSS - 180 U-stress
  ! NGRBSUND - 189 Sunshine duration
  ! NGRBMGWS - 196 Meridional component of gravity wave stress
  ! NGRBGWD  - 197 Gravity wave dissipation
  ! NGRBTSRC - 208 Top solar radiation clear sky
  ! NGRBSSRC - 210 Surface solar radiation clear sky
  ! NGRBOCLZ - 213 layer depth
  ! NGRBE    - 182 Evaporation
  ! NGRBSMLT -  45 Snow melt
  ! NGRBSRO   -  8 Surface Runoff
  ! NGRBSSRO  -  9 Sub-Surface Runoff
  CASE(NGRBLSP,NGRBSF,NGRBSSRD,NGRBSTRD,NGRBSSR,NGRBSTR,NGRBTTR,NGRBRO,NGRBTP,NGRBFZRA, &
       & NGRBTSR,NGRBDSRP,NGRBLSPF,NGRBUVB,NGRBPAR,NGRBBLD,NGRBSLHF,NGRBNSSS,NGRBEWSS, &
       & NGRBSUND,NGRBMGWS,NGRBGWD,NGRBTSRC,NGRBSSRC,NGRBOCLZ,NGRBE,NGRBSMLT,NGRBSRO,NGRBSSRO)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 11 - Individual ensemble forecast, control and perturbed,
      !      at a horizontal level or in a horizontal layer, in a
      !      continuous or non-continuous interval
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 11
    ELSE
      ! 8 - Average, accumulation, extreme values or other statistically
      !     processed values at a horizontal level or in a horizontal
      !     layer in a continuous or non-continuous time interval
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 8
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E ! Accumulation
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FROM_STEP0_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = -1


  ! NGRBPTYPESEVR6 - 260338 Precipitation type (most severe) in the last 6 hours
  CASE (222001:222256, 223001:223256) ! DGOV-353, has to be 'accum' also for SEC=0
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 11 - Individual ensemble forecast, control and perturbed,
      !      at a horizontal level or in a horizontal layer, in a
      !      continuous or non-continuous interval
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 11
    ELSE
      ! 8 - Average, accumulation, extreme values or other statistically
      !     processed values at a horizontal level or in a horizontal
      !     layer in a continuous or non-continuous time interval
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 8
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_ACCUMUL_E ! Accumulation
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FROM_STEP0_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = -1


  ! NGRB10FG -  49 gust at 10 m level since previous post-processing
  ! NGRBMX2T - 201 Maximum temperature at 2m since last post-processing
  CASE(NGRB10FG,NGRBMX2T)
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 11 - Individual ensemble forecast, control and perturbed,
      !      at a horizontal level or in a horizontal layer, in a
      !      continuous or non-continuous interval
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 11
    ELSE
      ! 8 - Average, accumulation, extreme values or other statistically
      !     processed values at a horizontal level or in a horizontal
      !     layer in a continuous or non-continuous time interval
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 8
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_MAX_E ! Max
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_FROM_LASTPP_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = -2


  CASE DEFAULT
    IF ( IS_ENSAMBLE_SIMULATION( YDMODEL_PARAMETERS ) ) THEN
      ! 1 - Individual ensemble forecast, control and perturbed,
      !     at a horizontal level or in a horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 1
    ELSE
      ! 0 - Analysis or forecast at a horizontal level or in a
      !     horizontal layer at a point in time
      KD_PRODUCT_DEFINITION_TEMPLATE_NUMBER = 0
    ENDIF
    KDTYPE_OF_STATISTICAL_PROCESSING = TYPE_OF_STATISTICAL_PROCESS_INSTANT_E ! Instant
    KDTYPE_OF_TIMERANGE              = TYPE_OF_TIME_RANGE_INSTANT_E
    KDLENGTH_OF_TIMERANGE_IN_SECONDS = 0

  END SELECT

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE COMPUTE_TIMING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE TIME_ASSUMPTIONS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
