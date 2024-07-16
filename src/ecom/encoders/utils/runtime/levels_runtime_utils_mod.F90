#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'levels_runtime_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'LEVELS_RUNTIME_UTILS_MOD'
MODULE LEVELS_RUNTIME_UTILS_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: LEVELS_RUNTIME_INIT
PUBLIC :: LEVELS_RUNTIME_FREE
PUBLIC :: SET_SURFACE_LEVEL_GRIBX_ATM
PUBLIC :: SET_SOL_SNOW_LEVEL_GRIBX_ATM
PUBLIC :: SET_SOL_SOIL_LEVEL_GRIBX_ATM
PUBLIC :: SET_SOL_ICE_LEVEL_GRIBX_ATM
PUBLIC :: SET_MODEL_LEVEL_GRIBX_ATM
PUBLIC :: SET_PRESSURE_LEVEL_GRIBX_ATM
PUBLIC :: SET_VORTICITY_LEVEL_GRIBX_ATM
PUBLIC :: SET_THETA_LEVEL_GRIBX_ATM
PUBLIC :: SET_LEVEL_WAM

PUBLIC :: IS_SNOW_LAYER
PUBLIC :: IS_SOIL_LAYER
PUBLIC :: IS_ICE_LAYER

CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'LEVELS_RUNTIME_INIT'
SUBROUTINE LEVELS_RUNTIME_INIT( CFG, MODEL_PARAMS )

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

END SUBROUTINE LEVELS_RUNTIME_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'LEVELS_RUNTIME_FREE'
SUBROUTINE LEVELS_RUNTIME_FREE( )

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

END SUBROUTINE LEVELS_RUNTIME_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IS_SOIL_LAYER'
FUNCTION IS_SOIL_LAYER( IGRIBCD, LEVEL ) RESULT(LSOILLAYER)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IGRIBCD
  INTEGER(KIND=JPIB_K), INTENT(IN) :: LEVEL

  ! Function result
  LOGICAL :: LSOILLAYER

  ! Local variables
  LOGICAL, DIMENSION(100) :: CONDITIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  CONDITIONS(1)  =  (IGRIBCD .EQ. 39)
  CONDITIONS(2)  =  (IGRIBCD .EQ. 40)
  CONDITIONS(3)  =  (IGRIBCD .EQ. 41)
  CONDITIONS(4)  =  (IGRIBCD .EQ. 42)
  CONDITIONS(5)  =  (IGRIBCD .EQ. 139)
  CONDITIONS(6)  =  (IGRIBCD .EQ. 170)
  CONDITIONS(7)  =  (IGRIBCD .EQ. 183)
  CONDITIONS(8)  =  (IGRIBCD .EQ. 236)
  CONDITIONS(9)  =  (IGRIBCD .EQ. 140)
  CONDITIONS(10) =  (IGRIBCD .EQ. 171)
  CONDITIONS(11) =  (IGRIBCD .EQ. 184)
  CONDITIONS(12) =  (IGRIBCD .EQ. 237)
  CONDITIONS(13) =  (IGRIBCD .EQ. 129039)
  CONDITIONS(14) =  (IGRIBCD .EQ. 129040)
  CONDITIONS(15) =  (IGRIBCD .EQ. 129041)
  CONDITIONS(16) =  (IGRIBCD .EQ. 129042)
  CONDITIONS(17) =  (IGRIBCD .EQ. 129139)
  CONDITIONS(18) =  (IGRIBCD .EQ. 129170)
  CONDITIONS(19) =  (IGRIBCD .EQ. 129183)
  CONDITIONS(20) =  (IGRIBCD .EQ. 129236)
  CONDITIONS(21) =  (IGRIBCD .EQ. 129140)
  CONDITIONS(22) =  (IGRIBCD .EQ. 129171)
  CONDITIONS(23) =  (IGRIBCD .EQ. 129184)
  CONDITIONS(24) =  (IGRIBCD .EQ. 129237)
  CONDITIONS(25) =  (IGRIBCD .EQ. 160140)
  CONDITIONS(26) =  (IGRIBCD .EQ. 160171)
  CONDITIONS(27) =  (IGRIBCD .EQ. 160184)
  CONDITIONS(28) =  (IGRIBCD .EQ. -999999)
  CONDITIONS(29) =  (IGRIBCD .EQ. 171039)
  CONDITIONS(30) =  (IGRIBCD .EQ. 171040)
  CONDITIONS(31) =  (IGRIBCD .EQ. 171041)
  CONDITIONS(32) =  (IGRIBCD .EQ. 171042)
  CONDITIONS(33) =  (IGRIBCD .EQ. 171139)
  CONDITIONS(34) =  (IGRIBCD .EQ. 171170)
  CONDITIONS(35) =  (IGRIBCD .EQ. 171183)
  CONDITIONS(36) =  (IGRIBCD .EQ. 171236)
  CONDITIONS(37) =  (IGRIBCD .EQ. 171140)
  CONDITIONS(38) =  (IGRIBCD .EQ. 171171)
  CONDITIONS(39) =  (IGRIBCD .EQ. 171184)
  CONDITIONS(40) =  (IGRIBCD .EQ. 171237)
  CONDITIONS(41) =  (IGRIBCD .EQ. 174039)
  CONDITIONS(42) =  (IGRIBCD .EQ. 174040)
  CONDITIONS(43) =  (IGRIBCD .EQ. 174041)
  CONDITIONS(44) =  (IGRIBCD .EQ. 174042)
  CONDITIONS(45) =  (IGRIBCD .EQ. 174139)
  CONDITIONS(46) =  (IGRIBCD .EQ. 174170)
  CONDITIONS(47) =  (IGRIBCD .EQ. 174183)
  CONDITIONS(48) =  (IGRIBCD .EQ. 174236)
  CONDITIONS(49) =  (IGRIBCD .EQ. 175039)
  CONDITIONS(50) =  (IGRIBCD .EQ. 175040)
  CONDITIONS(51) =  (IGRIBCD .EQ. 175041)
  CONDITIONS(52) =  (IGRIBCD .EQ. 175042)
  CONDITIONS(53) =  (IGRIBCD .EQ. 175139)
  CONDITIONS(54) =  (IGRIBCD .EQ. 175170)
  CONDITIONS(55) =  (IGRIBCD .EQ. 175183)
  CONDITIONS(56) =  (IGRIBCD .EQ. 175236)
  CONDITIONS(57) =  (IGRIBCD .EQ. 200039)
  CONDITIONS(58) =  (IGRIBCD .EQ. 200040)
  CONDITIONS(59) =  (IGRIBCD .EQ. 200041)
  CONDITIONS(60) =  (IGRIBCD .EQ. 200042)
  CONDITIONS(61) =  (IGRIBCD .EQ. 200139)
  CONDITIONS(62) =  (IGRIBCD .EQ. 200170)
  CONDITIONS(63) =  (IGRIBCD .EQ. 200183)
  CONDITIONS(64) =  (IGRIBCD .EQ. 200236)
  CONDITIONS(65) =  (IGRIBCD .EQ. 200140)
  CONDITIONS(66) =  (IGRIBCD .EQ. 200171)
  CONDITIONS(67) =  (IGRIBCD .EQ. 200184)
  CONDITIONS(68) =  (IGRIBCD .EQ. 200237)
  CONDITIONS(69) =  (IGRIBCD .EQ. 228040)
  CONDITIONS(70) =  (IGRIBCD .EQ. 228041)
  CONDITIONS(71) =  (IGRIBCD .EQ. 228042)
  CONDITIONS(72) =  (IGRIBCD .EQ. 228043)
  CONDITIONS(73) =  (IGRIBCD .EQ. 254001)
  CONDITIONS(74) =  (IGRIBCD .EQ. 254004)
  CONDITIONS(75) =  (IGRIBCD .EQ. 254007)
  CONDITIONS(76) =  (IGRIBCD .EQ. -999999)
  CONDITIONS(77) =  (IGRIBCD .EQ. 254002)
  CONDITIONS(78) =  (IGRIBCD .EQ. 254005)
  CONDITIONS(79) =  (IGRIBCD .EQ. 254008)
  CONDITIONS(80) =  (IGRIBCD .EQ. -999999)
  CONDITIONS(81) =  (IGRIBCD .EQ. 254003)
  CONDITIONS(82) =  (IGRIBCD .EQ. 254006)
  CONDITIONS(83) =  (IGRIBCD .EQ. 254009)
  CONDITIONS(84) =  (IGRIBCD .EQ. -999999)
  CONDITIONS(85) =  (IGRIBCD .EQ. 254010)
  CONDITIONS(86) =  (IGRIBCD .EQ. 254012)
  CONDITIONS(87) =  (IGRIBCD .EQ. 254014)
  CONDITIONS(88) =  (IGRIBCD .EQ. -999999)
  CONDITIONS(89) =  (IGRIBCD .EQ. 254011)
  CONDITIONS(90) =  (IGRIBCD .EQ. 254013)
  CONDITIONS(91) =  (IGRIBCD .EQ. 254015)
  CONDITIONS(92) =  (IGRIBCD .EQ. 200026)
  CONDITIONS(93) =  (IGRIBCD .EQ. 200199)
  CONDITIONS(94) =  (IGRIBCD .EQ. -999999)
  CONDITIONS(95) =  (IGRIBCD .EQ. -999999)
  CONDITIONS(96) =  (IGRIBCD .EQ. -999999)
  CONDITIONS(97) =  (IGRIBCD .EQ. -999999)
  CONDITIONS(98) =  (IGRIBCD .EQ. -999999)
  CONDITIONS(99) =  (IGRIBCD .EQ. -999999)
  CONDITIONS(100) = (IGRIBCD .EQ. -999999)

  ! True for snow layers
  LSOILLAYER = ANY(CONDITIONS)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION IS_SOIL_LAYER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IS_ICE_LAYER'
FUNCTION IS_ICE_LAYER( IGRIBCD, LEVEL ) RESULT(LSNOWLAYER)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IGRIBCD
  INTEGER(KIND=JPIB_K), INTENT(IN) :: LEVEL

  ! Function result
  LOGICAL :: LSNOWLAYER

  ! Local variables
  LOGICAL, DIMENSION(4) :: CONDITIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  CONDITIONS(1) = (IGRIBCD .EQ. 35)
  CONDITIONS(2) = (IGRIBCD .EQ. 36)
  CONDITIONS(3) = (IGRIBCD .EQ. 37)
  CONDITIONS(4) = (IGRIBCD .EQ. 38)

  ! True for snow layers
  LSNOWLAYER = ANY(CONDITIONS)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION IS_ICE_LAYER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IS_SNOW_LAYER'
FUNCTION IS_SNOW_LAYER( IGRIBCD, LEVEL ) RESULT(LSNOWLAYER)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,    ONLY: JPIB_K
  USE :: GRIB_CODES_MOD, ONLY: NGRBSD
  USE :: GRIB_CODES_MOD, ONLY: NGRBTSN
  USE :: GRIB_CODES_MOD, ONLY: NGRBRSN
  USE :: GRIB_CODES_MOD, ONLY: NGRBWSN

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IGRIBCD
  INTEGER(KIND=JPIB_K), INTENT(IN) :: LEVEL

  ! Function result
  LOGICAL :: LSNOWLAYER

  ! Local variables
  LOGICAL, DIMENSION(5) :: CONDITIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  ! NGRBSD   - 228141 - Snow depth (multi-layer)
  ! NGRBTSN  - 238    - Temperature of snow layer
  ! NGRBRSN  - 33     - Snow density
  ! NGRBWSN  - 228038 - Snow liquid water (multi-layer)

  CONDITIONS(1) = (IGRIBCD .EQ. NGRBSD)
  CONDITIONS(2) = (IGRIBCD .EQ. NGRBTSN)
  CONDITIONS(3) = (IGRIBCD .EQ. NGRBRSN)
  CONDITIONS(4) = (IGRIBCD .EQ. NGRBWSN)

  ! True for snow layers
  LSNOWLAYER = ANY(CONDITIONS) .AND. (LEVEL .NE. 0)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION IS_SNOW_LAYER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_SURFACE_LEVEL_GRIBX_ATM'
SUBROUTINE SET_SURFACE_LEVEL_GRIBX_ATM( MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ITOP
  INTEGER(KIND=JPIB_K) :: IBOT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! PP_METADATA_SET( METADATA,  'typeOfLevel', 'surface' )

  IF ( MSG%PARAM_ID_ .EQ. 228026 .OR. MSG%PARAM_ID_ .EQ. 228027 ) THEN
    ! Set type of level
    PP_METADATA_SET( METADATA,  'typeOfFirstFixedSurface', INT(103,JPIB_K) )
    PP_METADATA_SET( METADATA,  'scaleFactorOfFirstFixedSurface', INT(2,JPIB_K) )
    PP_METADATA_SET( METADATA,  'scaledValueOfFirstFixedSurface', INT(0,JPIB_K) )
    PP_METADATA_SET( METADATA,  'typeOfSecondFixedSurface', INT(255,JPIB_K) )
    PP_METADATA_SET_MISSING( METADATA,  'scaleFactorOfSecondFixedSurface' )
    PP_METADATA_SET_MISSING( METADATA,  'scaledValueOfSecondFixedSurface' )
  ELSEIF ( MSG%PARAM_ID_ .EQ. 228028 ) THEN
    ! Set type of level
    PP_METADATA_SET( METADATA,  'typeOfFirstFixedSurface', INT(103,JPIB_K) )
    PP_METADATA_SET( METADATA,  'scaleFactorOfFirstFixedSurface', INT(10,JPIB_K) )
    PP_METADATA_SET( METADATA,  'scaledValueOfFirstFixedSurface', INT(0,JPIB_K) )
    PP_METADATA_SET( METADATA,  'typeOfSecondFixedSurface', INT(255,JPIB_K) )
    PP_METADATA_SET_MISSING( METADATA,  'scaleFactorOfSecondFixedSurface' )
    PP_METADATA_SET_MISSING( METADATA,  'scaledValueOfSecondFixedSurface' )
  ELSE
    ! Set type of level
    PP_METADATA_SET( METADATA,  'typeOfFirstFixedSurface', INT(1,JPIB_K) )
    PP_METADATA_SET_MISSING( METADATA,  'scaleFactorOfFirstFixedSurface' )
    PP_METADATA_SET_MISSING( METADATA,  'scaledValueOfFirstFixedSurface' )
    PP_METADATA_SET( METADATA,  'typeOfSecondFixedSurface', INT(255,JPIB_K) )
    PP_METADATA_SET_MISSING( METADATA,  'scaleFactorOfSecondFixedSurface' )
    PP_METADATA_SET_MISSING( METADATA,  'scaledValueOfSecondFixedSurface' )
  ENDIF

  ! Set level
  PP_METADATA_SET( METADATA,  'level', INT(0,JPIB_K) )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Not implemented' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown message type' )
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

END SUBROUTINE SET_SURFACE_LEVEL_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_SOL_SNOW_LEVEL_GRIBX_ATM'
SUBROUTINE SET_SOL_SNOW_LEVEL_GRIBX_ATM( MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ITOP
  INTEGER(KIND=JPIB_K) :: IBOT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  PP_METADATA_SET( METADATA,  'typeOfFirstFixedSurface', INT(114,JPIB_K) )
  PP_METADATA_SET( METADATA,  'scaleFactorOfFirstFixedSurface', INT(0,JPIB_K) )
  PP_METADATA_SET( METADATA,  'scaleFactorOfSecondFixedSurface', INT(0,JPIB_K) )
  PP_METADATA_SET( METADATA,  'typeOfSecondFixedSurface', INT(114,JPIB_K) )
  PP_METADATA_SET( METADATA,  'scaledValueOfFirstFixedSurface', MSG%ILEVG_-1 )
  PP_METADATA_SET( METADATA,  'scaledValueOfSecondFixedSurface', MSG%ILEVG_ )

  PP_METADATA_SET( METADATA,  'level', MSG%ILEVG_ ) ! level must be defined at the end

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Not implemented' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown message type' )
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

END SUBROUTINE SET_SOL_SNOW_LEVEL_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_SOL_ICE_LEVEL_GRIBX_ATM'
SUBROUTINE SET_SOL_ICE_LEVEL_GRIBX_ATM( MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LEVEL

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

#if defined(NEW_SOL_ENCODING)
  ! Mapping levels
  SELECT CASE ( MSG%PARAM_ID_ )
  CASE (35)
    LEVEL=1
  CASE (36)
    LEVEL=2
  CASE (37)
    LEVEL=3
  CASE (38)
    LEVEL=4
  CASE DEFAULT
    LEVEL = MSG%ILEVG_
  END SELECT

  PP_METADATA_SET( METADATA,  'typeOfFirstFixedSurface', INT(152,JPIB_K) )
  PP_METADATA_SET( METADATA,  'scaleFactorOfFirstFixedSurface', INT(0,JPIB_K) )
  PP_METADATA_SET( METADATA,  'scaleFactorOfSecondFixedSurface', INT(0,JPIB_K) )
  PP_METADATA_SET( METADATA,  'typeOfSecondFixedSurface', INT(152,JPIB_K) )
  PP_METADATA_SET( METADATA,  'scaledValueOfFirstFixedSurface', LEVEL-1 )
  PP_METADATA_SET( METADATA,  'scaledValueOfSecondFixedSurface', LEVEL )

  PP_METADATA_SET( METADATA,  'level', LEVEL ) ! level must be defined at the end
#else
  ! Set type of level
  PP_METADATA_SET( METADATA,  'typeOfFirstFixedSurface', INT(1,JPIB_K) )
  PP_METADATA_SET_MISSING( METADATA,  'scaleFactorOfFirstFixedSurface' )
  PP_METADATA_SET_MISSING( METADATA,  'scaledValueOfFirstFixedSurface' )
  PP_METADATA_SET( METADATA,  'typeOfSecondFixedSurface', INT(255,JPIB_K) )
  PP_METADATA_SET_MISSING( METADATA,  'scaleFactorOfSecondFixedSurface' )
  PP_METADATA_SET_MISSING( METADATA,  'scaledValueOfSecondFixedSurface' )
  ! Set level
  PP_METADATA_SET( METADATA,  'level', INT(0,JPIB_K) )
#endif

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Not implemented' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown message type' )
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

END SUBROUTINE SET_SOL_ICE_LEVEL_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_SOL_SOIL_LEVEL_GRIBX_ATM'
SUBROUTINE SET_SOL_SOIL_LEVEL_GRIBX_ATM( MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables
  INTEGER(KIND=JPIB_K) :: LEVEL

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

#if defined(NEW_SOL_ENCODING)
  ! Mapping levels
  SELECT CASE ( MSG%PARAM_ID_ )
  CASE (39,139)
    LEVEL=1
  CASE (40,170)
    LEVEL=2
  CASE (41,183)
    LEVEL=3
  CASE (42,236)
    LEVEL=4
  CASE DEFAULT
    LEVEL = MSG%ILEVG_
  END SELECT

  PP_METADATA_SET( METADATA,  'typeOfFirstFixedSurface', INT(151,JPIB_K) )
  PP_METADATA_SET( METADATA,  'scaleFactorOfFirstFixedSurface', INT(0,JPIB_K) )
  PP_METADATA_SET( METADATA,  'scaleFactorOfSecondFixedSurface', INT(0,JPIB_K) )
  PP_METADATA_SET( METADATA,  'typeOfSecondFixedSurface', INT(151,JPIB_K) )
  PP_METADATA_SET( METADATA,  'scaledValueOfFirstFixedSurface', LEVEL-1 )
  PP_METADATA_SET( METADATA,  'scaledValueOfSecondFixedSurface', LEVEL )

  PP_METADATA_SET( METADATA,  'level', LEVEL ) ! level must be defined at the end
#else
  ! Set type of level
  PP_METADATA_SET( METADATA,  'typeOfFirstFixedSurface', INT(1,JPIB_K) )
  PP_METADATA_SET_MISSING( METADATA,  'scaleFactorOfFirstFixedSurface' )
  PP_METADATA_SET_MISSING( METADATA,  'scaledValueOfFirstFixedSurface' )
  PP_METADATA_SET( METADATA,  'typeOfSecondFixedSurface', INT(255,JPIB_K) )
  PP_METADATA_SET_MISSING( METADATA,  'scaleFactorOfSecondFixedSurface' )
  PP_METADATA_SET_MISSING( METADATA,  'scaledValueOfSecondFixedSurface' )
  ! Set level
  PP_METADATA_SET( METADATA,  'level', INT(0,JPIB_K) )
#endif



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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Not implemented' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown message type' )
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

END SUBROUTINE SET_SOL_SOIL_LEVEL_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_MODEL_LEVEL_GRIBX_ATM'
SUBROUTINE SET_MODEL_LEVEL_GRIBX_ATM( MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )


  PP_METADATA_SET( METADATA,  'typeOfLevel','hybrid')
  PP_METADATA_SET( METADATA,  'level', MSG%ILEVG_ )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Not implemented' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown message type' )
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

END SUBROUTINE SET_MODEL_LEVEL_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_PRESSURE_LEVEL_GRIBX_ATM'
SUBROUTINE SET_PRESSURE_LEVEL_GRIBX_ATM( MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )


  IF( MSG%ILEVG_ .GE. 100) THEN
    PP_METADATA_SET( METADATA,  'typeOfLevel','isobaricInhPa')
    PP_METADATA_SET( METADATA,  'level', MSG%ILEVG_/100)
  ELSE
    PP_METADATA_SET( METADATA,  'typeOfLevel','isobaricInPa')
    PP_METADATA_SET( METADATA,  'level', MSG%ILEVG_)
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Not implemented' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown message type' )
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

END SUBROUTINE SET_PRESSURE_LEVEL_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_VORTICITY_LEVEL_GRIBX_ATM'
SUBROUTINE SET_VORTICITY_LEVEL_GRIBX_ATM(MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )


  PP_METADATA_SET( METADATA,  'typeOfLevel','potentialVorticity')
  PP_METADATA_SET( METADATA,  'level', MSG%ILEVG_ )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Not implemented' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown message type' )
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

END SUBROUTINE SET_VORTICITY_LEVEL_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_THETA_LEVEL_GRIBX_ATM'
SUBROUTINE SET_THETA_LEVEL_GRIBX_ATM( MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )


  PP_METADATA_SET( METADATA,  'typeOfLevel','theta')
  PP_METADATA_SET( METADATA,  'level', MSG%ILEVG_ )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Not implemented' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown message type' )
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

END SUBROUTINE SET_THETA_LEVEL_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SET_LEVEL_WAM'
SUBROUTINE SET_LEVEL_WAM( MODEL_PARAMS, GRIB_INFO, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: OM_WAM_MSG_T
  USE :: GRIB_INFO_DATA_MOD, ONLY: GRIB_INFO_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),      TARGET,  INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_WAM_MSG_T),              INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Type of level Need to be moved out from here
  PP_METADATA_SET( METADATA,  'typeOfFirstFixedSurface', INT(1,JPIB_K) )
  PP_METADATA_SET_MISSING( METADATA,  'typeOfSecondFixedSurface' )
  PP_METADATA_SET_MISSING( METADATA,  'scaleFactorOfFirstFixedSurface' )
  PP_METADATA_SET_MISSING( METADATA,  'scaledValueOfFirstFixedSurface' )
  PP_METADATA_SET_MISSING( METADATA,  'scaleFactorOfSecondFixedSurface' )
  PP_METADATA_SET_MISSING( METADATA,  'scaledValueOfSecondFixedSurface' )
  PP_METADATA_SET( METADATA,  'level', INT(MSG%KLEV,JPIB_K) )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Not implemented' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown message type' )
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

END SUBROUTINE SET_LEVEL_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE LEVELS_RUNTIME_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
