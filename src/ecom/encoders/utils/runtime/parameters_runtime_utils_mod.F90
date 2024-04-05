! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


! Definition of the module
#define PP_FILE_NAME 'parameters_runtime_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MODULE PARAMETERS_RUNTIME_UTILS_MOD'
MODULE PARAMETERS_RUNTIME_UTILS_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: PARAMETERS_RUNTIME_INIT
PUBLIC :: PARAMETERS_RUNTIME_FREE
PUBLIC :: COMPUTE_PARAM_GRIBX_ATM
PUBLIC :: COMPUTE_PARAM_GRIBX_WAM

CONTAINS



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PARAMETERS_RUNTIME_INIT'
SUBROUTINE PARAMETERS_RUNTIME_INIT( CFG, MODEL_PARAMS )

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

END SUBROUTINE PARAMETERS_RUNTIME_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PARAMETERS_RUNTIME_FREE'
SUBROUTINE PARAMETERS_RUNTIME_FREE( )

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

END SUBROUTINE PARAMETERS_RUNTIME_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMPUTE_PARAM_GRIBX_WAM'
FUNCTION COMPUTE_PARAM_GRIBX_WAM(  MODEL_PARAMS, GRIB_INFO, MSG, CDTYPE, METADATA ) RESULT(IPARAM)

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
  TYPE(MODEL_PAR_T),    TARGET,    INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_WAM_MSG_T),              INTENT(IN)    :: MSG
  CHARACTER(LEN=*),                INTENT(IN)    :: CDTYPE
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Function Result
  INTEGER(KIND=JPIB_K) :: IPARAM

  ! Local varaibles
  INTEGER(KIND=JPIB_K) :: ITABLE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Initial value
  IPARAM = -99
  ITABLE = -99

  ! Initialize the parameter
  IF ( MSG%ITABLE .EQ. 128 ) THEN
    ! it seems that then default table is not used when defining paramId !
    IPARAM = MSG%IPARAM
  ELSE
    IPARAM = MSG%ITABLE*1000 + MSG%IPARAM
  ENDIF
  PP_METADATA_SET( METADATA,  'paramId', IPARAM )

  ! TODO decide where to put this code
  IF ( IPARAM .EQ. 251 ) THEN
    PP_METADATA_SET( METADATA,  'waveDirectionNumber', MSG%IANGLE )
    PP_METADATA_SET( METADATA,  'waveFrequencyNumber', MSG%IFREQ )
  ENDIF

  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION COMPUTE_PARAM_GRIBX_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMPUTE_PARAM_GRIBX_ATM'
FUNCTION COMPUTE_PARAM_GRIBX_ATM(  MODEL_PARAMS, GRIB_INFO, MSG, CDTYPE, METADATA ) RESULT(IPARAM)

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
  TYPE(MODEL_PAR_T),    TARGET,    INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),               INTENT(IN)    :: GRIB_INFO
  TYPE(OM_ATM_MSG_T),              INTENT(IN)    :: MSG
  CHARACTER(LEN=*),                INTENT(IN)    :: CDTYPE
  CLASS(METADATA_BASE_A), POINTER, INTENT(INOUT) :: METADATA

  ! Function Result
  INTEGER(KIND=JPIB_K) :: IPARAM

  ! Local varaibles
  INTEGER(KIND=JPIB_K) :: ITABLE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Initial value
  IPARAM = -99
  ITABLE = -99


  ! Initialize the parameter
  IF( MSG%PARAM_ID_ .GT. 1000 ) THEN
    ITABLE = MSG%PARAM_ID_ / 1000
    IPARAM = MOD(MSG%PARAM_ID_, 1000)
  ELSE
    ITABLE = 128
    IPARAM = MSG%PARAM_ID_
  ENDIF


  ! Update the parameter according to the type
  ! TODO: need to explain this messy logic
  SELECT CASE ( CDTYPE(1:2) )


  ! 4i - 4D analysis increments (gribCode=33)
  ! me - Model errors (gribCode=35)
  CASE( '4i', 'me' )
    IF( ITABLE .EQ. 128 ) IPARAM = 200000 + IPARAM
    IF( ITABLE .EQ. 210 ) IPARAM = 211000 + IPARAM


  ! sg - Sensitivity gradient (gribCode=50)
  ! sf - Sensitivity forecast (gribCode=52)
  CASE( 'sg', 'sf' )
    IF( ITABLE .EQ. 128 ) IPARAM = 129000 + IPARAM


  CASE DEFAULT
    IF( ITABLE .NE. 128 ) IPARAM = ITABLE*1000 + IPARAM

  END SELECT


  ! Trace end of procedure (on success)
  PP_METADATA_EXIT_PROCEDURE( METADATA )
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION COMPUTE_PARAM_GRIBX_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#if 0
FUNCTION IS_SIMULATED_SATELLITE_IMAGE( YDIOS, YLREQFLD, IPARAM ) RESULT(LSSI)

    USE :: PARKIND1, ONLY: JPIM
    USE :: YOMIO_SERV, ONLY: IO_SERV
    USE :: YOMIO_SERV_SREQ, ONLY: IO_SERV_SREQ

    USE :: YOM_GRIB_CODES, ONLY: NGRBCSBT
    USE :: YOM_GRIB_CODES, ONLY: NGRBCLBT

    USE :: YOMHOOK, ONLY: LHOOK
    USE :: YOMHOOK, ONLY: DR_HOOK
    USE :: YOMHOOK, ONLY: JPHOOK

IMPLICIT NONE

    ! Dummy arguments
    TYPE(IO_SERV),        INTENT(IN)  :: YDIOS
    TYPE(IO_SERV_SREQ),   INTENT(IN)  :: YLREQFLD
    INTEGER(KIND=JPIM),   INTENT(IN)  :: IPARAM

    ! Function Result
    LOGICAL :: LSSI

    ! Local variables
    REAL(KIND=JPHOOK) :: ZHOOK_HANDLE


    IF (LHOOK) CALL DR_HOOK('GRIB_ENCODER_EC_RUNTIME_LEVELS_UTILS_MOD::IS_SIMULATED_SATELLITE_IMAGE',0,ZHOOK_HANDLE)

    LSSI = .FALSE.

    SELECT CASE (IPARAM)

    ! NGRBCLBT - 260510 Cloudy brightness temperature
    ! NGRBCSBT - 260511 Clear-sky brightness temperature
    CASE (NGRBCLBT, NGRBCSBT)

        LSSI = .TRUE.

    CASE DEFAULT

        LSSI = .FALSE.

    END SELECT

    IF (LHOOK) CALL DR_HOOK('GRIB_ENCODER_EC_RUNTIME_LEVELS_UTILS_MOD::IS_SIMULATED_SATELLITE_IMAGE',1,ZHOOK_HANDLE)

    ! Exit point
    RETURN

END FUNCTION IS_SIMULATED_SATELLITE_IMAGE


SUBROUTINE SET_SSI( YDHANDLE, YDIOS, YDSREQ )

    USE :: PARKIND1, ONLY: JPIM
    USE :: YOMIO_SERV, ONLY: IO_SERV
    USE :: YOMIO_SERV_SREQ, ONLY: IO_SERV_SREQ
    USE :: VALUE_SETTER_MOD, ONLY: VALUE_SETTER

    USE :: YOMHOOK, ONLY: LHOOK
    USE :: YOMHOOK, ONLY: DR_HOOK
    USE :: YOMHOOK, ONLY: JPHOOK

IMPLICIT NONE

    ! Dummy arguments
    CLASS(VALUE_SETTER), INTENT(INOUT) :: YDHANDLE
    TYPE(IO_SERV),       INTENT(IN)    :: YDIOS
    TYPE(IO_SERV_SREQ),  INTENT(IN)    :: YDSREQ

    ! Local variables
    REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
    INTEGER(KIND=JPIM) :: IRCWN

    IF (LHOOK) CALL DR_HOOK('GRIB_ENCODER_EC_RUNTIME_LEVELS_UTILS_MOD::SET_SSI',0,ZHOOK_HANDLE)

    ASSOCIATE( KIMG    => YDSREQ%YFLDSC%ILEVG,            &
&              KSERIES => YDIOS%MODELPAR%YECGRIB%MSERIES, &
&              KSATID  => YDIOS%MODELPAR%YECGRIB%MSATID,  &
&              KINST   => YDIOS%MODELPAR%YECGRIB%MINST,   &
&              KCHAN   => YDIOS%MODELPAR%YECGRIB%MCHAN,   &
&              PCWN    => YDIOS%MODELPAR%YECGRIB%RCWN )


    IRCWN = 100_JPIM * NINT(PCWN(KIMG))
    CALL YDHANDLE%SET( 'satelliteSeries',KSERIES(KIMG))
    CALL YDHANDLE%SET( 'satelliteNumber',KSATID(KIMG))
    CALL YDHANDLE%SET( 'instrumentType',KINST(KIMG))
    CALL YDHANDLE%SET( 'channelNumber',KCHAN(KIMG))
    CALL YDHANDLE%SET( 'scaledValueOfCentralWaveNumber',IRCWN)

    END ASSOCIATE

    IF (LHOOK) CALL DR_HOOK('GRIB_ENCODER_EC_RUNTIME_LEVELS_UTILS_MOD::SET_SSI',1,ZHOOK_HANDLE)

    ! Exit point
    RETURN

END SUBROUTINE SET_SSI
#endif

END MODULE PARAMETERS_RUNTIME_UTILS_MOD