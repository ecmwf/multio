! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'time_runtime_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'TIME_RUNTIME_UTILS_MOD'
MODULE TIME_RUNTIME_UTILS_MOD

  ! Symbols imported from other modules within the project.
  USE :: TIME_ENCODERS_MOD, ONLY: ENCODERS_OPTIONS_T
  USE :: TIME_ENCODERS_MOD, ONLY: ENCODE_TIME_ATM_IF
  USE :: TIME_ENCODERS_MOD, ONLY: ENCODE_TIME_WAM_IF
  USE :: OM_CORE_MOD,       ONLY: CURR_TIME_T

IMPLICIT NONE

! Default visibility of the module
PRIVATE


TYPE :: TIME_MANAGER_CONTAINER_ATM_T
  PRIVATE
  PROCEDURE(ENCODE_TIME_ATM_IF), POINTER, NOPASS :: PATM_ => NULL()
CONTAINS
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: INITIALISE  => INITIALISE_ATM
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FINALISE    => FINALISE_ATM
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: ENCODE_TIME => ENCODE_TIME_ATM
END TYPE

TYPE :: TIME_MANAGER_CONTAINER_WAM_T
  PRIVATE
  PROCEDURE(ENCODE_TIME_WAM_IF), POINTER, NOPASS :: PWAM_ => NULL()
CONTAINS
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: INITIALISE  => INITIALISE_WAM
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: FINALISE    => FINALISE_WAM
  PROCEDURE, NON_OVERRIDABLE, PASS, PUBLIC :: ENCODE_TIME => ENCODE_TIME_WAM
END TYPE

TYPE(ENCODERS_OPTIONS_T) :: OPTIONS
TYPE(TIME_MANAGER_CONTAINER_ATM_T), ALLOCATABLE, DIMENSION(:) :: TIME_ENCODERS_ATM
TYPE(TIME_MANAGER_CONTAINER_WAM_T), ALLOCATABLE, DIMENSION(:) :: TIME_ENCODERS_WAM


! Whitelist of public symbols
PUBLIC :: TIME_RUNTIME_INIT
PUBLIC :: TIME_RUNTIME_FREE
PUBLIC :: TIME_ENCODERS_ATM
PUBLIC :: TIME_ENCODERS_WAM
PUBLIC :: TIME_INDEXER
PUBLIC :: COMPUTE_CURRENT_TIME
PUBLIC :: COMPUTE_REFERENCE_TIME


CONTAINS


!>
!> @brief Pre-sets the date and time of the simulation
!>
!> This function sets the time informations of a simulation into the provided metadata object
!> using information retrieved from the specified data structure (YDIOS).
!>
!> @param [inout] METADATA Metadata object where the GPI will be set.
!> @param [in]    YDIOS    Data structure used to retrieve the information necessary to compute the GPI.
!>
!> @todo Deprecation Notice: The functionality related to VAREPS is deprecated and will not be used in the future.
!>       All code related to VAREPS should be removed.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'COMPUTE_REFERENCE_TIME'
SUBROUTINE COMPUTE_REFERENCE_TIME( MODEL_PARAMS, DATADATE, DATATIME, DYYYY1, DMM1, DDD1, THH1, TMM1, TSS1 )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: JPIB_K
  USE :: OM_CORE_MOD,        ONLY: PROC_TOPO_T
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: DATETIME_UTILS_MOD, ONLY: PACK_HHMM
  USE :: DATETIME_UTILS_MOD, ONLY: SEC2HH_MM_SS
  USE :: DATETIME_UTILS_MOD, ONLY: UNPACK_YYYYMMDD
  USE :: DATETIME_UTILS_MOD, ONLY: PACK_YYYYMMDD
  USE :: DATETIME_UTILS_MOD, ONLY: DATE_SUB_DAYS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T), TARGET, INTENT(IN)  :: MODEL_PARAMS
  INTEGER(KIND=JPIB_K),      INTENT(OUT) :: DATADATE
  INTEGER(KIND=JPIB_K),      INTENT(OUT) :: DATATIME
  INTEGER(KIND=JPIB_K),      INTENT(OUT) :: DYYYY1
  INTEGER(KIND=JPIB_K),      INTENT(OUT) :: DMM1
  INTEGER(KIND=JPIB_K),      INTENT(OUT) :: DDD1
  INTEGER(KIND=JPIB_K),      INTENT(OUT) :: THH1
  INTEGER(KIND=JPIB_K),      INTENT(OUT) :: TMM1
  INTEGER(KIND=JPIB_K),      INTENT(OUT) :: TSS1

  ! Local variables
  LOGICAL, DIMENSION(2) :: CONDITION1
  LOGICAL, DIMENSION(2) :: CONDITION2

  INTEGER(KIND=JPIB_K) :: DYYYY
  INTEGER(KIND=JPIB_K) :: DMM
  INTEGER(KIND=JPIB_K) :: DDD
  INTEGER(KIND=JPIB_K) :: THH
  INTEGER(KIND=JPIB_K) :: TMM
  INTEGER(KIND=JPIB_K) :: TSS
  INTEGER(KIND=JPIB_K) :: IFCDA_INI
  INTEGER(KIND=JPIB_K) :: IFCHO_RES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ASSOCIATE ( YPI => MODEL_PARAMS%SIM_ )

    ! Extract date/time components
    CALL UNPACK_YYYYMMDD( YPI%NINDAT, DYYYY, DMM, DDD )
    CALL SEC2HH_MM_SS( YPI%NSSSSS, THH, TMM, TSS )

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
    CONDITION1(1) = (YPI%CTYPE .EQ. 'fc') ! 'type' is forecast (gribCode=9)
    CONDITION1(2) = (YPI%LOBSC1)          ! .T. = term of observations included in configuration 1

    ! Second special case
    CONDITION2(1) = (YPI%LVAREPS)     ! .T. when running with variable resolution
    CONDITION2(2) = (YPI%NLEG .GE. 2) ! current VAREPS leg number (eg 1(2) for the T399(T255) part of a T399-T255 VAREPS)


    ! If needed modify the time
    IF ( ALL(CONDITION1) ) THEN
      ! NOTE: This code works because NSTEPINI is supposed to be less than 24
      ! NSTEPINI: Initial step in hours for the initial conditions
      !           at the beginning of 4D-Var trajectory (usually 3 hours).
      !           It is used to update the step while saving the FCs along
      !           the first trajectory.
      THH1 = THH - YPI%NSTEPINI
      IF ( THH1 .LT. 0 ) THEN
        THH1 = THH1 + 24
        ! TODO: Replace custom function with Julian date provided in eccodes
        CALL DATE_SUB_DAYS( DYYYY, DMM, DDD, INT(-1,JPIB_K), DYYYY1, DMM1, DDD1 )
      ENDIF

    ELSEIF ( ALL(CONDITION2) ) THEN
      ! NFCHO_TRUNC_INI: forecast step used to define the ICs (ie NFCHO_TRUNC of previous VAREPS LEG)
      IFCDA_INI = YPI%NFCHO_TRUNC_INI/24
      IFCHO_RES = MOD(YPI%NFCHO_TRUNC_INI, 24)
      THH1 = THH - IFCHO_RES
      TMM1 = 0
      TSS1 = 0
      IF ( THH1 .LT. 0 ) THEN
        THH1 = THH1 + 24
        IFCDA_INI = IFCDA_INI + 1
      ENDIF
      ! TODO: Replace custom function with Julian date provided in eccodes
      CALL DATE_SUB_DAYS( DYYYY, DMM, DDD, -IFCDA_INI, DYYYY1, DMM1, DDD1 )

    ENDIF

    ! Output date and time
    DATADATE = PACK_YYYYMMDD( DYYYY1, DMM1, DDD1 )
    DATATIME = PACK_HHMM( THH1, TMM1 )

  END ASSOCIATE

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE COMPUTE_REFERENCE_TIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'COMPUTE_CURRENT_TIME'
FUNCTION COMPUTE_CURRENT_TIME( MODEL_PARAMS, ENCODING_INFO, ISTEP, TIME_HIST, CURR_TIME ) RESULT(EX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: OM_CORE_MOD,       ONLY: JPRD_K
  USE :: OM_CORE_MOD,       ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,       ONLY: OM_BASE_MSG_A
  USE :: OM_CORE_MOD,       ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,       ONLY: TIME_HISTORY_T
  USE :: OM_CORE_MOD,       ONLY: UNDEF_PARAM_E
  USE :: ENCODING_INFO_MOD, ONLY: ENCODING_INFO_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(MODEL_PAR_T),     INTENT(IN)    :: MODEL_PARAMS
  TYPE(ENCODING_INFO_T), INTENT(INOUT) :: ENCODING_INFO
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: ISTEP
  TYPE(TIME_HISTORY_T),  INTENT(OUT)   :: TIME_HIST
  TYPE(CURR_TIME_T),     INTENT(OUT)   :: CURR_TIME

  ! Function result
  LOGICAL :: EX

  ! Local variables
  INTEGER(KIND=JPIB_K)  :: DATADATE
  INTEGER(KIND=JPIB_K)  :: DATATIME
  INTEGER(KIND=JPIB_K)  :: DYYYY
  INTEGER(KIND=JPIB_K)  :: DMM
  INTEGER(KIND=JPIB_K)  :: DDD
  INTEGER(KIND=JPIB_K)  :: THH
  INTEGER(KIND=JPIB_K)  :: TMM
  INTEGER(KIND=JPIB_K)  :: TSS
  LOGICAL :: UPDATE_TIME_HISTORY
  LOGICAL :: TMP

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( TRIM(MODEL_PARAMS%SIM_%CTYPE).NE.'fc', 1 )
  PP_DEBUG_CRITICAL_COND_THROW( MODEL_PARAMS%SIM_%LPPSTEPS, 2 )


  ! Compute the reference time of the simulation
  ! TODO: For performaces reson this can be cached in case of "fc" type
  CALL COMPUTE_REFERENCE_TIME( MODEL_PARAMS, &
&                              CURR_TIME%DATADATE,        &
&                              CURR_TIME%DATATIME,        &
&                              CURR_TIME%SIM_REF_TIME(1), &
&                              CURR_TIME%SIM_REF_TIME(2), &
&                              CURR_TIME%SIM_REF_TIME(3), &
&                              CURR_TIME%SIM_REF_TIME(4), &
&                              CURR_TIME%SIM_REF_TIME(5), &
&                              CURR_TIME%SIM_REF_TIME(6)  )


  CURR_TIME%TSTEP = MODEL_PARAMS%SIM_%TSTEP
  CURR_TIME%ISEC  = INT( REAL(ISTEP,JPRD_K)*CURR_TIME%TSTEP, JPIB_K)

  CURR_TIME%ISEC = CURR_TIME%ISEC + MODEL_PARAMS%SIM_%NSTEPINI*3600


  ! This was supposed to be different from 0 only for the VAREPS case
  ! which is not supported anymore
  CURR_TIME%ISEC0=0

  ! Cehck if the field needs to be encoded
  EX = TO_BE_ENCODED( ENCODING_INFO, ISTEP, CURR_TIME, UPDATE_TIME_HISTORY )

  IF ( UPDATE_TIME_HISTORY ) THEN
    CALL ENCODING_INFO%TIME_HISTORY%ENQUEUE( ISTEP )
    TMP = ENCODING_INFO%TIME_HISTORY%GET_ALL( TIME_HIST%SIZE_, TIME_HIST%HIST_(1:)  )
  ELSE
    TIME_HIST%SIZE_ = UNDEF_PARAM_E
    TIME_HIST%HIST_ = UNDEF_PARAM_E
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Instant time encoding logic not implemented for mars type: "'//TRIM(MODEL_PARAMS%SIM_%CTYPE)//'"' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Logic not implemented for LPPSTEPS' )
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

END FUNCTION COMPUTE_CURRENT_TIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TO_BE_ENCODED'
FUNCTION TO_BE_ENCODED( ENCODING_INFO, ISTEP, CURR_TIME, UPDATE_TIME_HISTORY ) RESULT(CHECK)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: OM_CORE_MOD,       ONLY: JPRD_K
  USE :: OM_CORE_MOD,       ONLY: TIME_HISTORY_T
  USE :: OM_CORE_MOD,       ONLY: CURR_TIME_T
  USE :: ENCODING_INFO_MOD, ONLY: ENCODING_INFO_T
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_INSTANT_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_STEP0_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FROM_LASTPP_E
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGE_FIXED_SIZE_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE


  ! Dummy arguments
  TYPE(ENCODING_INFO_T), INTENT(IN)    :: ENCODING_INFO
  INTEGER(KIND=JPIB_K),  INTENT(IN)    :: ISTEP
  TYPE(CURR_TIME_T),     INTENT(INOUT) :: CURR_TIME
  LOGICAL,               INTENT(OUT)   :: UPDATE_TIME_HISTORY

  ! Function result
  LOGICAL :: CHECK

  ! Local variables
  LOGICAL :: EX
  INTEGER(KIND=JPIB_K) :: PREV_SEC
  INTEGER(KIND=JPIB_K) :: PREV_STEP

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ASSOCIATE ( GRIB_INFO => ENCODING_INFO%GRIB_INFO, TIME_HISTORY => ENCODING_INFO%TIME_HISTORY )

  IF ( TIME_HISTORY%IS_EMPTY() ) THEN
    UPDATE_TIME_HISTORY = .TRUE.
    SELECT CASE (GRIB_INFO%TYPE_OF_TIME_RANGE_)
    CASE (TYPE_OF_TIME_RANGE_INSTANT_E, TYPE_OF_TIME_RANGE_FROM_STEP0_E, TYPE_OF_TIME_RANGE_FROM_LASTPP_E)
      IF ( CURR_TIME%ISEC .EQ. CURR_TIME%ISEC0 ) THEN
        CHECK = GRIB_INFO%IS_STEP0_VALID_
        CURR_TIME%IS_STEP_0 = .TRUE.
      ELSE
        CHECK = .TRUE.
        CURR_TIME%IS_STEP_0 = .FALSE.
      ENDIF
    CASE (TYPE_OF_TIME_RANGE_FIXED_SIZE_E)
      IF ( CURR_TIME%ISEC - GRIB_INFO%OVERALL_LENGTH_OF_TIME_RANGE_ .LT. CURR_TIME%ISEC0 ) THEN
        CHECK = GRIB_INFO%IS_STEP0_VALID_
        CURR_TIME%IS_STEP_0 = .TRUE.
      ELSE
        CHECK = .TRUE.
        CURR_TIME%IS_STEP_0 = .FALSE.
      END IF
    CASE DEFAULT
      PP_DEBUG_CRITICAL_THROW( 1 )
    END SELECT
  ELSE
    SELECT CASE (GRIB_INFO%TYPE_OF_TIME_RANGE_)
    CASE (TYPE_OF_TIME_RANGE_INSTANT_E,TYPE_OF_TIME_RANGE_FROM_STEP0_E,TYPE_OF_TIME_RANGE_FROM_LASTPP_E)
      UPDATE_TIME_HISTORY = .TRUE.
      CHECK = .TRUE.
      CURR_TIME%IS_STEP_0 = .FALSE.
    CASE (TYPE_OF_TIME_RANGE_FIXED_SIZE_E)
      EX = TIME_HISTORY%GET(1_JPIB_K,PREV_STEP)
      PREV_SEC  = INT( REAL(PREV_STEP,JPRD_K)*CURR_TIME%TSTEP, JPIB_K)
      IF ( CURR_TIME%ISEC - GRIB_INFO%OVERALL_LENGTH_OF_TIME_RANGE_ .LT. PREV_SEC ) THEN
        UPDATE_TIME_HISTORY = .FALSE.
        CHECK = .FALSE.
        CURR_TIME%IS_STEP_0 = .TRUE.
      ELSE
        UPDATE_TIME_HISTORY = .TRUE.
        CHECK = .TRUE.
        CURR_TIME%IS_STEP_0 = .FALSE.
      END IF
    CASE DEFAULT
      PP_DEBUG_CRITICAL_THROW( 1 )
    END SELECT
  ENDIF

  END ASSOCIATE

  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=32) :: TMP

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      TMP=REPEAT(' ',32)
      WRITE(TMP,*) ENCODING_INFO%GRIB_INFO%TYPE_OF_TIME_RANGE_
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknknown type of timerange: '//TRIM(ADJUSTL(TMP))//')' )
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

END FUNCTION TO_BE_ENCODED
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TIME_INDEXER'
FUNCTION TIME_INDEXER( TYPE_OF_STATSTICSL_PROCESS, TYPE_OF_TIMERANGE, EDITION ) RESULT(IDX)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: TYPE_OF_STATSTICSL_PROCESS
  INTEGER(KIND=JPIB_K), INTENT(IN) :: TYPE_OF_TIMERANGE
  INTEGER(KIND=JPIB_K), INTENT(IN) :: EDITION

  ! Function result
  INTEGER(KIND=JPIB_K) :: IDX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Compute time encoder indexindex
  IDX = ( TYPE_OF_STATSTICSL_PROCESS-1 ) * 4 * 2 + &
&       ( TYPE_OF_TIMERANGE-1 ) * 2 + &
&       EDITION

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION TIME_INDEXER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TIME_RUNTIME_INIT'
SUBROUTINE TIME_RUNTIME_INIT( CFG, MODEL_PARAMS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: GRIB_EDITIONS
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_STATISTICAL_PROCESSES
  USE :: OM_CORE_MOD, ONLY: TYPE_OF_TIME_RANGES

  USE :: TIME_ENCODERS_MOD, ONLY: ENCODE_TIME_ATM_IF
  USE :: TIME_ENCODERS_MOD, ONLY: ENCODE_TIME_WAM_IF

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

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: K

  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  ! Allocate ATM encoder memory
  ALLOCATE( TIME_ENCODERS_ATM(64), STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )

  ! Allocate WAM encoder memory
  ALLOCATE( TIME_ENCODERS_WAM(64), STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )

  DO I = 1, SIZE(GRIB_EDITIONS)
    DO J = 1, SIZE(TYPE_OF_STATISTICAL_PROCESSES)
      DO K = 1, SIZE(TYPE_OF_TIME_RANGES)
        CALL TIME_ENCODERS_ATM( TIME_INDEXER( TYPE_OF_STATISTICAL_PROCESSES(J), TYPE_OF_TIME_RANGES(K), GRIB_EDITIONS(I) ) )% &
&                                 INITIALISE( TYPE_OF_STATISTICAL_PROCESSES(J), TYPE_OF_TIME_RANGES(K), GRIB_EDITIONS(I) )
        CALL TIME_ENCODERS_WAM( TIME_INDEXER( TYPE_OF_STATISTICAL_PROCESSES(J), TYPE_OF_TIME_RANGES(K), GRIB_EDITIONS(I) ) )% &
&                                 INITIALISE( TYPE_OF_STATISTICAL_PROCESSES(J), TYPE_OF_TIME_RANGES(K), GRIB_EDITIONS(I) )
      ENDDO
    ENDDO
  ENDDO

  ! Paranoid check
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG)
  ENDIF

  ! Trace end of procedure (on success)
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
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate time ATM encoders: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate time ATM encoders.' )
      ENDIF
    CASE (2)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate time WAM encoders: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate time WAM encoders.' )
      ENDIF
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

END SUBROUTINE TIME_RUNTIME_INIT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TIME_RUNTIME_FREE'
SUBROUTINE TIME_RUNTIME_FREE( )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,  ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Free ATM encoder memory
  DEALLOCATE( TIME_ENCODERS_ATM, STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 1 )

  ! Free WAM encoder memory
  DEALLOCATE( TIME_ENCODERS_WAM, STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )

  ! Trace end of procedure (on success)
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
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate time ATM encoders: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate time ATM encoders.' )
      ENDIF
    CASE (2)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate time WAM encoders: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate time WAM encoders.' )
      ENDIF
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

END SUBROUTINE TIME_RUNTIME_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'INITIALISE_ATM'
SUBROUTINE INITIALISE_ATM( THIS, TYPE_OF_STATSTICSL_PROCESS, TYPE_OF_TIMERANGE, EDITION )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: TIME_ENCODERS_MOD, ONLY: ENCODE_TIME_ATM_IF
  USE :: TIME_ENCODERS_MOD, ONLY: ATM_TIME_ENCODERS_FACTORY

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(TIME_MANAGER_CONTAINER_ATM_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K), INTENT(IN) :: TYPE_OF_STATSTICSL_PROCESS
  INTEGER(KIND=JPIB_K), INTENT(IN) :: TYPE_OF_TIMERANGE
  INTEGER(KIND=JPIB_K), INTENT(IN) :: EDITION

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  THIS%PATM_ => ATM_TIME_ENCODERS_FACTORY( TYPE_OF_STATSTICSL_PROCESS, TYPE_OF_TIMERANGE, EDITION )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE INITIALISE_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODE_TIME_ATM'
SUBROUTINE ENCODE_TIME_ATM( THIS, MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD,        ONLY: TIME_HISTORY_T
  USE :: OM_CORE_MOD,        ONLY: OM_ATM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(TIME_MANAGER_CONTAINER_ATM_T), INTENT(INOUT) :: THIS
  TYPE(MODEL_PAR_T),                   INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),                   INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),                INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),                   INTENT(IN)    :: CURR_TIME
  CLASS(OM_ATM_MSG_T),                 INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER,     INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.ASSOCIATED(THIS%PATM_), 1 )

  ! Call the time encoder
  CALL THIS%PATM_( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Time encoder not associated' )
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

END SUBROUTINE ENCODE_TIME_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'FINALISE_ATM'
SUBROUTINE FINALISE_ATM( THIS )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(TIME_MANAGER_CONTAINER_ATM_T), INTENT(INOUT) :: THIS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Nullify the function pointers
  NULLIFY( THIS%PATM_ )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE FINALISE_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'INITIALISE_WAM'
SUBROUTINE INITIALISE_WAM( THIS, TYPE_OF_STATSTICSL_PROCESS, TYPE_OF_TIMERANGE, EDITION )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: TIME_ENCODERS_MOD, ONLY: ENCODE_TIME_WAM_IF
  USE :: TIME_ENCODERS_MOD, ONLY: WAM_TIME_ENCODERS_FACTORY

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(TIME_MANAGER_CONTAINER_WAM_T), INTENT(INOUT) :: THIS
  INTEGER(KIND=JPIB_K), INTENT(IN) :: TYPE_OF_STATSTICSL_PROCESS
  INTEGER(KIND=JPIB_K), INTENT(IN) :: TYPE_OF_TIMERANGE
  INTEGER(KIND=JPIB_K), INTENT(IN) :: EDITION

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  THIS%PWAM_ => WAM_TIME_ENCODERS_FACTORY( TYPE_OF_STATSTICSL_PROCESS, TYPE_OF_TIMERANGE, EDITION )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE INITIALISE_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'ENCODE_TIME_WAM'
SUBROUTINE ENCODE_TIME_WAM( THIS, MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,        ONLY: MODEL_PAR_T
  USE :: OM_CORE_MOD,        ONLY: CURR_TIME_T
  USE :: OM_CORE_MOD,        ONLY: GRIB_INFO_T
  USE :: OM_CORE_MOD,        ONLY: TIME_HISTORY_T
  USE :: OM_CORE_MOD,        ONLY: OM_WAM_MSG_T
  USE :: METADATA_BASE_MOD,  ONLY: METADATA_BASE_A

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(TIME_MANAGER_CONTAINER_WAM_T), INTENT(INOUT) :: THIS
  TYPE(MODEL_PAR_T),                   INTENT(IN)    :: MODEL_PARAMS
  TYPE(GRIB_INFO_T),                   INTENT(IN)    :: GRIB_INFO
  TYPE(TIME_HISTORY_T),                INTENT(IN)    :: TIME_HIST
  TYPE(CURR_TIME_T),                   INTENT(IN)    :: CURR_TIME
  CLASS(OM_WAM_MSG_T),                 INTENT(IN)    :: MSG
  CLASS(METADATA_BASE_A), POINTER,     INTENT(INOUT) :: METADATA

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()
  PP_METADATA_ENTER_PROCEDURE( METADATA )

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( .NOT.ASSOCIATED(THIS%PWAM_), 1 )

  ! Call the proper encoder
  CALL THIS%PWAM_( MODEL_PARAMS, GRIB_INFO, TIME_HIST, CURR_TIME, MSG, METADATA, OPTIONS )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Time encoder not associated' )
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

END SUBROUTINE ENCODE_TIME_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'FINALISE_WAM'
SUBROUTINE FINALISE_WAM( THIS )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CLASS(TIME_MANAGER_CONTAINER_WAM_T), INTENT(INOUT) :: THIS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Nullify the function pointers
  NULLIFY( THIS%PWAM_ )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE FINALISE_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



END MODULE TIME_RUNTIME_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
