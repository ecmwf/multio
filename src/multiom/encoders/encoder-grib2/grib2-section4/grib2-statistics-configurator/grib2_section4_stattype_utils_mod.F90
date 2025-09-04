! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'grib2_section4_stattype_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'GRIB2_SECTION4_STATTYPE_UTILS_MOD'
MODULE GRIB2_SECTION4_STATTYPE_UTILS_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K

IMPLICIT NONE

!>
!> Default symbols visibility
PRIVATE

INTEGER(KIND=JPIB_K), PARAMETER :: MAX_STATS = 8_JPIB_K

!> Definition of the datatype used to represent the type of statistical processing
TYPE :: STATISTICAL_PROCESSING_T
  INTEGER(KIND=JPIB_K) :: NUMBER_OF_TIME_RANGES=0_JPIB_K
  INTEGER(KIND=JPIB_K), DIMENSION(MAX_STATS) :: TYPE_OF_STATISTICAL_PROCESSING
  INTEGER(KIND=JPIB_K), DIMENSION(MAX_STATS) :: TYPE_OF_TIME_INCREMENT
  INTEGER(KIND=JPIB_K), DIMENSION(MAX_STATS) :: INDICATOR_OF_UNIT_FOR_TIME_RANGE
  INTEGER(KIND=JPIB_K), DIMENSION(MAX_STATS) :: LENGTH_OF_TIME_RANGE
  INTEGER(KIND=JPIB_K), DIMENSION(MAX_STATS) :: INDICATOR_OF_UNIT_FOR_TIME_INCREMENT
  INTEGER(KIND=JPIB_K), DIMENSION(MAX_STATS) :: LENGTH_OF_TIME_INCREMENT
END TYPE STATISTICAL_PROCESSING_T

!> Whitelist of public symbols
PUBLIC :: GET_NUMBER_OF_TIMELOOPS_FROM_MARS
PUBLIC :: COMPUTE_END_DATETIME
PUBLIC :: COMPUTE_FORECAST_DATETIME
PUBLIC :: GET_GRIB2_TIME_DESCRIPTION_FROM_MARS


CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GET_NUMBER_OF_TIMELOOPS_FROM_MARS'
PP_THREAD_SAFE FUNCTION GET_NUMBER_OF_TIMELOOPS_FROM_MARS( &
&  MSG, NUMBER_OF_TIMELOOPS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,          ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(FORTRAN_MESSAGE_T), INTENT(IN)    :: MSG
  INTEGER(KIND=JPIB_K),    INTENT(OUT)   :: NUMBER_OF_TIMELOOPS
  TYPE(HOOKS_T),           INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  LOGICAL :: HAS_TIMESPAN
  LOGICAL :: HAS_STATTYPE

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_INPUT_VALUE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_LOGIC_ERROR = 2_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()


  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Initialize output and local variables
  NUMBER_OF_TIMELOOPS = -1_JPIB_K
  HAS_TIMESPAN = .FALSE.
  HAS_STATTYPE = .FALSE.

  ! Check for timespan and stattype
  IF ( MSG%TIMESPAN .NE. UNDEF_PARAM_E ) THEN
    HAS_TIMESPAN = .TRUE.
  END IF

  IF ( MSG%STATTYPE .NE. REPEAT('*',32) ) THEN
    HAS_STATTYPE = .TRUE.
  END IF

  ! Determine number of timeloops
  IF ( .NOT.HAS_TIMESPAN .AND. .NOT.HAS_STATTYPE ) THEN
    NUMBER_OF_TIMELOOPS = 0_JPIB_K
  ELSEIF ( HAS_TIMESPAN .AND. .NOT.HAS_STATTYPE ) THEN
    NUMBER_OF_TIMELOOPS = 1_JPIB_K
  ELSEIF ( HAS_TIMESPAN .AND. HAS_STATTYPE ) THEN
    NUMBER_OF_TIMELOOPS = 2_JPIB_K
    DO I = 1_JPIB_K, LEN_TRIM(MSG%STATTYPE)
      IF ( MSG%STATTYPE(I:I) == '_' ) THEN
        NUMBER_OF_TIMELOOPS = NUMBER_OF_TIMELOOPS + 1_JPIB_K
      ENDIF
    ENDDO
  ELSE
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_WRONG_INPUT_VALUE )
  ENDIF

  ! Sanity check
  PP_DEBUG_CRITICAL_COND_THROW( NUMBER_OF_TIMELOOPS .LT. 0_JPIB_K, ERRFLAG_LOGIC_ERROR )

  ! Trace end of procedure (on error)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE ( ERRFLAG_WRONG_INPUT_VALUE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong input value: inconsistent timespan/stattype combination' )
    CASE ( ERRFLAG_LOGIC_ERROR )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'logic error: number of timeloops < 0' )
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

END FUNCTION GET_NUMBER_OF_TIMELOOPS_FROM_MARS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARSE_STATYPE'
PP_THREAD_SAFE FUNCTION PARSE_STATYPE( &
&  MSG, STATTYPE, NBLOCKS, NTIME_LOOPS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,          ONLY: UNDEF_PARAM_E
  USE :: ENUMERATORS_MOD,          ONLY: CTYPE_OF_PERIOD2ITYPE_OF_PERIOD
  USE :: ENUMERATORS_MOD,          ONLY: ITYPE_OF_PERIOD2CTYPE_OF_PERIOD
  USE :: ENUMERATORS_MOD,          ONLY: CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS
  USE :: ENUMERATORS_MOD,          ONLY: ITYPE_OF_STATISTICAL_PROCESS2CTYPE_OF_STATISTICAL_PROCESS

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(FORTRAN_MESSAGE_T),              INTENT(IN)    :: MSG
  INTEGER(KIND=JPIB_K), DIMENSION(:,:), INTENT(OUT)   :: STATTYPE
  INTEGER(KIND=JPIB_K),                 INTENT(OUT)   :: NTIME_LOOPS
  INTEGER(KIND=JPIB_K),                 INTENT(OUT)   :: NBLOCKS
  TYPE(HOOKS_T),                        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  CHARACTER(LEN=2) :: CPER
  CHARACTER(LEN=2) :: COP
  INTEGER(KIND=JPIB_K) :: IPER
  INTEGER(KIND=JPIB_K) :: IOP
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: POS
  LOGICAL :: BVALIDPERIOD
  LOGICAL :: BVALIDOPERATION

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_NUMBER_OF_TIMELOOPS = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRONG_STATTYPE_DIMENSIONS = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_PERIOD = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_OPERATION = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STATTYPE_FORMAT = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_PERIOD = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_CONVERT_OPERATION = 7_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Get number of timeloops
  PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_NUMBER_OF_TIMELOOPS) &
&           GET_NUMBER_OF_TIMELOOPS_FROM_MARS( MSG, NTIME_LOOPS, HOOKS )

  ! Number of block in stattype is the number of timeloops - 1
  NBLOCKS = NTIME_LOOPS - 1_JPIB_K

  ! Error handling (basic dimensional checks)
  PP_DEBUG_CRITICAL_COND_THROW( NBLOCKS .LT. 0_JPIB_K, ERRFLAG_UNABLE_TO_GET_NUMBER_OF_TIMELOOPS )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(STATTYPE,1) .LT. NBLOCKS,  ERRFLAG_WRONG_STATTYPE_DIMENSIONS )
  PP_DEBUG_CRITICAL_COND_THROW( SIZE(STATTYPE,2) .NE. 2_JPIB_K, ERRFLAG_WRONG_STATTYPE_DIMENSIONS )

  ! Parse stattype if needed
  IF ( NBLOCKS .GT. 0_JPIB_K ) THEN

    POS = 1_JPIB_K
    DO I = 1_JPIB_K, NBLOCKS

      ! Extract period and operation
      CPER = MSG%STATTYPE(POS+0_JPIB_K :POS+1_JPIB_K )
      COP  = MSG%STATTYPE(POS+2_JPIB_K :POS+3_JPIB_K )

      ! Validate period and operation
      PP_TRYCALL(ERRFLAG_INVALID_PERIOD) IS_VALID_PERIOD( CPER, BVALIDPERIOD, HOOKS )
      PP_TRYCALL(ERRFLAG_INVALID_OPERATION) IS_VALID_OPERATION( COP, BVALIDOPERATION, HOOKS )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.BVALIDPERIOD,  ERRFLAG_INVALID_PERIOD )
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.BVALIDOPERATION, ERRFLAG_INVALID_OPERATION )

      ! Convert to enumerators
      PP_TRYCALL(ERRFLAG_CONVERT_PERIOD) CTYPE_OF_PERIOD2ITYPE_OF_PERIOD( CPER, IPER, HOOKS )
      PP_TRYCALL(ERRFLAG_CONVERT_OPERATION) CTYPE_OF_STATISTICAL_PROCESS2ITYPE_OF_STATISTICAL_PROCESS( COP, IOP, HOOKS )

      ! Store values
      STATTYPE(I,1) = IPER
      STATTYPE(I,2) = IOP

      POS = POS + 4_JPIB_K
      IF ( I .LT. NBLOCKS ) THEN
        PP_DEBUG_CRITICAL_COND_THROW( MSG%STATTYPE(POS:POS) .NE. '_', ERRFLAG_INVALID_STATTYPE_FORMAT )
        POS = POS + 1_JPIB_K
      ENDIF

    ENDDO

  ENDIF

  ! Trace end of procedure (on error)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE ( ERRFLAG_UNABLE_TO_GET_NUMBER_OF_TIMELOOPS )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get number of timeloops from MARS message' )
    CASE ( ERRFLAG_WRONG_STATTYPE_DIMENSIONS )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'wrong stattype dimensions' )
    CASE ( ERRFLAG_INVALID_PERIOD )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid period in stattype' )
    CASE ( ERRFLAG_INVALID_OPERATION )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid operation in stattype' )
    CASE ( ERRFLAG_INVALID_STATTYPE_FORMAT )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid stattype format' )
    CASE ( ERRFLAG_CONVERT_PERIOD )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert period from character to integer representation' )
    CASE ( ERRFLAG_CONVERT_OPERATION )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert operation from character to integer representation' )
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

END FUNCTION PARSE_STATYPE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'GET_GRIB2_TIME_DESCRIPTION_FROM_MARS'
PP_THREAD_SAFE FUNCTION GET_GRIB2_TIME_DESCRIPTION_FROM_MARS( &
&  MSG, PAR, OUTER_OP, TIME_DESCRIPTION, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: FORTRAN_MESSAGE_MOD,      ONLY: FORTRAN_MESSAGE_T
  USE :: PARAMETRIZATION_MOD,      ONLY: PARAMETRIZATION_T
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,          ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(FORTRAN_MESSAGE_T),        INTENT(IN)    :: MSG
  TYPE(PARAMETRIZATION_T),        INTENT(IN)    :: PAR
  INTEGER(KIND=JPIB_K),           INTENT(IN)    :: OUTER_OP
  TYPE(STATISTICAL_PROCESSING_T), INTENT(OUT)   :: TIME_DESCRIPTION
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: TMP
  INTEGER(KIND=JPIB_K) :: NBLOCKS
  INTEGER(KIND=JPIB_K) :: NTIME_LOOPS
  INTEGER(KIND=JPIB_K) :: END_YEAR
  INTEGER(KIND=JPIB_K) :: END_MONTH
  INTEGER(KIND=JPIB_K) :: END_DATE
  INTEGER(KIND=JPIB_K) :: END_TIME
  INTEGER(KIND=JPIB_K), DIMENSION(MAX_STATS,2) :: STATTYPE

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PARSE_STATYPE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNEXPECTED_NUMBER_OF_TIMELOOPS = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_TIME_ENUM = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_PERIOD_IN_HOURS = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_COMPUTE_END_DATETIME = 5_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Initialize output
  TIME_DESCRIPTION%NUMBER_OF_TIME_RANGES = -1_JPIB_K
  TIME_DESCRIPTION%TYPE_OF_STATISTICAL_PROCESSING = -1_JPIB_K
  TIME_DESCRIPTION%TYPE_OF_TIME_INCREMENT = -1_JPIB_K
  TIME_DESCRIPTION%INDICATOR_OF_UNIT_FOR_TIME_RANGE = -1_JPIB_K
  TIME_DESCRIPTION%LENGTH_OF_TIME_RANGE = -1_JPIB_K
  TIME_DESCRIPTION%INDICATOR_OF_UNIT_FOR_TIME_INCREMENT = -1_JPIB_K
  TIME_DESCRIPTION%LENGTH_OF_TIME_INCREMENT = -1_JPIB_K

  ! Compute end date/time
  PP_TRYCALL(ERRFLAG_COMPUTE_END_DATETIME) &
&    COMPUTE_OFFSET_DATETIME( MSG, MSG%STEP*3600_JPIB_K, END_DATE, END_TIME, HOOKS, YEAR=END_YEAR, MONTH=END_MONTH )

  ! Parse statype
  PP_TRYCALL(ERRFLAG_PARSE_STATYPE) &
&       PARSE_STATYPE( MSG, STATTYPE, NBLOCKS, NTIME_LOOPS, HOOKS )

  ! Error handling (basic dimensional checks)
  ! If number of time loops is lower than 1 something went wrong
  ! if it is 0 then is not a statistics
  ! if it is lower than 0 then it is an error
  PP_DEBUG_CRITICAL_COND_THROW( NTIME_LOOPS .LT. 1_JPIB_K, ERRFLAG_UNEXPECTED_NUMBER_OF_TIMELOOPS )

  ! Fill time description
  TIME_DESCRIPTION%NUMBER_OF_TIME_RANGES = NTIME_LOOPS
  DO I = 1, NTIME_LOOPS
    IF ( I .EQ. NTIME_LOOPS ) THEN
      TIME_DESCRIPTION%TYPE_OF_STATISTICAL_PROCESSING(1) = OUTER_OP
      TIME_DESCRIPTION%TYPE_OF_TIME_INCREMENT(1) = 2_JPIB_K ! Should be always 2 for multIO
      PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_TIME_ENUM) INDICATOR_OF_UNIT_FOR_TIME( 'h', TIME_DESCRIPTION%INDICATOR_OF_UNIT_FOR_TIME_RANGE(1), HOOKS )
      TIME_DESCRIPTION%LENGTH_OF_TIME_RANGE(1) = MSG%TIMESPAN ! TimeSpan is in hours
      PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_TIME_ENUM) INDICATOR_OF_UNIT_FOR_TIME( 's',  TIME_DESCRIPTION%INDICATOR_OF_UNIT_FOR_TIME_INCREMENT(1), HOOKS )
      TIME_DESCRIPTION%LENGTH_OF_TIME_INCREMENT(1) = INT(PAR%TIME%LENGTH_OF_TIME_STEP_IN_SECONDS_, KIND=JPIB_K)
    ELSEIF ( I .GT. 1_JPIB_K ) THEN
      TIME_DESCRIPTION%TYPE_OF_STATISTICAL_PROCESSING(I) = STATTYPE(I,1)
      TIME_DESCRIPTION%TYPE_OF_TIME_INCREMENT(I) = 2_JPIB_K ! Should be always 2 for multIO
      PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_TIME_ENUM) INDICATOR_OF_UNIT_FOR_TIME( 'h', TIME_DESCRIPTION%INDICATOR_OF_UNIT_FOR_TIME_RANGE(I), HOOKS )
      PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_PERIOD_IN_HOURS) PERIOD_TO_HOURS( STATTYPE(I,2), TIME_DESCRIPTION%LENGTH_OF_TIME_RANGE(I), END_YEAR, END_MONTH, HOOKS )
      PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_TIME_ENUM) INDICATOR_OF_UNIT_FOR_TIME( 's',  TIME_DESCRIPTION%INDICATOR_OF_UNIT_FOR_TIME_INCREMENT(I), HOOKS )
      IF ( I .EQ. NTIME_LOOPS-1_JPIB_K ) THEN
        TIME_DESCRIPTION%LENGTH_OF_TIME_INCREMENT(I) = MSG%TIMESPAN*3600_JPIB_K ! Last increment is always 1 hour
      ELSE
        PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_PERIOD_IN_HOURS) PERIOD_TO_HOURS( STATTYPE(I,2), TMP, END_YEAR, END_MONTH, HOOKS )
        TIME_DESCRIPTION%LENGTH_OF_TIME_RANGE(I) = TMP*3600_JPIB_K
      ENDIF
    ENDIF
  ENDDO

  ! Trace end of procedure (on error)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE ( ERRFLAG_PARSE_STATYPE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to parse stattype from MARS message' )
    CASE ( ERRFLAG_UNEXPECTED_NUMBER_OF_TIMELOOPS )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unexpected number of timeloops' )
    CASE ( ERRFLAG_UNABLE_TO_GET_TIME_ENUM )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to get time enumerator from string representation' )
    CASE ( ERRFLAG_UNABLE_TO_GET_PERIOD_IN_HOURS )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert period to hours' )
    CASE ( ERRFLAG_COMPUTE_END_DATETIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to compute end date/time' )
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

END FUNCTION GET_GRIB2_TIME_DESCRIPTION_FROM_MARS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IS_LOWERCASE'
PP_THREAD_SAFE FUNCTION IS_LOWERCASE( &
&  STR, BLOWERCASE, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*), INTENT(IN)    :: STR
  LOGICAL,          INTENT(OUT)   :: BLOWERCASE
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: C
  INTEGER(KIND=JPIB_K) :: I

  !> Error flags

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Check if the string is lowercase
  BLOWERCASE = .TRUE.
  checkLowercase: DO I = 1, LEN_TRIM(STR)
    C = IACHAR(STR(I:I))
    IF ( C .LT. IACHAR('a') .OR. C .GT. IACHAR('z') ) THEN
      BLOWERCASE = .FALSE.
      EXIT checkLowercase
    ENDIF
  ENDDO checkLowercase

  ! Trace end of procedure (on error)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION IS_LOWERCASE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INDICATOR_OF_UNIT_FOR_TIME'
PP_THREAD_SAFE FUNCTION INDICATOR_OF_UNIT_FOR_TIME( &
&  STR, TIME_ENUM, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: STR
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: TIME_ENUM
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNKNOWN_TIME_UNIT = 1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Check if the string is lowercase
  SELECT CASE ( TRIM(STR) )

  CASE ( 'mi', 'minutes' )
    TIME_ENUM = 0_JPIB_K

  CASE ( 'h', 'hours' )
    TIME_ENUM = 1_JPIB_K

  CASE ( 'd', 'days' )
    TIME_ENUM = 2_JPIB_K

  CASE ( 'mo', 'months' )
    TIME_ENUM = 3_JPIB_K

  CASE ( 'y', 'years' )
    TIME_ENUM = 4_JPIB_K

  CASE ( 'de', 'decades' )
    TIME_ENUM = 5_JPIB_K

  CASE ( 'n', 'normal' )
    TIME_ENUM = 6_JPIB_K

  CASE ( 'c', 'centuries' )
    TIME_ENUM = 7_JPIB_K

  CASE ( 's', 'seconds' )
    TIME_ENUM = 13_JPIB_K

  CASE DEFAULT
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNKNOWN_TIME_UNIT )
  END SELECT

  ! Trace end of procedure (on error)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE ( ERRFLAG_UNKNOWN_TIME_UNIT )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown time unit' )
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

END FUNCTION INDICATOR_OF_UNIT_FOR_TIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IS_VALID_PERIOD'
PP_THREAD_SAFE FUNCTION IS_VALID_PERIOD( &
&  STR, BVALIDPERIOD, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=2), INTENT(IN)    :: STR
  LOGICAL,          INTENT(OUT)   :: BVALIDPERIOD
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Check if the operation is valid
  SELECT CASE ( STR )
  CASE ( 'da', 'mo' )
    BVALIDPERIOD = .TRUE.
  CASE DEFAULT
    BVALIDPERIOD = .FALSE.
  END SELECT

  ! Trace end of procedure (on error)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION IS_VALID_PERIOD
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PERIOD_TO_HOURS'
PP_THREAD_SAFE FUNCTION PERIOD_TO_HOURS( &
&  PERIOD_ID, HOURS, YEAR, MONTH, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_OF_PERIOD_DAILY_E
  USE :: ENUMERATORS_MOD,          ONLY: TYPE_OF_PERIOD_MONTHLY_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=JPIB_K),           INTENT(IN)    :: PERIOD_ID
  INTEGER(KIND=JPIB_K),           INTENT(OUT)   :: HOURS
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(IN)    :: YEAR
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(IN)    :: MONTH
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_PERIOD = 1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Check if the operation is valid
  SELECT CASE ( PERIOD_ID )
  CASE ( TYPE_OF_PERIOD_DAILY_E )
    HOURS = 24
  CASE ( TYPE_OF_PERIOD_MONTHLY_E )
    IF ( PRESENT(YEAR) .AND. PRESENT(MONTH) ) THEN
      SELECT CASE ( MONTH )
      CASE ( 1, 3, 5, 7, 8, 10, 12 )
        HOURS = 744
      CASE ( 4, 6, 9, 11 )
        HOURS = 720
      CASE ( 2 )
        IF ( MOD(YEAR,4) == 0 ) THEN ! Julian calendar
! .AND. ( MOD(YEAR,100) /= 0 .OR. MOD(YEAR,400) == 0 ) ) THEN ! Gregorian calendar
          HOURS = 696
        ELSE
          HOURS = 672
        ENDIF
      CASE DEFAULT
        PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_PERIOD )
      END SELECT
    ELSE
      ! If year and month are not provided assume a month of 30 days
      HOURS = 720
    ENDIF
  CASE Default
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_PERIOD )
  END SELECT

  ! Trace end of procedure (on error)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE ( ERRFLAG_INVALID_PERIOD )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unknown period' )
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

END FUNCTION PERIOD_TO_HOURS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'IS_VALID_OPERATION'
PP_THREAD_SAFE FUNCTION IS_VALID_OPERATION( &
&  STR, BVALIDOPERATION, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  CHARACTER(LEN=2), INTENT(IN)    :: STR
  LOGICAL,          INTENT(OUT)   :: BVALIDOPERATION
  TYPE(HOOKS_T),    INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Check if the operation is valid
  SELECT CASE ( STR )
  CASE ( 'av', 'mn', 'mx', 'sd' )
    BVALIDOPERATION = .TRUE.
  CASE DEFAULT
    BVALIDOPERATION = .FALSE.
  END SELECT

  ! Trace end of procedure (on error)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN


END FUNCTION IS_VALID_OPERATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MONTH_TO_HOURS_JULIAN'
PP_THREAD_SAFE FUNCTION MONTH_TO_HOURS_JULIAN( &
&  YEAR, MONTH, HOURS, HOOKS ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,  ONLY: JPIB_K
  USE :: HOOKS_MOD,          ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: YEAR
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: MONTH     ! 1..12
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: HOURS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Locals
  INTEGER(KIND=JPIB_K) :: DAYS

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_MONTH = 1_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS


  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Check input
  PP_DEBUG_CRITICAL_COND_THROW( MONTH .LT. 1_JPIB_K,  ERRFLAG_INVALID_MONTH )
  PP_DEBUG_CRITICAL_COND_THROW( MONTH .GT. 12_JPIB_K, ERRFLAG_INVALID_MONTH )

  ! Compute days in month (Julian rule: leap if mod(year,4)==0)
  SELECT CASE ( MONTH )
  CASE ( 1_JPIB_K, 3_JPIB_K, 5_JPIB_K, 7_JPIB_K, 8_JPIB_K, 10_JPIB_K, 12_JPIB_K )
    DAYS = 31_JPIB_K
  CASE ( 4_JPIB_K, 6_JPIB_K, 9_JPIB_K, 11_JPIB_K )
    DAYS = 30_JPIB_K
  CASE ( 2_JPIB_K )
    IF (MOD(YEAR, 4_JPIB_K) == 0_JPIB_K) THEN
      DAYS = 29_JPIB_K
    ELSE
      DAYS = 28_JPIB_K
    END IF
  END SELECT

  HOURS = DAYS * 24_JPIB_K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE ( ERRFLAG_INVALID_MONTH )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'invalid month: must be in 1..12' )
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

END FUNCTION MONTH_TO_HOURS_JULIAN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMPUTE_OFFSET_DATETIME'
PP_THREAD_SAFE FUNCTION COMPUTE_OFFSET_DATETIME( &
& MSG, OFFSET_FROM_START_IN_SECONDS, DATE, TIME, HOOKS, &
& YEAR, MONTH, DAY, HOUR, MINUTE, SECOND ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPRB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: DATETIME_UTILS_MOD,  ONLY: PACK_YYYYMMDD
  USE :: DATETIME_UTILS_MOD,  ONLY: PACK_HHMMSS
  USE :: DATETIME_UTILS_MOD,  ONLY: PACK_HHMM
  USE :: DATETIME_UTILS_MOD,  ONLY: UNPACK_YYYYMMDD
  USE :: DATETIME_UTILS_MOD,  ONLY: UNPACK_HHMMSS
  USE :: DATETIME_UTILS_MOD,  ONLY: UNPACK_HHMM
  USE :: DATETIME_UTILS_MOD,  ONLY: SECONDS2DAYS

  !> Symbols imported from other libraries
  USE :: ECCODES, ONLY: CODES_DATETIME_TO_JULIAN
  USE :: ECCODES, ONLY: CODES_JULIAN_TO_DATETIME

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(FORTRAN_MESSAGE_T),        INTENT(IN)    :: MSG
  INTEGER(KIND=JPIB_K),           INTENT(IN)    :: OFFSET_FROM_START_IN_SECONDS
  INTEGER(KIND=JPIB_K),           INTENT(OUT)   :: DATE  ! YYYYMMDD
  INTEGER(KIND=JPIB_K),           INTENT(OUT)   :: TIME  ! HHMMSS
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: YEAR
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: MONTH
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: DAY
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: HOUR
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: MINUTE
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: SECOND

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: YEAR_LOC
  INTEGER(KIND=JPIB_K) :: MONTH_LOC
  INTEGER(KIND=JPIB_K) :: DAY_LOC
  INTEGER(KIND=JPIB_K) :: HOUR_LOC
  INTEGER(KIND=JPIB_K) :: MINUTE_LOC
  INTEGER(KIND=JPIB_K) :: SECOND_LOC
  INTEGER :: STAT
  REAL(KIND=JPRB_K) :: START_JULIAN_DATETIME
  REAL(KIND=JPRB_K) :: END_JULIAN_DATETIME

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COMPUTE_JULIAN = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COMPUTE_CURRENT_TIME = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PACK_DATE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PACK_TIME = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_UNPACK_DATE = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_UNPACK_TIME = 6_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Unpack start date/time
  PP_TRYCALL(ERRFLAG_UNABLE_TO_UNPACK_DATE) UNPACK_YYYYMMDD( MSG%DATE, YEAR_LOC, MONTH_LOC, DAY_LOC, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_UNPACK_TIME) UNPACK_HHMMSS( MSG%TIME, HOUR_LOC, MINUTE_LOC, SECOND_LOC, HOOKS )

  ! Compute current time
  CALL CODES_DATETIME_TO_JULIAN( &
&   YEAR_LOC, MONTH_LOC, DAY_LOC, HOUR_LOC, MINUTE_LOC, SECOND_LOC, START_JULIAN_DATETIME, STATUS=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT .NE. 0_JPIB_K, ERRFLAG_UNABLE_TO_COMPUTE_JULIAN )

  ! Compute current time in seconds
  END_JULIAN_DATETIME = START_JULIAN_DATETIME + REAL(OFFSET_FROM_START_IN_SECONDS, KIND=JPRB_K)*SECONDS2DAYS

  CALL CODES_JULIAN_TO_DATETIME( &
&   END_JULIAN_DATETIME, YEAR_LOC, MONTH_LOC, DAY_LOC, HOUR_LOC, MINUTE_LOC, SECOND_LOC, STATUS=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT .NE. 0_JPIB_K, ERRFLAG_UNABLE_TO_COMPUTE_CURRENT_TIME )

  ! Pack end date/time
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PACK_DATE) PACK_YYYYMMDD( YEAR_LOC, MONTH_LOC, DAY_LOC, DATE, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PACK_TIME) PACK_HHMMSS( HOUR_LOC, MINUTE_LOC, SECOND_LOC, TIME, HOOKS )

  ! Optionally return unpacked values
  IF ( PRESENT(YEAR) )    YEAR    = YEAR_LOC
  IF ( PRESENT(MONTH) )   MONTH   = MONTH_LOC
  IF ( PRESENT(DAY) )     DAY     = DAY_LOC
  IF ( PRESENT(HOUR) )    HOUR    = HOUR_LOC
  IF ( PRESENT(MINUTE) )  MINUTE  = MINUTE_LOC
  IF ( PRESENT(SECOND) )  SECOND  = SECOND_LOC

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE ( ERRFLAG_UNABLE_TO_COMPUTE_JULIAN )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to compute julian day' )
    CASE ( ERRFLAG_UNABLE_TO_COMPUTE_CURRENT_TIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to compute current time' )
    CASE ( ERRFLAG_UNABLE_TO_PACK_DATE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to pack date' )
    CASE ( ERRFLAG_UNABLE_TO_PACK_TIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to pack time' )
    CASE ( ERRFLAG_UNABLE_TO_UNPACK_DATE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to unpack date' )
    CASE ( ERRFLAG_UNABLE_TO_UNPACK_TIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to unpack time' )
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

END FUNCTION COMPUTE_OFFSET_DATETIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMPUTE_FORECAST_DATETIME'
PP_THREAD_SAFE FUNCTION COMPUTE_FORECAST_DATETIME( &
& MSG, FORECAST_DATE, FORECAST_TIME, HOOKS, &
& YEAR, MONTH, DAY, HOUR, MINUTE, SECOND ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPRB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: ENUMERATORS_MOD,     ONLY: TYPE_OF_PERIOD_MONTHLY_E
  USE :: ENUMERATORS_MOD,     ONLY: UNDEF_PARAM_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(FORTRAN_MESSAGE_T),        INTENT(IN)    :: MSG
  INTEGER(KIND=JPIB_K),           INTENT(OUT)   :: FORECAST_DATE  ! YYYYMMDD
  INTEGER(KIND=JPIB_K),           INTENT(OUT)   :: FORECAST_TIME  ! HHMMSS
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: YEAR
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: MONTH
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: DAY
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: HOUR
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: MINUTE
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: SECOND

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_TIMESPAN
  LOGICAL :: HAS_STATTYPE
  INTEGER(KIND=JPIB_K) :: OFFSET_IN_SECONDS
  INTEGER(KIND=JPIB_K) :: FYEAR
  INTEGER(KIND=JPIB_K) :: FMONTH
  INTEGER(KIND=JPIB_K) :: FDAY
  INTEGER(KIND=JPIB_K) :: FHOUR
  INTEGER(KIND=JPIB_K) :: FMINUTE
  INTEGER(KIND=JPIB_K) :: FSECOND
  INTEGER(KIND=JPIB_K) :: END_DATE
  INTEGER(KIND=JPIB_K) :: END_TIME
  INTEGER(KIND=JPIB_K) :: END_YEAR
  INTEGER(KIND=JPIB_K) :: END_MONTH
  INTEGER(KIND=JPIB_K) :: TMP

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNSUPPORTED_CASE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_COMPUTE_END_DATETIME = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_PERIOD_IN_HOURS = 3_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Check the cases
  HAS_TIMESPAN = .FALSE.
  HAS_STATTYPE = .FALSE.

  ! Check for timespan and stattype
  IF ( MSG%TIMESPAN .NE. UNDEF_PARAM_E ) THEN
    HAS_TIMESPAN = .TRUE.
  END IF

  IF ( MSG%STATTYPE .NE. REPEAT('*',32) ) THEN
    HAS_STATTYPE = .TRUE.
  END IF

  ! Compute the offset
  IF ( .NOT.HAS_TIMESPAN .AND. .NOT. HAS_STATTYPE ) THEN
    ! It is not a statistics, This should be an error forecast time equal to end time
    OFFSET_IN_SECONDS = MSG%STEP*3600_JPIB_K
  ELSEIF ( HAS_TIMESPAN .AND. .NOT. HAS_STATTYPE ) THEN
    ! This is the standard case of single loop statistics with timespan
    OFFSET_IN_SECONDS = MAX( (MSG%STEP - MSG%TIMESPAN)*3600_JPIB_K, 0_JPIB_K )
  ELSEIF ( HAS_TIMESPAN .AND. HAS_STATTYPE ) then
    IF ( MSG%STATTYPE(1:2) .EQ. 'mo' ) THEN
      ! Compute end date/time
      PP_TRYCALL(ERRFLAG_COMPUTE_END_DATETIME) &
&       COMPUTE_OFFSET_DATETIME( MSG, MSG%STEP*3600_JPIB_K, END_DATE, END_TIME, HOOKS, YEAR=END_YEAR, MONTH=END_MONTH )
      PP_TRYCALL(ERRFLAG_UNABLE_TO_GET_PERIOD_IN_HOURS) PERIOD_TO_HOURS( TYPE_OF_PERIOD_MONTHLY_E, TMP, END_YEAR, END_MONTH, HOOKS )
    ELSEIF ( MSG%STATTYPE(1:2) .EQ. 'da' ) THEN
      OFFSET_IN_SECONDS = MAX( (MSG%STEP - TMP)*3600_JPIB_K, 0_JPIB_K )
    ELSE
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNSUPPORTED_CASE )
    ENDIF
  ELSE
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNSUPPORTED_CASE )
  ENDIF

  ! TODO: in case of monthly statistics need to check.

  ! Compute forecast date/time
  PP_TRYCALL(ERRFLAG_COMPUTE_END_DATETIME) &
& COMPUTE_OFFSET_DATETIME( MSG, OFFSET_IN_SECONDS, FORECAST_DATE, FORECAST_TIME, HOOKS, &
& YEAR=FYEAR, MONTH=FMONTH, DAY=FDAY, HOUR=FHOUR, MINUTE=FMINUTE, SECOND=FSECOND )

  ! Handle optional outputs
  IF ( PRESENT(YEAR) )    YEAR    = FYEAR
  IF ( PRESENT(MONTH) )   MONTH   = FMONTH
  IF ( PRESENT(DAY) )     DAY     = FDAY
  IF ( PRESENT(HOUR) )    HOUR    = FHOUR
  IF ( PRESENT(MINUTE) )  MINUTE  = FMINUTE
  IF ( PRESENT(SECOND) )  SECOND  = FSECOND

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE ( ERRFLAG_UNSUPPORTED_CASE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unsupported case' )
    CASE ( ERRFLAG_COMPUTE_END_DATETIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to compute end date/time' )
    CASE ( ERRFLAG_UNABLE_TO_GET_PERIOD_IN_HOURS )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert period to hours' )
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

END FUNCTION COMPUTE_FORECAST_DATETIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'COMPUTE_END_DATETIME'
PP_THREAD_SAFE FUNCTION COMPUTE_END_DATETIME( &
& MSG, END_DATE, END_TIME, HOOKS, &
& YEAR, MONTH, DAY, HOUR, MINUTE, SECOND ) RESULT(RET)

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,   ONLY: JPRB_K
  USE :: HOOKS_MOD,           ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T
  USE :: ENUMERATORS_MOD,     ONLY: TYPE_OF_PERIOD_MONTHLY_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  TYPE(FORTRAN_MESSAGE_T),        INTENT(IN)    :: MSG
  INTEGER(KIND=JPIB_K),           INTENT(OUT)   :: END_DATE  ! YYYYMMDD
  INTEGER(KIND=JPIB_K),           INTENT(OUT)   :: END_TIME  ! HHMMSS
  TYPE(HOOKS_T),                  INTENT(INOUT) :: HOOKS
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: YEAR
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: MONTH
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: DAY
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: HOUR
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: MINUTE
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT)   :: SECOND

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: HAS_TIMESPAN
  LOGICAL :: HAS_STATTYPE
  INTEGER(KIND=JPIB_K) :: OFFSET_IN_SECONDS
  INTEGER(KIND=JPIB_K) :: EYEAR
  INTEGER(KIND=JPIB_K) :: EMONTH
  INTEGER(KIND=JPIB_K) :: EDAY
  INTEGER(KIND=JPIB_K) :: EHOUR
  INTEGER(KIND=JPIB_K) :: EMINUTE
  INTEGER(KIND=JPIB_K) :: ESECOND
  INTEGER(KIND=JPIB_K) :: TMP

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNSUPPORTED_CASE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_COMPUTE_END_DATETIME = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_GET_PERIOD_IN_HOURS = 3_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )

  ! Check the cases
  HAS_TIMESPAN = .FALSE.
  HAS_STATTYPE = .FALSE.

  ! End time
  OFFSET_IN_SECONDS = MSG%STEP*3600_JPIB_K

  ! Compute forecast date/time
  PP_TRYCALL(ERRFLAG_COMPUTE_END_DATETIME) &
& COMPUTE_OFFSET_DATETIME( MSG, OFFSET_IN_SECONDS, END_DATE, END_TIME, HOOKS, &
& YEAR=EYEAR, MONTH=EMONTH, DAY=EDAY, HOUR=EHOUR, MINUTE=EMINUTE, SECOND=ESECOND )

  ! Handle optional outputs
  IF ( PRESENT(YEAR) )    YEAR    = EYEAR
  IF ( PRESENT(MONTH) )   MONTH   = EMONTH
  IF ( PRESENT(DAY) )     DAY     = EDAY
  IF ( PRESENT(HOUR) )    HOUR    = EHOUR
  IF ( PRESENT(MINUTE) )  MINUTE  = EMINUTE
  IF ( PRESENT(SECOND) )  SECOND  = ESECOND

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
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
    CASE ( ERRFLAG_UNSUPPORTED_CASE )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unsupported case' )
    CASE ( ERRFLAG_COMPUTE_END_DATETIME )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to compute end date/time' )
    CASE ( ERRFLAG_UNABLE_TO_GET_PERIOD_IN_HOURS )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert period to hours' )
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

END FUNCTION COMPUTE_END_DATETIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE GRIB2_SECTION4_STATTYPE_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
