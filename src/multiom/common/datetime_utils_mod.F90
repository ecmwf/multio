!> @file
!>
!> @brief Module containing utilities for managing date and time.
!>
!> This module provides various utilities for handling date and time
!> operations.
!>
!> @author Mirco Valentini
!> @date   January 31, 2024
!>


! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'datetime_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'DATETIME_UTILS_MOD'
MODULE DATETIME_UTILS_MOD

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K

IMPLICIT NONE

! Default visibility
PRIVATE

!> Local paramters
INTEGER(KIND=JPIB_K), PARAMETER :: HOURS2SECONDS=3600_JPIB_K
INTEGER(KIND=JPIB_K), PARAMETER :: SECONDS_IN_DAY=86400_JPIB_K
REAL(KIND=JPRD_K), PARAMETER :: SECONDS2DAYS=1.0_JPRD_K/REAL(SECONDS_IN_DAY,KIND=JPRD_K)

! Whitelist of public symbols (General utils to convert formats)
! PUBLIC :: YYYY_MM_DD_TO_YYYYMMDD
! PUBLIC :: HH_MM_SS_TO_HHMMSS
! PUBLIC :: HH_MM_SS_TO_HHMM
! PUBLIC :: HH_MM_TO_HHMM

! PUBLIC :: YYYYMMDD_TO_YYYY_MM_DD
! PUBLIC :: HHMMSS_TO_HH_MM_SS
! PUBLIC :: HHMMSS_TO_HH_MM
! PUBLIC :: HHMM_TO_HH_MM

PUBLIC :: YYYYMMDD_HHMMSS_TO_DATETIME
PUBLIC :: UNPACK_YYYYMMDD
PUBLIC :: PACK_YYYYMMDD
PUBLIC :: SEC2DD_SS
PUBLIC :: SEC2HH_MM_SS
PUBLIC :: HH_MM_SS2SEC
PUBLIC :: PACK_HHMM
PUBLIC :: PACK_HHMMSS
PUBLIC :: UNPACK_HHMM
PUBLIC :: UNPACK_HHMMSS
PUBLIC :: DATE_SUB_DAYS
PUBLIC :: DATE_SUM_DAYS
PUBLIC :: HHMMSS2STRING
PUBLIC :: YYYYMMDD2STRING
! PUBLIC :: DATETIME_TO_YYYYMMDD_HHMMSS

! PUBLIC :: HH_MM_SS_TO_SECONDS
! PUBLIC :: SECONDS_TO_HH_MM_SS

! PUBLIC :: DD_HH_MM_SS_TO_SECONDS
! PUBLIC :: SECONDS_TO_DD_HH_MM_SS


! Whitelist of public symbols (General utils to sun/subtract date time)
! PUBLIC :: SUB_DATETIME
PUBLIC :: ADD_SECONDS_TO_DATETIME

! Whitelist of public symbols (parameters)
PUBLIC :: HOURS2SECONDS

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SEC2DD_SS'
PP_THREAD_SAFE FUNCTION SEC2DD_SS( NSSSSS, IDD, ISS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: NSSSSS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IDD
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: ISS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !< Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECONDS_LOWER_THAN_ZERO = 1_JPIB_K

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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( NSSSSS.LT.0, ERRFLAG_SECONDS_LOWER_THAN_ZERO )

  IDD = NSSSSS / SECONDS_IN_DAY
  ISS = MOD(NSSSSS, SECONDS_IN_DAY)

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_SECONDS_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Seconds lower than zero' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION SEC2DD_SS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SEC2HH_MM_SS'
PP_THREAD_SAFE FUNCTION SEC2HH_MM_SS( NSSSSS, IHH, IMM, ISS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: NSSSSS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IHH
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IMM
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: ISS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !< Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECONDS_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HOUR_OUT_OF_RANGE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MINUTE_OUT_OF_RANGE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECOND_OUT_OF_RANGE = 4_JPIB_K

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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( NSSSSS.LT.0, ERRFLAG_SECONDS_LOWER_THAN_ZERO )

  IHH = NSSSSS / 3600
  IMM = MOD(NSSSSS, 3600) / 60
  ISS = NSSSSS - IHH*3600 - IMM*60

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IHH.GT.23,   ERRFLAG_HOUR_OUT_OF_RANGE )
  PP_DEBUG_DEVELOP_COND_THROW( IHH.LT.0,    ERRFLAG_HOUR_OUT_OF_RANGE )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.GT.59,   ERRFLAG_MINUTE_OUT_OF_RANGE )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.LT.0,    ERRFLAG_MINUTE_OUT_OF_RANGE )
  PP_DEBUG_DEVELOP_COND_THROW( ISS.GT.59,   ERRFLAG_SECOND_OUT_OF_RANGE )
  PP_DEBUG_DEVELOP_COND_THROW( ISS.LT.0,    ERRFLAG_SECOND_OUT_OF_RANGE )


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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_HOUR_OUT_OF_RANGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Hour out of range' )
    CASE (ERRFLAG_MINUTE_OUT_OF_RANGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Minute out of range' )
    CASE (ERRFLAG_SECOND_OUT_OF_RANGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Second out of range' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION SEC2HH_MM_SS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HH_MM_SS2SEC'
PP_THREAD_SAFE FUNCTION HH_MM_SS2SEC( IHH, IMM, ISS, NSSSSS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IHH
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMM
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ISS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: NSSSSS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !< Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECONDS_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HOUR_OUT_OF_RANGE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MINUTE_OUT_OF_RANGE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECOND_OUT_OF_RANGE = 4_JPIB_K

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


  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IHH.GT.23,   ERRFLAG_HOUR_OUT_OF_RANGE )
  PP_DEBUG_DEVELOP_COND_THROW( IHH.LT.0,    ERRFLAG_HOUR_OUT_OF_RANGE )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.GT.59,   ERRFLAG_MINUTE_OUT_OF_RANGE )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.LT.0,    ERRFLAG_MINUTE_OUT_OF_RANGE )
  PP_DEBUG_DEVELOP_COND_THROW( ISS.GT.59,   ERRFLAG_SECOND_OUT_OF_RANGE )
  PP_DEBUG_DEVELOP_COND_THROW( ISS.LT.0,    ERRFLAG_SECOND_OUT_OF_RANGE )

  ! Convert Hours, Minutes and Seconds to seconds
  NSSSSS = IHH*3600 + IMM*60 + ISS

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( NSSSSS.LT.0, ERRFLAG_SECONDS_LOWER_THAN_ZERO )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_HOUR_OUT_OF_RANGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Hour out of range' )
    CASE (ERRFLAG_MINUTE_OUT_OF_RANGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Minute out of range' )
    CASE (ERRFLAG_SECOND_OUT_OF_RANGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Second out of range' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION HH_MM_SS2SEC
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'UNPACK_YYYYMMDD'
PP_THREAD_SAFE FUNCTION UNPACK_YYYYMMDD(  IYYYYMMDD, IYYYY, IMM, IDD, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IYYYYMMDD
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IYYYY
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IMM
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IDD
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Function result
  INTEGER(KIND=JPIB_K) :: DIM

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DATEPACK_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_YEAR_LOWER_THAN_ZERO = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MONTH_LOWER_THAN_ONE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MONTH_GREATER_THAN_TWELVE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DAY_LOWER_THAN_ONE = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DAY_GREATER_THAN_DAYS_IN_MONTH = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DAYS_IN_MONTH = 7_JPIB_K

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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IYYYYMMDD.LT.0, ERRFLAG_DATEPACK_LOWER_THAN_ZERO )

  ! Extract Year, Month and Day
  IDD   = MOD(IYYYYMMDD,100)
  IMM   = MOD(IYYYYMMDD/100,100)
  IYYYY = IYYYYMMDD/10000

  ! Comput the number of days in month
  PP_TRYCALL(ERRFLAG_DAYS_IN_MONTH) DAYS_IN_MONTH( IYYYY, IMM, DIM, HOOKS )

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IYYYY.LT.0, ERRFLAG_YEAR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.LT.1,   ERRFLAG_MONTH_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.GT.12,  ERRFLAG_MONTH_GREATER_THAN_TWELVE )
  PP_DEBUG_DEVELOP_COND_THROW( IDD.LT.1,   ERRFLAG_DAY_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( IDD.GT.DIM, ERRFLAG_DAY_GREATER_THAN_DAYS_IN_MONTH )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_DATEPACK_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Packed date invalid. PAcked date lower than 0' )
    CASE (ERRFLAG_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Year out of range. Year lower than 0' )
    CASE (ERRFLAG_MONTH_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Month out of range. Month index lower than 1' )
    CASE (ERRFLAG_MONTH_GREATER_THAN_TWELVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Month out of range. Month index higher than 12' )
    CASE (ERRFLAG_DAY_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Day out of range. Day index lower than 1' )
    CASE (ERRFLAG_DAY_GREATER_THAN_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Day out of range. Day index higher than the number of days in month' )
    CASE (ERRFLAG_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to compute the number of days in the month' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION UNPACK_YYYYMMDD
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PACK_YYYYMMDD'
FUNCTION PACK_YYYYMMDD( IYYYY, IMM, IDD, IYYYYMMDD, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IYYYY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMM
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IDD
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IYYYYMMDD
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: DIM

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_YEAR_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MONTH_LOWER_THAN_ONE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MONTH_GREATER_THAN_TWELVE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DAY_LOWER_THAN_ONE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DAY_GREATER_THAN_DAYS_IN_MONTH = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COMPUTE_DAYS_IN_MONTH = 6_JPIB_K

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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IYYYY.LT.0, ERRFLAG_YEAR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.LT.1,   ERRFLAG_MONTH_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.GT.12,  ERRFLAG_MONTH_GREATER_THAN_TWELVE )
  PP_DEBUG_DEVELOP_COND_THROW( IDD.LT.1,   ERRFLAG_DAY_LOWER_THAN_ONE )

  PP_TRYCALL( ERRFLAG_UNABLE_TO_COMPUTE_DAYS_IN_MONTH ) DAYS_IN_MONTH( IYYYY, IMM, DIM, HOOKS )
  PP_DEBUG_DEVELOP_COND_THROW( IDD.GT.DIM, ERRFLAG_DAY_GREATER_THAN_DAYS_IN_MONTH )

  ! Pack Year, Month and Day
  IYYYYMMDD = IYYYY*10000 + IMM*100 + IDD

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Year out of range. Year lower than 0' )
    CASE (ERRFLAG_MONTH_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Month out of range. Month index lower than 1' )
    CASE (ERRFLAG_MONTH_GREATER_THAN_TWELVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Month out of range. Month index higher than 12' )
    CASE (ERRFLAG_DAY_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Day out of range. Day index lower than 1' )
    CASE (ERRFLAG_DAY_GREATER_THAN_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Day out of range. Day index higher than the number of days in month' )
    CASE (ERRFLAG_UNABLE_TO_COMPUTE_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to compute the number of days in the month' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION PACK_YYYYMMDD
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YYYYMMDD2STRING'
FUNCTION YYYYMMDD2STRING( IYYYYMMDD, STR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IYYYYMMDD
  CHARACTER(LEN=10),    INTENT(OUT)   :: STR
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: DIM
  INTEGER(KIND=JPIB_K) :: YYYY
  INTEGER(KIND=JPIB_K) :: MN
  INTEGER(KIND=JPIB_K) :: DD

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNPACK = 0_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_YEAR_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MONTH_LOWER_THAN_ONE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MONTH_GREATER_THAN_TWELVE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DAY_LOWER_THAN_ONE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DAY_GREATER_THAN_DAYS_IN_MONTH = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COMPUTE_DAYS_IN_MONTH = 6_JPIB_K

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

  PP_TRYCALL(ERRFLAG_UNPACK) UNPACK_YYYYMMDD( IYYYYMMDD, YYYY, MN, DD, HOOKS )

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( YYYY.LT.0, ERRFLAG_YEAR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( MN.LT.1,   ERRFLAG_MONTH_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( MN.GT.12,  ERRFLAG_MONTH_GREATER_THAN_TWELVE )
  PP_DEBUG_DEVELOP_COND_THROW( DD.LT.1,   ERRFLAG_DAY_LOWER_THAN_ONE )

  PP_TRYCALL( ERRFLAG_UNABLE_TO_COMPUTE_DAYS_IN_MONTH ) DAYS_IN_MONTH( YYYY, MN, DIM, HOOKS )
  PP_DEBUG_DEVELOP_COND_THROW( DD.GT.DIM, ERRFLAG_DAY_GREATER_THAN_DAYS_IN_MONTH )

  ! Pack Year, Month and Day
  STR = REPEAT(' ',10)
  WRITE(STR,'(I4.4,A1,I2.2,A1,I2.2)') YYYY, '-', MN, '-', DD

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNPACK)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to unpack the date' )
    CASE (ERRFLAG_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Year out of range. Year lower than 0' )
    CASE (ERRFLAG_MONTH_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Month out of range. Month index lower than 1' )
    CASE (ERRFLAG_MONTH_GREATER_THAN_TWELVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Month out of range. Month index higher than 12' )
    CASE (ERRFLAG_DAY_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Day out of range. Day index lower than 1' )
    CASE (ERRFLAG_DAY_GREATER_THAN_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Day out of range. Day index higher than the number of days in month' )
    CASE (ERRFLAG_UNABLE_TO_COMPUTE_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to compute the number of days in the month' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION YYYYMMDD2STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HHMMSS2STRING'
FUNCTION HHMMSS2STRING( IHHMMSS, STR, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IHHMMSS
  CHARACTER(LEN=8),     INTENT(OUT)   :: STR
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_ERROR
  INTEGER(KIND=JPIB_K) :: HH
  INTEGER(KIND=JPIB_K) :: MM
  INTEGER(KIND=JPIB_K) :: SS

 !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIMEPACK_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HOUR_GREATER_THAN_23 = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNPACK = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_WRITE_ERROR = 4_JPIB_K

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

  PP_DEBUG_CRITICAL_COND_THROW( IHHMMSS.LT.0, ERRFLAG_TIMEPACK_LOWER_THAN_ZERO )

  PP_TRYCALL(ERRFLAG_UNPACK) UNPACK_HHMMSS( IHHMMSS, HH, MM, SS, HOOKS )

  PP_DEBUG_CRITICAL_COND_THROW( HH.GT.23, ERRFLAG_HOUR_GREATER_THAN_23 )


  ! Pack Year, Month and Day
  STR = REPEAT(' ',8)
  WRITE(STR,'(I2.2,A1,I2.2,A1,I2.2)',IOSTAT=WRITE_ERROR) HH, ':', MM, ':', SS
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_ERROR.NE.0, ERRFLAG_WRITE_ERROR )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_TIMEPACK_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Packed time invalid. Packed time lower than 0' )
    CASE (ERRFLAG_HOUR_GREATER_THAN_23)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Hour out of range. Hour index higher than 23' )
    CASE (ERRFLAG_UNPACK)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to unpack the time' )
    CASE (ERRFLAG_WRITE_ERROR)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error writing the time to the string' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION HHMMSS2STRING
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PACK_HHMM'
FUNCTION PACK_HHMM( IHH, IMM, IHHMM, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IHH
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMM
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IHHMM
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HOUR_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HOUR_GREATER_THAN_TWENTYTHREE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MINUTE_LOWER_THAN_ZERO = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MINUTE_GREATER_THAN_FIFTYNINE = 4_JPIB_K

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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IHH.LT.0,  ERRFLAG_HOUR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IHH.GT.23, ERRFLAG_HOUR_GREATER_THAN_TWENTYTHREE )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.LT.0,  ERRFLAG_MINUTE_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.GT.59, ERRFLAG_MINUTE_GREATER_THAN_FIFTYNINE )

  ! Pack Hours and minutes
  IHHMM = IHH*100 + IMM

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_HOUR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Hour out of range. Index of hour lower than 0' )
    CASE (ERRFLAG_HOUR_GREATER_THAN_TWENTYTHREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Hour out of range. Index of hour higher than 23' )
    CASE (ERRFLAG_MINUTE_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Minute out of range. Index of minute lower than 0' )
    CASE (ERRFLAG_MINUTE_GREATER_THAN_FIFTYNINE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Minute out of range. Index of minute higher than 59' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION PACK_HHMM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Unpack year, month and day.
!>
!> This function is used to unpack year, month and day into three
!> separate integers.
!>
!> @param [in]  hhmmsss  Packed time to be extracted
!> @param [out] hh       Hours computed from the packed time
!> @param [out] mm       Minutes computed from the packed time
!> @param [out] ss       Seconds computed from the packed time
!> @param [inout] hooks  Hooks for logging and error handling, tracing, logging etc.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'UNPACK_HHMM'
PP_THREAD_SAFE FUNCTION UNPACK_HHMM(  HHMM, HH, MM, HOOKS ) RESULT(RET)

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

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: HHMM
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: HH
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: MM
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIMEPACK_LOWER_THAN_ZERO = 1_JPIB_K
!  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIMEPACK_OUTOFBOUNDS = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HOUR_LOWER_THAN_ZERO = 3_JPIB_K
!  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HOUR_GREATER_THAN_23 = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MINUTE_LOWER_THAN_ZERO = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MINUTE_GREATER_THAN_SIXTY = 6_JPIB_K

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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( HHMM.LT.0_JPIB_K, ERRFLAG_TIMEPACK_LOWER_THAN_ZERO )
  ! PP_DEBUG_DEVELOP_COND_THROW( HHMM.GT.235959_JPIB_K, ERRFLAG_TIMEPACK_OUTOFBOUNDS )


  ! Extract Year, Month and Day
  MM = MOD(HHMM,100_JPIB_K)
  HH = HHMM/100_JPIB_K

  ! Compute days in month
  !! TODO:  PP_TRYCALL(ERRFLAG_UNABLE_TO_COMPUTE_DAYS_IN_MONTH) DAYS_IN_MONTH( YYYY, MM, DIM, HOOKS )

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( HH.LT.0,   ERRFLAG_HOUR_LOWER_THAN_ZERO )
  ! PP_DEBUG_DEVELOP_COND_THROW( HH.GT.23,  ERRFLAG_HOUR_GREATER_THAN_23 )
  PP_DEBUG_DEVELOP_COND_THROW( MM.LT.0,   ERRFLAG_MINUTE_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( MM.GT.59,  ERRFLAG_MINUTE_GREATER_THAN_SIXTY )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_TIMEPACK_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Packed time invalid. Packed time lower than 0' )
!    CASE (ERRFLAG_TIMEPACK_OUTOFBOUNDS)
!      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Packed time invalid. Packed time higher than 235959' )
    CASE (ERRFLAG_HOUR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Hour out of range. Hour index lower than 0' )
!    CASE (ERRFLAG_HOUR_GREATER_THAN_23)
!      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Hour out of range. Hour index higher than 23' )
    CASE (ERRFLAG_MINUTE_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Minute out of range. Minute index lower than 0' )
    CASE (ERRFLAG_MINUTE_GREATER_THAN_SIXTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Minute out of range. Minute index higher than 59' )
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

END FUNCTION UNPACK_HHMM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PACK_HHMMSS'
FUNCTION PACK_HHMMSS( IHH, IMM, ISS, IHHMMSS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IHH
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMM
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ISS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IHHMMSS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HOUR_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HOUR_GREATER_THAN_TWENTYTHREE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MINUTE_LOWER_THAN_ZERO = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MINUTE_GREATER_THAN_FIFTYNINE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECONDS_LOWER_THAN_ZERO = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECONDS_GREATER_THAN_FIFTYNINE = 6_JPIB_K

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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IHH.LT.0,  ERRFLAG_HOUR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IHH.GT.23, ERRFLAG_HOUR_GREATER_THAN_TWENTYTHREE )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.LT.0,  ERRFLAG_MINUTE_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.GT.59, ERRFLAG_MINUTE_GREATER_THAN_FIFTYNINE )
  PP_DEBUG_DEVELOP_COND_THROW( ISS.LT.0,  ERRFLAG_SECONDS_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( ISS.GT.59, ERRFLAG_SECONDS_GREATER_THAN_FIFTYNINE )

  ! Pack Hours and minutes
  IHHMMSS = IHH*10000_JPIB_K + IMM*100_JPIB_K + ISS

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_HOUR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Hour out of range. Index of hour lower than 0' )
    CASE (ERRFLAG_HOUR_GREATER_THAN_TWENTYTHREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Hour out of range. Index of hour higher than 23' )
    CASE (ERRFLAG_MINUTE_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Minute out of range. Index of minute lower than 0' )
    CASE (ERRFLAG_MINUTE_GREATER_THAN_FIFTYNINE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Minute out of range. Index of minute higher than 59' )
    CASE (ERRFLAG_SECONDS_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Seconds out of range. Index of seconds lower than 0' )
    CASE (ERRFLAG_SECONDS_GREATER_THAN_FIFTYNINE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Seconds out of range. Index of seconds higher than 59' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION PACK_HHMMSS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Unpack year, month and day.
!>
!> This function is used to unpack year, month and day into three
!> separate integers.
!>
!> @param [in]  hhmmsss  Packed time to be extracted
!> @param [out] hh       Hours computed from the packed time
!> @param [out] mm       Minutes computed from the packed time
!> @param [out] ss       Seconds computed from the packed time
!> @param [inout] hooks  Hooks for logging and error handling, tracing, logging etc.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'UNPACK_HHMMSS'
PP_THREAD_SAFE FUNCTION UNPACK_HHMMSS(  HHMMSS, HH, MM, SS, HOOKS ) RESULT(RET)

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

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: HHMMSS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: HH
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: MM
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: SS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIMEPACK_LOWER_THAN_ZERO = 1_JPIB_K
  ! INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_TIMEPACK_OUTOFBOUNDS = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HOUR_LOWER_THAN_ZERO = 3_JPIB_K
  ! INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HOUR_GREATER_THAN_23 = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MINUTE_LOWER_THAN_ZERO = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MINUTE_GREATER_THAN_SIXTY = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECOND_LOWER_THAN_ZERO = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECOND_GREATER_THAN_SIXTY = 8_JPIB_K

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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( HHMMSS.LT.0_JPIB_K, ERRFLAG_TIMEPACK_LOWER_THAN_ZERO )
!  PP_DEBUG_DEVELOP_COND_THROW( HHMMSS.GT.235959_JPIB_K, ERRFLAG_TIMEPACK_OUTOFBOUNDS )


  ! Extract Year, Month and Day
  SS = MOD(HHMMSS,100_JPIB_K)
  MM = MOD(HHMMSS/100_JPIB_K,100_JPIB_K)
  HH = HHMMSS/10000_JPIB_K

  ! Compute days in month
  !! TODO:  PP_TRYCALL(ERRFLAG_UNABLE_TO_COMPUTE_DAYS_IN_MONTH) DAYS_IN_MONTH( YYYY, MM, DIM, HOOKS )

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( HH.LT.0,   ERRFLAG_HOUR_LOWER_THAN_ZERO )
  ! PP_DEBUG_DEVELOP_COND_THROW( HH.GT.23,  ERRFLAG_HOUR_GREATER_THAN_23 )
  PP_DEBUG_DEVELOP_COND_THROW( MM.LT.0,   ERRFLAG_MINUTE_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( MM.GT.59,  ERRFLAG_MINUTE_GREATER_THAN_SIXTY )
  PP_DEBUG_DEVELOP_COND_THROW( SS.LT.0,   ERRFLAG_SECOND_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( SS.GT.59,  ERRFLAG_SECOND_GREATER_THAN_SIXTY )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_TIMEPACK_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Packed time invalid. Packed time lower than 0' )
!    CASE (ERRFLAG_TIMEPACK_OUTOFBOUNDS)
!      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Packed time invalid. Packed time higher than 235959' )
    CASE (ERRFLAG_HOUR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Hour out of range. Hour index lower than 0' )
!    CASE (ERRFLAG_HOUR_GREATER_THAN_23)
!      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Hour out of range. Hour index higher than 23' )
    CASE (ERRFLAG_MINUTE_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Minute out of range. Minute index lower than 0' )
    CASE (ERRFLAG_MINUTE_GREATER_THAN_SIXTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Minute out of range. Minute index higher than 59' )
    CASE (ERRFLAG_SECOND_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Second out of range. Second index lower than 0' )
    CASE (ERRFLAG_SECOND_GREATER_THAN_SIXTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Second out of range. Second index higher than 59' )
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

END FUNCTION UNPACK_HHMMSS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DAYS_IN_MONTH'
PP_THREAD_SAFE FUNCTION DAYS_IN_MONTH( IYYYY, IMM, NDAYS, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IYYYY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IMM
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: NDAYS
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: IS_LEAP

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_YEAR_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MONTH_LOWER_THAN_ONE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MONTH_GREATER_THAN_TWELVE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FALLBACK = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CHECK_LEAP = 5_JPIB_K

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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IYYYY.LT.0, ERRFLAG_YEAR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.LT.1,   ERRFLAG_MONTH_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.GT.12,  ERRFLAG_MONTH_GREATER_THAN_TWELVE )

  SELECT CASE ( IMM )

  CASE (1,3,5,7,8,10,12)
    ! 31 days months
    NDAYS = 31

  CASE (4,6,9,11)
    ! 30 days months
    NDAYS = 30

  CASE (2)
    ! Days of February
    PP_TRYCALL( ERRFLAG_UNABLE_TO_CHECK_LEAP ) IS_LEAP_YEAR( IYYYY, IS_LEAP, HOOKS )
    IF ( IS_LEAP ) THEN
      NDAYS = 29
    ELSE
      NDAYS = 28
    ENDIF
  CASE DEFAULT
    PP_DEBUG_DEVELOP_THROW( ERRFLAG_FALLBACK )
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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Year out of range. Year lower than 0' )
    CASE (ERRFLAG_MONTH_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Month out of range. Month index lower than 1' )
    CASE (ERRFLAG_MONTH_GREATER_THAN_TWELVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Month out of range. Month index higher than 12' )
    CASE (ERRFLAG_FALLBACK)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Month out of range. Fallback error' )
    CASE (ERRFLAG_UNABLE_TO_CHECK_LEAP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check if the year is a leap year' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION DAYS_IN_MONTH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DAYS_IN_MONTHS'
PP_THREAD_SAFE FUNCTION DAYS_IN_MONTHS( IYYYY, IDIM, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: IYYYY
  INTEGER(KIND=JPIB_K), DIMENSION(12), INTENT(OUT)   :: IDIM
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: IS_LEAP

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_YEAR_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CHECK_LEAP = 2_JPIB_K

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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IYYYY.LT.0, ERRFLAG_YEAR_LOWER_THAN_ZERO )

  ! 31 days months
  IDIM(1)  = 31
  IDIM(3)  = 31
  IDIM(5)  = 31
  IDIM(7)  = 31
  IDIM(8)  = 31
  IDIM(10) = 31
  IDIM(12) = 31

  ! 30 days months
  IDIM(4)  = 30
  IDIM(6)  = 30
  IDIM(9)  = 30
  IDIM(11) = 30

  ! Days of February
  PP_TRYCALL( ERRFLAG_UNABLE_TO_CHECK_LEAP ) IS_LEAP_YEAR( IYYYY, IS_LEAP, HOOKS )
  IF ( IS_LEAP ) THEN
    IDIM(2) = 29
  ELSE
    IDIM(2) = 28
  ENDIF

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Year out of range. Year lower than 0' )
    CASE (ERRFLAG_UNABLE_TO_CHECK_LEAP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check if the year is a leap year' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION DAYS_IN_MONTHS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'UPDATE_DAYS_IN_MONTHS'
PP_THREAD_SAFE FUNCTION UPDATE_DAYS_IN_MONTHS( IYYYY, IDIM, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: IYYYY
  INTEGER(KIND=JPIB_K), DIMENSION(12), INTENT(INOUT) :: IDIM
  TYPE(HOOKS_T),                       INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  LOGICAL :: IS_LEAP

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_YEAR_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CHECK_LEAP = 2_JPIB_K

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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IYYYY.LT.0, 1 )

  ! Days of February
  PP_TRYCALL( ERRFLAG_UNABLE_TO_CHECK_LEAP ) IS_LEAP_YEAR( IYYYY, IS_LEAP, HOOKS )
  IF ( IS_LEAP ) THEN
    IDIM(2) = 29
  ELSE
    IDIM(2) = 28
  ENDIF

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Year out of range. Year lower than 0' )
    CASE (ERRFLAG_UNABLE_TO_CHECK_LEAP)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to check if the year is a leap year' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION UPDATE_DAYS_IN_MONTHS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'IS_LEAP_YEAR'
PP_THREAD_SAFE FUNCTION IS_LEAP_YEAR( IYYYY, LY, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IYYYY
  LOGICAL,              INTENT(OUT)   :: LY
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL, DIMENSION(4) :: LEAP_CONDITIONS

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_YEAR_LOWER_THAN_ZERO = 1_JPIB_K

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
  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IYYYY.LT.0, ERRFLAG_YEAR_LOWER_THAN_ZERO )

  ! Conditions to have a leap-year
  LEAP_CONDITIONS(1) = (MOD(IYYYY,4) .EQ. 0)
  LEAP_CONDITIONS(2) = (MOD(IYYYY,400) .NE. 100)
  LEAP_CONDITIONS(3) = (MOD(IYYYY,400) .NE. 200)
  LEAP_CONDITIONS(4) = (MOD(IYYYY,400) .NE. 300)

  ! Compute the return value
  LY = ALL(LEAP_CONDITIONS)

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Year out of range. Year lower than 0' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION IS_LEAP_YEAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DATE_UPDATE'
PP_THREAD_SAFE FUNCTION DATE_UPDATE( IIYYYY, IIMM, IIDD, IDELTA_DAYS, IOYYYY, IOMM, IODD, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IIYYYY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IIMM
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IIDD
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IDELTA_DAYS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IOYYYY
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IOMM
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IODD
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  INTEGER(KIND=JPIB_K) :: IIDIM
  INTEGER(KIND=JPIB_K) :: IODIM

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_YEAR_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_MONTH_LOWER_THAN_ONE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_MONTH_GREATER_THAN_TWELVE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_DAY_LOWER_THAN_ONE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_DAY_GREATER_THAN_DAYS_IN_MONTH = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_YEAR_LOWER_THAN_ZERO = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_MONTH_LOWER_THAN_ONE = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_MONTH_GREATER_THAN_TWELVE = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_DAY_LOWER_THAN_ONE = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_DAY_GREATER_THAN_DAYS_IN_MONTH = 10_JPIB_K

  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SUM_DAYS = 11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SUB_DAYS = 12_JPIB_K

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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IIYYYY.LT.0, ERRFLAG_INPUT_YEAR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IIMM.LT.1,   ERRFLAG_INPUT_MONTH_GREATER_THAN_TWELVE )
  PP_DEBUG_DEVELOP_COND_THROW( IIMM.GT.12,  ERRFLAG_INPUT_DAY_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( IIDD.LT.1,   ERRFLAG_INPUT_DAY_GREATER_THAN_DAYS_IN_MONTH )

  ! Compute days in month
  PP_TRYCALL( ERRFLAG_INPUT_DAY_GREATER_THAN_DAYS_IN_MONTH ) DAYS_IN_MONTH( IIYYYY, IIMM, IIDIM, HOOKS )
  PP_DEBUG_DEVELOP_COND_THROW( IIDD.GT.IIDIM, ERRFLAG_INPUT_DAY_GREATER_THAN_DAYS_IN_MONTH )

  IF ( IDELTA_DAYS .GE. 0 ) THEN
    PP_TRYCALL(ERRFLAG_SUM_DAYS) DATE_SUM_DAYS( IIYYYY, IIMM, IIDD, IDELTA_DAYS, IOYYYY, IOMM, IODD, HOOKS )
  ELSE
    PP_TRYCALL(ERRFLAG_SUB_DAYS) DATE_SUB_DAYS( IIYYYY, IIMM, IIDD, -IDELTA_DAYS, IOYYYY, IOMM, IODD, HOOKS )
  ENDIF

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IOYYYY.LT.0, ERRFLAG_OUTPUT_YEAR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IOMM.LT.1,   ERRFLAG_OUTPUT_MONTH_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( IOMM.GT.12,  ERRFLAG_OUTPUT_MONTH_GREATER_THAN_TWELVE )
  PP_DEBUG_DEVELOP_COND_THROW( IODD.LT.1,   ERRFLAG_OUTPUT_DAY_LOWER_THAN_ONE )

  ! Compute days in month
  PP_TRYCALL( ERRFLAG_OUTPUT_DAY_GREATER_THAN_DAYS_IN_MONTH ) DAYS_IN_MONTH( IOYYYY, IOMM, IODIM, HOOKS )
  PP_DEBUG_DEVELOP_COND_THROW( IODD.GT.IODIM, ERRFLAG_OUTPUT_DAY_GREATER_THAN_DAYS_IN_MONTH )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_INPUT_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Year out of range. Year lower than 0' )
    CASE (ERRFLAG_INPUT_MONTH_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Month out of range. Month index lower than 1' )
    CASE (ERRFLAG_INPUT_MONTH_GREATER_THAN_TWELVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Month out of range. Month index higher than 12' )
    CASE (ERRFLAG_INPUT_DAY_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Day out of range. Day index lower than 1' )
    CASE (ERRFLAG_INPUT_DAY_GREATER_THAN_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Day out of range. Day index higher than the number of days in month' )
    CASE (ERRFLAG_OUTPUT_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Year out of range. Year lower than 0' )
    CASE (ERRFLAG_OUTPUT_MONTH_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Month out of range. Month index lower than 1' )
    CASE (ERRFLAG_OUTPUT_MONTH_GREATER_THAN_TWELVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Month out of range. Month index higher than 12' )
    CASE (ERRFLAG_OUTPUT_DAY_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Day out of range. Day index lower than 1' )
    CASE (ERRFLAG_OUTPUT_DAY_GREATER_THAN_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Day out of range. Day index higher than the number of days in month' )
    CASE (ERRFLAG_SUM_DAYS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to sum days' )
    CASE (ERRFLAG_SUB_DAYS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to subtract days' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION DATE_UPDATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DATE_SUM_DAYS'
PP_THREAD_SAFE FUNCTION DATE_SUM_DAYS( IIYYYY, IIMM, IIDD, IDELTA_DAYS, IOYYYY, IOMM, IODD, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IIYYYY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IIMM
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IIDD
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IDELTA_DAYS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IOYYYY
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IOMM
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IODD
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: JD
  INTEGER(KIND=JPIB_K) :: DIM
  INTEGER(KIND=JPIB_K), DIMENSION(12) :: KLM0

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DELTA_DAYS_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_YEAR_LOWER_THAN_ZERO = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_MONTH_LOWER_THAN_ONE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_MONTH_GREATER_THAN_TWELVE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_DAY_LOWER_THAN_ONE = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_DAY_GREATER_THAN_DAYS_IN_MONTH = 6_JPIB_K

  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_YEAR_LOWER_THAN_ZERO = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_MONTH_LOWER_THAN_ONE = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_MONTH_GREATER_THAN_TWELVE = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_DAY_LOWER_THAN_ONE = 10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_DAY_GREATER_THAN_DAYS_IN_MONTH = 11_JPIB_K

  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_UPDATE_DAYS_IN_MONTH = 12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_COMPUTE_DAYS_IN_MONTH = 13_JPIB_K



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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IDELTA_DAYS.LT.0, ERRFLAG_DELTA_DAYS_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IIYYYY.LT.0,      ERRFLAG_INPUT_YEAR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IIMM.LT.1,        ERRFLAG_INPUT_MONTH_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( IIMM.GT.12,       ERRFLAG_INPUT_MONTH_GREATER_THAN_TWELVE )
  PP_DEBUG_DEVELOP_COND_THROW( IIDD.LT.1,        ERRFLAG_INPUT_DAY_LOWER_THAN_ONE )


  PP_TRYCALL(ERRFLAG_COMPUTE_DAYS_IN_MONTH) DAYS_IN_MONTH( IIYYYY, IIMM, DIM, HOOKS )
  PP_DEBUG_DEVELOP_COND_THROW( IIDD.GT.DIM, ERRFLAG_INPUT_DAY_GREATER_THAN_DAYS_IN_MONTH )

  ! Initialization
  PP_TRYCALL(ERRFLAG_COMPUTE_DAYS_IN_MONTH) DAYS_IN_MONTHS( IIYYYY, KLM0, HOOKS )
  IODD   = IIDD
  IOMM   = IIMM
  IOYYYY = IIYYYY

  DO JD = 1, IDELTA_DAYS
    IODD = IODD + 1
    IF ( IODD .GT. KLM0( IOMM ) ) THEN
      IODD = 1
      IOMM = IOMM + 1
      IF( IOMM .GT. 12 ) THEN
        IOMM = 1
        IOYYYY = IOYYYY + 1
        PP_TRYCALL(ERRFLAG_UNABLE_TO_UPDATE_DAYS_IN_MONTH) UPDATE_DAYS_IN_MONTHS( IOYYYY, KLM0, HOOKS )
      ENDIF
    ENDIF
  ENDDO

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IOYYYY.LT.0, ERRFLAG_OUTPUT_YEAR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IOMM.LT.1,   ERRFLAG_OUTPUT_MONTH_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( IOMM.GT.12,  ERRFLAG_OUTPUT_MONTH_GREATER_THAN_TWELVE )
  PP_DEBUG_DEVELOP_COND_THROW( IODD.LT.1,   ERRFLAG_OUTPUT_DAY_LOWER_THAN_ONE )

  PP_TRYCALL(ERRFLAG_COMPUTE_DAYS_IN_MONTH) DAYS_IN_MONTH( IOYYYY, IOMM, DIM, HOOKS )
  PP_DEBUG_DEVELOP_COND_THROW( IODD.GT.DIM, ERRFLAG_OUTPUT_DAY_GREATER_THAN_DAYS_IN_MONTH )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_DELTA_DAYS_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Delta days lower than 0' )
    CASE (ERRFLAG_INPUT_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Year out of range. Year lower than 0' )
    CASE (ERRFLAG_INPUT_MONTH_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Month out of range. Month index lower than 1' )
    CASE (ERRFLAG_INPUT_MONTH_GREATER_THAN_TWELVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Month out of range. Month index higher than 12' )
    CASE (ERRFLAG_INPUT_DAY_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Day out of range. Day index lower than 1' )
    CASE (ERRFLAG_INPUT_DAY_GREATER_THAN_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Day out of range. Day index higher than the number of days in month' )
    CASE (ERRFLAG_OUTPUT_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Year out of range. Year lower than 0' )
    CASE (ERRFLAG_OUTPUT_MONTH_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Month out of range. Month index lower than 1' )
    CASE (ERRFLAG_OUTPUT_MONTH_GREATER_THAN_TWELVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Month out of range. Month index higher than 12' )
    CASE (ERRFLAG_OUTPUT_DAY_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Day out of range. Day index lower than 1' )
    CASE (ERRFLAG_OUTPUT_DAY_GREATER_THAN_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Day out of range. Day index higher than the number of days in month' )
    CASE (ERRFLAG_UNABLE_TO_UPDATE_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to update days in month' )
    CASE (ERRFLAG_COMPUTE_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to compute days in month' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION DATE_SUM_DAYS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DATE_SUB_DAYS'
PP_THREAD_SAFE FUNCTION DATE_SUB_DAYS( IIYYYY, IIMM, IIDD, IDELTA_DAYS, IOYYYY, IOMM, IODD, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IIYYYY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IIMM
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IIDD
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: IDELTA_DAYS
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IOYYYY
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IOMM
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: IODD
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET


  ! Local variables
  INTEGER(KIND=JPIB_K) :: JD
  INTEGER(KIND=JPIB_K) :: DIM
  INTEGER(KIND=JPIB_K), DIMENSION(12) :: KLM0

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DELTA_DAYS_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_YEAR_LOWER_THAN_ZERO = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_MONTH_LOWER_THAN_ONE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_MONTH_GREATER_THAN_TWELVE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_DAY_LOWER_THAN_ONE = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INPUT_DAY_GREATER_THAN_DAYS_IN_MONTH = 6_JPIB_K

  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_YEAR_LOWER_THAN_ZERO = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_MONTH_LOWER_THAN_ONE = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_MONTH_GREATER_THAN_TWELVE = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_DAY_LOWER_THAN_ONE = 10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_OUTPUT_DAY_GREATER_THAN_DAYS_IN_MONTH = 11_JPIB_K

  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_UPDATE_DAYS_IN_MONTH = 12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_COMPUTE_DAYS_IN_MONTH = 13_JPIB_K


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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IDELTA_DAYS.LT.0, ERRFLAG_DELTA_DAYS_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IIYYYY.LT.0,      ERRFLAG_INPUT_YEAR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IIMM.LT.1,        ERRFLAG_INPUT_MONTH_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( IIMM.GT.12,       ERRFLAG_INPUT_MONTH_GREATER_THAN_TWELVE )
  PP_DEBUG_DEVELOP_COND_THROW( IIDD.LT.1,        ERRFLAG_INPUT_DAY_LOWER_THAN_ONE )


  PP_TRYCALL(ERRFLAG_COMPUTE_DAYS_IN_MONTH) DAYS_IN_MONTH( IIYYYY, IIMM, DIM, HOOKS )
  PP_DEBUG_DEVELOP_COND_THROW( IIDD.GT.DIM, ERRFLAG_INPUT_DAY_GREATER_THAN_DAYS_IN_MONTH )

  ! Initialization
  PP_TRYCALL(ERRFLAG_COMPUTE_DAYS_IN_MONTH) DAYS_IN_MONTHS( IIYYYY, KLM0, HOOKS )
  IODD   = IIDD
  IOMM   = IIMM
  IOYYYY = IIYYYY

  DO JD = 1, IDELTA_DAYS
    IODD = IODD - 1
    IF ( IODD .LE. 0 ) THEN
      ! The correct way to compute the mod is:
      ! 1 + MOD( IOMM-1+N, 12 )
      ! since the month idx starts from 1 instead of 0.
      ! N is by definition -1, and a positive number is required
      ! as result; then it is possible to add 12.
      ! 1 + MOD( IOMM-1-1+12, 12 ) = 1 + MOD( IOMM + 10, 12 )
      IOMM = 1 + MOD( IOMM + 10, 12 )
      IF ( IOMM .EQ. 12 ) THEN
        IOYYYY = IOYYYY - 1
      ENDIF
      PP_TRYCALL(ERRFLAG_UNABLE_TO_UPDATE_DAYS_IN_MONTH) UPDATE_DAYS_IN_MONTHS(IOYYYY, KLM0, HOOKS)
      IODD = KLM0(IOMM)
    ENDIF
  ENDDO

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IOYYYY.LT.0, ERRFLAG_OUTPUT_YEAR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IOMM.LT.1,   ERRFLAG_OUTPUT_MONTH_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( IOMM.GT.12,  ERRFLAG_OUTPUT_MONTH_GREATER_THAN_TWELVE )
  PP_DEBUG_DEVELOP_COND_THROW( IODD.LT.1,   ERRFLAG_OUTPUT_DAY_LOWER_THAN_ONE )

  PP_TRYCALL(ERRFLAG_COMPUTE_DAYS_IN_MONTH) DAYS_IN_MONTH( IOYYYY, IOMM, DIM, HOOKS )
  PP_DEBUG_DEVELOP_COND_THROW( IODD.GT.DIM, ERRFLAG_OUTPUT_DAY_GREATER_THAN_DAYS_IN_MONTH )

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

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_DELTA_DAYS_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Delta days lower than 0' )
    CASE (ERRFLAG_INPUT_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Year out of range. Year lower than 0' )
    CASE (ERRFLAG_INPUT_MONTH_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Month out of range. Month index lower than 1' )
    CASE (ERRFLAG_INPUT_MONTH_GREATER_THAN_TWELVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Month out of range. Month index higher than 12' )
    CASE (ERRFLAG_INPUT_DAY_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Day out of range. Day index lower than 1' )
    CASE (ERRFLAG_INPUT_DAY_GREATER_THAN_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Input Day out of range. Day index higher than the number of days in month' )
    CASE (ERRFLAG_OUTPUT_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Year out of range. Year lower than 0' )
    CASE (ERRFLAG_OUTPUT_MONTH_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Month out of range. Month index lower than 1' )
    CASE (ERRFLAG_OUTPUT_MONTH_GREATER_THAN_TWELVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Month out of range. Month index higher than 12' )
    CASE (ERRFLAG_OUTPUT_DAY_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Day out of range. Day index lower than 1' )
    CASE (ERRFLAG_OUTPUT_DAY_GREATER_THAN_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Output Day out of range. Day index higher than the number of days in month' )
    CASE (ERRFLAG_UNABLE_TO_UPDATE_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to update days in month' )
    CASE (ERRFLAG_COMPUTE_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to compute days in month' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION DATE_SUB_DAYS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

!>
!> @brief Packs the hour and minute into a single integer.
!>
!> This function encodes the hour and minute into a single integer
!> for convenient storage and manipulation.
!>
!> @param [in] ihh Hour to be packed
!> @param [in] imm Minute to be packed
!>
!> @result packed hour and minute in a single inter
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'YYYYMMDD_HHMMSS_TO_DATETIME'
PP_THREAD_SAFE FUNCTION YYYYMMDD_HHMMSS_TO_DATETIME( YYYYMMDD, HHMMSS, DATETIME, HOOKS ) RESULT(RET)

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

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),               INTENT(IN)    :: YYYYMMDD
  INTEGER(KIND=JPIB_K),               INTENT(IN)    :: HHMMSS
  INTEGER(KIND=JPIB_K), DIMENSION(6), INTENT(OUT)   :: DATETIME
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_UNPACK_DATE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_UNPACK_TIME = 2_JPIB_K

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

  ! Error handling
  PP_TRYCALL(ERRFLAG_UNABLE_TO_UNPACK_DATE) UNPACK_YYYYMMDD( YYYYMMDD, DATETIME(1), DATETIME(2), DATETIME(3), HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_UNPACK_TIME) UNPACK_HHMMSS( HHMMSS, DATETIME(4), DATETIME(5), DATETIME(6), HOOKS )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_UNABLE_TO_UNPACK_DATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to unpack date' )
    CASE (ERRFLAG_UNABLE_TO_UNPACK_TIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to unpack time' )
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

  ! Exit point (on error)
  RETURN

END FUNCTION YYYYMMDD_HHMMSS_TO_DATETIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Unpack year, month and day.
!>
!> This function is used to unpack year, month and day into three
!> separate integers.
!>
!> @param [in]  yyyymmdd Packed date to be extracted
!> @param [out] yyyy     Year computed from the packed date
!> @param [out] mm       Month computed from the packed date
!> @param [out] dd       day computed from the packed date
!> @param [inout] hooks  Hooks for logging and error handling, tracing, logging etc.
!>
!> @note in order to check the validity of the date, the function will need to compute the number of days in the month.
!>       at the moment this functionality is disabled because the function DAYS_IN_MONTH is not uniquely defined,
!>       it requires the type of date we want to use (julian, gregorian, etc.). At the moment it is not clear which
!>       type of date we are using. (It seems that IFS uses gregorian dates while eccodes uses julian dates)
#if 0
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'UNPACK_YYYYMMDD'
PP_THREAD_SAFE FUNCTION UNPACK_YYYYMMDD(  YYYYMMDD, YYYY, MM, DD, HOOKS ) RESULT(RET)

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

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: YYYYMMDD
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: YYYY
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: MM
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: DD
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local variables
  !! TODO:  INTEGER(KIND=JPIB_K) :: DIM

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DATEPACK_LOWER_THAN_ZERO = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_YEAR_LOWER_THAN_ZERO = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MONTH_LOWER_THAN_ONE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MONTH_GREATER_THAN_TWELVE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DAY_LOWER_THAN_ONE = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DAY_GREATER_THAN_DAYS_IN_MONTH = 6_JPIB_K
  !! TODO: INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_COMPUTE_DAYS_IN_MONTH = 7

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

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( YYYYMMDD.LT.0, ERRFLAG_DATEPACK_LOWER_THAN_ZERO )

  ! Extract Year, Month and Day
  DD   = MOD(YYYYMMDD,100_JPIB_K)
  MM   = MOD(YYYYMMDD/100_JPIB_K,100_JPIB_K)
  YYYY = YYYYMMDD/10000_JPIB_K

  ! Compute days in month
  !! TODO:  PP_TRYCALL(ERRFLAG_UNABLE_TO_COMPUTE_DAYS_IN_MONTH) DAYS_IN_MONTH( YYYY, MM, DIM, HOOKS )

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( YYYY.LT.0, ERRFLAG_YEAR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( MM.LT.1,   ERRFLAG_MONTH_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( MM.GT.12,  ERRFLAG_MONTH_GREATER_THAN_TWELVE )
  PP_DEBUG_DEVELOP_COND_THROW( DD.LT.1,   ERRFLAG_DAY_LOWER_THAN_ONE )
  !! TODO: PP_DEBUG_DEVELOP_COND_THROW( DD.GT.DIM, ERRFLAG_DAY_GREATER_THAN_DAYS_IN_MONTH )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
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
    CASE (ERRFLAG_DATEPACK_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Packed date invalid. Packed date lower than 0' )
    CASE (ERRFLAG_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Year out of range. Year lower than 0' )
    CASE (ERRFLAG_MONTH_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Month out of range. Month index lower than 1' )
    CASE (ERRFLAG_MONTH_GREATER_THAN_TWELVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Month out of range. Month index higher than 12' )
    CASE (ERRFLAG_DAY_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Day out of range. Day index lower than 1' )
    CASE (ERRFLAG_DAY_GREATER_THAN_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Day out of range. Day index higher than the number of days in month' )
    !! TODO: CASE (ERRFLAG_UNABLE_TO_COMPUTE_DAYS_IN_MONTH)
    !! TODO:     PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to compute the number of days in the month' )
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

END FUNCTION UNPACK_YYYYMMDD
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE
#endif





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'ADD_SECONDS_TO_DATETIME'
PP_THREAD_SAFE FUNCTION ADD_SECONDS_TO_DATETIME( IN_DATETIME, SECONDS, OUT_DATETIME, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_DOUBLE

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPIM_K
  USE :: DATAKINDS_DEF_MOD,        ONLY: JPRD_K
  USE :: HOOKS_MOD,                ONLY: HOOKS_T

  ! Symbols imported from external libraries
  USE :: ECCODES, ONLY: CODES_JULIAN_TO_DATETIME
  USE :: ECCODES, ONLY: CODES_DATETIME_TO_JULIAN
  USE :: ECCODES, ONLY: CODES_SUCCESS
  USE :: ECCODES, ONLY: CODES_GET_ERROR_STRING

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Dummy arguments
  INTEGER(KIND=JPIB_K), DIMENSION(6), INTENT(IN)    :: IN_DATETIME
  INTEGER(KIND=JPIB_K),               INTENT(IN)    :: SECONDS
  INTEGER(KIND=JPIB_K), DIMENSION(6), INTENT(OUT)   :: OUT_DATETIME
  TYPE(HOOKS_T),                      INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  REAL(KIND=C_DOUBLE) :: JDT
  INTEGER(KIND=JPIM_K) :: KRET

  !> Error codes
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_DATETIME_TO_JULIAN=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CONVERT_JULIAN_TO_DATETIME=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_YEAR_LOWER_THAN_ZERO = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MONTH_LOWER_THAN_ONE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MONTH_GREATER_THAN_TWELVE = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DAY_LOWER_THAN_ONE = 6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_DAY_GREATER_THAN_DAYS_IN_MONTH = 7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HOUR_LOWER_THAN_ZERO = 8_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_HOUR_GREATER_THAN_23 = 9_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MINUTE_LOWER_THAN_ZERO = 10_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MINUTE_GREATER_THAN_SIXTY = 11_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECOND_LOWER_THAN_ZERO = 12_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_SECOND_GREATER_THAN_SIXTY = 13_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialization of good path return value
  PP_SET_ERR_SUCCESS( RET )


  ! Check the output datetime
  PP_DEBUG_DEVELOP_COND_THROW( IN_DATETIME(1).LT.0,   ERRFLAG_YEAR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IN_DATETIME(2).LT.1,   ERRFLAG_MONTH_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( IN_DATETIME(2).GT.12,  ERRFLAG_MONTH_GREATER_THAN_TWELVE )
  PP_DEBUG_DEVELOP_COND_THROW( IN_DATETIME(3).LT.1,   ERRFLAG_DAY_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( IN_DATETIME(4).LT.0,   ERRFLAG_HOUR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IN_DATETIME(4).GT.23,  ERRFLAG_HOUR_GREATER_THAN_23 )
  PP_DEBUG_DEVELOP_COND_THROW( IN_DATETIME(5).LT.0,   ERRFLAG_MINUTE_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IN_DATETIME(5).GT.59,  ERRFLAG_MINUTE_GREATER_THAN_SIXTY )
  PP_DEBUG_DEVELOP_COND_THROW( IN_DATETIME(6).LT.0,   ERRFLAG_SECOND_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( IN_DATETIME(6).GT.59,  ERRFLAG_SECOND_GREATER_THAN_SIXTY )

  ! Convert date time to julian date
!$omp critical(ECCODES_DATE_TIME)
  CALL CODES_DATETIME_TO_JULIAN( IN_DATETIME(1), IN_DATETIME(2), IN_DATETIME(3), IN_DATETIME(4), IN_DATETIME(5), IN_DATETIME(6), JDT, STATUS=KRET )
!$omp end critical(ECCODES_DATE_TIME)
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, ERRFLAG_UNABLE_TO_CONVERT_DATETIME_TO_JULIAN )

  ! Add forecast time to julian date
  JDT = JDT + REAL(SECONDS,KIND=C_DOUBLE) * SECONDS2DAYS

  ! Convert julian date to date time
  OUT_DATETIME = 0_JPIB_K
!$omp critical(ECCODES_DATE_TIME)
  CALL CODES_JULIAN_TO_DATETIME( JDT, OUT_DATETIME(1), OUT_DATETIME(2), OUT_DATETIME(3), OUT_DATETIME(4), OUT_DATETIME(5), OUT_DATETIME(6), STATUS=KRET )
!$omp end critical(ECCODES_DATE_TIME)
  PP_DEBUG_CRITICAL_COND_THROW( KRET.NE.CODES_SUCCESS, ERRFLAG_UNABLE_TO_CONVERT_JULIAN_TO_DATETIME )

  ! Check the output datetime
  PP_DEBUG_DEVELOP_COND_THROW( OUT_DATETIME(1).LT.0,   ERRFLAG_YEAR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( OUT_DATETIME(2).LT.1,   ERRFLAG_MONTH_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( OUT_DATETIME(2).GT.12,  ERRFLAG_MONTH_GREATER_THAN_TWELVE )
  PP_DEBUG_DEVELOP_COND_THROW( OUT_DATETIME(3).LT.1,   ERRFLAG_DAY_LOWER_THAN_ONE )
  PP_DEBUG_DEVELOP_COND_THROW( OUT_DATETIME(4).LT.0,   ERRFLAG_HOUR_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( OUT_DATETIME(4).GT.23,  ERRFLAG_HOUR_GREATER_THAN_23 )
  PP_DEBUG_DEVELOP_COND_THROW( OUT_DATETIME(5).LT.0,   ERRFLAG_MINUTE_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( OUT_DATETIME(5).GT.59,  ERRFLAG_MINUTE_GREATER_THAN_SIXTY )
  PP_DEBUG_DEVELOP_COND_THROW( OUT_DATETIME(6).LT.0,   ERRFLAG_SECOND_LOWER_THAN_ZERO )
  PP_DEBUG_DEVELOP_COND_THROW( OUT_DATETIME(6).GT.59,  ERRFLAG_SECOND_GREATER_THAN_SIXTY )

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
    CHARACTER(LEN=4096) :: ERR_MSG
    CHARACTER(LEN=192) :: CTMP
    INTEGER(KIND=JPIM_K) :: IOSTATUS

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (ERRFLAG_UNABLE_TO_CONVERT_DATETIME_TO_JULIAN)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert date time' )
      ERR_MSG = REPEAT(' ',4096)
      WRITE(ERR_MSG,*,IOSTAT=IOSTATUS) KRET
      IF ( IOSTATUS .EQ. 0 ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Grib error code: : '//TRIM(ADJUSTL(ERR_MSG)) )
      ENDIF
      ERR_MSG = REPEAT(' ',4096)
      CALL CODES_GET_ERROR_STRING( KRET, ERR_MSG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Grib error message: '//TRIM(ADJUSTL(ERR_MSG)) )
      ERR_MSG = REPEAT(' ',4096)
      WRITE(ERR_MSG,*,IOSTAT=IOSTATUS) SECONDS
      IF ( IOSTATUS .EQ. 0 ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Current time since start in seconds: '//TRIM(ADJUSTL(ERR_MSG)) )
      ENDIF
      ERR_MSG = REPEAT(' ',4096)
      WRITE(ERR_MSG,'(I4,I2,I2,I2,I2,I2)',IOSTAT=IOSTATUS) IN_DATETIME
      IF ( IOSTATUS .EQ. 0 ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Message time [YYYY,MM,DD,HH,MM,SS]: ['//TRIM(ADJUSTL(ERR_MSG))//']' )
      ENDIF

    CASE (ERRFLAG_UNABLE_TO_CONVERT_JULIAN_TO_DATETIME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert date time' )
      ERR_MSG = REPEAT(' ',4096)
      WRITE(ERR_MSG,*,IOSTAT=IOSTATUS) KRET
      IF ( IOSTATUS .EQ. 0 ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Grib error code: : '//TRIM(ADJUSTL(ERR_MSG)) )
      ENDIF
      ERR_MSG = REPEAT(' ',4096)
      CALL CODES_GET_ERROR_STRING( KRET, ERR_MSG )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Grib error message: '//TRIM(ADJUSTL(ERR_MSG)) )
      ERR_MSG = REPEAT(' ',4096)
      WRITE(ERR_MSG,*,IOSTAT=IOSTATUS) SECONDS
      IF ( IOSTATUS .EQ. 0 ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Current time since start in seconds: '//TRIM(ADJUSTL(ERR_MSG)) )
      ENDIF
      ERR_MSG = REPEAT(' ',4096)
      WRITE(ERR_MSG,'(I4,I2,I2,I2,I2,I2)',IOSTAT=IOSTATUS) IN_DATETIME
      IF ( IOSTATUS .EQ. 0 ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Message time [YYYY,MM,DD,HH,MM,SS]: ['//TRIM(ADJUSTL(ERR_MSG))//']' )
      ENDIF

    CASE (ERRFLAG_YEAR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Year out of range. Year lower than 0' )
    CASE (ERRFLAG_MONTH_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Month out of range. Month index lower than 1' )
    CASE (ERRFLAG_MONTH_GREATER_THAN_TWELVE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Month out of range. Month index higher than 12' )
    CASE (ERRFLAG_DAY_LOWER_THAN_ONE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Day out of range. Day index lower than 1' )
    CASE (ERRFLAG_DAY_GREATER_THAN_DAYS_IN_MONTH)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Day out of range. Day index higher than the number of days in month' )

    CASE (ERRFLAG_HOUR_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Hour out of range. Hour index lower than 0' )
    CASE (ERRFLAG_HOUR_GREATER_THAN_23)
      CTMP = REPEAT(' ',192)
      WRITE(CTMP,'(6I8)',IOSTAT=IOSTATUS) IN_DATETIME
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Hour out of range. Hour index higher than 23' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Current date time: ['//TRIM(ADJUSTL(CTMP)) //']' )
    CASE (ERRFLAG_MINUTE_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Minute out of range. Minute index lower than 0' )
    CASE (ERRFLAG_MINUTE_GREATER_THAN_SIXTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Minute out of range. Minute index higher than 59' )
    CASE (ERRFLAG_SECOND_LOWER_THAN_ZERO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Second out of range. Second index lower than 0' )
    CASE (ERRFLAG_SECOND_GREATER_THAN_SIXTY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Second out of range. Second index higher than 59' )

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

END FUNCTION ADD_SECONDS_TO_DATETIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE DATETIME_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME