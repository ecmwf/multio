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

IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: PACK_YYYYMMDD
PUBLIC :: UNPACK_YYYYMMDD
PUBLIC :: DAYS_IN_MONTH
PUBLIC :: DAYS_IN_MONTHS
PUBLIC :: UPDATE_DAYS_IN_MONTHS
PUBLIC :: IS_LEAP_YEAR
PUBLIC :: DATE_UPDATE
PUBLIC :: DATE_SUM_DAYS
PUBLIC :: DATE_SUB_DAYS
PUBLIC :: PACK_HHMM
PUBLIC :: SEC2HHMM
PUBLIC :: SEC2HH_MM
PUBLIC :: SEC2HH_MM_SS
PUBLIC :: HH_MM_SS2SEC

CONTAINS

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
#define PP_PROCEDURE_NAME 'PACK_HHMM'
FUNCTION PACK_HHMM( IHH, IMM ) RESULT(IHHMM)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IHH
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IMM

  ! Function result
  INTEGER(KIND=JPIB_K)  :: IHHMM

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IHH.LT.0,  1 )
  PP_DEBUG_DEVELOP_COND_THROW( IHH.GT.23, 2 )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.LT.0,  3 )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.GT.59, 4 )

  ! Pack Hours and minutes
  IHHMM = IHH*100 + IMM

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Hour out of range. Index of hour lower than 0' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Hour out of range. Index of hour higher than 23' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Minute out of range. Index of minute lower than 0' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Minute out of range. Index of minute higher than 59' )
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

END FUNCTION PACK_HHMM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Convert seconds into a packed integer with hour and minute.
!>
!> This function convert seconds into hour and minute and then encodes
!> them into a single integer for convenient storage and manipulation.
!>
!> @param [in] nsssss Seconds to be converted
!>
!> @result packed hour and minute in a single inter
!>
!> @see SEC2HH_MM
!> @see PACK_HHMM
!> @see HH_MM_SS2SEC
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'SEC2HHMM'
FUNCTION SEC2HHMM( NSSSSS ) RESULT(IHHMM)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: NSSSSS

  ! Function result
  INTEGER(KIND=JPIB_K) :: IHHMM

  ! Local variables
  INTEGER(KIND=JPIB_K) :: IHH
  INTEGER(KIND=JPIB_K) :: IMM

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( NSSSSS.LT.0,  1 )

  ! Compute hour and minutes out of seconds
  CALL SEC2HH_MM( NSSSSS, IHH, IMM )

  ! Pack hour and seconds
  IHHMM = PACK_HHMM( IHH, IMM )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Seconds out of range. Number of seconds lower than 0' )
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

END FUNCTION SEC2HHMM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Convert seconds into hour and minute.
!>
!> This function convert seconds into hour and minute.
!>
!> @param [in]  nsssss Seconds to be converted
!> @param [out] ihh    Hour computed from seconds
!> @param [out] imm    Minute computed from seconds
!>
!> @see SEC2HHMM
!> @see SEC2HH_MM_SS
!> @see PACK_HHMM
!> @see HH_MM_SS2SEC
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SEC2HH_MM'
SUBROUTINE SEC2HH_MM( NSSSSS, IHH, IMM )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: NSSSSS
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IHH
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IMM

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( NSSSSS.LT.0,  1 )

  IHH = NSSSSS / 3600
  IMM = MOD(NSSSSS, 3600) / 60

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Seconds out of range. Number of seconds lower than 0' )
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

END SUBROUTINE SEC2HH_MM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Converts hours, minutes, and seconds to seconds.
!>
!> This function converts hours, minutes, and seconds to seconds for
!> time calculations and manipulations.
!>
!> @param [in] ihh Hours to be used in the conversion.
!> @param [in] imm Minutes to be used in the conversion.
!> @param [in] iss Seconds to be used in the conversion.
!>
!> @result Total number of seconds resulting from the sum of hours,
!>         minutes, and seconds.
!>
!> @note The parameters ihh, imm, and iss should be within their respective valid ranges.
!>
!> @see SEC2HH_MM
!> @see PACK_HHMM
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'HH_MM_SS2SEC'
FUNCTION HH_MM_SS2SEC( IHH, IMM, ISS ) RESULT(NSSSSS)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IHH
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IMM
  INTEGER(KIND=JPIB_K), INTENT(IN) :: ISS

  ! Function result
  INTEGER(KIND=JPIB_K)  :: NSSSSS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IHH.GT.23, 1 )
  PP_DEBUG_DEVELOP_COND_THROW( IHH.LT.0,  2 )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.GT.59, 3 )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.LT.0,  4 )
  PP_DEBUG_DEVELOP_COND_THROW( ISS.GT.59, 5 )
  PP_DEBUG_DEVELOP_COND_THROW( ISS.LT.0,  6 )

  NSSSSS = IHH*3600 + IMM*60 + ISS

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Hour out of range. Index of hour lower than 0' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Hour out of range. Index of hour higher than 23' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Minute out of range. Index of minute lower than 0' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Minute out of range. Index of minute higher than 59' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Second out of range. Index of second lower than 0' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Second out of range. Index of second higher than 59' )
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

END FUNCTION HH_MM_SS2SEC
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Convert seconds into hour, minute and seconds.
!>
!> This function convert seconds into hour, minute and seconds.
!>
!> @param [in]  nsssss Seconds to be converted
!> @param [out] ihh    Hours   computed from seconds
!> @param [out] imm    Minutes computed from seconds
!> @param [out] iss    Seconds computed from seconds
!>
!> @see SEC2HHMM
!> @see SEC2HH_MM
!> @see PACK_HHMM
!> @see HH_MM_SS2SEC
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'SEC2HH_MM_SS'
SUBROUTINE SEC2HH_MM_SS( NSSSSS, IHH, IMM, ISS )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: NSSSSS
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IHH
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IMM
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: ISS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( NSSSSS.LT.0, 7 )

  IHH = NSSSSS / 3600
  IMM = MOD(NSSSSS, 3600) / 60
  ISS = NSSSSS - IHH*3600 - IMM*60

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IHH.GT.23,   1 )
  PP_DEBUG_DEVELOP_COND_THROW( IHH.LT.0,    2 )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.GT.59,   3 )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.LT.0,    4 )
  PP_DEBUG_DEVELOP_COND_THROW( ISS.GT.59,   5 )
  PP_DEBUG_DEVELOP_COND_THROW( ISS.LT.0,    6 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Hour out of range. Index of hour higher than 23' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Hour out of range. Index of hour lower than 0' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Minute out of range. Index of minute higher than 59' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Minute out of range. Index of minute lower than 0' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Second out of range. Index of second higher than 59' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Second out of range. Index of second lower than 0' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Seconds out of range. Number of seconds lower than 0' )
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

END SUBROUTINE SEC2HH_MM_SS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Packs the year, month and day into a single integer.
!>
!> This function encodes the year, month and dayinute into a single integer
!> for convenient storage and manipulation.
!>
!> @param [in] iyyyy Year to be packed
!> @param [in] imm   Month to be packed
!> @param [in] idd   Day to be packed
!>
!> @result packed year, month and day in a single inter
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PACK_YYYYMMDD'
FUNCTION PACK_YYYYMMDD( IYYYY, IMM, IDD ) RESULT(IYYYYMMDD)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IYYYY
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IMM
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IDD

  ! Function result
  INTEGER(KIND=JPIB_K)  :: IYYYYMMDD

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IYYYY.LT.0, 1 )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.LT.1,   2 )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.GT.12,  3 )
  PP_DEBUG_DEVELOP_COND_THROW( IDD.LT.1,   4 )
  PP_DEBUG_DEVELOP_COND_THROW( IDD.GT.DAYS_IN_MONTH(IYYYY,IMM), 5 )

  ! Pack Year, Month and Day
  IYYYYMMDD = IYYYY*10000 + IMM*100 + IDD

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Year out of range. Year lower than 0' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Month out of range. Month index lower than 1' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Month out of range. Month index higher than 12' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Day out of range. Day index lower than 1' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Day out of range. Day index higher than the number of days in month' )
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

END FUNCTION PACK_YYYYMMDD
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Unpack year, month and day.
!>
!> This function is used to unpack year, month and day into three
!> separate integers.
!>
!> @param [in]  iyyyymmdd Packed time to be extracted
!> @param [out] iyyyy     Year packed in the date
!> @param [out] imm       Minutes packed in the date
!> @param [out] idd       Seconds computed from the packed date
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'UNPACK_YYYYMMDD'
SUBROUTINE UNPACK_YYYYMMDD(  IYYYYMMDD, IYYYY, IMM, IDD )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: IYYYYMMDD
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IYYYY
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IMM
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IDD

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IYYYYMMDD.LT.0, 1 )

  ! Extract Year, Month and Day
  IDD   = MOD(IYYYYMMDD,100)
  IMM   = MOD(IYYYYMMDD/100,100)
  IYYYY = IYYYYMMDD/10000

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IYYYY.LT.0, 2 )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.LT.1,   3 )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.GT.12,  4 )
  PP_DEBUG_DEVELOP_COND_THROW( IDD.LT.1,   5 )
  PP_DEBUG_DEVELOP_COND_THROW( IDD.GT.DAYS_IN_MONTH(IYYYY,IMM), 6 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Packed date invalid. PAcked date lower than 0' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Year out of range. Year lower than 0' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Month out of range. Month index lower than 1' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Month out of range. Month index higher than 12' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Day out of range. Day index lower than 1' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Day out of range. Day index higher than the number of days in month' )
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

END SUBROUTINE UNPACK_YYYYMMDD
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Compute the number of days in a single month
!>
!> This function is used to compute the number of days in a month
!> given the year and the month
!>
!> @param [in] iyyyy Year
!> @param [in] imm   Id of the month
!>
!> @result ndays Number of days in the requested month
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DAYS_IN_MONTH'
FUNCTION DAYS_IN_MONTH( IYYYY, IMM ) RESULT(NDAYS)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IYYYY
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IMM

  ! Function Result
  INTEGER(KIND=JPIB_K) :: NDAYS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IYYYY.LT.0, 1 )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.LT.1,   2 )
  PP_DEBUG_DEVELOP_COND_THROW( IMM.GT.12,  3 )

  SELECT CASE ( IMM )

  CASE (1,3,5,7,8,10,12)
    ! 31 days months
    NDAYS = 31

  CASE (4,6,9,11)
    ! 30 days months
    NDAYS = 30

  CASE (2)
    ! Days of February
    IF ( IS_LEAP_YEAR( IYYYY ) ) THEN
      NDAYS = 29
    ELSE
      NDAYS = 28
    ENDIF
  CASE DEFAULT
    PP_DEBUG_DEVELOP_THROW( 4 )
  END SELECT

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Year out of range. Year lower than 0' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Month out of range. Month index lower than 1' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Month out of range. Month index higher than 12' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Month out of range. Fallback error' )
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

END FUNCTION DAYS_IN_MONTH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Compute the number of days in every month of a specific year
!>
!> This function is used to compute the number of days in every month
!> in one year given the year
!>
!> @param [in] iyyyy Year
!>
!> @result ndays Array with the number of days in every month of the requested year
!>
#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'DAYS_IN_MONTHS'
FUNCTION DAYS_IN_MONTHS( IYYYY ) RESULT(IDIM)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IYYYY

  ! Function Result
  INTEGER(KIND=JPIB_K), DIMENSION(12) :: IDIM

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IYYYY.LT.0, 1 )

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
  IF ( IS_LEAP_YEAR( IYYYY ) ) THEN
    IDIM(2) = 29
  ELSE
    IDIM(2) = 28
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Year out of range. Year lower than 0' )
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

END FUNCTION DAYS_IN_MONTHS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Update the number of days in one year
!>
!> This function is used to udate the number of days in a moth for a specified year.
!> For performance reasons is better to update only the February value
!> instead of regenerating everything.
!>
!> @param [in]    iyyyy Year
!> @param [inout] ndays Array with the number of days in every month of the requested year
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'UPDATE_DAYS_IN_MONTHS'
SUBROUTINE UPDATE_DAYS_IN_MONTHS( IYYYY, IDIM )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),                INTENT(IN)    :: IYYYY
  INTEGER(KIND=JPIB_K), DIMENSION(12), INTENT(INOUT) :: IDIM

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IYYYY.LT.0, 1 )

  ! Days of February
  IF ( IS_LEAP_YEAR( IYYYY ) ) THEN
    IDIM(2) = 29
  ELSE
    IDIM(2) = 28
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Year out of range. Year lower than 0' )
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

END SUBROUTINE UPDATE_DAYS_IN_MONTHS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Update the number of days in one year
!>
!> This function is used to udate the number of days in a moth for a specified year.
!> For performance reasons is better to update only the February value
!> instead of regenerating everything.
!>
!> @param [in]    iyyyy Year
!>
!> @result true if the year is a leap year
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'IS_LEAP_YEAR'
FUNCTION IS_LEAP_YEAR( IYYYY ) RESULT(LY)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: IYYYY

  ! Function Result
  LOGICAL :: LY

  ! Local variables
  LOGICAL, DIMENSION(4) :: LEAP_CONDITIONS

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IYYYY.LT.0, 1 )

  ! Conditions to have a leap-year
  LEAP_CONDITIONS(1) = (MOD(IYYYY,4) .EQ. 0)
  LEAP_CONDITIONS(2) = (MOD(IYYYY,400) .NE. 100)
  LEAP_CONDITIONS(3) = (MOD(IYYYY,400) .NE. 200)
  LEAP_CONDITIONS(4) = (MOD(IYYYY,400) .NE. 300)

  ! Compute the return value
  LY = ALL(LEAP_CONDITIONS)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Year out of range. Year lower than 0' )
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

END FUNCTION IS_LEAP_YEAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief modify a date by adding/subtracting days
!>
!> This function is used to udate the date by adding or subtracting a
!> predefined number of days
!>
!> @param [in]    iiyyyy      Start year
!> @param [in]    iimm        Start mont
!> @param [in]    iidd        Start Day
!> @param [in]    idelta_days Number of days (with sign) to be added/subtracted from the start date
!> @param [out]   ioyyyy      Output year
!> @param [out]   iomm        Output mont
!> @param [out]   iodd        Output Day
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'DATE_UPDATE'
SUBROUTINE DATE_UPDATE( IIYYYY, IIMM, IIDD, IDELTA_DAYS, IOYYYY, IOMM, IODD )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: IIYYYY
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: IIMM
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: IIDD
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: IDELTA_DAYS
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IOYYYY
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IOMM
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IODD

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IIYYYY.LT.0, 1 )
  PP_DEBUG_DEVELOP_COND_THROW( IIMM.LT.1,   2 )
  PP_DEBUG_DEVELOP_COND_THROW( IIMM.GT.12,  3 )
  PP_DEBUG_DEVELOP_COND_THROW( IIDD.LT.1,   4 )
  PP_DEBUG_DEVELOP_COND_THROW( IIDD.GT.DAYS_IN_MONTH(IIYYYY,IIMM), 5 )


  IF ( IDELTA_DAYS .GE. 0 ) THEN
    CALL DATE_SUM_DAYS( IIYYYY, IIMM, IIDD, IDELTA_DAYS, IOYYYY, IOMM, IODD )
  ELSE
    CALL DATE_SUB_DAYS( IIYYYY, IIMM, IIDD, -IDELTA_DAYS, IOYYYY, IOMM, IODD )
  ENDIF


  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IOYYYY.LT.0, 6 )
  PP_DEBUG_DEVELOP_COND_THROW( IOMM.LT.1,   7 )
  PP_DEBUG_DEVELOP_COND_THROW( IOMM.GT.12,  8 )
  PP_DEBUG_DEVELOP_COND_THROW( IODD.LT.1,   9 )
  PP_DEBUG_DEVELOP_COND_THROW( IODD.GT.DAYS_IN_MONTH(IOYYYY,IOMM), 10 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Year out of range. Year lower than 0' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Month out of range. Month index lower than 1' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Month out of range. Month index higher than 12' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Day out of range. Day index lower than 1' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Day out of range. Day index higher than the number of days in month' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Year out of range. Year lower than 0' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Month out of range. Month index lower than 1' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Month out of range. Month index higher than 12' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Day out of range. Day index lower than 1' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Day out of range. Day index higher than the number of days in month' )
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

END SUBROUTINE DATE_UPDATE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief modify a date by adding days
!>
!> This function is used to udate the date by adding a
!> predefined number of days
!>
!> @param [in]    iiyyyy      Start year
!> @param [in]    iimm        Start mont
!> @param [in]    iidd        Start Day
!> @param [in]    idelta_days Number of days to be added from the start date
!> @param [out]   ioyyyy      Output year
!> @param [out]   iomm        Output mont
!> @param [out]   iodd        Output Day
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'DATE_SUM_DAYS'
SUBROUTINE DATE_SUM_DAYS( IIYYYY, IIMM, IIDD, IDELTA_DAYS, IOYYYY, IOMM, IODD )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: IIYYYY
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: IIMM
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: IIDD
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: IDELTA_DAYS
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IOYYYY
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IOMM
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IODD

  ! Local variables
  INTEGER(KIND=JPIB_K) :: JD
  INTEGER(KIND=JPIB_K), DIMENSION(12) :: KLM0

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IDELTA_DAYS.LT.0, 1 )
  PP_DEBUG_DEVELOP_COND_THROW( IIYYYY.LT.0,      2 )
  PP_DEBUG_DEVELOP_COND_THROW( IIMM.LT.1,        3 )
  PP_DEBUG_DEVELOP_COND_THROW( IIMM.GT.12,       4 )
  PP_DEBUG_DEVELOP_COND_THROW( IIDD.LT.1,        5 )
  PP_DEBUG_DEVELOP_COND_THROW( IIDD.GT.DAYS_IN_MONTH(IIYYYY,IIMM), 6 )

  ! Initialization
  KLM0   = DAYS_IN_MONTHS(IIYYYY)
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
        CALL UPDATE_DAYS_IN_MONTHS( IOYYYY, KLM0 )
      ENDIF
    ENDIF
  ENDDO

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IOYYYY.LT.0, 7 )
  PP_DEBUG_DEVELOP_COND_THROW( IOMM.LT.1,   8 )
  PP_DEBUG_DEVELOP_COND_THROW( IOMM.GT.12,  9 )
  PP_DEBUG_DEVELOP_COND_THROW( IODD.LT.1,   10 )
  PP_DEBUG_DEVELOP_COND_THROW( IODD.GT.DAYS_IN_MONTH(IOYYYY,IOMM), 11 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Delta days lower than 0' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Year out of range. Year lower than 0' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Month out of range. Month index lower than 1' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Month out of range. Month index higher than 12' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Day out of range. Day index lower than 1' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Day out of range. Day index higher than the number of days in month' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Year out of range. Year lower than 0' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Month out of range. Month index lower than 1' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Month out of range. Month index higher than 12' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Day out of range. Day index lower than 1' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Day out of range. Day index higher than the number of days in month' )
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

END SUBROUTINE DATE_SUM_DAYS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief modify a date by subtracting days
!>
!> This function is used to udate the date by subtracting a
!> predefined number of days
!>
!> @param [in]    iiyyyy      Start year
!> @param [in]    iimm        Start mont
!> @param [in]    iidd        Start Day
!> @param [in]    idelta_days Number of days to be subtracted from the start date
!> @param [out]   ioyyyy      Output year
!> @param [out]   iomm        Output mont
!> @param [out]   iodd        Output Day
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'DATE_SUB_DAYS'
SUBROUTINE DATE_SUB_DAYS( IIYYYY, IIMM, IIDD, IDELTA_DAYS, IOYYYY, IOMM, IODD )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: IIYYYY
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: IIMM
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: IIDD
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: IDELTA_DAYS
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IOYYYY
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IOMM
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: IODD

  ! Local variables
  INTEGER(KIND=JPIB_K) :: JD
  INTEGER(KIND=JPIB_K), DIMENSION(12) :: KLM0

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IDELTA_DAYS.LT.0, 1 )
  PP_DEBUG_DEVELOP_COND_THROW( IIYYYY.LT.0,      2 )
  PP_DEBUG_DEVELOP_COND_THROW( IIMM.LT.1,        3 )
  PP_DEBUG_DEVELOP_COND_THROW( IIMM.GT.12,       4 )
  PP_DEBUG_DEVELOP_COND_THROW( IIDD.LT.1,        5 )
  PP_DEBUG_DEVELOP_COND_THROW( IIDD.GT.DAYS_IN_MONTH(IIYYYY,IIMM), 6 )

  ! Initialization
  KLM0   = DAYS_IN_MONTHS(IIYYYY)
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
      CALL UPDATE_DAYS_IN_MONTHS(IOYYYY, KLM0)
      IODD = KLM0(IOMM)
    ENDIF
  ENDDO

  ! Error handling
  PP_DEBUG_DEVELOP_COND_THROW( IOYYYY.LT.0, 7 )
  PP_DEBUG_DEVELOP_COND_THROW( IOMM.LT.1,   8 )
  PP_DEBUG_DEVELOP_COND_THROW( IOMM.GT.12,  9 )
  PP_DEBUG_DEVELOP_COND_THROW( IODD.LT.1,   10 )
  PP_DEBUG_DEVELOP_COND_THROW( IODD.GT.DAYS_IN_MONTH(IOYYYY,IOMM), 11 )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Delta days lower than 0' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Year out of range. Year lower than 0' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Month out of range. Month index lower than 1' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Month out of range. Month index higher than 12' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Day out of range. Day index lower than 1' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Input Day out of range. Day index higher than the number of days in month' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Year out of range. Year lower than 0' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Month out of range. Month index lower than 1' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Month out of range. Month index higher than 12' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Day out of range. Day index lower than 1' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output Day out of range. Day index higher than the number of days in month' )
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

END SUBROUTINE DATE_SUB_DAYS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE DATETIME_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME