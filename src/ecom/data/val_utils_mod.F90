!> @file om_values_mod.F90
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
#define PP_FILE_NAME 'val_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'VAL_UTILS_MOD'
MODULE VAL_UTILS_MOD

IMPLICIT NONE

! Default visibility
PRIVATE

! Whitelist of public symbols
PUBLIC :: VAL_CREATE_NAME
PUBLIC :: VAL_WOPEN
PUBLIC :: VAL_ROPEN
PUBLIC :: VAL_CLOSE
PUBLIC :: VAL_WRITE_SP
PUBLIC :: VAL_WRITE_DP
PUBLIC :: VAL_READ_SP
PUBLIC :: VAL_READ_DP
PUBLIC :: VAL_GENERATE_SP
PUBLIC :: VAL_GENERATE_DP

CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'VAL_CREATE_NAME'
SUBROUTINE VAL_CREATE_NAME( DIRECTORY, MSG_ID, PROC_ID, VALFNAME )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)  :: DIRECTORY
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: MSG_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: PROC_ID
  CHARACTER(LEN=*),     INTENT(OUT) :: VALFNAME

  ! Local variables
  LOGICAL :: VALEXIST
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: M
  INTEGER(KIND=JPIB_K) :: STAT


  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  N = LEN(VALFNAME)
  M = LEN_TRIM(DIRECTORY) + 26
  PP_DEBUG_CRITICAL_COND_THROW( M.GT.N, 1)

  ! Create the message name
  VALFNAME = REPEAT(' ',N)
  WRITE(VALFNAME,'(A,A,I8.8,A,I8.8,A)', IOSTAT=STAT) TRIM(ADJUSTL(DIRECTORY)), '/val_', MSG_ID, '_', PROC_ID, '.bin'
  PP_DEBUG_CRITICAL_COND_THROW(STAT.NE.0, 2)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Values file name variable too short' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create the values file name' )
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

END SUBROUTINE VAL_CREATE_NAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'VAL_WOPEN'
SUBROUTINE VAL_WOPEN( VALFNAME, VALUNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: VALFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: VALUNIT

  ! Local variables
  LOGICAL :: VALEXIST
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( FILE=TRIM(VALFNAME), EXIST=VALEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( VALEXIST, 1)

  ! Open the TOC file
  OPEN( NEWUNIT=VALUNIT, FILE=TRIM(VALFNAME), STATUS='REPLACE', ACCESS='STREAM', ACTION='WRITE', FORM='UNFORMATTED', IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Val file already exists' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to open val file' )
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

END SUBROUTINE VAL_WOPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'VAL_ROPEN'
SUBROUTINE VAL_ROPEN( VALFNAME, VALUNIT, BIG_ENDIAN_READ )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)  :: VALFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: VALUNIT
  LOGICAL,              INTENT(IN)  :: BIG_ENDIAN_READ

  ! Local variables
  LOGICAL :: VALEXIST
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( FILE=TRIM(VALFNAME), EXIST=VALEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.VALEXIST, 1)

  ! Open the TOC file
  IF ( BIG_ENDIAN_READ ) THEN
    OPEN( NEWUNIT=VALUNIT, FILE=TRIM(VALFNAME), STATUS='OLD', ACCESS='STREAM', ACTION='READ', FORM='UNFORMATTED', CONVERT='BIG_ENDIAN', IOSTAT=STAT )
  ELSE
    OPEN( NEWUNIT=VALUNIT, FILE=TRIM(VALFNAME), STATUS='OLD', ACCESS='STREAM', ACTION='READ', FORM='UNFORMATTED', IOSTAT=STAT )
  ENDIF
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to find val file: '//TRIM(VALFNAME) )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to open val file: '//TRIM(VALFNAME) )
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

END SUBROUTINE VAL_ROPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'VAL_CLOSE'
SUBROUTINE VAL_CLOSE( VALUNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: VALUNIT

  ! Local variables
  LOGICAL :: VALOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=VALUNIT, OPENED=VALOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.VALOPENED, 1)

  ! Open the TOC file
  CLOSE( UNIT=VALUNIT, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'unit not connected to a val file' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error closing val file' )
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

END SUBROUTINE VAL_CLOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'VAL_WRITE_SP'
SUBROUTINE VAL_WRITE_SP( VALUNIT, VALUES, WRITE_POS )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPRM_K
  USE :: OM_CORE_MOD, ONLY: VALUES_SP_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),            INTENT(IN)  :: VALUNIT
  REAL(KIND=JPRM_K), DIMENSION(:), INTENT(IN)  :: VALUES
  INTEGER(KIND=INT64),             INTENT(OUT) :: WRITE_POS

  ! Local variables
  LOGICAL :: VALOPENED
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local parameters
  CHARACTER(LEN=*), PARAMETER :: GUARD='GUARD_SP'

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=VALUNIT, OPENED=VALOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.VALOPENED, 1)

  ! Get the position in the file
  INQUIRE( VALUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

  ! Write the current step of the simulation and trigger restart
  WRITE( VALUNIT, IOSTAT=STAT ) INT( VALUES_SP_E, INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)
  WRITE( VALUNIT, IOSTAT=STAT ) INT( LBOUND(VALUES,1), INT64 )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)
  WRITE( VALUNIT, IOSTAT=STAT ) INT( UBOUND(VALUES,1), INT64 )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 5)

  ! Write the current step
  DO I = LBOUND(VALUES,1), UBOUND(VALUES,1)
    WRITE( VALUNIT, IOSTAT=STAT ) REAL(VALUES(I), REAL32)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 6)
  ENDDO

  ! Write guards
  WRITE( VALUNIT, IOSTAT=STAT ) GUARD
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 7)

  ! Perform a flush just to be sure
  FLUSH( VALUNIT )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=1024) :: TMP1
    CHARACTER(LEN=1024) :: TMP2

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Values file not opened or not exists' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the kind of values for the message' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write lower bound for the values' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write upper bound for the values' )
    CASE (6)
      TMP1 = REPEAT(' ', 1024)
      WRITE(TMP2,*) I
      TMP1 = REPEAT(' ', 1024)
      WRITE(TMP2,*) VALUES(I)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values('//TRIM(ADJUSTL(TMP1))//')='//TRIM(ADJUSTL(TMP2)) )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write guard' )
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

END SUBROUTINE VAL_WRITE_SP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'VAL_READ_SP'
SUBROUTINE VAL_READ_SP( VALUNIT, READ_POS, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPRM_K
  USE :: OM_CORE_MOD, ONLY: VALUES_SP_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),            INTENT(IN)  :: VALUNIT
  INTEGER(KIND=JPIB_K),            INTENT(IN)  :: READ_POS
  REAL(KIND=JPRM_K), DIMENSION(:), INTENT(OUT) :: VALUES

  ! Local variables
  LOGICAL :: VALOPENED
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=INT64)  :: PRECISION
  INTEGER(KIND=INT64)  :: LO
  INTEGER(KIND=INT64)  :: HI
  REAL(KIND=REAL32)    :: RTMP
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=8)     :: GUARD

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! WRITE(*,*) 'Passo di qui!!!!'

  ! Check if the file is opened
  INQUIRE( UNIT=VALUNIT, OPENED=VALOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.VALOPENED, 1)

  ! Write the current step of the simulation and trigger restart
  READ( VALUNIT, POS=READ_POS, IOSTAT=STAT ) PRECISION
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)
  PP_DEBUG_CRITICAL_COND_THROW( PRECISION.NE.VALUES_SP_E, 3 )
  READ( VALUNIT, IOSTAT=STAT ) LO
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)
  READ( VALUNIT, IOSTAT=STAT ) HI
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 5)

  ! Check memory size/status
  PP_DEBUG_CRITICAL_COND_THROW( LBOUND(VALUES,1).NE.LO, 6)
  PP_DEBUG_CRITICAL_COND_THROW( UBOUND(VALUES,1).NE.HI, 7)

  ! Write the current step
  DO I = LO, HI
    READ( VALUNIT, IOSTAT=STAT ) RTMP
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 8)
    VALUES(I) = REAL( RTMP, KIND(VALUES(I)) )
  ENDDO

  ! Read the guard
  READ( VALUNIT, IOSTAT=STAT ) GUARD
  PP_DEBUG_CRITICAL_COND_THROW( GUARD.NE.'GUARD_SP', 9)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=1024) :: TMP1
    CHARACTER(LEN=1024) :: TMP2

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Values file not opened or not exists' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read the values precision' )
    CASE (3)
        TMP1 = REPEAT(' ', 1024)
        WRITE(TMP1,*) VALUES_SP_E
        TMP1 = REPEAT(' ', 1024)
        WRITE(TMP2,*) PRECISION
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Precision do not match, expected: '//TRIM(ADJUSTL(TMP1))//', got: '//TRIM(ADJUSTL(TMP2)) )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lower bound' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read upper bound' )
    CASE (6)
        TMP1 = REPEAT(' ', 1024)
        WRITE(TMP1,*) LO
        TMP1 = REPEAT(' ', 1024)
        WRITE(TMP2,*) LBOUND(VALUES,1)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Lower bound do not match, expected: '//TRIM(ADJUSTL(TMP1))//', got: '//TRIM(ADJUSTL(TMP2)) )
      CASE (7)
        TMP1 = REPEAT(' ', 1024)
        WRITE(TMP1,*) HI
        TMP1 = REPEAT(' ', 1024)
        WRITE(TMP2,*) UBOUND(VALUES,1)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Upper bound do not match, expected: '//TRIM(ADJUSTL(TMP1))//', got: '//TRIM(ADJUSTL(TMP2)) )
    CASE (8)
      TMP1 = REPEAT(' ', 1024)
      WRITE(TMP1,*) I
      TMP1 = REPEAT(' ', 1024)
      WRITE(TMP2,*) VALUES(I)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read values('//TRIM(ADJUSTL(TMP1))//')='//TRIM(ADJUSTL(TMP2)) )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Guard do not match, expected "GUARD_SP" got "'//GUARD//'"' )
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

END SUBROUTINE VAL_READ_SP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'VAL_WRITE_DP'
SUBROUTINE VAL_WRITE_DP( VALUNIT, VALUES, WRITE_POS )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPRD_K
  USE :: OM_CORE_MOD, ONLY: VALUES_DP_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),            INTENT(IN)  :: VALUNIT
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(IN)  :: VALUES
  INTEGER(KIND=INT64),             INTENT(OUT) :: WRITE_POS

  ! Local variables
  LOGICAL :: VALOPENED
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local parameters
  CHARACTER(LEN=*), PARAMETER :: GUARD='GUARD_DP'

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=VALUNIT, OPENED=VALOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.VALOPENED, 1)

  ! Get the position in the file
  INQUIRE( VALUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

  ! Write the current step of the simulation and trigger restart
  WRITE( VALUNIT, IOSTAT=STAT ) INT( VALUES_DP_E, INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)
  WRITE( VALUNIT, IOSTAT=STAT ) INT( LBOUND(VALUES,1), INT64 )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)
  WRITE( VALUNIT, IOSTAT=STAT ) INT( UBOUND(VALUES,1), INT64 )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 5)

  ! Write the current step
  DO I = LBOUND(VALUES,1), UBOUND(VALUES,1)
    WRITE( VALUNIT, IOSTAT=STAT ) REAL(VALUES(I), REAL64)
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 6)
  ENDDO

  ! Write guards
  WRITE( VALUNIT, IOSTAT=STAT ) GUARD
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 7)

  ! Perform a flush just to be sure
  FLUSH( VALUNIT )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=128) :: TMP1
    CHARACTER(LEN=128) :: TMP2

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Values file not opened or not exists' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the kind of values for the message' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write lower bound for the values' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write upper bound for the values' )
    CASE (6)
      TMP1 = REPEAT(' ', 1024)
      WRITE(TMP1,*) I
      TMP1 = REPEAT(' ', 1024)
      WRITE(TMP2,*) VALUES(I)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write values('//TRIM(ADJUSTL(TMP1))//')='//TRIM(ADJUSTL(TMP2)) )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write guard' )
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

END SUBROUTINE VAL_WRITE_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'VAL_READ_DP'
SUBROUTINE VAL_READ_DP( VALUNIT, READ_POS, VALUES )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPRD_K
  USE :: OM_CORE_MOD, ONLY: VALUES_DP_E

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),            INTENT(IN)  :: VALUNIT
  INTEGER(KIND=JPIB_K),            INTENT(IN)  :: READ_POS
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(OUT) :: VALUES

  ! Local variables
  LOGICAL :: VALOPENED
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=INT64)  :: PRECISION
  INTEGER(KIND=INT64)  :: LO
  INTEGER(KIND=INT64)  :: HI
  REAL(KIND=REAL64)    :: RTMP
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=8)     :: GUARD

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=VALUNIT, OPENED=VALOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.VALOPENED, 1)

  ! Write the current step of the simulation and trigger restart
  READ( VALUNIT, POS=READ_POS, IOSTAT=STAT ) PRECISION
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)
  PP_DEBUG_CRITICAL_COND_THROW( PRECISION.NE.VALUES_DP_E, 3 )
  READ( VALUNIT, IOSTAT=STAT ) LO
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)
  READ( VALUNIT, IOSTAT=STAT ) HI
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 5)

  ! Check memory size/status
  PP_DEBUG_CRITICAL_COND_THROW( LBOUND(VALUES,1).NE.LO, 6)
  PP_DEBUG_CRITICAL_COND_THROW( UBOUND(VALUES,1).NE.HI, 7)

  ! Write the current step
  DO I = LO, HI
    READ( VALUNIT, IOSTAT=STAT ) RTMP
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 8)
    VALUES(I) = REAL( RTMP, KIND(VALUES(I)) )
  ENDDO

  ! Read the guard
  READ( VALUNIT, IOSTAT=STAT ) GUARD
  PP_DEBUG_CRITICAL_COND_THROW( GUARD.NE.'GUARD_DP', 9)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR
    CHARACTER(LEN=1024) :: TMP1
    CHARACTER(LEN=1024) :: TMP2

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Values file not opened or not exists' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read the values precision' )
    CASE (3)
      TMP1 = REPEAT(' ', 1024)
      WRITE(TMP1,*) VALUES_DP_E
      TMP1 = REPEAT(' ', 1024)
      WRITE(TMP2,*) PRECISION
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Precision do not match, expected: '//TRIM(ADJUSTL(TMP1))//', got: '//TRIM(ADJUSTL(TMP2)) )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lower bound' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read upper bound' )
    CASE (6)
      TMP1 = REPEAT(' ', 1024)
      WRITE(TMP1,*) LO
      TMP1 = REPEAT(' ', 1024)
      WRITE(TMP2,*) LBOUND(VALUES,1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Lower bound do not match, expected: '//TRIM(ADJUSTL(TMP1))//', got: '//TRIM(ADJUSTL(TMP2)) )
    CASE (7)
      TMP1 = REPEAT(' ', 1024)
      WRITE(TMP1,*) HI
      TMP1 = REPEAT(' ', 1024)
      WRITE(TMP2,*) UBOUND(VALUES,1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Upper bound do not match, expected: '//TRIM(ADJUSTL(TMP1))//', got: '//TRIM(ADJUSTL(TMP2)) )
    CASE (8)
      TMP1 = REPEAT(' ', 1024)
      WRITE(TMP1,*) I
      TMP1 = REPEAT(' ', 1024)
      WRITE(TMP2,*) VALUES(I)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read values('//TRIM(ADJUSTL(TMP1))//')='//TRIM(ADJUSTL(TMP2)) )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Guard do not match, expected "GUARD_DP" got "'//GUARD//'"' )
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

END SUBROUTINE VAL_READ_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'VAL_GENERATE_SP'
SUBROUTINE VAL_GENERATE_SP( NVALUES, MINVAL, MAXVAL, AVGVAL, NUNDEF, XUNDEF, VALUES )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPRD_K
  USE :: OM_CORE_MOD, ONLY: JPRM_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),            INTENT(IN)  :: NVALUES
  REAL(KIND=JPRD_K),               INTENT(IN)  :: MINVAL
  REAL(KIND=JPRD_K),               INTENT(IN)  :: MAXVAL
  REAL(KIND=JPRD_K),               INTENT(IN)  :: AVGVAL
  INTEGER(KIND=JPIB_K),            INTENT(IN)  :: NUNDEF
  REAL(KIND=JPRD_K),               INTENT(IN)  :: XUNDEF
  REAL(KIND=JPRM_K), DIMENSION(:), INTENT(OUT) :: VALUES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  VALUES = 1.0_JPRM_K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE VAL_GENERATE_SP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'VAL_GENERATE_DP'
SUBROUTINE VAL_GENERATE_DP( NVALUES, MINVAL, MAXVAL, AVGVAL, NUNDEF, XUNDEF, VALUES )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPRD_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),            INTENT(IN)  :: NVALUES
  REAL(KIND=JPRD_K),               INTENT(IN)  :: MINVAL
  REAL(KIND=JPRD_K),               INTENT(IN)  :: MAXVAL
  REAL(KIND=JPRD_K),               INTENT(IN)  :: AVGVAL
  INTEGER(KIND=JPIB_K),            INTENT(IN)  :: NUNDEF
  REAL(KIND=JPRD_K),               INTENT(IN)  :: XUNDEF
  REAL(KIND=JPRD_K), DIMENSION(:), INTENT(OUT) :: VALUES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  VALUES = 1.0_JPRD_K

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE VAL_GENERATE_DP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE VAL_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME