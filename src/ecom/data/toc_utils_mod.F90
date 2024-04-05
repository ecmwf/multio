! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'toc_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'TOC_UTILS_MOD'
MODULE TOC_UTILS_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

IMPLICIT NONE

! Enumerators
INTEGER(KIND=JPIB_K), PARAMETER :: UNDEF_E=-1
INTEGER(KIND=JPIB_K), PARAMETER :: SIM_INIT_E=0

INTEGER(KIND=JPIB_K), PARAMETER :: ATM_FIELD_E=1
INTEGER(KIND=JPIB_K), PARAMETER :: WAM_FIELD_E=2

INTEGER(KIND=JPIB_K), PARAMETER :: FLUSH_STEP_E=3
INTEGER(KIND=JPIB_K), PARAMETER :: FLUSH_STEP_RST_E=4
INTEGER(KIND=JPIB_K), PARAMETER :: FLUSH_STEP_LAST_E=5

INTEGER(KIND=JPIB_K), PARAMETER :: SIM_END_E=6


TYPE :: TOC_ENTRY_BASE_T
  INTEGER(KIND=JPIB_K) :: TYPE_
  INTEGER(KIND=JPIB_K) :: CLOCK_RATE_
END TYPE

TYPE, EXTENDS(TOC_ENTRY_BASE_T) :: TOC_SIM_INIT_T
  INTEGER(KIND=JPIB_K) :: CLOCK_COUNT_
END TYPE

TYPE, EXTENDS(TOC_ENTRY_BASE_T) :: TOC_ATM_FIELD_T
  INTEGER(KIND=JPIB_K) :: PARAM_ID_
  INTEGER(KIND=JPIB_K) :: U_ID_
  INTEGER(KIND=JPIB_K) :: STEP_ID_
  INTEGER(KIND=JPIB_K) :: PROC_ID_
  INTEGER(KIND=JPIB_K) :: REPRES_ID_
  INTEGER(KIND=JPIB_K) :: PREFIX_ID_
  INTEGER(KIND=JPIB_K) :: MSG_ADDR_
  INTEGER(KIND=JPIB_K) :: VAL_TYPE_
  INTEGER(KIND=JPIB_K) :: VAL_LB_
  INTEGER(KIND=JPIB_K) :: VAL_UB_
  INTEGER(KIND=JPIB_K) :: VAL_ADDR_
  INTEGER(KIND=JPIB_K) :: CLOCK_COUNT_
  INTEGER(KIND=JPIB_K) :: MSG_ID_
END TYPE

TYPE, EXTENDS(TOC_ENTRY_BASE_T) :: TOC_WAM_FIELD_T
  INTEGER(KIND=JPIB_K) :: PARAM_ID_
  INTEGER(KIND=JPIB_K) :: U_ID_
  INTEGER(KIND=JPIB_K) :: STEP_ID_
  INTEGER(KIND=JPIB_K) :: PROC_ID_
  INTEGER(KIND=JPIB_K) :: REPRES_ID_
  INTEGER(KIND=JPIB_K) :: PREFIX_ID_
  INTEGER(KIND=JPIB_K) :: MSG_ADDR_
  INTEGER(KIND=JPIB_K) :: VAL_TYPE_
  INTEGER(KIND=JPIB_K) :: VAL_LB_
  INTEGER(KIND=JPIB_K) :: VAL_UB_
  INTEGER(KIND=JPIB_K) :: VAL_ADDR_
  INTEGER(KIND=JPIB_K) :: CLOCK_COUNT_
  INTEGER(KIND=JPIB_K) :: MSG_ID_
END TYPE

TYPE, EXTENDS(TOC_ENTRY_BASE_T) :: TOC_FLUSH_STEP_T
  INTEGER(KIND=JPIB_K) :: STEP_
  INTEGER(KIND=JPIB_K) :: CLOCK_COUNT_
END TYPE

TYPE, EXTENDS(TOC_ENTRY_BASE_T) :: TOC_FLUSH_STEP_RST_T
  INTEGER(KIND=JPIB_K) :: STEP_
  INTEGER(KIND=JPIB_K) :: CLOCK_COUNT_
END TYPE

TYPE, EXTENDS(TOC_ENTRY_BASE_T) :: TOC_FLUSH_LAST_STEP_T
  INTEGER(KIND=JPIB_K) :: STEP_
  INTEGER(KIND=JPIB_K) :: CLOCK_COUNT_
END TYPE

TYPE, EXTENDS(TOC_ENTRY_BASE_T) :: TOC_SIM_END_T
  INTEGER(KIND=JPIB_K) :: CLOCK_COUNT_
END TYPE

TYPE :: TOC_CONTAINER_T
  CLASS(TOC_ENTRY_BASE_T), POINTER :: ENTRY_ => NULL()
END TYPE

! Whitelist of public symbols (datatypes)
PUBLIC :: TOC_ENTRY_BASE_T
PUBLIC :: TOC_SIM_INIT_T
PUBLIC :: TOC_ATM_FIELD_T
PUBLIC :: TOC_WAM_FIELD_T
PUBLIC :: TOC_FLUSH_STEP_T
PUBLIC :: TOC_FLUSH_STEP_RST_T
PUBLIC :: TOC_FLUSH_LAST_STEP_T
PUBLIC :: TOC_SIM_END_T
PUBLIC :: TOC_CONTAINER_T


!  Whitelist of public sybols (procedures)
PUBLIC :: TOC_CREATE_NAME
PUBLIC :: TOC_WOPEN
PUBLIC :: TOC_WRITE_FLUSH_BEGIN_OF_SIMULATION
PUBLIC :: TOC_WRITE_ATM
PUBLIC :: TOC_WRITE_WAM
PUBLIC :: TOC_WRITE_FLUSH_STEP
PUBLIC :: TOC_WRITE_FLUSH_STEP_AND_RESTART
PUBLIC :: TOC_WRITE_FLUSH_LAST_STEP
PUBLIC :: TOC_WRITE_FLUSH_END_OF_SIMULATION
PUBLIC :: TOC_READ
PUBLIC :: TOC_READ_ALL
PUBLIC :: TOC_FREE

CONTAINS





#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_CREATE_NAME'
SUBROUTINE TOC_CREATE_NAME( DIRECTORY, PROC_ID, TOCFNAME )

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
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: PROC_ID
  CHARACTER(LEN=*),     INTENT(OUT) :: TOCFNAME

  ! Local variables
  LOGICAL :: MSGEXIST
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
  N = LEN(TOCFNAME)
  M = LEN_TRIM(DIRECTORY) + 17
  PP_DEBUG_CRITICAL_COND_THROW( M.GT.N, 1)

  ! Create the message name
  TOCFNAME = REPEAT(' ',N)
  WRITE(TOCFNAME,'(A,A,I8.8,A)', IOSTAT=STAT) TRIM(ADJUSTL(DIRECTORY)), '/toc_', PROC_ID, '.bin'
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create the msg file name' )
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

END SUBROUTINE TOC_CREATE_NAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_WOPEN'
SUBROUTINE TOC_WOPEN( TOCFNAME, TOCUNIT, WRITE_POS, TOC_COUNTER )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)    :: TOCFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER

  ! Local variables
  LOGICAL :: TOCEXIST
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( FILE=TRIM(TOCFNAME), EXIST=TOCEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( TOCEXIST, 1)

  ! Open the TOC file
  OPEN( NEWUNIT=TOCUNIT, FILE=TRIM(TOCFNAME), STATUS='REPLACE', ACCESS='STREAM', ACTION='WRITE', FORM='UNFORMATTED', IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

  ! Make spaces for the tocsize
  TOC_COUNTER = 0
  WRITE(TOCUNIT, IOSTAT=STAT) INT(TOC_COUNTER,KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)

  ! Get the position inthe file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file already exists' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to open toc file' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write first element in the toc file' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position in the toc file' )
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

END SUBROUTINE TOC_WOPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_ROPEN'
FUNCTION TOC_ROPEN( TOCFNAME, TOCUNIT, BIG_ENDIAN_READ ) RESULT(NENTRIES)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)  :: TOCFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: TOCUNIT
  LOGICAL,              INTENT(IN)  :: BIG_ENDIAN_READ

  ! Function result
  INTEGER(KIND=JPIB_K) :: NENTRIES

  ! Local variables
  LOGICAL :: TOCEXIST
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( FILE=TRIM(TOCFNAME), EXIST=TOCEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCEXIST, 1)

  ! Open the TOC file
  IF ( BIG_ENDIAN_READ ) THEN
    OPEN( NEWUNIT=TOCUNIT, FILE=TRIM(TOCFNAME), STATUS='OLD', ACCESS='STREAM', ACTION='READ', FORM='UNFORMATTED', CONVERT='BIG_ENDIAN', IOSTAT=STAT )
  ELSE
    OPEN( NEWUNIT=TOCUNIT, FILE=TRIM(TOCFNAME), STATUS='OLD', ACCESS='STREAM', ACTION='READ', FORM='UNFORMATTED', IOSTAT=STAT )
  ENDIF
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

  ! Make spaces for the tocsize
  NENTRIES = 0
  READ(TOCUNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)
  NENTRIES = INT( ITMP, KIND=KIND(NENTRIES) )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to find toc file: '//TRIM(TOCFNAME) )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to open toc file: '//TRIM(TOCFNAME) )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read the number of toc entries' )
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

END FUNCTION TOC_ROPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_READ'
SUBROUTINE TOC_READ( TOCFNAME, TOC, BIG_ENDIAN_READ, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                                 INTENT(IN)  :: TOCFNAME
  TYPE(TOC_CONTAINER_T), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: TOC
  LOGICAL,                                          INTENT(IN)  :: BIG_ENDIAN_READ
  LOGICAL,                                          INTENT(IN)  :: VERBOSE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: TOCUNIT
  INTEGER(KIND=JPIB_K) :: NENTRIES
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: ETRY_TYPE
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(TOC), 1)

  ! Open the toc file
  NENTRIES = TOC_ROPEN( TOCFNAME, TOCUNIT, BIG_ENDIAN_READ )
  PP_DEBUG_CRITICAL_COND_THROW( NENTRIES.LE.0, 2)

  ! Allocate the table of contents
  ALLOCATE( TOC(NENTRIES), STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)

  ! Read all the entries
  DO I = 1, NENTRIES
    ETRY_TYPE = TOC_READ_NEXT_ENTRY_TOC( TOCUNIT, TOC(I)%ENTRY_ )
  ENDDO

  ! Close the toc file
  CALL TOC_CLOSE( TOCUNIT )

  ! Deallocate the error message if allocated
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG)
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc already allocated' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Nentries in toc lower of equal to 0' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate toc' )
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

END SUBROUTINE TOC_READ
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_READ_ALL'
SUBROUTINE TOC_READ_ALL( DIRECTORY, TOC, NPROCS, BIG_ENDIAN_READ, VERBOSE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                                 INTENT(IN)  :: DIRECTORY
  TYPE(TOC_CONTAINER_T), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: TOC
  INTEGER(KIND=JPIB_K),                             INTENT(IN)  :: NPROCS
  LOGICAL,                                          INTENT(IN)  :: BIG_ENDIAN_READ
  LOGICAL,                                          INTENT(IN)  :: VERBOSE

  ! Local variables
  CHARACTER(LEN=1024) :: FULLDIR
  CHARACTER(LEN=1024) :: TOCFNAME
  INTEGER(KIND=JPIB_K), DIMENSION(NPROCS) :: TOCUNIT
  INTEGER(KIND=JPIB_K), DIMENSION(NPROCS) :: NENTRIES
  INTEGER(KIND=JPIB_K) :: TOTNENTRIES
  INTEGER(KIND=JPIB_K) :: PROCID
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: STAT
  CLASS(TOC_ENTRY_BASE_T), POINTER :: TMP_ENTRY => NULL()
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( ALLOCATED(TOC), 1)


  ! Open the toc file
  TOTNENTRIES = 0
  DO PROCID = 1, NPROCS
    FULLDIR = REPEAT(' ',1024)
    WRITE(FULLDIR,'(A,A,I6.6,A)') TRIM(ADJUSTL(DIRECTORY)), '/io_serv.', PROCID, '.d'
    CALL TOC_CREATE_NAME( TRIM(FULLDIR), PROCID, TOCFNAME )
    NENTRIES(PROCID) = TOC_ROPEN( TOCFNAME, TOCUNIT(PROCID), BIG_ENDIAN_READ )
    PP_DEBUG_CRITICAL_COND_THROW( NENTRIES(PROCID).LE.0, 2)
    TOTNENTRIES = TOTNENTRIES + NENTRIES(PROCID)
  ENDDO

  ! Allocate the table of contents
  ALLOCATE( TOC(TOTNENTRIES), STAT=STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)

  ! Read all the entries
  CNT = 0
  OuterLoop: DO
    ProcLoop: DO PROCID = 1, NPROCS
      InnerLoop: DO
        SELECT CASE ( TOC_READ_NEXT_ENTRY_TOC( TOCUNIT(PROCID), TMP_ENTRY ) )
        CASE ( SIM_INIT_E, FLUSH_STEP_E, FLUSH_STEP_RST_E, FLUSH_STEP_LAST_E )
          IF ( PROCID .EQ. NPROCS ) THEN
            CNT = CNT + 1
            TOC(CNT)%ENTRY_ => TMP_ENTRY
            NULLIFY(TMP_ENTRY)
          ELSE
            IF ( ASSOCIATED(TMP_ENTRY) ) THEN
              DEALLOCATE(TMP_ENTRY)
              NULLIFY(TMP_ENTRY)
            ENDIF
          ENDIF
          EXIT InnerLoop
        CASE ( ATM_FIELD_E, WAM_FIELD_E )
          CNT = CNT + 1
          TOC(CNT)%ENTRY_ => TMP_ENTRY
          NULLIFY(TMP_ENTRY)
        CASE ( SIM_END_E )
          IF ( PROCID .EQ. NPROCS ) THEN
            CNT = CNT + 1
            TOC(CNT)%ENTRY_ => TMP_ENTRY
            NULLIFY(TMP_ENTRY)
          ELSE
            IF ( ASSOCIATED(TMP_ENTRY) ) THEN
              DEALLOCATE(TMP_ENTRY)
              NULLIFY(TMP_ENTRY)
            ENDIF
          ENDIF
          IF ( PROCID .EQ. NPROCS ) THEN
            EXIT OuterLoop
          ELSE
            EXIT InnerLoop
          ENDIF
        CASE DEFAULT
          PP_DEBUG_CRITICAL_THROW(4)
        END SELECT
      ENDDO InnerLoop
    ENDDO ProcLoop
  ENDDO OuterLoop

  ! Close the toc file
  DO PROCID = 1, NPROCS
    CALL TOC_CLOSE( TOCUNIT(PROCID) )
  ENDDO

  ! Deallocate the error message if allocated
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG)
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc already allocated' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Nentries in toc lower of equal to 0' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate toc' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unknown entry' )
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

END SUBROUTINE TOC_READ_ALL
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_FREE'
SUBROUTINE TOC_FREE( TOC )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(TOC_CONTAINER_T), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: TOC

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  IF ( ALLOCATED(TOC) ) THEN
    DO I = 1, SIZE(TOC)
      IF ( ASSOCIATED(TOC(I)%ENTRY_) ) THEN
        DEALLOCATE( TOC(I)%ENTRY_, STAT=STAT )
        PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 1)
        NULLIFY(TOC(I)%ENTRY_)
      ENDIF
    ENDDO
    DEALLOCATE( TOC, STAT=STAT )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to allocate a toc entry' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to deallocate toc' )
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

END SUBROUTINE TOC_FREE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'TOC_READ_NEXT_ENTRY_TOC'
FUNCTION TOC_READ_NEXT_ENTRY_TOC( TOCUNIT, NEXT_ENTRY ) RESULT(NEXT_ENTRY_E)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),             INTENT(IN)    :: TOCUNIT
  CLASS(TOC_ENTRY_BASE_T), POINTER, INTENT(INOUT) :: NEXT_ENTRY

  ! Function result
  INTEGER(KIND=JPIB_K) :: NEXT_ENTRY_E

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check the status of the next entry
  PP_DEBUG_CRITICAL_COND_THROW( ASSOCIATED(NEXT_ENTRY), 1 )

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 2)


  ! Make spaces for the tocsize
  NEXT_ENTRY_E = UNDEF_E
  READ(TOCUNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)
  NEXT_ENTRY_E = INT( ITMP, KIND=KIND(NEXT_ENTRY_E) )
  ! WRITE(*,*) ' + Read the next entry: ', NEXT_ENTRY_E

  ! Depending on the value of the next entry, associate the proper entry and call the specialised readed
  SELECT CASE(NEXT_ENTRY_E)

  CASE (SIM_INIT_E) ! 0
    ! WRITE(*,*) ' + Read SIM_INIT_E'
    ALLOCATE( TOC_SIM_INIT_T::NEXT_ENTRY, STAT=STAT, ERRMSG=ERRMSG  )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)
    NEXT_ENTRY%TYPE_ = SIM_INIT_E
    SELECT TYPE ( A => NEXT_ENTRY )
    CLASS IS ( TOC_SIM_INIT_T )
      CALL TOC_READ_FLUSH_BEGIN_OF_SIMULATION( TOCUNIT, A )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( 4)
    END SELECT

  CASE (ATM_FIELD_E) ! 1
    ! WRITE(*,*) ' + Read ATM_FIELD_E'
    ALLOCATE( TOC_ATM_FIELD_T::NEXT_ENTRY, STAT=STAT, ERRMSG=ERRMSG  )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 5)
    NEXT_ENTRY%TYPE_ = ATM_FIELD_E
    SELECT TYPE ( A => NEXT_ENTRY )
    CLASS IS ( TOC_ATM_FIELD_T )
      CALL TOC_READ_ATM( TOCUNIT, A )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( 5)
    END SELECT

  CASE (WAM_FIELD_E) ! 2
    ! WRITE(*,*) ' + Read WAM_FIELD_E'
    ALLOCATE( TOC_WAM_FIELD_T::NEXT_ENTRY, STAT=STAT, ERRMSG=ERRMSG  )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 6)
    NEXT_ENTRY%TYPE_ = WAM_FIELD_E
    SELECT TYPE ( A => NEXT_ENTRY )
    CLASS IS ( TOC_WAM_FIELD_T )
      CALL TOC_READ_WAM( TOCUNIT, A )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( 6)
    END SELECT

  CASE (FLUSH_STEP_E) ! 3
    ! WRITE(*,*) ' + Read FLUSH_STEP_E'
    ALLOCATE( TOC_FLUSH_STEP_T::NEXT_ENTRY, STAT=STAT, ERRMSG=ERRMSG  )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 7)
    NEXT_ENTRY%TYPE_ = FLUSH_STEP_E
    SELECT TYPE ( A => NEXT_ENTRY )
    CLASS IS ( TOC_FLUSH_STEP_T )
      CALL TOC_READ_FLUSH_STEP( TOCUNIT, A )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( 7)
    END SELECT

  CASE (FLUSH_STEP_RST_E) ! 4
    ! WRITE(*,*) ' + Read FLUSH_STEP_RST_E'
    ALLOCATE( TOC_FLUSH_STEP_RST_T::NEXT_ENTRY, STAT=STAT, ERRMSG=ERRMSG  )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 8)
    NEXT_ENTRY%TYPE_ = FLUSH_STEP_RST_E
    SELECT TYPE ( A => NEXT_ENTRY )
    CLASS IS ( TOC_FLUSH_STEP_RST_T )
      CALL TOC_READ_FLUSH_STEP_AND_RESTART( TOCUNIT, A )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( 8)
    END SELECT

  CASE (FLUSH_STEP_LAST_E) ! 5
    ! WRITE(*,*) ' + Read FLUSH_STEP_LAST_E'
    ALLOCATE( TOC_FLUSH_LAST_STEP_T::NEXT_ENTRY, STAT=STAT, ERRMSG=ERRMSG  )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 9)
    NEXT_ENTRY%TYPE_ = FLUSH_STEP_LAST_E
    SELECT TYPE ( A => NEXT_ENTRY )
    CLASS IS ( TOC_FLUSH_LAST_STEP_T )
      CALL TOC_READ_FLUSH_LAST_STEP( TOCUNIT, A )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( 9)
    END SELECT

  CASE (SIM_END_E) ! 6
    ! WRITE(*,*) ' + Read SIM_END_E'
    ALLOCATE( TOC_SIM_END_T::NEXT_ENTRY, STAT=STAT, ERRMSG=ERRMSG  )
    PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 10)
    NEXT_ENTRY%TYPE_ = SIM_END_E
    SELECT TYPE ( A => NEXT_ENTRY )
    CLASS IS ( TOC_SIM_END_T )
      CALL TOC_READ_FLUSH_END_OF_SIMULATION( TOCUNIT, A )
    CLASS DEFAULT
      PP_DEBUG_CRITICAL_THROW( 10)
    END SELECT

  CASE (UNDEF_E) ! -1

    PP_DEBUG_CRITICAL_THROW( 11 )

  CASE DEFAULT

    PP_DEBUG_CRITICAL_THROW( 12 )

  END SELECT

  ! Deallocate the error message if allocated
  IF ( ALLOCATED(ERRMSG) ) THEN
    DEALLOCATE(ERRMSG)
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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc entry already associated' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read toc kind' )
    CASE (4)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating \"SIM_INIT_E\":'//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating \"SIM_INIT_E\".' )
      ENDIF
    CASE (5)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating \"ATM_FIELD_E\":'//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating \"ATM_FIELD_E\".' )
      ENDIF
    CASE (6)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating \"WAM_FIELD_E\":'//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating \"WAM_FIELD_E\".' )
      ENDIF
    CASE (7)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating \"FLUSH_STEP_E\":'//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating \"FLUSH_STEP_E\".' )
      ENDIF
    CASE (8)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating \"FLUSH_STEP_RST_E\":'//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating \"FLUSH_STEP_RST_E\".' )
      ENDIF
    CASE (9)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating \"FLUSH_STEP_LAST_E\":'//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating \"FLUSH_STEP_LAST_E\".' )
      ENDIF
    CASE (10)
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating \"\SIM_END_E":'//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG)
      ELSE
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Error allocating \"SIM_END_E\".' )
      ENDIF
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc entry is set to undefined' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unexpet toc entry' )
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

END FUNCTION TOC_READ_NEXT_ENTRY_TOC
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_READ_FLUSH_BEGIN_OF_SIMULATION'
SUBROUTINE TOC_READ_FLUSH_BEGIN_OF_SIMULATION( TOCUNIT, TOC_ENTRY )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),  INTENT(IN)  :: TOCUNIT
  CLASS(TOC_SIM_INIT_T), INTENT(OUT) :: TOC_ENTRY

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  ! Initialise the toc entry
  TOC_ENTRY%CLOCK_COUNT_ = 0

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
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
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

END SUBROUTINE TOC_READ_FLUSH_BEGIN_OF_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'WRITE_FLUSH_BEGIN_OF_SIMULATION'
SUBROUTINE TOC_WRITE_FLUSH_BEGIN_OF_SIMULATION( TOCUNIT, WRITE_POS, TOC_COUNTER )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  ! Write the beginning of the simulation
  WRITE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT ) INT(SIM_INIT_E, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

  ! Get the position inthe file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)

  ! Write the beginning of the simulation
  TOC_COUNTER = TOC_COUNTER + 1
  WRITE(TOCUNIT, POS=1, IOSTAT=STAT ) INT( TOC_COUNTER, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
      CASE (2)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the toc entry in the file' )
      CASE (3)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position in the toc file' )
      CASE (4)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position n the toc' )
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

END SUBROUTINE TOC_WRITE_FLUSH_BEGIN_OF_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_READ_ATM'
SUBROUTINE TOC_READ_ATM( TOCUNIT, TOC_ENTRY )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),   INTENT(IN)  :: TOCUNIT
  CLASS(TOC_ATM_FIELD_T), INTENT(OUT) :: TOC_ENTRY

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  ! Read the wam informations
  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)
  TOC_ENTRY%PARAM_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%PARAM_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)
  TOC_ENTRY%U_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%U_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)
  TOC_ENTRY%STEP_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%STEP_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 5)
  TOC_ENTRY%PROC_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%PROC_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 6)
  TOC_ENTRY%REPRES_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%REPRES_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 7)
  TOC_ENTRY%PREFIX_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%PREFIX_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 8)
  TOC_ENTRY%MSG_ADDR_ = INT( ITMP, KIND=KIND(TOC_ENTRY%MSG_ADDR_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 9)
  TOC_ENTRY%VAL_TYPE_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_TYPE_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 9)
  TOC_ENTRY%VAL_LB_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_LB_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 9)
  TOC_ENTRY%VAL_UB_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_UB_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 10)
  TOC_ENTRY%VAL_ADDR_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_ADDR_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 11)
  TOC_ENTRY%MSG_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%MSG_ID_) )

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
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
      CASE (2)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read paramId' )
      CASE (3)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read uId' )
      CASE (4)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read stepId' )
      CASE (5)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read procId' )
      CASE (6)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read representation' )
      CASE (7)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read prefix' )
      CASE (8)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read message address' )
      CASE (9)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read value type' )
      CASE (10)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lower bound' )
      CASE (11)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read upper bound' )
      CASE (12)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read value address' )
      CASE (13)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read msgID' )
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

END SUBROUTINE TOC_READ_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'WRITE_ATM_TO_TOC'
SUBROUTINE TOC_WRITE_ATM( TOCUNIT, PARAM_ID, U_ID, STEP_ID, PROC_ID, &
& REPRES_ID, PREFIX_ID, MSG_ADDR, VAL_TYPE, VAL_LB, VAL_UB, VAL_ADDR, &
& MSG_ID, WRITE_POS, TOC_COUNTER )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PARAM_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: U_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PROC_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: REPRES_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PREFIX_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: MSG_ADDR
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_TYPE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_LB
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_UB
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_ADDR
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: MSG_ID
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  FLUSH( TOCUNIT )

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  ! Write the current step of the simulation and trigger restart
  WRITE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT ) INT(ATM_FIELD_E,KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

  FLUSH( TOCUNIT )

  ! Write the current step
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( PARAM_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( U_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( STEP_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 5)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( PROC_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 6)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( REPRES_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 7)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( PREFIX_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 8)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( MSG_ADDR, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 9)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_TYPE, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 10)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_LB, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 11)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_UB, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 12)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_ADDR, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 13)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( MSG_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 14)

  ! Get the position in the file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 13)

  ! Update the number of tocs in the toc file header
  TOC_COUNTER = TOC_COUNTER + 1
  WRITE(TOCUNIT, POS=1, IOSTAT=STAT ) INT(TOC_COUNTER, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 14)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the toc entry in the file' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the paramid in the toc file' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the unique id in the toc file' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the step in the toc file' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the process idx in the toc file' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the representation idx in the toc file' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the prefix idx in the toc file' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the message address in the toc file' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the value type in the toc file' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the lower bound of the data' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the upper bound of the data' )
    CASE (13)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the value addres idx in the toc file' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the message idx in the toc file' )
    CASE (15)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position in the toc file' )
    CASE (16)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position n the toc' )
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

END SUBROUTINE TOC_WRITE_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_READ_WAM'
SUBROUTINE TOC_READ_WAM( TOCUNIT, TOC_ENTRY )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),   INTENT(IN)  :: TOCUNIT
  CLASS(TOC_WAM_FIELD_T), INTENT(OUT) :: TOC_ENTRY

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  ! Read the wam informations
  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)
  TOC_ENTRY%PARAM_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%PARAM_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)
  TOC_ENTRY%U_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%U_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)
  TOC_ENTRY%STEP_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%STEP_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 5)
  TOC_ENTRY%PROC_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%PROC_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 6)
  TOC_ENTRY%REPRES_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%REPRES_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 7)
  TOC_ENTRY%PREFIX_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%PREFIX_ID_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 8)
  TOC_ENTRY%MSG_ADDR_ = INT( ITMP, KIND=KIND(TOC_ENTRY%MSG_ADDR_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 9)
  TOC_ENTRY%VAL_TYPE_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_TYPE_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 10)
  TOC_ENTRY%VAL_LB_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_LB_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 11)
  TOC_ENTRY%VAL_UB_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_UB_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 12)
  TOC_ENTRY%VAL_ADDR_ = INT( ITMP, KIND=KIND(TOC_ENTRY%VAL_ADDR_) )

  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 13)
  TOC_ENTRY%MSG_ID_ = INT( ITMP, KIND=KIND(TOC_ENTRY%MSG_ID_) )

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
          PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
        CASE (2)
          PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read paramId' )
        CASE (3)
          PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read uId' )
        CASE (4)
          PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read stepId' )
        CASE (5)
          PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read procId' )
        CASE (6)
          PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read representation' )
        CASE (7)
          PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read prefix' )
        CASE (8)
          PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read message address' )
        CASE (9)
          PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read value type' )
        CASE (10)
          PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read lower bound' )
        CASE (11)
          PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read upper bound' )
        CASE (12)
          PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read value address' )
        CASE (13)
          PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read msgID' )
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

END SUBROUTINE TOC_READ_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_WRITE_WAM'
SUBROUTINE TOC_WRITE_WAM( TOCUNIT, PARAM_ID, U_ID, STEP_ID, PROC_ID, &
& REPRES_ID, PREFIX_ID, MSG_ADDR, VAL_TYPE, VAL_LB, VAL_UB, VAL_ADDR, &
& MSG_ID, WRITE_POS, TOC_COUNTER )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PARAM_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: U_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PROC_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: REPRES_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PREFIX_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: MSG_ADDR
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_TYPE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_LB
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_UB
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: VAL_ADDR
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: MSG_ID
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  FLUSH( TOCUNIT )

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  ! Write the current step of the simulation and trigger restart
  WRITE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT ) INT(WAM_FIELD_E,INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

  FLUSH( TOCUNIT )

  ! Write the current step
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( PARAM_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( U_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( STEP_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 5)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( PROC_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 6)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( REPRES_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 7)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( PREFIX_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 8)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( MSG_ADDR, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 9)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_TYPE, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 10)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_LB, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 11)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_UB, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 12)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( VAL_ADDR, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 13)
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( MSG_ID, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 14)

  ! Get the position in the file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 15)

  ! Update the number of tocs in the toc file header
  TOC_COUNTER = TOC_COUNTER + 1
  WRITE(TOCUNIT, POS=1, IOSTAT=STAT ) INT(TOC_COUNTER, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 16)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the toc entry in the file' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the paramid in the toc file' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the unique id in the toc file' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the step in the toc file' )
    CASE (6)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the process idx in the toc file' )
    CASE (7)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the representation idx in the toc file' )
    CASE (8)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the prefix idx in the toc file' )
    CASE (9)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the message address in the toc file' )
    CASE (10)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the value type in the toc file' )
    CASE (11)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the lower bound of the data' )
    CASE (12)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the upper bound of the data' )
    CASE (13)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the value addres idx in the toc file' )
    CASE (14)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the message idx in the toc file' )
    CASE (15)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position in the toc file' )
    CASE (16)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position n the toc' )
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

END SUBROUTINE TOC_WRITE_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_READ_FLUSH_STEP'
SUBROUTINE TOC_READ_FLUSH_STEP( TOCUNIT, TOC_ENTRY )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),    INTENT(IN)  :: TOCUNIT
  CLASS(TOC_FLUSH_STEP_T), INTENT(OUT) :: TOC_ENTRY

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  ! Read the last step
  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)
  TOC_ENTRY%STEP_ = INT( ITMP, KIND=KIND(TOC_ENTRY%STEP_) )

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
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
      CASE (2)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read step' )
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

END SUBROUTINE TOC_READ_FLUSH_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_WRITE_FLUSH_STEP'
SUBROUTINE TOC_WRITE_FLUSH_STEP( TOCUNIT, ISTEP, WRITE_POS, TOC_COUNTER )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ISTEP
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  ! Write the current step of the simulation and trigger restart
  WRITE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT ) INT(FLUSH_STEP_E,KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

  ! Write the current step
  WRITE( TOCUNIT, IOSTAT=STAT ) INT(ISTEP,KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)

  ! Get the position in the file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)

  ! Update the number of tocs in the toc file header
  TOC_COUNTER = TOC_COUNTER + 1
  WRITE(TOCUNIT, POS=1, IOSTAT=STAT ) INT(TOC_COUNTER, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 5)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the toc entry in the file' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the last step in the toc file' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position in the toc file' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position n the toc' )
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

END SUBROUTINE TOC_WRITE_FLUSH_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_READ_FLUSH_STEP_AND_RESTART'
SUBROUTINE TOC_READ_FLUSH_STEP_AND_RESTART( TOCUNIT, TOC_ENTRY )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),        INTENT(IN)  :: TOCUNIT
  CLASS(TOC_FLUSH_STEP_RST_T), INTENT(OUT) :: TOC_ENTRY

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  ! Read the last step
  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)
  TOC_ENTRY%STEP_ = INT( ITMP, KIND=KIND(TOC_ENTRY%STEP_) )

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
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
      CASE (2)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read step' )
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

END SUBROUTINE TOC_READ_FLUSH_STEP_AND_RESTART
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'WRITE_FLUSH_STEP_AND_RESTART'
SUBROUTINE TOC_WRITE_FLUSH_STEP_AND_RESTART( TOCUNIT, ISTEP, WRITE_POS, TOC_COUNTER )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ISTEP
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  ! Write the current step of the simulation and trigger restart
  WRITE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT ) INT( FLUSH_STEP_RST_E, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

  ! Write the current step
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( ISTEP, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)

  ! Get the position in the file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)

  ! Update the number of tocs in the toc file header
  TOC_COUNTER = TOC_COUNTER + 1
  WRITE(TOCUNIT, POS=1, IOSTAT=STAT ) INT(TOC_COUNTER, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 5)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the toc entry in the file' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the last step in the toc file' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position in the toc file' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position n the toc' )
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

END SUBROUTINE TOC_WRITE_FLUSH_STEP_AND_RESTART
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_READ_FLUSH_LAST_STEP'
SUBROUTINE TOC_READ_FLUSH_LAST_STEP( TOCUNIT, TOC_ENTRY )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),         INTENT(IN)  :: TOCUNIT
  CLASS(TOC_FLUSH_LAST_STEP_T), INTENT(OUT) :: TOC_ENTRY

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  ! Read the last step
  READ( TOCUNIT, IOSTAT=STAT ) ITMP
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)
  TOC_ENTRY%STEP_ = INT( ITMP, KIND=KIND(TOC_ENTRY%STEP_) )


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
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
      CASE (2)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read step' )
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

END SUBROUTINE TOC_READ_FLUSH_LAST_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_WRITE_FLUSH_LAST_STEP'
SUBROUTINE TOC_WRITE_FLUSH_LAST_STEP( TOCUNIT, ISTEP, WRITE_POS, TOC_COUNTER )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: ISTEP
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  ! Write the last step of the simulation
  WRITE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT ) INT( FLUSH_STEP_LAST_E, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

  ! Write the last step
  WRITE( TOCUNIT, IOSTAT=STAT ) INT( ISTEP, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)

  ! Get the position in the file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)

  ! Update the number of tocs in the toc file header
  TOC_COUNTER = TOC_COUNTER + 1
  WRITE(TOCUNIT, POS=1, IOSTAT=STAT ) INT(TOC_COUNTER, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 5)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the toc entry in the file' )
    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the last step in the toc file' )
    CASE (4)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position in the toc file' )
    CASE (5)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position n the toc' )
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

END SUBROUTINE TOC_WRITE_FLUSH_LAST_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_READ_FLUSH_END_OF_SIMULATION'
SUBROUTINE TOC_READ_FLUSH_END_OF_SIMULATION( TOCUNIT, TOC_ENTRY )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: TOCUNIT
  CLASS(TOC_SIM_END_T), INTENT(OUT) :: TOC_ENTRY

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  TOC_ENTRY%CLOCK_COUNT_ = 1

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
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
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

END SUBROUTINE TOC_READ_FLUSH_END_OF_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_WRITE_FLUSH_END_OF_SIMULATION'
SUBROUTINE TOC_WRITE_FLUSH_END_OF_SIMULATION( TOCUNIT, WRITE_POS, TOC_COUNTER )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: TOCUNIT
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: WRITE_POS
  INTEGER(KIND=JPIB_K), INTENT(INOUT) :: TOC_COUNTER

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  ! Write the end of the simulation
  WRITE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT ) INT(SIM_END_E,KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 2)

  ! Get the position inthe file
  INQUIRE( TOCUNIT, POS=WRITE_POS, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3)

  ! Update the number of tocs in the toc file header
  TOC_COUNTER = TOC_COUNTER + 1
  WRITE(TOCUNIT, POS=1, IOSTAT=STAT ) INT(TOC_COUNTER, KIND=INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 4)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

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
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
      CASE (2)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to write the toc entry in the file' )
      CASE (3)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position in the toc file' )
      CASE (4)
        PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to inquire the current position n the toc' )
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

END SUBROUTINE TOC_WRITE_FLUSH_END_OF_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'TOC_CLOSE'
SUBROUTINE TOC_CLOSE( TOCUNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: TOCUNIT

  ! Local variables
  LOGICAL :: TOCOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the file is opened
  INQUIRE( UNIT=TOCUNIT, OPENED=TOCOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.TOCOPENED, 1)

  ! Perform a flush just to be sure
  FLUSH( TOCUNIT )

  ! Close the toc file and flush it
  CLOSE( TOCUNIT, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 1)

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Toc file not opened or not exists' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to close the file' )
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

END SUBROUTINE TOC_CLOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE TOC_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
