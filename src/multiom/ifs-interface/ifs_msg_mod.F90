!> @file om_message_mod.F90
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
#define PP_FILE_NAME 'ifs_msg_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'IFS_MSG_MOD'
MODULE IFS_MSG_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K

IMPLICIT NONE

PRIVATE

!> brief base class used to represent messages
TYPE :: OM_BASE_MSG_A

  !> @brief unique identifier of the message (level for atmosphere, a combination of freq. and angle for wave)
  INTEGER(KIND=JPIB_K) :: IUID_

  !> @brief grib prameter idntifier of the field
  INTEGER(KIND=JPIB_K) :: PARAM_ID_

  !> @brief Step of the simulation
  INTEGER(KIND=JPIB_K) :: ISTEP_

  !> @brief prefix that contain the type of level [sfc,m,p,t,v,wv_int,wv_spec]
  INTEGER(KIND=JPIB_K) :: IPREF_

  !> @brief Representation identifier [gridded,spectral] for the moment
  INTEGER(KIND=JPIB_K) :: IREPRES_

  !> @brief true if there are missing values in the message
  INTEGER(KIND=JPIB_K) :: NUNDF_

  !> @brief value of the missing values in the message
  REAL(KIND=JPRD_K) :: XUNDF_

  !> @brief minumum value of the values in the message
  REAL(KIND=JPRD_K) :: MINVAL_

  !> @brief maximum value of the values in the message
  REAL(KIND=JPRD_K) :: MAXVAL_

  !> @brief average value of the values in the message
  REAL(KIND=JPRD_K) :: AVGVAL_

  !> @brief coefficient returnd from FITSPECTRUM to pack spherical harmonics
  REAL(KIND=JPRD_K) :: ZP_

  !> @brief number of values in the message
  INTEGER(KIND=JPIB_K) :: NVALUES_

END TYPE


!> @brief message used to represent data from Atmosphere
TYPE, EXTENDS(OM_BASE_MSG_A) :: OM_ATM_MSG_T

  !> @brief level of the field
  INTEGER (KIND=JPIB_K) :: ILEVG_ = 0_JPIB_K

  !> @brief Grib level??? (Don't really know what is this)
  INTEGER(KIND=JPIB_K) :: NGRIBL_  = -1_JPIB_K

  !> @brief Previous post processing step in which the same field arrived
  INTEGER(KIND=JPIB_K) :: IPREVPP_ = -1_JPIB_K
END TYPE


!> @brief message used to represent data from Wave
TYPE, EXTENDS(OM_BASE_MSG_A) :: OM_WAM_MSG_T
  INTEGER (KIND=JPIB_K) :: IANGLE
  INTEGER (KIND=JPIB_K) :: IFREQ
  INTEGER (KIND=JPIB_K) :: NDATE_TIME_WINDOW_END
  INTEGER (KIND=JPIB_K) :: KCOUSTEP
  LOGICAL               :: LRSTST0
  INTEGER (KIND=JPIB_K) :: ITABLE
  INTEGER (KIND=JPIB_K) :: IPARAM
  INTEGER (KIND=JPIB_K) :: KLEV
  INTEGER (KIND=JPIB_K) :: IFCST
  INTEGER (KIND=JPIB_K) :: NSTEP
  CHARACTER(LEN=2)      :: MARSTYPE
  CHARACTER(LEN=14)     :: CDATE  !> @brief true if there are missing values in the message
END TYPE

! White list of the public symbols (datatypes)
PUBLIC :: OM_ATM_MSG_T
PUBLIC :: OM_WAM_MSG_T


! White list of the public symbols (procedures)
PUBLIC :: MSG_CREATE_NAME
PUBLIC :: MSG_WOPEN
PUBLIC :: MSG_ROPEN
PUBLIC :: MSG_CLOSE
PUBLIC :: MSG_PRINT_ATM
PUBLIC :: MSG_PRINT_WAM
PUBLIC :: MSG_WRITE_ATM
PUBLIC :: MSG_WRITE_WAM
PUBLIC :: MSG_READ_ATM
PUBLIC :: MSG_READ_WAM

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MSG_CREATE_NAME'
PP_THREAD_SAFE FUNCTION MSG_CREATE_NAME( DIRECTORY, MSG_ID, PROC_ID, MSGFNAME, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: DIRECTORY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: MSG_ID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PROC_ID
  CHARACTER(LEN=*),     INTENT(OUT)   :: MSGFNAME
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: MSGEXIST
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: M
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_NAME_TOO_SHORT=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_FILE_NAME=2_JPIB_K

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
  N = LEN(MSGFNAME)
  M = LEN_TRIM(DIRECTORY) + 26
  PP_DEBUG_CRITICAL_COND_THROW( M.GT.N, ERRFLAG_FILE_NAME_TOO_SHORT)

  ! Create the message name
  MSGFNAME = REPEAT(' ',N)
  WRITE(MSGFNAME,'(A,A,I8.8,A,I8.8,A)', IOSTAT=STAT) TRIM(ADJUSTL(DIRECTORY)), '/msg_', MSG_ID, '_', PROC_ID, '.bin'
  PP_DEBUG_CRITICAL_COND_THROW(STAT.NE.0, ERRFLAG_UNABLE_TO_CREATE_FILE_NAME)

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
    CASE (ERRFLAG_FILE_NAME_TOO_SHORT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Values file name variable too short' )
    CASE (ERRFLAG_UNABLE_TO_CREATE_FILE_NAME)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the msg file name' )
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

END FUNCTION  MSG_CREATE_NAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MSG_WOPEN'
PP_THREAD_SAFE FUNCTION MSG_WOPEN( MSGFNAME, MSGUNIT, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: MSGFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: MSGUNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: MSGEXIST
  INTEGER(KIND=JPIB_K) :: STAT

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MSG_FILE_ALREADY_EXISTS = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN_MSG_FILE = 2_JPIB_K

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

  ! Check if the file is opened
  INQUIRE( FILE=TRIM(MSGFNAME), EXIST=MSGEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( MSGEXIST, ERRFLAG_MSG_FILE_ALREADY_EXISTS )

  ! Open the TOC file
  OPEN( NEWUNIT=MSGUNIT, FILE=TRIM(MSGFNAME), STATUS='REPLACE', &
&       ACCESS='STREAM', ACTION='WRITE', FORM='UNFORMATTED', IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_OPEN_MSG_FILE )

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
    CASE (ERRFLAG_MSG_FILE_ALREADY_EXISTS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Val file already exists' )
    CASE (ERRFLAG_UNABLE_TO_OPEN_MSG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open msg file' )
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

END FUNCTION MSG_WOPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MSG_ROPEN'
PP_THREAD_SAFE FUNCTION MSG_ROPEN( MSGFNAME, MSGUNIT, BIG_ENDIAN_READ, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)  :: MSGFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: MSGUNIT
  LOGICAL,              INTENT(IN)  :: BIG_ENDIAN_READ
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: MSGEXIST
  INTEGER(KIND=JPIB_K) :: STAT

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FIND_MSG_FILE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN_MSG_FILE = 2_JPIB_K

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

  ! Check if the file is opened
  INQUIRE( FILE=TRIM(MSGFNAME), EXIST=MSGEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.MSGEXIST, ERRFLAG_UNABLE_TO_FIND_MSG_FILE)

  ! Open the TOC file
  IF ( BIG_ENDIAN_READ ) THEN
    OPEN( NEWUNIT=MSGUNIT, FILE=TRIM(MSGFNAME), STATUS='OLD', ACCESS='STREAM', &
&         ACTION='READ', FORM='UNFORMATTED', CONVERT='BIG_ENDIAN', IOSTAT=STAT )
  ELSE
    OPEN( NEWUNIT=MSGUNIT, FILE=TRIM(MSGFNAME), STATUS='OLD', ACCESS='STREAM', &
&   ACTION='READ', FORM='UNFORMATTED', IOSTAT=STAT )
  ENDIF
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_OPEN_MSG_FILE)

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
    CASE (ERRFLAG_UNABLE_TO_FIND_MSG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find msg file: '//TRIM(MSGFNAME) )
    CASE (ERRFLAG_UNABLE_TO_OPEN_MSG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open msg file: '//TRIM(MSGFNAME) )
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

END FUNCTION MSG_ROPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MSG_CLOSE'
PP_THREAD_SAFE FUNCTION  MSG_CLOSE( MSGUNIT, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: MSGUNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: MSGOPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNIT_NOT_CONNECTED_TO_MSG_FILE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CLOSING_MSG_FILE = 2_JPIB_K

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

  ! Check if the file is opened
  INQUIRE( UNIT=MSGUNIT, OPENED=MSGOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.MSGOPENED, ERRFLAG_UNIT_NOT_CONNECTED_TO_MSG_FILE)

  ! Open the TOC file
  CLOSE( UNIT=MSGUNIT, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ERROR_CLOSING_MSG_FILE)

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
    CASE (ERRFLAG_UNIT_NOT_CONNECTED_TO_MSG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unit not connected to a msg file' )
    CASE (ERRFLAG_ERROR_CLOSING_MSG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error closing msg file' )
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

END FUNCTION MSG_CLOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MSG_READ_ATM'
PP_THREAD_SAFE FUNCTION MSG_READ_ATM( MSGUNIT, MSG, ADDR, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,   ONLY: ATM_MSG_E
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: MSGUNIT
  TYPE(OM_ATM_MSG_T),   INTENT(OUT) :: MSG
  INTEGER(KIND=INT64),  INTENT(IN)  :: ADDR
  LOGICAL, OPTIONAL,    INTENT(IN)  :: VERBOSE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: MSGOPENED
  LOGICAL :: LOC_VERBOSE
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=INT64)  :: MSG_TYPE

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNIT_NOT_CONNECTED_TO_MSG_FILE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_MSG_TYPE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_MSG_TYPE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_ATMMSG = 4_JPIB_K

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

  ! Initialisation
  IF ( PRESENT(VERBOSE) ) THEN
    LOC_VERBOSE = VERBOSE
  ELSE
    LOC_VERBOSE = .FALSE.
  ENDIF

  ! Check that the file is opened
  INQUIRE( UNIT=MSGUNIT, OPENED=MSGOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.MSGOPENED, 1)

  ! Read the message type
  READ(MSGUNIT,POS=ADDR,IOSTAT=STAT ) MSG_TYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, 2 )
  PP_DEBUG_DEVELOP_COND_THROW( MSG_TYPE.NE.ATM_MSG_E, 3 )

  ! Allocate the proper message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_ATMMSG) READ_ATM(  MSG, MSGUNIT, LOC_VERBOSE, HOOKS )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNIT_NOT_CONNECTED_TO_MSG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Read unit not connected to a file' )
    CASE (ERRFLAG_UNABLE_TO_READ_MSG_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read message type from file' )
    CASE (ERRFLAG_INVALID_MSG_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid message type' )
    CASE (ERRFLAG_UNABLE_TO_READ_ATMMSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read atm message' )
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

END FUNCTION MSG_READ_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MSG_WRITE_ATM'
PP_THREAD_SAFE FUNCTION MSG_WRITE_ATM( UNIT, MSG, ADDR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,   ONLY: ATM_MSG_E
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(OM_ATM_MSG_T),   INTENT(IN)    :: MSG
  INTEGER(KIND=INT64),  INTENT(OUT)   :: ADDR
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_INQUIRE_POSITION = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_MSG_TYPE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_ATM = 3_JPIB_K

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

  ! Flush pending writes
  FLUSH(UNIT)

  ! Record the last position written
  INQUIRE( UNIT, POS=ADDR, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_INQUIRE_POSITION)

  WRITE(UNIT, IOSTAT=STAT) INT( ATM_MSG_E, INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_MSG_TYPE)
  FLUSH(UNIT)

  ! Write atmosphere
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_ATM) WRITE_ATM( MSG, UNIT, HOOKS )
  FLUSH(UNIT)

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_INQUIRE_POSITION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to inquire position' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_MSG_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write message type' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_ATM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write atm message' )
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

END FUNCTION MSG_WRITE_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MSG_PRINT_ATM'
PP_THREAD_SAFE FUNCTION MSG_PRINT_ATM( MSG, UNIT, HOOKS ) RESULT(RET)

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
  TYPE(OM_ATM_MSG_T),   INTENT(IN)    :: MSG
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_ATM = 1_JPIB_K

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

  ! Write atm
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_ATM) PRINT_ATM( MSG, UNIT, HOOKS )
  FLUSH(UNIT)

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_PRINT_ATM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to print atmosphere' )
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

END FUNCTION MSG_PRINT_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MSG_READ_WAM'
PP_THREAD_SAFE FUNCTION MSG_READ_WAM( MSGUNIT, MSG, ADDR, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,   ONLY: WAM_MSG_E
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: MSGUNIT
  TYPE(OM_WAM_MSG_T),   INTENT(OUT)   :: MSG
  INTEGER(KIND=INT64),  INTENT(IN)    :: ADDR
  LOGICAL, OPTIONAL,    INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: MSGOPENED
  LOGICAL :: LOC_VERBOSE
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=INT64)  :: MSG_TYPE

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNIT_NOT_CONNECTED_TO_MSG_FILE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_MSG_TYPE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_MSG_TYPE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_WAM_MSG = 4_JPIB_K

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

  ! Initialisation
  IF ( PRESENT(VERBOSE) ) THEN
    LOC_VERBOSE = VERBOSE
  ELSE
    LOC_VERBOSE = .FALSE.
  ENDIF

  ! Check that the file is opened
  INQUIRE( UNIT=MSGUNIT, OPENED=MSGOPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.MSGOPENED, ERRFLAG_UNIT_NOT_CONNECTED_TO_MSG_FILE)

  ! Read the message type
  READ(MSGUNIT,POS=ADDR,IOSTAT=STAT ) MSG_TYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_MSG_TYPE )
  PP_DEBUG_DEVELOP_COND_THROW( MSG_TYPE.NE.WAM_MSG_E, ERRFLAG_INVALID_MSG_TYPE )

  ! Allocate the proper message
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_WAM_MSG) READ_WAM(  MSG, MSGUNIT, LOC_VERBOSE, HOOKS )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

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
    CHARACTER(LEN=16) :: TMP

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNIT_NOT_CONNECTED_TO_MSG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Read unit not connected to a file' )
    CASE (ERRFLAG_UNABLE_TO_READ_MSG_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read message type from file' )
    CASE (ERRFLAG_INVALID_MSG_TYPE)
      WRITE(TMP,'(I10)',IOSTAT=STAT) MSG_TYPE
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid message type: '//TRIM(ADJUSTL(TMP)) )
    CASE (ERRFLAG_UNABLE_TO_READ_WAM_MSG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read wam message' )
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

END FUNCTION MSG_READ_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MSG_WRITE_WAM'
PP_THREAD_SAFE FUNCTION  MSG_WRITE_WAM( UNIT, MSG, ADDR, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRM_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: ENUMERATORS_MOD,   ONLY: WAM_MSG_E
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(OM_WAM_MSG_T),   INTENT(IN)    :: MSG
  INTEGER(KIND=INT64),  INTENT(OUT)   :: ADDR
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNIT_NOT_CONNECTED_TO_MSG_FILE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_MSG_TYPE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_WAM = 3_JPIB_K

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

  FLUSH(UNIT)

  ! Get the position inthe file
  INQUIRE( UNIT, POS=ADDR, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNIT_NOT_CONNECTED_TO_MSG_FILE)

  ! Write the kind of message
  WRITE(UNIT, IOSTAT=STAT) INT( WAM_MSG_E, INT64)
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_MSG_TYPE)
  FLUSH(UNIT)

  ! Write wam
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_WAM) WRITE_WAM( MSG, UNIT, HOOKS )
  FLUSH(UNIT)

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNIT_NOT_CONNECTED_TO_MSG_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unit not connected to msg file' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_MSG_TYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write message type' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write wam message' )
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

END FUNCTION MSG_WRITE_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MSG_PRINT_WAM'
PP_THREAD_SAFE FUNCTION MSG_PRINT_WAM( MSG, UNIT, HOOKS ) RESULT(RET)

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
  TYPE(OM_WAM_MSG_T),   INTENT(IN)    :: MSG
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_WAM = 1_JPIB_K

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

  ! Print wam
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_WAM) PRINT_WAM( MSG, UNIT, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_PRINT_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to print wam message' )
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

END FUNCTION MSG_PRINT_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WRITE_ATM'
PP_THREAD_SAFE FUNCTION WRITE_ATM( DATA, UNIT, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

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
  TYPE(OM_ATM_MSG_T),   INTENT(IN)    :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE = 1_JPIB_K

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

  ! Initialisation
  STAT = -99

  ! Common data
  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IUID_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%PARAM_ID_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ISTEP_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IPREF_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IREPRES_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NVALUES_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NUNDF_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%XUNDF_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%MINVAL_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%MAXVAL_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%AVGVAL_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%ZP_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  ! Data specific for atmosphere
  WRITE(UNIT,IOSTAT=STAT) INT( DATA%ILEVG_, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%NGRIBL_, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%IPREVPP_, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write' )
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

END FUNCTION WRITE_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_ATM'
PP_THREAD_SAFE FUNCTION READ_ATM(DATA, UNIT, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

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
  TYPE(OM_ATM_MSG_T),   INTENT(INOUT) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  LOGICAL,              INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=INT64) :: ITMP
  REAL(KIND=REAL64)   :: ZTMP
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ = 1_JPIB_K

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

  ! Read base data
  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%IUID_ = INT( ITMP, KIND( DATA%IUID_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%IUID_................ :: ', DATA%IUID_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%PARAM_ID_ = INT( ITMP, KIND( DATA%PARAM_ID_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%PARAM_ID_............ :: ', DATA%PARAM_ID_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%ISTEP_ = INT( ITMP, KIND( DATA%ISTEP_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%ISTEP_............... :: ', DATA%ISTEP_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%IPREF_ = INT( ITMP, KIND( DATA%IPREF_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%IPREF_............... :: ', DATA%IPREF_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%IREPRES_ = INT( ITMP, KIND( DATA%IREPRES_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%IREPRES_............. :: ', DATA%IREPRES_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%NVALUES_ = INT( ITMP, KIND( DATA%NVALUES_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%NVALUES_............. :: ', DATA%NVALUES_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%NUNDF_ = INT( ITMP, KIND( DATA%NUNDF_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%NUNDF_............... :: ', DATA%NUNDF_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%XUNDF_ = REAL( ZTMP, KIND( DATA%XUNDF_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%XUNDF_............... :: ', DATA%XUNDF_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%MINVAL_ = REAL( ZTMP, KIND( DATA%MINVAL_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%MINVAL_.............. :: ', DATA%MINVAL_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%MAXVAL_ = REAL( ZTMP, KIND( DATA%MAXVAL_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%MAXVAL_.............. :: ', DATA%MAXVAL_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%AVGVAL_ = REAL( ZTMP, KIND( DATA%AVGVAL_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%AVGVAL_.............. :: ', DATA%AVGVAL_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%ZP_ = REAL( ZTMP, KIND( DATA%ZP_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%ZP_.................. :: ', DATA%ZP_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%ILEVG_ = INT( ITMP, KIND(DATA%ILEVG_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_ATM%ILEVG_................ :: ', DATA%ILEVG_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%NGRIBL_ = INT( ITMP, KIND(DATA%NGRIBL_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_ATM%NGRIBL_............... :: ', DATA%NGRIBL_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%IPREVPP_ = INT( ITMP, KIND(DATA%IPREVPP_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_ATM%IPREVPP_.............. :: ', DATA%IPREVPP_
  ENDIF

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
    CASE (ERRFLAG_UNABLE_TO_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read atmoshpere message' )
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

END FUNCTION READ_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_ATM'
PP_THREAD_SAFE FUNCTION PRINT_ATM( DATA, UNIT, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T
  USE :: ENUMERATORS_MOD,   ONLY: ILEVTYPE2CLEVTYPE
  USE :: ENUMERATORS_MOD,   ONLY: IPREFIX2ILEVTYPE
  USE :: ENUMERATORS_MOD,   ONLY: IREPRES2CREPRES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_ATM_MSG_T),   INTENT(IN)    :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: ILEVTYPE
  CHARACTER(LEN=16)    :: CLEVTYPE
  CHARACTER(LEN=16)    :: CREPRES

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IPREFIX2ILEVTYPE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ILEVTYPE2CLEVTYPE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IREPRES2CREPRES = 4_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Conversions
  PP_TRYCALL(ERRFLAG_IPREFIX2ILEVTYPE)  IPREFIX2ILEVTYPE( DATA%IPREF_, DATA%PARAM_ID_, DATA%ILEVG_, DATA%IREPRES_, ILEVTYPE, HOOKS )
  PP_TRYCALL(ERRFLAG_ILEVTYPE2CLEVTYPE) ILEVTYPE2CLEVTYPE( ILEVTYPE, CLEVTYPE, HOOKS )
  PP_TRYCALL(ERRFLAG_IREPRES2CREPRES)   IREPRES2CREPRES( DATA%IREPRES_, CREPRES, HOOKS )

  ! Print
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%IUID_................ :: ', DATA%IUID_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%PARAM_ID_............ :: ', DATA%PARAM_ID_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%ISTEP_............... :: ', DATA%ISTEP_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%IPREF_............... :: ', DATA%IPREF_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%ILEVTYPE_............ :: ', ILEVTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%CLEVTYPE_............ :: ', TRIM(ADJUSTL(CLEVTYPE))
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%IREPRES_............. :: ', DATA%IREPRES_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%CREPRES_............. :: ', TRIM(ADJUSTL(CREPRES))
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%NVALUES_............. :: ', DATA%NVALUES_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%NUNDF_............... :: ', DATA%NUNDF_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%XUNDF_............... :: ', DATA%XUNDF_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%MINVAL_.............. :: ', DATA%MINVAL_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%MAXVAL_.............. :: ', DATA%MAXVAL_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%AVGVAL_.............. :: ', DATA%AVGVAL_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%ZPL_................. :: ', DATA%ZP_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_ATM%ILEVG_................ :: ', DATA%ILEVG_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_ATM%NGRIBL_............... :: ', DATA%NGRIBL_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_ATM%IPREVPP_.............. :: ', DATA%IPREVPP_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT )

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
    CASE (ERRFLAG_UNABLE_TO_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to print sim_par' )
    CASE (ERRFLAG_IPREFIX2ILEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert iprefix to ilevtype' )
    CASE (ERRFLAG_ILEVTYPE2CLEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert ilevtype to clevtype' )
    CASE (ERRFLAG_IREPRES2CREPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert irepres to crepres' )
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

END FUNCTION PRINT_ATM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WRITE_WAM'
PP_THREAD_SAFE FUNCTION WRITE_WAM( DATA, UNIT, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64

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
  TYPE(OM_WAM_MSG_T),   INTENT(IN)    :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_WAM = 1_JPIB_K

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

  ! Initialisation
  STAT = -99

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IUID_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%PARAM_ID_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ISTEP_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IPREF_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IREPRES_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NVALUES_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NUNDF_ , INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%XUNDF_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%MINVAL_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%MAXVAL_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%AVGVAL_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%ZP_ , REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%IANGLE, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%IFREQ, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%NDATE_TIME_WINDOW_END, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%KCOUSTEP, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%ITABLE, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%IPARAM, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%KLEV, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%IFCST, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT,IOSTAT=STAT) INT( DATA%NSTEP, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT,IOSTAT=STAT) DATA%LRSTST0
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT,IOSTAT=STAT) DATA%MARSTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

  WRITE(UNIT,IOSTAT=STAT) DATA%CDATE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE_WAM )

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

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_WRITE_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write: \"DATA%IUID_\"' )
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

END FUNCTION WRITE_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_WAM'
PP_THREAD_SAFE FUNCTION READ_WAM(DATA, UNIT, VERBOSE, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

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
  TYPE(OM_WAM_MSG_T),   INTENT(INOUT) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  LOGICAL,              INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=INT64)  :: ITMP
  REAL(KIND=REAL64)  :: ZTMP

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_WAM = 1_JPIB_K

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

  ! Initialisation
  STAT = -99

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%IUID_ = INT( ITMP, KIND( DATA%IUID_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%IUID_................ :: ', DATA%IUID_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%PARAM_ID_ = INT( ITMP, KIND( DATA%PARAM_ID_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%PARAM_ID_............ :: ', DATA%PARAM_ID_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%ISTEP_ = INT( ITMP, KIND( DATA%ISTEP_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%ISTEP_............... :: ', DATA%ISTEP_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%IPREF_ = INT( ITMP, KIND( DATA%IPREF_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%IPREF_............... :: ', DATA%IPREF_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%IREPRES_ = INT( ITMP, KIND( DATA%IREPRES_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%IREPRES_............. :: ', DATA%IREPRES_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%NVALUES_ = INT( ITMP, KIND( DATA%NVALUES_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%NVALUES_............. :: ', DATA%NVALUES_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%NUNDF_ = INT( ITMP, KIND( DATA%NUNDF_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%NUNDF_............... :: ', DATA%NUNDF_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%XUNDF_ = REAL( ZTMP, KIND( DATA%XUNDF_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%XUNDF_............... :: ', DATA%XUNDF_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%MINVAL_ = REAL( ZTMP, KIND( DATA%MINVAL_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%MINVAL_.............. :: ', DATA%MINVAL_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%MAXVAL_ = REAL( ZTMP, KIND( DATA%MAXVAL_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%MAXVAL_.............. :: ', DATA%MAXVAL_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%AVGVAL_ = REAL( ZTMP, KIND( DATA%AVGVAL_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%AVGVAL_.............. :: ', DATA%AVGVAL_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ZTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%ZP_ = REAL( ZTMP, KIND( DATA%ZP_) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_BASE%ZP_.................. :: ', DATA%ZP_
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%IANGLE = INT( ITMP, KIND(DATA%IANGLE) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%IANGLE................ :: ', DATA%IANGLE
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%IFREQ = INT( ITMP, KIND(DATA%IFREQ) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%IFREQ................. :: ', DATA%IFREQ
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%NDATE_TIME_WINDOW_END = INT( ITMP, KIND(DATA%NDATE_TIME_WINDOW_END) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%NDATE_TIME_WINDOW_END. :: ', DATA%NDATE_TIME_WINDOW_END
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%KCOUSTEP = INT( ITMP, KIND(DATA%KCOUSTEP) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%KCOUSTEP.............. :: ', DATA%KCOUSTEP
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%ITABLE = INT( ITMP, KIND(DATA%ITABLE) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%ITABLE................ :: ', DATA%ITABLE
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%IPARAM = INT( ITMP, KIND(DATA%IPARAM) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%IPARAM................ :: ', DATA%IPARAM
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%KLEV = INT( ITMP, KIND(DATA%KLEV) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%KLEV.................. :: ', DATA%KLEV
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%IFCST = INT( ITMP, KIND(DATA%IFCST) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%IFCST................. :: ', DATA%IFCST
  ENDIF

  READ(UNIT,IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  DATA%NSTEP = INT( ITMP, KIND(DATA%NSTEP) )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%NSTEP................. :: ', DATA%NSTEP
  ENDIF

  READ(UNIT,IOSTAT=STAT) DATA%LRSTST0
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%LRSTST0............... :: ', DATA%LRSTST0
  ENDIF

  READ(UNIT,IOSTAT=STAT) DATA%MARSTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%MARSTYPE.............. :: ', DATA%MARSTYPE
  ENDIF

  READ(UNIT,IOSTAT=STAT) DATA%CDATE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ_WAM )
  IF ( VERBOSE ) THEN
    WRITE(ERROR_UNIT,*) ' + MSG_WAM%CDATE................. :: ', DATA%CDATE
  ENDIF

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


    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_READ_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read' )
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

END FUNCTION READ_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_WAM'
PP_THREAD_SAFE FUNCTION PRINT_WAM( DATA, UNIT, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD,         ONLY: HOOKS_T

  USE :: ENUMERATORS_MOD, ONLY: IPREFIX2ILEVTYPE
  USE :: ENUMERATORS_MOD, ONLY: ILEVTYPE2CLEVTYPE
  USE :: ENUMERATORS_MOD, ONLY: IREPRES2CREPRES

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(OM_WAM_MSG_T),   INTENT(IN)    :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: ILEVTYPE
  CHARACTER(LEN=16)    :: CLEVTYPE
  CHARACTER(LEN=16)    :: CREPRES

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_WAM = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IPREFIX_TO_ILEVTYPE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ILEVTYPE2CLEVTYPE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IREPRES2CREPRES = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_IPREFIX2ILEVTYPE = 5_JPIB_K

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

  ! Conversions
  PP_TRYCALL(ERRFLAG_IPREFIX2ILEVTYPE)  IPREFIX2ILEVTYPE( DATA%IPREF_, DATA%PARAM_ID_, DATA%KLEV, DATA%IREPRES_, ILEVTYPE, HOOKS )
  PP_TRYCALL(ERRFLAG_ILEVTYPE2CLEVTYPE) ILEVTYPE2CLEVTYPE( ILEVTYPE, CLEVTYPE, HOOKS )
  PP_TRYCALL(ERRFLAG_IREPRES2CREPRES)   IREPRES2CREPRES( DATA%IREPRES_, CREPRES, HOOKS )


  ! Print
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%IUID_................ :: ', DATA%IUID_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%PARAM_ID_............ :: ', DATA%PARAM_ID_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%ISTEP_............... :: ', DATA%ISTEP_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%IPREF_............... :: ', DATA%IPREF_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%ILEVTYPE_............ :: ', ILEVTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%CLEVTYPE_............ :: ', TRIM(ADJUSTL(CLEVTYPE))
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%IREPRES_............. :: ', DATA%IREPRES_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%CREPRES_............. :: ', TRIM(ADJUSTL(CREPRES))
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%NVALUES_............. :: ', DATA%NVALUES_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%NUNDF_............... :: ', DATA%NUNDF_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%XUNDF_............... :: ', DATA%XUNDF_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%MINVAL_.............. :: ', DATA%MINVAL_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%MAXVAL_.............. :: ', DATA%MAXVAL_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%AVGVAL_.............. :: ', DATA%AVGVAL_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_BASE%ZPL_................. :: ', DATA%ZP_
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_WAM%IANGLE................ :: ', DATA%IANGLE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_WAM%IFREQ................. :: ', DATA%IFREQ
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_WAM%NDATE_TIME_WINDOW_END. :: ', DATA%NDATE_TIME_WINDOW_END
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_WAM%KCOUSTEP.............. :: ', DATA%KCOUSTEP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_WAM%ITABLE................ :: ', DATA%ITABLE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_WAM%IPARAM................ :: ', DATA%IPARAM
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_WAM%KLEV.................. :: ', DATA%KLEV
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_WAM%IFCST................. :: ', DATA%IFCST
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_WAM%NSTEP................. :: ', DATA%NSTEP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_WAM%LRSTST0............... :: ', DATA%LRSTST0
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_WAM%MARSTYPE.............. :: ', DATA%MARSTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )
  WRITE(UNIT,*,IOSTAT=STAT) ' + MSG_WAM%CDATE................. :: ', DATA%CDATE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_PRINT_WAM )

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
    CASE (ERRFLAG_UNABLE_TO_PRINT_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to print wam message' )
    CASE (ERRFLAG_IPREFIX_TO_ILEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert iprefix to ilevtype' )
    CASE (ERRFLAG_ILEVTYPE2CLEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert ilevtype to clevtype' )
    CASE (ERRFLAG_IREPRES2CREPRES)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert irepres to crepres' )
    CASE (ERRFLAG_IPREFIX2ILEVTYPE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to convert iprefix to ilevtype' )
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

END FUNCTION PRINT_WAM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE IFS_MSG_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME