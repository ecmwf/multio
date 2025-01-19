! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

#define PP_FILE_NAME 'test_mars001_prog.F90'
#define PP_SECTION_TYPE 'PROGRAM'
#define PP_SECTION_NAME 'TEST_MARS001_PROG'
#define PP_PROCEDURE_TYPE 'PROGRAM'
#define PP_PROCEDURE_NAME 'MAIN'
PROGRAM TEST_MARS001_PROG

  !> Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: HOOKS_MOD, ONLY: HOOKS_T
  USE :: FORTRAN_MESSAGE_MOD, ONLY: FORTRAN_MESSAGE_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  !> Local variables
  TYPE(FORTRAN_MESSAGE_T) :: MARS
  INTEGER(KIND=JPIB_K)    :: PARAM
  INTEGER(KIND=JPIB_K)    :: LEVTYPE
  INTEGER(KIND=JPIB_K)    :: LEVEL
  INTEGER(KIND=JPIB_K)    :: CLASS
  INTEGER(KIND=JPIB_K)    :: STREAM
  INTEGER(KIND=JPIB_K)    :: TYPE
  INTEGER(KIND=JPIB_K)    :: DATE
  INTEGER(KIND=JPIB_K)    :: TIME
  CHARACTER(LEN=4)        :: EXPVER
  LOGICAL                 :: HAS_PARAM
  INTEGER(KIND=JPIB_K)    :: DEALLOC_STAT
  LOGICAL                 :: HAS_LEVTYPE
  CHARACTER(LEN=:), ALLOCATABLE :: JSON
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG
  TYPE(HOOKS_T)           :: HOOKS

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_INIT=1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_SET_KEYWORD=2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_HAS_KEYWORD=3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_GET_KEYWORD=4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_FREE=5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_TO_JSON=6_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE=7_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_MARS_PRINT=8_JPIB_K

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for logging purposes
  PP_LOG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  !> Set the unit and offset
  CALL HOOKS%DEBUG_HOOK_%INIT( )

  !> Initialize the test encoder object
  PP_TRYCALL(ERRFLAG_MARS_INIT) MARS%INIT( HOOKS )
  WRITE(*,*) 'MARS initialized'

  !> Try to set some mars keywords
  PARAM = 123456
  LEVTYPE = 2
  LEVEL= 0
  CLASS = 2
  TYPE = 3
  STREAM = 4
  EXPVER='TEST'
  DATE=20210101
  TIME=120000

  PP_TRYCALL(ERRFLAG_MARS_SET_KEYWORD) MARS%SET( 'param',    PARAM,   HOOKS )
  PP_TRYCALL(ERRFLAG_MARS_SET_KEYWORD) MARS%SET( 'levtype',  LEVTYPE, HOOKS )
  PP_TRYCALL(ERRFLAG_MARS_SET_KEYWORD) MARS%SET( 'levelist', LEVEL,   HOOKS )
  PP_TRYCALL(ERRFLAG_MARS_SET_KEYWORD) MARS%SET( 'stream',   STREAM,  HOOKS )
  PP_TRYCALL(ERRFLAG_MARS_SET_KEYWORD) MARS%SET( 'class',    CLASS,   HOOKS )
  PP_TRYCALL(ERRFLAG_MARS_SET_KEYWORD) MARS%SET( 'type',     TYPE,    HOOKS )
  PP_TRYCALL(ERRFLAG_MARS_SET_KEYWORD) MARS%SET( 'expver',   EXPVER,  HOOKS )
  PP_TRYCALL(ERRFLAG_MARS_SET_KEYWORD) MARS%SET( 'date',     DATE,    HOOKS )
  PP_TRYCALL(ERRFLAG_MARS_SET_KEYWORD) MARS%SET( 'time',     TIME,    HOOKS )

  PP_TRYCALL(ERRFLAG_MARS_HAS_KEYWORD) MARS%HAS( 'param', HAS_PARAM, HOOKS )
  WRITE(*,*) 'MARS has param: ', HAS_PARAM

  PARAM = 0
  PP_TRYCALL(ERRFLAG_MARS_GET_KEYWORD) MARS%GET( 'param', PARAM, HOOKS )
  WRITE(*,*) 'MARS get param: ', PARAM


  PP_TRYCALL(ERRFLAG_MARS_PRINT) MARS%PRINT( 6_JPIB_K, HOOKS )
  PP_TRYCALL(ERRFLAG_MARS_TO_JSON) MARS%TO_JSON( JSON, HOOKS )
  IF ( ALLOCATED(JSON) ) THEN
    WRITE(*,'(A,A)') 'MARS to JSON: ', JSON
    DEALLOCATE( JSON, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  END IF

  !> Initialize the test encoder object
  PP_TRYCALL(ERRFLAG_MARS_FREE) MARS%FREE( HOOKS )
  WRITE(*,*) 'MARS finalized'

  !> Be sure we don't have any memory leaks
  CALL HOOKS%DEBUG_HOOK_%FREE( )

  !> Exit point (on success)
  STOP 0

! Error handler
PP_ERROR_HANDLER

#if defined( PP_DEBUG_ENABLE_ERROR_HANDLING )
!$omp critical(ERROR_HANDLER)

  BLOCK

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_MARS_INIT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error initializing the test encoder object' )
    CASE (ERRFLAG_MARS_FREE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error freeing the test encoder object' )
    CASE (ERRFLAG_MARS_SET_KEYWORD)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error setting the test encoder object keywords' )
    CASE (ERRFLAG_MARS_HAS_KEYWORD)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error checking if the test encoder object has a keyword' )
    CASE (ERRFLAG_MARS_GET_KEYWORD)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error getting the test encoder object keywords' )
    CASE (ERRFLAG_MARS_TO_JSON)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error converting the test encoder object to JSON' )
    CASE (ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate memory' )
      IF (ALLOCATED(ERRMSG)) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: ' // TRIM(ERRMSG) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE (ERRFLAG_MARS_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error printing the test encoder object' )
    CASE DEFAULT
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unknown error' )
    END SELECT

    ! Print the error stack
    CALL HOOKS%DEBUG_HOOK_%PRINT_ERROR_STACK( 6_JPIB_K )

    ! Free the error stack
    CALL HOOKS%DEBUG_HOOK_%FREE( )

    ! Write the error message and stop the program
    PP_DEBUG_ABORT

  END BLOCK

!$omp end critical(ERROR_HANDLER)
#endif

  ! Exit point (on error)
  STOP 1

END PROGRAM TEST_MARS001_PROG
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME