! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'om_profile_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'OM_PROFILE_MOD'
MODULE OM_PROFILE_MOD

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

IMPLICIT NONE

TYPE :: PROFILE_T
  INTEGER(KIND=JPIB_K) :: PROFUNIT_
  INTEGER(KIND=JPIB_K) :: SIMULATION_TIC_
  INTEGER(KIND=JPIB_K) :: FLUSH_TIC_
  INTEGER(KIND=JPIB_K) :: MESSAGE_TIC_
END TYPE

PUBLIC :: PROFILE_T
PUBLIC :: PROFILE_START_SIMULATION
PUBLIC :: PROFILE_MESSAGE
PUBLIC :: PROFILE_FLUSH
PUBLIC :: PROFILE_FLUSH_AND_RESTART
PUBLIC :: PROFILE_FLUSH_LAST_STEP
PUBLIC :: PROFILE_END_SIMULATION


CONTAINS


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PROFILE_START_SIMULATION'
SUBROUTINE PROFILE_START_SIMULATION( PROFILE, DIRECTORY, PROC_ID )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_TIC
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_GETMEM

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(PROFILE_T),      INTENT(INOUT) :: PROFILE
  CHARACTER(LEN=*),     INTENT(IN)    :: DIRECTORY
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PROC_ID

  ! Local variabels
  INTEGER(KIND=JPIB_K) :: NOW
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: TMP
  INTEGER(KIND=JPIB_K) :: DUMMY

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  TMP = 0

  CALL OM_TIC( NOW )

  PROFILE%SIMULATION_TIC_ = NOW
  PROFILE%FLUSH_TIC_      = NOW
  PROFILE%MESSAGE_TIC_    = NOW

  ! Inititlise timing
  TMP(1) = 0
  TMP(2) = 0
  TMP(3) = 0

  ! Get the current memory consumption
  CALL OM_GETMEM( TMP(4), TMP(5), TMP(6) )

  ! Write profile data
  DUMMY = -99
  CALL PROFILE_WOPEN( DIRECTORY, PROC_ID, PROFILE%PROFUNIT_, NOW, TMP(4) )
  CALL PROFILE_WRITE_DATA( PROFILE%PROFUNIT_, 'START SIM.', TMP, TMP(1), DUMMY, DUMMY )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PROFILE_START_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PROFILE_MESSAGE'
SUBROUTINE PROFILE_MESSAGE( PROFILE, STEP, PARAMID, UID )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_TIC
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_TOC
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_GETMEM

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(PROFILE_T),      INTENT(INOUT) :: PROFILE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PARAMID
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UID

  ! Local variabels
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: TMP
  INTEGER(KIND=JPIB_K) :: NOW

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Time elapsed from the beginning of the simulation
  CALL OM_TOC( PROFILE%SIMULATION_TIC_, TMP(1) )

  ! Time elapsed from the last flush
  CALL OM_TOC( PROFILE%FLUSH_TIC_,      TMP(2) )

  ! Time elapsed from the last message
  CALL OM_TOC( PROFILE%MESSAGE_TIC_,    TMP(3) )

  ! Get the current memory consumption
  CALL OM_GETMEM( TMP(4), TMP(5), TMP(6) )

  ! Get the current time
  CALL OM_TIC( NOW )

  ! Update the last message time
  PROFILE%MESSAGE_TIC_ = NOW

  ! Write profile data
  CALL PROFILE_WRITE_DATA( PROFILE%PROFUNIT_, 'MESSAGE', TMP, STEP, PARAMID, UID )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PROFILE_MESSAGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PROFILE_FLUSH'
SUBROUTINE PROFILE_FLUSH( PROFILE, STEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_TIC
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_TOC
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_GETMEM

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(PROFILE_T),      INTENT(INOUT) :: PROFILE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP

  ! Local variabels
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: TMP
  INTEGER(KIND=JPIB_K) :: NOW
  INTEGER(KIND=JPIB_K) :: DUMMY

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Time elapsed from the beginning of the simulation
  CALL OM_TOC( PROFILE%SIMULATION_TIC_, TMP(1) )

  ! Time elapsed from the last flush
  CALL OM_TOC( PROFILE%FLUSH_TIC_,      TMP(2) )

  ! Time elapsed from the last message
  CALL OM_TOC( PROFILE%MESSAGE_TIC_,    TMP(3) )

  ! Get the current memory consumption
  CALL OM_GETMEM( TMP(4), TMP(5), TMP(6) )

  ! Get the current time
  CALL OM_TIC( NOW )

  ! Update the last message time
  PROFILE%MESSAGE_TIC_ = NOW
  PROFILE%FLUSH_TIC_   = NOW

  ! Write profile data
  DUMMY = -99
  CALL PROFILE_WRITE_DATA( PROFILE%PROFUNIT_, 'FLUSH', TMP, STEP, DUMMY, DUMMY )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PROFILE_FLUSH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PROFILE_FLUSH_AND_RESTART'
SUBROUTINE PROFILE_FLUSH_AND_RESTART( PROFILE, STEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_TIC
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_TOC
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_GETMEM

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(PROFILE_T),      INTENT(INOUT) :: PROFILE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP

  ! Local variabels
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: TMP
  INTEGER(KIND=JPIB_K) :: NOW
  INTEGER(KIND=JPIB_K) :: DUMMY

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Time elapsed from the beginning of the simulation
  CALL OM_TOC( PROFILE%SIMULATION_TIC_, TMP(1) )

  ! Time elapsed from the last flush
  CALL OM_TOC( PROFILE%FLUSH_TIC_,      TMP(2) )

  ! Time elapsed from the last message
  CALL OM_TOC( PROFILE%MESSAGE_TIC_,    TMP(3) )

  ! Get the current memory consumption
  CALL OM_GETMEM( TMP(4), TMP(5), TMP(6) )

  ! Get the current time
  CALL OM_TIC( NOW )

  ! Update the last message time
  PROFILE%MESSAGE_TIC_ = NOW
  PROFILE%FLUSH_TIC_   = NOW

  ! Write profile data
  DUMMY = -99
  CALL PROFILE_WRITE_DATA( PROFILE%PROFUNIT_, 'FLUSH/RESTART', TMP, STEP, DUMMY, DUMMY )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PROFILE_FLUSH_AND_RESTART
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PROFILE_FLUSH_LAST_STEP'
SUBROUTINE PROFILE_FLUSH_LAST_STEP( PROFILE, STEP )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_TIC
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_TOC
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_GETMEM

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(PROFILE_T),      INTENT(INOUT) :: PROFILE
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: STEP

  ! Local variabels
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: TMP
  INTEGER(KIND=JPIB_K) :: NOW
  INTEGER(KIND=JPIB_K) :: DUMMY

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Time elapsed from the beginning of the simulation
  CALL OM_TOC( PROFILE%SIMULATION_TIC_, TMP(1) )

  ! Time elapsed from the last flush
  CALL OM_TOC( PROFILE%FLUSH_TIC_,      TMP(2) )

  ! Time elapsed from the last message
  CALL OM_TOC( PROFILE%MESSAGE_TIC_,    TMP(3) )

  ! Get the current memory consumption
  CALL OM_GETMEM( TMP(4), TMP(5), TMP(6) )

  ! Get the current time
  CALL OM_TIC( NOW )

  ! Update the last message time
  PROFILE%MESSAGE_TIC_ = NOW
  PROFILE%FLUSH_TIC_   = NOW

  ! Write profile data
  DUMMY = -99
  CALL PROFILE_WRITE_DATA( PROFILE%PROFUNIT_, 'LAST STEP', TMP, STEP, DUMMY, DUMMY )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PROFILE_FLUSH_LAST_STEP
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PROFILE_END_SIMULATION'
SUBROUTINE PROFILE_END_SIMULATION( PROFILE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_TIC
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_TOC
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_GETMEM

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(PROFILE_T), INTENT(INOUT) :: PROFILE

  ! Local variabels
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: TMP
  INTEGER(KIND=JPIB_K) :: NOW
  INTEGER(KIND=JPIB_K) :: DUMMY

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Time elapsed from the beginning of the simulation
  CALL OM_TOC( PROFILE%SIMULATION_TIC_, TMP(1) )

  ! Time elapsed from the last flush
  CALL OM_TOC( PROFILE%FLUSH_TIC_,      TMP(2) )

  ! Time elapsed from the last message
  CALL OM_TOC( PROFILE%MESSAGE_TIC_,    TMP(3) )

  ! Get the current memory consumption
  CALL OM_GETMEM( TMP(4), TMP(5), TMP(6) )

  ! Write profile data
  DUMMY = -99
  CALL PROFILE_WRITE_DATA( PROFILE%PROFUNIT_, 'END OF SIM.', TMP, DUMMY, DUMMY, DUMMY )
  CALL PROFILE_CLOSE( PROFILE%PROFUNIT_ )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END SUBROUTINE PROFILE_END_SIMULATION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PROFILE_WOPEN'
SUBROUTINE PROFILE_WOPEN( DIRECTORY, PROC_ID, PROFUNIT, NOW, TOTMEM )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,          ONLY: JPIB_K
  USE :: OM_GENERAL_UTILS_MOD, ONLY: OM_GET_HOSTNAME

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)  :: DIRECTORY
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: PROC_ID
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: PROFUNIT
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: NOW
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: TOTMEM

  ! Local variables
  CHARACTER(LEN=128)   :: FILENAME
  CHARACTER(LEN=256)   :: HOSTNAME
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialise filename
  FILENAME = REPEAT(' ',128)
  HOSTNAME = REPEAT(' ',256)
  CALL OM_GET_HOSTNAME( HOSTNAME )

  ! Create the filename
  WRITE( FILENAME, '(A,A,I6.6,A)' ) TRIM(DIRECTORY), '/profile_', PROC_ID, '.dat'

  ! Open the file
  OPEN( NEWUNIT=PROFUNIT, FILE=FILENAME, STATUS='REPLACE', ACTION='WRITE', FORM='FORMATTED', IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 1 )

  ! Write the header
  WRITE(PROFUNIT,'(A,A)')  '# HOSTNAME ::', TRIM(ADJUSTL(HOSTNAME))
  WRITE(PROFUNIT,'(A,I8)') '# PROC_IDX ::', PROC_ID
  WRITE(PROFUNIT,'(A)')    '# '
  WRITE(PROFUNIT,'(A)')    '# This file has been automatically geneated by:'
  WRITE(PROFUNIT,'(A,A)')  '# -> file........... ::', PP_FILE_NAME
  WRITE(PROFUNIT,'(A,A)')  '# -> sectionType.... ::', PP_SECTION_TYPE
  WRITE(PROFUNIT,'(A,A)')  '# -> sectionName.... ::', PP_SECTION_NAME
  WRITE(PROFUNIT,'(A,A)')  '# -> procedureType.. ::', PP_PROCEDURE_TYPE
  WRITE(PROFUNIT,'(A,A)')  '# -> procedureName.. ::', PP_PROCEDURE_NAME
  WRITE(PROFUNIT,'(A,I8)') '# -> line........... ::', __LINE__
  WRITE(PROFUNIT,'(A)')    '# '
  WRITE(PROFUNIT,'(A)')    '# TAG :: Position at which the time is measured'
  WRITE(PROFUNIT,'(A)')    '# T1  :: Time from the beginning of the simulation'
  WRITE(PROFUNIT,'(A)')    '# T2  :: Time from the last flush'
  WRITE(PROFUNIT,'(A)')    '# T3  :: Time from the last message'
  WRITE(PROFUNIT,'(A)')    '# M1  :: System memory usage in Bytes'
  WRITE(PROFUNIT,'(A)')    '# M2  :: Task memory usage in Bytes'


  WRITE(PROFUNIT,'(A,I16)') '# '
  WRITE(PROFUNIT,'(A,I16)') '# Time of start of the simulation in [ns]:     ', NOW
  WRITE(PROFUNIT,'(A,I16)') '# Total memory available on the node in bytes: ', TOTMEM
  WRITE(PROFUNIT,'(A,I16)') '# '

  WRITE(PROFUNIT,'(A1,A)') '#', REPEAT('-',153)
  WRITE(PROFUNIT,'(A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1,A16,A1)') '|', &
&  '   TAG          ',  '|', &
&  '   STEP         ',  '|', &
&  '   PARAM_ID     ',  '|', &
&  '   UID          ',  '|', &
&  '   T1 [ns]      ',  '|', &
&  '   T2 [ns]      ',  '|', &
&  '   T3 [ns]      ',  '|', &
&  '   M1 [B]       ',  '|', &
&  '   M2 [B]       ',  '|'
  WRITE(PROFUNIT,'(A1,A)') '#', REPEAT('-',153)

  FLUSH(PROFUNIT)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to open profile file: '//TRIM(ADJUSTL(FILENAME)) )
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

END SUBROUTINE PROFILE_WOPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PROFILE_WRITE_DATA'
SUBROUTINE PROFILE_WRITE_DATA( PROFUNIT, PROFTAG, PROFDATA, STEP, PARAM_ID, UID )

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K),               INTENT(IN) :: PROFUNIT
  CHARACTER(LEN=*),                   INTENT(IN) :: PROFTAG
  INTEGER(KIND=JPIB_K), DIMENSION(6), INTENT(IN) :: PROFDATA
  INTEGER(KIND=JPIB_K),               INTENT(IN) :: STEP
  INTEGER(KIND=JPIB_K),               INTENT(IN) :: PARAM_ID
  INTEGER(KIND=JPIB_K),               INTENT(IN) :: UID

  ! Local variables
  LOGICAL :: EX
  LOGICAL :: OPENED

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialise filename
  INQUIRE( UNIT=PROFUNIT, OPENED=EX )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 1 )

  WRITE(PROFUNIT,'(A1,A16,A1,I16,A1,I16,A1,I16,A1,I16,A1,I16,A1,I16,A1,I16,A1,I16,A1)') '|', &
&  PROFTAG,     '|', &
&  STEP,        '|', &
&  PARAM_ID,    '|', &
&  UID,         '|', &
&  PROFDATA(1), '|', &
&  PROFDATA(2), '|', &
&  PROFDATA(3), '|', &
&  PROFDATA(5), '|', &
&  PROFDATA(6), '|'

  FLUSH(PROFUNIT)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Profile file not opened' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to close profile file' )
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

END SUBROUTINE PROFILE_WRITE_DATA
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'PROFILE_CLOSE'
SUBROUTINE PROFILE_CLOSE( PROFUNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: PROFUNIT

  ! Local variables
  LOGICAL :: EX
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialise filename
  INQUIRE( UNIT=PROFUNIT, OPENED=EX )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, 1 )

  ! Footer
  WRITE(PROFUNIT,'(A1,A)') '#', REPEAT('-',153)

  ! Open the file
  CLOSE( UNIT=PROFUNIT, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 1 )


  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN


! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! HAndle different errors
    SELECT CASE(ERRIDX)
    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Profile file not opened' )
    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to close profile file' )
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

END SUBROUTINE PROFILE_CLOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


END MODULE OM_PROFILE_MOD