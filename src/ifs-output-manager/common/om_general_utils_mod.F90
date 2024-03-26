! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'om_general_utils_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'OM_GENERAL_UTILS_MOD'
MODULE OM_GENERAL_UTILS_MOD

IMPLICIT NONE

PRIVATE

! Whitelist of public symbols
PUBLIC :: OM_ENVVAR_IS_DEFINED
PUBLIC :: OM_READ_ENVVAR
PUBLIC :: LOG_CURR_TIME
PUBLIC :: LOG_VERSION
PUBLIC :: LOG_SYSINFO
PUBLIC :: LOG_MEMORY
PUBLIC :: OM_MKDIR
PUBLIC :: OM_CHDIR
PUBLIC :: OM_TOLOWER
PUBLIC :: OM_TOUPPER
PUBLIC :: OM_GET_HOSTNAME
PUBLIC :: OM_GETPID
PUBLIC :: OM_GETMEM
PUBLIC :: OM_TIC
PUBLIC :: OM_TOC
PUBLIC :: OM_READ_TYPE_FROM_ENV
PUBLIC :: OM_READ_YAML_FROM_ENV
PUBLIC :: OM_IS_LITTLE_ENDIAN

CONTAINS

!>
!> @brief Retrieves the output manager type from the 'OUTPUT_MANAGER_YAML' environment variable.
!>
!> This function reads the 'OUTPUT_MANAGER_YAML' environment variable to determine the configure
!> the main YAML configuration file. If the variable is not defined, the default value
!> '../output-manager-config.yaml' is assumed.
!>
!> @attention the folder is "../" because by default each instance of the output manager run in
!>            the folder calles io_serv.<procId>.d
!>
!> @param [out] OMYAML Name of the main YAML configuraiton file
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'OM_READ_YAML_FROM_ENV'
SUBROUTINE OM_READ_YAML_FROM_ENV( OMYAML )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*), INTENT(OUT) :: OMYAML

  ! Local variables
  INTEGER(KIND=JPIB_K) :: NENVLN
  INTEGER(KIND=JPIB_K) :: STAT
  LOGICAL :: EX

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialise the YAML name
  OMYAML = REPEAT(' ',LEN(OMYAML))

  ! Read Output Manager Type
  IF ( OM_ENVVAR_IS_DEFINED( 'OUTPUT_MANAGER_YAML', NDLEN=NENVLN ) ) THEN

    ! I the value is longer than 32 characters it is invalid
    PP_DEBUG_DEVELOP_COND_THROW( (NENVLN.GT.LEN(OMYAML)), 1 )

    ! Read the environment variable
    CALL OM_READ_ENVVAR( 'OUTPUT_MANAGER_YAML', OMYAML, NENVLN )

    ! Check if the file exsts
    INQUIRE( FILE=TRIM(OMYAML), EXIST=EX )
    PP_DEBUG_DEVELOP_COND_THROW( .NOT.EX, 2 )

  ELSE

    ! Default value for output manager type when environment variable
    ! is not defined.
    ! "../" Because the output manager "main" directory is: "io_serv.0000?.d"
    OMYAML = '../output-manager-config.yaml'

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'OUTPUT_MANAGER_YAML env. var. too long' )

    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'OUTPUT_MANAGER_YAML unable to find the file' )

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

END SUBROUTINE OM_READ_YAML_FROM_ENV
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


!>
!> @brief Retrieves the output manager type from the 'OUTPUT_MANAGER_TYPE' environment variable.
!>
!> This function reads the 'OUTPUT_MANAGER_TYPE' environment variable to determine the desired
!> type of output manager to be constructed. If the variable is not defined, the default value
!> 'NOOP' is assumed.
!>
!> @return The name of the output manager to be implemented.
!>
#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'OM_READ_TYPE_FROM_ENV'
SUBROUTINE OM_READ_TYPE_FROM_ENV( OMTYPE )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*), INTENT(OUT) :: OMTYPE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: NENVLN
  INTEGER(KIND=JPIB_K) :: STAT

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialise the YAML name
  OMTYPE = REPEAT(' ',LEN(OMTYPE))

  ! Read Output Manager Type
  IF ( OM_ENVVAR_IS_DEFINED( 'OUTPUT_MANAGER_TYPE', NDLEN=NENVLN ) ) THEN

    ! I the value is longer than 32 characters it is invalid
    PP_DEBUG_DEVELOP_COND_THROW( (NENVLN.GT.32), 1 )

    ! Read the environment variable
    CALL OM_READ_ENVVAR( 'OUTPUT_MANAGER_TYPE', OMTYPE, NENVLN )

    ! Convert to uppercase the environment variable
    CALL OM_TOUPPER( OMTYPE )

  ELSE

    ! Default value for output manager type when environment variable
    ! is not defined
    OMTYPE = 'NOOP'

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
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'OUTPUT_MANAGER_TYPE env. var. too long' )

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

END SUBROUTINE OM_READ_TYPE_FROM_ENV
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'OM_ENVVAR_IS_DEFINED'
FUNCTION OM_ENVVAR_IS_DEFINED( CDENVVARNAME, NDLEN ) RESULT(LDIS_DEFINED)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),               INTENT(IN)  :: CDENVVARNAME
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(OUT) :: NDLEN

  ! Function Result
  LOGICAL :: LDIS_DEFINED

  ! Local variables
  INTEGER(KIND=JPIB_K) :: NLLEN
  INTEGER(KIND=JPIM_K) :: STAT
  INTEGER(KIND=JPIM_K) :: NLLEN4 
  
  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the envisonment variable is readable
  CALL GET_ENVIRONMENT_VARIABLE( CDENVVARNAME, LENGTH=NLLEN4, STATUS=STAT )

  ! Read Output Manager Type
  IF ( STAT .EQ. 0 ) THEN

    NLLEN = NLLEN4
    LDIS_DEFINED = .TRUE.

  ELSE

    NLLEN = NLLEN4
    LDIS_DEFINED = .FALSE.

  ENDIF

  ! Optional arguments
  IF ( PRESENT(NDLEN) ) THEN
    NDLEN = NLLEN
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END FUNCTION OM_ENVVAR_IS_DEFINED
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'LOG_SYSINFO'
SUBROUTINE LOG_SYSINFO( LOGUNIT, NPROCSIO, MYPROCIO )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: LOGUNIT
  INTEGER(KIND=JPIB_K), INTENT(IN) :: NPROCSIO
  INTEGER(KIND=JPIB_K), INTENT(IN) :: MYPROCIO

  ! Local variables
  CHARACTER(LEN=1024)  :: HOSTNAME

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  CALL OM_GET_HOSTNAME( HOSTNAME )

  WRITE(LOGUNIT,'(A)') ' '
  WRITE(LOGUNIT,'(A)') ' SYSTEM/MULTIPROCESSOR INFO'
  WRITE(LOGUNIT,'(A)') ' --------------------------'
  WRITE(LOGUNIT,'(A,I8)') ' + NPROCSIO............: ', NPROCSIO
  WRITE(LOGUNIT,'(A,I8)') ' + MYPROCIO............: ', MYPROCIO
  WRITE(LOGUNIT,'(A,A)')  ' + HOSTNAME............: ', TRIM(ADJUSTL(HOSTNAME))
  WRITE(LOGUNIT,'(A,I8)') ' + PID.................: ', OM_GETPID()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE LOG_SYSINFO
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'LOG_MEMORY'
SUBROUTINE LOG_MEMORY( LOGUNIT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPRD_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: LOGUNIT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: TOT_MEM
  INTEGER(KIND=JPIB_K) :: SYS_USAGE
  INTEGER(KIND=JPIB_K) :: TASK_USAGE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  CALL OM_GETMEM( TOT_MEM, SYS_USAGE, TASK_USAGE )

  WRITE(LOGUNIT,'(A)') ' '
  WRITE(LOGUNIT,'(A)') ' SYSTEM MEMORY'
  WRITE(LOGUNIT,'(A)') ' --------------------------'
  WRITE(LOGUNIT,'(A,A)')      ' + TOTAL MEMORY............: ', TRIM(FORMAT_MEMORY(TOT_MEM))
  WRITE(LOGUNIT,'(A,A)')      ' + SYSTEM USAGE............: ', TRIM(FORMAT_MEMORY(SYS_USAGE))
  WRITE(LOGUNIT,'(A,A)')      ' + TASK USAGE..............: ', TRIM(FORMAT_MEMORY(TASK_USAGE))
  WRITE(LOGUNIT,'(A,F5.2,A)') ' + TASK/SYSTEM PERCENTAGE..: ', REAL(TASK_USAGE,JPRD_K)/REAL(SYS_USAGE,JPRD_K)*100, '%'

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE LOG_MEMORY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'LOG_VERSION'
SUBROUTINE LOG_VERSION( LOGUNIT )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: COMPILER_VERSION
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: COMPILER_OPTIONS

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD,       ONLY: JPIB_K
  USE :: VERSION_UTILS_MOD, ONLY: IFS_CYCLE
  USE :: VERSION_UTILS_MOD, ONLY: OUTPUT_MANAGER_MAJOR
  USE :: VERSION_UTILS_MOD, ONLY: OUTPUT_MANAGER_MINOR
  USE :: VERSION_UTILS_MOD, ONLY: OUTPUT_MANAGER_PATCH
  USE :: VERSION_UTILS_MOD, ONLY: OUTPUT_MANAGER_BUILD_FLAVOUR
  USE :: VERSION_UTILS_MOD, ONLY: OUTPUT_MANAGER_COMPILER
  USE :: VERSION_UTILS_MOD, ONLY: OUTPUT_MANAGER_COMPILER_ID
  USE :: VERSION_UTILS_MOD, ONLY: OUTPUT_MANAGER_COMPILER_VERSION
  USE :: VERSION_UTILS_MOD, ONLY: OUTPUT_MANAGER_COMPILER_FLAGS
  USE :: VERSION_UTILS_MOD, ONLY: OUTPUT_MANAGER_EXE_LINKER_FLAGS
  USE :: VERSION_UTILS_MOD, ONLY: OUTPUT_MANAGER_SHARED_LINKER_FLAGS
  USE :: VERSION_UTILS_MOD, ONLY: GIT_SHA
  USE :: VERSION_UTILS_MOD, ONLY: OUTPUT_MANAGER_SYSTEM_NAME
  USE :: VERSION_UTILS_MOD, ONLY: OUTPUT_MANAGER_BUILD_TYPE
  USE :: VERSION_UTILS_MOD, ONLY: OUTPUT_MANAGER_BUILD_DATE
  USE :: VERSION_UTILS_MOD, ONLY: OUTPUT_MANAGER_HOSTNAME

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: LOGUNIT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K), DIMENSION(8) :: VALUES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  WRITE(LOGUNIT,'(A)') ' '
  WRITE(LOGUNIT,'(A)') ' OUTPUT MANAGER VERSION INFO'
  WRITE(LOGUNIT,'(A)') ' ---------------------------'
  WRITE(LOGUNIT,'(A,A)') ' + IFS_CYCLE............................: ', IFS_CYCLE
  WRITE(LOGUNIT,'(A,A)') ' + OUTPUT_MANAGER_MAJOR.................: ', OUTPUT_MANAGER_MAJOR
  WRITE(LOGUNIT,'(A,A)') ' + OUTPUT_MANAGER_MINOR.................: ', OUTPUT_MANAGER_MINOR
  WRITE(LOGUNIT,'(A,A)') ' + OUTPUT_MANAGER_PATCH.................: ', OUTPUT_MANAGER_PATCH
  WRITE(LOGUNIT,'(A,A)') ' + OUTPUT_MANAGER_BUILD_FLAVOUR.........: ', OUTPUT_MANAGER_BUILD_FLAVOUR
  WRITE(LOGUNIT,'(A,A)') ' + OUTPUT_MANAGER_COMPILER..............: ', OUTPUT_MANAGER_COMPILER
  WRITE(LOGUNIT,'(A,A)') ' + OUTPUT_MANAGER_COMPILER_ID...........: ', OUTPUT_MANAGER_COMPILER_ID
  WRITE(LOGUNIT,'(A,A)') ' + OUTPUT_MANAGER_COMPILER_VERSION......: ', OUTPUT_MANAGER_COMPILER_VERSION
  WRITE(LOGUNIT,'(A,A)') ' + OUTPUT_MANAGER_COMPILER_FLAGS........: ', OUTPUT_MANAGER_COMPILER_FLAGS
  WRITE(LOGUNIT,'(A,A)') ' + OUTPUT_MANAGER_EXE_LINKER_FLAGS......: ', OUTPUT_MANAGER_EXE_LINKER_FLAGS
  WRITE(LOGUNIT,'(A,A)') ' + OUTPUT_MANAGER_SHARED_LINKER_FLAGS...: ', OUTPUT_MANAGER_SHARED_LINKER_FLAGS
  WRITE(LOGUNIT,'(A,A)') ' + GIT_SHA..............................: ', GIT_SHA
  WRITE(LOGUNIT,'(A,A)') ' + OUTPUT_MANAGER_SYSTEM_NAME...........: ', OUTPUT_MANAGER_SYSTEM_NAME
  WRITE(LOGUNIT,'(A,A)') ' + OUTPUT_MANAGER_BUILD_TYPE............: ', OUTPUT_MANAGER_BUILD_TYPE
  WRITE(LOGUNIT,'(A,A)') ' + OUTPUT_MANAGER_BUILD_DATE............: ', OUTPUT_MANAGER_BUILD_DATE
  WRITE(LOGUNIT,'(A,A)') ' + OUTPUT_MANAGER_HOSTNAME..............: ', OUTPUT_MANAGER_HOSTNAME
  WRITE(LOGUNIT,'(A,A)') ' + COMPILER_VERSION.....................: ', COMPILER_VERSION()
  WRITE(LOGUNIT,'(A,A)') ' + COMPILER_OPTIONS.....................: ', COMPILER_OPTIONS()

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE LOG_VERSION
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'LOG_CURR_TIME'
SUBROUTINE LOG_CURR_TIME( LOGUNIT, CDTXT )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: LOGUNIT
  CHARACTER(LEN=*),     INTENT(IN) :: CDTXT

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K), DIMENSION(8) :: VALUES

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Get the time
  CALL DATE_AND_TIME( VALUES=VALUES )

  ! Write the logging
  WRITE(LOGUNIT,'(A)')    ' '
  WRITE(LOGUNIT,'(A)')    ' CURRENT TIME'
  WRITE(LOGUNIT,'(A)')    ' ------------'
  WRITE(LOGUNIT,'(A,A)')  ' + ', TRIM(ADJUSTL(CDTXT))
  WRITE(LOGUNIT,'(A,I8)') ' + YEAR, INCLUDING THE CENTURY...........: ', VALUES(1)
  WRITE(LOGUNIT,'(A,I8)') ' + MONTH OF THE YEAR.....................: ', VALUES(2)
  WRITE(LOGUNIT,'(A,I8)') ' + DAY OF THE MONTH......................: ', VALUES(3)
  WRITE(LOGUNIT,'(A,I8)') ' + TIME DIFFERENCE FROM UTC IN MINUTES...: ', VALUES(4)
  WRITE(LOGUNIT,'(A,I8)') ' + HOUR OF THE DAY.......................: ', VALUES(5)
  WRITE(LOGUNIT,'(A,I8)') ' + MINUTES OF THE HOUR...................: ', VALUES(6)
  WRITE(LOGUNIT,'(A,I8)') ' + SECONDS OF THE MINUTE.................: ', VALUES(7)
  WRITE(LOGUNIT,'(A,I8)') ' + MILLISECONDS OF THE SECOND............: ', VALUES(8)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE LOG_CURR_TIME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'OM_READ_ENVVAR'
SUBROUTINE OM_READ_ENVVAR( CDENVVARNAME, CDENVVARVAL, NDLEN )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIM_K
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),     INTENT(IN)  :: CDENVVARNAME
  CHARACTER(LEN=*),     INTENT(OUT) :: CDENVVARVAL
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: NDLEN

  ! Local variables
  INTEGER(KIND=JPIM_K) :: STAT
  INTEGER(KIND=JPIM_K) :: NDLEN4

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Check if the envisonment variable is readable
  CALL GET_ENVIRONMENT_VARIABLE( CDENVVARNAME, LENGTH=NDLEN4, STATUS=STAT )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 1 )
  PP_DEBUG_CRITICAL_COND_THROW( LEN(CDENVVARVAL) .LT. NDLEN4, 2 )

  ! Initialize the variable
  CDENVVARVAL = REPEAT(' ',LEN(CDENVVARVAL))

  ! Read the environment variable
  CALL GET_ENVIRONMENT_VARIABLE( CDENVVARNAME, VALUE=CDENVVARVAL, STATUS=STAT )
  NDLEN = NDLEN4

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, 3 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Impossible to read environment variable: '//TRIM(CDENVVARNAME) )

    CASE (2)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Output ariable too short for the environment variable' )

    CASE (3)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Failed to read environment variable: '//TRIM(CDENVVARNAME) )

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

END SUBROUTINE OM_READ_ENVVAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'OM_MKDIR'
SUBROUTINE OM_MKDIR( DIRECTORY_NAME, MODE )

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),               INTENT(IN) :: DIRECTORY_NAME
  INTEGER(KIND=JPIB_K), OPTIONAL, INTENT(IN) :: MODE

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_INT)  :: C_MODE
  INTEGER(KIND=C_INT)  :: C_STATUS
  CHARACTER(KIND=C_CHAR,LEN=LEN_TRIM(DIRECTORY_NAME)+1), TARGET :: C_DIRECTORY_NAME

  ! Local parameters (Default permissions to be used to create a directory)
  INTEGER(C_INT), PARAMETER :: S_IRWXU = 448

  ! Explicit interfaces
  INTERFACE
    FUNCTION C_MKDIR( C_PATH, C_MODE ) RESULT(C_STATUS) BIND(C, NAME="mkdir")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR),    VALUE, INTENT(IN) :: C_PATH
      INTEGER(C_INT), VALUE, INTENT(IN) :: C_MODE
      INTEGER(C_INT) :: C_STATUS
    END FUNCTION C_MKDIR
  END INTERFACE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Fill the c character
  C_DIRECTORY_NAME = REPEAT(C_NULL_CHAR,LEN_TRIM(DIRECTORY_NAME)+1)
  DO I = 1, LEN_TRIM(DIRECTORY_NAME)
      C_DIRECTORY_NAME(I:I) = DIRECTORY_NAME(I:I)
  ENDDO

  ! CALL the C function
  IF (PRESENT(MODE)) THEN
      C_MODE = INT(MODE,C_INT)
      C_STATUS = C_MKDIR( C_LOC(C_DIRECTORY_NAME), C_MODE )
  ELSE
      C_STATUS = C_MKDIR( C_LOC(C_DIRECTORY_NAME), S_IRWXU )
  END IF

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( C_STATUS.NE.0, 1 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to create directory: '//TRIM(DIRECTORY_NAME) )

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

END SUBROUTINE OM_MKDIR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'OM_CHDIR'
SUBROUTINE OM_CHDIR( DIRECTORY_NAME )

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*), INTENT(IN) :: DIRECTORY_NAME

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_INT)  :: C_STATUS
  CHARACTER(KIND=C_CHAR,LEN=LEN_TRIM(DIRECTORY_NAME)+1), TARGET :: C_DIRECTORY_NAME

  ! Explicit interfaces
  INTERFACE
    FUNCTION C_CHDIR( C_PATH ) RESULT(C_STATUS) BIND(C, NAME="chdir")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      TYPE(C_PTR),    VALUE, INTENT(IN) :: C_PATH
      INTEGER(C_INT) :: C_STATUS
    END FUNCTION C_CHDIR
  END INTERFACE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Fill the c character
  C_DIRECTORY_NAME = REPEAT(C_NULL_CHAR,LEN_TRIM(DIRECTORY_NAME)+1)
  DO I = 1, LEN_TRIM(DIRECTORY_NAME)
      C_DIRECTORY_NAME(I:I) = DIRECTORY_NAME(I:I)
  ENDDO

  ! CALL the C function
  C_STATUS = C_CHDIR( C_LOC(C_DIRECTORY_NAME) )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( C_STATUS.NE.0, 1 )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to move into directory: '//TRIM(DIRECTORY_NAME) )

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

END SUBROUTINE OM_CHDIR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'OM_TOUPPER'
SUBROUTINE OM_TOUPPER( IOSTRING )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*), INTENT(INOUT) :: IOSTRING

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: ASCIIVALUE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Convert each character to uppercase
  DO I = 1, LEN_TRIM(IOSTRING)
    ASCIIVALUE = ICHAR(IOSTRING(I:I))
    IF (ASCIIVALUE .GE. ICHAR('a') .AND. ASCIIVALUE .LE. ICHAR('z')) THEN
      IOSTRING(I:I) = CHAR(ASCIIVALUE - ICHAR('a') + ICHAR('A'))
    END IF
  END DO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE OM_TOUPPER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'OM_TOLOWER'
SUBROUTINE OM_TOLOWER( IOSTRING )

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*), INTENT(INOUT) :: IOSTRING

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: ASCIIVALUE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Convert each character to lower
  DO I = 1, LEN_TRIM(IOSTRING)
    ASCIIVALUE = ICHAR(IOSTRING(I:I))
    IF (ASCIIVALUE .GE. ICHAR('A') .AND. ASCIIVALUE .LE. ICHAR('Z')) THEN
      IOSTRING(I:I) = CHAR(ASCIIVALUE - ICHAR('A') + ICHAR('a'))
    END IF
  END DO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

END SUBROUTINE OM_TOLOWER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'OM_GET_HOSTNAME'
SUBROUTINE OM_GET_HOSTNAME( CDHOSTNAME )

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*), INTENT(OUT) :: CDHOSTNAME

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=C_SIZE_T)  :: C_LENGTH
  INTEGER(KIND=C_INT)     :: C_STATUS
  CHARACTER(KIND=C_CHAR,LEN=LEN(CDHOSTNAME)+1), TARGET :: C_CLHOSTNAME

  ! Local parameters
  INTEGER(C_INT), PARAMETER :: HOST_NAME_MAX = 255

  ! Explicit interfaces
  INTERFACE
    FUNCTION C_GETHOSTNAME( C_CDVAR, C_NDLEN ) RESULT(C_STATUS) BIND(C, NAME="gethostname")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_SIZE_T
    IMPLICIT NONE
      TYPE(C_PTR),            VALUE, INTENT(IN) :: C_CDVAR
      INTEGER(KIND=C_SIZE_T), VALUE, INTENT(IN) :: C_NDLEN
      INTEGER(C_INT) :: C_STATUS
    END FUNCTION C_GETHOSTNAME
  END INTERFACE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialise c buffers
  C_CLHOSTNAME = REPEAT(C_NULL_CHAR,LEN(C_CLHOSTNAME))
  C_LENGTH = INT( LEN(C_CLHOSTNAME), C_INT )

  ! Call the c function
  C_STATUS = C_GETHOSTNAME( C_LOC(C_CLHOSTNAME), C_LENGTH )

  ! Error handling
  PP_DEBUG_CRITICAL_COND_THROW( C_STATUS.NE.0, 1 )

  ! Copy the hostname back to a fortran string
  CDHOSTNAME = REPEAT(' ', LEN(CDHOSTNAME))
  I = 1
  DO
    IF ( I .LE. LEN(CDHOSTNAME)  .AND. C_CLHOSTNAME(I:I) .NE. C_NULL_CHAR) THEN
      CDHOSTNAME(I:I) = C_CLHOSTNAME(I:I)
      I = I + 1
    ELSE
      EXIT
    ENDIF
  ENDDO

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point
  RETURN

! Error handler
PP_ERROR_HANDLER

  ErrorHandler: BLOCK

    ! Error handling variables
    CHARACTER(LEN=:), ALLOCATABLE :: STR

    ! Handle different errors
    SELECT CASE(ERRIDX)

    CASE (1)
      PP_DEBUG_CREATE_ERROR_MSG( STR, 'Unable to read hostname' )

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


END SUBROUTINE OM_GET_HOSTNAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'OM_GETPID'
FUNCTION OM_GETPID( ) RESULT(PID)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Function result
  INTEGER(KIND=JPIB_K) :: PID

  ! Local variables
  INTEGER(KIND=C_INT)  :: C_PID
  ! Local parameters (Default permissions to be used to create a directory)
  INTEGER(C_INT), PARAMETER :: S_IRWXU = 448

  ! Explicit interfaces
  INTERFACE
    FUNCTION C_GETPID( ) RESULT(C_PID) BIND(C, NAME="getpid")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT
    IMPLICIT NONE
      INTEGER(C_INT) :: C_PID
    END FUNCTION C_GETPID
  END INTERFACE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Call the c function
  C_PID = C_GETPID()

  ! Cast the result
  PID = INT(C_PID, KIND(PID) )

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION OM_GETPID
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'OM_GETMEM'
SUBROUTINE OM_GETMEM( TOT_MEM, SYS_USAGE, TASK_USAGE )

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT64_T
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(OUT)  :: TOT_MEM
  INTEGER(KIND=JPIB_K), INTENT(OUT)  :: SYS_USAGE
  INTEGER(KIND=JPIB_K), INTENT(OUT)  :: TASK_USAGE

  ! Local variables
  INTEGER(KIND=C_INT64_T), TARGET :: C_TOT_MEM
  INTEGER(KIND=C_INT64_T), TARGET :: C_SYS_USAGE
  INTEGER(KIND=C_INT64_T), TARGET :: C_TASK_USAGE

  ! Explicit interfaces
  INTERFACE
    SUBROUTINE C_GET_MEM_USAGE( C_TOT_MEM, C_SYS_USAGE, C_TASK_USAGE) BIND(C, NAME="om_get_mem_usage")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: C_TOT_MEM
      TYPE(C_PTR), VALUE, INTENT(IN) :: C_SYS_USAGE
      TYPE(C_PTR), VALUE, INTENT(IN) :: C_TASK_USAGE
    END SUBROUTINE C_GET_MEM_USAGE
  END INTERFACE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation of the variables
  C_TOT_MEM    = INT( -99, C_INT64_T)
  C_SYS_USAGE  = INT( -99, C_INT64_T)
  C_TASK_USAGE = INT( -99, C_INT64_T)

  ! Call the utility to read the memory state
  CALL C_GET_MEM_USAGE( C_LOC(C_TOT_MEM), C_LOC(C_SYS_USAGE), C_LOC(C_TASK_USAGE) )

  ! Cast the result
  TOT_MEM    = INT( C_TOT_MEM,    JPIB_K)
  SYS_USAGE  = INT( C_SYS_USAGE,  JPIB_K)
  TASK_USAGE = INT( C_TASK_USAGE, JPIB_K)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END SUBROUTINE OM_GETMEM
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'FORMAT_MEMORY'
FUNCTION FORMAT_MEMORY(BYTES) RESULT(FORMATTED_MEMORY)

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K
  USE :: OM_CORE_MOD, ONLY: JPRD_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(IN) :: BYTES

  ! Function result
  CHARACTER(LEN=32) :: FORMATTED_MEMORY

  ! Local variables
  REAL(KIND=JPRD_K) :: MEM
  CHARACTER(4) :: SUFFIX

  ! define the suffixes for memory units
  CHARACTER(LEN=4), DIMENSION(7), PARAMETER :: UNITS(7) = ['[B ]', '[KB]', '[MB]', '[GB]', '[TB]', '[PB]', '[EB]']
  INTEGER(KIND=JPIB_K) :: I

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! convert bytes to the appropriate unit
  MEM = REAL(BYTES, JPRD_K)
  DO I = 1, SIZE(UNITS)
    IF ( MEM .LT. 1024.0_JPRD_K ) THEN
      SUFFIX = UNITS(I)
      EXIT
    ENDIF
    MEM = MEM / 1024.0_JPRD_K
  END DO

  ! format the memory value with the appropriate suffix
  WRITE(FORMATTED_MEMORY, '(F10.2,A)') MEM, SUFFIX

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION FORMAT_MEMORY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'OM_TOC'
SUBROUTINE OM_TOC( NSEC, DNSEC )

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT64_T
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Function result
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: NSEC
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: DNSEC

  ! Local variables
  INTEGER(KIND=C_INT64_T), TARGET :: C_NSEC
  INTEGER(KIND=C_INT64_T), TARGET :: C_DNSEC

  ! Explicit interfaces
  INTERFACE
    SUBROUTINE C_TOC( C_NSEC, C_DNSEC) BIND(C, NAME="om_toc")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT64_T
    IMPLICIT NONE
      INTEGER(C_INT64_T), VALUE, INTENT(IN) :: C_NSEC
      TYPE(C_PTR),        VALUE, INTENT(IN) :: C_DNSEC
    END SUBROUTINE C_TOC
  END INTERFACE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation of the variables
  C_NSEC   = INT( NSEC, C_INT64_T)
  C_DNSEC  = INT( -99,  C_INT64_T)

  ! Call the utility to read the memory state
  CALL C_TOC( C_NSEC, C_LOC(C_DNSEC) )

  ! Cast the result
  DNSEC = INT( C_DNSEC, JPIB_K)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END SUBROUTINE OM_TOC
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'SUBROUTINE'
#define PP_PROCEDURE_NAME 'OM_TIC'
SUBROUTINE OM_TIC( NSEC )

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT64_T
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  INTEGER(KIND=JPIB_K), INTENT(OUT) :: NSEC

  ! Local variables
  INTEGER(KIND=C_INT64_T), TARGET :: C_NSEC

  ! Explicit interfaces
  INTERFACE
    SUBROUTINE C_TIC( C_NSEC ) BIND(C, NAME="om_tic")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: C_NSEC
    END SUBROUTINE C_TIC
  END INTERFACE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation of the variables
  C_NSEC  = INT( -99,  C_INT64_T)

  ! Call the utility to read the memory state
  CALL C_TIC( C_LOC(C_NSEC) )

  ! Cast the result
  NSEC = INT( C_NSEC, JPIB_K)

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END SUBROUTINE OM_TIC
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'OM_IS_LITTLE_ENDIAN'
FUNCTION OM_IS_LITTLE_ENDIAN( ) RESULT(LDRET)

  ! Symbols imported from intrinsic modules
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_INT8_T
  USE, INTRINSIC :: ISO_C_BINDING,   ONLY: C_LOC

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Function result
  LOGICAL :: LDRET

  ! Local variables
  INTEGER(KIND=C_INT8_T), TARGET :: C_ISLITTLE

  ! Explicit interfaces
  INTERFACE
    SUBROUTINE C_IS_LITTLE_ENDIAN( C_ISLITTLE ) BIND(C, NAME="om_is_little_endian")
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
      TYPE(C_PTR), VALUE, INTENT(IN) :: C_ISLITTLE
    END SUBROUTINE C_IS_LITTLE_ENDIAN
  END INTERFACE

  ! Local variables declared by the preprocessor for debugging purposes
  PP_DEBUG_DECL_VARS

  ! Local variables declared by the preprocessor for tracing purposes
  PP_TRACE_DECL_VARS

  ! Trace begin of procedure
  PP_TRACE_ENTER_PROCEDURE()

  ! Initialisation of the variables
  C_ISLITTLE  = INT( -99,  C_INT8_T)

  ! Call the utility to read the memory state
  CALL C_IS_LITTLE_ENDIAN( C_LOC(C_ISLITTLE) )

  ! Cast the result
  IF ( C_ISLITTLE .EQ. 0_C_INT8_T ) THEN
    LDRET = .FALSE.
  ELSE
    LDRET = .TRUE.
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point (on success)
  RETURN

END FUNCTION OM_IS_LITTLE_ENDIAN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE OM_GENERAL_UTILS_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
