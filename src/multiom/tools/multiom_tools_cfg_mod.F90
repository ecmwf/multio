! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"

! Definition of the module
#define PP_FILE_NAME 'multiom_tools_cfg_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'MULTIOM_TOOLS_CFG_MOD'
MODULE MULTIOM_TOOLS_CFG_MOD

  ! Symbols imported from the main library of the project.
  USE :: MULTIOM_API, ONLY: JPIB_K

IMPLICIT NONE

! Default visibility
PRIVATE

TYPE :: INT_CONTAINER_T
  INTEGER(KIND=JPIB_K) :: PAR
  TYPE(INT_CONTAINER_T), POINTER :: NEXT=>NULL()
END TYPE

TYPE :: COMMAND_LINE_ARGS_T
  LOGICAL :: DRYRUN
  LOGICAL :: VERBOSE
  LOGICAL :: BIG_ENDIAN_READ
  INTEGER(KIND=JPIB_K) :: PROC_IDX=1
  INTEGER(KIND=JPIB_K) :: NPROCS=-99
  INTEGER(KIND=JPIB_K) :: NENTRIES
  CHARACTER(LEN=32)    :: OUTPUT_MANAGER_TYPE
  CHARACTER(LEN=4096)  :: INPUT_DIR
  CHARACTER(LEN=4096)  :: YAML_CONFIGURATION
  INTEGER(JPIB_K), DIMENSION(:), ALLOCATABLE :: PARAM_ID
  INTEGER(JPIB_K), DIMENSION(:), ALLOCATABLE :: ZPARAM_ID
  INTEGER(JPIB_K), DIMENSION(:), ALLOCATABLE :: U_ID
  INTEGER(JPIB_K), DIMENSION(:), ALLOCATABLE :: STEP_ID
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: REPRES_ID
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: PREFIX_ID
END TYPE

! Whitelist of public symbols (Datatypes)
PUBLIC :: COMMAND_LINE_ARGS_T

! Whitelist of public symbols (procedures)
PUBLIC :: MATCH
PUBLIC :: INIT_COMMAND_LINE_OPTIONS
PUBLIC :: FREE_COMMAND_LINE_OPTIONS
PUBLIC :: PRINT_COMMAND_LINE_OPTIONS
PUBLIC :: PARSE_COMMAND_LINE_OPTIONS

CONTAINS

#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'MATCH'
PP_THREAD_SAFE FUNCTION MATCH( CFG, PARAM_ID, U_ID, STEP_ID, &
& PROC_ID, REPRES_ID, PREFIX_ID, LMATCH, HOOKS ) RESULT(RET)

  ! Symbols imported from the main library of the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: CFG
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: PARAM_ID
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: U_ID
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: STEP_ID
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: PROC_ID
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: REPRES_ID
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: PREFIX_ID
  LOGICAL,                   INTENT(OUT)   :: LMATCH
  TYPE(HOOKS_T),             INTENT(IN)    :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

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

  LMATCH = .TRUE.
  IF ( LMATCH .AND. ALLOCATED( CFG%PARAM_ID ) ) THEN
    LMATCH = ANY( PARAM_ID .EQ. CFG%PARAM_ID )
  ENDIF

  IF ( LMATCH .AND. ALLOCATED( CFG%ZPARAM_ID ) ) THEN
    LMATCH = .NOT.ANY( PARAM_ID .EQ. CFG%ZPARAM_ID )
  ENDIF

  IF ( LMATCH .AND. ALLOCATED( CFG%U_ID ) ) THEN
    LMATCH = ANY( U_ID .EQ. CFG%U_ID )
  ENDIF

  IF ( LMATCH .AND. ALLOCATED( CFG%STEP_ID ) ) THEN
    LMATCH = ANY( STEP_ID .EQ. CFG%STEP_ID )
  ENDIF

  IF ( LMATCH .AND. ALLOCATED( CFG%REPRES_ID ) ) THEN
    LMATCH = ANY( REPRES_ID .EQ. CFG%REPRES_ID )
  ENDIF

  IF ( LMATCH .AND. ALLOCATED( CFG%PREFIX_ID ) ) THEN
    LMATCH = ANY( PREFIX_ID .EQ. CFG%PREFIX_ID )
  ENDIF

  ! Trace end of procedure (on success)
  PP_TRACE_EXIT_PROCEDURE_ON_SUCCESS()

  ! Exit point on success
  RETURN

END FUNCTION MATCH
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'USAGE'
PP_THREAD_SAFE FUNCTION USAGE( HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE


  ! Dummy arguments
  TYPE(HOOKS_T), INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STATUS

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

  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + PROTOTYPE: ./outputManager.x [options]'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + --------------------------------------'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + OPTIONS ARE: '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + -t || --output-manager-type :: type of output manager to use. (Default: "NO-IO-INFO-LOG")'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' +                                 |-> "NO-IO-INFO-LOG"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' +                                 |-> "DUMP-FORTRAN-DATA-REPRODUCER"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' +                                 |-> "GRIB-MSG-TO-FILE"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' +                                 |-> "GRIB-MSG-TO-MULTIO"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' +                                 |-> "GRIB-HEADER-TO-MULTIO"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' +                                 |-> "FULL-GRIB-HEADER-TO-MULTIO"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' +                                 |-> "FORTRAN-METADATA-TO-MULTIO"'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + -i || --input-dir           :: input directory where all the binary reproducers are (Default: ".")'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + -y || --yaml-cfg            :: name of the "yaml" configuration file of the output manager (Default: "./output_manager_cfg.yaml")'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + -d || --dry-run             :: Just print to screen the "toc.bin" file. (Default: .FALSE.)'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + -v || --verbose             :: Enable verbosity in read operations. (Default: .FALSE.)'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + -b || --big-endian-read     :: Convert endian to big endian in read. This feature has to be used on macos (Default: .FALSE.)'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + -p || --param-id            :: list of the param-ids to be processed. Then grammar can be:'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' +                                 |-> "*"         a star: all the param ids are processd (Defaul behaviour)'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' +                                 |-> "1"         a number: only the paramid that match the number is processed'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' +                                 |-> "[1,2,3,4]" a list of numbers: all the param ids that match the numbers in the list are processed'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + -z || --excluded-param-id   :: list of the param-ids to be excluded'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + -l || --level               :: list of levels to be processed. The grammar is the same used for param ids'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + -s || --step                :: list of steps to be processed. The grammar is the same used for param ids'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + -r || --representation      :: [1=gridded, 2=spectral]'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + -n || --n-procs             :: use data from a specific io-server (number of processors of the io-server to use)'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + -q || --level-type          :: [1=model_level, 2=pressure_level, 3=vorticity_level, 4=theta_level, 5=surface_level, 6=wave_int, 7=wave_spec]'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + -h || --help || ?           :: print this message'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' +'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + ATTENTION: the values NEED to be surrounded by double quotes to avoid unexpected behaviours!!!!'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + EXAMPLE: ecom-feed  -t NO-IO-INFO-LOG  -i "." -y "/ec/res4/scratch/mavm/develop_v7/raps/multio_yaml/output-manager-config.yaml"   -v -p [137]'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STATUS) ' + '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )

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
    CASE(ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write to the output unit' )
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

END FUNCTION USAGE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'INIT_COMMAND_LINE_OPTIONS'
PP_THREAD_SAFE FUNCTION INIT_COMMAND_LINE_OPTIONS( CFG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: CFG
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 1_JPIB_K

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

  ! Default initialisation of command line options
  CFG%DRYRUN = .FALSE.
  CFG%VERBOSE = .FALSE.
  CFG%BIG_ENDIAN_READ = .FALSE.
  CFG%OUTPUT_MANAGER_TYPE = 'NO-IO-INFO-LOG'
  CFG%INPUT_DIR = '.'
  CFG%YAML_CONFIGURATION = './output_manager_cfg.yaml'
  CFG%NPROCS = -99

  IF ( ALLOCATED(CFG%PARAM_ID) ) THEN
    DEALLOCATE( CFG%PARAM_ID, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  ENDIF

  IF ( ALLOCATED(CFG%U_ID) ) THEN
    DEALLOCATE( CFG%U_ID, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  ENDIF

  IF ( ALLOCATED(CFG%STEP_ID) ) THEN
    DEALLOCATE( CFG%STEP_ID, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  ENDIF

  IF ( ALLOCATED(CFG%REPRES_ID ) ) THEN
    DEALLOCATE(CFG%REPRES_ID, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  ENDIF

  IF ( ALLOCATED(CFG%PREFIX_ID ) ) THEN
    DEALLOCATE( CFG%PREFIX_ID, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
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
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate memory' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      ENDIF
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

END FUNCTION INIT_COMMAND_LINE_OPTIONS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'FREE_COMMAND_LINE_OPTIONS'
PP_THREAD_SAFE FUNCTION FREE_COMMAND_LINE_OPTIONS( CFG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: CFG
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 1_JPIB_K

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

  ! Default initialisation of command line options
  CFG%DRYRUN = .FALSE.
  CFG%VERBOSE = .FALSE.
  CFG%BIG_ENDIAN_READ = .FALSE.
  CFG%OUTPUT_MANAGER_TYPE = REPEAT( ' ', 32 )
  CFG%INPUT_DIR = REPEAT( ' ', 4096 )
  CFG%YAML_CONFIGURATION = REPEAT( ' ', 4096 )
  CFG%NPROCS = 1

  IF ( ALLOCATED( CFG%PARAM_ID) ) THEN
    DEALLOCATE( CFG%PARAM_ID, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  ENDIF

  IF ( ALLOCATED( CFG%U_ID) ) THEN
    DEALLOCATE( CFG%U_ID, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  ENDIF

  IF ( ALLOCATED( CFG%STEP_ID) ) THEN
    DEALLOCATE( CFG%STEP_ID, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  ENDIF

  IF ( ALLOCATED( CFG%REPRES_ID ) ) THEN
    DEALLOCATE( CFG%REPRES_ID, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  ENDIF

  IF ( ALLOCATED( CFG%PREFIX_ID ) ) THEN
    DEALLOCATE( CFG%PREFIX_ID, STAT=DEALLOC_STAT, ERRMSG=ERRMSG )
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
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
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate memory' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      ENDIF
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

END FUNCTION FREE_COMMAND_LINE_OPTIONS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_COMMAND_LINE_OPTIONS'
PP_THREAD_SAFE FUNCTION PRINT_COMMAND_LINE_OPTIONS( CFG, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: CFG
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: WRITE_STAT
  CHARACTER(LEN=1024) :: CTMP
  CHARACTER(LEN=32) :: CINT
  CHARACTER(LEN=32) :: CFMT

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

  ! Print values of the configuration
  WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT) '  '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A150)', IOSTAT=WRITE_STAT) REPEAT('-',150)
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT) ' ECOM :: Command line options'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A150)', IOSTAT=WRITE_STAT) REPEAT('-',150)
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT) '+ DryRun                  :: ', CFG%DRYRUN
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT) '+ Verbose                 :: ', CFG%VERBOSE
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT) '+ Big endian read         :: ', CFG%BIG_ENDIAN_READ
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT) '+ Output manager type     :: ', TRIM(ADJUSTL(CFG%OUTPUT_MANAGER_TYPE))
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )

  CTMP=REPEAT(' ',LEN(CTMP))
  CINT=REPEAT(' ',32)
  CFMT=REPEAT(' ',32)
  CTMP=' + Input directory         :: '//TRIM(ADJUSTL(CFG%INPUT_DIR))
  WRITE(CINT,'(I10)', IOSTAT=WRITE_STAT) LEN_TRIM(CTMP)
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(CFMT,'(A)', IOSTAT=WRITE_STAT) '(A'//TRIM(ADJUSTL(CINT))//')'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STAT) TRIM(CTMP)
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )

  CTMP=REPEAT(' ',LEN(CTMP))
  CINT=REPEAT(' ',32)
  CFMT=REPEAT(' ',32)
  CTMP=' + YAML configuration file :: '//TRIM(ADJUSTL(CFG%YAML_CONFIGURATION))
  WRITE(CINT,'(I10)', IOSTAT=WRITE_STAT) LEN_TRIM(CTMP)
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(CFMT,'(A)', IOSTAT=WRITE_STAT) '(A'//TRIM(ADJUSTL(CINT))//')'
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,'(A)', IOSTAT=WRITE_STAT) TRIM(CTMP)
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )

  IF ( ALLOCATED( CFG%PARAM_ID) ) THEN
    CINT=REPEAT(' ',32)
    WRITE(CINT,'(I10)', IOSTAT=WRITE_STAT) CFG%PARAM_ID
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT)  '+ param ids               :: '//TRIM(ADJUSTL(CINT))
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  ELSE
    WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT)  '+ param ids               :: all'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  ENDIF

  IF ( ALLOCATED( CFG%U_ID) ) THEN
    CINT=REPEAT(' ',32)
    WRITE(CINT,'(I10)', IOSTAT=WRITE_STAT) CFG%U_ID
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT)  '+ levels ids              :: '//TRIM(ADJUSTL(CINT))
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  ELSE
    WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT)  '+ levels ids              :: all'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  ENDIF

  IF ( ALLOCATED( CFG%STEP_ID) ) THEN
    CINT=REPEAT(' ',32)
    WRITE(CINT,'(I10)', IOSTAT=WRITE_STAT) CFG%STEP_ID
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT)  '+ steps ids               :: '//TRIM(ADJUSTL(CINT))
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  ELSE
    WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT)  '+ steps ids               :: all'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  ENDIF

  IF ( ALLOCATED( CFG%REPRES_ID) ) THEN
    CINT=REPEAT(' ',32)
    WRITE(CINT,'(I10)', IOSTAT=WRITE_STAT) CFG%REPRES_ID
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT)  '+ representation          :: '//TRIM(ADJUSTL(CINT))
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  ELSE
    WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT)  '+ representation          :: all'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  ENDIF

  IF ( ALLOCATED( CFG%PREFIX_ID) ) THEN
    CINT=REPEAT(' ',32)
    WRITE(CINT,'(I10)', IOSTAT=WRITE_STAT) CFG%PREFIX_ID
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT)  '+ prefix                  :: '//TRIM(ADJUSTL(CINT))
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  ELSE
    WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT)  '+ prefix                  :: all'
    PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  ENDIF
  WRITE(OUTPUT_UNIT,'(A150)', IOSTAT=WRITE_STAT) REPEAT('-',150)
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
  WRITE(OUTPUT_UNIT,*, IOSTAT=WRITE_STAT) '  '
  PP_DEBUG_CRITICAL_COND_THROW( WRITE_STAT .NE. 0, ERRFLAG_UNABLE_TO_WRITE )

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
    CASE(ERRFLAG_UNABLE_TO_WRITE)
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

END FUNCTION PRINT_COMMAND_LINE_OPTIONS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PARSE_COMMAND_LINE_OPTIONS'
PP_THREAD_SAFE FUNCTION PARSE_COMMAND_LINE_OPTIONS( CFG, HOOKS ) RESULT(RET)

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIM_K
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: CFG
  TYPE(HOOKS_T),             INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  CHARACTER(LEN=4096) :: TMP
  INTEGER(KIND=JPIB_K) :: NUM_ARGS
  INTEGER :: I
  INTEGER(KIND=JPIB_K) :: POS
  INTEGER(KIND=JPIB_K) :: N
  INTEGER :: STATUS
  CHARACTER(LEN=1024) :: COMMAND_LINE_ARG
  CHARACTER(LEN=1024) :: COMMAND_LINE_SWITCH
  CHARACTER(LEN=1024) :: TMP_STR
  CHARACTER(LEN=1) :: S
  LOGICAL :: EX

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_COMMAND_LINE_ARG = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_INTEGER_ARRAY = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_INTEGER = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNDEFINED_FLAG = 5_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_USAGE = 6_JPIB_K

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

  ! Read the number of arguments
  NUM_ARGS = COMMAND_ARGUMENT_COUNT()
  I = 0

  ! Consume the command line arguments
  CommandLineArgsLoop:  DO

    !> Initialize vars
    COMMAND_LINE_ARG=REPEAT(' ',1024)
    COMMAND_LINE_SWITCH=REPEAT(' ',1024)
    TMP_STR=REPEAT(' ',1024)

    !> Update the argument counter
    I = I + 1

    !> Exit condition
    IF ( I .GT. NUM_ARGS ) THEN
      EXIT CommandLineArgsLoop
    ENDIF

    !> Read the i-th command line argument
    CALL GET_COMMAND_ARGUMENT( I, COMMAND_LINE_ARG, STATUS=STATUS )
    PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_READ_COMMAND_LINE_ARG )

    !> Extract the switch
    COMMAND_LINE_SWITCH = TRIM(ADJUSTL(COMMAND_LINE_ARG))

    !> Select the correct behaviour
    SELECT CASE(COMMAND_LINE_SWITCH)

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-t', '--output-manager-type' )
      I = I + 1
      CFG%OUTPUT_MANAGER_TYPE = REPEAT(' ',32)
      TMP = REPEAT( ' ', 4096 )
      CALL GET_COMMAND_ARGUMENT( I, TMP, STATUS=STATUS )
      PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_READ_COMMAND_LINE_ARG )
      CFG%OUTPUT_MANAGER_TYPE = TRIM(TMP)

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-i', '--input-dir' )
      I = I + 1
      CFG%INPUT_DIR = REPEAT(' ',4096)
      CALL GET_COMMAND_ARGUMENT( I, CFG%INPUT_DIR, STATUS=STATUS )
      PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_READ_COMMAND_LINE_ARG )

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-y', '--yaml-cfg' )
      I = I + 1
      CFG%YAML_CONFIGURATION = REPEAT(' ',4096)
      CALL GET_COMMAND_ARGUMENT( I, CFG%YAML_CONFIGURATION, STATUS=STATUS )
      PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_READ_COMMAND_LINE_ARG )

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-d', '--dry-run' )
      CFG%DRYRUN = .TRUE.

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-v', '--verbose' )
      CFG%VERBOSE = .TRUE.

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-b', '--big-endian-read' )
      CFG%BIG_ENDIAN_READ = .TRUE.

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-p', '--param-id' )
      I = I + 1
      TMP = REPEAT(' ',4096)
      CALL GET_COMMAND_ARGUMENT( I, TMP, STATUS=STATUS )
      PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_READ_COMMAND_LINE_ARG )
      IF ( TRIM(ADJUSTL(TMP)) .EQ. '*' ) THEN
        WRITE(*,*,IOSTAT=STATUS) 'parameters = all'
        PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
      ELSE
        POS = 0
        PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_INTEGER_ARRAY) READ_INTEGER_ARRAY( TRIM(TMP), POS, '-', CFG%PARAM_ID, S, HOOKS )
      ENDIF

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-z', '--excluded-param-id' )
      I = I + 1
      TMP = REPEAT(' ',4096)
      CALL GET_COMMAND_ARGUMENT( I, TMP, STATUS=STATUS )
      PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_READ_COMMAND_LINE_ARG )
      IF ( TRIM(ADJUSTL(TMP)) .EQ. '*' ) THEN
        WRITE(*,*,IOSTAT=STATUS) 'parameters = all'
        PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
      ELSE
        POS = 0
        PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_INTEGER_ARRAY) READ_INTEGER_ARRAY( TRIM(TMP), POS, '-', CFG%ZPARAM_ID, S, HOOKS )
      ENDIF

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-l', '--level' )
      I = I + 1
      CALL GET_COMMAND_ARGUMENT( I, TMP, STATUS=STATUS )
      PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_READ_COMMAND_LINE_ARG )
      IF ( TRIM(ADJUSTL(TMP)) .EQ. '*' ) THEN
        WRITE(*,*,IOSTAT=STATUS) 'levels = all'
        PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
      ELSE
        POS = 0
        PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_INTEGER_ARRAY) READ_INTEGER_ARRAY( TRIM(TMP), POS, '-', CFG%U_ID, S, HOOKS )
      ENDIF

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-s', '--step' )
      I = I + 1
      CALL GET_COMMAND_ARGUMENT( I, TMP, STATUS=STATUS )
      PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_READ_COMMAND_LINE_ARG )
      IF ( TRIM(ADJUSTL(TMP)) .EQ. '*' ) THEN
        WRITE(*,*,IOSTAT=STATUS) 'steps = all'
        PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
      ELSE
        POS = 0
        PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_INTEGER_ARRAY) READ_INTEGER_ARRAY( TRIM(TMP), POS, '-', CFG%STEP_ID, S, HOOKS )
      ENDIF

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-r', '--representation' )
      I = I + 1
      CALL GET_COMMAND_ARGUMENT( I, TMP, STATUS=STATUS )
      PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_READ_COMMAND_LINE_ARG )
      IF ( TRIM(ADJUSTL(TMP)) .EQ. '*' ) THEN
        WRITE(*,*,IOSTAT=STATUS) 'steps = all'
        PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
      ELSE
        POS = 0
        PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_INTEGER_ARRAY) READ_INTEGER_ARRAY( TRIM(TMP), POS, '-', CFG%REPRES_ID, S, HOOKS )
      ENDIF

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-n', '--n-procs' )
      I = I + 1
      CALL GET_COMMAND_ARGUMENT( I, TMP, STATUS=STATUS )
      PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_READ_COMMAND_LINE_ARG )
      IF ( TRIM(ADJUSTL(TMP)) .EQ. '*' ) THEN
        WRITE(*,*,IOSTAT=STATUS) 'steps = all'
        PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
      ELSE
        POS = 0
        PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_INTEGER) READ_INTEGER( TRIM(TMP), POS, '-', CFG%NPROCS, S, HOOKS )
      ENDIF

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-q', '--level-type' )
      I = I + 1
      CALL GET_COMMAND_ARGUMENT( I, TMP, STATUS=STATUS )
      PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_READ_COMMAND_LINE_ARG )
      IF ( TRIM(ADJUSTL(TMP)) .EQ. '*' ) THEN
        WRITE(*,*,IOSTAT=STATUS) 'steps = all'
        PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
      ELSE
        POS = 0
        PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_INTEGER_ARRAY) READ_INTEGER_ARRAY( TRIM(TMP), POS, '-', CFG%PREFIX_ID, S, HOOKS )
      ENDIF

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-h', '--help', '?' )
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_USAGE) USAGE( HOOKS )
      STOP

    ! ----------------------------------------------------------------------------------------------
    CASE DEFAULT
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_USAGE) USAGE( HOOKS )
      WRITE(*,*,IOSTAT=STATUS) 'ERROR :: Unknown command line argument ||', COMMAND_LINE_ARG, '||'
      PP_DEBUG_CRITICAL_COND_THROW( STATUS .NE. 0, ERRFLAG_UNABLE_TO_WRITE )
      PP_DEBUG_CRITICAL_THROW( ERRFLAG_UNDEFINED_FLAG )

    END SELECT

  ENDDO CommandLineArgsLoop

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
    CASE(ERRFLAG_UNABLE_TO_READ_COMMAND_LINE_ARG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read command line argument' )
    CASE(ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write' )
    CASE(ERRFLAG_UNABLE_TO_READ_INTEGER_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read integer array' )
    CASE(ERRFLAG_UNABLE_TO_READ_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read integer' )
    CASE(ERRFLAG_UNDEFINED_FLAG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Undefined flag' )
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'passed flag: "'//TRIM(ADJUSTL(COMMAND_LINE_ARG))//'"' )
    CASE(ERRFLAG_UNABLE_TO_CALL_USAGE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call usage' )
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

END FUNCTION PARSE_COMMAND_LINE_OPTIONS
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_INTEGER_ARRAY'
PP_THREAD_SAFE FUNCTION READ_INTEGER_ARRAY( UNIT, POS, TERM_, N, S, HOOKS ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                                INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),                            INTENT(INOUT) :: POS
  CHARACTER(LEN=*),                                INTENT(IN)    :: TERM_
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: N
  CHARACTER(LEN=1),                                INTENT(OUT)   :: S
  TYPE(HOOKS_T),                                   INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local Variables
  LOGICAL :: EX
  TYPE(INT_CONTAINER_T), POINTER :: HEAD=>NULL()
  TYPE(INT_CONTAINER_T), POINTER :: THIS=>NULL()
  LOGICAL :: SCALAR
  LOGICAL :: R
  LOGICAL :: END_
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: TERM
  CHARACTER(LEN=1) :: BUF
  INTEGER(KIND=JPIB_K) :: ALLOC_STAT
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CALL_READ_INTEGER = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_ALLOCATE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_READING_INTEGER_ARRAY = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_FLAG = 5_JPIB_K

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

  !...Initialization
  EX = .TRUE.
  R = .TRUE.
  TERM = TERM_
  END_ = .FALSE.

  ! Read Data From UNIT
  CNT = 0
  S = ' '
  main : DO

    ! if thre is an already readed character skip read operation
    IF ( R ) THEN
      POS = POS+1
      IF ( POS .GT. LEN(UNIT) ) THEN
        STAT = -1
      ELSE
        STAT = 0
        BUF = UNIT(POS:POS)
      ENDIF
    ELSE
      BUF = S
      STAT = 0
    ENDIF

    ! End of file exception
    IF ( STAT .EQ. -1 ) THEN
      R = .TRUE.
      EXIT main
    ENDIF

    ! Check if the token is an array
    IF ( BUF .EQ. '[' .AND. CNT .EQ. 0 ) THEN
      SCALAR = .FALSE.
      TERM = TERM//']'
      CNT = CNT + 1
      ALLOCATE( HEAD, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )
      THIS => HEAD
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER) READ_INTEGER( UNIT, POS, TERM, THIS%PAR, S, HOOKS )
      R = .FALSE.
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_INVALID_FLAG)
      ! IF (.NOT. EX ) EXIT main
      CYCLE main
    ENDIF

    ! Check if the token is not an array
    IF ( BUF .NE. '[' .AND. &
         BUF .NE. ' ' .AND. &
         CNT .EQ. 0 ) THEN
      SCALAR = .TRUE.
      CNT = CNT + 1
      ALLOCATE( HEAD, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )
      THIS => HEAD
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER) READ_INTEGER( UNIT, POS, TERM, THIS%PAR, S, HOOKS, BUF )
      R = .FALSE.
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_INVALID_FLAG)
      ! IF (.NOT. EX ) EXIT main
      CYCLE main
    ENDIF

    ! Read another token
    IF ( BUF .EQ. ',' ) THEN
      CNT = CNT + 1
      ! Coherence check
      IF ( CNT .GT. 1 .AND. SCALAR ) THEN
        DO J = 1, LEN_TRIM(TERM_)
          IF ( BUF .EQ. TERM_(J:J) ) THEN
            S = BUF
            CNT = CNT - 1
            EXIT main
          ENDIF
        ENDDO
        EX = .FALSE.
        CNT = CNT - 1
        PP_DEBUG_CRITICAL_THROW( ERRFLAG_ERROR_READING_INTEGER_ARRAY )
        EXIT main
      END IF
      IF ( END_ ) THEN
        DO J = 1, LEN_TRIM(TERM_)
          IF ( BUF .EQ. TERM_(J:J) ) THEN
            S = BUF
            CNT = CNT - 1
            EXIT main
          ENDIF
        ENDDO
        CYCLE main
      END IF
      ALLOCATE( THIS%NEXT, STAT=ALLOC_STAT, ERRMSG=ERRMSG )
      PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )
      THIS => THIS%NEXT
      PP_TRYCALL(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER) READ_INTEGER( UNIT, POS, TERM, THIS%PAR, S, HOOKS )
      R = .FALSE.
      PP_DEBUG_CRITICAL_COND_THROW( .NOT.EX, ERRFLAG_INVALID_FLAG)
      ! IF (.NOT. EX ) EXIT main
      CYCLE main
    ENDIF

    ! jump the close bracket
    IF ( BUF .EQ. ']' ) THEN
      R = .TRUE.
      END_ =.TRUE.
      CYCLE main
    ENDIF

    ! Exit condition
    DO J = 1, LEN_TRIM(TERM_)
      IF ( BUF .EQ. TERM_(J:J) ) THEN
        S = BUF
        EXIT main
      END IF
    ENDDO

    R = .TRUE.

  ENDDO main

  ! Error handling
  IF ( CNT .EQ. 0 .OR. .NOT.EX ) THEN
    S = ' '
    EX = .FALSE.
    THIS=>HEAD
    DO
      IF ( .NOT. ASSOCIATED(THIS) ) THEN
        EXIT
      ENDIF
      THIS => THIS%NEXT
      DEALLOCATE(HEAD, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      HEAD => THIS
    END DO
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_ERROR_READING_INTEGER_ARRAY )
  ENDIF

  ! Allocate output memory
  ALLOCATE( N(CNT), STAT=ALLOC_STAT, ERRMSG=ERRMSG )
  PP_DEBUG_CRITICAL_COND_THROW( ALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_ALLOCATE )

  ! Fill the integer array
  THIS=>HEAD
  CNT = 0
  DO

    CNT = CNT + 1

    IF ( .NOT. ASSOCIATED(THIS) ) THEN
      EXIT
    ENDIF

    N(CNT) = THIS%PAR

    THIS => THIS%NEXT

    DEALLOCATE(HEAD, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )

    HEAD => THIS

  ENDDO

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
    CASE(ERRFLAG_UNABLE_TO_CALL_READ_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to call read integer' )
    CASE(ERRFLAG_UNABLE_TO_ALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to allocate' )
      IF ( ALLOCATED( ERRMSG ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate' )
      IF ( ALLOCATED( ERRMSG ) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE( ERRMSG, STAT=DEALLOC_STAT )
      END IF
    CASE(ERRFLAG_ERROR_READING_INTEGER_ARRAY)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading integer array' )
    CASE(ERRFLAG_INVALID_FLAG)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid flag' )
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

END FUNCTION READ_INTEGER_ARRAY
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_INTEGER'
PP_THREAD_SAFE FUNCTION READ_INTEGER( UNIT, POS, TERM, N, S, HOOKS, F ) RESULT(RET)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: MULTIOM_API, ONLY: JPIB_K
  USE :: MULTIOM_API, ONLY: HOOKS_T

  ! Symbols imported by the preprocessor for debugging purposes
  PP_DEBUG_USE_VARS

  ! Symbols imported by the preprocessor for logging purposes
  PP_LOG_USE_VARS

  ! Symbols imported by the preprocessor for tracing purposes
  PP_TRACE_USE_VARS

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),           INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),       INTENT(INOUT) :: POS
  CHARACTER(LEN=*),           INTENT(IN)    :: TERM
  INTEGER(KIND=JPIB_K),       INTENT(OUT)   :: N
  CHARACTER(LEN=1),           INTENT(OUT)   :: S
  TYPE(HOOKS_T),              INTENT(INOUT) :: HOOKS
  CHARACTER(LEN=1), OPTIONAL, INTENT(IN)    :: F

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local Variables
  LOGICAL :: EX
  LOGICAL :: READ_
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: TMP
  CHARACTER(LEN=1) :: BUF
  INTEGER(KIND=JPIB_K) :: DEALLOC_STAT
  INTEGER(KIND=JPIB_K) :: READ_STAT
  CHARACTER(LEN=:), ALLOCATABLE :: ERRMSG

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_DEALLOCATE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_READING_INTEGER = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_INTEGER_NOT_ALLOCATED = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_CHARACTER = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_INVALID_STATE = 5_JPIB_K

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

  ! Initialization
  EX = .TRUE.
  TMP=''

  ! Read Data From UNIT
  CNT = 0
  READ_ = .TRUE.
  main : DO

    IF ( PRESENT(F) .AND. READ_ ) THEN

      BUF = F
      READ_ = .FALSE.
      STAT = 0

    ELSE

      POS = POS+1
      IF ( POS .GT. LEN(UNIT) ) THEN
        STAT = -1
      ELSE
        STAT = 0
        BUF = UNIT(POS:POS)
      END IF

    END IF

    ! End of file
    IF ( STAT .EQ. -1 ) THEN
      EXIT main
    END IF

    ! Check for special terminators
    DO J = 1, LEN(TERM)
      IF ( BUF .EQ. TERM(J:J) ) THEN
        S = BUF
        EXIT main
      END IF
    END DO

    ! Check for standard terminator
    IF ( BUF .EQ. ',' ) THEN
      S = BUF
      EXIT main
    END IF

    ! Update token
    IF ( BUF .EQ. ' ' .AND. CNT .EQ. 0 ) THEN
      CYCLE main
    ELSE
      CNT = CNT + 1
      TMP = TMP(:)//BUF
    END IF

  END DO main

  ! Error handling
  IF ( STAT .EQ. -2 ) THEN
    IF ( ALLOCATED(TMP) ) THEN
      DEALLOCATE(TMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
      PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
    END IF
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_INVALID_STATE )
  END IF

  ! Create output variable
  TMP = TRIM(ADJUSTL(TMP(:)))
  DO I = 1 , LEN(TMP)
    IF ( (IACHAR( TMP(I:I) ) .LT. 48 .OR. IACHAR( TMP(I:I) ) .GT. 57) .AND. IACHAR( TMP(I:I) ) .NE. 45 ) THEN
      EX = .FALSE.
      IF ( ALLOCATED(TMP) ) THEN
        DEALLOCATE(TMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
        PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
      END IF
      PP_DEBUG_CRITICAL_THROW(ERRFLAG_INVALID_CHARACTER)
    END IF
  END DO

  ! Read the number and free memory
  IF ( ALLOCATED(TMP) ) THEN
    ! WRITE(*,*) ' + Reading: ', TMP(:)
    READ(TMP(:),*, IOSTAT=READ_STAT) N
    PP_DEBUG_CRITICAL_COND_THROW( READ_STAT .NE. 0, ERRFLAG_ERROR_READING_INTEGER )
    DEALLOCATE(TMP, STAT=DEALLOC_STAT, ERRMSG=ERRMSG)
    PP_DEBUG_CRITICAL_COND_THROW( DEALLOC_STAT .NE. 0, ERRFLAG_UNABLE_TO_DEALLOCATE )
  ELSE
    PP_DEBUG_CRITICAL_THROW( ERRFLAG_ERROR_INTEGER_NOT_ALLOCATED )
  END IF

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
    CASE(ERRFLAG_UNABLE_TO_DEALLOCATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to deallocate' )
      IF ( ALLOCATED(ERRMSG) ) THEN
        PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error message: '//TRIM(ADJUSTL(ERRMSG)) )
        DEALLOCATE(ERRMSG, STAT=DEALLOC_STAT)
      END IF
    CASE(ERRFLAG_ERROR_READING_INTEGER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error reading integer' )
    CASE(ERRFLAG_ERROR_INTEGER_NOT_ALLOCATED)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error integer not allocated' )
    CASE(ERRFLAG_INVALID_CHARACTER)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid character' )
    CASE(ERRFLAG_INVALID_STATE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Invalid state' )
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

END FUNCTION READ_INTEGER
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE MULTIOM_TOOLS_CFG_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
