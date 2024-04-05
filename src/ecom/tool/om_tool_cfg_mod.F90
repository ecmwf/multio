MODULE OM_TOOL_CFG_MOD

  ! Symbols imported from the main library of the project.
  USE :: OM_API_MOD, ONLY: JPIB_K

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
  INTEGER(KIND=JPIB_K) :: NPROCS
  INTEGER(KIND=JPIB_K) :: NENTRIES
  CHARACTER(LEN=32)    :: OUTPUT_MANAGER_TYPE
  CHARACTER(LEN=4096)  :: INPUT_DIR
  CHARACTER(LEN=4096)  :: YAML_CONFIGURATION
  INTEGER(JPIB_K), DIMENSION(:), ALLOCATABLE :: PARAM_ID
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

FUNCTION MATCH( CFG, PARAM_ID, U_ID, STEP_ID, &
& PROC_ID, REPRES_ID, PREFIX_ID ) RESULT(EX)

  ! Symbols imported from the main library of the project.
  USE :: OM_API_MOD, ONLY: JPIB_K

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: CFG
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: PARAM_ID
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: U_ID
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: STEP_ID
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: PROC_ID
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: REPRES_ID
  INTEGER(KIND=JPIB_K),      INTENT(IN)    :: PREFIX_ID

  ! Function result
  LOGICAL :: EX

  EX = .TRUE.
  IF ( EX .AND. ALLOCATED( CFG%PARAM_ID ) ) THEN
    EX = ANY( PARAM_ID .EQ. CFG%PARAM_ID )
  ENDIF

  IF ( EX .AND. ALLOCATED( CFG%U_ID ) ) THEN
    EX = ANY( U_ID .EQ. CFG%U_ID )
  ENDIF

  IF ( EX .AND. ALLOCATED( CFG%STEP_ID ) ) THEN
    EX = ANY( STEP_ID .EQ. CFG%STEP_ID )
  ENDIF

  IF ( EX .AND. ALLOCATED( CFG%REPRES_ID ) ) THEN
    EX = ANY( REPRES_ID .EQ. CFG%REPRES_ID )
  ENDIF

  IF ( EX .AND. ALLOCATED( CFG%PREFIX_ID ) ) THEN
    EX = ANY( PREFIX_ID .EQ. CFG%PREFIX_ID )
  ENDIF

  RETURN

END FUNCTION MATCH


SUBROUTINE USAGE()

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

IMPLICIT NONE

  WRITE(OUTPUT_UNIT,*) ' + PROTOTYPE: ./outputManager.x [options]'
  WRITE(OUTPUT_UNIT,*) ' + --------------------------------------'
  WRITE(OUTPUT_UNIT,*) ' + '
  WRITE(OUTPUT_UNIT,*) ' + OPTIONS ARE: '
  WRITE(OUTPUT_UNIT,*) ' + -t || --output-manager-type :: type of output manager to use. (Default: "NOOP")'
  WRITE(OUTPUT_UNIT,*) ' +                                 |-> "NOOP"'
  WRITE(OUTPUT_UNIT,*) ' +                                 |-> "DUMP"'
  WRITE(OUTPUT_UNIT,*) ' +                                 |-> "GRIBX"'
  WRITE(OUTPUT_UNIT,*) ' +                                 |-> "GRIBX2MULTIO_BINARY"'
  WRITE(OUTPUT_UNIT,*) ' +                                 |-> "GRIBX2MULTIO_RAW"'
  WRITE(OUTPUT_UNIT,*) ' +                                 |-> "MULTIO_RAW"'
  WRITE(OUTPUT_UNIT,*) ' +                                 |-> "MULTIO_NO_ENC"'
  WRITE(OUTPUT_UNIT,*) ' + -i || --input-dir           :: input directory where all the binary reproducers are (Default: ".")'
  WRITE(OUTPUT_UNIT,*) ' + -y || --yaml-cfg            :: name of the "yaml" configuration file of the output manager (Default: "./output_manager_cfg.yaml")'
  WRITE(OUTPUT_UNIT,*) ' + -d || --dry-run             :: Just print to screen the "toc.bin" file. (Default: .FALSE.)'
  WRITE(OUTPUT_UNIT,*) ' + -v || --verbose             :: Enable verbosity in read operations. (Default: .FALSE.)'
  WRITE(OUTPUT_UNIT,*) ' + -b || --big-endian-read     :: Convert endian to big endian in read. This feature has to be used on macos (Default: .FALSE.)'
  WRITE(OUTPUT_UNIT,*) ' + -p || --param-id            :: list of the param-ids to be processed. Then grammar can be:'
  WRITE(OUTPUT_UNIT,*) ' +                                 |-> "*"         a star: all the param ids are processd (Defaul behaviour)'
  WRITE(OUTPUT_UNIT,*) ' +                                 |-> "1"         a number: only the paramid that match the number is processed'
  WRITE(OUTPUT_UNIT,*) ' +                                 |-> "[1,2,3,4]" a list of numbers: all the param ids that match the numbers in the list are processed'
  WRITE(OUTPUT_UNIT,*) ' + -l || --level               :: list of levels to be processed. The grammar is the same used for param ids'
  WRITE(OUTPUT_UNIT,*) ' + -s || --step                :: list of steps to be processed. The grammar is the same used for param ids'
  WRITE(OUTPUT_UNIT,*) ' + -r || --representation      :: [1=gridded, 2=spectral]'
  WRITE(OUTPUT_UNIT,*) ' + -n || --n-procs             :: number of processors'
  WRITE(OUTPUT_UNIT,*) ' + -q || --level-type          :: [1=model_level, 2=pressure_level, 3=vorticity_level, 4=theta_level, 5=surface_level, 6=wave_int, 7=wave_spec]'
  WRITE(OUTPUT_UNIT,*) ' + -h || --help || ?           :: print this message'
  WRITE(OUTPUT_UNIT,*) ' +'
  WRITE(OUTPUT_UNIT,*) ' + ATTENTION: the values NEED to be surrounded by double quotes to avoid unexpected behaviours!!!!'
  WRITE(OUTPUT_UNIT,*) ' + '
  WRITE(OUTPUT_UNIT,*) ' + EXAMPLE: output_manager_tool.SP  -t NOOP  -i "." -y "/ec/res4/scratch/mavm/develop_v7/raps/multio_yaml/output-manager-config.yaml"   -v -p [137]'
  WRITE(OUTPUT_UNIT,*) ' + '

  RETURN

END SUBROUTINE USAGE


SUBROUTINE INIT_COMMAND_LINE_OPTIONS( CFG )
IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: CFG

  ! Default initialisation of command line options
  CFG%DRYRUN = .FALSE.
  CFG%VERBOSE = .FALSE.
  CFG%BIG_ENDIAN_READ = .FALSE.
  CFG%OUTPUT_MANAGER_TYPE = 'NOOP'
  CFG%INPUT_DIR = '.'
  CFG%YAML_CONFIGURATION = './output_manager_cfg.yaml'
  CFG%NPROCS = 1

  IF ( ALLOCATED( CFG%PARAM_ID) ) THEN
    DEALLOCATE( CFG%PARAM_ID )
  ENDIF

  IF ( ALLOCATED( CFG%U_ID) ) THEN
    DEALLOCATE( CFG%U_ID )
  ENDIF

  IF ( ALLOCATED( CFG%STEP_ID) ) THEN
    DEALLOCATE( CFG%STEP_ID )
  ENDIF

  IF ( ALLOCATED( CFG%REPRES_ID ) ) THEN
    DEALLOCATE( CFG%REPRES_ID )
  ENDIF

  IF ( ALLOCATED( CFG%PREFIX_ID ) ) THEN
    DEALLOCATE( CFG%PREFIX_ID )
  ENDIF

  ! Exit point
  RETURN

END SUBROUTINE INIT_COMMAND_LINE_OPTIONS


SUBROUTINE FREE_COMMAND_LINE_OPTIONS( CFG )
IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: CFG

  ! Default initialisation of command line options
  CFG%DRYRUN = .FALSE.
  CFG%VERBOSE = .FALSE.
  CFG%BIG_ENDIAN_READ = .FALSE.
  CFG%OUTPUT_MANAGER_TYPE = REPEAT( ' ', 32 )
  CFG%INPUT_DIR = REPEAT( ' ', 4096 )
  CFG%YAML_CONFIGURATION = REPEAT( ' ', 4096 )
  CFG%NPROCS = 1

  IF ( ALLOCATED( CFG%PARAM_ID) ) THEN
    DEALLOCATE( CFG%PARAM_ID )
  ENDIF

  IF ( ALLOCATED( CFG%U_ID) ) THEN
    DEALLOCATE( CFG%U_ID )
  ENDIF

  IF ( ALLOCATED( CFG%STEP_ID) ) THEN
    DEALLOCATE( CFG%STEP_ID )
  ENDIF

  IF ( ALLOCATED( CFG%REPRES_ID ) ) THEN
    DEALLOCATE( CFG%REPRES_ID )
  ENDIF

  IF ( ALLOCATED( CFG%PREFIX_ID ) ) THEN
    DEALLOCATE( CFG%PREFIX_ID )
  ENDIF

  ! Exit point
  RETURN

END SUBROUTINE FREE_COMMAND_LINE_OPTIONS



SUBROUTINE PRINT_COMMAND_LINE_OPTIONS( CFG )

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: CFG

  ! Print values of the configuration
  WRITE(OUTPUT_UNIT,*) ' + DryRun                  :: ', CFG%DRYRUN
  WRITE(OUTPUT_UNIT,*) ' + Verbose                 :: ', CFG%VERBOSE
  WRITE(OUTPUT_UNIT,*) ' + Big endian read         :: ', CFG%BIG_ENDIAN_READ
  WRITE(OUTPUT_UNIT,*) ' + Output manager type     :: ', TRIM(ADJUSTL(CFG%OUTPUT_MANAGER_TYPE))
  WRITE(OUTPUT_UNIT,*) ' + Input directory         :: ', TRIM(ADJUSTL(CFG%INPUT_DIR))
  WRITE(OUTPUT_UNIT,*) ' + YAML configuration file :: ', TRIM(ADJUSTL(CFG%YAML_CONFIGURATION))

  IF ( ALLOCATED( CFG%PARAM_ID) ) THEN
    WRITE(OUTPUT_UNIT,*)  ' + param ids               :: ', CFG%PARAM_ID
  ELSE
    WRITE(OUTPUT_UNIT,*)  ' + param ids               :: all'
  ENDIF

  IF ( ALLOCATED( CFG%U_ID) ) THEN
    WRITE(OUTPUT_UNIT,*)  ' + levels ids              :: ', CFG%U_ID
  ELSE
    WRITE(OUTPUT_UNIT,*)  ' + levels ids              :: all'
  ENDIF

  IF ( ALLOCATED( CFG%STEP_ID) ) THEN
    WRITE(OUTPUT_UNIT,*)  ' + steps ids               :: ', CFG%STEP_ID
  ELSE
    WRITE(OUTPUT_UNIT,*)  ' + steps ids               :: all'
  ENDIF

  IF ( ALLOCATED( CFG%REPRES_ID) ) THEN
    WRITE(OUTPUT_UNIT,*)  ' + representation          :: ', CFG%REPRES_ID
  ELSE
    WRITE(OUTPUT_UNIT,*)  ' + representation          :: all'
  ENDIF

  IF ( ALLOCATED( CFG%PREFIX_ID) ) THEN
    WRITE(OUTPUT_UNIT,*)  ' + prefix                  :: ', CFG%PREFIX_ID
  ELSE
    WRITE(OUTPUT_UNIT,*)  ' + prefix                  :: all'
  ENDIF

  ! Exit point
  RETURN

END SUBROUTINE PRINT_COMMAND_LINE_OPTIONS


SUBROUTINE PARSE_COMMAND_LINE_OPTIONS( CFG )

  ! Symbols imported from the main library of the project.
  USE :: OM_API_MOD, ONLY: JPIM_K
  USE :: OM_API_MOD, ONLY: JPIB_K

IMPLICIT NONE

  ! Dummy arguments
  TYPE(COMMAND_LINE_ARGS_T), INTENT(INOUT) :: CFG

  ! Local variables
  CHARACTER(LEN=4096) :: TMP
  INTEGER(KIND=JPIB_K) :: NUM_ARGS
  INTEGER(KIND=JPIM_K) :: I
  INTEGER(KIND=JPIB_K) :: POS
  INTEGER(KIND=JPIB_K) :: N
  CHARACTER(LEN=1024) :: COMMAND_LINE_ARG
  CHARACTER(LEN=1024) :: COMMAND_LINE_SWITCH
  CHARACTER(LEN=1024) :: TMP_STR
  CHARACTER(LEN=1) :: S
  LOGICAL :: EX

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
    IF ( I .GT. NUM_ARGS ) EXIT CommandLineArgsLoop

    !> Read the i-th command line argument
    CALL GET_COMMAND_ARGUMENT( I, COMMAND_LINE_ARG )

    !> Extract the switch
    COMMAND_LINE_SWITCH = TRIM(ADJUSTL(COMMAND_LINE_ARG))

    !> Select the correct behaviour
    SELECT CASE(COMMAND_LINE_SWITCH)

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-t', '--output-manager-type' )
      I = I + 1
      CFG%OUTPUT_MANAGER_TYPE = REPEAT(' ',32)
      TMP = REPEAT( ' ', 4096 )
      CALL GET_COMMAND_ARGUMENT( I, TMP )
      CFG%OUTPUT_MANAGER_TYPE = TRIM(TMP)

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-i', '--input-dir' )
      I = I + 1
      CFG%INPUT_DIR = REPEAT(' ',4096)
      CALL GET_COMMAND_ARGUMENT( I, CFG%INPUT_DIR )

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-y', '--yaml-cfg' )
      I = I + 1
      CFG%YAML_CONFIGURATION = REPEAT(' ',4096)
      CALL GET_COMMAND_ARGUMENT( I, CFG%YAML_CONFIGURATION )

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
      CALL GET_COMMAND_ARGUMENT( I, TMP )
      IF ( TRIM(ADJUSTL(TMP)) .EQ. '*' ) THEN
        WRITE(*,*) 'parameters = all'
      ELSE
        POS = 0
        EX = READ_INTEGER_ARRAY( TRIM(TMP), POS, '-', CFG%PARAM_ID, S )
      ENDIF

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-l', '--level' )
      I = I + 1
      CALL GET_COMMAND_ARGUMENT( I, TMP )
      IF ( TRIM(ADJUSTL(TMP)) .EQ. '*' ) THEN
        WRITE(*,*) 'levels = all'
      ELSE
        POS = 0
        EX = READ_INTEGER_ARRAY( TRIM(TMP), POS, '-', CFG%U_ID, S )
      ENDIF

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-s', '--step' )
      I = I + 1
      CALL GET_COMMAND_ARGUMENT( I, TMP )
      IF ( TRIM(ADJUSTL(TMP)) .EQ. '*' ) THEN
        WRITE(*,*) 'steps = all'
      ELSE
        POS = 0
        EX = READ_INTEGER_ARRAY( TRIM(TMP), POS, '-', CFG%STEP_ID, S )
      ENDIF

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-r', '--representation' )
      I = I + 1
      CALL GET_COMMAND_ARGUMENT( I, TMP )
      IF ( TRIM(ADJUSTL(TMP)) .EQ. '*' ) THEN
        WRITE(*,*) 'steps = all'
      ELSE
        POS = 0
        EX = READ_INTEGER_ARRAY( TRIM(TMP), POS, '-', CFG%REPRES_ID, S )
      ENDIF

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-n', '--n-procs' )
      I = I + 1
      CALL GET_COMMAND_ARGUMENT( I, TMP )
      IF ( TRIM(ADJUSTL(TMP)) .EQ. '*' ) THEN
        WRITE(*,*) 'steps = all'
      ELSE
        POS = 0
        EX = READ_INTEGER( TRIM(TMP), POS, '-', CFG%NPROCS, S )
      ENDIF

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-q', '--level-type' )
      I = I + 1
      CALL GET_COMMAND_ARGUMENT( I, TMP )
      IF ( TRIM(ADJUSTL(TMP)) .EQ. '*' ) THEN
        WRITE(*,*) 'steps = all'
      ELSE
        POS = 0
        EX = READ_INTEGER_ARRAY( TRIM(TMP), POS, '-', CFG%PREFIX_ID, S )
      ENDIF

    ! ----------------------------------------------------------------------------------------------
    CASE ( '-h', '--help', '?' )
      CALL USAGE()
      STOP

    ! ----------------------------------------------------------------------------------------------
    CASE DEFAULT
      CALL USAGE()
      WRITE(*,*) 'ERROR :: Unknown command line argument ||', COMMAND_LINE_ARG, '||'
      STOP

    END SELECT

  ENDDO CommandLineArgsLoop

END SUBROUTINE PARSE_COMMAND_LINE_OPTIONS


FUNCTION READ_INTEGER_ARRAY( UNIT, POS, TERM_, N, S )  RESULT(EX)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),                                INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),                            INTENT(INOUT) :: POS
  CHARACTER(LEN=*),                                INTENT(IN)    :: TERM_
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: N
  CHARACTER(LEN=1),                                INTENT(OUT)   :: S

  ! Function result
  LOGICAL :: EX

  ! Local Variables
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
      ALLOCATE( HEAD )
      THIS => HEAD
      EX = READ_INTEGER( UNIT, POS, TERM, THIS%PAR, S )
      R = .FALSE.
      IF (.NOT. EX ) EXIT main
      CYCLE main
    ENDIF

    ! Check if the token is not an array
    IF ( BUF .NE. '[' .AND. &
         BUF .NE. ' ' .AND. &
         CNT .EQ. 0 ) THEN
      SCALAR = .TRUE.
      CNT = CNT + 1
      ALLOCATE( HEAD )
      THIS => HEAD
      EX = READ_INTEGER( UNIT, POS, TERM, THIS%PAR, S, BUF )
      R = .FALSE.
      IF (.NOT. EX ) EXIT main
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
      ALLOCATE( THIS%NEXT )
      THIS => THIS%NEXT
      EX = READ_INTEGER( UNIT, POS, TERM, THIS%PAR, S )
      R = .FALSE.
      IF (.NOT. EX ) EXIT main
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
      IF ( .NOT. ASSOCIATED(THIS) ) EXIT
      THIS => THIS%NEXT
      DEALLOCATE(HEAD)
      HEAD => THIS
    END DO
    RETURN
  ENDIF

  ! Allocate output memory
  ALLOCATE( N(CNT) )

  ! Fill the integer array
  THIS=>HEAD
  CNT = 0
  DO

    CNT = CNT + 1

    IF ( .NOT. ASSOCIATED(THIS) ) EXIT

    N(CNT) = THIS%PAR

    THIS => THIS%NEXT

    DEALLOCATE(HEAD)

    HEAD => THIS

  ENDDO

END FUNCTION READ_INTEGER_ARRAY


FUNCTION READ_INTEGER( UNIT, POS, TERM, N, S, F ) RESULT(EX)

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_CORE_MOD, ONLY: JPIB_K

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*),           INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K),       INTENT(INOUT) :: POS
  CHARACTER(LEN=*),           INTENT(IN)    :: TERM
  INTEGER(KIND=JPIB_K),       INTENT(OUT)   :: N
  CHARACTER(LEN=1),           INTENT(OUT)   :: S
  CHARACTER(LEN=1), OPTIONAL, INTENT(IN)    :: F

  ! Function result
  LOGICAL :: EX

  ! Local Variables
  LOGICAL :: READ_
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: STAT
  CHARACTER(LEN=:), ALLOCATABLE :: TMP
  CHARACTER(LEN=1) :: BUF

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
      DEALLOCATE(TMP)
    END IF
    EX = .FALSE.
    RETURN
  END IF

  ! Create output variable
  TMP = TRIM(ADJUSTL(TMP(:)))
  DO I = 1 , LEN(TMP)
    IF ( (IACHAR( TMP(I:I) ) .LT. 48 .OR. IACHAR( TMP(I:I) ) .GT. 57) .AND. IACHAR( TMP(I:I) ) .NE. 45 ) THEN
      EX = .FALSE.
      IF ( ALLOCATED(TMP) ) THEN
        DEALLOCATE(TMP)
      END IF
      RETURN
    END IF

  END DO

  ! Read the number and free memory
  IF ( ALLOCATED(TMP) ) THEN
    ! WRITE(*,*) ' + Reading: ', TMP(:)
    READ(TMP(:),*) N
    DEALLOCATE(TMP)
  ELSE
    WRITE(ERROR_UNIT,*) 'ERRROR parsing command line arguments ', __FILE__, __LINE__
    STOP
  END IF

  ! Exit point
  RETURN

END FUNCTION READ_INTEGER

END MODULE OM_TOOL_CFG_MOD
