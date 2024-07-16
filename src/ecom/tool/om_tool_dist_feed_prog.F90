PROGRAM OM_TOOL_DISTRIBUTE_PROG

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32

  ! Symbols imported from other modules within the project.
  USE :: OM_TOOL_UTILS_MOD, ONLY: SET_HOOK_VERBOSITY
  USE :: OM_TOOL_UTILS_MOD, ONLY: OM_ABORT
  USE :: OM_TOOL_UTILS_MOD, ONLY: DR_HOOK_DEFAULT8
  USE :: OM_TOOL_UTILS_MOD, ONLY: PREPARE_OUTPUT_MANAGER_EC
  USE :: OM_TOOL_UTILS_MOD, ONLY: READ_ATM_MESSAGE
  USE :: OM_TOOL_UTILS_MOD, ONLY: READ_WAM_MESSAGE
  USE :: OM_TOOL_UTILS_MOD, ONLY: READ_VAL_SP
  USE :: OM_TOOL_UTILS_MOD, ONLY: READ_VAL_DP

  USE :: OM_TOOL_CFG_MOD, ONLY: COMMAND_LINE_ARGS_T
  USE :: OM_TOOL_CFG_MOD, ONLY: INIT_COMMAND_LINE_OPTIONS
  USE :: OM_TOOL_CFG_MOD, ONLY: PARSE_COMMAND_LINE_OPTIONS
  USE :: OM_TOOL_CFG_MOD, ONLY: PRINT_COMMAND_LINE_OPTIONS
  USE :: OM_TOOL_CFG_MOD, ONLY: FREE_COMMAND_LINE_OPTIONS
  USE :: OM_TOOL_CFG_MOD, ONLY: MATCH
  USE :: OM_DISTRIBUTOR_MOD, ONLY: DISTRIBUTOR_T

  ! Symbols imported from the main library of the project.
  USE :: OM_API_MOD, ONLY: IPREFIX2ILEVTYPE
  USE :: OM_API_MOD, ONLY: ILEVTYPE2CLEVTYPE
  USE :: OM_API_MOD, ONLY: IREPRES2CREPRES
  USE :: OM_API_MOD, ONLY: JPIB_K
  USE :: OM_API_MOD, ONLY: VALUES_SP_E
  USE :: OM_API_MOD, ONLY: VALUES_DP_E
  USE :: OM_API_MOD, ONLY: OM_ATM_MSG_T
  USE :: OM_API_MOD, ONLY: OM_WAM_MSG_T
  USE :: OM_API_MOD, ONLY: MODEL_PAR_T
  USE :: OM_API_MOD, ONLY: PROC_TOPO_T
  USE :: OM_API_MOD, ONLY: OUTPUT_MANAGER_BASE_A
  USE :: OM_API_MOD, ONLY: MAKE_OUTPUT_MANAGER
  USE :: OM_API_MOD, ONLY: DESTROY_OUTPUT_MANAGER
  USE :: OM_API_MOD, ONLY: CHECK_REAL_KIND
  USE :: OM_API_MOD, ONLY: OM_SET_ABORT_PROCEDURE
  USE :: OM_API_MOD, ONLY: OM_SET_ERROR_UNIT
  USE :: OM_API_MOD, ONLY: OM_SET_DR_HOOK
  USE :: OM_API_MOD, ONLY: MSG_PRINT_ATM
  USE :: OM_API_MOD, ONLY: MSG_PRINT_WAM
  USE :: OM_API_MOD, ONLY: TOC_CONTAINER_T
  USE :: OM_API_MOD, ONLY: TOC_CREATE_NAME
  USE :: OM_API_MOD, ONLY: TOC_ENTRY_BASE_T
  USE :: OM_API_MOD, ONLY: TOC_SIM_INIT_T
  USE :: OM_API_MOD, ONLY: TOC_ATM_FIELD_T
  USE :: OM_API_MOD, ONLY: TOC_WAM_FIELD_T
  USE :: OM_API_MOD, ONLY: TOC_FLUSH_STEP_T
  USE :: OM_API_MOD, ONLY: TOC_FLUSH_STEP_RST_T
  USE :: OM_API_MOD, ONLY: TOC_FLUSH_LAST_STEP_T
  USE :: OM_API_MOD, ONLY: TOC_SIM_END_T
  USE :: OM_API_MOD, ONLY: TOC_READ_ALL
  USE :: OM_API_MOD, ONLY: TOC_READ
  USE :: OM_API_MOD, ONLY: TOC_FREE

  USE :: OM_API_MOD, ONLY: KEY_T
  USE :: OM_API_MOD, ONLY: MAP_T
  USE :: OM_API_MOD, ONLY: MAP_INIT
  USE :: OM_API_MOD, ONLY: MAP_INSERT
  USE :: OM_API_MOD, ONLY: MAP_GET_SORTED_KEYS_INT
  USE :: OM_API_MOD, ONLY: MAP_FREE

IMPLICIT NONE

  !
  ! Local variables
  TYPE(COMMAND_LINE_ARGS_T) :: CFG
  CLASS(OUTPUT_MANAGER_BASE_A), POINTER :: YLOM
  TYPE(PROC_TOPO_T) :: YLTOPO
  TYPE(MODEL_PAR_T) :: YLOMP
  INTEGER(KIND=JPIB_K) :: TOCID
  INTEGER(KIND=JPIB_K) :: CNT
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: FFFF
  INTEGER(KIND=JPIB_K) :: OFFSET
  INTEGER(KIND=JPIB_K) :: DOFFSET
  INTEGER(KIND=JPIB_K) :: CFGUNIT
  TYPE(OM_ATM_MSG_T) :: YLATMMSG
  TYPE(OM_WAM_MSG_T) :: YLWAMMSG
  CHARACTER(LEN=2048) :: TOCFNAME
  CHARACTER(LEN=2048) :: MSGFNAME
  CHARACTER(LEN=2048) :: FULLDIR
  CHARACTER(LEN=2048) :: LOGNAME
  INTEGER(KIND=INT32), POINTER :: V
  CLASS(*), POINTER :: VALUE
  LOGICAL :: EX
  LOGICAL :: CHECK
  TYPE(TOC_CONTAINER_T), DIMENSION(:), ALLOCATABLE :: TOC
  REAL(KIND=REAL32), POINTER, DIMENSION(:) :: VALUES_SP
  REAL(KIND=REAL64), POINTER, DIMENSION(:) :: VALUES_DP

  INTEGER(KIND=INT32), DIMENSION(:), ALLOCATABLE :: PIDS
  TYPE(DISTRIBUTOR_T) :: DISTRIBUTOR
  LOGICAL :: IS_NEW

  CALL MP_INIT()

  ! Set the default values for the command line options
  CALL INIT_COMMAND_LINE_OPTIONS( CFG )

  ! Parse command line options
  CALL PARSE_COMMAND_LINE_OPTIONS( CFG )

  ! Log teh configuration of hte tool
  CALL PRINT_COMMAND_LINE_OPTIONS( CFG )


  ! Set the verbosity of the hooks at begin and end of every function
  CALL SET_HOOK_VERBOSITY( .FALSE. )

  ! Configure the DR_HOOK handlers
  CALL OM_SET_DR_HOOK( .TRUE., DR_HOOK_DEFAULT8 )

  ! Configure the abort procedure to be used inside the output manager
  CALL OM_SET_ABORT_PROCEDURE( OM_ABORT )

  ! Configure the error unit to be used by the output manager
  CALL OM_SET_ERROR_UNIT( ERROR_UNIT )

  ! Extract topology informations and all model parameters from the IOserver data structure
  CALL PREPARE_OUTPUT_MANAGER_EC( TRIM(CFG%INPUT_DIR), CFG%PROC_IDX, YLTOPO, YLOMP, CFG%BIG_ENDIAN_READ, CFG%VERBOSE )

  ! Consttruct an outu manager
  IF ( .NOT.CFG%DRYRUN ) THEN
    CALL MAKE_OUTPUT_MANAGER( CFG%OUTPUT_MANAGER_TYPE, YLTOPO, YLOMP, CFG%YAML_CONFIGURATION, YLOM )
  ENDIF

  ! Read and merge all the toc files
  IF ( CFG%NPROCS .GT. 0 ) THEN
    TOCFNAME = REPEAT( ' ', 2048 )
    WRITE(FULLDIR,'(A,A,I6.6,A)') TRIM(ADJUSTL(CFG%INPUT_DIR)), '/io_serv.', CFG%NPROCS, '.d'
    CALL TOC_CREATE_NAME( TRIM(FULLDIR), CFG%NPROCS, TOCFNAME )
    CALL TOC_READ( TOCFNAME, TOC, CFG%BIG_ENDIAN_READ, CFG%VERBOSE )
  ELSE
    CALL TOC_READ_ALL( TRIM(CFG%INPUT_DIR), TOC, YLOMP%SIM_%NPROC_IO, CFG%BIG_ENDIAN_READ, CFG%VERBOSE )
  ENDIF


  WRITE(LOGNAME,'(A,I6.6,A)') 'distributor.', MP_ME(), '.log'
  OPEN(NEWUNIT=FFFF, FILE=LOGNAME, STATUS='REPLACE', ACTION='WRITE', FORM='FORMATTED')
  WRITE(FFFF,'(A150)') REPEAT('-',150)
  WRITE(FFFF,'(A20,A10,A10,A10,A10,A10,A10,A4,A20,A10)') ' message type', 'paramId', 'level/uid', 'step', 'proc', 'repres', 'prefix', '    ', 'crepres', 'clevtype'

  CALL DISTRIBUTOR%INIT()

  ! Loop over the toc entries
  CNT = 0
  FeederLoop: DO TOCID = 1, SIZE(TOC)

    SELECT TYPE ( A => TOC(TOCID)%ENTRY_ )

    ! ==============================================================================================
    CLASS IS ( TOC_SIM_INIT_T )
      IF ( CFG%DRYRUN ) THEN
        WRITE(FFFF,'(A150)') REPEAT('-',150)
        WRITE(FFFF,*) 'Begin of simulation'
      ENDIF
      CYCLE FeederLoop

    ! ==============================================================================================
    CLASS IS ( TOC_ATM_FIELD_T )
      IF ( MATCH( CFG, A%PARAM_ID_, A%U_ID_, A%STEP_ID_, A%PROC_ID_, A%REPRES_ID_, A%PREFIX_ID_ ) ) THEN
        CHECK = DISTRIBUTOR%ACCESS_OR_CREATE( CNT, A%PARAM_ID_, A%REPRES_ID_, IPREFIX2ILEVTYPE(A%PREFIX_ID_,A%PARAM_ID_,A%U_ID_,A%REPRES_ID_), A%U_ID_, IS_NEW )
        IF ( IS_NEW ) THEN
          CNT = CNT + 1
        ENDIF
        IF ( .NOT.CFG%DRYRUN ) THEN
          IF ( CHECK ) THEN
            SELECT CASE ( A%VAL_TYPE_ )
            CASE ( VALUES_SP_E )

              ! Read the next message to be processed
              CALL READ_ATM_MESSAGE( TRIM(CFG%INPUT_DIR), A%PROC_ID_, A%MSG_ID_, A%MSG_ADDR_, &
&                  YLATMMSG, CFG%BIG_ENDIAN_READ, CFG%VERBOSE )

              ! Read the values
              CALL READ_VAL_SP( TRIM(CFG%INPUT_DIR), A%PROC_ID_, A%MSG_ID_, A%VAL_ADDR_, &
&                    A%VAL_LB_, A%VAL_UB_, VALUES_SP, CFG%BIG_ENDIAN_READ, CFG%VERBOSE )

              ! Write data to the output manager
              CALL YLOM%WRITE_ATM_SP( YLATMMSG, VALUES_SP )

            CASE ( VALUES_DP_E )

              ! Read the next message to be processed
              CALL READ_ATM_MESSAGE( TRIM(CFG%INPUT_DIR), A%PROC_ID_, A%MSG_ID_, A%MSG_ADDR_, YLATMMSG, CFG%BIG_ENDIAN_READ, CFG%VERBOSE )

              ! Rdad the values
              CALL READ_VAL_DP( TRIM(CFG%INPUT_DIR), A%PROC_ID_, A%MSG_ID_, A%VAL_ADDR_, &
&                    A%VAL_LB_, A%VAL_UB_, VALUES_DP, CFG%BIG_ENDIAN_READ, CFG%VERBOSE )

              ! Write data to the output manager
              CALL YLOM%WRITE_ATM_DP( YLATMMSG, VALUES_DP )

            CASE DEFAULT
              WRITE(FFFF,*) 'Unknown val type'
              STOP
            END SELECT
          ENDIF
        ELSE
          WRITE(FFFF,'(A,I10,I10,I10,I10,I10,I10,A4,A20,A10,I3,L,L)') 'Atm. message:       ', A%PARAM_ID_, A%U_ID_, A%STEP_ID_, A%PROC_ID_, A%REPRES_ID_, A%PREFIX_ID_, '    ', IREPRES2CREPRES(A%REPRES_ID_), ILEVTYPE2CLEVTYPE(IPREFIX2ILEVTYPE(A%PREFIX_ID_,A%PARAM_ID_,A%U_ID_,A%REPRES_ID_)), MP_ME(), IS_NEW, CHECK
        ENDIF
      ENDIF
    ! ==============================================================================================
    CLASS IS ( TOC_WAM_FIELD_T )
      IF ( MATCH( CFG, A%PARAM_ID_, A%U_ID_, A%STEP_ID_, A%PROC_ID_, A%REPRES_ID_, A%PREFIX_ID_ ) ) THEN
        CHECK = DISTRIBUTOR%ACCESS_OR_CREATE( CNT, A%PARAM_ID_, A%REPRES_ID_, IPREFIX2ILEVTYPE(A%PREFIX_ID_,A%PARAM_ID_,A%U_ID_,A%REPRES_ID_), A%U_ID_, IS_NEW )
        IF ( IS_NEW ) THEN
          CNT = CNT + 1
        ENDIF
        IF ( .NOT.CFG%DRYRUN ) THEN
          IF ( CHECK ) THEN
            ! Check that the message file exist
            SELECT CASE ( A%VAL_TYPE_ )
            CASE ( VALUES_SP_E )

              ! Read the next message to be processed
              CALL READ_WAM_MESSAGE( TRIM(CFG%INPUT_DIR), A%PROC_ID_, A%MSG_ID_, A%MSG_ADDR_, YLWAMMSG, CFG%BIG_ENDIAN_READ, CFG%VERBOSE )

              ! Read the values
              CALL READ_VAL_SP( TRIM(CFG%INPUT_DIR), A%PROC_ID_, A%MSG_ID_, A%VAL_ADDR_, &
&                  A%VAL_LB_, A%VAL_UB_, VALUES_SP, CFG%BIG_ENDIAN_READ, CFG%VERBOSE )

              ! Write data to the output manager
              CALL YLOM%WRITE_WAM_SP( YLWAMMSG, VALUES_SP )

            CASE ( VALUES_DP_E )

              ! Read the next message to be processed
              CALL READ_WAM_MESSAGE( TRIM(CFG%INPUT_DIR), A%PROC_ID_, A%MSG_ID_, A%MSG_ADDR_, YLWAMMSG, CFG%BIG_ENDIAN_READ, CFG%VERBOSE )

              ! Rdad the values
              CALL READ_VAL_DP( TRIM(CFG%INPUT_DIR), A%PROC_ID_, A%MSG_ID_, A%VAL_ADDR_, &
&                  A%VAL_LB_, A%VAL_UB_, VALUES_DP, CFG%BIG_ENDIAN_READ, CFG%VERBOSE )

              ! Write data to the output manager
              CALL YLOM%WRITE_WAM_DP( YLWAMMSG, VALUES_DP )

            CASE DEFAULT
              WRITE(FFFF,*) 'Unknown val type'
              STOP
            END SELECT
          ENDIF
        ELSE
          WRITE(FFFF,'(A,I10,I10,I10,I10,I10,I10,A4,A20,A10,I3,L,L)') 'Wam. message:       ', A%PARAM_ID_, A%U_ID_, A%STEP_ID_, A%PROC_ID_, A%REPRES_ID_, A%PREFIX_ID_, '    ', IREPRES2CREPRES(A%REPRES_ID_), ILEVTYPE2CLEVTYPE(IPREFIX2ILEVTYPE(A%PREFIX_ID_,A%PARAM_ID_,A%U_ID_,A%REPRES_ID_)), MP_ME(), IS_NEW, CHECK
        ENDIF
      ENDIF

    ! ==============================================================================================
    CLASS IS ( TOC_FLUSH_STEP_T )
      IF ( .NOT.CFG%DRYRUN ) THEN
        CALL YLOM%FLUSH_STEP( A%STEP_ )
      ELSE
        WRITE(FFFF,'(A150)') REPEAT('-',150)
        WRITE(FFFF,'(A,I10)') 'Flush:              ', A%STEP_
      ENDIF

    ! ==============================================================================================
    CLASS IS ( TOC_FLUSH_STEP_RST_T )
      IF ( .NOT.CFG%DRYRUN ) THEN
        CALL YLOM%FLUSH_STEP_AND_TRIGGER_RESTART( A%STEP_ )
      ELSE
        WRITE(FFFF,'(A150)') REPEAT('-',150)
        WRITE(FFFF,'(A,I10)') 'Flush and restart:  ', A%STEP_
      ENDIF

    ! ==============================================================================================
    CLASS IS ( TOC_FLUSH_LAST_STEP_T )
      IF ( .NOT.CFG%DRYRUN ) THEN
        CALL YLOM%FLUSH_LAST_STEP( A%STEP_ )
      ELSE
        WRITE(FFFF,'(A150)') REPEAT('-',150)
        WRITE(FFFF,'(A,I10)') 'Flush last step:    ', A%STEP_
      ENDIF

    ! ==============================================================================================
    CLASS IS ( TOC_SIM_END_T )
      IF ( .NOT.CFG%DRYRUN ) THEN
        ! WRITE(*,*) 'I AM THE DESTROYER'
        CALL DESTROY_OUTPUT_MANAGER( YLOM )
      ELSE
        WRITE(FFFF,'(A150)') REPEAT('-',150)
        WRITE(FFFF,*) 'End of simulation'
      ENDIF
      EXIT FeederLoop

    ! ==============================================================================================
    CLASS DEFAULT
      WRITE(FFFF,*) 'ERROR: Enknown entry in toc file'
      STOP

    END SELECT

  ENDDO FeederLoop

  ! Free memory
  CLOSE(FFFF)
  CALL FREE_COMMAND_LINE_OPTIONS( CFG )
  CALL TOC_FREE( TOC )
  CALL DISTRIBUTOR%FREE()

  CALL MP_FREE()

CONTAINS

FUNCTION MP_ME() RESULT(M)
  USE :: OM_DISTRIBUTOR_MOD, ONLY: ME
IMPLICIT NONE
  INTEGER :: M
  M = ME
  RETURN
END FUNCTION MP_ME

SUBROUTINE MP_INIT( )
  USE :: OM_DISTRIBUTOR_MOD, ONLY: NPROCS
  USE :: OM_DISTRIBUTOR_MOD, ONLY: ME
  USE :: MPI, ONLY: MPI_INIT
  USE :: MPI, ONLY: MPI_COMM_WORLD
  USE :: MPI, ONLY: MPI_COMM_SIZE
  USE :: MPI, ONLY: MPI_COMM_RANK
IMPLICIT NONE
  INTEGER :: IERR
  CALL MPI_INIT( IERR )
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, NPROCS, IERR)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, ME, IERR)
  RETURN
END SUBROUTINE MP_INIT


SUBROUTINE MP_FREE( )
  USE :: MPI, ONLY: MPI_FINALIZE
IMPLICIT NONE
  INTEGER :: IERR
  CALL MPI_FINALIZE( IERR )
  RETURN
END SUBROUTINE MP_FREE



END PROGRAM OM_TOOL_DISTRIBUTE_PROG
