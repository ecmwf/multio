PROGRAM OM_TOOL_PROG

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
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: OFFSET
  INTEGER(KIND=JPIB_K) :: DOFFSET
  INTEGER(KIND=JPIB_K) :: CFGUNIT
  TYPE(OM_ATM_MSG_T) :: YLATMMSG
  TYPE(OM_WAM_MSG_T) :: YLWAMMSG
  CHARACTER(LEN=2048) :: TOCFNAME
  CHARACTER(LEN=2048) :: MSGFNAME
  CHARACTER(LEN=2048) :: FULLDIR
  INTEGER(KIND=INT32), POINTER :: V
  CLASS(*), POINTER :: VALUE
  LOGICAL :: EX
  TYPE(TOC_CONTAINER_T), DIMENSION(:), ALLOCATABLE :: TOC
  REAL(KIND=REAL32), POINTER, DIMENSION(:) :: VALUES_SP
  REAL(KIND=REAL64), POINTER, DIMENSION(:) :: VALUES_DP

  INTEGER(KIND=INT32), DIMENSION(:), ALLOCATABLE :: PIDS
  TYPE(MAP_T), DIMENSION(2,11) :: MAP
  TYPE(KEY_T) :: KEY

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

  ! Read and merge all the toc files
  IF ( CFG%NPROCS .GT. 0 ) THEN
    TOCFNAME = REPEAT( ' ', 2048 )
    WRITE(FULLDIR,'(A,A,I6.6,A)') TRIM(ADJUSTL(CFG%INPUT_DIR)), '/io_serv.', CFG%NPROCS, '.d'
    CALL TOC_CREATE_NAME( TRIM(FULLDIR), CFG%NPROCS, TOCFNAME )
    CALL TOC_READ( TOCFNAME, TOC, CFG%BIG_ENDIAN_READ, CFG%VERBOSE )
  ELSE
    CALL TOC_READ_ALL( TRIM(CFG%INPUT_DIR), TOC, YLOMP%SIM_%NPROC_IO, CFG%BIG_ENDIAN_READ, CFG%VERBOSE )
  ENDIF

  WRITE(OUTPUT_UNIT,'(A150)') REPEAT('-',150)
  WRITE(OUTPUT_UNIT,'(A20,A10,A10,A10,A10,A10,A10,A4,A20,A10)') ' message type', 'paramId', 'level/uid', 'step', 'proc', 'repres', 'prefix', '    ', 'crepres', 'clevtype'

  DO I = 1, 2
    DO J = 1, 11
      CALL MAP_INIT( MAP(I,J) )
    ENDDO
  ENDDO

  ! Loop over the toc entries
  FeederLoop: DO TOCID = 1, SIZE(TOC)

    SELECT TYPE ( A => TOC(TOCID)%ENTRY_ )

    ! ==============================================================================================
    CLASS IS ( TOC_SIM_INIT_T )
      WRITE(OUTPUT_UNIT,'(A150)') REPEAT('-',150)
      WRITE(OUTPUT_UNIT,*) 'Begin of simulation'
      CYCLE FeederLoop

    ! ==============================================================================================
    CLASS IS ( TOC_ATM_FIELD_T )
      KEY%K = A%PARAM_ID_
      NULLIFY(V)
      ALLOCATE(V)
      V = A%PARAM_ID_
      VALUE => V
      CALL MAP_INSERT( MAP(A%REPRES_ID_, IPREFIX2ILEVTYPE(A%PREFIX_ID_,A%PARAM_ID_)), KEY, VALUE )

    ! ==============================================================================================
    CLASS IS ( TOC_WAM_FIELD_T )
      KEY%K = A%PARAM_ID_
      NULLIFY(V)
      ALLOCATE(V)
      V = A%PARAM_ID_
      VALUE => V
      CALL MAP_INSERT( MAP(A%REPRES_ID_, IPREFIX2ILEVTYPE(A%PREFIX_ID_,A%PARAM_ID_)), KEY, VALUE )

    ! ==============================================================================================
    CLASS IS ( TOC_FLUSH_STEP_T )
      WRITE(OUTPUT_UNIT,'(A150)') REPEAT('-',150)
      WRITE(OUTPUT_UNIT,'(A,I10)') 'Flush:              ', A%STEP_

    ! ==============================================================================================
    CLASS IS ( TOC_FLUSH_STEP_RST_T )
      WRITE(OUTPUT_UNIT,'(A150)') REPEAT('-',150)
      WRITE(OUTPUT_UNIT,'(A,I10)') 'Flush and restart:  ', A%STEP_

    ! ==============================================================================================
    CLASS IS ( TOC_FLUSH_LAST_STEP_T )
      WRITE(OUTPUT_UNIT,'(A150)') REPEAT('-',150)
      WRITE(OUTPUT_UNIT,'(A,I10)') 'Flush last step:    ', A%STEP_

    ! ==============================================================================================
    CLASS IS ( TOC_SIM_END_T )
      WRITE(OUTPUT_UNIT,'(A150)') REPEAT('-',150)
      WRITE(OUTPUT_UNIT,*) 'End of simulation'
      EXIT FeederLoop

    ! ==============================================================================================
    CLASS DEFAULT
      WRITE(*,*) 'ERROR: Enknown entry in toc file'
      STOP

    END SELECT

  ENDDO FeederLoop

  ! Write the configuration file
  OPEN( NEWUNIT=CFGUNIT, FILE='test.yaml', STATUS='UNKNOWN', ACTION='WRITE', FORM='FORMATTED' )
  OFFSET=0
  DOFFSET=2

  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'no-io-info-log:'
  OFFSET=OFFSET+DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'encoding-info: true'
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'verbose: true'
  OFFSET=OFFSET-DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '

  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'dump-fortran-data-reproducer:'
  OFFSET=OFFSET+DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'path: ''./'''
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'dump-values: true'
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'verbose: true'
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'profile: true'
  OFFSET=OFFSET-DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '

  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'grib-msg-to-file:'
  OFFSET=OFFSET+DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'path: ''allfields.grib'''
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'verbose: true'
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'profile: true'
  OFFSET=OFFSET-DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '

  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'grib-msg-to-multio:'
  OFFSET=OFFSET+DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'multio-plans-file: ''../multio-ifs-wam-config.yaml'''
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'verbose: true'
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'profile: true'
  OFFSET=OFFSET-DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '

  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'grib-header-to-multio:'
  OFFSET=OFFSET+DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'multio-plans-file: ''../multio-debug-plan.yaml'''
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'verbose: true'
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'profile: true'
  OFFSET=OFFSET-DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '

  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'full-grib-header-to-multio:'
  OFFSET=OFFSET+DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'multio-plans-file: ''../multio-debug-plan.yaml'''
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'verbose: true'
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'profile: true'
  OFFSET=OFFSET-DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '

  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'fortran-metadata-to-multio:'
  OFFSET=OFFSET+DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'multio-plans-file: ''../multio-debug-plan.yaml'''
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'verbose: true'
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'profile: true'
  OFFSET=OFFSET-DOFFSET

  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '
  WRITE(UNIT=CFGUNIT, FMT='(A)') REPEAT(' ', OFFSET), 'encoding-rules:'
  OFFSET=OFFSET+DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'defaults:'
  OFFSET=OFFSET+DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'encode:'
  OFFSET=OFFSET+DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'gribEdition: 2'
  OFFSET=OFFSET-DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '
  OFFSET=OFFSET-DOFFSET
  WRITE(UNIT=CFGUNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'rules:'
  ! OFFSET=OFFSET+DOFFSET

  DO I = 1, 2
    DO J = 1, 11
      WRITE(*,*) ' - Map dimension: ', MAP(I,J)%SIZE
      IF ( MAP(I,J)%SIZE .GT. 0 ) THEN
        ALLOCATE( PIDS(MAP(I,J)%SIZE) )
        EX = MAP_GET_SORTED_KEYS_INT( MAP(I,J), PIDS )
        CALL WRITE_RULE( CFGUNIT, I, J, PIDS )
        DEALLOCATE(PIDS)
      ENDIF
    ENDDO
  ENDDO

  CLOSE(UNIT=CFGUNIT)

  ! Free memory
  CALL FREE_COMMAND_LINE_OPTIONS( CFG )
  CALL TOC_FREE( TOC )


  DO I = 1, 2
    DO J = 1, 11
      CALL MAP_FREE( MAP(I,J) )
    ENDDO
  ENDDO

CONTAINS

SUBROUTINE WRITE_RULE( UNIT, REPRES, LEVTYPE, PARAMIDS )
IMPLICIT NONE

  INTEGER(KIND=JPIB_K), INTENT(IN) :: UNIT
  INTEGER(KIND=JPIB_K), INTENT(IN) :: LEVTYPE
  INTEGER(KIND=JPIB_K), INTENT(IN) :: REPRES
  INTEGER(KIND=INT32), DIMENSION(:), INTENT(IN) :: PARAMIDS
  INTEGER(KIND=JPIB_K) :: K
  CHARACTER(LEN=16) :: CTMP
  CHARACTER(LEN=1024) :: RULE_NAME
  RULE_NAME=REPEAT(' ',1024)
  RULE_NAME='grib2_'//TRIM(ADJUSTL(IREPRES2CREPRES(I)))//'_'//TRIM(ADJUSTL(ILEVTYPE2CLEVTYPE(J)))
  OFFSET=OFFSET+DOFFSET
  WRITE(UNIT=UNIT, FMT='(A,A,A,A)') REPEAT(' ', OFFSET), '- rule: ''', TRIM(ADJUSTL(RULE_NAME)), ''''
  OFFSET=OFFSET+DOFFSET
  WRITE(UNIT=UNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'filter: '
  OFFSET=OFFSET+DOFFSET
  WRITE(UNIT=UNIT, FMT='(A,A,A,A,A)') REPEAT(' ', OFFSET), 'levtype: ', '[''', TRIM(ADJUSTL(ILEVTYPE2CLEVTYPE(LEVTYPE))), ''']'
  WRITE(UNIT=UNIT, FMT='(A,A,A,A,A)') REPEAT(' ', OFFSET), 'repres:  ', '[''', TRIM(ADJUSTL(IREPRES2CREPRES(REPRES))), ''']'
  WRITE(UNIT=UNIT, FMT='(A,A,A)', ADVANCE='NO') REPEAT(' ', OFFSET), 'paramId:  ', '[ '
  DO K = 1, SIZE(PARAMIDS)-1
    WRITE(CTMP, '(I10)') PARAMIDS(K)
    WRITE(UNIT=UNIT, FMT='(A,A)', ADVANCE='NO') TRIM(ADJUSTL(CTMP)), ', '
  ENDDO
  WRITE(CTMP, '(I10)') PARAMIDS(SIZE(PARAMIDS))
  WRITE(UNIT=UNIT, FMT='(A,A)') TRIM(ADJUSTL(CTMP)), ' ]'
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '
  OFFSET=OFFSET-DOFFSET
  WRITE(UNIT=UNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'encode: '
  OFFSET=OFFSET+DOFFSET
  WRITE(UNIT=UNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'gribEdition: 2'
  IF ( REPRES .EQ. 1 ) THEN
    WRITE(UNIT=UNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'packingType: ''grid_simple'''
  ELSE
    WRITE(UNIT=UNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'packingType: ''spectral_complex'''
  ENDIF
  WRITE(UNIT=UNIT, FMT='(A,A)') REPEAT(' ', OFFSET), 'directToFDB: false'
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '
  WRITE(UNIT=CFGUNIT, FMT='(A)') ' '
  OFFSET=OFFSET-DOFFSET
  OFFSET=OFFSET-DOFFSET
  OFFSET=OFFSET-DOFFSET

END SUBROUTINE WRITE_RULE

END PROGRAM OM_TOOL_PROG
