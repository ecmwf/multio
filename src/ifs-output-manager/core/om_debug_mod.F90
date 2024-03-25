MODULE OM_DEBUG_MOD

  ! Symbolds imported from intrinsic modules
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD,  ONLY: JPEB_K
  USE :: OM_DATA_KIND_MOD,  ONLY: JPIB_K
  USE :: OM_DATA_TYPES_MOD, ONLY: OM_ATM_MSG_T
  USE :: OM_DATA_TYPES_MOD, ONLY: OM_WAM_MSG_T

IMPLICIT NONE

INTEGER(KIND=JPIB_K) :: JPERR_UNIT=ERROR_UNIT

TYPE(OM_ATM_MSG_T), POINTER :: ATMMSG => NULL()
TYPE(OM_WAM_MSG_T), POINTER :: WAMMSG => NULL()

PROCEDURE(OM_MSG_PRINT_ATM_IF), POINTER :: OM_PRINT_ATM_PVT => NULL()
PROCEDURE(OM_MSG_PRINT_WAM_IF), POINTER :: OM_PRINT_WAM_PVT => NULL()

LOGICAL  :: VARS_INITIALIZED=.FALSE.
CHARACTER(LEN=256)   :: GHOSTNAME=REPEAT(' ', 256)
CHARACTER(LEN=16)    :: GPID=REPEAT(' ', 16)
CHARACTER(LEN=16)    :: GMPI_RANK=REPEAT(' ', 16)
CHARACTER(LEN=16)    :: GMPI_SIZE=REPEAT(' ', 16)

INTERFACE
SUBROUTINE OM_ABORT_IF( CDTEXT )
IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: CDTEXT
END SUBROUTINE OM_ABORT_IF

SUBROUTINE OM_MSG_PRINT_ATM_IF( MSG, UNIT )
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
  USE :: OM_DATA_TYPES_MOD, ONLY: OM_ATM_MSG_T
IMPLICIT NONE
  TYPE(OM_ATM_MSG_T),   INTENT(IN)  :: MSG
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: UNIT
END SUBROUTINE OM_MSG_PRINT_ATM_IF

SUBROUTINE OM_MSG_PRINT_WAM_IF( MSG, UNIT )
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
  USE :: OM_DATA_TYPES_MOD, ONLY: OM_WAM_MSG_T
IMPLICIT NONE
  TYPE(OM_WAM_MSG_T),   INTENT(IN)  :: MSG
  INTEGER(KIND=JPIB_K), INTENT(IN)  :: UNIT
END SUBROUTINE OM_MSG_PRINT_WAM_IF

END INTERFACE

PROCEDURE(OM_ABORT_IF), POINTER :: OM_ABORT_PVT => NULL()

INTERFACE OM_CREATE_ERRMSG
  MODULE PROCEDURE OM_CREATE_ERRMSG_STD
  MODULE PROCEDURE OM_CREATE_ERRMSG_GRIB
END INTERFACE


! Whitelist of public symbols (parameters)
PUBLIC :: JPEB_K
PUBLIC :: JPERR_UNIT

! Whitelist of public symbols (procedures)
PUBLIC :: OM_ABORT
PUBLIC :: OM_CREATE_ERRMSG
PUBLIC :: OM_SET_ABORT_PROCEDURE
PUBLIC :: OM_SET_ERROR_UNIT
PUBLIC :: OM_INIT_DEBUG_VARS
PUBLIC :: OM_SET_CURRENT_MESSAGE_ATM
PUBLIC :: OM_SET_CURRENT_MESSAGE_WAM

CONTAINS

SUBROUTINE OM_INIT_DEBUG_VARS( HOSTNAME, PID, MPI_RANK, MPI_SIZE )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD,  ONLY: JPIB_K

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=*), INTENT(IN) :: HOSTNAME
  INTEGER(KIND=JPIB_K), INTENT(IN) :: PID
  INTEGER(KIND=JPIB_K), INTENT(IN) :: MPI_RANK
  INTEGER(KIND=JPIB_K), INTENT(IN) :: MPI_SIZE

  ! Local variables
  CHARACTER(LEN=16) :: TMP

  GHOSTNAME = TRIM(ADJUSTL(HOSTNAME))

  TMP = REPEAT(' ',16)
  WRITE(TMP,'(I16)') PID
  GPID = TRIM(ADJUSTL(TMP))

  TMP = REPEAT(' ',16)
  WRITE(TMP,'(I16)') MPI_RANK
  GMPI_RANK = TRIM(ADJUSTL(TMP))

  TMP = REPEAT(' ',16)
  WRITE(TMP,'(I16)') MPI_SIZE
  GMPI_SIZE = TRIM(ADJUSTL(TMP))

  VARS_INITIALIZED = .TRUE.

END SUBROUTINE OM_INIT_DEBUG_VARS


SUBROUTINE OM_SET_ABORT_PROCEDURE( ABORT_PROCEDURE )
IMPLICIT NONE

PROCEDURE(OM_ABORT_IF) :: ABORT_PROCEDURE

OM_ABORT_PVT => ABORT_PROCEDURE

END SUBROUTINE OM_SET_ABORT_PROCEDURE




SUBROUTINE OM_SET_CURRENT_MESSAGE_ATM( MSG, PRINT_HDL )
  USE :: OM_DATA_TYPES_MOD, ONLY: OM_ATM_MSG_T
IMPLICIT NONE
TYPE(OM_ATM_MSG_T), TARGET, INTENT(IN) :: MSG
PROCEDURE(OM_MSG_PRINT_ATM_IF) :: PRINT_HDL

ATMMSG => MSG
WAMMSG => NULL()

OM_PRINT_ATM_PVT => PRINT_HDL
OM_PRINT_WAM_PVT => NULL()

END SUBROUTINE OM_SET_CURRENT_MESSAGE_ATM

SUBROUTINE OM_SET_CURRENT_MESSAGE_WAM( MSG, PRINT_HDL )
  USE :: OM_DATA_TYPES_MOD, ONLY: OM_WAM_MSG_T
IMPLICIT NONE
TYPE(OM_WAM_MSG_T), TARGET, INTENT(IN) :: MSG
PROCEDURE(OM_MSG_PRINT_WAM_IF) :: PRINT_HDL

ATMMSG => NULL()
WAMMSG => MSG

OM_PRINT_ATM_PVT => NULL()
OM_PRINT_WAM_PVT => PRINT_HDL

END SUBROUTINE OM_SET_CURRENT_MESSAGE_WAM



SUBROUTINE OM_SET_ERROR_UNIT( CUSTOM_ERROR_UNIT )
IMPLICIT NONE
INTEGER(KIND=JPIB_K), INTENT(IN) :: CUSTOM_ERROR_UNIT

JPERR_UNIT = CUSTOM_ERROR_UNIT

END SUBROUTINE OM_SET_ERROR_UNIT



SUBROUTINE OM_CREATE_ERRMSG_STD( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, LINE, ERRIDX, MSG )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPEB_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: STR
  CHARACTER(LEN=*),              INTENT(IN)  :: FNAME
  CHARACTER(LEN=*),              INTENT(IN)  :: SECTION_TYPE
  CHARACTER(LEN=*),              INTENT(IN)  :: SECTION_NAME
  CHARACTER(LEN=*),              INTENT(IN)  :: PROC_TYPE
  CHARACTER(LEN=*),              INTENT(IN)  :: PROC_NAME
  INTEGER(KIND=JPEB_K),          INTENT(IN)  :: LINE
  INTEGER(KIND=JPEB_K),          INTENT(IN)  :: ERRIDX
  CHARACTER(LEN=*),              INTENT(IN)  :: MSG

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ERR_LEN
  CHARACTER(LEN=16)    :: CLLINE
  CHARACTER(LEN=16)    :: CLERRIDX

  ! Convert integers to strings
  CLLINE = REPEAT(' ',16)
  CLERRIDX = REPEAT(' ',16)
  WRITE(CLLINE,'(I10)') LINE
  WRITE(CLERRIDX,'(I10)') ERRIDX

  IF ( VARS_INITIALIZED ) THEN
    ERR_LEN = 250 + &
&             LEN(FNAME) + &
&             LEN(SECTION_TYPE) + &
&             LEN(SECTION_NAME) + &
&             LEN(PROC_TYPE) + &
&             LEN(PROC_NAME) + &
&             LEN(MSG) + &
&             LEN(CLLINE) + &
&             LEN(CLERRIDX) + &
&             LEN(GHOSTNAME) + &
&             LEN(GPID) + &
&             LEN(GMPI_RANK) + &
&             LEN(GMPI_SIZE)

    IF ( ALLOCATED(STR) ) THEN
      DEALLOCATE(STR)
    ENDIF

    ALLOCATE( CHARACTER(ERR_LEN)::STR )
    STR = REPEAT( ' ', ERR_LEN )
    ! Pack the error message
    WRITE(STR,'(37A)') &
&    'ERROR :: {', &
&    'hostname:"',       TRIM(GHOSTNAME),         '"; ', &
&    'pid:"',            TRIM(GPID),              '"; ', &
&    'mpi-rank:"',       TRIM(GMPI_RANK),         '"; ', &
&    'mpi-size:"',       TRIM(GMPI_SIZE),         '"; ', &
&    'file:"',           TRIM(FNAME),             '"; ', &
&    'sectionType:"',    TRIM(SECTION_TYPE),      '"; ', &
&    'sectionName:"',    TRIM(SECTION_NAME),      '"; ', &
&    'procedureType:"',  TRIM(PROC_TYPE),         '"; ', &
&    'procedureName:"',  TRIM(PROC_NAME),         '"; ', &
&    'line:',            TRIM(ADJUSTL(CLLINE)),   '; ' , &
&    'errorIdentifier:', TRIM(ADJUSTL(CLERRIDX)), '; ' , &
&    'msg:"',            TRIM(MSG),               '"}'
  ELSE
    ERR_LEN = 250 + &
&             LEN(FNAME) + &
&             LEN(SECTION_TYPE) + &
&             LEN(SECTION_NAME) + &
&             LEN(PROC_TYPE) + &
&             LEN(PROC_NAME) + &
&             LEN(MSG) + &
&             LEN(CLLINE) + &
&             LEN(CLERRIDX)

    IF ( ALLOCATED(STR) ) THEN
      DEALLOCATE(STR)
    ENDIF

    ALLOCATE( CHARACTER(ERR_LEN)::STR )
    STR = REPEAT( ' ', ERR_LEN )
    ! Pack the error message
    WRITE(STR,'(25A)') &
&    'ERROR :: {', &
&    'file:"',           TRIM(FNAME),             '"; ', &
&    'sectionType:"',    TRIM(SECTION_TYPE),      '"; ', &
&    'sectionName:"',    TRIM(SECTION_NAME),      '"; ', &
&    'procedureType:"',  TRIM(PROC_TYPE),         '"; ', &
&    'procedureName:"',  TRIM(PROC_NAME),         '"; ', &
&    'line:',            TRIM(ADJUSTL(CLLINE)),   '; ' , &
&    'errorIdentifier:', TRIM(ADJUSTL(CLERRIDX)), '; ' , &
&    'msg:"',            TRIM(MSG),               '"}'
  ENDIF

  RETURN

END SUBROUTINE OM_CREATE_ERRMSG_STD


SUBROUTINE OM_CREATE_ERRMSG_GRIB( STR, FNAME, SECTION_TYPE, SECTION_NAME, PROC_TYPE, PROC_NAME, LINE, ERRIDX, MSG, GRIBERRID, GRIBERRMSG )

  ! Symbols imported from other modules within the project.
  USE :: OM_DATA_KIND_MOD, ONLY: JPEB_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
  USE :: OM_DATA_KIND_MOD, ONLY: JPIM_K

IMPLICIT NONE

  ! Dummy arguments
  CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: STR
  CHARACTER(LEN=*),              INTENT(IN)  :: FNAME
  CHARACTER(LEN=*),              INTENT(IN)  :: SECTION_TYPE
  CHARACTER(LEN=*),              INTENT(IN)  :: SECTION_NAME
  CHARACTER(LEN=*),              INTENT(IN)  :: PROC_TYPE
  CHARACTER(LEN=*),              INTENT(IN)  :: PROC_NAME
  INTEGER(KIND=JPEB_K),          INTENT(IN)  :: LINE
  INTEGER(KIND=JPEB_K),          INTENT(IN)  :: ERRIDX
  CHARACTER(LEN=*),              INTENT(IN)  :: MSG
  INTEGER(KIND=JPIM_K),          INTENT(IN)  :: GRIBERRID
  CHARACTER(LEN=*),              INTENT(IN)  :: GRIBERRMSG

  ! Local variables
  INTEGER(KIND=JPIB_K) :: ERR_LEN
  CHARACTER(LEN=16)    :: CLLINE
  CHARACTER(LEN=16)    :: CLERRIDX
  CHARACTER(LEN=16)    :: CLGRIBERRID

  ! Convert integers to strings
  CLLINE = REPEAT(' ',16)
  CLERRIDX = REPEAT(' ',16)
  CLGRIBERRID = REPEAT(' ',16)
  WRITE(CLLINE,'(I10)') LINE
  WRITE(CLERRIDX,'(I10)') ERRIDX
  WRITE(CLGRIBERRID,'(I10)') GRIBERRID

  IF ( VARS_INITIALIZED ) THEN
    ERR_LEN = 250 + &
&             LEN(FNAME) + &
&             LEN(SECTION_TYPE) + &
&             LEN(SECTION_NAME) + &
&             LEN(PROC_TYPE) + &
&             LEN(PROC_NAME) + &
&             LEN(MSG) + &
&             LEN(CLLINE) + &
&             LEN(CLERRIDX) + &
&             LEN(CLGRIBERRID) + &
&             LEN_TRIM(GRIBERRMSG)+ &
&             LEN(GHOSTNAME) + &
&             LEN(GPID) + &
&             LEN(GMPI_RANK) + &
&             LEN(GMPI_SIZE)

    IF ( ALLOCATED(STR) ) THEN
      DEALLOCATE(STR)
    ENDIF

    ALLOCATE( CHARACTER(ERR_LEN)::STR )
    STR = REPEAT( ' ', ERR_LEN )
    ! Pack the error message
    WRITE(STR,'(43A)') &
&    'ERROR :: {', &
&    'hostname:"',       TRIM(GHOSTNAME),            '"; ', &
&    'pid:"',            TRIM(GPID),                 '"; ', &
&    'mpi-rank:"',       TRIM(GMPI_RANK),            '"; ', &
&    'mpi-size:"',       TRIM(GMPI_SIZE),            '"; ', &
&    'file:"',           TRIM(FNAME),                '"; ', &
&    'sectionType:"',    TRIM(SECTION_TYPE),         '"; ', &
&    'sectionName:"',    TRIM(SECTION_NAME),         '"; ', &
&    'procedureType:"',  TRIM(PROC_TYPE),            '"; ', &
&    'procedureName:"',  TRIM(PROC_NAME),            '"; ', &
&    'line:',            TRIM(ADJUSTL(CLLINE)),      '; ' , &
&    'errorIdentifier:', TRIM(ADJUSTL(CLERRIDX)),    '; ' , &
&    'msg:"',            TRIM(MSG),                  '"; ' , &
&    'gribErrorIdx:',    TRIM(ADJUSTL(CLGRIBERRID)), '; ' , &
&    'gribErrorMsg:"',   TRIM(GRIBERRMSG),           '"}'
  ELSE
    ERR_LEN = 250 + &
&             LEN(FNAME) + &
&             LEN(SECTION_TYPE) + &
&             LEN(SECTION_NAME) + &
&             LEN(PROC_TYPE) + &
&             LEN(PROC_NAME) + &
&             LEN(MSG) + &
&             LEN(CLLINE) + &
&             LEN(CLERRIDX) + &
&             LEN(CLGRIBERRID) + &
&             LEN_TRIM(GRIBERRMSG)

    IF ( ALLOCATED(STR) ) THEN
      DEALLOCATE(STR)
    ENDIF

    ALLOCATE( CHARACTER(ERR_LEN)::STR )
    STR = REPEAT( ' ', ERR_LEN )
    ! Pack the error message
    WRITE(STR,'(31A)') &
&    'ERROR :: {', &
&    'file:"',           TRIM(FNAME),                '"; ', &
&    'sectionType:"',    TRIM(SECTION_TYPE),         '"; ', &
&    'sectionName:"',    TRIM(SECTION_NAME),         '"; ', &
&    'procedureType:"',  TRIM(PROC_TYPE),            '"; ', &
&    'procedureName:"',  TRIM(PROC_NAME),            '"; ', &
&    'line:',            TRIM(ADJUSTL(CLLINE)),      '; ' , &
&    'errorIdentifier:', TRIM(ADJUSTL(CLERRIDX)),    '; ' , &
&    'msg:"',            TRIM(MSG),                  '"; ' , &
&    'gribErrorIdx:',    TRIM(ADJUSTL(CLGRIBERRID)), '; ' , &
&    'gribErrorMsg:"',   TRIM(GRIBERRMSG),           '"}'
  ENDIF

  RETURN

END SUBROUTINE OM_CREATE_ERRMSG_GRIB


SUBROUTINE OM_ABORT( CDMSG )
IMPLICIT NONE
CHARACTER(LEN=*), INTENT(IN) :: CDMSG


CALL SLEEP(1)
FLUSH(JPERR_UNIT)

IF ( ASSOCIATED(ATMMSG) .AND. ASSOCIATED(OM_PRINT_ATM_PVT) ) THEN
  WRITE(JPERR_UNIT,*) 'ERROR :: Unrecoverabe error detected while processing atmosphere msg: '
  CALL OM_PRINT_ATM_PVT( ATMMSG, JPERR_UNIT )
ENDIF

IF ( ASSOCIATED(WAMMSG) .AND. ASSOCIATED(OM_PRINT_WAM_PVT) ) THEN
  WRITE(JPERR_UNIT,*) 'ERROR :: Unrecoverabe error detected while processing wave msg: '
  CALL OM_PRINT_WAM_PVT( WAMMSG, JPERR_UNIT )
ENDIF

FLUSH(JPERR_UNIT)
CALL SLEEP(1)

IF ( ASSOCIATED( OM_ABORT_PVT ) ) THEN
  CALL OM_ABORT_PVT( CDMSG )
ELSE
  STOP
ENDIF

END SUBROUTINE OM_ABORT



END MODULE OM_DEBUG_MOD