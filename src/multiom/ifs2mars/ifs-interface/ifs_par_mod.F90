! Include preprocessor utils
#include "output_manager_preprocessor_utils.h"
#include "output_manager_preprocessor_trace_utils.h"
#include "output_manager_preprocessor_logging_utils.h"
#include "output_manager_preprocessor_errhdl_utils.h"


#define PP_FILE_NAME 'ifs_par_mod.F90'
#define PP_SECTION_TYPE 'MODULE'
#define PP_SECTION_NAME 'IFS_PAR_MOD'
MODULE IFS_PAR_MOD

  ! Symbols imported from other modules within the project.
  USE :: DATAKINDS_DEF_MOD, ONLY: JPIB_K
  USE :: DATAKINDS_DEF_MOD, ONLY: JPRD_K

IMPLICIT NONE

PRIVATE

TYPE :: PROC_TOPO_T

  ! Model MPI #1 + IO tasks communicator
  INTEGER(KIND=JPIB_K) :: NCOMM_W1IO

  ! Model communicator
  INTEGER(KIND=JPIB_K) :: NCOMM_WR

  ! IO server communicator
  INTEGER(KIND=JPIB_K) :: NCOMM_IO

  ! MPI_COMM_WORLD communicator
  INTEGER(KIND=JPIB_K) :: NCOMM_WRIO

  ! Rank of current task in MPI_COMM_WORLD
  INTEGER(KIND=JPIB_K) :: MYPROC_WRIO

  ! Rank of current task in the IO group
  INTEGER(KIND=JPIB_K) :: MYPROC_IO

  ! Rank of current task in the working group (==YOMMP%MYPROC)
  INTEGER(KIND=JPIB_K) :: MYPROC_WR

  ! Number of tasks in COMM WORLD
  INTEGER(KIND=JPIB_K) :: NPROC_WRIO

  ! Number of tasks in IO group
  INTEGER(KIND=JPIB_K) :: NPROC_IO

  ! TRUE if the current task is running an IO server
  LOGICAL :: LIO_SERVER

  ! TRUE is the current task is running an IO client
  LOGICAL :: LIO_CLIENT
END TYPE

TYPE :: SIM_PAR_T

  ! Table version to be used for grib 2 encoding
  INTEGER(KIND=JPIB_K) :: IGRIB2_TABLES_VERSION_LATEST

  ! Number of tasks in IO group
  INTEGER(KIND=JPIB_K) :: NPROC_IO

  ! yomgrib.F90 - grib coding descriptors
  ! -------------------------------------
  ! NCYCLE : cycle identifier
  INTEGER(KIND=JPIB_K) :: NCYCLE

  ! NLOCGRB - ECMWF LOCAL USAGE IDENTIFIER
  INTEGER(KIND=JPIB_K) :: NLOCGRB

! NBITSSHLNSP - Number of bits for GRIB encoding of LNSP (default=16)
  INTEGER(KIND=JPIB_K) :: NBITSSHLNSP

! NBITSEXPR   - Number of bits for GRIB encoding of experimental parameters (default=-1 in which case multio is deciding)
  INTEGER(KIND=JPIB_K) :: NBITSEXPR

  ! CTYPE      : type for use by FDB
  CHARACTER(LEN=2) :: CTYPE

  ! CFCLASS    : class for use by FDB
  CHARACTER(LEN=2) :: CFCLASS

  ! NLEG    - current VAREPS leg number (eg 1(2) for the T399(T255) part of a T399-T255 VAREPS)
  INTEGER(KIND=JPIB_K) :: NLEG

  ! NTOTENS - TOTAL NUMBER OF FORCASTS IN ENSEMBLE
  INTEGER(KIND=JPIB_K) :: NTOTENS

  ! NENSFNB - ENSAMBLE FORECAST NUMBER
  INTEGER(KIND=JPIB_K) :: NENSFNB

  ! NWINOFF - OFFSET OF ANALYSIS FROM END OF ASSIMILATION WINDOW [HOURS]
  INTEGER(KIND=JPIB_K) :: NWINSIZE
  INTEGER(KIND=JPIB_K) :: NWINOFF

  ! NJDIAG  - SENSITIVITY DIAGNOSTIC NUMBER (1=>J1, 2=>J2, 3=>J3, 4=>J4)
  INTEGER(KIND=JPIB_K) :: NJDIAG

  ! NJDOMAI - SENSITIVITY DIAGNOSTIC MASK REGION (0=>global, 1=>europe, 2=>N.H., 3=>S.H.)
  INTEGER(KIND=JPIB_K) :: NJDOMAI

  ! ???????????????????
  INTEGER(KIND=JPIB_K) :: NJITER

  ! NSTREAM - EXPLICIT STREAM NUMBER (OR ZERO FOR DEFAULT STREAMS TO BE USED)
  INTEGER(KIND=JPIB_K) :: NSTREAM

  ! NSYSTEM - FOR USE IN SEASONAL STREAM (DIFFERENT OPERATIONAL SYSTEMS)
  INTEGER(KIND=JPIB_K) :: NSYSTEM

  ! NMETHOD - FOR USE IN SEASONAL STREAM (DIFFERENT ENSEMBLES)
  INTEGER(KIND=JPIB_K) :: NMETHOD

  ! NREFERENCE - FOR USE IN HINDCAST STREAM
  INTEGER(KIND=JPIB_K) :: NREFERENCE

  ! NCONSENSUS - FOR MULTI_ANALYSIS STREAM (1 if consensus)
  INTEGER(KIND=JPIB_K) :: NCONSENSUS

  ! NDWD    - FOR MULTI_ANALYSIS STREAM (1 if DWD analysis used)
  INTEGER(KIND=JPIB_K) :: NDWD

  ! NMFR    - FOR MULTI_ANALYSIS STREAM
  INTEGER(KIND=JPIB_K) :: NMFR

  ! NNCEP   - FOR MULTI_ANALYSIS STREAM
  INTEGER(KIND=JPIB_K) :: NNCEP

  ! NUKM   - FOR MULTI_ANALYSIS STREAM
  INTEGER(KIND=JPIB_K) :: NUKM


  ! yomrip0.F90 - time related quantities
  ! NINDAT : run initial date in the form AAAAMMDD
  INTEGER(KIND=JPIB_K) :: NINDAT

  ! NSSSSS : initial time in seconds (e.g. for 12h, 43200)
  INTEGER(KIND=JPIB_K) :: NSSSSS

  ! RTIMST : ABSOLUTE TIME OF THE MODEL AT START
  REAL(KIND=JPRD_K) :: RTIMST


  ! yomct0.F90 - control variables for the job (Constant across the job)
  ! --------------------------------------------------------------------
  ! NCONF      : configuration of the job
  !                0- 99 : 3-D integration job
  !              100-199 : variational job
  !              200-299 : 2-D integration job
  !              300-349 : KALMAN filter
  !              350-399 : predictability model             (currently unused)
  !              400-499 : test of the adjoint
  !              500-599 : test of the tangent linear model
  !              600-699 : eigenvalue/vector solvers
  !              700-799 : optimal interpolation
  !              800-899 : sensitivity
  !              900-999 : miscellaneous other configurations.
  !                    1 : 3-D primitive equation model
  !                  131 : incremental 4-D VAR/3-D VAR
  !                  201 : shallow-water model
  !                  202 : vorticity equation model
  !                  302 : simplified extended Kalman filter (SEKF)
  !                  401 : test of adjoint with 3-D P.E. model
  !                  421 : test of adjoint with shallow-water model
  !                  422 : test of adjoint with vorticity equation model
  !                  501 : test of tangent linear with 3-D P.E. model
  !                  521 : test of tangent linear with shallow-water model
  !                  522 : test of tangent linear with vorticity equation model
  !                  601 : eigenvalue/vector solver for 3-D P.E. model
  !                  701 : optimal interpolation with CANARI
  !                  801 : sensitivity with 3-D P.E. model
  !                  901 : set up initial conditions (CPREP1)
  !                  923 : initialisation of climatologic files
  !                  931 : creation of an ARPEGE file containing the SST (INCLITC).
  !                  932 : interpolates the sea-ice concentration field from
  !                        satellite data to the ARPEGE grid (CSEAICE).
  !                  933 : interpolates SST from OSTIA and sea-ice concentration
  !                  from OSI SAF.
  INTEGER(KIND=JPIB_K) :: NCONF

  ! NSTEPINI: Initial step in hours for the initial conditions
  !           at the beginning of 4D-Var trajectory (usually 3 hours).
  !           It is used to update the step while saving the FCs along
  !           the first trajectory.
  INTEGER(KIND=JPIB_K) :: NSTEPINI

  LOGICAL :: LPPSTEPS

  REAL(KIND=JPRD_K) :: TSTEP

  ! LOBSC1  : .T. = term of observations included in configuration 1
  LOGICAL :: LOBSC1

  ! CNMEXP     : name of the experiment
  !              An experiment is identified by its name (16 characters)
  !              and its cycle (typically same experiment but without a bug)
  CHARACTER (LEN = 16) ::  CNMEXP


  ! yomvareps.F90 - control varibles for vareps
  ! -------------------------------------------
  ! LVAREPS          : .T. when running with variable resolution
  LOGICAL :: LVAREPS

  ! NFCHO_TRUNC_INI  : forecast step used to define the ICs (ie NFCHO_TRUNC of previous VAREPS LEG)
  INTEGER(KIND=JPIB_K) :: NFCHO_TRUNC_INI


  ! ocean atmosphere coupling
  LOGICAL :: LDMCC04

END TYPE




TYPE :: GEO_PAR_T
  ! pardim.F90 - parameters dimensions
  ! ----------------------------------
  ! JPMXLE : MAXIMUM NUMBER OF LEVELS
  INTEGER(KIND=JPIB_K) :: JPMXLE

  ! JPMXGL : MAXIMUM NUMBER OF GAUSSIAN LATITUDES
  INTEGER(KIND=JPIB_K) :: JPMXGL


  ! yomdim.F90 - Geometry dimensions
  ! --------------------------------
  ! NSMAX   : truncation order
  INTEGER(KIND=JPIB_K) :: ISMAX
  ! NDGLG  : number of rows of latitudes
  INTEGER(KIND=JPIB_K) :: ILATS
  ! NDLON  : length of a row of latitude near equator
  INTEGER(KIND=JPIB_K) :: ILONS
  ! dependant (ILATS+1)/2
  INTEGER(KIND=JPIB_K) :: IDGNH


  ! yomleg.F90 - Legendre Polynomials
  !----------------------------------
  ! RLATIG(0) : theta of the zero (1) latitude
  REAL(KIND=JPRD_K) :: ZNLAT
  ! RLATIG(0) : theta of the last (NDGLG) latitude
  REAL(KIND=JPRD_K) :: ZSLAT


  ! yomvert.F90 - vertical coordinates handleing
  ! IFLEV : UBOUND(VBH) -> VBH(0:IFLEV) : B of the vertical coordinate
  INTEGER(KIND=JPIB_K) :: IFLEV

  ! ZVERT = [ VBH, VAH ]
  ! VBH : (0:NFLEVG) : B of the vertical coordinate
  ! VAH : (0:NFLEVG) ;  =VALH*VP00
  REAL(KIND=JPRD_K), DIMENSION(:), ALLOCATABLE :: ZVERT


  ! yomgem.F90 - Geometry definition
  !----------------------------------

  !     NHTYP  : 0 = regular grid
  !            : 2 = number of points read on namelist namrgri
  INTEGER(KIND=JPIB_K) :: NHTYP

  !     NSTTYP : 1 = POLE OF STRETCHING, POLE OF THE COLLOCATION GRID
  !                 AT THE NORTHERN POLE OF THE REAL EARTH.
  !              2 = THE POLE OF STRETCHING IS ANYWHERE ON THE REAL EARTH
  !             AND ON THE EQUATOR OF THE COLLOCATION GRID ON THE MERIDIAN PI.
  !                  THE EQUATOR OF THE COLLOCATION GRID IS TANGENT
  !             TO A PARALLEL OF THE EARTH.
  INTEGER(KIND=JPIB_K) :: NSTTYP

  ! RMUCEN : MU OF THE POLE OF STRETCHING
  REAL(KIND=JPRD_K) :: RMUCEN

  ! RLOCEN : LONGITUDE OF THE POLE OF STRETCHING
  REAL(KIND=JPRD_K) :: RLOCEN

  ! RSTRET : STRETCHING FACTOR
  REAL(KIND=JPRD_K) :: RSTRET

  ! NLOENG(1:ILATS) : number of active points on a parallel (global)
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: ILOENG


  ! yomgrib.F90
  !----------------------------------
  INTEGER(KIND=JPIB_K), DIMENSION(:,:), ALLOCATABLE :: NSFLEVS


END TYPE

TYPE :: ATM_PAR_T
  ! TODO
  ! Chemistry etc...
END TYPE

TYPE :: WAM_PAR_T

  !
  ! yowfred.F90

  ! *FR* REAL FREQUENCIES IN HERTZ.  (1:NFRE_RED)
  REAL(KIND=JPRD_K), DIMENSION(:), ALLOCATABLE :: FR

  ! *TH* REAL DIRECTIONS IN RADIANS. (1:NANG)
  REAL(KIND=JPRD_K), DIMENSION(:), ALLOCATABLE :: TH

  !
  ! yowcoup.F90
  !      *LWCOUSAMEGRID        TRUE when coupled and the atmospheric grid and the wave grid is the same
  LOGICAL :: LWCOUSAMEGRID

  !
  ! yowparam.F90
  ! *CLDOMAIN*  CHARACTER DEFINES THE DOMAIN OF THE MODEL (for the
  !                       FDB and for selection of some variables)
  CHARACTER(LEN=1)   :: CLDOMAIN

  ! *NGX*      INTEGER  NUMBER OF LONGITUDES IN GRID.
  INTEGER(KIND=JPIB_K) :: NGX

  ! *NGY*      INTEGER  NUMBER OF LATITUDES IN GRID.
  INTEGER(KIND=JPIB_K) :: NGY

  ! *NANG*     INTEGER  NUMBER OF ANGLES.
  INTEGER(KIND=JPIB_K) :: NANG

  ! *NFRE_RED* INTEGER  REDUCED NUMBER OF FREQUENCIES FOR THE PROPAGATION AND IO
  !                     BY DEFAULT = NFRE
  INTEGER(KIND=JPIB_K) :: NFRE_RED


  !
  ! yowgribhd.F90
  !   IMDLGRBID_G           INTEGER   GLOBAL MODEL IDENTIFICATION FOR GRIB CODING
  !                                   IT CAN ALSO BE MODIFIED IN THE INPUT NAMELIST.
  INTEGER(KIND=JPIB_K) :: IMDLGRBID_G

  !   IMDLGRBID_M           INTEGER   LAW MODEL IDENTIFICATION FOR GRIB CODING
  !                                   IT CAN ALSO BE MODIFIED IN THE INPUT NAMELIST.
  INTEGER(KIND=JPIB_K) :: IMDLGRBID_M

  !   NDATE_TIME_WINDOW_END INTEGER ?
  INTEGER(KIND=JPIB_K) :: NDATE_TIME_WINDOW_END

  !   NWINOFF               INTEGER ?
  INTEGER(KIND=JPIB_K) :: NWINOFF

  !   NGRIB_VERSION         INTEGER ?
  INTEGER(KIND=JPIB_K) :: NGRIB_VERSION

  !   NTENCODE              INTEGER   TOTAL NUMBER OF GRID POINTS FOR ENCODING
  INTEGER(KIND=JPIB_K) :: NTENCODE

  !   NGRBRESI              INTEGER   NUMBER OF BITS USED TO ENCODE INTEGRATED
  !                                   PARAMETERS
  INTEGER(KIND=JPIB_K) :: NGRBRESI

  !   NGRBRESS              INTEGER   NUMBER OF BITS USED TO ENCODE SPECTRA
  INTEGER(KIND=JPIB_K) :: NGRBRESS

  !   PPMISS                REAL      ALL SPECTRAL VALUES LESS OR EQUAL PPMISS ARE
  !                                   REPLACED BY THE MISSING DATA INDICATOR
  REAL(KIND=JPRD_K) :: PPMISS

  !   PPEPS                 REAL      SMALL NUMBER USED IN SPECTRAL PACKING OF 251
  REAL(KIND=JPRD_K) :: PPEPS

  !   PPREC                 REAL      REFERENCE VALUE FOR SPECTRAL PACKING OF 251
  REAL(KIND=JPRD_K) :: PPREC

  !   PPRESOL               REAL      MAXIMUN RESOLUTION POSSIBLE WHEN ENCODING
  !                                   SPECTRA (PARAMETER 251).
  REAL(KIND=JPRD_K) :: PPRESOL

  !   PPMIN_RESET           REAL      CAN BE USED TO SET THE MINIMUM OF PPMIN
  !                                   IN WGRIBOUT TO A LOWER VALUE.
  REAL(KIND=JPRD_K) :: PPMIN_RESET

  !   HOPERI                CHARACTER GRIB ENCODING ACTION FOR INTEGRATED FIELDS.
  CHARACTER(LEN=1) :: HOPERI

  !   HOPERS                CHARACTER GRIB ENCODING ACTION FOR SPECTRA.
  CHARACTER(LEN=1) :: HOPERS

  !   LGRHDIFS              LOGICAL   IF TRUE THEN GRIB HEADER WILL USE INFORMATION
  !                                   AS PROVIDED BY THE IFS.
  LOGICAL :: LGRHDIFS

  !   LNEWLVTP              LOGICAL   IF TRUE THE NEW LEVTYPE DEFINITION WILL BE USED.
  LOGICAL :: LNEWLVTP

  !   LPADPOLES             LOGICAL   TRUE IF POLES ARE PADDED WHEN SAVIND TO GRIB.
  LOGICAL :: LPADPOLES

  !   LL_GRID_SIMPLE_MATRIX IF TRUE THEN THE 2D SPECTRA WILL USE THE LEGACY grid_simple_matrix
  !                         TO ENCODE THE 2D SPECTRA in GRIB1. THIS SHOULD BE PHASED OUT as soon as feasible!
  LOGICAL :: LL_GRID_SIMPLE_MATRIX


  !
  ! yowmap.F90
  !  *IRGG*    INTEGER   GRID CODE: 0 = REGULAR, 1 = IRREGULAR.
  INTEGER(KIND=JPIB_K) :: IRGG

  !  *IQGAUSS* INTEGER   =1 IF A QUASI GAUSSIAN GRID IS USED.
  !                      =0 OTHERWISE.
  INTEGER(KIND=JPIB_K) :: IQGAUSS

  !  *AMOWEP*  REAL      MOST WESTERN LONGITUDE IN GRID (DEGREE).
  REAL(KIND=JPRD_K) :: AMOWEP

  !  *AMOSOP*  REAL      MOST SOUTHERN LATITUDE IN GRID (DEGREE).
  REAL(KIND=JPRD_K) :: AMOSOP

  !  *AMOEAP*  REAL      MOST EASTERN LONGITUDE IN GRID (DEGREE).
  REAL(KIND=JPRD_K) :: AMOEAP

  !  *AMONOP*  REAL      MOST NORTHERN LATITUDE IN GRID (DEGREE).
  REAL(KIND=JPRD_K) :: AMONOP

  !  *XDELLA*  REAL      GRID INCREMENT FOR LATITUDE (DEGREE).
  REAL(KIND=JPRD_K) :: XDELLA

  !  *XDELLO*  REAL      CONSTANT GRID INCREMENT FOR LONGITUDE
  REAL(KIND=JPRD_K) :: XDELLO

  !  *NLONRGG* INTEGER   NUMBER OF GRID POINTS PER LATITUDES (NOT THE PL ARRAY, NEED WORK). (1:NGY)
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: NLONRGG


  !
  ! yowpcons.F90
  !  *ZMISS* REAL MISSING DATA INDICATOR
  !               (SET IN CHIEF OR VIA THE IFS).
  REAL(KIND=JPRD_K) :: ZMISS


  !
  ! yowstat.F90
  !  *NENSFNB*   INTEGER ENSEMBLE FORECAST NUMBER (DEFAULT=0)
  INTEGER(KIND=JPIB_K) :: NENSFNB

  !  *NTOTENS*   INTEGER TOTAL ENSEMBLE FORECAST MEMBERS (DEFAULT=0)
  INTEGER(KIND=JPIB_K) :: NTOTENS

  !  *NSYSNB*    INTEGER SYSTEM NUMBER TO BE USED FOR GRIBBING OF
  !                      SEASONAL DATA (DEFAULT=-1).
  INTEGER(KIND=JPIB_K) :: NSYSNB

  !  *NMETNB*    INTEGER METHOD NUMBER TO BE USED FOR GRIBBING OF
  !                      SEASONAL DATA (DEFAULT=-1).
  INTEGER(KIND=JPIB_K) :: NMETNB

  !  *ISTREAM*   INTEGER STREAM NUMBER WHEN CODING DATA IN GRID
  !                      IF SET TO 0 IT WILL NOT BE USED AND
  !                      INSTEAD MARSTYPE WILL BVE USED TO DETERMINE
  !                      THE STREAM.
  INTEGER(KIND=JPIB_K) :: ISTREAM

  !  *NLOCGRB*   INTEGER LOCAL GRIB TABLE NUMBER.
  INTEGER(KIND=JPIB_K) :: NLOCGRB

  !  *NCONCENSUS INTEGER ONLY USED IN THE CONTEXT OF MULTI-ANALYSIS
  !                      ENSEMBLE FORECASTS. IT SPECIFIED WHETHER
  !                      ONE (NCONCENSUS=0) OR MORE ANALYSES ARE
  !                      USED IN THE INITIAL CONDITIONS.
  INTEGER(KIND=JPIB_K) :: NCONSENSUS

  !  *NDWD*      INTEGER ONLY USED IN THE CONTEXT OF MULTI-ANALYSIS
  !                      ENSEMBLE FORECASTS. IT SPECIFIED WHETHER
  !                      DWD IS USED IN THE INITIAL CONDITIONS.
  INTEGER(KIND=JPIB_K) :: NDWD

  !  *NMFR*      INTEGER ONLY USED IN THE CONTEXT OF MULTI-ANALYSIS
  !                      ENSEMBLE FORECASTS. IT SPECIFIED WHETHER
  !                      METEO FRANCE IS USED IN THE INITIAL
  !                      CONDITIONS.
  INTEGER(KIND=JPIB_K) :: NMFR

  !  *NNCEP*     INTEGER ONLY USED IN THE CONTEXT OF MULTI-ANALYSIS
  !                      ENSEMBLE FORECASTS. IT SPECIFIED WHETHER
  !                      NCEP IS USED IN THE INITIAL CONDITIONS.
  INTEGER(KIND=JPIB_K) :: NNCEP

  !  *NUKM*      INTEGER ONLY USED IN THE CONTEXT OF MULTI-ANALYSIS
  !                      ENSEMBLE FORECASTS. IT SPECIFIED WHETHER
  !                      THE MET OFFICE IS USED IN THE INITIAL
  !                      CONDITIONS.
  INTEGER(KIND=JPIB_K) :: NUKM

  !  *IREFDATE*  INTEGER REFERENCE DATE FOR MONTHLY FORECAST
  !                      HINDCAST RUNS.
  INTEGER(KIND=JPIB_K) :: IREFDATE

  !  *MARSTYPE*  CHAR*2  CHARACTER STRING INDICATING THE CURRENT
  !                      STATUS OF THE MODEL.
  CHARACTER(LEN=2) :: MARSTYPE

  !  *YCLASS*    CHAR*2  CHARACTER STRING INDICATING THE CLASS OF
  !                      THE CURRENT RUN.
  CHARACTER(LEN=2) :: YCLASS

  !  *YEXPVER*   CHAR*4  CHARACTER STRING INDICATING THE EXPERIMENT
  !                      VERSION NUMBER OF THE CURRENT RUN.
  CHARACTER(LEN=4) :: YEXPVER


!     GRIB2 TABLE VERSION FOR WAVE SPECTRA
  INTEGER(KIND=JPIB_K) :: NSPEC2TAB

!     GRIB2 TEMPLATE NUMBER FOR WAVE SPECTRA
  INTEGER(KIND=JPIB_K) :: NSPEC2TMPD   ! DETERMINISTIC
  INTEGER(KIND=JPIB_K) :: NSPEC2TMPP  ! PROBABILISTIC

!     GRIB2 TEMPLATE NUMBER FOR PARAMETER DEFINED OVER A WAVE PERIOD RANGE
  INTEGER(KIND=JPIB_K) :: NTRG2TMPD   ! DETERMINISTIC
  INTEGER(KIND=JPIB_K) :: NTRG2TMPP   ! PROBABILISTIC



!          *ITMIN*   MINIMUM WAVE PERIOD FOR WHICH THE PARAMETER IS DEFINED (s)
!          *ITMAX*   MAXIMUM WAVE PERIOD FOR WHICH THE PARAMETER IS DEFINED (s)
  INTEGER(KIND=JPIB_K) :: ITMIN
  INTEGER(KIND=JPIB_K) :: ITMAX

END TYPE

TYPE :: SAT_PAR_T
  INTEGER(KIND=JPIB_K) :: NSATSIM
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE  :: MSERIES
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE  :: MSATID
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE  :: MINST
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE  :: MCHAN
  REAL(KIND=JPRD_K),    DIMENSION(:), ALLOCATABLE  :: RCWN
END TYPE

TYPE :: MODEL_PAR_T
  TYPE(SIM_PAR_T) :: SIM_
  TYPE(GEO_PAR_T) :: GEO_
  TYPE(ATM_PAR_T) :: ATM_
  TYPE(WAM_PAR_T) :: WAM_
  TYPE(SAT_PAR_T) :: SAT_
END TYPE


! Whitelist of public symbols (datatypes)
PUBLIC :: PROC_TOPO_T
PUBLIC :: MODEL_PAR_T
PUBLIC :: SIM_PAR_T
PUBLIC :: GEO_PAR_T
PUBLIC :: ATM_PAR_T
PUBLIC :: WAM_PAR_T
PUBLIC :: SAT_PAR_T


! Whitelist of public symbols (procedures)
PUBLIC :: PAR_CREATE_NAME
PUBLIC :: PAR_GET_ENDIANNES
PUBLIC :: PAR_WOPEN
PUBLIC :: PAR_ROPEN
PUBLIC :: PAR_CLOSE
PUBLIC :: PAR_PRINT
PUBLIC :: PAR_WRITE
PUBLIC :: PAR_READ

CONTAINS


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PAR_CREATE_NAME'
PP_THREAD_SAFE FUNCTION PAR_CREATE_NAME( DIRECTORY, PROC_ID, PARFNAME, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PROC_ID
  CHARACTER(LEN=*),     INTENT(OUT)   :: PARFNAME
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: VALEXIST
  INTEGER(KIND=JPIB_K) :: N
  INTEGER(KIND=JPIB_K) :: M
  INTEGER(KIND=JPIB_K) :: STAT

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_FILE_NAME_TOO_SHORT = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CREATE_FILE_NAME = 2_JPIB_K

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
  N = LEN(PARFNAME)
  M = LEN_TRIM(DIRECTORY) + 17
  PP_DEBUG_CRITICAL_COND_THROW( M.GT.N, ERRFLAG_FILE_NAME_TOO_SHORT)

  ! Create the message name
  PARFNAME = REPEAT(' ',N)
  WRITE(PARFNAME,'(A,A,I8.8,A,I8.8,A)', IOSTAT=STAT) TRIM(ADJUSTL(DIRECTORY)), '/par_', PROC_ID, '.bin'
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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to create the par file name' )
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

END FUNCTION PAR_CREATE_NAME
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PAR_WOPEN'
PP_THREAD_SAFE FUNCTION PAR_WOPEN( PARFNAME, PARUNIT, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: PARFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: PARUNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: VALEXIST
  INTEGER(KIND=JPIB_K) :: STAT

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_PAR_FILE_ALREADY_EXISTS = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN_PAR_FILE = 2_JPIB_K

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
  INQUIRE( FILE=TRIM(PARFNAME), EXIST=VALEXIST )
  PP_DEBUG_CRITICAL_COND_THROW( VALEXIST, ERRFLAG_PAR_FILE_ALREADY_EXISTS)

  ! Open the TOC file
  OPEN( NEWUNIT=PARUNIT, FILE=TRIM(PARFNAME), STATUS='REPLACE', &
&       ACCESS='STREAM', ACTION='WRITE', FORM='UNFORMATTED', IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_OPEN_PAR_FILE)

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
    CASE (ERRFLAG_PAR_FILE_ALREADY_EXISTS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Val file already exists' )
    CASE (ERRFLAG_UNABLE_TO_OPEN_PAR_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open par file' )
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

END FUNCTION PAR_WOPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PAR_ROPEN'
PP_THREAD_SAFE FUNCTION PAR_ROPEN( PARFNAME, PARUNIT, BIG_ENDIAN_READ, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: PARFNAME
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: PARUNIT
  LOGICAL,              INTENT(IN)    :: BIG_ENDIAN_READ
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: PAREXIST
  INTEGER(KIND=JPIB_K) :: STAT

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FIND_PAR_FILE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN_PAR_FILE = 2_JPIB_K

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
  INQUIRE( FILE=TRIM(PARFNAME), EXIST=PAREXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.PAREXIST, ERRFLAG_UNABLE_TO_FIND_PAR_FILE )

  ! Open the TOC file
  IF ( BIG_ENDIAN_READ ) THEN
    OPEN( NEWUNIT=PARUNIT, FILE=TRIM(PARFNAME), STATUS='OLD', ACCESS='STREAM', &
&         ACTION='READ', FORM='UNFORMATTED', CONVERT='BIG_ENDIAN', IOSTAT=STAT )
  ELSE
    OPEN( NEWUNIT=PARUNIT, FILE=TRIM(PARFNAME), STATUS='OLD', ACCESS='STREAM', &
&         ACTION='READ', FORM='UNFORMATTED', IOSTAT=STAT )
  ENDIF
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_OPEN_PAR_FILE )

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
    CASE (ERRFLAG_UNABLE_TO_FIND_PAR_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find par file: '//TRIM(PARFNAME) )
    CASE (ERRFLAG_UNABLE_TO_OPEN_PAR_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open par file: '//TRIM(PARFNAME) )
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

END FUNCTION PAR_ROPEN
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE





#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PAR_GET_ENDIANNES'
PP_THREAD_SAFE FUNCTION PAR_GET_ENDIANNES( PARFNAME, BIG_ENDIAN_READ, HOOKS ) RESULT(RET)

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
  CHARACTER(LEN=*),     INTENT(IN)    :: PARFNAME
  LOGICAL,              INTENT(OUT)   :: BIG_ENDIAN_READ
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: PARUNIT
  INTEGER(KIND=JPIB_K) :: TABLE_VERSION
  LOGICAL :: PAREXIST
  INTEGER(KIND=JPIB_K) :: STAT

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_FIND_PAR_FILE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_OPEN_PAR_FILE = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_TABLE_VERSION = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_CLOSE_PAR_FILE = 4_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_UNABLE_TO_DETERMINE_ENDIANNESS = 5_JPIB_K


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
  INQUIRE( FILE=TRIM(PARFNAME), EXIST=PAREXIST )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.PAREXIST, ERRFLAG_UNABLE_TO_FIND_PAR_FILE )

  ! First guess is that the file is little endian
  BIG_ENDIAN_READ = .FALSE.

  ! Open the PAR file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_OPEN_PAR_FILE) PAR_ROPEN( PARFNAME, PARUNIT, BIG_ENDIAN_READ, HOOKS )

  ! Read the first integer
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_TABLE_VERSION) READ_TABLE_VERSION_PAR( PARUNIT, TABLE_VERSION, HOOKS )

  ! Close the PAR file
  PP_TRYCALL(ERRFLAG_UNABLE_TO_CLOSE_PAR_FILE) PAR_CLOSE( PARUNIT, HOOKS )

  ! Check if the table version is in the expected range
  IF ( TABLE_VERSION.LT.0 .OR. TABLE_VERSION.GT.200 ) THEN

    ! Update the endinaness
    BIG_ENDIAN_READ = .TRUE.

    ! Open the PAR file
    PP_TRYCALL(ERRFLAG_UNABLE_TO_OPEN_PAR_FILE) PAR_ROPEN( PARFNAME, PARUNIT, BIG_ENDIAN_READ, HOOKS )

    ! Read the first integer
    PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_TABLE_VERSION) READ_TABLE_VERSION_PAR( PARUNIT, TABLE_VERSION, HOOKS )

    ! Close the PAR file
    PP_TRYCALL(ERRFLAG_UNABLE_TO_CLOSE_PAR_FILE) PAR_CLOSE( PARUNIT, HOOKS )

    ! Check if the table version is in the expected range
    PP_DEBUG_CRITICAL_COND_THROW( TABLE_VERSION.LT.0 .OR. TABLE_VERSION.GT.200, ERRFLAG_UNABLE_UNABLE_TO_DETERMINE_ENDIANNESS )

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
    CASE (ERRFLAG_UNABLE_TO_FIND_PAR_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to find par file: '//TRIM(PARFNAME) )
    CASE (ERRFLAG_UNABLE_TO_OPEN_PAR_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to open par file: '//TRIM(PARFNAME) )
    CASE (ERRFLAG_UNABLE_TO_READ_TABLE_VERSION)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read table version' )
    CASE (ERRFLAG_UNABLE_TO_CLOSE_PAR_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to close par file' )
    CASE (ERRFLAG_UNABLE_UNABLE_TO_DETERMINE_ENDIANNESS)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to determine endianness' )
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

END FUNCTION PAR_GET_ENDIANNES
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PAR_CLOSE'
PP_THREAD_SAFE FUNCTION PAR_CLOSE( PARUNIT, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: PARUNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  LOGICAL :: PAROPENED
  INTEGER(KIND=JPIB_K) :: STAT

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNIT_NOT_CONNECTED_TO_PAR_FILE = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_ERROR_CLOSING_PAR_FILE = 2_JPIB_K

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
  INQUIRE( UNIT=PARUNIT, OPENED=PAROPENED )
  PP_DEBUG_CRITICAL_COND_THROW( .NOT.PAROPENED, ERRFLAG_UNIT_NOT_CONNECTED_TO_PAR_FILE )

  ! Open the TOC file
  CLOSE( UNIT=PARUNIT, IOSTAT=STAT )
  PP_DEBUG_CRITICAL_COND_THROW( STAT.NE.0, ERRFLAG_ERROR_CLOSING_PAR_FILE)

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
    CASE (ERRFLAG_UNIT_NOT_CONNECTED_TO_PAR_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unit not connected to a par file' )
    CASE (ERRFLAG_ERROR_CLOSING_PAR_FILE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Error closing par file' )
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

END FUNCTION PAR_CLOSE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PAR_WRITE'
PP_THREAD_SAFE FUNCTION PAR_WRITE( DATA, UNIT, HOOKS ) RESULT(RET)

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
  TYPE(MODEL_PAR_T),    INTENT(IN)    :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_SIM = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_GEO = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_WAM = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_WRITE_SAT = 4_JPIB_K

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

  ! Call the nested write procedures
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_SIM) WRITE_SIM_PAR( DATA%SIM_, UNIT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_GEO) WRITE_GEO_PAR( DATA%GEO_, UNIT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_WAM) WRITE_WAM_PAR( DATA%WAM_, UNIT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_WRITE_SAT) WRITE_SAT_PAR( DATA%SAT_, UNIT, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_WRITE_SIM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to write sim_par' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_GEO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to write geo_par' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to write wam_par' )
    CASE (ERRFLAG_UNABLE_TO_WRITE_SAT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to write sat_par' )
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

END FUNCTION PAR_WRITE
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PAR_PRINT'
PP_THREAD_SAFE FUNCTION PAR_PRINT( DATA, UNIT, HOOKS ) RESULT(RET)

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
  TYPE(MODEL_PAR_T),    INTENT(IN)    :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  !> Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_SIM = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_GEO = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_WAM = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT_SAT = 4_JPIB_K

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

  ! Call the nested print procedures
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_SIM) PRINT_SIM_PAR( DATA%SIM_, UNIT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_GEO) PRINT_GEO_PAR( DATA%GEO_, UNIT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_WAM) PRINT_WAM_PAR( DATA%WAM_, UNIT, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_PRINT_SAT) PRINT_SAT_PAR( DATA%SAT_, UNIT, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_PRINT_SIM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to print sim_par' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_GEO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to print geo_par' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to print wam_par' )
    CASE (ERRFLAG_UNABLE_TO_PRINT_SAT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to print sat_par' )
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

END FUNCTION PAR_PRINT
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PAR_READ'
PP_THREAD_SAFE FUNCTION PAR_READ( DATA, UNIT, VERBOSE, HOOKS ) RESULT(RET)

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
  TYPE(MODEL_PAR_T),    INTENT(INOUT) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  LOGICAL,              INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  !> Local error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SIM = 1_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_GEO = 2_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_WAM = 3_JPIB_K
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_READ_SAT = 4_JPIB_K

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

  ! Call read procedures
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SIM) READ_SIM_PAR( DATA%SIM_, UNIT, VERBOSE, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_GEO) READ_GEO_PAR( DATA%GEO_, UNIT, VERBOSE, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_WAM) READ_WAM_PAR( DATA%WAM_, UNIT, VERBOSE, HOOKS )
  PP_TRYCALL(ERRFLAG_UNABLE_TO_READ_SAT) READ_SAT_PAR( DATA%SAT_, UNIT, VERBOSE, HOOKS )

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
    CASE (ERRFLAG_UNABLE_TO_READ_SIM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read sim_par' )
    CASE (ERRFLAG_UNABLE_TO_READ_GEO)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read geo_par' )
    CASE (ERRFLAG_UNABLE_TO_READ_WAM)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read wam_par' )
    CASE (ERRFLAG_UNABLE_TO_READ_SAT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to read sat_par' )
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

END FUNCTION PAR_READ
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_SIM_PAR'
PP_THREAD_SAFE FUNCTION PRINT_SIM_PAR( DATA, UNIT, HOOKS ) RESULT(RET)

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
  TYPE(SIM_PAR_T),      INTENT(IN)    :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: IOERR

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT = 1_JPIB_K

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

  ! Print the simulation parameters
  WRITE(UNIT,*,IOSTAT=IOERR) ' '
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' SIMULATION PARAMETERS'
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' ---------------------'
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%IGRIB2_TABLES_VERSION_LATEST :: ', DATA%IGRIB2_TABLES_VERSION_LATEST
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NCYCLE...................... :: ', DATA%NCYCLE
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NLOCGRB..................... :: ', DATA%NLOCGRB
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NBITSSHLNSP................. :: ', DATA%NBITSSHLNSP
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NBITSEXPR................... :: ', DATA%NBITSEXPR
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%CTYPE....................... :: ', DATA%CTYPE
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%CFCLASS..................... :: ', DATA%CFCLASS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NLEG........................ :: ', DATA%NLEG
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NTOTENS..................... :: ', DATA%NTOTENS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NENSFNB..................... :: ', DATA%NENSFNB
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NWINSIZE.................... :: ', DATA%NWINSIZE
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NWINOFF..................... :: ', DATA%NWINOFF
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NJDIAG...................... :: ', DATA%NJDIAG
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NJDOMAI..................... :: ', DATA%NJDOMAI
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NJITER...................... :: ', DATA%NJITER
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NSTREAM..................... :: ', DATA%NSTREAM
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NSYSTEM..................... :: ', DATA%NSYSTEM
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NMETHOD..................... :: ', DATA%NMETHOD
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NREFERENCE.................. :: ', DATA%NREFERENCE
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NCONSENSUS.................. :: ', DATA%NCONSENSUS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NDWD........................ :: ', DATA%NDWD
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NMFR........................ :: ', DATA%NMFR
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NNCEP....................... :: ', DATA%NNCEP
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NUKM........................ :: ', DATA%NUKM
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NINDAT...................... :: ', DATA%NINDAT
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NSSSSS...................... :: ', DATA%NSSSSS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%RTIMST...................... :: ', DATA%RTIMST
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NCONF....................... :: ', DATA%NCONF
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%LPPSTEPS.................... :: ', DATA%LPPSTEPS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%TSTEP....................... :: ', DATA%TSTEP
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NSTEPINI.................... :: ', DATA%NSTEPINI
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%LOBSC1...................... :: ', DATA%LOBSC1
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%CNMEXP...................... :: ', DATA%CNMEXP
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%LVAREPS..................... :: ', DATA%LVAREPS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NFCHO_TRUNC_INI............. :: ', DATA%NFCHO_TRUNC_INI
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%LDMCC04..................... :: ', DATA%LDMCC04
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NPROC_IO.................... :: ', DATA%NPROC_IO
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to print' )
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

END FUNCTION PRINT_SIM_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WRITE_SIM_PAR'
PP_THREAD_SAFE FUNCTION WRITE_SIM_PAR(DATA, UNIT, HOOKS ) RESULT(RET)

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
  TYPE(SIM_PAR_T),      INTENT(IN)    :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT

  !> Error flags
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

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IGRIB2_TABLES_VERSION_LATEST, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NCYCLE, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NLOCGRB, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NBITSSHLNSP, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NBITSEXPR, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%CTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%CFCLASS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NLEG, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NTOTENS, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NENSFNB, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  ! WRITE(UNIT, IOSTAT=STAT) INT( DATA%NWINSIZE, INT64)
  ! PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NWINOFF, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NJDIAG, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NJDOMAI, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NJITER, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSTREAM, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSYSTEM, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NMETHOD, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NREFERENCE, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NCONSENSUS, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NDWD, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NMFR, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NNCEP, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NUKM, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NINDAT, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSSSSS, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%RTIMST, REAL64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NCONF, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%LPPSTEPS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%TSTEP, REAL64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSTEPINI, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%LOBSC1
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%CNMEXP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%LVAREPS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NFCHO_TRUNC_INI, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%LDMCC04
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%NPROC_IO
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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write simulation parameters' )
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

END FUNCTION WRITE_SIM_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE



#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_TABLE_VERSION_PAR'
PP_THREAD_SAFE FUNCTION READ_TABLE_VERSION_PAR( UNIT, TABLE_VERSION, HOOKS ) RESULT(RET)

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
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  INTEGER(KIND=JPIB_K), INTENT(OUT)   :: TABLE_VERSION
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=INT64)  :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT

  !> Error flags
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

  ! Read the table version
  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  TABLE_VERSION = INT( ITMP, KIND(TABLE_VERSION))

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
    CASE (ERRFLAG_UNABLE_TO_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to Read simulation parameters' )
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

END FUNCTION READ_TABLE_VERSION_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE




#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_SIM_PAR'
PP_THREAD_SAFE FUNCTION READ_SIM_PAR( DATA, UNIT, VERBOSE, HOOKS ) RESULT(RET)

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
  TYPE(SIM_PAR_T),      INTENT(INOUT) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  LOGICAL,              INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=INT64)  :: ITMP
  REAL(KIND=REAL64)    :: RTMP
  INTEGER(KIND=JPIB_K) :: STAT

  !> Error flags
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


  READ(UNIT, IOSTAT=STAT) ITMP
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%TABLE_VERSION_LATEST: ', ITMP, STAT
  ENDIF
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  DATA%IGRIB2_TABLES_VERSION_LATEST = INT( ITMP, KIND(DATA%IGRIB2_TABLES_VERSION_LATEST))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NCYCLE: ', ITMP, STAT
  ENDIF
  DATA%NCYCLE = INT( ITMP, KIND(DATA%NCYCLE))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NLOCGRB: ', ITMP, STAT
  ENDIF
  DATA%NLOCGRB = INT( ITMP, KIND(DATA%NLOCGRB))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%:NBITSSHLNSP ', ITMP, STAT
  ENDIF
  DATA%NBITSSHLNSP = INT( ITMP, KIND(DATA%NBITSSHLNSP))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%:NBITSEXPR ', ITMP, STAT
  ENDIF
  DATA%NBITSEXPR = INT( ITMP, KIND(DATA%NBITSEXPR))



  READ(UNIT, IOSTAT=STAT) DATA%CTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%CTYPE: ',  DATA%CTYPE, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%CFCLASS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%CFCLASS: ', DATA%CFCLASS, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NLEG: ', ITMP, STAT
  ENDIF
  DATA%NLEG = INT( ITMP, KIND(DATA%NLEG))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NTOTENS: ', ITMP, STAT
  ENDIF
  DATA%NTOTENS = INT( ITMP, KIND(DATA%NTOTENS))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NENSFNB: ', ITMP, STAT
  ENDIF
  DATA%NENSFNB = INT( ITMP, KIND(DATA%NENSFNB))

  ! READ(UNIT, IOSTAT=STAT) ITMP
  ! PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  ! IF ( VERBOSE ) THEN
  !   WRITE(*,*) ' + PAR%SIM_%NWINSIZE: ', ITMP, STAT
  ! ENDIF
  ! DATA%NWINSIZE = INT( ITMP, KIND(DATA%NWINSIZE))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NWINOFF: ', ITMP, STAT
  ENDIF
  DATA%NWINOFF = INT( ITMP, KIND(DATA%NWINOFF))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NJDIAG: ', ITMP, STAT
  ENDIF
  DATA%NJDIAG = INT( ITMP, KIND(DATA%NJDIAG))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NJDOMAI: ', ITMP, STAT
  ENDIF
  DATA%NJDOMAI = INT( ITMP, KIND(DATA%NJDOMAI))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NJITER: ', ITMP, STAT
  ENDIF
  DATA%NJITER = INT( ITMP, KIND(DATA%NJITER))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NSTREAM: ', ITMP, STAT
  ENDIF
  DATA%NSTREAM = INT( ITMP, KIND(DATA%NSTREAM))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NSYSTEM: ', ITMP, STAT
  ENDIF
  DATA%NSYSTEM = INT( ITMP, KIND(DATA%NSYSTEM))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NMETHOD: ', ITMP, STAT
  ENDIF
  DATA%NMETHOD = INT( ITMP, KIND(DATA%NMETHOD))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NREFERENCE: ', ITMP, STAT
  ENDIF
  DATA%NREFERENCE = INT( ITMP, KIND(DATA%NREFERENCE))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NCONSENSUS: ', ITMP, STAT
  ENDIF
  DATA%NCONSENSUS = INT( ITMP, KIND(DATA%NCONSENSUS))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NDWD: ', ITMP, STAT
  ENDIF
  DATA%NDWD = INT( ITMP, KIND(DATA%NDWD))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NMFR: ', ITMP, STAT
  ENDIF
  DATA%NMFR = INT( ITMP, KIND(DATA%NMFR))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NNCEP: ', ITMP, STAT
  ENDIF
  DATA%NNCEP = INT( ITMP, KIND(DATA%NNCEP))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NUKM: ', ITMP, STAT
  ENDIF
  DATA%NUKM = INT( ITMP, KIND(DATA%NUKM))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NINDAT: ', ITMP, STAT
  ENDIF
  DATA%NINDAT = INT( ITMP, KIND(DATA%NINDAT))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NSSSSS: ', ITMP, STAT
  ENDIF
  DATA%NSSSSS = INT( ITMP, KIND(DATA%NSSSSS))

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%RTIMST: ', RTMP, STAT
  ENDIF
  DATA%RTIMST = REAL( RTMP, KIND(DATA%RTIMST))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NCONF: ', ITMP, STAT
  ENDIF
  DATA%NCONF = INT( ITMP, KIND(DATA%NCONF))

  READ(UNIT, IOSTAT=STAT) DATA%LPPSTEPS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%LPPSTEPS: ', DATA%LPPSTEPS, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%TSTEP: ', RTMP, STAT
  ENDIF
  DATA%TSTEP = REAL( RTMP, KIND(DATA%TSTEP) )


  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NSTEPINI: ', ITMP, STAT
  ENDIF
  DATA%NSTEPINI = INT( ITMP, KIND(DATA%NSTEPINI))

  READ(UNIT, IOSTAT=STAT) DATA%LOBSC1
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%LOBSC1: ', DATA%LOBSC1, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%CNMEXP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%CNMEXP: ', DATA%CNMEXP, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%LVAREPS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%LVAREPS: ', DATA%LVAREPS, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NFCHO_TRUNC_INI: ', ITMP, STAT
  ENDIF
  DATA%NFCHO_TRUNC_INI = INT( ITMP, KIND(DATA%NFCHO_TRUNC_INI))

  READ(UNIT, IOSTAT=STAT) DATA%LDMCC04
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%LDMCC04: ', DATA%LDMCC04, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SIM_%NPROC_IO: ', ITMP, STAT
  ENDIF
  DATA%NPROC_IO = INT( ITMP, KIND(DATA%NPROC_IO))

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
    CASE (ERRFLAG_UNABLE_TO_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to Read simulation parameters' )
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

END FUNCTION READ_SIM_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_GEO_PAR'
PP_THREAD_SAFE FUNCTION PRINT_GEO_PAR( DATA, UNIT, HOOKS ) RESULT(RET)

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
  TYPE(GEO_PAR_T),      INTENT(IN)    :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: IOERR

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT = 1_JPIB_K

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

  WRITE(UNIT,*,IOSTAT=IOERR) ' '
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' GEOMETRY PARAMETERS'
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' -------------------'
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + GEOMETRY_PARAMS%JPMXLE........................ :: ', DATA%JPMXLE
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + GEOMETRY_PARAMS%JPMXGL........................ :: ', DATA%JPMXGL
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + GEOMETRY_PARAMS%ISMAX......................... :: ', DATA%ISMAX
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + GEOMETRY_PARAMS%ILATS......................... :: ', DATA%ILATS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + GEOMETRY_PARAMS%ILONS......................... :: ', DATA%ILONS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + GEOMETRY_PARAMS%IDGNH......................... :: ', DATA%IDGNH
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + GEOMETRY_PARAMS%ZNLAT......................... :: ', DATA%ZNLAT
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + GEOMETRY_PARAMS%ZSLAT......................... :: ', DATA%ZSLAT
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + GEOMETRY_PARAMS%IFLEV......................... :: ', DATA%IFLEV
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + ALLOCATED(GEOMETRY_PARAMS%ZVERT).............. :: ', ALLOCATED(DATA%ZVERT)
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  FLUSH(UNIT)

  IF ( ALLOCATED(DATA%ZVERT) ) THEN
    WRITE(UNIT,*,IOSTAT=IOERR) ' + LBOUND(GEOMETRY_PARAMS%ZVERT,1)............... :: ', LBOUND(DATA%ZVERT,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    WRITE(UNIT,*,IOSTAT=IOERR) ' + UBOUND(GEOMETRY_PARAMS%ZVERT,1)............... :: ', UBOUND(DATA%ZVERT,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    DO I = LBOUND(DATA%ZVERT,1), UBOUND(DATA%ZVERT,1)
      IF ( ABS(DATA%ZVERT(I)) .LT. 1.0E-12_JPRD_K ) THEN
        WRITE(UNIT,'(A,I6,A,A)',IOSTAT=IOERR) ' + GEOMETRY_PARAMS%ZVERT(',I,').............. :: ', 'NULL'
        PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
        FLUSH(UNIT)
      ELSE
        WRITE(UNIT,'(A,I6,A,F32.14)',IOSTAT=IOERR) ' + GEOMETRY_PARAMS%ZVERT(',I,').............. :: ', DATA%ZVERT(I)
        ! PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
        FLUSH(UNIT)
      ENDIF
    ENDDO
  ENDIF


  WRITE(UNIT,*,IOSTAT=IOERR) ' + GEOMETRY_PARAMS%NHTYP......................... :: ', DATA%NHTYP
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + GEOMETRY_PARAMS%NSTTYP........................ :: ', DATA%NSTTYP
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + GEOMETRY_PARAMS%RMUCEN........................ :: ', DATA%RMUCEN
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + GEOMETRY_PARAMS%RLOCEN........................ :: ', DATA%RLOCEN
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + GEOMETRY_PARAMS%RSTRET........................ :: ', DATA%RSTRET
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)
  WRITE(UNIT,*,IOSTAT=IOERR) ' + ALLOCATED(GEOMETRY_PARAMS%ILOENG)............. :: ', ALLOCATED(DATA%ILOENG)
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  FLUSH(UNIT)

  IF ( ALLOCATED(DATA%ILOENG) ) THEN
    WRITE(UNIT,*,IOSTAT=IOERR) ' + LBOUND(GEOMETRY_PARAMS%ILOENG,1).............. :: ', LBOUND(DATA%ILOENG,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    FLUSH(UNIT)
    WRITE(UNIT,*,IOSTAT=IOERR) ' + UBOUND(GEOMETRY_PARAMS%ILOENG,1).............. :: ', UBOUND(DATA%ILOENG,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    FLUSH(UNIT)
    DO I = LBOUND(DATA%ILOENG,1), UBOUND(DATA%ILOENG,1)
      WRITE(UNIT,'(A,I6,A,I8)',IOSTAT=IOERR) ' +     GEOMETRY_PARAMS%ILOENG(',I ,').. : ', DATA%ILOENG(I)
      PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
      FLUSH(UNIT)
    ENDDO
  ENDIF

  WRITE(UNIT,*,IOSTAT=IOERR) ' + ALLOCATED(GEOMETRY_PARAMS%NSFLEVS)............ :: ', ALLOCATED(DATA%NSFLEVS)
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  IF ( ALLOCATED(DATA%NSFLEVS) ) THEN
    WRITE(UNIT,*,IOSTAT=IOERR) ' + LBOUND(GEOMETRY_PARAMS%NSFLEVS,1)............. :: ', LBOUND(DATA%NSFLEVS,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    WRITE(UNIT,*,IOSTAT=IOERR) ' + UBOUND(GEOMETRY_PARAMS%NSFLEVS,1)............. :: ', UBOUND(DATA%NSFLEVS,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    WRITE(UNIT,*,IOSTAT=IOERR) ' + LBOUND(GEOMETRY_PARAMS%NSFLEVS,2)............. :: ', LBOUND(DATA%NSFLEVS,2)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    WRITE(UNIT,*,IOSTAT=IOERR) ' + UBOUND(GEOMETRY_PARAMS%NSFLEVS,2)............. :: ', UBOUND(DATA%NSFLEVS,2)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )

    DO I = LBOUND(DATA%NSFLEVS,1), UBOUND(DATA%NSFLEVS,1)
      DO J = LBOUND(DATA%NSFLEVS,2), UBOUND(DATA%NSFLEVS,2)
        WRITE(UNIT,'(A,I3,A,I3,A,I8)',IOSTAT=IOERR) ' + GEOMETRY_PARAMS%NSFLEVS(',I,',', J,')....... :: ', DATA%NSFLEVS(I,J)
        PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
      ENDDO
    ENDDO
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
    CASE (ERRFLAG_UNABLE_TO_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to print geometry parameters' )
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

END FUNCTION PRINT_GEO_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WRITE_GEO_PAR'
PP_THREAD_SAFE FUNCTION WRITE_GEO_PAR( DATA, UNIT, HOOKS ) RESULT(RET)

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
  TYPE(GEO_PAR_T),      INTENT(IN)    :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J

  !> Error flags
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

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%JPMXLE, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%JPMXGL, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ISMAX, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ILATS, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ILONS, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IDGNH, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )


  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%ZNLAT, REAL64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%ZSLAT, REAL64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IFLEV, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )



  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%ZVERT)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  IF ( ALLOCATED(DATA%ZVERT) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%ZVERT,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%ZVERT,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    DO I = LBOUND(DATA%ZVERT,1), UBOUND(DATA%ZVERT,1)
      WRITE(UNIT, IOSTAT=STAT) REAL( DATA%ZVERT(I), REAL64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    ENDDO
  ENDIF


  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NHTYP, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSTTYP, INT64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%RMUCEN, REAL64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%RLOCEN, REAL64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%RSTRET, REAL64)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )


  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%ILOENG)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  IF ( ALLOCATED(DATA%ILOENG) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%ILOENG,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%ILOENG,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    DO I = LBOUND(DATA%ILOENG,1), UBOUND(DATA%ILOENG,1)
      WRITE(UNIT, IOSTAT=STAT) INT( DATA%ILOENG(I), INT64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    ENDDO
  ENDIF



  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%NSFLEVS)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  IF ( ALLOCATED(DATA%NSFLEVS) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%NSFLEVS,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%NSFLEVS,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%NSFLEVS,2), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%NSFLEVS,2), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

    DO I = LBOUND(DATA%NSFLEVS,1), UBOUND(DATA%NSFLEVS,1)
      DO J = LBOUND(DATA%NSFLEVS,2), UBOUND(DATA%NSFLEVS,2)
        WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSFLEVS(I,J), INT64)
        PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
      ENDDO
    ENDDO
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

    CASE (ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write geometry parameters' )
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

END FUNCTION WRITE_GEO_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_GEO_PAR'
PP_THREAD_SAFE FUNCTION READ_GEO_PAR( DATA, UNIT, VERBOSE, HOOKS ) RESULT(RET)

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
  TYPE(GEO_PAR_T),      INTENT(INOUT) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  LOGICAL,              INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  REAL(KIND=REAL64)    :: RTMP
  INTEGER(KIND=INT64)  :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=INT64) :: LB1
  INTEGER(KIND=INT64) :: LB2
  INTEGER(KIND=INT64) :: UB1
  INTEGER(KIND=INT64) :: UB2
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  LOGICAL :: IS_ZVERT_ALLOCATED
  LOGICAL :: IS_ILOENG_ALLOCATED
  LOGICAL :: IS_NSFLEVS_ALLOCATED

  !> Error flags
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

  IF ( ALLOCATED(DATA%ZVERT) ) THEN
    DEALLOCATE(DATA%ZVERT)
  ENDIF
  IF ( ALLOCATED(DATA%ILOENG) ) THEN
    DEALLOCATE(DATA%ILOENG)
  ENDIF
  IF ( ALLOCATED(DATA%NSFLEVS) ) THEN
    DEALLOCATE(DATA%NSFLEVS)
  ENDIF

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%JPMXLE: ', ITMP, STAT
  ENDIF
  DATA%JPMXLE = INT( ITMP, KIND(DATA%JPMXLE))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%JPMXGL: ', ITMP, STAT
  ENDIF
  DATA%JPMXGL = INT( ITMP, KIND(DATA%JPMXGL))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%ISMAX: ', ITMP, STAT
  ENDIF
  DATA%ISMAX = INT( ITMP, KIND(DATA%ISMAX))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%ILATS: ', ITMP, STAT
  ENDIF
  DATA%ILATS = INT( ITMP, KIND(DATA%ILATS))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%ILONS: ', ITMP, STAT
  ENDIF
  DATA%ILONS = INT( ITMP, KIND(DATA%ILONS))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%IDGNH: ', ITMP, STAT
  ENDIF
  DATA%IDGNH = INT( ITMP, KIND(DATA%IDGNH))

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%ZNLAT: ', RTMP, STAT
  ENDIF
  DATA%ZNLAT = REAL( RTMP, KIND(DATA%ZNLAT))

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%ZSLAT: ', RTMP, STAT
  ENDIF
  DATA%ZSLAT = REAL( RTMP, KIND(DATA%ZSLAT))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%IFLEV: ', ITMP, STAT
  ENDIF
  DATA%IFLEV = INT( ITMP, KIND(DATA%IFLEV))

  READ(UNIT, IOSTAT=STAT) IS_ZVERT_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%IS_ZVERT_ALLOCATED: ', IS_ZVERT_ALLOCATED, STAT
  ENDIF

  IF ( IS_ZVERT_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%UB1: ', UB1, STAT
    ENDIF
    ALLOCATE( DATA%ZVERT(LB1:UB1), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    DO I = LBOUND(DATA%ZVERT,1), UBOUND(DATA%ZVERT,1)
      READ(UNIT, IOSTAT=STAT) RTMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
      IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%GEO_%ZVERT(', I ,'): ', RTMP, STAT
      ENDIF
      DATA%ZVERT(I) = REAL( RTMP, KIND(DATA%ZVERT(I)))
    ENDDO
  ENDIF

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%NHTYP: ', ITMP, STAT
  ENDIF
  DATA%NHTYP = INT( ITMP, KIND(DATA%NHTYP))

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%NSTTYP: ', ITMP, STAT
  ENDIF
  DATA%NSTTYP = INT( ITMP, KIND(DATA%NSTTYP))

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%RMUCEN: ', RTMP, STAT
  ENDIF
  DATA%RMUCEN = REAL( RTMP, KIND(DATA%RMUCEN))

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%RLOCEN: ', RTMP, STAT
  ENDIF
  DATA%RLOCEN = REAL( RTMP, KIND(DATA%RLOCEN))

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%RSTRET: ', RTMP, STAT
  ENDIF
  DATA%RSTRET = REAL( RTMP, KIND(DATA%RSTRET))


  READ(UNIT, IOSTAT=STAT) IS_ILOENG_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%IS_ILOENG_ALLOCATED: ', IS_ILOENG_ALLOCATED, STAT
  ENDIF

  IF ( IS_ILOENG_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%ITMP: ', ITMP, STAT
    ENDIF
    ALLOCATE( DATA%ILOENG(LB1:UB1), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    DO I = LBOUND(DATA%ILOENG,1), UBOUND(DATA%ILOENG,1)
      READ(UNIT, IOSTAT=STAT) ITMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
      IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%GEO_%ILOENG(', I ,'): ', ITMP, STAT
      ENDIF
      DATA%ILOENG(I) = INT( ITMP, KIND(DATA%ILOENG(I)))
    ENDDO
  ENDIF

  READ(UNIT, IOSTAT=STAT) IS_NSFLEVS_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%GEO_%IS_NSFLEVS_ALLOCATED: ', IS_NSFLEVS_ALLOCATED, STAT
  ENDIF

  IF ( IS_NSFLEVS_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%UB1: ', UB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) LB2
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%LB2: ', LB2, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB2
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%GEO_%UB2: ', UB2, STAT
    ENDIF
    ALLOCATE( DATA%NSFLEVS(LB1:UB1, LB2:UB2 ), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    DO I = LBOUND(DATA%NSFLEVS,1), UBOUND(DATA%NSFLEVS,1)
      DO J = LBOUND(DATA%NSFLEVS,2), UBOUND(DATA%NSFLEVS,2)
        READ(UNIT, IOSTAT=STAT) ITMP
        PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
        IF ( VERBOSE ) THEN
          WRITE(*,*) ' + PAR%GEO_%NSFLEVS(', I,', ', J,'): ', ITMP, STAT
        ENDIF
        DATA%NSFLEVS(I,J) = INT( ITMP, KIND(DATA%NSFLEVS(I,J)))
      ENDDO
    ENDDO
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

    CASE (ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read geometry parameters' )
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

END FUNCTION READ_GEO_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WRITE_WAM_PAR'
PP_THREAD_SAFE FUNCTION WRITE_WAM_PAR(DATA, UNIT, HOOKS ) RESULT(RET)

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
  TYPE(WAM_PAR_T),      INTENT(IN)    :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: I

  !> Error flags
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


  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%FR)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  IF ( ALLOCATED(DATA%FR) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%FR,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%FR,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    DO I = LBOUND(DATA%FR,1), UBOUND(DATA%FR,1)
      WRITE(UNIT, IOSTAT=STAT) REAL( DATA%FR(I), REAL64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    ENDDO
  ENDIF


  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%TH)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  IF ( ALLOCATED(DATA%TH) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%TH,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%TH,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    DO I = LBOUND(DATA%TH,1), UBOUND(DATA%TH,1)
      WRITE(UNIT, IOSTAT=STAT) REAL( DATA%TH(I), REAL64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    ENDDO
  ENDIF

  WRITE(UNIT, IOSTAT=STAT) DATA%LWCOUSAMEGRID
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%CLDOMAIN
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NGX, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NGY, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NANG, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NFRE_RED, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IMDLGRBID_G, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IMDLGRBID_M, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NDATE_TIME_WINDOW_END, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NWINOFF, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NGRIB_VERSION, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NTENCODE, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NGRBRESI, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NGRBRESS, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )



  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%PPMISS, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%PPEPS, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%PPREC, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%PPRESOL, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%PPMIN_RESET, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )


  WRITE(UNIT, IOSTAT=STAT) DATA%HOPERI
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%HOPERS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%LGRHDIFS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%LNEWLVTP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%LPADPOLES
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%LL_GRID_SIMPLE_MATRIX
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )




  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IRGG, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IQGAUSS, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )


  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%AMOWEP, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%AMOSOP, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%AMOEAP, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%AMONOP, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%XDELLA, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%XDELLO, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )


  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%NLONRGG)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  IF ( ALLOCATED(DATA%NLONRGG) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%NLONRGG,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%NLONRGG,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    DO I = LBOUND(DATA%NLONRGG,1), UBOUND(DATA%NLONRGG,1)
      WRITE(UNIT, IOSTAT=STAT) INT( DATA%NLONRGG(I), INT64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    ENDDO
  ENDIF

  WRITE(UNIT, IOSTAT=STAT) REAL( DATA%ZMISS, REAL64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NENSFNB, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NTOTENS, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSYSNB, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NMETNB, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ISTREAM, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NLOCGRB, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NCONSENSUS, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NDWD, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NMFR, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NNCEP, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NUKM, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%IREFDATE, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )


  WRITE(UNIT, IOSTAT=STAT) DATA%MARSTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%YCLASS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) DATA%YEXPVER
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )



  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSPEC2TAB, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSPEC2TMPD, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NSPEC2TMPP, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NTRG2TMPD, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%NTRG2TMPP, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )


  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ITMIN, INT64 )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )

  WRITE(UNIT, IOSTAT=STAT) INT( DATA%ITMAX, INT64 )
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

    ! Local variables
    CHARACTER(LEN=32) :: TMP1

    ! Error handling variables
    PP_DEBUG_PUSH_FRAME()

    ! Handle different errors
    SELECT CASE(ERRIDX)
    CASE (ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write wave parameters' )
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

END FUNCTION WRITE_WAM_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_WAM_PAR'
PP_THREAD_SAFE FUNCTION PRINT_WAM_PAR( DATA, UNIT, HOOKS ) RESULT(RET)

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
  TYPE(WAM_PAR_T),      INTENT(IN)    :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: IOERR

  ! Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT = 1_JPIB_K

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

  WRITE(UNIT,*,IOSTAT=IOERR) ' '
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' WAM PARAMETERS'
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' --------------'
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + ALLOCATED(DATA%FR)............................ :: ', ALLOCATED(DATA%FR)
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )

  IF ( ALLOCATED(DATA%FR) ) THEN
    WRITE(UNIT,*,IOSTAT=IOERR) ' + LBOUND(SIMULATION_PARAMS%FR,1)................ :: ', LBOUND(DATA%FR,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    WRITE(UNIT,*,IOSTAT=IOERR) ' + UBOUND(SIMULATION_PARAMS%FR,1)................ :: ', UBOUND(DATA%FR,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    DO I = LBOUND(DATA%FR,1), UBOUND(DATA%FR,1)
      WRITE(UNIT,'(A,I6,A,F11.4)',IOSTAT=IOERR) ' + SIMULATION_PARAMS%FR(',I,')............... :: ', DATA%FR(I)
      PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    ENDDO
  ENDIF

  WRITE(UNIT,*,IOSTAT=IOERR) ALLOCATED(DATA%TH)
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )

  IF ( ALLOCATED(DATA%TH) ) THEN
    WRITE(UNIT,*,IOSTAT=IOERR) ' + LBOUND(SIMULATION_PARAMS%TH,1)................ :: ', LBOUND(DATA%TH,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    WRITE(UNIT,*,IOSTAT=IOERR) ' + UBOUND(SIMULATION_PARAMS%TH,1)................ :: ', UBOUND(DATA%TH,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    DO I = LBOUND(DATA%TH,1), UBOUND(DATA%TH,1)
      WRITE(UNIT,'(A,I6,A,F11.4)',IOSTAT=IOERR) ' + SIMULATION_PARAMS%TH(', I,')..................... : ', DATA%TH(I)
      PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    ENDDO
  ENDIF

  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%LWCOUSAMEGRID............... :: ', DATA%LWCOUSAMEGRID
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%CLDOMAIN.................... :: ', DATA%CLDOMAIN
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NGX......................... :: ', DATA%NGX
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NGY......................... :: ', DATA%NGY
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NANG........................ :: ', DATA%NANG
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NFRE_RED.................... :: ', DATA%NFRE_RED
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%IMDLGRBID_G................. :: ', DATA%IMDLGRBID_G
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%IMDLGRBID_M................. :: ', DATA%IMDLGRBID_M
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NDATE_TIME_WINDOW_END....... :: ', DATA%NDATE_TIME_WINDOW_END
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NWINOFF..................... :: ', DATA%NWINOFF
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NGRIB_VERSION............... :: ', DATA%NGRIB_VERSION
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NTENCODE.................... :: ', DATA%NTENCODE
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NGRBRESI.................... :: ', DATA%NGRBRESI
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NGRBRESS.................... :: ', DATA%NGRBRESS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%PPMISS...................... :: ', DATA%PPMISS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%PPEPS....................... :: ', DATA%PPEPS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%PPREC....................... :: ', DATA%PPREC
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%PPRESOL..................... :: ', DATA%PPRESOL
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%PPMIN_RESET................. :: ', DATA%PPMIN_RESET
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%HOPERI...................... :: ', DATA%HOPERI
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%HOPERS...................... :: ', DATA%HOPERS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%LGRHDIFS.................... :: ', DATA%LGRHDIFS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%LNEWLVTP.................... :: ', DATA%LNEWLVTP
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%LPADPOLES................... :: ', DATA%LPADPOLES
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%LL_GRID_SIMPLE_MATRIX....... :: ', DATA%LL_GRID_SIMPLE_MATRIX
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%IRGG........................ :: ', DATA%IRGG
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%IQGAUSS..................... :: ', DATA%IQGAUSS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%AMOWEP...................... :: ', DATA%AMOWEP
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%AMOSOP...................... :: ', DATA%AMOSOP
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%AMOEAP...................... :: ', DATA%AMOEAP
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%AMONOP...................... :: ', DATA%AMONOP
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%XDELLA...................... :: ', DATA%XDELLA
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%XDELLO...................... :: ', DATA%XDELLO
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + ALLOCATED(SIMULATION_PARAMS%NLONRGG).......... :: ', ALLOCATED(DATA%NLONRGG)
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )

  IF ( ALLOCATED(DATA%NLONRGG) ) THEN
    WRITE(UNIT,*,IOSTAT=IOERR) ' + LBOUND(SIMULATION_PARAMS%NLONRGG,1)........... :: ', LBOUND(DATA%NLONRGG,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    WRITE(UNIT,*,IOSTAT=IOERR) ' + UBOUND(SIMULATION_PARAMS%NLONRGG,1)........... :: ', UBOUND(DATA%NLONRGG,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    DO I = LBOUND(DATA%NLONRGG,1), UBOUND(DATA%NLONRGG,1)
      WRITE(UNIT,'(A,I6,A,I8)',IOSTAT=IOERR) ' + SIMULATION_PARAMS%NLONRGG(',I,').......... :: ', DATA%NLONRGG(I)
      PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    ENDDO
  ENDIF

  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%ZMISS....................... :: ', DATA%ZMISS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NENSFNB..................... :: ', DATA%NENSFNB
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NTOTENS..................... :: ', DATA%NTOTENS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NSYSNB...................... :: ', DATA%NSYSNB
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NMETNB...................... :: ', DATA%NMETNB
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%ISTREAM..................... :: ', DATA%ISTREAM
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NLOCGRB..................... :: ', DATA%NLOCGRB
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NCONSENSUS.................. :: ', DATA%NCONSENSUS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NDWD........................ :: ', DATA%NDWD
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NMFR........................ :: ', DATA%NMFR
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NNCEP....................... :: ', DATA%NNCEP
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NUKM........................ :: ', DATA%NUKM
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%IREFDATE.................... :: ', DATA%IREFDATE
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%MARSTYPE.................... :: ', DATA%MARSTYPE
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%YCLASS...................... :: ', DATA%YCLASS
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%YEXPVER..................... :: ', DATA%YEXPVER
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NSPEC2TAB................... :: ', DATA%NSPEC2TAB
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NSPEC2TMPD.................. :: ', DATA%NSPEC2TMPD
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NSPEC2TMPP.................. :: ', DATA%NSPEC2TMPP
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NTRG2TMPD................... :: ', DATA%NTRG2TMPD
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%NTRG2TMPP................... :: ', DATA%NTRG2TMPP
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%ITMIN....................... :: ', DATA%ITMIN
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' + SIMULATION_PARAMS%ITMAX....................... :: ', DATA%ITMAX
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )

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
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to print wave parameters' )
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

END FUNCTION PRINT_WAM_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_WAM_PAR'
PP_THREAD_SAFE FUNCTION READ_WAM_PAR( DATA, UNIT, VERBOSE, HOOKS ) RESULT(RET)

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
  TYPE(WAM_PAR_T),      INTENT(INOUT) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  LOGICAL,              INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: I
  REAL(KIND=REAL64)   :: RTMP
  INTEGER(KIND=INT64) :: ITMP
  INTEGER(KIND=INT64) :: ILO
  INTEGER(KIND=INT64) :: IHI
  LOGICAL :: IS_ALLOCATED_FR
  LOGICAL :: IS_ALLOCATED_TH
  LOGICAL :: IS_ALLOCATED_NLONRGG

  !> Error flags
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


  READ(UNIT, IOSTAT=STAT) IS_ALLOCATED_FR
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IS_ALLOCATED_FR ', IS_ALLOCATED_FR, STAT
  ENDIF

  IF ( IS_ALLOCATED_FR ) THEN
    READ(UNIT, IOSTAT=STAT) ILO
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%WAM_%ILO: ', ILO, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) IHI
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%WAM_%IHI: ', IHI, STAT
    ENDIF
    IF ( ALLOCATED(DATA%FR) ) THEN
      DEALLOCATE(DATA%FR)
    ENDIF
    ALLOCATE( DATA%FR(ILO:IHI), STAT=STAT )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    DO I = LBOUND(DATA%FR,1), UBOUND(DATA%FR,1)
      READ(UNIT, IOSTAT=STAT) RTMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
      IF ( VERBOSE ) THEN
        WRITE(*,'(A,I8,A,F11.4,I8)') ' + PAR%WAM_%FR(', I, ')= ', RTMP, STAT
      ENDIF
      DATA%FR(I) = REAL( RTMP, KIND(DATA%FR(I)))
    ENDDO
  ENDIF


  READ(UNIT, IOSTAT=STAT) IS_ALLOCATED_TH
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IS_ALLOCATED_TH ', IS_ALLOCATED_TH, STAT
  ENDIF

  IF ( IS_ALLOCATED_TH ) THEN
    READ(UNIT, IOSTAT=STAT) ILO
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%WAM_%ILO: ', ILO, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) IHI
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%WAM_%IHI: ', IHI, STAT
    ENDIF
    IF ( ALLOCATED(DATA%TH) ) THEN
      DEALLOCATE(DATA%TH)
    ENDIF
    ALLOCATE( DATA%TH(ILO:IHI), STAT=STAT )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    DO I = LBOUND(DATA%TH,1), UBOUND(DATA%TH,1)
      READ(UNIT, IOSTAT=STAT) RTMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
      IF ( VERBOSE ) THEN
        WRITE(*,'(A,I8,A,F11.4,I8)') ' + PAR%WAM_%TH(', I, ')= ', RTMP, STAT
      ENDIF
      DATA%TH(I) = REAL( RTMP, KIND(DATA%TH(I)) )
    ENDDO
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%LWCOUSAMEGRID
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%LWCOUSAMEGRID: ', DATA%LWCOUSAMEGRID, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%CLDOMAIN
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%CLDOMAIN: ', DATA%CLDOMAIN, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NGX: ', ITMP, STAT
  ENDIF
  DATA%NGX = INT( ITMP, KIND(DATA%NGX) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NGY: ', ITMP, STAT
  ENDIF
  DATA%NGY = INT( ITMP, KIND(DATA%NGY) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NANG: ', ITMP, STAT
  ENDIF
  DATA%NANG = INT( ITMP, KIND(DATA%NANG) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NFRE_RED: ', ITMP, STAT
  ENDIF
  DATA%NFRE_RED = INT( ITMP, KIND(DATA%NFRE_RED) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IMDLGRBID_G: ', ITMP, STAT
  ENDIF
  DATA%IMDLGRBID_G = INT( ITMP, KIND(DATA%IMDLGRBID_G) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IMDLGRBID_M: ', ITMP, STAT
  ENDIF
  DATA%IMDLGRBID_M = INT( ITMP, KIND(DATA%IMDLGRBID_M) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NDATE_TIME_WINDOW_END: ', ITMP, STAT
  ENDIF
  DATA%NDATE_TIME_WINDOW_END = INT( ITMP, KIND(DATA%NDATE_TIME_WINDOW_END) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NWINOFF: ', ITMP, STAT
  ENDIF
  DATA%NWINOFF = INT( ITMP, KIND(DATA%NWINOFF) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NGRIB_VERSION: ', ITMP, STAT
  ENDIF
  DATA%NGRIB_VERSION = INT( ITMP, KIND(DATA%NGRIB_VERSION) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NTENCODE: ', ITMP, STAT
  ENDIF
  DATA%NTENCODE = INT( ITMP, KIND(DATA%NTENCODE) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NGRBRESI: ', ITMP, STAT
  ENDIF
  DATA%NGRBRESI = INT( ITMP, KIND(DATA%NGRBRESI) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NGRBRESS: ', ITMP, STAT
  ENDIF
  DATA%NGRBRESS = INT( ITMP, KIND(DATA%NGRBRESS) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%PPMISS: ', RTMP, STAT
  ENDIF
  DATA%PPMISS = REAL( RTMP, KIND(DATA%PPMISS) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%PPEPS: ', RTMP, STAT
  ENDIF
  DATA%PPEPS = REAL( RTMP, KIND(DATA%PPEPS) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%PPREC: ', RTMP, STAT
  ENDIF
  DATA%PPREC = REAL( RTMP, KIND(DATA%PPREC) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%PPRESOL: ', RTMP, STAT
  ENDIF
  DATA%PPRESOL = REAL( RTMP, KIND(DATA%PPRESOL) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%PPMIN_RESET: ', RTMP, STAT
  ENDIF
  DATA%PPMIN_RESET = REAL( RTMP, KIND(DATA%PPMIN_RESET) )

  READ(UNIT, IOSTAT=STAT) DATA%HOPERI
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%HOPERI: ', DATA%HOPERI, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%HOPERS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%HOPERS: ', DATA%HOPERS, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%LGRHDIFS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%LGRHDIFS: ', DATA%LGRHDIFS, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%LNEWLVTP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%LNEWLVTP: ', DATA%LNEWLVTP, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%LPADPOLES
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%LPADPOLES: ', DATA%LPADPOLES, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%LL_GRID_SIMPLE_MATRIX
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%LL_GRID_SIMPLE_MATRIX: ', DATA%LL_GRID_SIMPLE_MATRIX, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IRGG: ', ITMP, STAT
  ENDIF
  DATA%IRGG = INT( ITMP, KIND(DATA%IRGG) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IQGAUSS: ', ITMP, STAT
  ENDIF
  DATA%IQGAUSS = INT( ITMP, KIND(DATA%IQGAUSS) )


  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%AMOWEP: ', ITMP, STAT
  ENDIF
  DATA%AMOWEP = REAL( RTMP, KIND(DATA%AMOWEP) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%AMOSOP: ', RTMP, STAT
  ENDIF
  DATA%AMOSOP = REAL( RTMP, KIND(DATA%AMOSOP) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%AMOEAP: ', RTMP, STAT
  ENDIF
  DATA%AMOEAP = REAL( RTMP, KIND(DATA%AMOEAP) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%AMONOP: ', RTMP, STAT
  ENDIF
  DATA%AMONOP = REAL( RTMP, KIND(DATA%AMONOP) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%XDELLA: ', RTMP, STAT
  ENDIF
  DATA%XDELLA = REAL( RTMP, KIND(DATA%XDELLA) )

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%XDELLO: ', RTMP, STAT
  ENDIF
  DATA%XDELLO = REAL( RTMP, KIND(DATA%XDELLO) )

  READ(UNIT, IOSTAT=STAT) IS_ALLOCATED_NLONRGG
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IS_ALLOCATED_NLONRGG: ', IS_ALLOCATED_NLONRGG, STAT
  ENDIF

  IF ( IS_ALLOCATED_NLONRGG ) THEN
    READ(UNIT, IOSTAT=STAT) ILO
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%WAM_%ILO: ', ILO, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) IHI
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%WAM_%IHI: ', IHI, STAT
    ENDIF
    IF ( ALLOCATED(DATA%NLONRGG) ) THEN
      DEALLOCATE(DATA%NLONRGG)
    ENDIF
    ALLOCATE( DATA%NLONRGG(ILO:IHI), STAT=STAT )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    DO I = LBOUND(DATA%NLONRGG,1), UBOUND(DATA%NLONRGG,1)
      READ(UNIT, IOSTAT=STAT) ITMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
      IF ( VERBOSE ) THEN
        WRITE(*,'(A,I8,A,F11.4,I8)') ' + PAR%WAM_%NLONRGG(', I ,') = ', RTMP, STAT
      ENDIF
      DATA%NLONRGG(I) = INT( ITMP, KIND(DATA%NLONRGG(I)))
    ENDDO
  ENDIF

  READ(UNIT, IOSTAT=STAT) RTMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%ZMISS: ', RTMP, STAT
  ENDIF
  DATA%ZMISS = REAL( RTMP, KIND(DATA%ZMISS) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NENSFNB: ', ITMP, STAT
  ENDIF
  DATA%NENSFNB = INT( ITMP, KIND(DATA%NENSFNB) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NTOTENS: ', ITMP, STAT
  ENDIF
  DATA%NTOTENS = INT( ITMP, KIND(DATA%NTOTENS) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NSYSNB: ', ITMP, STAT
  ENDIF
  DATA%NSYSNB = INT( ITMP, KIND(DATA%NSYSNB) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NMETNB: ', ITMP, STAT
  ENDIF
  DATA%NMETNB = INT( ITMP, KIND(DATA%NMETNB) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%ISTREAM: ', ITMP, STAT
  ENDIF
  DATA%ISTREAM = INT( ITMP, KIND(DATA%ISTREAM) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NLOCGRB: ', ITMP, STAT
  ENDIF
  DATA%NLOCGRB = INT( ITMP, KIND(DATA%NLOCGRB) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NCONSENSUS: ', ITMP, STAT
  ENDIF
  DATA%NCONSENSUS = INT( ITMP, KIND(DATA%NCONSENSUS) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NDWD: ', ITMP, STAT
  ENDIF
  DATA%NDWD = INT( ITMP, KIND(DATA%NDWD) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NMFR: ', ITMP, STAT
  ENDIF
  DATA%NMFR = INT( ITMP, KIND(DATA%NMFR) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NNCEP: ', ITMP, STAT
  ENDIF
  DATA%NNCEP = INT( ITMP, KIND(DATA%NNCEP) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NUKM: ', ITMP, STAT
  ENDIF
  DATA%NUKM = INT( ITMP, KIND(DATA%NUKM) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%IREFDATE: ', ITMP, STAT
  ENDIF
  DATA%IREFDATE = INT( ITMP, KIND(DATA%IREFDATE) )

  READ(UNIT, IOSTAT=STAT) DATA%MARSTYPE
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%MARSTYPE: ', DATA%MARSTYPE, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%YCLASS
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%YCLASS: ', DATA%YCLASS, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) DATA%YEXPVER
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%YEXPVER: ', DATA%YEXPVER, STAT
  ENDIF

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NSPEC2TAB: ', ITMP, STAT
  ENDIF
  DATA%NSPEC2TAB = INT( ITMP, KIND(DATA%NSPEC2TAB) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NSPEC2TMPD: ', ITMP, STAT
  ENDIF
  DATA%NSPEC2TMPD = INT( ITMP, KIND(DATA%NSPEC2TMPD) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NSPEC2TMPP: ', ITMP, STAT
  ENDIF
  DATA%NSPEC2TMPP = INT( ITMP, KIND(DATA%NSPEC2TMPP) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NTRG2TMPD: ', ITMP, STAT
  ENDIF
  DATA%NTRG2TMPD = INT( ITMP, KIND(DATA%NTRG2TMPD) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%NTRG2TMPP: ', ITMP, STAT
  ENDIF
  DATA%NTRG2TMPP = INT( ITMP, KIND(DATA%NTRG2TMPP) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%ITMIN: ', ITMP, STAT
  ENDIF
  DATA%ITMIN = INT( ITMP, KIND(DATA%ITMIN) )

  READ(UNIT, IOSTAT=STAT) ITMP
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%WAM_%ITMAX: ', ITMP, STAT
  ENDIF
  DATA%ITMAX = INT( ITMP, KIND(DATA%ITMAX) )


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

    CASE (ERRFLAG_UNABLE_TO_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read wave parameters' )
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

END FUNCTION READ_WAM_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'PRINT_SAT_PAR'
PP_THREAD_SAFE FUNCTION PRINT_SAT_PAR( DATA, UNIT, HOOKS ) RESULT(RET)

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
  TYPE(SAT_PAR_T),      INTENT(IN)    :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J
  INTEGER(KIND=JPIB_K) :: IOERR

  !> Error flags
  INTEGER(KIND=JPIB_K), PARAMETER :: ERRFLAG_UNABLE_TO_PRINT = 1_JPIB_K

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


  WRITE(UNIT,*,IOSTAT=IOERR) ' '
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' SATELLITES PARAMETERS'
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  WRITE(UNIT,*,IOSTAT=IOERR) ' ---------------------'
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )

  WRITE(UNIT,*,IOSTAT=IOERR) ' + ALLOCATED(SATELLITES_PARAMS%MSERIES).............. :: ', ALLOCATED(DATA%MSERIES)
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  IF ( ALLOCATED(DATA%MSERIES) ) THEN
    WRITE(UNIT,*,IOSTAT=IOERR) ' + LBOUND(SATELLITES_PARAMS%MSERIES,1)............... :: ', LBOUND(DATA%MSERIES,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    WRITE(UNIT,*,IOSTAT=IOERR) ' + UBOUND(SATELLITES_PARAMS%MSERIES,1)............... :: ', UBOUND(DATA%MSERIES,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    DO I = LBOUND(DATA%MSERIES,1), UBOUND(DATA%MSERIES,1)
      WRITE(UNIT,'(A,I6,A,I10)',IOSTAT=IOERR) ' + SATELLITES_PARAMS%MSERIES(',I,').............. :: ', DATA%MSERIES(I)
      PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    ENDDO
  ENDIF

  WRITE(UNIT,*,IOSTAT=IOERR) ' + ALLOCATED(SATELLITES_PARAMS%MSATID).............. :: ', ALLOCATED(DATA%MSATID)
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  IF ( ALLOCATED(DATA%MSATID) ) THEN
    WRITE(UNIT,*,IOSTAT=IOERR) ' + LBOUND(SATELLITES_PARAMS%MSATID,1)............... :: ', LBOUND(DATA%MSATID,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    WRITE(UNIT,*,IOSTAT=IOERR) ' + UBOUND(SATELLITES_PARAMS%MSATID,1)............... :: ', UBOUND(DATA%MSATID,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    DO I = LBOUND(DATA%MSATID,1), UBOUND(DATA%MSATID,1)
      WRITE(UNIT,'(A,I6,A,I10)',IOSTAT=IOERR) ' + SATELLITES_PARAMS%MSATID(',I,').............. :: ', DATA%MSATID(I)
      PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    ENDDO
  ENDIF

  WRITE(UNIT,*,IOSTAT=IOERR) ' + ALLOCATED(SATELLITES_PARAMS%MINST).............. :: ', ALLOCATED(DATA%MINST)
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  IF ( ALLOCATED(DATA%MINST) ) THEN
    WRITE(UNIT,*,IOSTAT=IOERR) ' + LBOUND(SATELLITES_PARAMS%MINST,1)............... :: ', LBOUND(DATA%MINST,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    WRITE(UNIT,*,IOSTAT=IOERR) ' + UBOUND(SATELLITES_PARAMS%MINST,1)............... :: ', UBOUND(DATA%MINST,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    DO I = LBOUND(DATA%MINST,1), UBOUND(DATA%MINST,1)
      WRITE(UNIT,'(A,I6,A,I10)',IOSTAT=IOERR) ' + SATELLITES_PARAMS%MINST(',I,').............. :: ', DATA%MINST(I)
      PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    ENDDO
  ENDIF

  WRITE(UNIT,*,IOSTAT=IOERR) ' + ALLOCATED(SATELLITES_PARAMS%MCHAN).............. :: ', ALLOCATED(DATA%MCHAN)
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  IF ( ALLOCATED(DATA%MCHAN) ) THEN
    WRITE(UNIT,*,IOSTAT=IOERR) ' + LBOUND(SATELLITES_PARAMS%MCHAN,1)............... :: ', LBOUND(DATA%MCHAN,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    WRITE(UNIT,*,IOSTAT=IOERR) ' + UBOUND(SATELLITES_PARAMS%MCHAN,1)............... :: ', UBOUND(DATA%MCHAN,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    DO I = LBOUND(DATA%MCHAN,1), UBOUND(DATA%MCHAN,1)
      WRITE(UNIT,'(A,I6,A,I10)',IOSTAT=IOERR) ' + SATELLITES_PARAMS%MCHAN(',I,').............. :: ', DATA%MCHAN(I)
      PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    ENDDO
  ENDIF

  WRITE(UNIT,*,IOSTAT=IOERR) ' + ALLOCATED(SATELLITES_PARAMS%RCWN).............. :: ', ALLOCATED(DATA%RCWN)
  PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
  IF ( ALLOCATED(DATA%RCWN) ) THEN
    WRITE(UNIT,*,IOSTAT=IOERR) ' + LBOUND(SATELLITES_PARAMS%RCWN,1)............... :: ', LBOUND(DATA%RCWN,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    WRITE(UNIT,*,IOSTAT=IOERR) ' + UBOUND(SATELLITES_PARAMS%RCWN,1)............... :: ', UBOUND(DATA%RCWN,1)
    PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    DO I = LBOUND(DATA%RCWN,1), UBOUND(DATA%RCWN,1)
      WRITE(UNIT,'(A,I6,A,F11.4)',IOSTAT=IOERR) ' + SATELLITES_PARAMS%RCWN(',I,').............. :: ', DATA%RCWN(I)
      PP_DEBUG_CRITICAL_COND_THROW( IOERR.NE.0, ERRFLAG_UNABLE_TO_PRINT )
    ENDDO
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
    CASE (ERRFLAG_UNABLE_TO_PRINT)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'unable to print satellite parameters' )
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

END FUNCTION PRINT_SAT_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'WRITE_SAT_PAR'
PP_THREAD_SAFE FUNCTION WRITE_SAT_PAR(DATA, UNIT, HOOKS ) RESULT(RET)

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
  TYPE(SAT_PAR_T),      INTENT(IN)    :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=JPIB_K) :: I
  INTEGER(KIND=JPIB_K) :: J

  !> Error flags
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

  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%MSERIES)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( ALLOCATED(DATA%MSERIES) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%MSERIES,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%MSERIES,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    DO I = LBOUND(DATA%MSERIES,1), UBOUND(DATA%MSERIES,1)
      WRITE(UNIT, IOSTAT=STAT) INT( DATA%MSERIES(I), INT64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    ENDDO
  ENDIF

  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%MSATID)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( ALLOCATED(DATA%MSATID) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%MSATID,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%MSATID,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    DO I = LBOUND(DATA%MSATID,1), UBOUND(DATA%MSATID,1)
      WRITE(UNIT, IOSTAT=STAT) INT( DATA%MSATID(I), INT64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    ENDDO
  ENDIF

  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%MINST)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( ALLOCATED(DATA%MINST) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%MINST,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%MINST,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    DO I = LBOUND(DATA%MINST,1), UBOUND(DATA%MINST,1)
      WRITE(UNIT, IOSTAT=STAT) INT( DATA%MINST(I), INT64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    ENDDO
  ENDIF

  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%MCHAN)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( ALLOCATED(DATA%MCHAN) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%MCHAN,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%MCHAN,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    DO I = LBOUND(DATA%MCHAN,1), UBOUND(DATA%MCHAN,1)
      WRITE(UNIT, IOSTAT=STAT) INT( DATA%MCHAN(I), INT64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    ENDDO
  ENDIF

  WRITE(UNIT, IOSTAT=STAT) ALLOCATED(DATA%RCWN)
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
  IF ( ALLOCATED(DATA%RCWN) ) THEN
    WRITE(UNIT, IOSTAT=STAT) INT( LBOUND(DATA%RCWN,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    WRITE(UNIT, IOSTAT=STAT) INT( UBOUND(DATA%RCWN,1), INT64)
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    DO I = LBOUND(DATA%RCWN,1), UBOUND(DATA%RCWN,1)
      WRITE(UNIT, IOSTAT=STAT) REAL( DATA%RCWN(I), REAL64)
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_WRITE )
    ENDDO
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

    CASE (ERRFLAG_UNABLE_TO_WRITE)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to write satellite parameters' )
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

END FUNCTION WRITE_SAT_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE


#define PP_PROCEDURE_TYPE 'FUNCTION'
#define PP_PROCEDURE_NAME 'READ_SAT_PAR'
PP_THREAD_SAFE FUNCTION READ_SAT_PAR( DATA, UNIT, VERBOSE, HOOKS ) RESULT(RET)

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
  TYPE(SAT_PAR_T),      INTENT(INOUT) :: DATA
  INTEGER(KIND=JPIB_K), INTENT(IN)    :: UNIT
  LOGICAL,              INTENT(IN)    :: VERBOSE
  TYPE(HOOKS_T),        INTENT(INOUT) :: HOOKS

  ! Function result
  INTEGER(KIND=JPIB_K) :: RET

  ! Local variables
  REAL(KIND=REAL64)    :: RTMP
  INTEGER(KIND=INT64)  :: ITMP
  INTEGER(KIND=JPIB_K) :: STAT
  INTEGER(KIND=INT64) :: LB1
  INTEGER(KIND=INT64) :: UB1
  INTEGER(KIND=JPIB_K) :: I
  LOGICAL :: IS_MSERIES_ALLOCATED
  LOGICAL :: IS_MSATID_ALLOCATED
  LOGICAL :: IS_MINST_ALLOCATED
  LOGICAL :: IS_MCHAN_ALLOCATED
  LOGICAL :: IS_RCWN_ALLOCATED

  !> Error flags
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

  IF ( ALLOCATED(DATA%MSERIES) ) THEN
    DEALLOCATE(DATA%MSERIES)
  ENDIF
  IF ( ALLOCATED(DATA%MSATID) ) THEN
    DEALLOCATE(DATA%MSATID)
  ENDIF
  IF ( ALLOCATED(DATA%MINST) ) THEN
    DEALLOCATE(DATA%MINST)
  ENDIF
  IF ( ALLOCATED(DATA%MCHAN) ) THEN
    DEALLOCATE(DATA%MCHAN)
  ENDIF
  IF ( ALLOCATED(DATA%RCWN) ) THEN
    DEALLOCATE(DATA%RCWN)
  ENDIF

  READ(UNIT, IOSTAT=STAT) IS_MSERIES_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SAT_%IS_MSERIES_ALLOCATED: ', IS_MSERIES_ALLOCATED, STAT
  ENDIF
  IF ( IS_MSERIES_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%UB1: ', UB1, STAT
    ENDIF
    ALLOCATE( DATA%MSERIES(LB1:UB1), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    DO I = LBOUND(DATA%MSERIES,1), UBOUND(DATA%MSERIES,1)
      READ(UNIT, IOSTAT=STAT) ITMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
      IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%GEO_%MSERIES(', I,'): ', ITMP, STAT
      ENDIF
      DATA%MSERIES(I) = INT( ITMP, KIND(DATA%MSERIES(I)))
    ENDDO
  ENDIF


  READ(UNIT, IOSTAT=STAT) IS_MSATID_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SAT_%IS_MSATID_ALLOCATED: ', IS_MSATID_ALLOCATED, STAT
  ENDIF
  IF ( IS_MSATID_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%UB1: ', UB1, STAT
    ENDIF
    ALLOCATE( DATA%MSATID(LB1:UB1), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    DO I = LBOUND(DATA%MSATID,1), UBOUND(DATA%MSATID,1)
      READ(UNIT, IOSTAT=STAT) ITMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
      IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%GEO_%MSATID(', I,'): ', ITMP, STAT
      ENDIF
      DATA%MSATID(I) = INT( ITMP, KIND(DATA%MSATID(I)))
    ENDDO
  ENDIF


  READ(UNIT, IOSTAT=STAT) IS_MINST_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SAT_%IS_MINST_ALLOCATED: ', IS_MINST_ALLOCATED, STAT
  ENDIF
  IF ( IS_MINST_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%UB1: ', UB1, STAT
    ENDIF
    ALLOCATE( DATA%MINST(LB1:UB1), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    DO I = LBOUND(DATA%MINST,1), UBOUND(DATA%MINST,1)
      READ(UNIT, IOSTAT=STAT) ITMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
      IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%GEO_%MINST(', I,'): ', ITMP, STAT
      ENDIF
      DATA%MINST(I) = INT( ITMP, KIND(DATA%MINST(I)))
    ENDDO
  ENDIF


  READ(UNIT, IOSTAT=STAT) IS_MCHAN_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SAT_%IS_MCHAN_ALLOCATED: ', IS_MCHAN_ALLOCATED, STAT
  ENDIF
  IF ( IS_MCHAN_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%UB1: ', UB1, STAT
    ENDIF
    ALLOCATE( DATA%MCHAN(LB1:UB1), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    DO I = LBOUND(DATA%MCHAN,1), UBOUND(DATA%MCHAN,1)
      READ(UNIT, IOSTAT=STAT) ITMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
      IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%GEO_%MCHAN(', I,'): ', ITMP, STAT
      ENDIF
      DATA%MCHAN(I) = INT( ITMP, KIND(DATA%MCHAN(I)))
    ENDDO
  ENDIF


  READ(UNIT, IOSTAT=STAT) IS_RCWN_ALLOCATED
  PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
  IF ( VERBOSE ) THEN
    WRITE(*,*) ' + PAR%SAT_%IS_RCWN_ALLOCATED: ', IS_RCWN_ALLOCATED, STAT
  ENDIF
  IF ( IS_RCWN_ALLOCATED ) THEN
    READ(UNIT, IOSTAT=STAT) LB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%LB1: ', LB1, STAT
    ENDIF
    READ(UNIT, IOSTAT=STAT) UB1
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    IF ( VERBOSE ) THEN
      WRITE(*,*) ' + PAR%SAT_%UB1: ', UB1, STAT
    ENDIF
    ALLOCATE( DATA%RCWN(LB1:UB1), STAT=STAT  )
    PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
    DO I = LBOUND(DATA%RCWN,1), UBOUND(DATA%RCWN,1)
      READ(UNIT, IOSTAT=STAT) RTMP
      PP_DEBUG_DEVELOP_COND_THROW( STAT.NE.0, ERRFLAG_UNABLE_TO_READ )
      IF ( VERBOSE ) THEN
        WRITE(*,*) ' + PAR%GEO_%RCWN(', I,'): ', ITMP, STAT
      ENDIF
      DATA%RCWN(I) = REAL( RTMP, KIND(DATA%RCWN(I)))
    ENDDO
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
    CASE (ERRFLAG_UNABLE_TO_READ)
      PP_DEBUG_PUSH_MSG_TO_FRAME( 'Unable to read satelite parameters' )
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

END FUNCTION READ_SAT_PAR
#undef PP_PROCEDURE_NAME
#undef PP_PROCEDURE_TYPE

END MODULE IFS_PAR_MOD
#undef PP_SECTION_NAME
#undef PP_SECTION_TYPE
#undef PP_FILE_NAME
