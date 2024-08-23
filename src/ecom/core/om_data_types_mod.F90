MODULE OM_DATA_TYPES_MOD

    ! Symbols imported from other modules within the project.
    USE :: OM_DATA_KIND_MOD, ONLY: JPIB_K
    USE :: OM_DATA_KIND_MOD, ONLY: JPRD_K

IMPLICIT NONE

! Default visibility
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
  ! NINDAT : run initial date in the form YYYYMMDD
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

  REAL(KIND=JPIB_K) :: TSTEP

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


TYPE :: MODEL_PAR_SERIAL_T
  INTEGER(KIND=JPIB_K), DIMENSION(44) :: DIMS_
  INTEGER(KIND=JPIB_K), DIMENSION(:), ALLOCATABLE :: IBUF_
  REAL(KIND=JPRD_K),    DIMENSION(:), ALLOCATABLE :: RBUF_
  CHARACTER(LEN=16),    DIMENSION(:), ALLOCATABLE :: CBUF_
END TYPE


!> brief base class used to represent messages
TYPE :: OM_BASE_MSG_A

  !> @brief unique identifier of the message (level for atmosphere, a cobination of freq. and angle for wave)
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

TYPE :: CURR_TIME_T

  !> Reference time of the simulation (DataDate,DataTime)
  INTEGER(KIND=JPIB_K), DIMENSION(6) :: SIM_REF_TIME = 0_JPIB_K

  !> PAcked version of the above information
  INTEGER(KIND=JPIB_K) :: DATADATE
  INTEGER(KIND=JPIB_K) :: DATATIME

  !> Current time in seconds
  INTEGER(KIND=JPIB_K) :: ISEC = 0_JPIB_K

  !> Start time in seconds
  INTEGER(KIND=JPIB_K) :: ISEC0 = 0_JPIB_K

  !> Current step id
  INTEGER(KIND=JPIB_K) :: ISTEP = 0_JPIB_K

  !> Start step id
  INTEGER(KIND=JPIB_K) :: ISTEP0 = 0_JPIB_K

  !> Length of the time step in seconds
  REAL(KIND=JPRD_K) :: TSTEP = 0.0_JPRD_K

  !> True if the current timestep is is at the beginning of the simulation
  !> and requires special handling
  LOGICAL :: IS_STEP_0
END TYPE

! Whitelist of public symbols
PUBLIC :: CURR_TIME_T
PUBLIC :: PROC_TOPO_T
PUBLIC :: MODEL_PAR_T
PUBLIC :: MODEL_PAR_SERIAL_T
PUBLIC :: SIM_PAR_T
PUBLIC :: SAT_PAR_T
PUBLIC :: GEO_PAR_T
PUBLIC :: ATM_PAR_T
PUBLIC :: WAM_PAR_T
PUBLIC :: OM_BASE_MSG_A
PUBLIC :: OM_ATM_MSG_T
PUBLIC :: OM_WAM_MSG_T


END MODULE OM_DATA_TYPES_MOD
