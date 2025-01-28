#if 0
IF (LHOOK) CALL DR_HOOK('GRIB_UTILS_MOD:GRIB_SET_TIME',0,ZHOOK_HANDLE)

IF( LDPPSTEPS ) THEN
  ISEC  = INT(REAL(KSTEP,KIND=JPRD)*3600._JPRD)
ELSE
  ISEC  = INT(REAL(KSTEP,KIND=JPRD)*PTSTEP)
ENDIF

IF(TRIM(CDTYPE) == 'fc') THEN
  ISEC = ISEC+KSTEPINI*3600
ENDIF

!* CHANGE GRIB HEADERS TO WRITE FC IF VAREPS

!   Consider a VAREPS system with two legs: ModelA and modelB

!           t=0              ISEC0  ISECOVERLAP
!   ModelA:  |-----------------|----x----|

!                             t=0  ISEC
!   ModelB:                    |----x-------------------|------------|
!
!   In ModelB, to define correctly the time-step in the grib-header,
!   ISEC is shifted by ISEC0. Then, if ISEC is less than ISECOVERLAP
!   data are written in the overlap stream efov (1034-1032), otherwise
!   in stream enfo (1035-1033).

LDVALID = .TRUE.

IF(ISEC <= 0) THEN
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','instant')
  IF(TRIM(CDTYPE) /= 'an') THEN
    CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'timeRangeIndicator',1)
  ENDIF
ENDIF


ISECSTART = -9999
SELECT CASE (KGRIBCD)
CASE(NGRBMX2T,NGRB10FG,NGRBMXTPR)
  !       these parameters are max since the last post-processing step
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','max')
  ISECSTART = MAX(KPREVPP*NINT(PTSTEP)+ISEC0,ISEC0)
  TIMEPROC = ISEC-ISECSTART
CASE(NGRBMN2T,NGRBMNTPR)
  !       these parameters are min since the last post-processing step
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','min')
  ISECSTART = MAX(KPREVPP*NINT(PTSTEP)+ISEC0,ISEC0)
CASE(NGRBMX2T3, NGRB10FG3, NGRBMXTPR3)
  !       these parameters are max over the last 3 hours
  IF (MOD(ISEC,3600) /= 0) CALL ABOR1('GRIB_SET_TIME : 3 hour max only on whole hours ')
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','max')
  ISECSTART = MAX(ISEC-3*3600,ISEC0)
  ! IF (ISECSTART < ISEC0) LDVALID = .FALSE.
CASE(NGRBMN2T3, NGRBMNTPR3)
  !       these parameters are min over the last 3 hours
  IF (MOD(ISEC,3600) /= 0) CALL ABOR1('GRIB_SET_TIME : 3 hour min only on whole hours ')
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','min')
  ISECSTART = MAX(ISEC-3*3600,ISEC0)
  ! IF (ISECSTART < ISEC0) LDVALID = .FALSE.
CASE(NGRBMX2T6, NGRB10FG6, NGRBMXTPR6)
  !       these parameters are max over the last 6 hours
  IF (MOD(ISEC,3600) /= 0) CALL ABOR1('GRIB_SET_TIME : 6 hour max only on whole hours ')
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','max')
  ISECSTART = MAX(ISEC-6*3600,ISEC0)
  ! IF (ISECSTART < ISEC0) LDVALID = .FALSE.
CASE(NGRBMN2T6, NGRBMNTPR6)
  !       these parameters are min over the last 6 hours
  IF (MOD(ISEC,3600) /= 0) CALL ABOR1('GRIB_SET_TIME : 6 hour min only on whole hours ')
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','min')
  ISECSTART = MAX(ISEC-6*3600,ISEC0)
  ! IF (ISECSTART < ISEC0) LDVALID = .FALSE.
CASE(NGRBLITOTA1, NGRBLICGA1)
  !       these parameters are averages over the last 1 hour
  IF (MOD(ISEC,3600) /= 0) CALL ABOR1('GRIB_SET_TIME : 1 hour avg only on whole hours ')
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','avg')
  ISECSTART = ISEC - 1*3600
  IF (ISECSTART < ISEC0) LDVALID = .FALSE.
CASE(NGRBLITOTA3, NGRBLICGA3)
  !       these parameters are averages over the last 3 hours
  IF (MOD(ISEC,3600) /= 0) CALL ABOR1('GRIB_SET_TIME : 3 hour avg only on whole hours ')
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','avg')
  ISECSTART = ISEC - 3*3600
  IF (ISECSTART < ISEC0) LDVALID = .FALSE.
CASE(NGRBLITOTA6, NGRBLICGA6)
  !       these parameters are averages over the last 6 hours
  IF (MOD(ISEC,3600) /= 0) CALL ABOR1('GRIB_SET_TIME : 6 hour avg only on whole hours ')
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','avg')
  ISECSTART = ISEC - 6*3600
  IF (ISECSTART < ISEC0) LDVALID = .FALSE.
CASE(NGRBMXCAP6, NGRBMXCAPS6)
  !       these parameters are maximums over the last 6 hours
  IF (MOD(ISEC,3600) /= 0) CALL ABOR1('GRIB_SET_TIME : 6 hour avg only on whole hours ')
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','max')
  ISECSTART = ISEC - 6*3600
  IF (ISECSTART < ISEC0) LDVALID = .FALSE.
CASE(NGRBPTYPEMODE1)
  !       these parameters are most frequent over the last 1 hours
  IF (MOD(ISEC,3600) /= 0) CALL ABOR1('GRIB_SET_TIME : 1 hour mode (most frequent) only on whole hours ')
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','mode')
  ISECSTART = ISEC - 1*3600
  IF (ISECSTART < ISEC0) LDVALID = .FALSE.
CASE(NGRBPTYPEMODE3)
  !       these parameters are most frequent over the last 3 hours
  IF (MOD(ISEC,3600) /= 0) CALL ABOR1('GRIB_SET_TIME : 3 hour mode (most frequent) only on whole hours ')
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','mode')
  ISECSTART = ISEC - 3*3600
  IF (ISECSTART < ISEC0) LDVALID = .FALSE.
CASE(NGRBPTYPEMODE6)
  !       these parameters are most frequent over the last 6 hours
  IF (MOD(ISEC,3600) /= 0) CALL ABOR1('GRIB_SET_TIME : 6 hour mode (most frequent) only on whole hours ')
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','mode')
  ISECSTART = ISEC - 6*3600
  IF (ISECSTART < ISEC0) LDVALID = .FALSE.
CASE(NGRBPTYPESEVR1)
  !       these parameters are most severe over the last 1 hours
  IF (MOD(ISEC,3600) /= 0) CALL ABOR1('GRIB_SET_TIME : 1 hour most severe only on whole hours ')
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','severity')
  ISECSTART = ISEC - 1*3600
  IF (ISECSTART < ISEC0) LDVALID = .FALSE.
CASE(NGRBPTYPESEVR3)
  !       these parameters are most severe over the last 3 hours
  IF (MOD(ISEC,3600) /= 0) CALL ABOR1('GRIB_SET_TIME : 3 hour most severe only on whole hours ')
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','severity')
  ISECSTART = ISEC - 3*3600
  IF (ISECSTART < ISEC0) LDVALID = .FALSE.
CASE(NGRBPTYPESEVR6)
  !       these parameters are most severe over the last 6 hours
  IF (MOD(ISEC,3600) /= 0) CALL ABOR1('GRIB_SET_TIME : 6 hour most severe only on whole hours ')
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','severity')
  ISECSTART = ISEC-6*3600
  IF (ISECSTART < ISEC0) LDVALID = .FALSE.
CASE(NGRBCH4WET, &
  & 222001:222256, 223001:223256, &
  & NGRBLSPF,NGRBUVB,NGRBBLD,NGRBSSHF,NGRBSLHF,NGRBSSRD,NGRBSTRD, &
  & NGRBSSR,NGRBSTR,NGRBTSR,NGRBTTR,NGRBEWSS,NGRBNSSS,NGRBLGWS,NGRBMGWS, &
  & NGRBGWD,NGRBTSRC,NGRBTTRC,NGRBSSRC,NGRBSTRC,NGRBTISR,NGRBVIMD,NGRBFDIR, &
  & NGRBCDIR,NGRBSSRDC,NGRBSTRDC,NGRBSRO,NGRBSSRO,NGRBES,NGRBSMLT,NGRBLSP, &
  & NGRBCP,NGRBSF,NGRBE,NGRBRO,NGRBTP,NGRBCSF,NGRBLSF,NGRBPEV,NGRBPARCS, &
  & NGRBPAR,NGRBSUND,NGRBNEE,NGRBGPP,NGRBREC,NGRBFZRA,NGRBDSRP,NGRBTIVIEG, &
  & NGRBTIVING,NGRBTIDVIWG,NGRBTIDVIG,NGRBTIVIEEN,NGRBTIVINEN,NGRBTIVIEKE, &
  & NGRBTIVINKE,NGRBTIVIETE,NGRBTIVINTE,NGRBTIDVIEN,NGRBTIDVIKE,NGRBTIDVITE, &
  & NGRBTIDVIWEN,NGRBTIDVIM,NGRBTIVIEM,NGRBTIVINM,NGRBTIDVIWV,NGRBTIDVILW, &
  & NGRBTIDVIIW,NGRBTIDVIR,NGRBTIDVIS,NGRBTIVIEWV,NGRBTIVINWV,NGRBTIVIELW, &
  & NGRBTIVINLW,NGRBTIVIEIW,NGRBTIVINIW,NGRBTIVIER,NGRBTIVINR,NGRBTIVIES, &
  & NGRBTIVINS,NGRBTIVIEOZ,NGRBTIVINOZ,NGRBTIDVIOZ,NGRBTIVISOZ,&
  & NGRBMINERA:NGRBMAXERA)
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','accum')
  ISECSTART = MAX(KPREVPP*NINT(PTSTEP)+ISEC0,ISEC0)
CASE DEFAULT
  CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'stepType','instant')
END SELECT

#endif