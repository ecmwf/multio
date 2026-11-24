PROGRAM test_clte_datetime
  USE datakinds_def_mod, ONLY: JPIB_K
  USE enumerators_mod, ONLY: STREAM_CLTE_E, STREAM_OPER_E, UNDEF_PARAM_E
  USE grib_codes_mod, ONLY: NGRBMAX_I10FG
  USE ifs2mars_mod, ONLY: IFS2MARS_SET_DATETIME
  USE ifs_msg_mod, ONLY: OM_ATM_MSG_T
  USE ifs_par_mod, ONLY: MODEL_PAR_T
  USE fortran_message_mod, ONLY: FORTRAN_MESSAGE_T
  USE parametrization_mod, ONLY: PARAMETRIZATION_T
  USE hooks_mod, ONLY: HOOKS_T
  IMPLICIT NONE

  TYPE(MODEL_PAR_T) :: model
  TYPE(OM_ATM_MSG_T) :: field
  TYPE(HOOKS_T) :: hooks

  model%sim_%ctype = 'fc'
  model%sim_%nstream = STREAM_CLTE_E
  model%sim_%nindat = 19880101
  model%sim_%nsssss = 0
  model%sim_%nstepini = 0
  model%sim_%lppsteps = .FALSE.
  model%sim_%lobsc1 = .FALSE.
  model%sim_%lvareps = .FALSE.
  model%sim_%nleg = 1
  model%sim_%tstep = 200
  field%param_id_ = NGRBMAX_I10FG
  hooks%verbose_ = .FALSE.

  ! Midnight is the END of the first window; the next window starts on the new date.
  field%istep_ = 24*18
  field%iprevpp_ = 23*18
  CALL check(19880101, 2300, 1, 1_JPIB_K)
  field%istep_ = 25*18
  field%iprevpp_ = 24*18
  CALL check(19880102, 0, 1, 1_JPIB_K)

  ! Ordinary forecasts retain the forecast origin and full lead time.
  model%sim_%nstream = STREAM_OPER_E
  CALL check(19880101, 0, 25, 1_JPIB_K)
  model%sim_%nstream = STREAM_CLTE_E
  field%param_id_ = 130
  CALL check(19880102, 100, 0, UNDEF_PARAM_E)

  ! Origin = 12:00 minus the 3 h initial step. With LPPSTEPS the current
  ! step is in hours; the previous PP step is still in 200 s model steps.
  ! End = 09:00 + (3+2) h = 14:00; start = 09:00 + 3 h = 12:00.
  model%sim_%lobsc1 = .TRUE.
  model%sim_%nsssss = 12*3600
  model%sim_%nstepini = 3
  model%sim_%lppsteps = .TRUE.
  field%param_id_ = NGRBMAX_I10FG
  field%istep_ = 2
  field%iprevpp_ = 3*18
  CALL check(19880101, 1200, 2, 2_JPIB_K)

CONTAINS
  SUBROUTINE check(date, time, step, span)
    INTEGER, INTENT(IN) :: date, time, step
    INTEGER(KIND=JPIB_K), INTENT(IN) :: span
    TYPE(FORTRAN_MESSAGE_T) :: msg
    TYPE(PARAMETRIZATION_T) :: par
    INTEGER(KIND=JPIB_K) :: ret

    msg%stream = model%sim_%nstream
    ret = IFS2MARS_SET_DATETIME(field, model, msg, par, hooks)
    IF (ret /= 0) ERROR STOP 'IFS2MARS_SET_DATETIME failed'
    IF (msg%date /= date .OR. msg%time /= time .OR. msg%step /= step .OR. msg%timespan /= span) THEN
      PRINT *, 'Expected date/time/step/timespan:', date, time, step, span
      PRINT *, 'Actual date/time/step/timespan:  ', msg%date, msg%time, msg%step, msg%timespan
      ERROR STOP 'Unexpected MARS time metadata'
    ENDIF
  END SUBROUTINE
END PROGRAM
